# extract.R - Data extraction from misha tracks

#' Extract data for a panel
#'
#' Extracts track data for a panel at the given region, applies transforms.
#'
#' Supports three extraction modes configured via `plot.extraction_mode`:
#' - "fixed" (default): Uses fixed iterator + rollmean smoothing. Best for
#'   vtracks with `func=sum` where values scale with bin size.
#' - "dynamic": Adjusts iterator based on view span (like misha.vis). The
#'   iterator itself provides smoothing via aggregation, so rollmean is skipped.
#'   More efficient for large regions but values may differ at different zoom levels.
#' - "dynamic_smooth": Adjusts iterator for resolution AND dynamically sets
#'   vtrack sshift/eshift for smoothing. Combines adaptive resolution with
#'   proper value scaling. Uses `smoothing_bp` config or state for window size.
#'
#' @param browser Browser object
#' @param panel Panel configuration
#' @param region Interval to extract (data frame with chrom, start, end)
#' @param use_cache Whether to use caching
#' @return Data frame with extracted and transformed data
#' @keywords internal
extract_panel_data <- function(browser, panel, region, use_cache = TRUE) {
    profile <- getOption("misha.browser.profile", FALSE)
    timings <- list()

    if (panel$type != "data") {
        return(NULL)
    }
    if (length(panel$tracks) == 0) {
        return(NULL)
    }

    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(NULL)
    }

    # Get extraction mode: "fixed" (default), "dynamic", or "dynamic_smooth"
    extraction_mode <- browser$cfg$plot$extraction_mode %||% "fixed"
    base_iter <- browser$cfg$plot$iterator %||% 32
    target_points <- browser$cfg$plot$target_points %||% 4000

    # Track vtracks that need iterator restoration
    modified_vtracks <- character(0)

    if (extraction_mode == "dynamic") {
        # Dynamic mode: adjust iterator based on view span
        # The iterator provides smoothing via aggregation, no rollmean needed
        span <- get_span(region)
        iterator <- calc_iterator(span, base_iter, target_points)
    } else if (extraction_mode == "dynamic_smooth") {
        # Dynamic smooth mode: adjust iterator for resolution AND
        # set vtrack sshift/eshift for smoothing
        span <- get_span(region)
        iterator <- calc_iterator(span, base_iter, target_points)

        # Calculate smoothing window (from state or config)
        smoothing_bp <- browser$state$smooth_window %||%
            browser$cfg$plot$smoothing_bp %||%
            3200
        half_shift <- as.integer(round(smoothing_bp / 2))

        # Set vtrack iterators for smoothing
        for (track in panel$tracks) {
            if (is_vtrack(track, browser)) {
                tryCatch(
                    {
                        misha::gvtrack.iterator(
                            track,
                            sshift = as.integer(-half_shift),
                            eshift = half_shift
                        )
                        modified_vtracks <- c(modified_vtracks, track)
                    },
                    error = function(e) {
                        cli::cli_warn("Failed to set vtrack iterator for {track}: {e$message}")
                    }
                )
            }
        }
    } else {
        # Fixed mode: use base iterator, apply rollmean for smoothing
        iterator <- base_iter
    }

    # Resolve track specifications (handles expressions and inline vtracks)
    t0 <- Sys.time()
    track_specs <- resolve_track_specs(panel$tracks, browser, browser$cfg)
    track_exprs <- track_specs$exprs
    track_names <- track_specs$names
    temp_vtracks <- track_specs$temp_vtracks
    timings$resolve <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    # Ensure cleanup happens even if an error occurs during extraction
    on.exit(
        {
            reset_vtrack_iterators(modified_vtracks)
            cleanup_temp_vtracks(temp_vtracks)
        },
        add = TRUE
    )

    # Generate cache key (include mode to avoid cache conflicts)
    # For dynamic_smooth, include smoothing_bp in key
    smoothing_key <- if (extraction_mode == "dynamic_smooth") {
        browser$state$smooth_window %||% browser$cfg$plot$smoothing_bp %||% 3200
    } else {
        browser$state$smooth_window
    }

    # Use pre-computed panel signature if available (from validate_panel)
    panel_signature <- panel$._cache_signature %||% digest::digest(list(
        transforms = panel$transforms,
        grouping = panel$grouping,
        facet_by = panel$facet_by,
        plot_type = panel$plot_type
    ))

    # Build maps for vtrack transforms and their signatures (single pass)
    vtracks_list <- browser$cfg$vtracks %||% list()
    vtrack_transform_map <- list()
    vtrack_sig_map <- list()
    if (length(vtracks_list) > 0) {
        for (vt in vtracks_list) {
            vtrack_transform_map[[vt$name]] <- vt$transforms
            vtrack_sig_map[[vt$name]] <- vt$._transform_signature %||% digest::digest(vt$transforms)
        }
    }
    vtrack_signatures <- vapply(track_names, function(name) {
        vtrack_sig_map[[name]] %||% ""
    }, character(1))
    vtrack_signature <- digest::digest(vtrack_signatures)
    cache_k <- cache_key(
        panel$tracks,
        panel_signature,
        vtrack_signature,
        region$chrom, region$start, region$end,
        iterator,
        extraction_mode,
        smoothing_key
    )

    # Check cache
    if (use_cache && cache_exists(cache_k)) {
        # on.exit() handles cleanup
        if (profile) {
            cli::cli_text("      [cache hit]")
        }
        return(cache_get(cache_k))
    }

    # Extract data
    t1 <- Sys.time()
    data <- extract_tracks(
        tracks = track_exprs,
        region = region,
        iterator = iterator,
        colnames = track_names
    )
    timings$gextract <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

    # Note: Cleanup is handled by on.exit() registered above

    if (is.null(data) || nrow(data) == 0) {
        return(NULL)
    }

    # Apply transforms
    t2 <- Sys.time()
    transforms <- panel$transforms %||% list()
    if (extraction_mode %in% c("dynamic", "dynamic_smooth")) {
        # In dynamic/dynamic_smooth modes, skip smooth transforms
        # (iterator or vtrack aggregation provides smoothing)
        transforms <- Filter(function(t) t$type != "smooth", transforms)
    } else if (!is.null(browser$state$smooth_window)) {
        # In fixed mode, update smooth window from state
        transforms <- update_smooth_window(transforms, browser$state$smooth_window)
    }

    value_cols <- setdiff(names(data), c("chrom", "start", "end", "pos", "intervalID"))
    data <- apply_transforms(data, transforms, value_cols)

    # Apply per-vtrack transforms after panel transforms
    if (length(track_names) > 0 && length(vtrack_transform_map) > 0) {
        for (track_name in track_names) {
            vt_transforms <- vtrack_transform_map[[track_name]]
            if (is.null(vt_transforms) || length(vt_transforms) == 0) next

            vt_transforms <- vt_transforms %||% list()
            if (extraction_mode %in% c("dynamic", "dynamic_smooth")) {
                vt_transforms <- Filter(function(t) t$type != "smooth", vt_transforms)
            } else if (!is.null(browser$state$smooth_window)) {
                vt_transforms <- update_smooth_window(vt_transforms, browser$state$smooth_window)
            }
            data <- apply_transforms(data, vt_transforms, track_name)
        }
    }
    timings$transform <- as.numeric(difftime(Sys.time(), t2, units = "secs"))

    # Add grouping metadata
    t3 <- Sys.time()
    data <- add_track_metadata(data, panel, track_names)
    timings$metadata <- as.numeric(difftime(Sys.time(), t3, units = "secs"))

    # Cache result
    cache_set(cache_k, data)

    if (profile) {
        cli::cli_text("      resolve: {round(timings$resolve, 3)}s, gextract: {round(timings$gextract, 3)}s, transform: {round(timings$transform, 3)}s, metadata: {round(timings$metadata, 3)}s")
    }

    data
}

#' Resolve track specifications to extractable expressions
#'
#' Handles three types of track specifications:
#' - String: track name, vtrack reference, or expression vtrack
#' - Object with 'expr': inline track expression
#' - Object with 'src'/'func': inline vtrack definition
#'
#' @param tracks List of track specifications
#' @param browser Browser object (for expr_vtracks lookup)
#' @param cfg Browser configuration (for inline vtrack creation)
#' @return List with 'exprs' (expressions to extract) and 'names' (column names)
#' @keywords internal
resolve_track_specs <- function(tracks, browser = NULL, cfg = NULL) {
    exprs <- character(0)
    names <- character(0)
    temp_vtracks <- character(0) # Track temporary vtracks to clean up

    # Get vtrack expressions (maps name -> expression)
    vtrack_expressions <- browser$state$vtrack_expressions %||% list()

    for (i in seq_along(tracks)) {
        track <- tracks[[i]]

        if (is.character(track)) {
            # Simple string - could be track name or vtrack reference
            if (track %in% names(vtrack_expressions)) {
                # Known vtrack - use its expression (may differ from name)
                exprs <- c(exprs, vtrack_expressions[[track]])
                names <- c(names, track)
            } else {
                # Regular track name (not a defined vtrack)
                exprs <- c(exprs, track)
                names <- c(names, track)
            }
        } else if (is.list(track)) {
            if (!is.null(track$expr)) {
                # Inline expression
                track_name <- track$name %||% paste0("expr_", i)
                exprs <- c(exprs, track$expr)
                names <- c(names, track_name)
            } else if (!is.null(track$src) || !is.null(track$func)) {
                # Inline vtrack definition
                track_name <- track$name %||% paste0("inline_vt_", i)

                # Validate and create the vtrack
                vt <- validate_vtrack(
                    c(list(name = track_name), track),
                    index = i
                )
                tryCatch(
                    {
                        create_vtrack(vt, cfg)
                        temp_vtracks <- c(temp_vtracks, track_name)
                        exprs <- c(exprs, track_name)
                        names <- c(names, track_name)
                    },
                    error = function(e) {
                        cli::cli_warn("Failed to create inline vtrack: {e$message}")
                    }
                )
            }
        }
    }

    list(exprs = exprs, names = names, temp_vtracks = temp_vtracks)
}

#' Clean up temporary vtracks
#'
#' @param vtracks Character vector of vtrack names to remove
#' @keywords internal
cleanup_temp_vtracks <- function(vtracks) {
    if (is.null(vtracks) || length(vtracks) == 0) {
        return(invisible(NULL))
    }
    for (vt in vtracks) {
        tryCatch(
            misha::gvtrack.rm(vt),
            error = function(e) NULL
        )
    }
    invisible(NULL)
}

#' Extract tracks from misha
#'
#' @param tracks Character vector of track names/expressions
#' @param region Interval data frame
#' @param iterator Iterator value
#' @param colnames Column names for the result (defaults to tracks)
#' @return Data frame with extracted data
#' @keywords internal
extract_tracks <- function(tracks, region, iterator = 32, colnames = NULL) {
    if (length(tracks) == 0) {
        return(NULL)
    }

    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(NULL)
    }

    # Default colnames to tracks
    if (is.null(colnames)) {
        colnames <- tracks
    }

    # Validate track existence before extraction
    # Note: This catches non-existent tracks early with a helpful error message
    # Skip validation for expressions (containing operators) and vtracks
    for (i in seq_along(tracks)) {
        track <- tracks[i]
        # Skip if it looks like an expression (contains operators or parentheses)
        if (grepl("[+*/()[:space:]-]", track)) {
            next
        }
        # Check if track exists as a regular track or vtrack
        if (!track_exists(track)) {
            # Check if it's a known vtrack
            vtrack_info <- tryCatch(misha::gvtrack.info(track), error = function(e) NULL)
            if (is.null(vtrack_info)) {
                cli::cli_warn(c(
                    "Track not found: {track}",
                    "i" = "Check that the track exists in the misha database or is defined as a vtrack"
                ))
            }
        }
    }

    tryCatch(
        {
            data <- misha::gextract(
                tracks,
                region,
                iterator = iterator,
                colnames = colnames
            )

            if (is.null(data) || nrow(data) == 0) {
                return(NULL)
            }

            # Sort by genomic position (gextract uses intervalID for order, not position)
            data <- data[order(data$chrom, data$start, data$end), ]

            # Add position column
            data$pos <- (data$start + data$end) / 2

            # Remove intervalID if present (no longer needed after sorting)
            data$intervalID <- NULL

            data
        },
        error = function(e) {
            cli::cli_warn("Failed to extract tracks: {e$message}")
            NULL
        }
    )
}

#' Add track metadata based on grouping pattern
#'
#' Uses data.table for efficient wide-to-long transformation and joins.
#'
#' @param data Data frame with track columns
#' @param panel Panel configuration
#' @param track_names Resolved track names (column names in data)
#' @return Data frame in long format with metadata columns
#' @keywords internal
add_track_metadata <- function(data, panel, track_names = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        return(data)
    }

    grouping <- panel$grouping

    # Use provided track_names or extract from panel$tracks
    if (is.null(track_names)) {
        track_cols <- sapply(panel$tracks, function(t) {
            if (is.character(t)) t else t$name %||% "unknown"
        })
    } else {
        track_cols <- track_names
    }

    # Get position columns to preserve
    pos_cols <- c("chrom", "start", "end", "pos")
    pos_cols <- intersect(pos_cols, names(data))

    # Use data.table::melt for efficient wide-to-long conversion (2-5x faster)
    dt <- data.table::as.data.table(data)
    long_data <- data.table::melt(
        dt,
        id.vars = pos_cols,
        measure.vars = track_cols,
        variable.name = "track",
        value.name = "value",
        variable.factor = FALSE # Keep track as character
    )

    # Add metadata columns based on grouping (vectorized)
    if (!is.null(grouping)) {
        pattern <- grouping$pattern %||% "^(?<source>.+)\\.(?<mark>.+)$"
        overrides <- grouping$overrides %||% list()

        # Build metadata lookup table for unique tracks (much faster than row-by-row)
        unique_tracks <- unique(long_data$track)
        track_metadata <- data.table::data.table(track = unique_tracks)

        # Extract capture group names from pattern
        test_match <- regexpr(pattern, "test.test", perl = TRUE)
        capture_names <- attr(test_match, "capture.names")
        capture_names <- capture_names[nchar(capture_names) > 0]

        # Initialize metadata columns
        for (field in capture_names) {
            data.table::set(track_metadata, j = field, value = NA_character_)
        }

        # Process each unique track (only ~13 iterations, not 250K)
        for (i in seq_along(unique_tracks)) {
            track_name <- unique_tracks[i]

            # Check for explicit override first
            if (track_name %in% names(overrides)) {
                override <- overrides[[track_name]]
                for (field in names(override)) {
                    if (!field %in% names(track_metadata)) {
                        data.table::set(track_metadata, j = field, value = NA_character_)
                    }
                    data.table::set(track_metadata, i = i, j = field, value = override[[field]])
                }
            } else {
                # Try regex extraction
                match <- regexpr(pattern, track_name, perl = TRUE)
                if (match > 0) {
                    capture_starts <- attr(match, "capture.start")
                    capture_lengths <- attr(match, "capture.length")
                    match_capture_names <- attr(match, "capture.names")

                    if (!is.null(match_capture_names)) {
                        for (j in seq_along(match_capture_names)) {
                            field <- match_capture_names[j]
                            if (nchar(field) > 0) {
                                if (!field %in% names(track_metadata)) {
                                    data.table::set(track_metadata, j = field, value = NA_character_)
                                }
                                val <- substr(
                                    track_name,
                                    capture_starts[j],
                                    capture_starts[j] + capture_lengths[j] - 1
                                )
                                data.table::set(track_metadata, i = i, j = field, value = val)
                            }
                        }
                    }
                }
            }
        }

        # Fill NA values with track name
        meta_cols <- setdiff(names(track_metadata), "track")
        for (col in meta_cols) {
            na_idx <- which(is.na(track_metadata[[col]]))
            if (length(na_idx) > 0) {
                data.table::set(track_metadata,
                    i = na_idx, j = col,
                    value = track_metadata$track[na_idx]
                )
            }
        }

        # Merge metadata into long_data using data.table merge
        long_data <- data.table::merge.data.table(long_data, track_metadata, by = "track", all.x = TRUE)
    }

    # Return as data.frame for compatibility with downstream code
    as.data.frame(long_data)
}

#' Extract annotation data (genes, exons, TSS)
#'
#' @param panel Annotation panel configuration
#' @param region Interval data frame
#' @return List with exons, tss, genes data frames
#' @keywords internal
extract_annotation_data <- function(panel, region) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(NULL)
    }

    profile <- getOption("misha.browser.profile", FALSE)
    label_field <- panel$gene_label_field %||% "geneSymbol"
    cache_k <- cache_key(
        "annotations",
        panel$exon_source,
        panel$tss_source,
        label_field,
        region$chrom,
        region$start,
        region$end
    )
    if (cache_exists(cache_k)) {
        if (profile) {
            cli::cli_text("      [annotation cache hit]")
        }
        return(cache_get(cache_k))
    }

    result <- list(exons = NULL, tss = NULL, genes = NULL)

    # Extract exons
    if (!is.null(panel$exon_source)) {
        tryCatch(
            {
                exons <- suppressWarnings(misha::gintervals.neighbors(
                    panel$exon_source,
                    region[, c("chrom", "start", "end")],
                    maxdist = 0
                ))
                if (!is.null(exons) && nrow(exons) > 0) {
                    result$exons <- exons
                }
            },
            error = function(e) {
                cli::cli_warn("Failed to load exons: {e$message}")
            }
        )
    }

    # Extract TSS
    if (!is.null(panel$tss_source)) {
        tryCatch(
            {
                tss <- suppressWarnings(misha::gintervals.neighbors(
                    panel$tss_source,
                    region[, c("chrom", "start", "end")],
                    maxdist = 0
                ))
                if (!is.null(tss) && nrow(tss) > 0) {
                    result$tss <- tss
                }
            },
            error = function(e) {
                cli::cli_warn("Failed to load TSS: {e$message}")
            }
        )
    }

    # Compute gene metadata from exons
    if (!is.null(result$exons)) {
        if (label_field %in% names(result$exons)) {
            result$genes <- result$exons %>%
                dplyr::group_by(.data[[label_field]]) %>%
                dplyr::summarise(
                    min_start = min(start),
                    max_end = max(end),
                    strand = dplyr::first(strand),
                    .groups = "drop"
                ) %>%
                dplyr::arrange(min_start)
        }
    }

    cache_set(cache_k, result)
    result
}

#' Load navigator regions
#'
#' @param browser Browser object
#' @return Data frame with regions and labels
#' @keywords internal
load_navigator_regions <- function(browser) {
    nav <- browser$cfg$navigator
    if (is.null(nav$source)) {
        return(NULL)
    }

    with_cache(cache_key("navigator", nav$source, nav$label_field), function() {
        tryCatch(
            {
                if (intervals_exist(nav$source)) {
                    misha::gintervals.load(nav$source)
                } else if (file.exists(nav$source)) {
                    readr::read_csv(nav$source, show_col_types = FALSE)
                } else {
                    NULL
                }
            },
            error = function(e) {
                cli::cli_warn("Failed to load navigator regions: {e$message}")
                NULL
            }
        )
    })
}

#' Check if a track is a virtual track
#'
#' Uses cached vtrack list from browser state for performance.
#' Falls back to misha::gvtrack.info() if browser not provided.
#'
#' @param track Track name
#' @param browser Browser object (optional, for cached lookup)
#' @return TRUE if track is a virtual track
#' @keywords internal
is_vtrack <- function(track, browser = NULL) {
    # Use cached list if browser provided (fast path)
    if (!is.null(browser) && !is.null(browser$state$misha_vtrack_names)) {
        return(track %in% browser$state$misha_vtrack_names)
    }

    # Fallback to misha call (slow path)
    tryCatch(
        {
            info <- misha::gvtrack.info(track)
            !is.null(info)
        },
        error = function(e) {
            FALSE
        }
    )
}

#' Reset vtrack iterators to default
#'
#' Removes custom sshift/eshift from virtual tracks by calling
#' gvtrack.iterator with NULL values.
#'
#' @param vtracks Character vector of vtrack names
#' @keywords internal
reset_vtrack_iterators <- function(vtracks) {
    if (length(vtracks) == 0) {
        return(invisible(NULL))
    }
    for (track in vtracks) {
        tryCatch(
            {
                # Reset by setting shifts to NULL (removes the override)
                misha::gvtrack.iterator(track, sshift = NULL, eshift = NULL)
            },
            error = function(e) {
                # Silently ignore errors during cleanup
            }
        )
    }
    invisible(NULL)
}

#' Extract data for multiple panels in parallel
#'
#' Uses the `future` package to extract data for multiple panels concurrently.
#' Requires `future` and `promises` packages to be installed and a parallel
#' plan to be set (e.g., `future::plan(future::multisession)`).
#'
#' Note: This only parallelizes data extraction. ggplot rendering must
#' remain sequential as ggplot is not thread-safe.
#'
#' @param browser Browser object
#' @param panels List of panel configurations
#' @param region Viewing region
#' @param use_cache Whether to use caching
#' @return Named list of extracted data frames (keyed by panel name)
#' @keywords internal
extract_panels_parallel <- function(browser, panels, region, use_cache = TRUE) {
    # Check if future is available
    if (!requireNamespace("future", quietly = TRUE)) {
        cli::cli_warn("Package 'future' not installed. Falling back to sequential extraction.")
        return(extract_panels_sequential(browser, panels, region, use_cache))
    }

    # Filter to data panels only (other panel types don't use extract_panel_data)
    data_panels <- Filter(function(p) (p$type %||% "data") == "data", panels)

    if (length(data_panels) == 0) {
        return(list())
    }

    # Check if we're already in a future (avoid nested parallelism)
    if (inherits(future::plan(), "sequential")) {
        cli::cli_inform("Sequential future plan detected. Running extraction sequentially.")
        return(extract_panels_sequential(browser, panels, region, use_cache))
    }

    profile <- getOption("misha.browser.profile", FALSE)
    t0 <- Sys.time()

    # Get misha root to pass to workers (workers don't inherit gsetroot state)
    misha_root <- browser$cfg$._misha_root

    # Launch futures for each panel's data extraction
    data_futures <- lapply(data_panels, function(panel) {
        future::future(
            {
                # Set misha root in worker (workers don't inherit parent's gsetroot)
                if (!is.null(misha_root) && nzchar(misha_root)) {
                    tryCatch(
                        misha::gsetroot(misha_root),
                        error = function(e) {
                            cli::cli_warn("Worker failed to set misha root: {e$message}")
                        }
                    )
                }
                extract_panel_data(browser, panel, region, use_cache)
            },
            seed = TRUE
        )
    })

    # Collect results
    results <- lapply(data_futures, future::value)

    # Name results by panel name
    panel_names <- vapply(data_panels, function(p) p$name %||% "unnamed", character(1))
    names(results) <- panel_names

    if (profile) {
        elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        cli::cli_text("Parallel extraction of {length(data_panels)} panels: {round(elapsed, 3)}s")
    }

    results
}

#' Extract data for multiple panels sequentially
#'
#' Sequential fallback for parallel extraction.
#'
#' @param browser Browser object
#' @param panels List of panel configurations
#' @param region Viewing region
#' @param use_cache Whether to use caching
#' @return Named list of extracted data frames
#' @keywords internal
extract_panels_sequential <- function(browser, panels, region, use_cache = TRUE) {
    data_panels <- Filter(function(p) (p$type %||% "data") == "data", panels)

    if (length(data_panels) == 0) {
        return(list())
    }

    results <- lapply(data_panels, function(panel) {
        extract_panel_data(browser, panel, region, use_cache)
    })

    panel_names <- vapply(data_panels, function(p) p$name %||% "unnamed", character(1))
    names(results) <- panel_names

    results
}
