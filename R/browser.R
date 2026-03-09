# browser.R - Main browser S3 class for misha.browser

#' Create a new genome browser
#'
#' Creates a browser object from a configuration file or programmatically.
#'
#' @param config Path to YAML configuration file, or NULL for programmatic setup
#' @param misha_root Path to misha database root (used if config is NULL)
#' @param title Browser title (used if config is NULL)
#' @param profile Profile name to use from configuration file
#' @return A browser object
#' @export
#' @examples
#' \dontrun{
#' # From configuration file
#' browser <- browser_create(config = "my_browser.yaml")
#'
#' # Programmatically
#' browser <- browser_create(misha_root = "/path/to/misha") %>%
#'     browser_add_panel(name = "signal", tracks = c("track1", "track2"))
#' }
browser_create <- function(config = NULL, misha_root = NULL, title = "Genome Browser",
                           profile = NULL) {
    timings <- list()
    t0 <- Sys.time()

    t_cfg <- Sys.time()
    if (!is.null(config)) {
        cfg <- browser_load_config(config, profile = profile)
    } else {
        cfg <- browser_create_config(misha_root = misha_root, title = title)
    }
    timings$config <- as.numeric(difftime(Sys.time(), t_cfg, units = "secs"))

    browser <- structure(
        list(
            cfg = cfg,
            state = list(
                current_region = NULL,
                highlight = NULL,
                smooth_window = cfg$ui$smooth_window_default,
                active_tracks = NULL,
                vlines_enabled = sapply(cfg$vlines, function(v) v$enabled %||% TRUE)
            )
        ),
        class = "browser"
    )

    # Set misha root if available
    t_root <- Sys.time()
    if (!is.null(cfg$._misha_root)) {
        if (!dir.exists(cfg$._misha_root)) {
            cli::cli_abort(c(
                "Misha root directory does not exist",
                "x" = "Path: {cfg$._misha_root}",
                "i" = "Check the 'misha_root' setting in your configuration profile"
            ))
        }
        tryCatch(
            misha::gsetroot(cfg$._misha_root),
            error = function(e) {
                cli::cli_abort(c(
                    "Failed to initialize misha database",
                    "x" = e$message,
                    "i" = "Path: {cfg$._misha_root}"
                ))
            }
        )
    }
    timings$set_root <- as.numeric(difftime(Sys.time(), t_root, units = "secs"))

    # Initialize virtual tracks
    t_vtracks <- Sys.time()
    browser <- init_vtracks(browser)
    timings$vtracks <- as.numeric(difftime(Sys.time(), t_vtracks, units = "secs"))

    # Set initial region
    t_region <- Sys.time()
    browser <- init_start_region(browser)
    timings$start_region <- as.numeric(difftime(Sys.time(), t_region, units = "secs"))

    timings$total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    browser$state$startup_timings <- timings

    browser
}

#' Initialize virtual tracks from configuration
#'
#' @param browser Browser object
#' @return Updated browser object
#' @keywords internal
init_vtracks <- function(browser) {
    vtracks <- browser$cfg$vtracks

    # Cache the list of all existing vtracks from misha (for fast is_vtrack lookups)
    browser$state$misha_vtrack_names <- tryCatch(
        misha::gvtrack.ls(),
        error = function(e) character(0)
    )

    # Track vtrack expressions (maps name -> expression for extraction)
    # This allows wrapping vtracks with functions like pmax(vtrack, 0)
    browser$state$vtrack_expressions <- list()

    if (length(vtracks) == 0) {
        return(browser)
    }

    # Pre-allocate vector for new vtrack names to avoid O(n^2) concatenation
    new_vtrack_names <- character(length(vtracks))
    successfully_created <- 0

    for (i in seq_along(vtracks)) {
        vt <- vtracks[[i]]

        # Expression vtracks: store expression for extraction but don't create misha vtrack
        if (vt$vtype == "expr") {
            # Store the raw expression for use during extraction
            browser$state$vtrack_expressions[[vt$name]] <- vt$expr
            # Don't add to misha_vtrack_names since this isn't a real misha vtrack
            next
        }

        tryCatch(
            {
                create_vtrack(vt, browser$cfg)
                # Store the expression (defaults to name, but can be wrapped)
                browser$state$vtrack_expressions[[vt$name]] <- vt$expression
                # Track successful creation
                successfully_created <- successfully_created + 1
                new_vtrack_names[successfully_created] <- vt$name
            },
            error = function(e) {
                if (!grepl("exists", e$message, ignore.case = TRUE)) {
                    cli::cli_warn("Failed to create vtrack '{vt$name}': {e$message}")
                }
            }
        )
    }

    # Add all new vtrack names at once (efficient single concatenation)
    if (successfully_created > 0) {
        browser$state$misha_vtrack_names <- unique(c(
            browser$state$misha_vtrack_names,
            new_vtrack_names[seq_len(successfully_created)]
        ))
    }

    browser
}

#' Create a virtual track from configuration
#'
#' @param vt Vtrack configuration
#' @param cfg Browser configuration (for resolving paths)
#' @keywords internal
create_vtrack <- function(vt, cfg = NULL) {
    # Build gvtrack.create arguments
    create_args <- list(vtrack = vt$name)

    # Determine vtype if not set
    func <- vt$func %||% "sum"
    vtype <- vt$vtype
    if (is.null(vtype)) {
        if (is_sequence_function(func)) {
            vtype <- "sequence"
        } else if (is_intervals_function(func)) {
            vtype <- "intervals"
        } else {
            vtype <- "standard"
        }
    }

    # Source: NULL for sequence-based, track/intervals for others
    if (vtype == "sequence") {
        create_args$src <- NULL
    } else if (vtype == "intervals" || is_intervals_function(func)) {
        # Resolve intervals source (handles @uploaded:, database intervals, file paths)
        src <- vt$src
        if (!is.null(src) && nchar(src) > 0) {
            resolved_src <- tryCatch(
                resolve_intervals_source(src),
                error = function(e) {
                    # Fall back to using src directly if resolution fails
                    cli::cli_warn("Could not resolve intervals source '{src}': {e$message}")
                    src
                }
            )
            create_args$src <- resolved_src
        } else {
            create_args$src <- vt$src
        }
    } else {
        create_args$src <- vt$src
    }

    # Function
    if (!is.null(vt$func)) {
        create_args$func <- vt$func
    }

    # Function parameters
    if (!is.null(vt$params)) {
        params <- vt$params

        # Handle PSSM resolution for PWM functions
        if (is_pwm_function(func) && !is.null(params$pssm)) {
            if (is.character(params$pssm)) {
                # Resolve PSSM source (handles @uploaded:, prego motifs, file paths)
                params$pssm <- tryCatch(
                    resolve_pssm_source(params$pssm),
                    error = function(e) {
                        # Try legacy path resolution
                        pssm_path <- resolve_path(params$pssm, cfg)
                        if (file.exists(pssm_path)) {
                            as.matrix(utils::read.csv(pssm_path, row.names = 1))
                        } else {
                            stop("Could not resolve PSSM: ", e$message)
                        }
                    }
                )
            }
        }

        # Add params to create_args
        if (is.list(params) && !is.null(names(params))) {
            for (pname in names(params)) {
                create_args[[pname]] <- params[[pname]]
            }
        } else if (length(params) == 1 && !is.list(params)) {
            # Handle unnamed single param (e.g., quantile percentile, neighbor.count distance)
            create_args$params <- params
        } else if (is.list(params) && is.null(names(params)) && length(params) == 1) {
            create_args$params <- params[[1]]
        }
    }

    # Filter
    if (!is.null(vt$filter)) create_args$filter <- vt$filter

    # Create the vtrack
    do.call(misha::gvtrack.create, create_args)

    # Set iterator if shifts specified
    if (!is.null(vt$sshift) || !is.null(vt$eshift) || !is.null(vt$dim)) {
        iter_args <- list(vtrack = vt$name)
        if (!is.null(vt$sshift)) iter_args$sshift <- vt$sshift
        if (!is.null(vt$eshift)) iter_args$eshift <- vt$eshift
        if (!is.null(vt$dim)) iter_args$dim <- vt$dim
        tryCatch(
            do.call(misha::gvtrack.iterator, iter_args),
            error = function(e) NULL
        )
    }

    invisible(NULL)
}

#' Initialize start region from configuration
#'
#' @param browser Browser object
#' @return Updated browser object
#' @keywords internal
init_start_region <- function(browser) {
    start <- browser$cfg$start

    if (!is.null(start$coords)) {
        browser$state$current_region <- make_region(
            start$coords$chrom, start$coords$start, start$coords$end
        )
    } else if (!is.null(start$gene)) {
        nav_source <- browser$cfg$navigator$source
        label_field <- browser$cfg$navigator$label_field
        extension <- start$span_bp %||% browser$cfg$navigator$extension

        tryCatch(
            {
                all_regions <- if (intervals_exist(nav_source)) {
                    misha::gintervals.load(nav_source)
                } else if (file.exists(nav_source)) {
                    data.table::fread(nav_source, data.table = FALSE)
                } else {
                    NULL
                }

                if (!is.null(all_regions) && label_field %in% names(all_regions)) {
                    gene_region <- all_regions[all_regions[[label_field]] == start$gene, ]
                    if (nrow(gene_region) > 0) {
                        gene_region <- gene_region[1, ]
                        center <- interval_center(gene_region)
                        browser$state$current_region <- make_region(
                            gene_region$chrom, center - extension / 2, center + extension / 2
                        )
                    }
                }
            },
            error = function(e) {
                cli::cli_warn("Failed to locate start gene '{start$gene}': {e$message}")
            }
        )
    }

    # Fallback to a default region if nothing set
    if (is.null(browser$state$current_region)) {
        browser$state$current_region <- make_region("chr1", 1e6, 2e6)
    }

    browser
}

#' Print method for browser object
#'
#' @param x Browser object
#' @param ... Additional arguments (ignored)
#' @export
print.browser <- function(x, ...) {
    cli::cli_h1(x$cfg$ui$title %||% "Genome Browser")

    # Current region
    region <- x$state$current_region
    if (!is.null(region)) {
        cli::cli_text("Region: {format_coords(region$chrom, region$start, region$end)}")
        cli::cli_text("Span: {scales::comma(get_span(region))} bp")
    }

    # Panels
    cli::cli_h2("Panels ({length(x$cfg$panels)})")
    for (panel in x$cfg$panels) {
        type_label <- switch(panel$type,
            "data" = cli::col_blue("data"),
            "annotation" = cli::col_green("annotation"),
            "ideogram" = cli::col_yellow("ideogram"),
            panel$type
        )

        if (panel$type == "data") {
            cli::cli_bullets(c("*" = "{panel$name} [{type_label}]: {length(panel$tracks)} tracks"))
        } else {
            cli::cli_bullets(c("*" = "{panel$name} [{type_label}]"))
        }
    }

    # Virtual tracks
    if (length(x$cfg$vtracks) > 0) {
        cli::cli_h2("Virtual Tracks ({length(x$cfg$vtracks)})")
        # Build vtrack -> panel name mapping (for pass-through vtracks used in panels)
        vtrack_to_panel <- character()
        for (panel in x$cfg$panels) {
            if (panel$type != "data" || is.null(panel$tracks)) next
            tracks <- if (is.list(panel$tracks)) unlist(panel$tracks) else panel$tracks
            for (track in tracks) {
                if (is.character(track) && nzchar(trimws(track)) &&
                    !track %in% names(vtrack_to_panel)) {
                    vtrack_to_panel[track] <- panel$name
                }
            }
        }
        for (vt in x$cfg$vtracks) {
            right <- vt$src %||% vt$expr %||% "sequence"
            # Use panel name when vtrack is a pass-through (name == src) used in a panel
            left <- if (identical(vt$name, vt$src) && vt$name %in% names(vtrack_to_panel)) {
                vtrack_to_panel[vt$name]
            } else {
                vt$name
            }
            cli::cli_bullets(c("*" = "{left} -> {right}"))
        }
    }

    # Vertical lines
    if (length(x$cfg$vlines) > 0) {
        enabled_count <- sum(x$state$vlines_enabled)
        cli::cli_h2("Vertical Lines ({enabled_count}/{length(x$cfg$vlines)} enabled)")
    }

    invisible(x)
}

#' Load a browser from saved configuration
#'
#' @param file Path to YAML configuration file
#' @param profile Profile name to use
#' @return Browser object
#' @noRd
browser_load <- function(file, profile = NULL) {
    browser_create(config = file, profile = profile)
}

#' Save browser configuration to file
#'
#' @param browser Browser object
#' @param file Output file path
#' @return Browser object (invisibly)
#' @export
browser_save <- function(browser, file) {
    browser_save_config(browser$cfg, file)
    invisible(browser)
}

#' Add a data panel to the browser
#'
#' @param browser Browser object
#' @param name Panel name
#' @param tracks Character vector of track names
#' @param grouping List with color_by, pattern, overrides
#' @param facet_by Variable to facet by (from grouping pattern)
#' @param transforms List of transforms to apply
#' @param plot_type Plot type: "line", "area", "point", "histogram"
#' @param colors Named vector of colors
#' @param ylim Y-axis limits
#' @param height Relative height
#' @param ... Additional panel options
#' @return Updated browser object
#' @export
browser_add_panel <- function(browser, name, tracks = NULL,
                              grouping = NULL,
                              facet_by = NULL,
                              transforms = list(),
                              plot_type = "line",
                              colors = NULL,
                              ylim = NULL,
                              height = 2,
                              ...) {
    # Auto-create vtracks for each track
    if (!is.null(tracks) && length(tracks) > 0) {
        existing_vtrack_names <- vapply(
            browser$cfg$vtracks %||% list(),
            function(v) v$name,
            character(1)
        )
        for (track in tracks) {
            # Auto-vtrack creation applies only to plain track-name strings
            if (!is.character(track) || length(track) != 1 || is.na(track) || !nzchar(track)) {
                next
            }
            # Skip if vtrack already exists with this name
            if (track %in% existing_vtrack_names) {
                next
            }
            vtrack <- list(
                name = track,
                src = track,
                func = "avg"
            )
            browser$cfg$vtracks <- c(browser$cfg$vtracks %||% list(), list(vtrack))
            existing_vtrack_names <- c(existing_vtrack_names, track)
        }
    }

    panel <- list(
        name = name,
        type = "data",
        tracks = tracks,
        grouping = grouping,
        facet_by = facet_by,
        transforms = transforms,
        plot_type = plot_type,
        colors = colors,
        ylim = ylim,
        height = height
    )

    # Add extra options
    extra <- list(...)
    for (opt in names(extra)) {
        panel[[opt]] <- extra[[opt]]
    }

    panel <- validate_panel(panel, length(browser$cfg$panels) + 1)
    browser$cfg$panels <- c(browser$cfg$panels, list(panel))

    browser
}

#' Add a virtual track to the browser
#'
#' Adds a virtual track definition to the browser configuration. Supports
#' standard vtracks (with a source track and aggregation function) and
#' expression vtracks (computed from a track expression).
#'
#' @param browser Browser object
#' @param name Name for the virtual track
#' @param src Source track name (for standard vtracks)
#' @param func Aggregation function (default "avg"). Common values: "avg", "sum", "min", "max"
#' @param vtype Vtrack type: "standard", "expr", "sequence", "intervals". Auto-detected if NULL.
#' @param expr Track expression string (for expression vtracks, e.g. "log2(1 + trackname)")
#' @param expression Optional expression wrapping the vtrack name (e.g. "pmax(vtrack_name, 0)")
#' @param sshift Start shift in bp
#' @param eshift End shift in bp
#' @param dim Dimension for 2D tracks
#' @param ... Additional vtrack parameters
#' @return Updated browser object
#' @export
#' @examples
#' \dontrun{
#' browser <- browser_create() %>%
#'     browser_add_vtrack("ctcf_log2", expr = "log2(1 + chipseq.ctcf)") %>%
#'     browser_add_vtrack("my_signal", src = "some.track", func = "avg") %>%
#'     browser_add_panel(name = "signal", tracks = c("ctcf_log2", "my_signal"))
#' }
browser_add_vtrack <- function(browser, name, src = NULL, func = "avg",
                               vtype = NULL, expr = NULL, expression = NULL,
                               sshift = NULL, eshift = NULL, dim = NULL, ...) {
    if (is.null(src) && is.null(expr)) {
        cli::cli_abort("Either {.arg src} or {.arg expr} must be provided")
    }

    if (!is.null(expr)) {
        vtrack <- list(name = name, vtype = "expr", expr = expr)
    } else {
        vtrack <- list(name = name, src = src, func = func)
        if (!is.null(vtype)) vtrack$vtype <- vtype
        if (!is.null(expression)) vtrack$expression <- expression
        if (!is.null(sshift)) vtrack$sshift <- sshift
        if (!is.null(eshift)) vtrack$eshift <- eshift
        if (!is.null(dim)) vtrack$dim <- dim
    }

    # Store any extra parameters
    extra <- list(...)
    for (opt in names(extra)) {
        vtrack[[opt]] <- extra[[opt]]
    }

    # Check for duplicate vtrack names and replace if found
    existing_names <- vapply(
        browser$cfg$vtracks %||% list(),
        function(v) v$name,
        character(1)
    )
    idx <- match(name, existing_names)
    if (!is.na(idx)) {
        browser$cfg$vtracks[[idx]] <- vtrack
    } else {
        browser$cfg$vtracks <- c(browser$cfg$vtracks %||% list(), list(vtrack))
    }

    browser
}

#' Add a transform to a panel
#'
#' @param browser Browser object
#' @param panel_name Name of panel to add transform to
#' @param type Transform type: "smooth", "log2", "log10", "sqrt", "zscore", "minmax", "clip", "expr"
#' @param ... Transform-specific parameters
#' @return Updated browser object
#' @export
browser_add_transform <- function(browser, panel_name, type, ...) {
    panel <- get_panel(browser$cfg, panel_name)
    if (is.null(panel)) {
        cli::cli_abort("Panel '{panel_name}' not found")
    }

    transform <- list(type = type)
    params <- list(...)
    for (p in names(params)) {
        transform[[p]] <- params[[p]]
    }

    panel$transforms <- c(panel$transforms, list(transform))
    browser$cfg <- set_panel(browser$cfg, panel_name, panel)

    browser
}

#' Set tracks for a panel
#'
#' @param browser Browser object
#' @param panel_name Name of panel
#' @param tracks New track list
#' @return Updated browser object
#' @export
browser_set_tracks <- function(browser, panel_name, tracks) {
    panel <- get_panel(browser$cfg, panel_name)
    if (is.null(panel)) {
        cli::cli_abort("Panel '{panel_name}' not found")
    }

    # Auto-create vtracks for each track
    if (!is.null(tracks) && length(tracks) > 0) {
        existing_vtrack_names <- vapply(
            browser$cfg$vtracks %||% list(),
            function(v) v$name,
            character(1)
        )
        for (track in tracks) {
            # Auto-vtrack creation applies only to plain track-name strings
            if (!is.character(track) || length(track) != 1 || is.na(track) || !nzchar(track)) {
                next
            }
            # Skip if vtrack already exists with this name
            if (track %in% existing_vtrack_names) {
                next
            }
            vtrack <- list(
                name = track,
                src = track,
                func = "avg"
            )
            browser$cfg$vtracks <- c(browser$cfg$vtracks %||% list(), list(vtrack))
            existing_vtrack_names <- c(existing_vtrack_names, track)
        }
    }

    panel$tracks <- tracks
    browser$cfg <- set_panel(browser$cfg, panel_name, panel)

    browser
}

#' Set Y-axis limits for a panel
#'
#' @param browser Browser object
#' @param panel_name Name of panel
#' @param ylim Y-axis limits as c(min, max)
#' @return Updated browser object
#' @export
browser_set_ylim <- function(browser, panel_name, ylim) {
    panel <- get_panel(browser$cfg, panel_name)
    if (is.null(panel)) {
        cli::cli_abort("Panel '{panel_name}' not found")
    }

    panel$ylim <- ylim
    browser$cfg <- set_panel(browser$cfg, panel_name, panel)

    browser
}

#' Add vertical lines to the browser
#'
#' @param browser Browser object
#' @param name Vline set name
#' @param source Source type: "file", "intervals", "inline", "current"
#' @param file File path (for source = "file")
#' @param intervals Intervals name or list (for source = "intervals" or "inline")
#' @param color Line color
#' @param linetype Line type ("solid", "dashed", "dotted")
#' @param show_bounds Whether to show both start and end of intervals
#' @param enabled Whether vlines are enabled by default
#' @return Updated browser object
#' @export
browser_add_vlines <- function(browser, name,
                               source = "file",
                               file = NULL,
                               intervals = NULL,
                               color = "grey50",
                               linetype = "dashed",
                               show_bounds = TRUE,
                               enabled = TRUE) {
    vline <- list(
        name = name,
        source = source,
        color = color,
        linetype = linetype,
        show_bounds = show_bounds,
        enabled = enabled
    )

    if (!is.null(file)) vline$file <- file
    if (!is.null(intervals)) vline$intervals <- intervals

    vline <- validate_vline(vline, length(browser$cfg$vlines) + 1)
    browser$cfg$vlines <- c(browser$cfg$vlines, list(vline))
    browser$state$vlines_enabled <- c(browser$state$vlines_enabled, enabled)

    browser
}

#' Set current region
#'
#' @param browser Browser object
#' @param region Data frame with chrom, start, end or coordinate string
#' @return Updated browser object
#' @export
browser_set_region <- function(browser, region) {
    if (is.character(region)) {
        region <- parse_coords(region)
    }
    browser$state$current_region <- sanitize_interval(region)
    browser
}

#' Get current region
#'
#' @param browser Browser object
#' @return Current region as data frame
#' @export
browser_get_region <- function(browser) {
    browser$state$current_region
}
