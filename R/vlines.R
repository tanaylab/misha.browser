# vlines.R - Vertical line interval handling for misha.browser

#' Load vertical line intervals
#'
#' @param vline Vline configuration
#' @param region Current viewing region
#' @param data_dir Data directory for resolving relative paths
#' @return Data frame with positions to draw lines
#' @keywords internal
load_vline_intervals <- function(vline, region, data_dir = ".") {
    if (!vline$enabled) {
        return(NULL)
    }

    source <- vline$source %||% "file"
    region <- sanitize_interval(region)

    intervals <- switch(source,
        "file" = load_vline_file(vline, data_dir),
        "intervals" = load_vline_misha(vline),
        "inline" = load_vline_inline(vline),
        "current" = load_vline_current(region),
        NULL
    )

    if (is.null(intervals) || nrow(intervals) == 0) {
        return(NULL)
    }

    # Filter to current region
    if (!is.null(region)) {
        intervals <- intervals[
            intervals$chrom == region$chrom &
                intervals$end >= region$start &
                intervals$start <= region$end,
        ]
    }

    if (nrow(intervals) == 0) {
        return(NULL)
    }

    intervals
}

#' Load vlines from file
#'
#' @param vline Vline configuration
#' @param data_dir Data directory
#' @return Intervals data frame
#' @keywords internal
load_vline_file <- function(vline, data_dir = ".") {
    file_path <- vline$._resolved_file %||% resolve_path(data_dir, vline$file)
    if (is.null(file_path) || !file.exists(file_path)) {
        return(NULL)
    }

    with_cache(cache_key("vline_file", file_path), function() {
        tryCatch(
            {
                ext <- tolower(tools::file_ext(file_path))
                intervals <- if (ext == "bed") {
                    readr::read_tsv(file_path,
                        col_names = c("chrom", "start", "end"),
                        show_col_types = FALSE, col_types = "cnn"
                    )
                } else if (ext == "tsv") {
                    readr::read_tsv(file_path, show_col_types = FALSE)
                } else {
                    readr::read_csv(file_path, show_col_types = FALSE)
                }

                if (!is.null(intervals) && nrow(intervals) > 0) {
                    # Normalize column names
                    if ("chr" %in% names(intervals)) names(intervals)[names(intervals) == "chr"] <- "chrom"
                    if ("chromStart" %in% names(intervals)) names(intervals)[names(intervals) == "chromStart"] <- "start"
                    if ("chromEnd" %in% names(intervals)) names(intervals)[names(intervals) == "chromEnd"] <- "end"
                }
                intervals
            },
            error = function(e) {
                cli::cli_warn("Failed to load vline file '{file_path}': {e$message}")
                NULL
            }
        )
    })
}

#' Load vlines from misha intervals
#'
#' @param vline Vline configuration
#' @return Intervals data frame
#' @keywords internal
load_vline_misha <- function(vline) {
    intervals_name <- vline$intervals
    if (is.null(intervals_name)) {
        return(NULL)
    }

    with_cache(cache_key("vline_misha", intervals_name), function() {
        tryCatch(
            {
                if (intervals_exist(intervals_name)) {
                    misha::gintervals.load(intervals_name)
                } else {
                    NULL
                }
            },
            error = function(e) {
                cli::cli_warn("Failed to load misha intervals '{intervals_name}': {e$message}")
                NULL
            }
        )
    })
}

#' Load vlines from inline definition
#'
#' @param vline Vline configuration
#' @return Intervals data frame
#' @keywords internal
load_vline_inline <- function(vline) {
    intervals_list <- vline$intervals
    if (is.null(intervals_list) || length(intervals_list) == 0) {
        return(NULL)
    }

    dplyr::bind_rows(lapply(intervals_list, function(iv) {
        if (is.data.frame(iv)) {
            iv
        } else if (is.list(iv)) {
            data.frame(
                chrom = iv$chrom,
                start = iv$start,
                end = iv$end %||% iv$start,
                stringsAsFactors = FALSE
            )
        } else {
            NULL
        }
    }))
}

#' Load vlines from current region boundaries
#'
#' @param region Current viewing region
#' @return Intervals data frame
#' @keywords internal
load_vline_current <- function(region) {
    if (is.null(region)) {
        return(NULL)
    }

    data.frame(
        chrom = region$chrom,
        start = c(region$start, region$end),
        end = c(region$start, region$end),
        stringsAsFactors = FALSE
    )
}

#' Get vline positions for plotting
#'
#' @param vline Vline configuration
#' @param intervals Loaded intervals
#' @return Numeric vector of x-positions
#' @keywords internal
get_vline_positions <- function(vline, intervals) {
    if (is.null(intervals) || nrow(intervals) == 0) {
        return(numeric(0))
    }

    show_bounds <- vline$show_bounds %||% TRUE

    if (show_bounds) {
        c(intervals$start, intervals$end)
    } else {
        (intervals$start + intervals$end) / 2
    }
}

#' Get all vline data for plotting
#'
#' @param browser Browser object
#' @param region Current viewing region
#' @return List of vline specifications for plotting
#' @keywords internal
get_all_vlines <- function(browser, region) {
    vlines_cfg <- browser$cfg$vlines
    vlines_enabled <- browser$state$vlines_enabled

    if (length(vlines_cfg) == 0) {
        return(list())
    }

    result <- list()

    for (i in seq_along(vlines_cfg)) {
        vline <- vlines_cfg[[i]]

        # Check if enabled in state
        if (length(vlines_enabled) >= i && !vlines_enabled[i]) next

        intervals <- load_vline_intervals(
            vline,
            region,
            browser$cfg$._data_dir %||% "."
        )

        if (!is.null(intervals) && nrow(intervals) > 0) {
            positions <- get_vline_positions(vline, intervals)
            positions <- positions[is.finite(positions)]

            if (length(positions) > 0) {
                result[[vline$name]] <- list(
                    positions = unique(positions),
                    color = vline$color %||% "grey50",
                    linetype = vline$linetype %||% "dashed"
                )
            }
        }
    }

    result
}
