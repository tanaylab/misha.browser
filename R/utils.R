# utils.R - Utility functions for misha.browser

#' Null coalescing operator
#'
#' Returns the first non-NULL value.
#'
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @name null-coalesce
#' @keywords internal
`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
}

#' Check if value is NULL or NA
#'
#' @param x Value to check
#' @return TRUE if x is NULL, NA, or empty string
#' @keywords internal
is_empty <- function(x) {
    if (is.null(x)) {
        return(TRUE)
    }
    if (length(x) == 0) {
        return(TRUE)
    }
    if (length(x) == 1 && is.na(x)) {
        return(TRUE)
    }
    if (is.character(x) && length(x) == 1 && identical(x, "")) {
        return(TRUE)
    }
    FALSE
}

#' Resolve a path relative to a base directory
#'
#' @param base_dir Base directory
#' @param path Path to resolve (absolute paths returned as-is)
#' @return Resolved absolute path
#' @keywords internal
resolve_path <- function(base_dir, path) {
    if (is_empty(path)) {
        return(NULL)
    }
    if (grepl("^[/~]", path)) {
        return(normalizePath(path, mustWork = FALSE))
    }
    normalizePath(file.path(base_dir, path), mustWork = FALSE)
}

#' Parse a coordinate string
#'
#' Parses strings like "chr1:1000-2000" or "chr1 1000 2000" into a data frame.
#'
#' @param text Coordinate string
#' @return Data frame with chrom, start, end columns, or NULL if parsing fails
#' @keywords internal
parse_coords <- function(text) {
    if (is_empty(text)) {
        return(NULL)
    }

    # Clean input
    text <- gsub(",", "", text)
    text <- trimws(text)

    # Parse chr:start-end or chr start end (spaces/tabs)
    parts <- unlist(strsplit(text, "[[:space:]:-]+"))
    parts <- parts[parts != ""]

    if (length(parts) < 3) {
        return(NULL)
    }

    chrom <- parts[1]
    if (!grepl("^chr", chrom)) chrom <- paste0("chr", chrom)

    start <- suppressWarnings(as.numeric(parts[2]))
    end <- suppressWarnings(as.numeric(parts[3]))

    if (is.na(start) || is.na(end)) {
        return(NULL)
    }

    data.frame(chrom = chrom, start = start, end = end, stringsAsFactors = FALSE)
}

#' Sanitize an interval
#'
#' Ensures interval has valid chrom, start, end and that start < end.
#'
#' @param interval Data frame with chrom, start, end
#' @return Sanitized interval or NULL if invalid
#' @keywords internal
sanitize_interval <- function(interval) {
    if (is.null(interval)) {
        return(NULL)
    }
    if (is.data.frame(interval)) {
        if (nrow(interval) < 1) {
            return(NULL)
        }
        interval <- interval[1, , drop = FALSE]
    }
    if (!all(c("chrom", "start", "end") %in% names(interval))) {
        return(NULL)
    }

    interval$start <- suppressWarnings(as.numeric(interval$start[1]))
    interval$end <- suppressWarnings(as.numeric(interval$end[1]))
    if (!is.finite(interval$start) || !is.finite(interval$end)) {
        return(NULL)
    }

    if (interval$end < interval$start) {
        tmp <- interval$start
        interval$start <- interval$end
        interval$end <- tmp
    }

    interval$start <- max(1, floor(interval$start))
    interval$end <- max(interval$start + 1, ceiling(interval$end))
    interval
}

#' Calculate dynamic iterator based on span
#'
#' Ensures approximately target_points data points regardless of view span.
#'
#' @param span Viewing span in bp
#' @param base_iter Base iterator from config
#' @param target_points Target number of points (default 4000)
#' @return Calculated iterator value
#' @keywords internal
calc_iterator <- function(span, base_iter = 32, target_points = 4000) {
    max(base_iter, ceiling(span / target_points))
}

#' Merge two lists recursively
#'
#' @param base Base list
#' @param override Override list (takes precedence)
#' @return Merged list
#' @noRd
merge_list <- function(base, override) {
    if (is.null(override)) {
        return(base)
    }
    if (is.null(base)) {
        return(override)
    }

    for (name in names(override)) {
        if (name %in% names(base) && is.list(base[[name]]) && is.list(override[[name]])) {
            base[[name]] <- merge_list(base[[name]], override[[name]])
        } else {
            base[[name]] <- override[[name]]
        }
    }
    base
}

#' Check if a misha track exists
#'
#' @param track Track name
#' @return TRUE if track exists
#' @keywords internal
track_exists <- function(track) {
    tryCatch(
        misha::gtrack.exists(track),
        error = function(e) FALSE
    )
}

#' Check if misha intervals exist
#'
#' @param intervals Intervals name
#' @return TRUE if intervals exist
#' @keywords internal
intervals_exist <- function(intervals) {
    tryCatch(
        misha::gintervals.exists(intervals),
        error = function(e) FALSE
    )
}

#' Format genomic coordinates for display
#'
#' @param chrom Chromosome

#' @param start Start position
#' @param end End position
#' @return Formatted string like "chr1:1,000,000-2,000,000"
#' @keywords internal
format_coords <- function(chrom, start, end) {
    sprintf(
        "%s:%s-%s",
        chrom,
        format(as.integer(start), big.mark = ",", scientific = FALSE),
        format(as.integer(end), big.mark = ",", scientific = FALSE)
    )
}

#' Get span width
#'
#' @param interval Interval data frame
#' @return Span in bp
#' @keywords internal
get_span <- function(interval) {
    interval <- sanitize_interval(interval)
    if (is.null(interval)) {
        return(0)
    }
    interval$end - interval$start
}

#' Get interval center
#'
#' @param interval Interval data frame with start and end
#' @return Center position (numeric)
#' @keywords internal
interval_center <- function(interval) {
    if (is.null(interval)) {
        return(NULL)
    }
    (interval$start + interval$end) / 2
}

#' Get interval width
#'
#' @param interval Interval data frame with start and end
#' @return Width in bp
#' @keywords internal
interval_width <- function(interval) {
    if (is.null(interval)) {
        return(0)
    }
    interval$end - interval$start
}

#' Create a sanitized region data frame
#'
#' @param chrom Chromosome
#' @param start Start position
#' @param end End position
#' @return Sanitized region data frame
#' @keywords internal
make_region <- function(chrom, start, end) {
    sanitize_interval(data.frame(
        chrom = chrom,
        start = start,
        end = end
    ))
}
