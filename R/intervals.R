# intervals.R - Generic intervals panel for misha.browser

#' Render intervals panel
#'
#' @param panel Panel configuration
#' @param region Viewing region
#' @param vlines_data Vertical lines data for overlay
#' @return ggplot object
#' @keywords internal
render_intervals_panel <- function(panel, region, vlines_data = NULL) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void())
    }

    intervals <- extract_intervals_data(panel, region)
    x_limits <- c(region$start, region$end)

    p <- ggplot2::ggplot()
    p <- add_vlines_to_plot(p, vlines_data, x_limits)

    if (!is.null(intervals) && nrow(intervals) > 0) {
        # Clip interval coordinates to the visible region so that intervals larger
        # than the view (e.g. HiC domains/TADs) are displayed as blocks spanning
        # the entire visible region. Without clipping, scale_x_continuous(limits)
        # censors out-of-bounds coords to NA, causing geom_rect to not draw.
        reg_start <- region$start
        reg_end <- region$end
        intervals$start <- pmax(intervals$start, reg_start)
        intervals$end <- pmin(intervals$end, reg_end)
        intervals <- intervals[order(intervals$start), ]
        intervals$y_level <- assign_interval_levels(intervals)

        color_by <- panel$color_by %||% ""
        panel_colors <- panel$colors %||% list()
        cfg_colors <- panel$._cfg_colors %||% list()

        if (nchar(color_by) > 0 && color_by %in% names(intervals)) {
            color_values <- unique(intervals[[color_by]])
            color_values <- color_values[!is.na(color_values)]
            color_map <- resolve_interval_colors(color_values, panel_colors, cfg_colors)
            p <- p + ggplot2::geom_rect(
                data = intervals,
                ggplot2::aes(
                    xmin = start, xmax = end,
                    ymin = y_level - 0.35, ymax = y_level + 0.35,
                    fill = .data[[color_by]]
                ),
                color = panel$outline_color %||% "grey20",
                linewidth = 0.2,
                na.rm = TRUE
            ) +
                ggplot2::scale_fill_manual(values = color_map, na.value = "grey60")
        } else {
            fill_color <- panel$color %||% "grey60"
            p <- p + ggplot2::geom_rect(
                data = intervals,
                ggplot2::aes(
                    xmin = start, xmax = end,
                    ymin = y_level - 0.35, ymax = y_level + 0.35
                ),
                fill = fill_color,
                color = panel$outline_color %||% "grey20",
                linewidth = 0.2,
                na.rm = TRUE
            )
        }

        label_field <- panel$label_field %||% ""
        if (nchar(label_field) > 0 && label_field %in% names(intervals) &&
            isTRUE(panel$show_labels %||% FALSE)) {
            intervals$label_x <- (intervals$start + intervals$end) / 2
            p <- p + ggplot2::geom_text(
                data = intervals,
                ggplot2::aes(x = label_x, y = y_level + 0.55, label = .data[[label_field]]),
                size = 2.8,
                color = panel$label_color %||% "black",
                na.rm = TRUE
            )
        }

        y_max <- max(intervals$y_level) + 0.8
        p <- p + ggplot2::scale_y_continuous(limits = c(0.5, y_max))
    }

    y_title <- panel$y_title %||% ""

    p <- p +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = x_limits) +
        ggplot2::coord_cartesian(xlim = x_limits, expand = FALSE) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.margin = ggplot2::margin(b = 2, t = 5),
            axis.title.y = if (nchar(y_title) > 0) ggplot2::element_text(size = 9, angle = 90) else ggplot2::element_blank()
        ) +
        ggplot2::labs(y = y_title)

    p
}

#' Extract intervals for the panel
#'
#' @param panel Panel configuration
#' @param region Viewing region
#' @return Data frame with intervals
#' @keywords internal
extract_intervals_data <- function(panel, region) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(NULL)
    }

    if (panel$source == "file") {
        # Use resolved path if available (relative paths resolved against data_dir)
        file_path <- panel$._resolved_file %||% panel$file
        if (is.null(file_path) || !file.exists(file_path)) {
            return(NULL)
        }
        intervals <- tryCatch(
            readr::read_tsv(file_path, show_col_types = FALSE),
            error = function(e) NULL
        )
    } else {
        intervals_name <- panel$intervals %||% panel$source
        if (is.null(intervals_name) || !intervals_exist(intervals_name)) {
            return(NULL)
        }
        intervals <- tryCatch(
            misha::gintervals.neighbors(
                intervals_name,
                region[, c("chrom", "start", "end")],
                maxdist = 0
            ),
            error = function(e) NULL
        )
    }

    if (is.null(intervals) || nrow(intervals) == 0) {
        return(NULL)
    }

    if (!"chrom" %in% names(intervals) && "chr" %in% names(intervals)) {
        intervals$chrom <- intervals$chr
    }
    if (!"start" %in% names(intervals) && "chromStart" %in% names(intervals)) {
        intervals$start <- intervals$chromStart
    }
    if (!"end" %in% names(intervals) && "chromEnd" %in% names(intervals)) {
        intervals$end <- intervals$chromEnd
    }

    if (!all(c("chrom", "start", "end") %in% names(intervals))) {
        return(NULL)
    }

    intervals <- intervals[intervals$chrom == region$chrom, ]
    if (nrow(intervals) == 0) {
        return(NULL)
    }

    # Keep only intervals that overlap the viewing region (start < reg_end & end > reg_start).
    # For intervals larger than the region (e.g. HiC domains), this ensures they are included.
    reg_start <- region$start
    reg_end <- region$end
    overlaps <- intervals$start < reg_end & intervals$end > reg_start
    intervals <- intervals[overlaps, , drop = FALSE]
    if (nrow(intervals) == 0) {
        return(NULL)
    }

    filter_field <- panel$filter_field %||% ""
    if (nchar(filter_field) > 0 && filter_field %in% names(intervals)) {
        values <- panel$filter_values %||% NULL
        regex <- panel$filter_regex %||% ""
        if (!is.null(values) && length(values) > 0) {
            intervals <- intervals[intervals[[filter_field]] %in% values, ]
        } else if (nchar(regex) > 0) {
            intervals <- intervals[grepl(regex, intervals[[filter_field]]), ]
        }
    }

    if (nrow(intervals) == 0) {
        return(NULL)
    }

    intervals
}

#' Assign Y-levels to intervals using sweep-line algorithm
#'
#' O(n log n) algorithm that tracks the rightmost end position per level,
#' avoiding the O(n²) cost of checking all previous intervals at each level.
#'
#' @param intervals Data frame with start/end
#' @return Integer vector of Y-levels
#' @keywords internal
assign_interval_levels <- function(intervals) {
    if (nrow(intervals) == 0) {
        return(integer(0))
    }
    if (nrow(intervals) == 1) {
        return(1L)
    }

    # Sort by start position and remember original order
    order_idx <- order(intervals$start)
    intervals_sorted <- intervals[order_idx, ]
    n <- nrow(intervals_sorted)
    y_levels <- integer(n)

    # Track the rightmost end position for each level (sweep-line approach)
    level_ends <- numeric(0)
    max_levels <- 20L # Safety limit

    for (i in seq_len(n)) {
        start_i <- intervals_sorted$start[i]
        end_i <- intervals_sorted$end[i]

        # Find first level where the rightmost end <= current start
        available_level <- 0L
        for (lvl in seq_along(level_ends)) {
            if (level_ends[lvl] <= start_i) {
                available_level <- lvl
                break
            }
        }

        if (available_level > 0L) {
            # Use existing level, update its rightmost end
            y_levels[i] <- available_level
            level_ends[available_level] <- end_i
        } else if (length(level_ends) < max_levels) {
            # Need a new level
            new_level <- length(level_ends) + 1L
            y_levels[i] <- new_level
            level_ends <- c(level_ends, end_i)
        } else {
            # Max levels reached, use last level
            y_levels[i] <- max_levels
            level_ends[max_levels] <- max(level_ends[max_levels], end_i)
        }
    }

    # Restore original order
    result <- integer(n)
    result[order_idx] <- y_levels
    result
}

#' Resolve colors for interval values
#'
#' @param values Character vector of values
#' @param panel_colors Panel-specific colors
#' @param cfg_colors Global config colors
#' @return Named color vector
#' @keywords internal
resolve_interval_colors <- function(values, panel_colors, cfg_colors) {
    panel_colors <- panel_colors %||% list()
    cfg_colors <- cfg_colors %||% list()

    if (!is.list(panel_colors)) panel_colors <- as.list(panel_colors)
    if (!is.list(cfg_colors)) cfg_colors <- as.list(cfg_colors)

    colors <- character(length(values))
    names(colors) <- values
    default_color <- cfg_colors[["_default"]] %||% panel_colors[["_default"]] %||% "grey50"

    for (val in values) {
        resolved <- lookup_color_value(val, cfg_colors)
        if (is.null(resolved)) {
            resolved <- lookup_color_value(val, panel_colors)
        }
        if (!is.null(resolved)) {
            colors[val] <- resolved
        } else {
            colors[val] <- generate_color(val, default_color)
        }
    }

    colors
}
