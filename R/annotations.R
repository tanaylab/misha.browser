# annotations.R - Gene annotation panel for misha.browser

#' Create annotation panel plot
#'
#' Renders gene models with exons, introns, and TSS arrows.
#'
#' @param panel Panel configuration
#' @param region Viewing region
#' @param vlines_data Vertical lines data for overlay
#' @return ggplot object
#' @keywords internal
render_annotation_panel <- function(panel, region, vlines_data = NULL) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void())
    }

    # Extract annotation data
    annot_data <- extract_annotation_data(panel, region)

    exons <- annot_data$exons
    tss <- annot_data$tss
    genes <- annot_data$genes

    x_limits <- c(region$start, region$end)
    label_field <- panel$gene_label_field %||% "geneSymbol"
    color <- panel$color %||% "navy"

    # Base plot
    p <- ggplot2::ggplot()

    # Add vertical lines first (background)
    p <- add_vlines_to_plot(p, vlines_data, x_limits)

    if (!is.null(exons) && nrow(exons) > 0 && !is.null(genes) && nrow(genes) > 0) {
        # Assign Y-levels to avoid overlap (greedy stacking)
        genes$y_level <- assign_gene_levels(genes)

        # Merge Y-levels to exons and tss
        exons_plot <- dplyr::left_join(
            exons,
            genes[, c(label_field, "y_level")],
            by = label_field
        )

        if (!is.null(tss) && nrow(tss) > 0 && label_field %in% names(tss)) {
            tss_plot <- dplyr::inner_join(
                tss,
                genes[, c(label_field, "y_level")],
                by = label_field
            )
        } else {
            tss_plot <- data.frame()
        }

        # Compute intron lines (full extent of each gene)
        introns_plot <- exons_plot |>
            dplyr::group_by(.data[[label_field]], y_level) |>
            dplyr::summarise(start = min(start), end = max(end), .groups = "drop")

        # Arrow length (2% of window)
        arrow_len <- (x_limits[2] - x_limits[1]) * 0.02

        # Add TSS arrows
        if (nrow(tss_plot) > 0 && "strand" %in% names(tss_plot)) {
            tss_plot <- tss_plot |>
                dplyr::mutate(
                    x_start = ifelse(strand == 1, start, end),
                    x_end = ifelse(strand == 1, start + arrow_len, end - arrow_len)
                ) |>
                dplyr::filter(is.finite(x_start), is.finite(x_end), is.finite(y_level))
        }

        # Draw intron lines
        p <- p + ggplot2::geom_segment(
            data = introns_plot,
            ggplot2::aes(x = start, xend = end, y = y_level, yend = y_level),
            color = color, linewidth = 0.5, na.rm = TRUE
        )

        # Draw exon rectangles
        p <- p + ggplot2::geom_rect(
            data = exons_plot,
            ggplot2::aes(xmin = start, xmax = end, ymin = y_level - 0.25, ymax = y_level + 0.25),
            fill = color, color = NA, na.rm = TRUE
        )

        # Draw TSS arrows
        if (nrow(tss_plot) > 0 && panel$show_strand_arrows %||% TRUE) {
            p <- p + ggplot2::geom_segment(
                data = tss_plot,
                ggplot2::aes(x = x_start, xend = x_end, y = y_level, yend = y_level),
                arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "inches"), type = "closed"),
                color = color, linewidth = 0.8, na.rm = TRUE
            )
        }

        # Gene labels
        p <- p + ggplot2::geom_text(
            data = genes,
            ggplot2::aes(
                x = (min_start + max_end) / 2, y = y_level + 0.45,
                label = .data[[label_field]]
            ),
            size = 3, fontface = "italic", color = "black", na.rm = TRUE
        )

        y_max <- max(genes$y_level) + 0.8

        p <- p + ggplot2::scale_y_continuous(limits = c(0.5, y_max))
    }

    # Apply coordinate system and theme
    p <- p +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = x_limits) +
        ggplot2::coord_cartesian(xlim = x_limits, expand = FALSE) +
        ggplot2::theme_void() +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(b = 2, t = 5)
        )

    p
}

#' Assign Y-levels to genes using sweep-line algorithm
#'
#' O(n log n) algorithm that tracks the rightmost end position per level,
#' avoiding the O(n²) cost of checking all previous genes at each level.
#'
#' @param genes Data frame with min_start, max_end columns
#' @return Integer vector of Y-levels
#' @keywords internal
assign_gene_levels <- function(genes) {
    if (nrow(genes) == 0) {
        return(integer(0))
    }
    if (nrow(genes) == 1) {
        return(1L)
    }

    # Sort by start position and remember original order
    order_idx <- order(genes$min_start)
    genes_sorted <- genes[order_idx, ]
    n <- nrow(genes_sorted)
    y_levels <- integer(n)

    # Track the rightmost end position for each level (sweep-line approach)
    # This avoids O(n) scan of all previous intervals at each level
    level_ends <- numeric(0)
    max_levels <- 20L # Safety limit

    for (i in seq_len(n)) {
        start_i <- genes_sorted$min_start[i]
        end_i <- genes_sorted$max_end[i]

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
