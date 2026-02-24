# plot.R - Main plotting function for misha.browser

#' Plot the genome browser
#'
#' Renders all panels for the current or specified region.
#'
#' @param browser Browser object
#' @param region Optional region to plot (uses current if NULL)
#' @param gene Optional gene name to center on
#' @param span Optional span in bp (used with gene)
#' @param parallel Logical, enable parallel panel data extraction (default: from option).
#'   Requires `future` package and a parallel plan to be set
#'   (e.g., `future::plan(future::multisession)`).
#' @param profile Logical, enable profiling output (default: from option)
#' @return A patchwork plot object
#' @export
#' @examples
#' \dontrun{
#' browser <- browser_create(config = "config.yaml")
#' browser_plot(browser)
#' browser_plot(browser, gene = "Tbx5", span = 2e6)
#'
#' # Enable profiling
#' options(misha.browser.profile = TRUE)
#' browser_plot(browser)
#'
#' # Enable parallel extraction
#' future::plan(future::multisession, workers = 4)
#' browser_plot(browser, parallel = TRUE)
#' }
browser_plot <- function(browser, region = NULL, gene = NULL, span = NULL,
                         parallel = getOption("misha.browser.parallel", FALSE),
                         profile = getOption("misha.browser.profile", FALSE)) {
    timings <- list()
    t0 <- Sys.time()

    # Determine region to plot
    if (!is.null(gene)) {
        browser <- browser_goto_gene(browser, gene, span)
        region <- browser$state$current_region
    } else if (is.null(region)) {
        region <- browser$state$current_region
    } else if (is.character(region)) {
        region <- parse_coords(region)
    }

    region <- sanitize_interval(region)
    if (is.null(region)) {
        cli::cli_abort("No valid region to plot")
    }

    # Apply expansion if configured
    expansion <- browser$cfg$plot$expansion %||% 0
    if (expansion > 0) {
        region <- expand_region(region, expansion)
    }

    # Get vertical lines data
    t1 <- Sys.time()
    vlines_data <- get_all_vlines(browser, region)
    timings$vlines <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

    # Pre-extract data for data panels (optionally in parallel)
    pre_extracted <- NULL
    if (parallel) {
        t_extract <- Sys.time()
        pre_extracted <- extract_panels_parallel(browser, browser$cfg$panels, region)
        timings$parallel_extract <- as.numeric(difftime(Sys.time(), t_extract, units = "secs"))
    }

    # Render each panel
    plots <- list()
    heights <- numeric()
    panel_timings <- list()

    for (panel in browser$cfg$panels) {
        t_panel <- Sys.time()
        panel_name <- panel$name %||% "unnamed"

        # Get pre-extracted data for this panel if available
        panel_data <- if (!is.null(pre_extracted)) pre_extracted[[panel_name]] else NULL

        p <- render_panel(browser, panel, region, vlines_data, panel_data)
        panel_timings[[panel_name]] <- as.numeric(
            difftime(Sys.time(), t_panel, units = "secs")
        )
        if (!is.null(p)) {
            plots <- c(plots, list(p))
            heights <- c(heights, panel$height %||% 1)
        }
    }
    timings$panels <- panel_timings

    if (length(plots) == 0) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::labs(title = "No panels to display"))
    }

    # Add x-axis label to the last plot
    plots[[length(plots)]] <- add_x_axis(
        plots[[length(plots)]],
        region
    )

    # Combine with patchwork
    t2 <- Sys.time()
    combined <- patchwork::wrap_plots(plots, ncol = 1) +
        patchwork::plot_layout(heights = heights, guides = "collect") &
        ggplot2::theme(legend.position = "bottom")
    timings$combine <- as.numeric(difftime(Sys.time(), t2, units = "secs"))

    timings$total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    if (profile) {
        cli::cli_h3("Browser Plot Profiling")
        cli::cli_text("Total: {round(timings$total, 3)}s")
        cli::cli_text("Vlines: {round(timings$vlines, 3)}s")
        if (!is.null(timings$parallel_extract)) {
            cli::cli_text("Parallel Extract: {round(timings$parallel_extract, 3)}s")
        }
        cli::cli_text("Combine: {round(timings$combine, 3)}s")
        cli::cli_h3("Panel Timings")
        for (name in names(timings$panels)) {
            cli::cli_text("  {name}: {round(timings$panels[[name]], 3)}s")
        }
    }

    combined
}

#' Render a single panel
#'
#' @param browser Browser object
#' @param panel Panel configuration
#' @param region Viewing region
#' @param vlines_data Vertical lines data
#' @param pre_extracted_data Optional pre-extracted data for data panels
#' @return ggplot object
#' @keywords internal
render_panel <- function(browser, panel, region, vlines_data,
                         pre_extracted_data = NULL) {
    type <- panel$type %||% "data"
    panel$._cfg_colors <- browser$cfg$colors %||% list()

    p <- switch(type,
        "data" = render_data_panel(
            browser, panel, region, vlines_data,
            pre_extracted_data
        ),
        "annotation" = render_annotation_panel(panel, region, vlines_data),
        "ideogram" = render_ideogram_panel(panel, region),
        "intervals" = render_intervals_panel(panel, region, vlines_data),
        {
            cli::cli_warn("Unknown panel type: {type}")
            NULL
        }
    )

    highlight <- browser$state$highlight
    if (!is.null(p) && !is.null(highlight) && nrow(highlight) > 0) {
        p <- add_highlight_overlay(p, highlight)
    }

    p
}

#' Add X-axis to bottom panel
#'
#' @param p ggplot object
#' @param region Viewing region
#' @return Updated ggplot object
#' @keywords internal
add_x_axis <- function(p, region) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(p)
    }

    p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(),
        axis.text.x = ggplot2::element_text(),
        axis.ticks.x = ggplot2::element_line()
    ) + ggplot2::labs(
        x = paste0("Genomic Coordinate (", region$chrom, ")")
    )
}

#' Expand region by a given amount
#'
#' @param region Region data frame
#' @param expansion Expansion in bp (total, split evenly)
#' @return Expanded region
#' @keywords internal
expand_region <- function(region, expansion) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(NULL)
    }

    half_exp <- expansion / 2
    region$start <- region$start - half_exp
    region$end <- region$end + half_exp

    sanitize_interval(region)
}

#' Add highlight region to all panels
#'
#' @param browser Browser object
#' @param start Highlight start
#' @param end Highlight end
#' @return Updated browser object
#' @keywords internal
browser_set_highlight <- function(browser, start, end) {
    browser$state$highlight <- data.frame(start = start, end = end)
    browser
}

#' Clear highlight region
#'
#' @param browser Browser object
#' @return Updated browser object
#' @keywords internal
browser_clear_highlight <- function(browser) {
    browser$state$highlight <- NULL
    browser
}

#' Add highlight overlay to a panel
#'
#' @param p ggplot object
#' @param highlight Data frame with start/end
#' @return Updated ggplot object
#' @keywords internal
add_highlight_overlay <- function(p, highlight) {
    start <- highlight$start[1]
    end <- highlight$end[1]
    if (!is.finite(start) || !is.finite(end)) {
        return(p)
    }
    xmin <- min(start, end)
    xmax <- max(start, end)

    p + ggplot2::annotate(
        "rect",
        xmin = xmin, xmax = xmax,
        ymin = -Inf, ymax = Inf,
        fill = "gold", alpha = .HIGHLIGHT_ALPHA
    )
}
