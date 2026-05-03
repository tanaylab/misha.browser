# panels.R - Panel rendering for misha.browser

# Cache for genome-wide quantile computations (persists across renders)
.global_quantile_cache <- new.env(parent = emptyenv())

#' Render a data panel
#'
#' @param browser Browser object
#' @param panel Panel configuration
#' @param region Viewing region
#' @param vlines_data Vertical lines data
#' @param pre_extracted_data Optional pre-extracted data (for parallel mode)
#' @return ggplot object
#' @keywords internal
render_data_panel <- function(browser, panel, region, vlines_data = NULL,
                              pre_extracted_data = NULL) {
    profile <- getOption("misha.browser.profile", FALSE)
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void())
    }

    # Use pre-extracted data if provided, otherwise extract now
    if (!is.null(pre_extracted_data)) {
        data <- pre_extracted_data
        if (profile) {
            cli::cli_text("    [{panel$name}] using pre-extracted data, rows: {nrow(data) %||% 0}")
        }
    } else {
        # Extract and transform data
        t_extract <- Sys.time()
        data <- extract_panel_data(browser, panel, region)
        if (profile) {
            extract_time <- as.numeric(difftime(Sys.time(), t_extract, units = "secs"))
            cli::cli_text("    [{panel$name}] extract: {round(extract_time, 3)}s, rows: {nrow(data) %||% 0}")
        }
    }
    if (is.null(data) || nrow(data) == 0) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void())
    }

    x_limits <- c(region$start, region$end)

    # Get color mapping
    color_by <- panel$grouping$color_by %||% "track"
    colors <- get_panel_colors(panel, data, color_by, browser$cfg$colors)

    # Inject panel-name column for synthetic facet (Feature 1)
    show_name <- isTRUE(panel$show_name %||% TRUE)
    if (show_name && !is.null(panel$name) && nzchar(panel$name)) {
        data[["._panel_name"]] <- panel$name
    } else {
        show_name <- FALSE
    }

    # Base plot with data
    p <- ggplot2::ggplot(data, ggplot2::aes(x = pos, y = value))

    # Add vertical lines first (background)
    p <- add_vlines_to_plot(p, vlines_data, x_limits)

    # Add data layer based on plot type
    p <- add_data_layer(p, panel, color_by)

    # Apply faceting (Feature 1 panel-name strip + optional facet_by)
    has_facet_by <- !is.null(panel$facet_by) && panel$facet_by %in% names(data)
    has_panel_strip <- show_name && "._panel_name" %in% names(data)

    if (has_facet_by && has_panel_strip) {
        p <- p + ggplot2::facet_grid(
            stats::as.formula(paste(panel$facet_by, "~ ._panel_name")),
            scales = "free_y", switch = "y"
        )
    } else if (has_facet_by) {
        p <- p + ggplot2::facet_grid(
            stats::as.formula(paste(panel$facet_by, "~ .")),
            scales = "free_y"
        )
    } else if (has_panel_strip) {
        p <- p + ggplot2::facet_grid(
            ._panel_name ~ .,
            switch = "y"
        )
    }

    # Add horizontal lines (after faceting so stats apply per facet)
    p <- add_hlines_to_plot(p, panel, data, browser = browser)

    # Apply color scale only if color mapping is used (not for area plots with fixed colors)
    plot_type <- panel$plot_type %||% "line"
    uses_color_mapping <- plot_type %in% c("line", "point", "segment") ||
        (plot_type == "histogram" && is.null(panel$fill))

    if (uses_color_mapping && length(colors) > 0) {
        p <- p + ggplot2::scale_color_manual(values = colors, na.value = "grey50")
    }
    if (plot_type == "histogram" && is.null(panel$fill) && length(colors) > 0) {
        p <- p + ggplot2::scale_fill_manual(values = colors, na.value = "grey50")
    }

    # Apply axis settings
    p <- apply_panel_scales(p, panel, x_limits)

    # Apply theme
    p <- apply_panel_theme(p, panel)

    p
}

#' Add data layer to plot based on plot type
#'
#' @param p ggplot object
#' @param panel Panel configuration
#' @param color_by Column to map to color
#' @return Updated ggplot object
#' @keywords internal
add_data_layer <- function(p, panel, color_by) {
    plot_type <- panel$plot_type %||% "line"
    alpha <- panel$alpha %||% 0.8
    linewidth <- panel$linewidth %||% 0.7

    color_aes <- if (color_by %in% c("track", "source", "mark")) {
        ggplot2::aes(color = .data[[color_by]])
    } else {
        ggplot2::aes()
    }

    switch(plot_type,
        "line" = {
            p + ggplot2::geom_line(
                color_aes,
                linewidth = linewidth, alpha = alpha, na.rm = TRUE
            )
        },
        "area" = {
            fill_color <- panel$fill %||% panel$color %||% "grey50"
            line_color <- panel$color %||% "grey30"
            p + ggplot2::geom_area(
                fill = fill_color, alpha = alpha * 0.6, na.rm = TRUE
            ) + ggplot2::geom_line(
                color = line_color, linewidth = linewidth * 0.7, na.rm = TRUE
            )
        },
        "point" = {
            p + ggplot2::geom_point(
                color_aes,
                size = panel$size %||% 1, alpha = alpha, na.rm = TRUE
            )
        },
        "histogram" = {
            p + ggplot2::geom_col(
                color_aes,
                width = panel$bar_width %||% 1, alpha = alpha, na.rm = TRUE
            )
        },
        "segment" = {
            p + ggplot2::geom_segment(
                ggplot2::aes(
                    x = pos, xend = pos,
                    y = panel$baseline %||% 0, yend = value,
                    !!!color_aes
                ),
                linewidth = linewidth, alpha = alpha, na.rm = TRUE
            )
        },
        # Default: line
        p + ggplot2::geom_line(
            color_aes,
            linewidth = linewidth, alpha = alpha, na.rm = TRUE
        )
    )
}

#' Get colors for panel
#'
#' @param panel Panel configuration
#' @param data Data frame
#' @param color_by Column name to color by
#' @return Named vector of colors
#' @keywords internal
get_panel_colors <- function(panel, data, color_by, config_colors = NULL) {
    panel_colors <- panel$colors
    if (is.null(panel_colors)) panel_colors <- list()
    if (!is.list(panel_colors)) {
        panel_colors <- as.list(panel_colors)
    }
    if (is.null(config_colors)) config_colors <- list()
    if (!is.list(config_colors)) {
        config_colors <- as.list(config_colors)
    }
    config_colors <- c(config_colors, panel_colors)

    # Get unique values in data
    if (color_by %in% names(data)) {
        unique_vals <- unique(data[[color_by]])
    } else {
        unique_vals <- unique(data$track)
    }
    unique_vals <- unique_vals[!is.na(unique_vals)]

    # Build color vector
    colors <- character(length(unique_vals))
    names(colors) <- unique_vals

    default_color <- config_colors[["_default"]] %||% "grey50"

    for (val in unique_vals) {
        resolved <- lookup_color_value(val, config_colors)
        if (!is.null(resolved)) {
            colors[val] <- resolved
        } else {
            # Generate consistent color based on name
            colors[val] <- generate_color(val, default_color)
        }
    }

    colors
}

#' Generate a consistent color for a string
#'
#' Uses deterministic hash-based color generation without modifying
#' global random state (avoids set.seed side effects).
#'
#' @param name String to generate color for
#' @param default Default color if generation fails
#' @return Color string
#' @keywords internal
generate_color <- function(name, default = "grey50") {
    # Use xxhash32 for fast deterministic hash (no global state modification)
    hash <- digest::digest(name, algo = "xxhash32", serialize = FALSE)
    # Convert first 4 hex chars to hue value (0-360)
    h <- (strtoi(substr(hash, 1, 4), 16L) %% 360)
    grDevices::hcl(h = h, c = 70, l = 50)
}

#' Apply scales to panel
#'
#' @param p ggplot object
#' @param panel Panel configuration
#' @param x_limits X-axis limits
#' @return Updated ggplot object
#' @keywords internal
apply_panel_scales <- function(p, panel, x_limits) {
    # X scale
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0), limits = x_limits)

    # Y scale
    y_labels <- panel$y_labels %||% "numeric"
    label_func <- switch(y_labels,
        "percent" = scales::percent,
        "scientific" = scales::scientific,
        "comma" = scales::comma,
        scales::number
    )

    p <- p + ggplot2::scale_y_continuous(
        expand = c(0, 0),
        labels = label_func
    )

    # Coordinate system with no expansion; use coord_cartesian to clip without dropping data
    if (!is.null(panel$ylim)) {
        p <- p + ggplot2::coord_cartesian(
            xlim = x_limits,
            ylim = panel$ylim,
            expand = FALSE
        )
    } else {
        p <- p + ggplot2::coord_cartesian(xlim = x_limits, expand = FALSE)
    }

    p
}

#' Apply theme to panel
#'
#' @param p ggplot object
#' @param panel Panel configuration
#' @return Updated ggplot object
#' @keywords internal
apply_panel_theme <- function(p, panel) {
    p <- p + ggplot2::theme_bw()

    # Legend
    legend_pos <- if (panel$show_legend %||% TRUE) "bottom" else "none"

    # Y-axis title
    y_title <- panel$y_title %||% ""

    p <- p + ggplot2::theme(
        legend.position = legend_pos,
        strip.background = ggplot2::element_rect(fill = "grey95"),
        strip.text = ggplot2::element_text(size = 11, face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(b = 2, t = 2)
    ) + ggplot2::labs(y = y_title, color = NULL)

    p
}

#' Add vertical lines to a plot
#'
#' @param p ggplot object
#' @param vlines_data List of vline specifications
#' @param x_limits X-axis limits
#' @return Updated ggplot object
#' @keywords internal
add_vlines_to_plot <- function(p, vlines_data, x_limits) {
    if (is.null(vlines_data) || length(vlines_data) == 0) {
        return(p)
    }

    for (vline in vlines_data) {
        positions <- vline$positions
        # Filter to visible range
        positions <- positions[positions >= x_limits[1] & positions <= x_limits[2]]

        if (length(positions) > 0) {
            p <- p + ggplot2::geom_vline(
                xintercept = positions,
                linetype = vline$linetype %||% "dashed",
                color = vline$color %||% "grey50",
                linewidth = 0.5,
                na.rm = TRUE
            )
        }
    }

    p
}

#' Add horizontal lines to a plot
#'
#' @param p ggplot object
#' @param panel Panel configuration
#' @param data Data frame with values for stat calculations
#' @param browser Optional browser object (needed for quantile_global)
#' @return Updated ggplot object
#' @keywords internal
add_hlines_to_plot <- function(p, panel, data, browser = NULL) {
    hlines <- panel$hlines
    if (is.null(hlines) || length(hlines) == 0) {
        return(p)
    }

    for (hline in hlines) {
        # Calculate y-position
        if (!is.null(hline$y)) {
            y_pos <- hline$y
        } else if (!is.null(hline$stat)) {
            y_pos <- switch(hline$stat,
                "mean" = mean(data$value, na.rm = TRUE),
                "median" = median(data$value, na.rm = TRUE),
                "quantile" = quantile(data$value, hline$q, na.rm = TRUE),
                "quantile_global" = compute_global_quantile(browser, panel, hline)
            )
        } else {
            next # Skip if neither y nor stat specified
        }

        if (is.null(y_pos) || is.na(y_pos)) next

        # Add the line
        p <- p + ggplot2::geom_hline(
            yintercept = y_pos,
            color = hline$color,
            linetype = hline$linetype,
            linewidth = hline$linewidth
        )

        # Add label if specified (using annotate)
        if (!is.null(hline$label) && nchar(hline$label) > 0) {
            p <- p + ggplot2::annotate(
                "text",
                x = Inf, y = y_pos,
                label = hline$label,
                hjust = 1.1, vjust = -0.3,
                size = 3, color = hline$color
            )
        }
    }

    p
}

#' Compute a genome-wide quantile for a track
#'
#' Extracts the specified track across the full genome, applies the panel's
#' transforms, and computes the requested quantile. Results are cached so
#' the expensive extraction only happens once per track/transform/quantile
#' combination.
#'
#' @param browser Browser object
#' @param panel Panel configuration (used for transforms)
#' @param hline Hline configuration with `q` and optional `track` fields
#' @return Numeric quantile value, or NA on failure
#' @keywords internal
compute_global_quantile <- function(browser, panel, hline) {
    if (is.null(browser)) {
        cli::cli_warn("quantile_global requires browser object; falling back to NA")
        return(NA_real_)
    }

    q <- hline$q %||% 0.5
    track_name <- hline$track %||% panel$tracks[[1]]

    # Build cache key from track config + transforms + quantile
    transform_sig <- digest::digest(panel$transforms)
    cache_key <- paste0("gq_", track_name, "_", transform_sig, "_", q)

    if (exists(cache_key, envir = .global_quantile_cache)) {
        return(get(cache_key, envir = .global_quantile_cache))
    }

    cli::cli_text("Computing genome-wide quantile (q={q}) for track {track_name}...")

    result <- tryCatch(
        {
            # Find the vtrack config for this track
            vt_cfg <- NULL
            for (vt in browser$cfg$vtracks) {
                if (vt$name == track_name) {
                    vt_cfg <- vt
                    break
                }
            }

            # Resolve the track expression
            track_specs <- resolve_track_specs(track_name, browser, browser$cfg)
            track_expr <- track_specs$exprs
            col_name <- track_specs$names
            temp_vtracks <- track_specs$temp_vtracks
            on.exit(cleanup_temp_vtracks(temp_vtracks), add = TRUE)

            # Extract genome-wide
            iterator <- browser$cfg$plot$iterator %||% .DEFAULT_ITERATOR
            d <- extract_tracks(
                tracks = track_expr,
                region = misha::gintervals.all(),
                iterator = iterator,
                colnames = col_name
            )

            if (is.null(d) || nrow(d) == 0) {
                return(NA_real_)
            }

            # Apply transforms (same as panel, excluding smooth for speed)
            transforms <- panel$transforms %||% list()
            transforms <- Filter(function(t) t$type != "smooth", transforms)
            if (length(transforms) > 0) {
                d <- apply_transforms(d, transforms, col_name)
            }

            # Compute quantile on the track column
            vals <- d[[col_name]]
            vals <- vals[!is.na(vals)]

            if (length(vals) == 0) {
                return(NA_real_)
            }

            quantile(vals, q, names = FALSE)
        },
        error = function(e) {
            cli::cli_warn("Failed to compute global quantile for {track_name}: {e$message}")
            NA_real_
        }
    )

    # Cache the result
    assign(cache_key, result, envir = .global_quantile_cache)
    cli::cli_text("  -> q{q} = {round(result, 4)}")

    result
}
