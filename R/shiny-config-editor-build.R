# shiny-config-editor-build.R - Config building from form inputs
#
# Contains build_config_from_inputs() which reconstructs a full config
# object from the Shiny form state, and expand_colors_with_tracks()
# which auto-generates color entries for tracks.

#' Build configuration from form inputs
#' @keywords internal
build_config_from_inputs <- function(input, vtracks, panels, colors, vlines, base_cfg) {
    cfg <- base_cfg

    # General settings
    cfg$ui$title <- input$gen_title
    cfg$ui$span_default <- input$gen_span_default
    cfg$ui$smooth_window_default <- input$gen_smooth_default
    cfg$ui$show_coordinates <- input$gen_show_coords

    # Start region
    if (!is.null(input$gen_start_gene) && nchar(input$gen_start_gene) > 0) {
        cfg$start$gene <- input$gen_start_gene
        cfg$start$coords <- NULL
    } else if (!is.null(input$gen_start_coords) && nchar(input$gen_start_coords) > 0) {
        parsed <- tryCatch(parse_coords(input$gen_start_coords), error = function(e) NULL)
        if (!is.null(parsed)) {
            cfg$start$coords <- list(
                chrom = parsed$chrom,
                start = parsed$start,
                end = parsed$end
            )
            cfg$start$gene <- NULL
        }
    }
    cfg$start$span_bp <- input$gen_span_bp

    # Plot settings
    cfg$plot$iterator <- input$plot_iterator
    cfg$plot$expansion <- input$plot_expansion
    cfg$plot$target_points <- input$plot_target_points
    cfg$plot$extraction_mode <- input$plot_extraction_mode
    cfg$plot$theme <- input$plot_theme

    # Navigator settings
    cfg$navigator$source <- input$nav_source
    cfg$navigator$label_field <- input$nav_label_field
    cfg$navigator$extension <- input$nav_extension

    # Vtracks - read from form inputs
    cfg$vtracks <- lapply(seq_along(vtracks), function(i) {
        vt <- vtracks[[i]] %||% list()
        vt$name <- input[[paste0("vt_name_", i)]] %||% vt$name
        vt$func <- input[[paste0("vt_func_", i)]] %||% vt$func
        vt$sshift <- input[[paste0("vt_sshift_", i)]] %||% vt$sshift
        vt$eshift <- input[[paste0("vt_eshift_", i)]] %||% vt$eshift
        expr_val <- input[[paste0("vt_expr_", i)]]
        # Default expression to name if empty
        vt$expression <- if (is.null(expr_val) || expr_val == "") vt$name else expr_val

        func <- vt$func

        # Handle source based on function type
        if (is_intervals_function(func)) {
            # Intervals source
            vt$src <- input[[paste0("vt_intervals_src_", i)]] %||% vt$src
        } else if (is_sequence_function(func)) {
            # No source for sequence functions
            vt$src <- NULL
        } else {
            # Regular track source
            vt$src <- input[[paste0("vt_src_", i)]] %||% vt$src
        }

        # Handle function-specific parameters
        if (is_pwm_function(func)) {
            # PWM parameters
            pssm <- input[[paste0("vt_pssm_", i)]]
            if (!is.null(pssm) && nchar(pssm) > 0) {
                vt$params <- list(
                    pssm = pssm,
                    bidirect = input[[paste0("vt_bidirect_", i)]] %||% TRUE,
                    prior = input[[paste0("vt_prior_", i)]] %||% 0.01,
                    extend = input[[paste0("vt_extend_", i)]] %||% TRUE
                )
                if (func == "pwm.count") {
                    vt$params$score_thresh <- input[[paste0("vt_score_thresh_", i)]] %||% 0
                }
            }
        } else if (is_kmer_function(func)) {
            # Kmer parameters
            kmer <- input[[paste0("vt_kmer_", i)]]
            if (!is.null(kmer) && nchar(kmer) > 0) {
                strand_str <- input[[paste0("vt_kmer_strand_", i)]] %||% "0"
                vt$params <- list(
                    kmer = kmer,
                    strand = as.integer(strand_str)
                )
            }
        } else if (func == "neighbor.count") {
            # Max distance parameter
            max_dist <- input[[paste0("vt_max_dist_", i)]]
            if (!is.null(max_dist) && is.finite(max_dist)) {
                vt$params <- max_dist
            }
        } else if (func == "quantile") {
            # Percentile parameter
            percentile <- input[[paste0("vt_quantile_", i)]]
            if (!is.null(percentile) && is.finite(percentile)) {
                vt$params <- percentile
            }
        }

        # Handle transforms
        vt_transforms <- vt$transforms %||% list()
        new_transforms <- list()

        for (j in seq_along(vt_transforms)) {
            type <- input[[paste0("vt_tr_type_", i, "_", j)]] %||% vt_transforms[[j]]$type
            if (is.null(type) || nchar(type) == 0) next

            tr <- list(type = type)
            if (type == "smooth") {
                tr$window <- input[[paste0("vt_tr_window_", i, "_", j)]] %||% vt_transforms[[j]]$window
                tr$align <- input[[paste0("vt_tr_align_", i, "_", j)]] %||% vt_transforms[[j]]$align
            } else if (type %in% c("log2", "log10")) {
                tr$offset <- input[[paste0("vt_tr_offset_", i, "_", j)]] %||% vt_transforms[[j]]$offset
            } else if (type == "clip") {
                tr$min <- input[[paste0("vt_tr_min_", i, "_", j)]] %||% vt_transforms[[j]]$min
                tr$max <- input[[paste0("vt_tr_max_", i, "_", j)]] %||% vt_transforms[[j]]$max
            } else if (type == "quantile") {
                probs_text <- input[[paste0("vt_tr_probs_", i, "_", j)]] %||% ""
                if (nchar(probs_text) > 0) {
                    probs <- as.numeric(strsplit(probs_text, "\\s*,\\s*")[[1]])
                    probs <- probs[is.finite(probs)]
                    if (length(probs) > 0) {
                        tr$probs <- probs
                    }
                }
            } else if (type == "expr") {
                tr$expr <- input[[paste0("vt_tr_expr_", i, "_", j)]] %||% vt_transforms[[j]]$expr
            }

            new_transforms <- c(new_transforms, list(tr))
        }

        vt$transforms <- if (length(new_transforms) > 0) new_transforms else NULL
        vt
    })

    # Panels - read from form inputs
    cfg$panels <- lapply(seq_along(panels), function(i) {
        panel <- panels[[i]] %||% list()
        panel_type <- input[[paste0("pnl_type_", i)]] %||% panel$type

        panel$name <- input[[paste0("pnl_name_", i)]] %||% panel$name
        panel$type <- panel_type
        panel$height <- input[[paste0("pnl_height_", i)]] %||% panel$height

        if (panel_type == "data") {
            panel$tracks <- as.list(input[[paste0("pnl_tracks_", i)]] %||% panel$tracks)
            panel$plot_type <- input[[paste0("pnl_plot_type_", i)]] %||% panel$plot_type
            panel$show_legend <- input[[paste0("pnl_legend_", i)]] %||% panel$show_legend

            ylim_min <- input[[paste0("pnl_ylim_min_", i)]]
            ylim_max <- input[[paste0("pnl_ylim_max_", i)]]
            if (!is.null(ylim_min) && !is.null(ylim_max) &&
                is.finite(ylim_min) && is.finite(ylim_max)) {
                panel$ylim <- c(ylim_min, ylim_max)
            } else {
                panel$ylim <- NULL
            }

            y_title <- input[[paste0("pnl_y_title_", i)]]
            if (!is.null(y_title) && nchar(y_title) > 0) {
                panel$y_title <- y_title
            }

            facet <- input[[paste0("pnl_facet_", i)]]
            if (!is.null(facet) && nchar(facet) > 0) {
                panel$facet_by <- facet
            }

            color_by <- input[[paste0("pnl_color_by_", i)]]
            pattern <- input[[paste0("pnl_pattern_", i)]]
            if ((!is.null(color_by) && nchar(color_by) > 0) ||
                (!is.null(pattern) && nchar(pattern) > 0)) {
                panel$grouping <- list()
                if (!is.null(color_by) && nchar(color_by) > 0) {
                    panel$grouping$color_by <- color_by
                }
                if (!is.null(pattern) && nchar(pattern) > 0) {
                    panel$grouping$pattern <- pattern
                }
            }

            # Rebuild hlines from form inputs
            hlines_count <- length(panel$hlines %||% list())
            if (hlines_count > 0) {
                new_hlines <- list()
                for (hl_idx in seq_len(hlines_count)) {
                    hl_type <- input[[paste0("hl_type_", i, "_", hl_idx)]]

                    new_hl <- list()
                    if (!is.null(hl_type)) {
                        if (hl_type == "y") {
                            new_hl$y <- input[[paste0("hl_y_", i, "_", hl_idx)]]
                        } else {
                            new_hl$stat <- hl_type
                            if (hl_type == "quantile") {
                                new_hl$q <- input[[paste0("hl_q_", i, "_", hl_idx)]]
                            }
                        }
                        new_hl$color <- input[[paste0("hl_color_", i, "_", hl_idx)]] %||% "grey50"
                        new_hl$linetype <- input[[paste0("hl_linetype_", i, "_", hl_idx)]] %||% "dashed"

                        label <- input[[paste0("hl_label_", i, "_", hl_idx)]]
                        if (!is.null(label) && nchar(label) > 0) {
                            new_hl$label <- label
                        }

                        new_hlines <- c(new_hlines, list(new_hl))
                    }
                }
                panel$hlines <- new_hlines
            } else {
                panel$hlines <- NULL
            }

            # Preserve transforms and colors from original
        } else if (panel_type == "annotation") {
            panel$exon_source <- input[[paste0("pnl_exon_src_", i)]] %||% panel$exon_source
            panel$tss_source <- input[[paste0("pnl_tss_src_", i)]] %||% panel$tss_source
            panel$gene_label_field <- input[[paste0("pnl_gene_label_", i)]] %||% panel$gene_label_field
        } else if (panel_type == "intervals") {
            source_type <- input[[paste0("pnl_intervals_source_", i)]] %||% panel$source %||% "intervals"
            ref_value <- input[[paste0("pnl_intervals_ref_", i)]] %||% panel$intervals %||% panel$file
            panel$source <- source_type
            if (source_type == "file") {
                panel$file <- ref_value
                panel$intervals <- NULL
            } else {
                panel$intervals <- ref_value
                panel$file <- NULL
            }

            panel$label_field <- input[[paste0("pnl_intervals_label_", i)]] %||% panel$label_field
            panel$color_by <- input[[paste0("pnl_intervals_color_by_", i)]] %||% panel$color_by
            panel$show_labels <- input[[paste0("pnl_intervals_labels_", i)]] %||% panel$show_labels
            panel$color <- input[[paste0("pnl_intervals_color_", i)]] %||% panel$color
            panel$outline_color <- input[[paste0("pnl_intervals_outline_", i)]] %||% panel$outline_color

            panel$filter_field <- input[[paste0("pnl_intervals_filter_field_", i)]] %||% panel$filter_field
            values_text <- input[[paste0("pnl_intervals_filter_values_", i)]] %||% ""
            if (nchar(values_text) > 0) {
                values <- trimws(strsplit(values_text, "\\s*,\\s*")[[1]])
                values <- values[nchar(values) > 0]
                panel$filter_values <- values
            } else {
                panel$filter_values <- NULL
            }
            panel$filter_regex <- input[[paste0("pnl_intervals_filter_regex_", i)]] %||% panel$filter_regex
        }

        panel
    })

    # Colors - read from form inputs
    auto_names <- attr(colors, "auto_names")
    if (is.null(auto_names)) {
        auto_names <- character(0)
    }
    auto_default <- colors[["_default"]] %||% "grey50"
    new_colors <- list()
    for (i in seq_along(colors)) {
        name <- input[[paste0("color_name_", i)]] %||% names(colors)[i]
        value <- input[[paste0("color_value_", i)]] %||% colors[[i]]
        if (!is.null(name) && nchar(name) > 0) {
            new_colors[[name]] <- value
        }
    }
    if (length(auto_names) > 0 && length(new_colors) > 0) {
        for (name in auto_names) {
            if (!is.null(new_colors[[name]])) {
                auto_color <- generate_color(name, auto_default)
                if (identical(new_colors[[name]], auto_color)) {
                    new_colors[[name]] <- NULL
                }
            }
        }
    }
    cfg$colors <- new_colors

    # Vlines - read from form inputs
    cfg$vlines <- lapply(seq_along(vlines), function(i) {
        vl <- vlines[[i]] %||% list()
        source_type <- input[[paste0("vl_source_", i)]] %||% vl$source
        vl$name <- input[[paste0("vl_name_", i)]] %||% vl$name
        vl$source <- source_type
        vl$color <- input[[paste0("vl_color_", i)]] %||% vl$color
        vl$linetype <- input[[paste0("vl_linetype_", i)]] %||% vl$linetype
        vl$show_bounds <- input[[paste0("vl_bounds_", i)]] %||% vl$show_bounds
        vl$enabled <- input[[paste0("vl_enabled_", i)]] %||% vl$enabled

        file_or_intervals <- input[[paste0("vl_file_", i)]] %||% vl$file %||% vl$intervals
        if (source_type == "file") {
            vl$file <- file_or_intervals
            vl$intervals <- NULL
            vl$misha <- NULL
        } else if (source_type %in% c("intervals", "misha")) {
            vl$intervals <- file_or_intervals
            vl$file <- NULL
        }

        vl
    })

    cfg
}

#' Expand colors list with track names for UI display
#' @keywords internal
expand_colors_with_tracks <- function(colors, panels) {
    base_colors <- colors %||% list()
    if (!is.list(base_colors)) {
        base_colors <- as.list(base_colors)
    }

    track_names <- character(0)
    if (!is.null(panels)) {
        for (panel in panels) {
            if (panel$type == "data" && !is.null(panel$tracks)) {
                track_names <- c(track_names, unlist(panel$tracks))
            }
        }
    }
    track_names <- unique(as.character(track_names))
    track_names <- track_names[!is.na(track_names) & nzchar(track_names)]

    known_names <- names(base_colors)
    if (is.null(known_names)) {
        known_names <- character(0)
    }
    auto_names <- setdiff(track_names, known_names)
    auto_default <- base_colors[["_default"]] %||% "grey50"
    for (name in auto_names) {
        base_colors[[name]] <- generate_color(name, auto_default)
    }
    attr(base_colors, "auto_names") <- auto_names

    base_colors
}
