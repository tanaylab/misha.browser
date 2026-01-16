# shiny-server.R - Shiny server logic for misha.browser

#' Create browser server function
#'
#' @param browser Browser object
#' @return Shiny server function
#' @keywords internal
browser_server <- function(browser) {
    function(input, output, session) {
        # === Reactive Values ===
        browser_rv <- shiny::reactiveVal(browser)
        history <- shiny::reactiveValues(stack = list(), index = 0)
        active_tracks <- shiny::reactiveVal(get_default_tracks(browser))
        original_config <- browser$cfg

        config_editor <- config_editor_server("config_editor", browser_rv, original_config)

        # === Initialize ===
        shiny::observe({
            # Initialize history with current region
            init_region <- sanitize_interval(browser$state$current_region)
            if (!is.null(init_region)) {
                history$stack <- list(init_region)
                history$index <- 1
            }

            # Populate region selector
            regions <- load_navigator_regions(browser)
            if (!is.null(regions)) {
                label_field <- browser$cfg$navigator$label_field %||% "geneSymbol"
                choices <- prepare_region_choices(regions, label_field)
                shinyWidgets::updateVirtualSelect(
                    inputId = "region_select",
                    choices = choices,
                    session = session
                )
            }
        })

        # === Helper Functions ===
        region_key <- function(reg) {
            reg <- sanitize_interval(reg)
            if (is.null(reg)) {
                return(NA_character_)
            }
            sprintf("%s:%d-%d", reg$chrom, reg$start, reg$end)
        }

        set_region <- function(new_reg, push = TRUE) {
            new_reg <- sanitize_interval(new_reg)
            if (is.null(new_reg)) {
                return()
            }

            br <- browser_rv()
            cur <- sanitize_interval(br$state$current_region)

            if (push) {
                if (!is.null(cur) && identical(region_key(cur), region_key(new_reg))) {
                    # Same region, just update
                    br$state$current_region <- new_reg
                    browser_rv(br)
                    return()
                }

                # Truncate history if we're not at the end
                if (history$index < length(history$stack)) {
                    history$stack <- history$stack[seq_len(history$index)]
                }

                history$stack[[length(history$stack) + 1]] <- new_reg
                history$index <- length(history$stack)
            }

            br$state$current_region <- new_reg
            browser_rv(br)
        }

        # === Navigation Observers ===

        # Movement buttons
        shiny::observeEvent(input$mv_left_big, {
            br <- browser_rv()
            br <- browser_move_left(br, 0.5)
            set_region(br$state$current_region)
        })

        shiny::observeEvent(input$mv_left_small, {
            br <- browser_rv()
            br <- browser_move_left(br, 0.1)
            set_region(br$state$current_region)
        })

        shiny::observeEvent(input$mv_right_small, {
            br <- browser_rv()
            br <- browser_move_right(br, 0.1)
            set_region(br$state$current_region)
        })

        shiny::observeEvent(input$mv_right_big, {
            br <- browser_rv()
            br <- browser_move_right(br, 0.5)
            set_region(br$state$current_region)
        })

        # Zoom buttons
        shiny::observeEvent(input$zoom_in, {
            br <- browser_rv()
            br <- browser_zoom_in(br, 2)
            set_region(br$state$current_region)
        })

        shiny::observeEvent(input$zoom_out, {
            br <- browser_rv()
            br <- browser_zoom_out(br, 2)
            set_region(br$state$current_region)
        })

        # History navigation
        shiny::observeEvent(input$history_back, {
            if (history$index > 1) {
                history$index <- history$index - 1
                set_region(history$stack[[history$index]], push = FALSE)
            }
        })

        shiny::observeEvent(input$history_forward, {
            if (history$index < length(history$stack)) {
                history$index <- history$index + 1
                set_region(history$stack[[history$index]], push = FALSE)
            }
        })

        # Coordinate search
        shiny::observeEvent(input$coord_search_search, {
            shiny::req(input$coord_search)
            new_reg <- parse_coords(input$coord_search)
            if (!is.null(new_reg)) {
                set_region(new_reg)
            } else {
                shiny::showNotification("Invalid coordinates", type = "warning")
            }
        })

        # Region selector
        shiny::observeEvent(input$go_region, {
            shiny::req(input$region_select)
            new_reg <- parse_coords(input$region_select)
            if (!is.null(new_reg)) {
                extension <- browser$cfg$navigator$extension %||% 1e6
                new_reg <- expand_interval(new_reg, extension)
                set_region(new_reg)
            }
        })

        # Span control (numeric input)
        shiny::observeEvent(input$apply_span, {
            br <- browser_rv()
            region <- br$state$current_region
            new_span <- as.numeric(input$span_input)
            if (!is.null(region) && !is.na(new_span) && new_span >= 1000) {
                new_reg <- set_interval_width(region, new_span)
                set_region(new_reg)
            }
        })

        # Track selection
        shiny::observeEvent(input$apply_tracks, {
            tracks <- input$track_select
            if (length(tracks) < 1) {
                shiny::showNotification("No tracks selected.", type = "warning")
                return()
            }
            active_tracks(tracks)
        })

        # Smooth window (debounced to avoid excessive updates during typing)
        smooth_window_debounced <- shiny::debounce(
            shiny::reactive(input$smooth_window),
            millis = 500
        )

        shiny::observe({
            sw <- smooth_window_debounced()
            if (!is.null(sw) && is.finite(sw)) {
                br <- browser_rv()
                if (!identical(br$state$smooth_window, sw)) {
                    br$state$smooth_window <- sw
                    browser_rv(br)
                }
            }
        })

        # Extraction mode help
        shiny::observeEvent(input$extraction_help, {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Extraction modes and parameters",
                    easyClose = TRUE,
                    size = "l",
                    shiny::tagList(
                        shiny::tags$p(
                            "Extraction mode controls how misha.browser samples tracks and applies smoothing."
                        ),
                        shiny::tags$h4("Shared formulas"),
                        shiny::tags$ul(
                            shiny::tags$li(
                                shiny::tags$b("Span"),
                                ": span = end - start (bp)"
                            ),
                            shiny::tags$li(
                                shiny::tags$b("Dynamic iterator"),
                                ": iterator = max(base_iter, ceiling(span / target_points))"
                            ),
                            shiny::tags$li(
                                shiny::tags$b("Smooth slider (state)"),
                                ": smooth_window = input Smooth value (units: bins in fixed mode, bp in dynamic_smooth)"
                            ),
                            shiny::tags$li(
                                shiny::tags$b("Dynamic smooth window"),
                                ": smoothing_bp = smooth_window if set, else plot.smoothing_bp"
                            )
                        ),
                        shiny::tags$h4("fixed (step-by-step)"),
                        shiny::tags$ol(
                            shiny::tags$li(
                                "Set ",
                                shiny::tags$code("iterator = base_iter"),
                                " (from plot.iterator)."
                            ),
                            shiny::tags$li(
                                "Extract raw values with misha at that iterator (bin size)."
                            ),
                            shiny::tags$li(
                                "Apply panel transforms in order. If a ",
                                shiny::tags$code("smooth"),
                                " transform exists and Smooth is set, replace its window with ",
                                shiny::tags$code("smooth_window"),
                                " (in bins)."
                            ),
                            shiny::tags$li(
                                "Apply per-track transforms defined in YAML ",
                                shiny::tags$code("vtracks[].transforms"),
                                " (same rules as above)."
                            ),
                            shiny::tags$li(
                                "Effective smoothing in bp is ",
                                shiny::tags$code("smooth_window * iterator"),
                                " because the rolling window is measured in bins."
                            )
                        ),
                        shiny::tags$h4("dynamic (step-by-step)"),
                        shiny::tags$ol(
                            shiny::tags$li(
                                "Compute ",
                                shiny::tags$code("iterator = max(base_iter, ceiling(span / target_points))"),
                                "."
                            ),
                            shiny::tags$li(
                                "Extract raw values with misha at the dynamic iterator."
                            ),
                            shiny::tags$li(
                                "Remove any ",
                                shiny::tags$code("smooth"),
                                " transforms (panel + per-track YAML transforms) to avoid double-smoothing."
                            ),
                            shiny::tags$li(
                                "Apply remaining transforms in order."
                            )
                        ),
                        shiny::tags$h4("dynamic_smooth (step-by-step)"),
                        shiny::tags$ol(
                            shiny::tags$li(
                                "Compute ",
                                shiny::tags$code("iterator = max(base_iter, ceiling(span / target_points))"),
                                "."
                            ),
                            shiny::tags$li(
                                "Compute ",
                                shiny::tags$code("smoothing_bp"),
                                " and set vtrack iterator shifts ",
                                shiny::tags$code("sshift = -round(smoothing_bp/2)"),
                                ", ",
                                shiny::tags$code("eshift = round(smoothing_bp/2)"),
                                " for tracks in the panel."
                            ),
                            shiny::tags$li(
                                "Extract raw values with misha at the dynamic iterator and shifted vtracks."
                            ),
                            shiny::tags$li(
                                "Remove any ",
                                shiny::tags$code("smooth"),
                                " transforms (panel + per-track YAML transforms); apply remaining transforms."
                            )
                        )
                    ),
                    footer = shiny::modalButton("Close")
                )
            )
        })

        # Extraction mode settings (main window)
        shiny::observeEvent(
            list(
                input$extraction_mode,
                input$plot_iterator,
                input$plot_target_points,
                input$plot_smoothing_bp
            ),
            {
                br <- browser_rv()
                if (!is.null(input$extraction_mode)) {
                    br$cfg$plot$extraction_mode <- input$extraction_mode
                }
                if (!is.null(input$plot_iterator) && is.finite(input$plot_iterator)) {
                    br$cfg$plot$iterator <- input$plot_iterator
                }
                if (!is.null(input$plot_target_points) && is.finite(input$plot_target_points)) {
                    br$cfg$plot$target_points <- input$plot_target_points
                }
                if (!is.null(input$plot_smoothing_bp) && is.finite(input$plot_smoothing_bp)) {
                    br$cfg$plot$smoothing_bp <- input$plot_smoothing_bp
                }
                browser_rv(br)
            }
        )

        # Brushing
        shiny::observeEvent(input$plot_brush, {
            brush <- input$plot_brush
            br <- browser_rv()
            reg <- sanitize_interval(br$state$current_region)

            # Validate inputs
            if (is.null(reg) || is.null(brush$xmin) || is.null(brush$xmax)) {
                return()
            }
            if (!is.finite(brush$xmin) || !is.finite(brush$xmax)) {
                return()
            }

            new_start <- brush$xmin
            new_end <- brush$xmax

            # Ensure start < end (swap if needed)
            if (new_start > new_end) {
                tmp <- new_start
                new_start <- new_end
                new_end <- tmp
            }

            # Handle normalized coordinates (0-1)
            if (new_start >= 0 && new_end <= 1) {
                plot_start <- reg$start
                plot_end <- reg$end
                new_start <- plot_start + (plot_end - plot_start) * new_start
                new_end <- plot_start + (plot_end - plot_start) * new_end
            }

            # Validate bounds
            if (new_start < 0) {
                new_start <- 0
            }

            # Minimum region size (100bp)
            min_region_size <- 100
            if ((new_end - new_start) < min_region_size) {
                mid <- (new_start + new_end) / 2
                new_start <- mid - min_region_size / 2
                new_end <- mid + min_region_size / 2
            }

            if (input$brush_mode == "zoom") {
                new_reg <- sanitize_interval(data.frame(
                    chrom = reg$chrom,
                    start = as.integer(new_start),
                    end = as.integer(new_end)
                ))
                if (!is.null(new_reg)) {
                    set_region(new_reg)
                }
            } else {
                # Highlight mode
                br$state$highlight <- data.frame(start = new_start, end = new_end)
                browser_rv(br)
            }
        })

        # Clear highlight
        shiny::observeEvent(input$clear_highlight, {
            br <- browser_rv()
            br$state$highlight <- NULL
            browser_rv(br)
        })

        # Clear cache
        shiny::observeEvent(input$clear_cache, {
            browser_clear_cache()
            shiny::showNotification("Cache cleared", type = "message")
        })

        # Config editor
        shiny::observeEvent(input$open_config_editor, {
            config_editor$show_modal()
        })

        # Vlines toggles
        shiny::observe({
            br <- browser_rv()
            n_vlines <- length(br$cfg$vlines)
            if (n_vlines == 0) {
                return()
            }

            enabled <- sapply(seq_len(n_vlines), function(i) {
                val <- input[[paste0("vline_", i)]]
                if (is.null(val)) TRUE else val
            })

            br$state$vlines_enabled <- enabled
            browser_rv(br)
        })

        # === Outputs ===
        output$browser_title <- shiny::renderUI({
            br <- browser_rv()
            shiny::titlePanel(br$cfg$ui$title %||% "Genome Browser")
        })

        # Current location text
        output$current_loc_text <- shiny::renderText({
            br <- browser_rv()
            reg <- sanitize_interval(br$state$current_region)
            if (is.null(reg)) {
                return("Viewing: invalid region")
            }
            highlight <- br$state$highlight
            highlight_text <- "Highlight: none"
            if (!is.null(highlight) && nrow(highlight) > 0) {
                start <- suppressWarnings(as.numeric(highlight$start[1]))
                end <- suppressWarnings(as.numeric(highlight$end[1]))
                if (is.finite(start) && is.finite(end)) {
                    if (end < start) {
                        tmp <- start
                        start <- end
                        end <- tmp
                    }
                    highlight_text <- sprintf("Highlight: %s bp", scales::comma(max(0, end - start)))
                } else {
                    highlight_text <- "Highlight: invalid"
                }
            }
            sprintf(
                "Viewing: %s (Width: %s bp) | %s",
                format_coords(reg$chrom, reg$start, reg$end),
                scales::comma(get_span(reg)),
                highlight_text
            )
        })

        # Smoothing bp display
        output$smoothing_bp_text <- shiny::renderText({
            br <- browser_rv()
            reg <- sanitize_interval(br$state$current_region)
            extraction_mode <- input$extraction_mode %||% br$cfg$plot$extraction_mode %||% "fixed"
            smooth_window <- input$smooth_window %||% 10
            base_iter <- input$plot_iterator %||% br$cfg$plot$iterator %||% 32
            target_points <- input$plot_target_points %||% br$cfg$plot$target_points %||% 4000
            smoothing_bp <- input$plot_smoothing_bp %||% br$cfg$plot$smoothing_bp %||% 3200

            # Calculate effective iterator
            if (extraction_mode %in% c("dynamic", "dynamic_smooth") && !is.null(reg)) {
                span <- get_span(reg)
                iterator <- calc_iterator(span, base_iter, target_points)
            } else {
                iterator <- base_iter
            }

            # Build display string
            sampling_str <- sprintf("sampling=%s bp", scales::comma(iterator))

            smoothing_str <- if (extraction_mode == "dynamic") {
                "smoothing=dynamic"
            } else if (extraction_mode == "dynamic_smooth") {
                # In dynamic_smooth mode, smooth_window is used directly as bp
                sprintf("smoothing=%s bp", scales::comma(smooth_window))
            } else {
                # In fixed mode: effective bp = smooth_window * iterator
                effective_bp <- smooth_window * iterator
                sprintf("smoothing=%s bp", scales::comma(effective_bp))
            }

            parts <- c(
                sprintf("Extraction: %s", extraction_mode),
                sprintf("base_iter=%s bp", scales::comma(base_iter)),
                if (extraction_mode %in% c("dynamic", "dynamic_smooth")) {
                    sprintf("target_points=%s", scales::comma(target_points))
                } else {
                    NULL
                },
                if (extraction_mode == "dynamic_smooth") {
                    sprintf("smoothing_bp=%s bp", scales::comma(smoothing_bp))
                } else {
                    NULL
                },
                sampling_str,
                smoothing_str
            )
            paste(parts, collapse = " | ")
        })

        # Main plot
        output$main_plot <- shiny::renderPlot({
            shiny::withProgress(message = "Rendering plot", value = 0, {
                br <- browser_rv()
                region <- br$state$current_region
                tracks <- active_tracks()

                shiny::req(region)

                # Check region size
                span <- get_span(region)
                if (span > 10e6) {
                    shiny::showNotification("Region too large (>10Mb). Please zoom in.",
                        type = "error"
                    )
                    return(NULL)
                }

                shiny::incProgress(0.2, detail = "Preparing data")

                # Update browser with selected tracks
                if (length(tracks) > 0) {
                    for (i in seq_along(br$cfg$panels)) {
                        if (br$cfg$panels[[i]]$type == "data") {
                            br$cfg$panels[[i]]$tracks <- tracks
                            break
                        }
                    }
                }

                shiny::incProgress(0.6, detail = "Plotting panels")
                browser_plot(br, region = region)
            })
        })

        # URL state handling
        shiny::observe({
            query <- shiny::parseQueryString(session$clientData$url_search)
            if (!is.null(query$loc)) {
                new_reg <- parse_coords(query$loc)
                if (!is.null(new_reg)) {
                    set_region(new_reg)
                }
            }
        })
    }
}
