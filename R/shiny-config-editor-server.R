# shiny-config-editor-server.R - Config editor module server logic
#
# Contains config_editor_server() and all its internal observers/reactives
# for CRUD operations on vtracks, panels, colors, vlines, and uploads.

#' Config Editor Module Server
#'
#' @param id Module namespace ID
#' @param browser_rv Reactive value containing browser object
#' @param original_config Original configuration for reset
#' @return NULL (module server)
#' @keywords internal
config_editor_server <- function(id, browser_rv, original_config) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Draft configuration (working copy)
        draft_cfg <- shiny::reactiveVal(NULL)

        # Track list for vtracks, panels, colors, vlines (dynamic CRUD)
        draft_vtracks <- shiny::reactiveVal(list())
        draft_panels <- shiny::reactiveVal(list())
        draft_colors <- shiny::reactiveVal(list())
        draft_vlines <- shiny::reactiveVal(list())

        # Uploaded data (intervals, PSSMs)
        uploaded_intervals <- shiny::reactiveVal(list())
        uploaded_pssms <- shiny::reactiveVal(list())

        # Initialize draft when modal opens
        shiny::observeEvent(browser_rv(),
            {
                if (!is.null(browser_rv())) {
                    cfg <- browser_rv()$cfg
                    draft_cfg(cfg)
                    draft_vtracks(cfg$vtracks %||% list())
                    draft_panels(cfg$panels %||% list())
                    draft_colors(expand_colors_with_tracks(cfg$colors, cfg$panels))
                    draft_vlines(cfg$vlines %||% list())
                }
            },
            ignoreNULL = TRUE,
            once = FALSE
        )

        # ==================== VTRACK CRUD ====================

        output$vtrack_list <- shiny::renderUI({
            vtracks <- draft_vtracks()
            if (length(vtracks) == 0) {
                return(shiny::tags$p("No vtracks defined.", class = "text-muted"))
            }

            vtrack_items <- lapply(seq_along(vtracks), function(i) {
                vt <- vtracks[[i]]
                func <- vt$func %||% "sum"
                func_id <- ns(paste0("vt_func_", i))

                # Determine what source UI to show based on function
                source_ui <- if (is_intervals_function(func)) {
                    # Intervals source dropdown
                    intervals_choices <- build_intervals_choices(uploaded_intervals())
                    shiny::selectizeInput(
                        ns(paste0("vt_intervals_src_", i)),
                        "Intervals Source",
                        choices = intervals_choices,
                        selected = vt$src %||% "",
                        options = list(placeholder = "Select intervals...")
                    )
                } else if (is_sequence_function(func)) {
                    # No source needed for sequence functions
                    shiny::tags$p(
                        shiny::tags$em("No source required (uses genome sequence)"),
                        class = "text-muted small",
                        style = "margin-top: 30px;"
                    )
                } else {
                    # Regular track source
                    shiny::textInput(
                        ns(paste0("vt_src_", i)),
                        "Source Track",
                        value = vt$src %||% ""
                    )
                }

                # Function-specific parameters
                func_params_ui <- if (is_pwm_function(func)) {
                    # PWM parameters
                    pssm_choices <- build_pssm_choices(uploaded_pssms())
                    params <- vt$params %||% list()
                    shiny::tagList(
                        shiny::fluidRow(
                            shiny::column(
                                4,
                                shiny::selectizeInput(
                                    ns(paste0("vt_pssm_", i)),
                                    "PSSM",
                                    choices = pssm_choices,
                                    selected = params$pssm %||% "",
                                    options = list(placeholder = "Select PSSM...")
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::checkboxInput(
                                    ns(paste0("vt_bidirect_", i)),
                                    "Bidirectional",
                                    value = params$bidirect %||% TRUE
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::numericInput(
                                    ns(paste0("vt_prior_", i)),
                                    "Prior",
                                    value = params$prior %||% 0.01,
                                    min = 0, max = 1, step = 0.01
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::checkboxInput(
                                    ns(paste0("vt_extend_", i)),
                                    "Extend",
                                    value = params$extend %||% TRUE
                                )
                            ),
                            if (func == "pwm.count") {
                                shiny::column(
                                    2,
                                    shiny::numericInput(
                                        ns(paste0("vt_score_thresh_", i)),
                                        "Score Thresh",
                                        value = params$score.thresh %||% params$score_thresh %||% 0,
                                        step = 0.1
                                    )
                                )
                            }
                        )
                    )
                } else if (is_kmer_function(func)) {
                    # Kmer parameters
                    params <- vt$params %||% list()
                    shiny::fluidRow(
                        shiny::column(
                            4,
                            shiny::textInput(
                                ns(paste0("vt_kmer_", i)),
                                "K-mer Sequence",
                                value = params$kmer %||% "",
                                placeholder = "e.g., CG, ATCG"
                            )
                        ),
                        shiny::column(
                            3,
                            shiny::selectInput(
                                ns(paste0("vt_kmer_strand_", i)),
                                "Strand",
                                choices = c("Both" = "0", "Forward" = "1", "Reverse" = "-1"),
                                selected = as.character(params$strand %||% 0)
                            )
                        )
                    )
                } else if (func == "neighbor.count") {
                    # neighbor.count max distance
                    params <- vt$params
                    max_dist <- if (is.numeric(params)) params else 0
                    shiny::fluidRow(
                        shiny::column(
                            4,
                            shiny::numericInput(
                                ns(paste0("vt_max_dist_", i)),
                                "Max Distance",
                                value = max_dist,
                                min = 0, step = 1000
                            )
                        )
                    )
                } else if (func == "quantile") {
                    # quantile percentile
                    params <- vt$params
                    percentile <- if (is.numeric(params)) params else 0.5
                    shiny::fluidRow(
                        shiny::column(
                            4,
                            shiny::numericInput(
                                ns(paste0("vt_quantile_", i)),
                                "Percentile",
                                value = percentile,
                                min = 0, max = 1, step = 0.01
                            )
                        )
                    )
                } else {
                    NULL
                }

                shiny::div(
                    class = "card mb-2",
                    shiny::div(
                        class = "card-body py-2",
                        # Row 1: Name, Function, sshift, eshift, Delete
                        shiny::fluidRow(
                            shiny::column(
                                3,
                                shiny::textInput(
                                    ns(paste0("vt_name_", i)),
                                    "Name",
                                    value = vt$name %||% ""
                                )
                            ),
                            shiny::column(
                                3,
                                shiny::selectInput(
                                    ns(paste0("vt_func_", i)),
                                    "Function",
                                    choices = get_vtrack_function_choices(),
                                    selected = func
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::numericInput(
                                    ns(paste0("vt_sshift_", i)),
                                    "sshift",
                                    value = vt$sshift %||% 0
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::numericInput(
                                    ns(paste0("vt_eshift_", i)),
                                    "eshift",
                                    value = vt$eshift %||% 0
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::div(
                                    style = "margin-top: 25px;",
                                    shiny::actionButton(
                                        ns(paste0("delete_vt_", i)),
                                        "",
                                        icon = shiny::icon("trash"),
                                        class = "btn-outline-danger btn-sm"
                                    )
                                )
                            )
                        ),
                        # Row 2: Source (conditional based on function type)
                        shiny::fluidRow(
                            shiny::column(6, source_ui)
                        ),
                        # Row 3: Function-specific parameters (if any)
                        func_params_ui,
                        # Row 4: Expression wrapper
                        shiny::fluidRow(
                            shiny::column(
                                8,
                                shiny::textInput(
                                    ns(paste0("vt_expr_", i)),
                                    "Expression Wrapper",
                                    value = vt$expression %||% "",
                                    placeholder = "e.g., pmax(vtrack_name, 0)"
                                )
                            )
                        )
                    )
                )
            })

            transforms_section <- lapply(seq_along(vtracks), function(i) {
                vt <- vtracks[[i]]
                vt_transforms <- vt$transforms %||% list()

                transform_rows <- lapply(seq_along(vt_transforms), function(j) {
                    tr <- vt_transforms[[j]] %||% list()
                    type_id <- ns(paste0("vt_tr_type_", i, "_", j))

                    shiny::div(
                        class = "card mb-2",
                        shiny::div(
                            class = "card-body py-2",
                            shiny::fluidRow(
                                shiny::column(
                                    3,
                                    shiny::selectInput(
                                        type_id,
                                        "Type",
                                        choices = c(
                                            "smooth", "log2", "log10",
                                            "sqrt", "zscore", "minmax",
                                            "clip", "quantile", "expr"
                                        ),
                                        selected = tr$type %||% "smooth"
                                    )
                                ),
                                shiny::column(
                                    2,
                                    shiny::actionButton(
                                        ns(paste0("delete_vt_tr_", i, "_", j)),
                                        "",
                                        icon = shiny::icon("trash"),
                                        class = "btn-outline-danger btn-sm"
                                    )
                                )
                            ),
                            shiny::conditionalPanel(
                                condition = sprintf("input['%s'] == 'smooth'", type_id),
                                shiny::fluidRow(
                                    shiny::column(
                                        3,
                                        shiny::numericInput(
                                            ns(paste0("vt_tr_window_", i, "_", j)),
                                            "Window",
                                            value = tr$window %||% 10,
                                            min = 1, step = 1
                                        )
                                    ),
                                    shiny::column(
                                        3,
                                        shiny::selectInput(
                                            ns(paste0("vt_tr_align_", i, "_", j)),
                                            "Align",
                                            choices = c("center", "left", "right"),
                                            selected = tr$align %||% "center"
                                        )
                                    )
                                )
                            ),
                            shiny::conditionalPanel(
                                condition = sprintf("input['%s'] == 'log2' || input['%s'] == 'log10'", type_id, type_id),
                                shiny::fluidRow(
                                    shiny::column(
                                        3,
                                        shiny::numericInput(
                                            ns(paste0("vt_tr_offset_", i, "_", j)),
                                            "Offset",
                                            value = tr$offset %||% 1,
                                            step = 0.1
                                        )
                                    )
                                )
                            ),
                            shiny::conditionalPanel(
                                condition = sprintf("input['%s'] == 'clip'", type_id),
                                shiny::fluidRow(
                                    shiny::column(
                                        3,
                                        shiny::numericInput(
                                            ns(paste0("vt_tr_min_", i, "_", j)),
                                            "Min",
                                            value = tr$min %||% NA,
                                            step = 0.1
                                        )
                                    ),
                                    shiny::column(
                                        3,
                                        shiny::numericInput(
                                            ns(paste0("vt_tr_max_", i, "_", j)),
                                            "Max",
                                            value = tr$max %||% NA,
                                            step = 0.1
                                        )
                                    )
                                )
                            ),
                            shiny::conditionalPanel(
                                condition = sprintf("input['%s'] == 'quantile'", type_id),
                                shiny::fluidRow(
                                    shiny::column(
                                        6,
                                        shiny::textInput(
                                            ns(paste0("vt_tr_probs_", i, "_", j)),
                                            "Probs (comma-separated)",
                                            value = {
                                                probs <- tr$probs %||% c(0.01, 0.99)
                                                paste(probs, collapse = ", ")
                                            }
                                        )
                                    )
                                )
                            ),
                            shiny::conditionalPanel(
                                condition = sprintf("input['%s'] == 'expr'", type_id),
                                shiny::fluidRow(
                                    shiny::column(
                                        12,
                                        shiny::textInput(
                                            ns(paste0("vt_tr_expr_", i, "_", j)),
                                            "Expression (use x, pos)",
                                            value = tr$expr %||% ""
                                        )
                                    )
                                )
                            )
                        )
                    )
                })

                shiny::div(
                    class = "mb-3",
                    shiny::tags$label("Transforms", class = "control-label"),
                    shiny::actionButton(
                        ns(paste0("add_vt_tr_", i)),
                        "Add Transform",
                        class = "btn-outline-primary btn-sm",
                        icon = shiny::icon("plus")
                    ),
                    shiny::div(style = "margin-top: 8px;", shiny::tagList(transform_rows))
                )
            })

            shiny::tagList(
                unlist(
                    lapply(seq_along(vtrack_items), function(i) {
                        item <- list(vtrack_items[[i]], transforms_section[[i]])
                        if (i < length(vtrack_items)) {
                            item <- c(item, list(shiny::hr()))
                        }
                        item
                    }),
                    recursive = FALSE
                )
            )
        })

        # Add vtrack
        shiny::observeEvent(input$add_vtrack, {
            vtracks <- draft_vtracks()
            new_vt <- list(
                name = paste0("new_vtrack_", length(vtracks) + 1),
                src = "",
                func = "sum"
            )
            draft_vtracks(c(vtracks, list(new_vt)))
        })

        # Delete vtrack observers (dynamic)
        # Track observer handles to prevent memory leaks
        delete_vt_observers <- shiny::reactiveVal(list())

        shiny::observe({
            vtracks <- draft_vtracks()

            # Destroy old observers (use isolate to avoid reactive dependency loop)
            old_obs <- shiny::isolate(delete_vt_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(vtracks), function(i) {
                shiny::observeEvent(input[[paste0("delete_vt_", i)]],
                    {
                        current <- draft_vtracks()
                        if (i <= length(current)) {
                            draft_vtracks(current[-i])
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            delete_vt_observers(new_obs)
        })

        # Function change observers - update vtrack to trigger UI refresh
        # Track observer handles to prevent memory leaks
        func_change_observers <- shiny::reactiveVal(list())

        shiny::observe({
            vtracks <- draft_vtracks()

            # Destroy old observers (use isolate to avoid reactive dependency loop)
            old_obs <- shiny::isolate(func_change_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(vtracks), function(i) {
                shiny::observeEvent(input[[paste0("vt_func_", i)]],
                    {
                        new_func <- input[[paste0("vt_func_", i)]]
                        current <- draft_vtracks()
                        if (i <= length(current) && !is.null(new_func)) {
                            old_func <- current[[i]]$func %||% "sum"
                            # Only update if function type category changed
                            old_type <- if (is_intervals_function(old_func)) "intervals" else if (is_sequence_function(old_func)) "sequence" else "standard"
                            new_type <- if (is_intervals_function(new_func)) "intervals" else if (is_sequence_function(new_func)) "sequence" else "standard"
                            if (old_type != new_type) {
                                # Capture current input values before refresh
                                current[[i]]$name <- input[[paste0("vt_name_", i)]] %||% current[[i]]$name
                                current[[i]]$sshift <- input[[paste0("vt_sshift_", i)]] %||% current[[i]]$sshift
                                current[[i]]$eshift <- input[[paste0("vt_eshift_", i)]] %||% current[[i]]$eshift
                                current[[i]]$expression <- input[[paste0("vt_expr_", i)]] %||% current[[i]]$expression
                                # Clear source when changing type (different input field)
                                current[[i]]$src <- ""
                            }
                            current[[i]]$func <- new_func
                            draft_vtracks(current)
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            func_change_observers(new_obs)
        })

        # Add vtrack transform
        # Track observer handles to prevent memory leaks
        add_vt_tr_observers <- shiny::reactiveVal(list())

        shiny::observe({
            vtracks <- draft_vtracks()

            # Destroy old observers
            old_obs <- shiny::isolate(add_vt_tr_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(vtracks), function(i) {
                shiny::observeEvent(input[[paste0("add_vt_tr_", i)]],
                    {
                        current <- draft_vtracks()
                        if (i <= length(current)) {
                            vt <- current[[i]]
                            vt$transforms <- c(vt$transforms %||% list(), list(list(type = "smooth", window = 10)))
                            current[[i]] <- vt
                            draft_vtracks(current)
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            add_vt_tr_observers(new_obs)
        })

        # Delete vtrack transform
        # Track observer handles to prevent memory leaks
        delete_vt_tr_observers <- shiny::reactiveVal(list())

        shiny::observe({
            vtracks <- draft_vtracks()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_vt_tr_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers (nested: for each vtrack, for each transform)
            new_obs <- list()
            for (i in seq_along(vtracks)) {
                vt <- vtracks[[i]]
                transforms <- vt$transforms %||% list()
                for (j in seq_along(transforms)) {
                    obs <- shiny::observeEvent(input[[paste0("delete_vt_tr_", i, "_", j)]],
                        {
                            # Capture i and j in closure
                            local_i <- i
                            local_j <- j
                            current <- draft_vtracks()
                            if (local_i <= length(current)) {
                                cur_vt <- current[[local_i]]
                                cur_transforms <- cur_vt$transforms %||% list()
                                if (local_j <= length(cur_transforms)) {
                                    cur_transforms <- cur_transforms[-local_j]
                                    cur_vt$transforms <- cur_transforms
                                    current[[local_i]] <- cur_vt
                                    draft_vtracks(current)
                                }
                            }
                        },
                        ignoreInit = TRUE
                    )
                    new_obs <- c(new_obs, list(obs))
                }
            }
            delete_vt_tr_observers(new_obs)
        })

        # ==================== PANEL CRUD ====================

        output$panel_accordion <- shiny::renderUI({
            panels <- draft_panels()
            if (length(panels) == 0) {
                return(shiny::tags$p("No panels defined.", class = "text-muted"))
            }

            # Get available tracks (vtracks + misha tracks from panels)
            available_tracks <- get_available_tracks(draft_vtracks(), panels)

            accordion_items <- lapply(seq_along(panels), function(i) {
                panel <- panels[[i]]
                shiny::tags$details(
                    open = i == 1,
                    shiny::tags$summary(
                        paste0(panel$name %||% paste("Panel", i), " [", panel$type %||% "data", "]")
                    ),
                    create_panel_form(ns, panel, i, available_tracks)
                )
            })

            shiny::div(
                class = ns("panel-accordion"),
                shiny::tagList(
                    unlist(
                        lapply(seq_along(accordion_items), function(i) {
                            item <- list(accordion_items[[i]])
                            if (i < length(accordion_items)) {
                                item <- c(item, list(shiny::hr()))
                            }
                            item
                        }),
                        recursive = FALSE
                    )
                )
            )
        })

        # Add panel
        shiny::observeEvent(input$add_panel, {
            panels <- draft_panels()
            new_panel <- list(
                name = paste0("new_panel_", length(panels) + 1),
                type = "data",
                tracks = list(),
                plot_type = "line",
                height = .DEFAULT_DATA_PANEL_HEIGHT
            )
            draft_panels(c(panels, list(new_panel)))
        })

        # Delete panel observers (dynamic)
        # Track observer handles to prevent memory leaks
        delete_pnl_observers <- shiny::reactiveVal(list())

        shiny::observe({
            panels <- draft_panels()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_pnl_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(panels), function(i) {
                shiny::observeEvent(input[[paste0("delete_pnl_", i)]],
                    {
                        current <- draft_panels()
                        if (i <= length(current)) {
                            draft_panels(current[-i])
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            delete_pnl_observers(new_obs)
        })

        # ==================== COLORS CRUD ====================

        output$color_list <- shiny::renderUI({
            colors <- draft_colors()
            if (length(colors) == 0) {
                return(shiny::tags$p("No colors defined.", class = "text-muted"))
            }

            color_items <- lapply(seq_along(colors), function(i) {
                color_name <- names(colors)[i]
                color_value <- colors[[i]]

                shiny::fluidRow(
                    class = "mb-2",
                    shiny::column(
                        5,
                        shiny::textInput(
                            ns(paste0("color_name_", i)),
                            NULL,
                            value = color_name,
                            placeholder = "Color name"
                        )
                    ),
                    shiny::column(
                        5,
                        colourpicker::colourInput(
                            ns(paste0("color_value_", i)),
                            NULL,
                            value = color_value %||% "#000000",
                            showColour = "both"
                        )
                    ),
                    shiny::column(
                        2,
                        shiny::actionButton(
                            ns(paste0("delete_color_", i)),
                            "",
                            icon = shiny::icon("trash"),
                            class = "btn-outline-danger btn-sm"
                        )
                    )
                )
            })

            shiny::tagList(color_items)
        })

        # Add color
        shiny::observeEvent(input$add_color, {
            colors <- draft_colors()
            new_name <- paste0("color_", length(colors) + 1)
            colors[[new_name]] <- "#808080"
            draft_colors(colors)
        })

        # Delete color observers (dynamic)
        # Track observer handles to prevent memory leaks
        delete_color_observers <- shiny::reactiveVal(list())

        shiny::observe({
            colors <- draft_colors()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_color_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(colors), function(i) {
                shiny::observeEvent(input[[paste0("delete_color_", i)]],
                    {
                        current <- draft_colors()
                        if (i <= length(current)) {
                            current[i] <- NULL
                            draft_colors(current)
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            delete_color_observers(new_obs)
        })

        # ==================== VLINES CRUD ====================

        output$vline_list <- shiny::renderUI({
            vlines <- draft_vlines()
            if (length(vlines) == 0) {
                return(shiny::tags$p("No vlines defined.", class = "text-muted"))
            }

            vline_items <- lapply(seq_along(vlines), function(i) {
                vl <- vlines[[i]]
                shiny::div(
                    class = "card mb-2",
                    shiny::div(
                        class = "card-body py-2",
                        shiny::fluidRow(
                            shiny::column(
                                3,
                                shiny::textInput(
                                    ns(paste0("vl_name_", i)),
                                    "Name",
                                    value = vl$name %||% ""
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::selectInput(
                                    ns(paste0("vl_source_", i)),
                                    "Source",
                                    choices = c("file", "intervals", "inline", "current", "misha"),
                                    selected = vl$source %||% "file"
                                )
                            ),
                            shiny::column(
                                4,
                                shiny::textInput(
                                    ns(paste0("vl_file_", i)),
                                    "File/Intervals",
                                    value = vl$file %||% vl$intervals %||% ""
                                )
                            ),
                            shiny::column(
                                3,
                                colourpicker::colourInput(
                                    ns(paste0("vl_color_", i)),
                                    "Color",
                                    value = vl$color %||% "grey50"
                                )
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                3,
                                shiny::selectInput(
                                    ns(paste0("vl_linetype_", i)),
                                    "Linetype",
                                    choices = c("solid", "dashed", "dotted", "longdash", "twodash"),
                                    selected = vl$linetype %||% "dashed"
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::checkboxInput(
                                    ns(paste0("vl_bounds_", i)),
                                    "Show Bounds",
                                    value = vl$show_bounds %||% TRUE
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::checkboxInput(
                                    ns(paste0("vl_enabled_", i)),
                                    "Enabled",
                                    value = vl$enabled %||% TRUE
                                )
                            ),
                            shiny::column(
                                5,
                                shiny::div(
                                    style = "margin-top: 25px;",
                                    shiny::actionButton(
                                        ns(paste0("delete_vl_", i)),
                                        "",
                                        icon = shiny::icon("trash"),
                                        class = "btn-outline-danger btn-sm"
                                    )
                                )
                            )
                        )
                    )
                )
            })

            shiny::tagList(vline_items)
        })

        # Add vline
        shiny::observeEvent(input$add_vline, {
            vlines <- draft_vlines()
            new_vl <- list(
                name = paste0("vline_", length(vlines) + 1),
                source = "file",
                file = "",
                color = "grey50",
                linetype = "dashed",
                enabled = TRUE
            )
            draft_vlines(c(vlines, list(new_vl)))
        })

        # Delete vline observers (dynamic)
        # Track observer handles to prevent memory leaks
        delete_vl_observers <- shiny::reactiveVal(list())

        shiny::observe({
            vlines <- draft_vlines()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_vl_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(vlines), function(i) {
                shiny::observeEvent(input[[paste0("delete_vl_", i)]],
                    {
                        current <- draft_vlines()
                        if (i <= length(current)) {
                            draft_vlines(current[-i])
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            delete_vl_observers(new_obs)
        })

        # ==================== UPLOADS CRUD ====================

        # Render uploaded intervals list
        output$uploaded_intervals_list <- shiny::renderUI({
            intervals <- uploaded_intervals()
            if (length(intervals) == 0) {
                return(shiny::tags$p("No intervals uploaded.", class = "text-muted"))
            }

            interval_items <- lapply(names(intervals), function(name) {
                item <- intervals[[name]]
                shiny::div(
                    class = "card mb-2",
                    shiny::div(
                        class = "card-body py-2",
                        shiny::fluidRow(
                            shiny::column(
                                4,
                                shiny::tags$strong(name)
                            ),
                            shiny::column(
                                3,
                                shiny::tags$span(item$filename, class = "text-muted")
                            ),
                            shiny::column(
                                3,
                                shiny::tags$span(
                                    paste(item$rows, "rows"),
                                    class = "text-muted"
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::actionButton(
                                    ns(paste0("delete_intervals_", name)),
                                    "",
                                    icon = shiny::icon("trash"),
                                    class = "btn-outline-danger btn-sm"
                                )
                            )
                        )
                    )
                )
            })

            shiny::tagList(interval_items)
        })

        # Render uploaded PSSMs list
        output$uploaded_pssms_list <- shiny::renderUI({
            pssms <- uploaded_pssms()
            if (length(pssms) == 0) {
                return(shiny::tags$p("No PSSMs uploaded.", class = "text-muted"))
            }

            pssm_items <- lapply(names(pssms), function(name) {
                item <- pssms[[name]]
                shiny::div(
                    class = "card mb-2",
                    shiny::div(
                        class = "card-body py-2",
                        shiny::fluidRow(
                            shiny::column(
                                4,
                                shiny::tags$strong(name)
                            ),
                            shiny::column(
                                3,
                                shiny::tags$span(item$filename, class = "text-muted")
                            ),
                            shiny::column(
                                3,
                                shiny::tags$span(item$dimensions, class = "text-muted")
                            ),
                            shiny::column(
                                2,
                                shiny::actionButton(
                                    ns(paste0("delete_pssm_", name)),
                                    "",
                                    icon = shiny::icon("trash"),
                                    class = "btn-outline-danger btn-sm"
                                )
                            )
                        )
                    )
                )
            })

            shiny::tagList(pssm_items)
        })

        # Add intervals - show modal
        shiny::observeEvent(input$add_intervals, {
            shiny::showModal(shiny::modalDialog(
                title = "Add Intervals",
                size = "m",
                shiny::textInput(
                    ns("new_intervals_name"),
                    "Name (used to reference in vtracks)",
                    placeholder = "e.g., my_peaks"
                ),
                shiny::radioButtons(
                    ns("intervals_source_type"),
                    "Source",
                    choices = c(
                        "Upload File" = "upload",
                        "File Path" = "path",
                        "Intervals Set" = "set"
                    ),
                    selected = "upload"
                ),
                shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'upload'", ns("intervals_source_type")),
                    shiny::fileInput(
                        ns("intervals_file"),
                        "Choose File",
                        accept = c(".bed", ".tsv", ".txt", ".csv")
                    ),
                    shiny::helpText("Formats: BED, TSV (chrom, start, end)")
                ),
                shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'path'", ns("intervals_source_type")),
                    shiny::textInput(
                        ns("intervals_path"),
                        "File Path",
                        placeholder = "/path/to/intervals.tsv"
                    )
                ),
                shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'set'", ns("intervals_source_type")),
                    shiny::selectizeInput(
                        ns("intervals_set"),
                        "Intervals Set",
                        choices = tryCatch(
                            misha::gintervals.ls(),
                            error = function(e) character(0)
                        ),
                        options = list(placeholder = "Select intervals set...")
                    )
                ),
                footer = shiny::tagList(
                    shiny::actionButton(ns("cancel_add_intervals"), "Cancel"),
                    shiny::actionButton(ns("confirm_add_intervals"), "Add",
                        class = "btn-primary"
                    )
                )
            ))
        })

        # Cancel add intervals - return to config editor
        shiny::observeEvent(input$cancel_add_intervals, {
            shiny::removeModal()
            shiny::showModal(create_config_modal(ns, draft_cfg()))
        })

        # Confirm add intervals
        shiny::observeEvent(input$confirm_add_intervals, {
            name <- input$new_intervals_name
            source_type <- input$intervals_source_type

            # Validate name
            if (is.null(name) || name == "") {
                shiny::showNotification("Please enter a name", type = "error")
                return()
            }

            # Check if name already exists
            if (name %in% names(uploaded_intervals())) {
                shiny::showNotification("An intervals set with this name already exists",
                    type = "error"
                )
                return()
            }

            # Parse intervals based on source type
            intervals_data <- tryCatch(
                {
                    if (source_type == "upload") {
                        file_info <- input$intervals_file
                        if (is.null(file_info)) {
                            stop("Please select a file")
                        }
                        list(
                            data = parse_intervals_file(file_info$datapath),
                            filename = file_info$name
                        )
                    } else if (source_type == "path") {
                        path <- input$intervals_path
                        if (is.null(path) || path == "") {
                            stop("Please enter a file path")
                        }
                        list(
                            data = parse_intervals_file(path),
                            filename = basename(path)
                        )
                    } else if (source_type == "set") {
                        set_name <- input$intervals_set
                        if (is.null(set_name) || set_name == "") {
                            stop("Please select an intervals set")
                        }
                        list(
                            data = misha::gintervals.load(set_name),
                            filename = set_name
                        )
                    }
                },
                error = function(e) {
                    shiny::showNotification(
                        paste("Error loading intervals:", e$message),
                        type = "error"
                    )
                    NULL
                }
            )

            if (is.null(intervals_data)) {
                return()
            }

            # Store in reactive value
            current <- uploaded_intervals()
            current[[name]] <- list(
                data = intervals_data$data,
                filename = intervals_data$filename,
                rows = nrow(intervals_data$data)
            )
            uploaded_intervals(current)

            # Also store in global upload storage for use in vtracks
            store_uploaded_intervals(name, intervals_data$data, intervals_data$filename)

            shiny::removeModal()
            shiny::showNotification(
                paste("Intervals '", name, "' added successfully"),
                type = "message"
            )

            # Re-show config editor modal
            shiny::showModal(create_config_modal(ns, draft_cfg()))
        })

        # Add PSSM - show modal
        shiny::observeEvent(input$add_pssm, {
            # Get prego motif choices (cached)
            prego_choices <- get_prego_motif_choices()

            shiny::showModal(shiny::modalDialog(
                title = "Add PSSM",
                size = "m",
                shiny::textInput(
                    ns("new_pssm_name"),
                    "Name (used to reference in vtracks)",
                    placeholder = "e.g., my_motif"
                ),
                shiny::radioButtons(
                    ns("pssm_source_type"),
                    "Source",
                    choices = c(
                        "Prego Motif" = "prego",
                        "Upload File" = "upload",
                        "File Path" = "path"
                    ),
                    selected = "prego"
                ),
                shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'prego'", ns("pssm_source_type")),
                    shiny::selectizeInput(
                        ns("prego_motif"),
                        "Prego Motif",
                        choices = prego_choices,
                        options = list(
                            placeholder = "Search motifs...",
                            maxOptions = 100
                        )
                    )
                ),
                shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'upload'", ns("pssm_source_type")),
                    shiny::fileInput(
                        ns("pssm_file"),
                        "Choose File",
                        accept = c(".tsv", ".txt", ".csv", ".meme", ".jaspar")
                    ),
                    shiny::helpText("Formats: TSV (A,C,G,T cols), MEME, JASPAR")
                ),
                shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'path'", ns("pssm_source_type")),
                    shiny::textInput(
                        ns("pssm_path"),
                        "File Path",
                        placeholder = "/path/to/motif.pssm"
                    )
                ),
                footer = shiny::tagList(
                    shiny::actionButton(ns("cancel_add_pssm"), "Cancel"),
                    shiny::actionButton(ns("confirm_add_pssm"), "Add",
                        class = "btn-primary"
                    )
                )
            ))
        })

        # Cancel add PSSM - return to config editor
        shiny::observeEvent(input$cancel_add_pssm, {
            shiny::removeModal()
            shiny::showModal(create_config_modal(ns, draft_cfg()))
        })

        # Confirm add PSSM
        shiny::observeEvent(input$confirm_add_pssm, {
            name <- input$new_pssm_name
            source_type <- input$pssm_source_type

            # Validate name
            if (is.null(name) || name == "") {
                shiny::showNotification("Please enter a name", type = "error")
                return()
            }

            # Check if name already exists
            if (name %in% names(uploaded_pssms())) {
                shiny::showNotification("A PSSM with this name already exists",
                    type = "error"
                )
                return()
            }

            # Parse PSSM based on source type
            pssm_data <- tryCatch(
                {
                    if (source_type == "prego") {
                        motif_name <- input$prego_motif
                        if (is.null(motif_name) || motif_name == "") {
                            stop("Please select a motif")
                        }
                        list(
                            data = get_prego_pssm(motif_name),
                            filename = motif_name
                        )
                    } else if (source_type == "upload") {
                        file_info <- input$pssm_file
                        if (is.null(file_info)) {
                            stop("Please select a file")
                        }
                        list(
                            data = parse_pssm_file(file_info$datapath),
                            filename = file_info$name
                        )
                    } else if (source_type == "path") {
                        path <- input$pssm_path
                        if (is.null(path) || path == "") {
                            stop("Please enter a file path")
                        }
                        list(
                            data = parse_pssm_file(path),
                            filename = basename(path)
                        )
                    }
                },
                error = function(e) {
                    shiny::showNotification(
                        paste("Error loading PSSM:", e$message),
                        type = "error"
                    )
                    NULL
                }
            )

            if (is.null(pssm_data)) {
                return()
            }

            # Store in reactive value
            current <- uploaded_pssms()
            current[[name]] <- list(
                data = pssm_data$data,
                filename = pssm_data$filename,
                dimensions = paste0(nrow(pssm_data$data), "x", ncol(pssm_data$data))
            )
            uploaded_pssms(current)

            # Also store in global upload storage for use in vtracks
            store_uploaded_pssm(name, pssm_data$data, pssm_data$filename)

            shiny::removeModal()
            shiny::showNotification(
                paste("PSSM '", name, "' added successfully"),
                type = "message"
            )

            # Re-show config editor modal
            shiny::showModal(create_config_modal(ns, draft_cfg()))
        })

        # Delete intervals observers (dynamic)
        # Track observer handles to prevent memory leaks
        delete_intervals_observers <- shiny::reactiveVal(list())

        shiny::observe({
            intervals <- uploaded_intervals()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_intervals_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(names(intervals), function(name) {
                shiny::observeEvent(input[[paste0("delete_intervals_", name)]],
                    {
                        current <- uploaded_intervals()
                        current[[name]] <- NULL
                        uploaded_intervals(current)
                        delete_uploaded_intervals(name)
                    },
                    ignoreInit = TRUE
                )
            })
            delete_intervals_observers(new_obs)
        })

        # Delete PSSM observers (dynamic)
        # Track observer handles to prevent memory leaks
        delete_pssm_observers <- shiny::reactiveVal(list())

        shiny::observe({
            pssms <- uploaded_pssms()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_pssm_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(names(pssms), function(name) {
                shiny::observeEvent(input[[paste0("delete_pssm_", name)]],
                    {
                        current <- uploaded_pssms()
                        current[[name]] <- NULL
                        uploaded_pssms(current)
                        delete_uploaded_pssm(name)
                    },
                    ignoreInit = TRUE
                )
            })
            delete_pssm_observers(new_obs)
        })

        # ==================== PANEL HLINES ====================

        # Render hlines for each panel
        shiny::observe({
            panels <- draft_panels()
            lapply(seq_along(panels), function(panel_idx) {
                current_panels <- draft_panels()
                if (panel_idx > length(current_panels)) {
                    return(NULL)
                }

                output[[paste0("hline_list_", panel_idx)]] <- shiny::renderUI({
                    panel <- current_panels[[panel_idx]]
                    hlines <- panel$hlines %||% list()

                    if (length(hlines) == 0) {
                        return(shiny::tags$p("No horizontal lines.", class = "text-muted small"))
                    }

                    hline_items <- lapply(seq_along(hlines), function(i) {
                        hl <- hlines[[i]]

                        shiny::div(
                            class = "card mb-2",
                            shiny::div(
                                class = "card-body py-2",
                                shiny::fluidRow(
                                    shiny::column(
                                        3,
                                        shiny::selectInput(
                                            ns(paste0("hl_type_", panel_idx, "_", i)),
                                            "Type",
                                            choices = c(
                                                "Fixed" = "y", "Mean" = "mean",
                                                "Median" = "median", "Quantile" = "quantile"
                                            ),
                                            selected = if (!is.null(hl$y)) "y" else (hl$stat %||% "mean")
                                        )
                                    ),
                                    shiny::column(
                                        2,
                                        shiny::uiOutput(ns(paste0("hl_value_", panel_idx, "_", i)))
                                    ),
                                    shiny::column(
                                        3,
                                        colourpicker::colourInput(
                                            ns(paste0("hl_color_", panel_idx, "_", i)),
                                            "Color",
                                            value = hl$color %||% "grey50"
                                        )
                                    ),
                                    shiny::column(
                                        2,
                                        shiny::selectInput(
                                            ns(paste0("hl_linetype_", panel_idx, "_", i)),
                                            "Line",
                                            choices = c("solid", "dashed", "dotted", "longdash"),
                                            selected = hl$linetype %||% "dashed"
                                        )
                                    ),
                                    shiny::column(
                                        2,
                                        shiny::div(
                                            style = "margin-top: 25px;",
                                            shiny::actionButton(
                                                ns(paste0("delete_hline_", panel_idx, "_", i)),
                                                "",
                                                icon = shiny::icon("trash"),
                                                class = "btn-outline-danger btn-sm"
                                            )
                                        )
                                    )
                                ),
                                shiny::fluidRow(
                                    shiny::column(
                                        12,
                                        shiny::textInput(
                                            ns(paste0("hl_label_", panel_idx, "_", i)),
                                            "Label (optional)",
                                            value = hl$label %||% ""
                                        )
                                    )
                                )
                            )
                        )
                    })

                    shiny::tagList(hline_items)
                })

                # Render dynamic value inputs for each hline
                panel <- current_panels[[panel_idx]]
                hlines <- panel$hlines %||% list()
                lapply(seq_along(hlines), function(i) {
                    output[[paste0("hl_value_", panel_idx, "_", i)]] <- shiny::renderUI({
                        hl_type <- input[[paste0("hl_type_", panel_idx, "_", i)]]
                        hl <- hlines[[i]]

                        if (is.null(hl_type)) {
                            hl_type <- if (!is.null(hl$y)) "y" else (hl$stat %||% "mean")
                        }

                        if (hl_type == "y") {
                            shiny::numericInput(
                                ns(paste0("hl_y_", panel_idx, "_", i)),
                                "Y",
                                value = input[[paste0("hl_y_", panel_idx, "_", i)]] %||% hl$y %||% 0
                            )
                        } else if (hl_type == "quantile") {
                            shiny::numericInput(
                                ns(paste0("hl_q_", panel_idx, "_", i)),
                                "Q",
                                value = input[[paste0("hl_q_", panel_idx, "_", i)]] %||% hl$q %||% 0.95,
                                min = 0, max = 1, step = 0.05
                            )
                        } else {
                            shiny::div()
                        }
                    })
                })
            })
        })

        # Add hline to panel
        # Track observer handles to prevent memory leaks
        add_hline_observers <- shiny::reactiveVal(list())

        shiny::observe({
            panels <- draft_panels()

            # Destroy old observers
            old_obs <- shiny::isolate(add_hline_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers
            new_obs <- lapply(seq_along(panels), function(panel_idx) {
                shiny::observeEvent(input[[paste0("add_hline_", panel_idx)]],
                    {
                        new_hline <- list(
                            y = 0,
                            color = "grey50",
                            linetype = "dashed"
                        )

                        # Add to panel
                        panels <- draft_panels()
                        if (panel_idx <= length(panels)) {
                            panels[[panel_idx]]$hlines <- c(panels[[panel_idx]]$hlines %||% list(), list(new_hline))
                            draft_panels(panels)
                        }
                    },
                    ignoreInit = TRUE
                )
            })
            add_hline_observers(new_obs)
        })

        # Delete hline observers
        # Track observer handles to prevent memory leaks
        delete_hline_observers <- shiny::reactiveVal(list())

        shiny::observe({
            panels <- draft_panels()

            # Destroy old observers
            old_obs <- shiny::isolate(delete_hline_observers())
            for (obs in old_obs) {
                obs$destroy()
            }

            # Create new observers (nested: for each panel, for each hline)
            new_obs <- list()
            for (panel_idx in seq_along(panels)) {
                panel <- panels[[panel_idx]]
                hlines <- panel$hlines %||% list()
                for (hline_idx in seq_along(hlines)) {
                    obs <- shiny::observeEvent(input[[paste0("delete_hline_", panel_idx, "_", hline_idx)]],
                        {
                            # Capture indices in closure
                            local_panel_idx <- panel_idx
                            local_hline_idx <- hline_idx
                            current_panels <- draft_panels()
                            if (local_panel_idx <= length(current_panels)) {
                                current_hlines <- current_panels[[local_panel_idx]]$hlines %||% list()
                                if (local_hline_idx <= length(current_hlines)) {
                                    current_panels[[local_panel_idx]]$hlines <- current_hlines[-local_hline_idx]
                                    draft_panels(current_panels)
                                }
                            }
                        },
                        ignoreInit = TRUE
                    )
                    new_obs <- c(new_obs, list(obs))
                }
            }
            delete_hline_observers(new_obs)
        })

        # ==================== VALIDATION ====================

        output$validation_errors <- shiny::renderUI({
            if (is.null(draft_cfg())) {
                return(NULL)
            }
            # Rebuild config from current form state
            cfg <- build_config_from_inputs(
                input, draft_vtracks(), draft_panels(),
                draft_colors(), draft_vlines(), draft_cfg()
            )
            errors <- validate_config_full(cfg)

            if (length(errors) > 0) {
                format_validation_errors(errors)
            } else {
                NULL
            }
        })

        # ==================== APPLY CONFIG ====================

        shiny::observeEvent(input$apply_config, {
            # Build final config
            cfg <- build_config_from_inputs(
                input, draft_vtracks(), draft_panels(),
                draft_colors(), draft_vlines(), draft_cfg()
            )

            # Validate
            errors <- validate_config_full(cfg)
            if (length(errors) > 0) {
                shiny::showNotification(
                    paste("Validation errors:", length(errors), "issues found"),
                    type = "error"
                )
                return()
            }

            # Update browser
            browser <- browser_rv()
            old_vtracks <- browser$cfg$vtracks

            # Remove old vtracks
            for (vt in old_vtracks) {
                tryCatch(
                    misha::gvtrack.rm(vt$name),
                    error = function(e) NULL
                )
            }

            # Update config
            browser$cfg <- cfg

            # Recreate vtracks
            browser$state$vtrack_expressions <- list()
            for (vt in cfg$vtracks) {
                tryCatch(
                    {
                        create_vtrack(vt, cfg)
                        browser$state$vtrack_expressions[[vt$name]] <- vt$expression
                    },
                    error = function(e) {
                        cli::cli_warn("Failed to create vtrack '{vt$name}': {e$message}")
                    }
                )
            }

            # Update vlines enabled state
            browser$state$vlines_enabled <- sapply(cfg$vlines, function(v) v$enabled %||% TRUE)

            browser_rv(browser)
            browser_clear_cache()

            shiny::showNotification("Configuration applied successfully", type = "message")
            shiny::removeModal()
        })

        # ==================== RESET CONFIG ====================

        shiny::observeEvent(input$reset_config, {
            draft_cfg(original_config)
            draft_vtracks(original_config$vtracks %||% list())
            draft_panels(original_config$panels %||% list())
            draft_colors(expand_colors_with_tracks(original_config$colors, original_config$panels))
            draft_vlines(original_config$vlines %||% list())

            shiny::showNotification("Reset to original configuration", type = "message")
        })

        # ==================== SAVE TO FILE ====================

        output$save_to_file <- shiny::downloadHandler(
            filename = function() {
                paste0("browser_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")
            },
            content = function(file) {
                cfg <- build_config_from_inputs(
                    input, draft_vtracks(), draft_panels(),
                    draft_colors(), draft_vlines(), draft_cfg()
                )

                # Remove internal fields before saving
                cfg <- clean_config_for_export(cfg)

                yaml::write_yaml(cfg, file)
            }
        )

        # Return function to show modal
        list(
            show_modal = function() {
                browser <- browser_rv()
                if (!is.null(browser)) {
                    cfg <- browser$cfg
                    draft_cfg(cfg)
                    draft_vtracks(cfg$vtracks %||% list())
                    draft_panels(cfg$panels %||% list())
                    draft_colors(expand_colors_with_tracks(cfg$colors, cfg$panels))
                    draft_vlines(cfg$vlines %||% list())

                    shiny::showModal(create_config_modal(ns, cfg))
                }
            }
        )
    })
}
