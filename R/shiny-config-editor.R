# shiny-config-editor.R - Shiny module for configuration editing

#' Config Editor Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
config_editor_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        # Modal will be shown via showModal
    )
}

#' Create the config editor modal
#'
#' @param ns Namespace function
#' @param cfg Current configuration
#' @return Modal dialog
#' @keywords internal
create_config_modal <- function(ns, cfg) {
    shiny::modalDialog(
        title = "Configuration Editor",
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
            shiny::actionButton(ns("reset_config"), "Reset to Default",
                class = "btn-outline-secondary"
            ),
            shiny::downloadButton(ns("save_to_file"), "Save to File",
                class = "btn-outline-primary"
            ),
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("apply_config"), "Apply",
                class = "btn-success"
            )
        ),

        # Validation errors display
        shiny::uiOutput(ns("validation_errors")),

        # Tabbed interface
        bslib::navset_tab(
            id = ns("config_tabs"),
            bslib::nav_panel(
                "General",
                config_general_tab_ui(ns, cfg)
            ),
            bslib::nav_panel(
                "Plot",
                config_plot_tab_ui(ns, cfg)
            ),
            bslib::nav_panel(
                "Navigator",
                config_navigator_tab_ui(ns, cfg)
            ),
            bslib::nav_panel(
                "Vtracks",
                config_vtracks_tab_ui(ns, cfg)
            ),
            bslib::nav_panel(
                "Panels",
                config_panels_tab_ui(ns, cfg)
            ),
            bslib::nav_panel(
                "Colors",
                config_colors_tab_ui(ns, cfg)
            ),
            bslib::nav_panel(
                "Vlines",
                config_vlines_tab_ui(ns, cfg)
            )
        )
    )
}

#' General tab UI
#' @keywords internal
config_general_tab_ui <- function(ns, cfg) {
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::textInput(
                    ns("gen_title"),
                    "Browser Title",
                    value = cfg$ui$title %||% "Genome Browser"
                )
            )
        ),
        shiny::hr(),
        shiny::h5("Start Region"),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::textInput(
                    ns("gen_start_gene"),
                    "Gene Name",
                    value = cfg$start$gene %||% ""
                )
            ),
            shiny::column(
                2,
                shiny::tags$label("OR", style = "margin-top: 30px; display: block; text-align: center;")
            ),
            shiny::column(
                6,
                shiny::textInput(
                    ns("gen_start_coords"),
                    "Coordinates (chr:start-end)",
                    value = if (!is.null(cfg$start$coords)) {
                        sprintf(
                            "%s:%d-%d", cfg$start$coords$chrom,
                            cfg$start$coords$start, cfg$start$coords$end
                        )
                    } else {
                        ""
                    }
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::numericInput(
                    ns("gen_span_bp"),
                    "Start Span (bp)",
                    value = cfg$start$span_bp %||% cfg$ui$span_default %||% 2e6,
                    min = 1000, step = 100000
                )
            )
        ),
        shiny::hr(),
        shiny::h5("UI Defaults"),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::numericInput(
                    ns("gen_span_default"),
                    "Span Default (bp)",
                    value = cfg$ui$span_default %||% 2e6,
                    min = 1000, step = 100000
                )
            ),
            shiny::column(
                4,
                shiny::numericInput(
                    ns("gen_smooth_default"),
                    "Smooth Window Default",
                    value = cfg$ui$smooth_window_default %||% 10,
                    min = 1, step = 5
                )
            ),
            shiny::column(
                4,
                shiny::checkboxInput(
                    ns("gen_show_coords"),
                    "Show Coordinates",
                    value = cfg$ui$show_coordinates %||% TRUE
                )
            )
        )
    )
}

#' Plot tab UI
#' @keywords internal
config_plot_tab_ui <- function(ns, cfg) {
    plot_cfg <- cfg$plot %||% list()

    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::numericInput(
                    ns("plot_iterator"),
                    "Iterator",
                    value = plot_cfg$iterator %||% 32,
                    min = 1, step = 1
                )
            ),
            shiny::column(
                4,
                shiny::numericInput(
                    ns("plot_expansion"),
                    "Expansion",
                    value = plot_cfg$expansion %||% 12000,
                    min = 0, step = 1000
                )
            ),
            shiny::column(
                4,
                shiny::numericInput(
                    ns("plot_target_points"),
                    "Target Points",
                    value = plot_cfg$target_points %||% 4000,
                    min = 100, step = 500
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::selectInput(
                    ns("plot_extraction_mode"),
                    "Extraction Mode",
                    choices = c("fixed", "dynamic", "dynamic_smooth"),
                    selected = plot_cfg$extraction_mode %||% "dynamic"
                )
            ),
            shiny::column(
                4,
                shiny::selectInput(
                    ns("plot_theme"),
                    "Theme",
                    choices = c("bw", "minimal", "classic", "gray"),
                    selected = plot_cfg$theme %||% "bw"
                )
            )
        )
    )
}

#' Navigator tab UI
#' @keywords internal
config_navigator_tab_ui <- function(ns, cfg) {
    nav_cfg <- cfg$navigator %||% list()

    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::textInput(
                    ns("nav_source"),
                    "Source Intervals",
                    value = nav_cfg$source %||% ""
                ),
                shiny::helpText("e.g., intervs.global.tss")
            ),
            shiny::column(
                6,
                shiny::textInput(
                    ns("nav_label_field"),
                    "Label Field",
                    value = nav_cfg$label_field %||% "geneSymbol"
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::numericInput(
                    ns("nav_extension"),
                    "Extension (bp)",
                    value = nav_cfg$extension %||% 1e6,
                    min = 0, step = 100000
                )
            )
        )
    )
}

#' Vtracks tab UI
#' @keywords internal
config_vtracks_tab_ui <- function(ns, cfg) {
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                12,
                shiny::actionButton(ns("add_vtrack"), "Add Vtrack",
                    class = "btn-primary btn-sm", icon = shiny::icon("plus")
                )
            )
        ),
        shiny::hr(),

        # Dynamic vtrack list
        shiny::uiOutput(ns("vtrack_list"))
    )
}

#' Panels tab UI
#' @keywords internal
config_panels_tab_ui <- function(ns, cfg) {
    shiny::tagList(
        shiny::tags$script(shiny::HTML(sprintf(
            "document.addEventListener('toggle', function(e) {
                if (!e.target.matches('.%s details')) return;
                if (!e.target.open) return;
                var container = e.target.closest('.%s');
                if (!container) return;
                container.querySelectorAll('details').forEach(function(d) {
                    if (d !== e.target) d.open = false;
                });
            });",
            ns("panel-accordion"), ns("panel-accordion")
        ))),
        shiny::fluidRow(
            shiny::column(
                12,
                shiny::actionButton(ns("add_panel"), "Add Panel",
                    class = "btn-primary btn-sm", icon = shiny::icon("plus")
                )
            )
        ),
        shiny::hr(),

        # Accordion for panels
        shiny::uiOutput(ns("panel_accordion"))
    )
}

#' Colors tab UI
#' @keywords internal
config_colors_tab_ui <- function(ns, cfg) {
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                12,
                shiny::actionButton(ns("add_color"), "Add Color",
                    class = "btn-primary btn-sm", icon = shiny::icon("plus")
                )
            )
        ),
        shiny::hr(),

        # Color mapping list
        shiny::uiOutput(ns("color_list"))
    )
}

#' Vlines tab UI
#' @keywords internal
config_vlines_tab_ui <- function(ns, cfg) {
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                12,
                shiny::actionButton(ns("add_vline"), "Add Vline",
                    class = "btn-primary btn-sm", icon = shiny::icon("plus")
                )
            )
        ),
        shiny::hr(),

        # Vline list
        shiny::uiOutput(ns("vline_list"))
    )
}

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

        # Initialize draft when modal opens
        shiny::observeEvent(browser_rv(),
            {
                if (!is.null(browser_rv())) {
                    cfg <- browser_rv()$cfg
                    draft_cfg(cfg)
                    draft_vtracks(cfg$vtracks %||% list())
                    draft_panels(cfg$panels %||% list())
                    draft_colors(as.list(cfg$colors %||% list()))
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
                shiny::div(
                    class = "card mb-2",
                    shiny::div(
                        class = "card-body py-2",
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
                                shiny::textInput(
                                    ns(paste0("vt_src_", i)),
                                    "Source Track",
                                    value = vt$src %||% ""
                                )
                            ),
                            shiny::column(
                                2,
                                shiny::selectInput(
                                    ns(paste0("vt_func_", i)),
                                    "Function",
                                    choices = list(
                                        "Track Summary" = c("avg", "sum", "min", "max", "stddev", "quantile", "nearest", "size"),
                                        "Point Values" = c("first", "last", "sample", "exists"),
                                        "Global Stats" = c("global.percentile", "global.percentile.max", "global.percentile.min")
                                        # "Positions" = c(
                                        #     "max.pos.abs", "max.pos.relative", "min.pos.abs", "min.pos.relative",
                                        #     "first.pos.abs", "first.pos.relative", "last.pos.abs", "last.pos.relative",
                                        #     "sample.pos.abs", "sample.pos.relative"
                                        # ),
                                        # "Intervals" = c("distance", "distance.center", "distance.edge", "coverage", "neighbor.count")
                                        # "Sequence & Motifs" = c(
                                        #     "pwm", "pwm.max", "pwm.max.pos", "pwm.count",
                                        #     "kmer.count", "kmer.frac", "masked.count", "masked.frac"
                                        # )
                                    ),
                                    selected = vt$func %||% "sum"
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
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                8,
                                shiny::textInput(
                                    ns(paste0("vt_expr_", i)),
                                    "Expression Wrapper",
                                    value = vt$expression %||% "",
                                    placeholder = "e.g., pmax(vtrack_name, 0)"
                                )
                            ),
                            shiny::column(
                                4,
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
        shiny::observe({
            vtracks <- draft_vtracks()
            lapply(seq_along(vtracks), function(i) {
                shiny::observeEvent(input[[paste0("delete_vt_", i)]],
                    {
                        current <- draft_vtracks()
                        if (i <= length(current)) {
                            draft_vtracks(current[-i])
                        }
                    },
                    ignoreInit = TRUE,
                    once = TRUE
                )
            })
        })

        # Add vtrack transform
        shiny::observe({
            vtracks <- draft_vtracks()
            lapply(seq_along(vtracks), function(i) {
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
                    ignoreInit = TRUE,
                    once = FALSE
                )
            })
        })

        # Delete vtrack transform
        shiny::observe({
            vtracks <- draft_vtracks()
            lapply(seq_along(vtracks), function(i) {
                vt <- vtracks[[i]]
                transforms <- vt$transforms %||% list()
                lapply(seq_along(transforms), function(j) {
                    shiny::observeEvent(input[[paste0("delete_vt_tr_", i, "_", j)]],
                        {
                            current <- draft_vtracks()
                            if (i <= length(current)) {
                                cur_vt <- current[[i]]
                                cur_transforms <- cur_vt$transforms %||% list()
                                if (j <= length(cur_transforms)) {
                                    cur_transforms <- cur_transforms[-j]
                                    cur_vt$transforms <- cur_transforms
                                    current[[i]] <- cur_vt
                                    draft_vtracks(current)
                                }
                            }
                        },
                        ignoreInit = TRUE,
                        once = TRUE
                    )
                })
            })
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
                height = 2
            )
            draft_panels(c(panels, list(new_panel)))
        })

        # Delete panel observers (dynamic)
        shiny::observe({
            panels <- draft_panels()
            lapply(seq_along(panels), function(i) {
                shiny::observeEvent(input[[paste0("delete_pnl_", i)]],
                    {
                        current <- draft_panels()
                        if (i <= length(current)) {
                            draft_panels(current[-i])
                        }
                    },
                    ignoreInit = TRUE,
                    once = TRUE
                )
            })
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
        shiny::observe({
            colors <- draft_colors()
            lapply(seq_along(colors), function(i) {
                shiny::observeEvent(input[[paste0("delete_color_", i)]],
                    {
                        current <- draft_colors()
                        if (i <= length(current)) {
                            current[i] <- NULL
                            draft_colors(current)
                        }
                    },
                    ignoreInit = TRUE,
                    once = TRUE
                )
            })
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
        shiny::observe({
            vlines <- draft_vlines()
            lapply(seq_along(vlines), function(i) {
                shiny::observeEvent(input[[paste0("delete_vl_", i)]],
                    {
                        current <- draft_vlines()
                        if (i <= length(current)) {
                            draft_vlines(current[-i])
                        }
                    },
                    ignoreInit = TRUE,
                    once = TRUE
                )
            })
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
        shiny::observe({
            panels <- draft_panels()
            lapply(seq_along(panels), function(panel_idx) {
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
        })

        # Delete hline observers
        shiny::observe({
            panels <- draft_panels()
            lapply(seq_along(panels), function(panel_idx) {
                panel <- panels[[panel_idx]]
                hlines <- panel$hlines %||% list()
                lapply(seq_along(hlines), function(hline_idx) {
                    shiny::observeEvent(input[[paste0("delete_hline_", panel_idx, "_", hline_idx)]],
                        {
                            current_panels <- draft_panels()
                            if (panel_idx <= length(current_panels)) {
                                current_hlines <- current_panels[[panel_idx]]$hlines %||% list()
                                if (hline_idx <= length(current_hlines)) {
                                    current_panels[[panel_idx]]$hlines <- current_hlines[-hline_idx]
                                    draft_panels(current_panels)
                                }
                            }
                        },
                        ignoreInit = TRUE,
                        once = TRUE
                    )
                })
            })
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
                    misha::gvtrack.rm(vt$name, force = TRUE),
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
            draft_colors(as.list(original_config$colors %||% list()))
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
                    draft_colors(as.list(cfg$colors %||% list()))
                    draft_vlines(cfg$vlines %||% list())

                    shiny::showModal(create_config_modal(ns, cfg))
                }
            }
        )
    })
}

#' Create panel editing form
#' @keywords internal
create_panel_form <- function(ns, panel, index, available_tracks) {
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::textInput(
                    ns(paste0("pnl_name_", index)),
                    "Name",
                    value = panel$name %||% ""
                )
            ),
            shiny::column(
                4,
                shiny::selectInput(
                    ns(paste0("pnl_type_", index)),
                    "Type",
                    choices = c("data", "annotation", "intervals", "ideogram"),
                    selected = panel$type %||% "data"
                )
            ),
            shiny::column(
                4,
                shiny::numericInput(
                    ns(paste0("pnl_height_", index)),
                    "Height",
                    value = panel$height %||% 2,
                    min = 0.5, step = 0.5
                )
            )
        ),

        # Data panel options
        shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'data'", ns(paste0("pnl_type_", index))),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shinyWidgets::virtualSelectInput(
                        ns(paste0("pnl_tracks_", index)),
                        "Tracks",
                        choices = available_tracks,
                        selected = unlist(panel$tracks),
                        multiple = TRUE,
                        search = TRUE
                    )
                ),
                shiny::column(
                    3,
                    shiny::selectInput(
                        ns(paste0("pnl_plot_type_", index)),
                        "Plot Type",
                        choices = c("line", "area", "point", "histogram"),
                        selected = panel$plot_type %||% "line"
                    )
                ),
                shiny::column(
                    3,
                    shiny::checkboxInput(
                        ns(paste0("pnl_legend_", index)),
                        "Show Legend",
                        value = panel$show_legend %||% TRUE
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    3,
                    shiny::numericInput(
                        ns(paste0("pnl_ylim_min_", index)),
                        "Y Min",
                        value = if (!is.null(panel$ylim)) panel$ylim[1] else NA
                    )
                ),
                shiny::column(
                    3,
                    shiny::numericInput(
                        ns(paste0("pnl_ylim_max_", index)),
                        "Y Max",
                        value = if (!is.null(panel$ylim)) panel$ylim[2] else NA
                    )
                ),
                shiny::column(
                    6,
                    shiny::textInput(
                        ns(paste0("pnl_y_title_", index)),
                        "Y Title",
                        value = panel$y_title %||% ""
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::textInput(
                        ns(paste0("pnl_facet_", index)),
                        "Facet By",
                        value = panel$facet_by %||% ""
                    )
                ),
                shiny::column(
                    6,
                    shiny::textInput(
                        ns(paste0("pnl_color_by_", index)),
                        "Color By",
                        value = panel$grouping$color_by %||% ""
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    12,
                    shiny::textInput(
                        ns(paste0("pnl_pattern_", index)),
                        "Grouping Pattern (regex)",
                        value = panel$grouping$pattern %||% "",
                        placeholder = "e.g., ^(?<source>.+)\\.(?<mark>.+)$"
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    12,
                    shiny::h5("Horizontal Lines"),
                    shiny::actionButton(
                        ns(paste0("add_hline_", index)),
                        "Add H-Line",
                        icon = shiny::icon("plus"),
                        class = "btn-sm btn-outline-primary mb-2"
                    ),
                    shiny::uiOutput(ns(paste0("hline_list_", index)))
                )
            )
        ),

        # Annotation panel options
        shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'annotation'", ns(paste0("pnl_type_", index))),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::textInput(
                        ns(paste0("pnl_exon_src_", index)),
                        "Exon Source",
                        value = panel$exon_source %||% ""
                    )
                ),
                shiny::column(
                    6,
                    shiny::textInput(
                        ns(paste0("pnl_tss_src_", index)),
                        "TSS Source",
                        value = panel$tss_source %||% ""
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::textInput(
                        ns(paste0("pnl_gene_label_", index)),
                        "Gene Label Field",
                        value = panel$gene_label_field %||% "geneSymbol"
                    )
                )
            )
        ),

        # Intervals panel options
        shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'intervals'", ns(paste0("pnl_type_", index))),
            shiny::fluidRow(
                shiny::column(
                    4,
                    shiny::selectInput(
                        ns(paste0("pnl_intervals_source_", index)),
                        "Source Type",
                        choices = c("intervals", "file"),
                        selected = panel$source %||% "intervals"
                    )
                ),
                shiny::column(
                    8,
                    shiny::textInput(
                        ns(paste0("pnl_intervals_ref_", index)),
                        "Intervals Name or File",
                        value = panel$intervals %||% panel$file %||% ""
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    4,
                    shiny::textInput(
                        ns(paste0("pnl_intervals_label_", index)),
                        "Label Field",
                        value = panel$label_field %||% ""
                    )
                ),
                shiny::column(
                    4,
                    shiny::textInput(
                        ns(paste0("pnl_intervals_color_by_", index)),
                        "Color By",
                        value = panel$color_by %||% ""
                    )
                ),
                shiny::column(
                    4,
                    shiny::checkboxInput(
                        ns(paste0("pnl_intervals_labels_", index)),
                        "Show Labels",
                        value = panel$show_labels %||% FALSE
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    4,
                    shiny::textInput(
                        ns(paste0("pnl_intervals_filter_field_", index)),
                        "Filter Field",
                        value = panel$filter_field %||% ""
                    )
                ),
                shiny::column(
                    4,
                    shiny::textInput(
                        ns(paste0("pnl_intervals_filter_values_", index)),
                        "Filter Values (comma)",
                        value = {
                            vals <- panel$filter_values %||% character(0)
                            paste(vals, collapse = ", ")
                        }
                    )
                ),
                shiny::column(
                    4,
                    shiny::textInput(
                        ns(paste0("pnl_intervals_filter_regex_", index)),
                        "Filter Regex",
                        value = panel$filter_regex %||% ""
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    4,
                    colourpicker::colourInput(
                        ns(paste0("pnl_intervals_color_", index)),
                        "Fill Color",
                        value = panel$color %||% "grey60",
                        showColour = "both"
                    )
                ),
                shiny::column(
                    4,
                    colourpicker::colourInput(
                        ns(paste0("pnl_intervals_outline_", index)),
                        "Outline Color",
                        value = panel$outline_color %||% "grey20",
                        showColour = "both"
                    )
                )
            )
        ),

        # Delete button
        shiny::div(
            style = "margin-top: 10px;",
            shiny::actionButton(
                ns(paste0("delete_pnl_", index)),
                "Delete Panel",
                icon = shiny::icon("trash"),
                class = "btn-outline-danger btn-sm"
            )
        )
    )
}

#' Get available tracks from vtracks and existing panel tracks
#' @keywords internal
get_available_tracks <- function(vtracks, panels) {
    # Vtrack names
    vt_names <- sapply(vtracks, function(v) v$name %||% "")
    vt_names <- vt_names[vt_names != ""]

    # Track names from panels
    panel_tracks <- character(0)
    for (panel in panels) {
        if (!is.null(panel$tracks)) {
            panel_tracks <- c(panel_tracks, unlist(panel$tracks))
        }
    }

    unique(c(vt_names, panel_tracks))
}

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
        vt$src <- input[[paste0("vt_src_", i)]] %||% vt$src
        vt$func <- input[[paste0("vt_func_", i)]] %||% vt$func
        vt$sshift <- input[[paste0("vt_sshift_", i)]] %||% vt$sshift
        vt$eshift <- input[[paste0("vt_eshift_", i)]] %||% vt$eshift
        vt$expression <- input[[paste0("vt_expr_", i)]] %||% vt$expression
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
    new_colors <- list()
    for (i in seq_along(colors)) {
        name <- input[[paste0("color_name_", i)]] %||% names(colors)[i]
        value <- input[[paste0("color_value_", i)]] %||% colors[[i]]
        if (!is.null(name) && nchar(name) > 0) {
            new_colors[[name]] <- value
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

#' Clean configuration for YAML export
#' @keywords internal
clean_config_for_export <- function(cfg) {
    # Remove internal fields that start with ._
    remove_internal <- function(x) {
        if (is.list(x)) {
            # Remove fields starting with ._
            to_remove <- grep("^\\._", names(x), value = TRUE)
            for (field in to_remove) {
                x[[field]] <- NULL
            }
            # Recurse
            for (name in names(x)) {
                x[[name]] <- remove_internal(x[[name]])
            }
        }
        x
    }

    remove_internal(cfg)
}
