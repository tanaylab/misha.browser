# shiny-config-editor.R - Config editor UI components
#
# Contains the top-level config_editor_ui(), create_config_modal(),
# and the eight tab UI helper functions.
#
# Server logic lives in shiny-config-editor-server.R
# Panel form helpers live in shiny-config-editor-panels.R
# Config building / color expansion lives in shiny-config-editor-build.R

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
            ),
            bslib::nav_panel(
                "Uploads",
                config_uploads_tab_ui(ns, cfg)
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
                    value = plot_cfg$expansion %||% 0,
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

#' Uploads tab UI
#' @keywords internal
config_uploads_tab_ui <- function(ns, cfg) {
    shiny::tagList(
        # Intervals Section
        shiny::div(
            class = "mb-4",
            shiny::h5(
                shiny::icon("map-marker-alt"),
                " Intervals",
                shiny::actionButton(
                    ns("add_intervals"),
                    "Add",
                    class = "btn-primary btn-sm float-end",
                    icon = shiny::icon("plus")
                )
            ),
            shiny::hr(),
            shiny::uiOutput(ns("uploaded_intervals_list"))
        ),

        # PSSMs Section
        shiny::div(
            class = "mb-4",
            shiny::h5(
                shiny::icon("dna"),
                " PSSMs (Motif Matrices)",
                shiny::actionButton(
                    ns("add_pssm"),
                    "Add",
                    class = "btn-primary btn-sm float-end",
                    icon = shiny::icon("plus")
                )
            ),
            shiny::hr(),
            shiny::uiOutput(ns("uploaded_pssms_list"))
        )
    )
}
