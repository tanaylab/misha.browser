# shiny-config-editor-panels.R - Panel form UI and track helpers
#
# Contains create_panel_form() for building panel editing forms
# and get_available_tracks() for resolving track choices.

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
                        choices = c("line", "area", "point", "histogram", "segment"),
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
