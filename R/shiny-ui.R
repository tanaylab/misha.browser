# shiny-ui.R - Shiny UI definition for misha.browser

#' Create browser UI
#'
#' @param browser Browser object (for configuration)
#' @return Shiny UI definition
#' @keywords internal
browser_ui <- function(browser) {
    cfg <- browser$cfg

    shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::tags$head(
            shiny::tags$style(shiny::HTML("
                .control-panel {
                    background-color: #f8f9fa;
                    padding: 10px 15px;
                    border-radius: 5px;
                    margin-bottom: 10px;
                    border: 1px solid #e9ecef;
                }
                .control-panel .row {
                    display: flex;
                    align-items: flex-end;
                }
                .control-panel .row > div {
                    display: flex;
                    flex-direction: column;
                    justify-content: flex-end;
                }
                .control-label { margin-bottom: 3px; font-weight: 500; font-size: 12px; }
                .shiny-plot-output { border: 1px solid #ddd; }
                .btn-row {
                    display: flex;
                    align-items: flex-end;
                    gap: 5px;
                    flex-wrap: nowrap;
                }
                .btn-row .btn { margin-bottom: 0; }
                .btn-row .btn-group { margin-bottom: 0; }
                .btn-row .vscomp-wrapper { margin-bottom: 0; }
                .form-group { margin-bottom: 0; }
            "))
        ),

        # Title
        shiny::uiOutput("browser_title"),

        # Top Controls
        shiny::div(
            class = "control-panel",
            shiny::fluidRow(
                # Navigation buttons
                shiny::column(
                    3,
                    shiny::tags$label("Navigation", class = "control-label"),
                    shiny::div(
                        class = "btn-row",
                        shinyWidgets::actionGroupButtons(
                            inputIds = c("mv_left_big", "mv_left_small", "mv_right_small", "mv_right_big"),
                            labels = c("<<", "<", ">", ">>"),
                            status = "primary", size = "sm"
                        ),
                        shinyWidgets::actionGroupButtons(
                            inputIds = c("zoom_out", "zoom_in"),
                            labels = c("-", "+"),
                            status = "success", size = "sm"
                        ),
                        shiny::actionButton("history_back", "",
                            icon = shiny::icon("arrow-left"),
                            class = "btn-outline-secondary btn-sm"
                        ),
                        shiny::actionButton("history_forward", "",
                            icon = shiny::icon("arrow-right"),
                            class = "btn-outline-secondary btn-sm"
                        )
                    )
                ),

                # Coordinate search
                shiny::column(
                    2,
                    shinyWidgets::searchInput(
                        inputId = "coord_search",
                        label = "Coordinates",
                        placeholder = "chr5:1-2000000",
                        btnSearch = shiny::icon("arrow-right"),
                        btnReset = shiny::icon("times"),
                        width = "100%"
                    )
                ),

                # Gene/region selector + Go button
                shiny::column(
                    2,
                    shiny::tags$label("Go to Region", class = "control-label"),
                    shiny::div(
                        class = "btn-row",
                        shiny::div(
                            style = "flex: 1; min-width: 100px;",
                            shinyWidgets::virtualSelectInput(
                                inputId = "region_select",
                                label = NULL,
                                choices = NULL,
                                search = TRUE,
                                placeholder = "Select...",
                                width = "100%"
                            )
                        ),
                        shiny::actionButton("go_region", "Go",
                            class = "btn-info btn-sm"
                        )
                    )
                ),

                # Brush mode + Clear highlight
                shiny::column(
                    2,
                    shiny::tags$label("Brush Mode", class = "control-label"),
                    shiny::div(
                        class = "btn-row",
                        shinyWidgets::radioGroupButtons(
                            inputId = "brush_mode",
                            label = NULL,
                            choices = c("Zoom" = "zoom", "Highlight" = "highlight"),
                            selected = "zoom",
                            size = "sm",
                            status = "outline-secondary"
                        ),
                        shiny::actionButton("clear_highlight", "Clear",
                            class = "btn-outline-warning btn-sm"
                        )
                    )
                ),

                # Extraction mode
                shiny::column(
                    1,
                    shiny::selectInput(
                        inputId = "extraction_mode",
                        label = shiny::tagList(
                            "Mode ",
                            shiny::actionLink(
                                inputId = "extraction_help",
                                label = NULL,
                                icon = shiny::icon("circle-question"),
                                class = "text-muted",
                                style = "margin-left: 4px;"
                            )
                        ),
                        choices = c("fixed", "dynamic", "dynamic_smooth"),
                        selected = cfg$plot$extraction_mode %||% "fixed",
                        width = "100%"
                    )
                ),

                # Smooth window
                shiny::column(
                    2,
                    shiny::numericInput(
                        "smooth_window",
                        "Smooth",
                        value = cfg$ui$smooth_window_default %||% 10,
                        min = 1, step = 5, width = "100%"
                    )
                )
            ),

            # Location info row (compact)
            shiny::div(
                style = "margin-top: 5px; padding-left: 15px;",
                shiny::div(
                    shiny::span(
                        shiny::textOutput("current_loc_text", inline = TRUE),
                        style = "color: #666; font-family: monospace; font-size: 13px;"
                    )
                )
            )
        ),

        # Main plot
        shiny::fluidRow(
            shiny::column(
                12,
                shinycssloaders::withSpinner(
                    shiny::plotOutput(
                        "main_plot",
                        height = "700px",
                        brush = shiny::brushOpts(
                            id = "plot_brush",
                            direction = "x",
                            resetOnNew = TRUE
                        )
                    ),
                    type = 6
                )
            )
        ),
        shiny::div(
            style = "margin-top: 4px; padding-left: 15px;",
            shiny::span(
                shiny::textOutput("smoothing_bp_text", inline = TRUE),
                style = "color: #666; font-family: monospace; font-size: 12px;"
            )
        ),

        # Bottom controls
        shiny::div(
            class = "control-panel",
            style = "margin-top: 10px;",
            shiny::fluidRow(
                # Span control (numeric)
                shiny::column(
                    2,
                    shiny::numericInput(
                        inputId = "span_input",
                        label = "Span (bp)",
                        value = cfg$ui$span_default %||% 2e6,
                        min = 1000,
                        step = 100000,
                        width = "100%"
                    ),
                    shiny::actionButton("apply_span", "Apply",
                        class = "btn-secondary btn-sm", width = "100%"
                    )
                ),

                # Track selection
                shiny::column(
                    5,
                    shinyWidgets::virtualSelectInput(
                        inputId = "track_select",
                        label = "Tracks",
                        choices = get_track_choices(browser),
                        selected = get_default_tracks(browser),
                        multiple = TRUE,
                        search = TRUE,
                        placeholder = "Select tracks...",
                        width = "100%"
                    ),
                    shiny::actionButton("apply_tracks", "Apply Tracks",
                        class = "btn-secondary btn-sm", width = "100%"
                    )
                ),

                # Vertical lines toggles
                shiny::column(
                    3,
                    vlines_ui(browser)
                ),

                # Cache control
                shiny::column(
                    2,
                    shiny::tags$label("Tools", class = "control-label"),
                    shiny::div(
                        class = "btn-row",
                        shiny::actionButton("open_config_editor", "Config",
                            icon = shiny::icon("gear"),
                            class = "btn-outline-primary btn-sm"
                        ),
                        shiny::actionButton("clear_cache", "Clear",
                            class = "btn-outline-danger btn-sm"
                        )
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    3,
                    shiny::numericInput(
                        inputId = "plot_iterator",
                        label = "Iterator",
                        value = cfg$plot$iterator %||% 32,
                        min = 1,
                        step = 1,
                        width = "100%"
                    )
                ),
                shiny::column(
                    3,
                    shiny::conditionalPanel(
                        condition = "input.extraction_mode != 'fixed'",
                        shiny::numericInput(
                            inputId = "plot_target_points",
                            label = "Target pts",
                            value = cfg$plot$target_points %||% 4000,
                            min = 100,
                            step = 100,
                            width = "100%"
                        )
                    )
                ),
                shiny::column(
                    3,
                    shiny::conditionalPanel(
                        condition = "input.extraction_mode == 'dynamic_smooth'",
                        shiny::numericInput(
                            inputId = "plot_smoothing_bp",
                            label = "Smoothing bp",
                            value = cfg$plot$smoothing_bp %||% 3200,
                            min = 1,
                            step = 100,
                            width = "100%"
                        )
                    )
                )
            )
        ),
        config_editor_ui("config_editor")
    )
}

#' Get track choices for selection UI
#'
#' @param browser Browser object
#' @return Character vector of track names
#' @keywords internal
get_track_choices <- function(browser) {
    # Get all tracks from all data panels
    all_tracks <- character(0)
    for (panel in browser$cfg$panels) {
        if (panel$type == "data" && !is.null(panel$tracks)) {
            all_tracks <- c(all_tracks, extract_track_names(panel$tracks))
        }
    }
    ui_defaults <- browser$cfg$ui$default_tracks
    if (!is.null(ui_defaults)) {
        all_tracks <- c(all_tracks, extract_track_names(ui_defaults))
    }
    unique(all_tracks)
}

#' Extract track names from a tracks list
#'
#' Handles both string tracks and list-based track specs (with expr/name fields).
#' Only includes character tracks and list entries with explicit 'name' field.
#'
#' @param tracks List or vector of track specifications
#' @return Character vector of track names
#' @keywords internal
extract_track_names <- function(tracks) {
    if (is.null(tracks) || length(tracks) == 0) {
        return(character(0))
    }

    track_names <- character(0)
    for (track in tracks) {
        if (is.character(track)) {
            # Simple string track name
            track_names <- c(track_names, track)
        } else if (is.list(track) && !is.null(track$name)) {
            # List with explicit name field
            track_names <- c(track_names, track$name)
        }
        # Skip inline expressions without explicit name
    }

    track_names[nzchar(track_names)]
}

#' Get default selected tracks
#'
#' @param browser Browser object
#' @return Character vector of default track names
#' @keywords internal
get_default_tracks <- function(browser) {
    defaults <- browser$cfg$ui$default_tracks
    if (!is.null(defaults)) {
        defaults <- extract_track_names(defaults)
        if (length(defaults) > 0) {
            choices <- get_track_choices(browser)
            defaults <- defaults[defaults %in% choices]
            if (length(defaults) > 0) {
                return(defaults)
            }
        }
    }
    # Return tracks from first data panel as defaults
    for (panel in browser$cfg$panels) {
        if (panel$type == "data" && !is.null(panel$tracks)) {
            return(extract_track_names(panel$tracks))
        }
    }
    character(0)
}

#' Create vlines toggle UI
#'
#' @param browser Browser object
#' @return Shiny UI element
#' @keywords internal
vlines_ui <- function(browser) {
    vlines <- browser$cfg$vlines
    if (length(vlines) == 0) {
        return(NULL)
    }

    checkboxes <- lapply(seq_along(vlines), function(i) {
        vl <- vlines[[i]]
        shiny::checkboxInput(
            inputId = paste0("vline_", i),
            label = vl$name,
            value = vl$enabled %||% TRUE,
            width = "auto"
        )
    })

    shiny::div(
        shiny::tags$label("Vertical Lines"),
        shiny::div(
            style = "display: flex; flex-wrap: wrap; gap: 10px;",
            checkboxes
        )
    )
}

#' Prepare region choices for virtual select
#'
#' @param regions Data frame with regions
#' @param label_field Column to use for labels
#' @return List suitable for virtualSelectInput
#' @keywords internal
prepare_region_choices <- function(regions, label_field) {
    if (is.null(regions) || nrow(regions) == 0) {
        return(NULL)
    }
    if (!label_field %in% names(regions)) {
        return(NULL)
    }

    choices <- lapply(seq_len(nrow(regions)), function(i) {
        r <- regions[i, ]
        list(
            label = sprintf(
                "%s (%s:%s-%s)",
                r[[label_field]],
                r$chrom,
                format(r$start, big.mark = ",", scientific = FALSE),
                format(r$end, big.mark = ",", scientific = FALSE)
            ),
            value = sprintf("%s:%d-%d", r$chrom, r$start, r$end)
        )
    })

    choices
}
