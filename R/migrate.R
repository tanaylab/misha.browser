# migrate.R - Migration utilities from misha.vis to misha.browser

#' Convert misha.vis configuration to misha.browser YAML
#'
#' Attempts to convert a misha.vis configuration list to a misha.browser
#' YAML configuration file.
#'
#' @param vis_config misha.vis configuration list (from vis_create or YAML)
#' @param output_file Output YAML file path
#' @param misha_root Optional misha root path
#' @return Invisibly returns the converted configuration
#' @export
#' @examples
#' \dontrun{
#' # If you have a misha.vis config
#' old_config <- yaml::read_yaml("vis_config.yaml")
#' browser_convert_vis_config(old_config, "browser_config.yaml")
#' }
browser_convert_vis_config <- function(vis_config, output_file = "browser_config.yaml",
                                       misha_root = NULL) {
    cli::cli_h1("Converting misha.vis config to misha.browser format")

    # Initialize new config
    new_config <- list(
        profiles = list(
            local = list(
                misha_root = misha_root %||% ""
            )
        ),
        panels = list(),
        vtracks = list(),
        vlines = list(),
        plot = list(
            iterator = .DEFAULT_ITERATOR,
            expansion = .DEFAULT_EXPANSION,
            theme = .DEFAULT_THEME,
            target_points = .DEFAULT_TARGET_POINTS
        ),
        ui = list(
            title = vis_config$title %||% .DEFAULT_TITLE,
            span_default = .DEFAULT_SPAN,
            smooth_window_default = .DEFAULT_SMOOTH_WINDOW,
            show_coordinates = TRUE
        ),
        navigator = list(
            source = .DEFAULT_NAV_SOURCE,
            label_field = .DEFAULT_NAV_LABEL_FIELD,
            extension = .DEFAULT_NAV_EXTENSION
        ),
        start = list()
    )

    # Convert elements
    elements <- vis_config$elements %||% list()
    cli::cli_alert_info("Found {length(elements)} elements to convert")

    for (elem in elements) {
        converted <- convert_vis_element(elem)
        if (!is.null(converted$panel)) {
            new_config$panels <- c(new_config$panels, list(converted$panel))
            cli::cli_alert_success("Converted panel: {converted$panel$name}")
        }
    }

    # Convert vtracks
    vtracks <- vis_config$vtracks %||% list()
    for (vt in vtracks) {
        new_config$vtracks <- c(new_config$vtracks, list(list(
            name = vt$name,
            track = vt$track,
            func = vt$func %||% "sum"
        )))
    }

    # Convert display_defaults to colors if present
    display_defaults <- vis_config$display_defaults %||% list()

    # Save config
    yaml::write_yaml(new_config, output_file)
    cli::cli_alert_success("Configuration saved to {output_file}")
    cli::cli_alert_info("Please review the converted config and adjust as needed")

    invisible(new_config)
}

#' Convert a single misha.vis element
#'
#' @param elem misha.vis element configuration
#' @return List with panel configuration
#' @keywords internal
convert_vis_element <- function(elem) {
    result <- list(panel = NULL)

    elem_type <- elem$type %||% "data_track"

    if (elem_type == "data_track") {
        panel <- list(
            name = elem$name %||% paste0("panel_", sample(1000, 1)),
            type = "data",
            tracks = c(elem$track),
            plot_type = convert_plot_type(elem$plot_type),
            height = elem$height %||% .DEFAULT_DATA_PANEL_HEIGHT,
            transforms = list()
        )

        # Add color if specified
        if (!is.null(elem$color)) {
            panel$colors <- list()
            panel$colors[[elem$track]] <- elem$color
        }

        # Add ylim if specified
        if (!is.null(elem$ylim)) {
            panel$ylim <- elem$ylim
        }

        result$panel <- panel
    } else if (elem_type == "genes") {
        panel <- list(
            name = elem$name %||% "genes",
            type = "annotation",
            exon_source = "intervs.global.exon",
            tss_source = "intervs.global.tss",
            gene_label_field = "geneSymbol",
            height = elem$height %||% .DEFAULT_ANNOTATION_HEIGHT
        )
        result$panel <- panel
    } else if (elem_type == "ideogram") {
        panel <- list(
            name = elem$name %||% "ideogram",
            type = "ideogram",
            height = elem$height %||% .DEFAULT_IDEOGRAM_HEIGHT
        )
        result$panel <- panel
    }

    result
}

#' Convert misha.vis plot_type to misha.browser format
#'
#' @param vis_type misha.vis plot type
#' @return misha.browser plot type
#' @keywords internal
convert_plot_type <- function(vis_type) {
    if (is.null(vis_type)) {
        return("line")
    }

    # misha.vis uses Gviz-style types
    switch(vis_type,
        "p" = "point",
        "l" = "line",
        "polygon" = "area",
        "histogram" = "histogram",
        "h" = "histogram",
        "line"
    )
}
