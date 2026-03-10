# shiny-config-validation.R - Validation functions for config editor

#' Full configuration validation for editor
#'
#' @param cfg Configuration list
#' @return List of validation errors (empty list if valid)
#' @keywords internal
validate_config_full <- function(cfg) {
    errors <- list()

    # General validation
    errors <- c(errors, validate_general_section(cfg))

    # Plot validation
    errors <- c(errors, validate_plot_section(cfg$plot))

    # Navigator validation
    errors <- c(errors, validate_navigator_section(cfg$navigator))

    # Vtrack validation
    for (i in seq_along(cfg$vtracks)) {
        vt_errors <- validate_vtrack_full(cfg$vtracks[[i]], i)
        errors <- c(errors, vt_errors)
    }

    # Panel validation
    vtrack_names <- sapply(cfg$vtracks, function(v) v$name)
    for (i in seq_along(cfg$panels)) {
        pnl_errors <- validate_panel_full(cfg$panels[[i]], i, vtrack_names)
        errors <- c(errors, pnl_errors)
    }

    # Vline validation
    for (i in seq_along(cfg$vlines)) {
        vl_errors <- validate_vline_full(cfg$vlines[[i]], i)
        errors <- c(errors, vl_errors)
    }

    # Color validation
    errors <- c(errors, validate_colors(cfg$colors))

    errors
}

#' Validate general section
#' @param cfg Full configuration
#' @return List of errors
#' @keywords internal
validate_general_section <- function(cfg) {
    errors <- list()

    # Title is optional but should be string if present
    if (!is.null(cfg$ui$title) && !is.character(cfg$ui$title)) {
        errors$title <- "Title must be a string"
    }

    # Span default validation
    if (!is.null(cfg$ui$span_default)) {
        if (!is.numeric(cfg$ui$span_default) || cfg$ui$span_default < 100) {
            errors$span_default <- "Span default must be numeric and >= 100"
        }
    }

    # Smooth window default validation
    if (!is.null(cfg$ui$smooth_window_default)) {
        if (!is.numeric(cfg$ui$smooth_window_default) || cfg$ui$smooth_window_default < 1) {
            errors$smooth_default <- "Smooth window default must be >= 1"
        }
    }

    errors
}

#' Validate plot section
#' @param plot Plot configuration
#' @return List of errors
#' @keywords internal
validate_plot_section <- function(plot) {
    errors <- list()

    if (is.null(plot)) {
        return(errors)
    }

    # Iterator validation
    if (!is.null(plot$iterator)) {
        if (!is.numeric(plot$iterator) || plot$iterator < 1) {
            errors$iterator <- "Iterator must be >= 1"
        }
    }

    # Expansion validation
    if (!is.null(plot$expansion)) {
        if (!is.numeric(plot$expansion) || plot$expansion < 0) {
            errors$expansion <- "Expansion must be >= 0"
        }
    }

    # Target points validation
    if (!is.null(plot$target_points)) {
        if (!is.numeric(plot$target_points) || plot$target_points < 100) {
            errors$target_points <- "Target points must be >= 100"
        }
    }

    # Extraction mode validation
    valid_modes <- c("fixed", "dynamic", "dynamic_smooth")
    if (!is.null(plot$extraction_mode) && !plot$extraction_mode %in% valid_modes) {
        errors$extraction_mode <- paste("Extraction mode must be one of:", paste(valid_modes, collapse = ", "))
    }

    errors
}

#' Validate navigator section
#' @param nav Navigator configuration
#' @return List of errors
#' @keywords internal
validate_navigator_section <- function(nav) {
    errors <- list()

    if (is.null(nav)) {
        return(errors)
    }

    # Source validation - allow misha intervals or a file path
    if (!is.null(nav$source) && nchar(nav$source) > 0) {
        source_ok <- intervals_exist(nav$source) || file.exists(nav$source)
        if (!source_ok) {
            errors$nav_source <- paste("Navigator source not found:", nav$source)
        }
    }

    # Extension validation
    if (!is.null(nav$extension)) {
        if (!is.numeric(nav$extension) || nav$extension < 0) {
            errors$nav_extension <- "Extension must be >= 0"
        }
    }

    errors
}

#' Validate a virtual track with comprehensive checks
#'
#' @param vtrack Vtrack configuration
#' @param index Index for error naming
#' @return List of errors
#' @keywords internal
validate_vtrack_full <- function(vtrack, index) {
    errors <- list()
    prefix <- paste0("vtrack_", index, "_")

    # Name required
    if (is_empty(vtrack$name)) {
        errors[[paste0(prefix, "name")]] <- "Vtrack name is required"
    } else if (!grepl("^[a-zA-Z][a-zA-Z0-9._]*$", vtrack$name)) {
        errors[[paste0(prefix, "name")]] <- "Invalid vtrack name (use letters, numbers, dots, underscores)"
    }

    # Determine vtrack type based on function
    func <- vtrack$func %||% "sum"
    vtype <- vtrack$vtype
    expression <- vtrack$expression %||% vtrack$expr
    if (is.null(vtype)) {
        if (!is.null(expression) && is.null(vtrack$src) && is.null(vtrack$func)) {
            vtype <- "expr"
        } else if (is_sequence_function(func)) {
            vtype <- "sequence"
        } else if (is_intervals_function(func)) {
            vtype <- "intervals"
        } else {
            vtype <- "standard"
        }
    }

    # All valid functions
    all_valid_funcs <- unlist(get_vtrack_function_choices())

    # Function validation
    if (!is.null(func) && !func %in% all_valid_funcs) {
        errors[[paste0(prefix, "func")]] <- paste("Invalid function:", func)
    }

    # Source validation based on function type
    if (vtype == "standard" || requires_source_track(func)) {
        # Standard vtracks need a source track
        if (is_empty(vtrack$src)) {
            errors[[paste0(prefix, "src")]] <- "Source track is required"
        } else if (!grepl("^@uploaded:", vtrack$src) && !track_exists(vtrack$src)) {
            errors[[paste0(prefix, "src")]] <- paste("Track not found:", vtrack$src)
        }
    } else if (vtype == "intervals" || is_intervals_function(func)) {
        # Intervals vtracks need an intervals source
        if (is_empty(vtrack$src)) {
            errors[[paste0(prefix, "src")]] <- "Intervals source is required"
        }
        # We don't validate existence of @uploaded: sources (they're session-scoped)
        # For database intervals, check existence
        if (!is_empty(vtrack$src) && !grepl("^@uploaded:", vtrack$src) && !grepl("^/", vtrack$src)) {
            if (!intervals_exist(vtrack$src)) {
                errors[[paste0(prefix, "src")]] <- paste("Intervals not found:", vtrack$src)
            }
        }
    }
    # Sequence functions don't need source validation

    # PWM function parameter validation
    if (is_pwm_function(func)) {
        params <- vtrack$params
        if (is.null(params) || is_empty(params$pssm)) {
            errors[[paste0(prefix, "pssm")]] <- "PSSM is required for PWM functions"
        }
    }

    # Kmer function parameter validation
    if (is_kmer_function(func)) {
        params <- vtrack$params
        if (is.null(params) || is_empty(params$kmer)) {
            errors[[paste0(prefix, "kmer")]] <- "K-mer sequence is required"
        } else if (!grepl("^[ACGTacgt]+$", params$kmer)) {
            errors[[paste0(prefix, "kmer")]] <- "K-mer must contain only A, C, G, T characters"
        }
    }

    # Expression validation (for pure expression vtracks)
    if (vtype == "expr") {
        if (is_empty(expression)) {
            errors[[paste0(prefix, "expression")]] <- "Expression is required for expression vtracks"
        } else {
            # Try to parse expression
            tryCatch(
                {
                    parse(text = expression)
                },
                error = function(e) {
                    errors[[paste0(prefix, "expression")]] <<- paste("Invalid expression:", e$message)
                }
            )
        }
    }

    # Wrapper expression validation
    if (!is_empty(expression) && !is.null(vtrack$name) && expression != vtrack$name) {
        tryCatch(
            {
                parse(text = expression)
            },
            error = function(e) {
                errors[[paste0(prefix, "expression")]] <<- paste("Invalid expression:", e$message)
            }
        )
    }

    # Shift validation
    if (!is.null(vtrack$sshift) && !is.numeric(vtrack$sshift)) {
        errors[[paste0(prefix, "sshift")]] <- "Start shift must be numeric"
    }
    if (!is.null(vtrack$eshift) && !is.numeric(vtrack$eshift)) {
        errors[[paste0(prefix, "eshift")]] <- "End shift must be numeric"
    }

    errors
}

#' Validate a panel with comprehensive checks
#'
#' @param panel Panel configuration
#' @param index Index for error naming
#' @param vtrack_names Character vector of defined vtrack names
#' @return List of errors
#' @keywords internal
validate_panel_full <- function(panel, index, vtrack_names = character(0)) {
    errors <- list()
    prefix <- paste0("panel_", index, "_")

    # Name required
    if (is_empty(panel$name)) {
        errors[[paste0(prefix, "name")]] <- "Panel name is required"
    }

    # Type validation
    valid_types <- c("data", "annotation", "ideogram", "intervals")
    if (is.null(panel$type) || !panel$type %in% valid_types) {
        errors[[paste0(prefix, "type")]] <- paste("Panel type must be one of:", paste(valid_types, collapse = ", "))
    }

    # Data panel specific validation
    if (!is.null(panel$type) && panel$type == "data") {
        # Track validation
        if (length(panel$tracks) == 0) {
            errors[[paste0(prefix, "tracks")]] <- "At least one track is required for data panels"
        } else {
            for (track in panel$tracks) {
                track_name <- if (is.character(track)) track else track$name
                if (!is.null(track_name)) {
                    # Check if track exists as vtrack or misha track
                    if (!track_name %in% vtrack_names && !track_exists(track_name)) {
                        errors[[paste0(prefix, "track_", track_name)]] <- paste("Track not found:", track_name)
                    }
                }
            }
        }

        # Grouping pattern validation
        if (!is.null(panel$grouping$pattern)) {
            tryCatch(
                {
                    grepl(panel$grouping$pattern, "test", perl = TRUE)
                },
                error = function(e) {
                    errors[[paste0(prefix, "pattern")]] <<- paste("Invalid regex pattern:", e$message)
                }
            )
        }

        # Plot type validation
        valid_plot_types <- c("line", "area", "point", "histogram")
        if (!is.null(panel$plot_type) && !panel$plot_type %in% valid_plot_types) {
            errors[[paste0(prefix, "plot_type")]] <- paste("Plot type must be one of:", paste(valid_plot_types, collapse = ", "))
        }

        # Y-limit validation
        if (!is.null(panel$ylim)) {
            if (!is.numeric(panel$ylim) || length(panel$ylim) != 2) {
                errors[[paste0(prefix, "ylim")]] <- "Y limits must be numeric vector of length 2"
            }
        }
    }

    # Annotation panel validation
    if (!is.null(panel$type) && panel$type == "annotation") {
        if (!is.null(panel$exon_source) && !intervals_exist(panel$exon_source)) {
            errors[[paste0(prefix, "exon_source")]] <- paste("Exon intervals not found:", panel$exon_source)
        }
        if (!is.null(panel$tss_source) && !intervals_exist(panel$tss_source)) {
            errors[[paste0(prefix, "tss_source")]] <- paste("TSS intervals not found:", panel$tss_source)
        }
    }

    # Intervals panel validation
    if (!is.null(panel$type) && panel$type == "intervals") {
        source_type <- panel$source %||% "intervals"
        if (source_type == "file") {
            file_path <- panel$file %||% ""
            if (is_empty(file_path)) {
                errors[[paste0(prefix, "file")]] <- "Intervals file path is required"
            } else if (!file.exists(file_path)) {
                errors[[paste0(prefix, "file")]] <- paste("Intervals file not found:", file_path)
            }
        } else {
            intervals_name <- panel$intervals %||% panel$source
            if (is_empty(intervals_name)) {
                errors[[paste0(prefix, "intervals")]] <- "Intervals name is required"
            } else if (!intervals_exist(intervals_name)) {
                errors[[paste0(prefix, "intervals")]] <- paste("Intervals not found:", intervals_name)
            }
        }
    }

    # Height validation
    if (!is.null(panel$height)) {
        if (!is.numeric(panel$height) || panel$height <= 0) {
            errors[[paste0(prefix, "height")]] <- "Height must be > 0"
        }
    }

    errors
}

#' Validate a vline configuration
#'
#' @param vline Vline configuration
#' @param index Index for error naming
#' @return List of errors
#' @keywords internal
validate_vline_full <- function(vline, index) {
    errors <- list()
    prefix <- paste0("vline_", index, "_")

    # Name required
    if (is_empty(vline$name)) {
        errors[[paste0(prefix, "name")]] <- "Vline name is required"
    }

    # Source validation
    valid_sources <- c("file", "intervals", "inline", "current", "misha")
    if (is.null(vline$source) || !vline$source %in% valid_sources) {
        errors[[paste0(prefix, "source")]] <- paste("Source must be one of:", paste(valid_sources, collapse = ", "))
    }

    # File source validation
    if (!is.null(vline$source) && vline$source == "file") {
        file_path <- vline$._resolved_file %||% vline$file
        if (is_empty(file_path)) {
            errors[[paste0(prefix, "file")]] <- "File path is required for file source"
        } else if (!file.exists(file_path)) {
            errors[[paste0(prefix, "file")]] <- paste("File not found:", file_path)
        }
    }

    # Intervals/misha source validation
    if (!is.null(vline$source) && vline$source %in% c("intervals", "misha")) {
        intervals_name <- vline$intervals %||% vline$misha
        if (is_empty(intervals_name)) {
            errors[[paste0(prefix, "intervals")]] <- "Intervals name is required"
        } else if (!intervals_exist(intervals_name)) {
            errors[[paste0(prefix, "intervals")]] <- paste("Intervals not found:", intervals_name)
        }
    }

    # Color validation (basic check)
    if (!is.null(vline$color)) {
        if (!is.character(vline$color)) {
            errors[[paste0(prefix, "color")]] <- "Color must be a string"
        }
    }

    # Linetype validation
    valid_linetypes <- c("solid", "dashed", "dotted", "longdash", "twodash", "dotdash")
    if (!is.null(vline$linetype) && !vline$linetype %in% valid_linetypes) {
        errors[[paste0(prefix, "linetype")]] <- paste("Linetype must be one of:", paste(valid_linetypes, collapse = ", "))
    }

    errors
}

#' Validate color mappings
#'
#' @param colors Named list of colors
#' @return List of errors
#' @keywords internal
validate_colors <- function(colors) {
    errors <- list()

    if (is.null(colors) || length(colors) == 0) {
        return(errors)
    }

    for (name in names(colors)) {
        color <- colors[[name]]
        if (!is.character(color) || length(color) != 1) {
            errors[[paste0("color_", name)]] <- paste("Invalid color for", name)
        }
        # Basic hex color validation
        if (is.character(color) && grepl("^#", color)) {
            if (!grepl("^#[0-9A-Fa-f]{6}$|^#[0-9A-Fa-f]{8}$", color)) {
                errors[[paste0("color_", name)]] <- paste("Invalid hex color:", color)
            }
        }
    }

    errors
}

#' Format validation errors for display
#'
#' @param errors List of validation errors
#' @return HTML formatted error list
#' @keywords internal
format_validation_errors <- function(errors) {
    if (length(errors) == 0) {
        return(NULL)
    }

    error_items <- sapply(names(errors), function(key) {
        shiny::tags$li(
            shiny::tags$strong(key), ": ", errors[[key]]
        )
    })

    shiny::div(
        class = "alert alert-danger",
        shiny::tags$strong("Validation Errors:"),
        shiny::tags$ul(error_items)
    )
}
