# config.R - Configuration loading and validation for misha.browser

#' Load browser configuration from YAML file
#'
#' Loads and validates a YAML configuration file for the genome browser.
#' The configuration can contain multiple profiles (e.g., "local" and "server")
#' with different misha database paths and settings. If no profile is specified,
#' the function auto-detects based on which paths exist.
#'
#' @param file Path to YAML configuration file
#' @param profile Profile name to use (default: auto-detect based on available paths).
#'   Common profiles are "local" and "server".
#' @return Parsed and validated configuration list with resolved paths and defaults.
#'   Internal fields (prefixed with `._`) contain resolved paths and metadata.
#'
#' @details
#' The configuration file should contain:
#' \itemize{
#'   \item \code{profiles}: Environment-specific settings (misha_root, base_dir, data_dir)
#'   \item \code{panels}: Panel definitions for data visualization
#'   \item \code{vtracks}: Virtual track definitions
#'   \item \code{vlines}: Vertical line annotations
#'   \item \code{plot}: Plot settings (iterator, extraction_mode, theme)
#'   \item \code{ui}: User interface settings (title, defaults)
#'   \item \code{navigator}: Gene navigator configuration
#'   \item \code{start}: Initial region settings
#' }
#'
#' @seealso \code{\link{browser_save_config}} to save configuration,
#'   \code{\link{browser_create_config}} to create configuration programmatically,
#'   \code{\link{browser_create}} to create a browser from configuration
#'
#' @export
#' @examples
#' \dontrun{
#' # Load with auto-detected profile
#' cfg <- browser_load_config("my_browser.yaml")
#'
#' # Load with specific profile
#' cfg <- browser_load_config("my_browser.yaml", profile = "server")
#'
#' # Use loaded config to create browser
#' browser <- browser_create(config = "my_browser.yaml")
#' }
browser_load_config <- function(file, profile = NULL) {
    if (!file.exists(file)) {
        cli::cli_abort("Configuration file not found: {file}")
    }

    cfg <- yaml::read_yaml(file)
    cfg <- validate_config(cfg)
    cfg <- resolve_profile(cfg, profile, dirname(file))
    cfg$._config_file <- normalizePath(file)
    cfg
}

#' Resolve profile and paths
#'
#' @param cfg Configuration list
#' @param profile Profile name (NULL for auto-detect)
#' @param config_dir Directory containing config file
#' @return Configuration with resolved profile
#' @keywords internal
resolve_profile <- function(cfg, profile = NULL, config_dir = ".") {
    profiles <- cfg$profiles %||% list()

    # Auto-detect profile if not specified
    if (is.null(profile) || !profile %in% names(profiles)) {
        # Try server first, then local
        if ("server" %in% names(profiles) &&
            !is.null(profiles$server$misha_root) &&
            dir.exists(profiles$server$misha_root)) {
            profile <- "server"
        } else if ("local" %in% names(profiles)) {
            profile <- "local"
        } else if (length(profiles) > 0) {
            profile <- names(profiles)[1]
        } else {
            profile <- NULL
        }
    }

    if (!is.null(profile) && profile %in% names(profiles)) {
        profile_cfg <- profiles[[profile]]
        cfg$._profile <- profile
        cfg$._profile_cfg <- profile_cfg

        # Resolve paths relative to config directory
        base_dir <- resolve_path(config_dir, profile_cfg$base_dir %||% ".")
        data_dir <- resolve_path(base_dir, profile_cfg$data_dir %||% ".")

        cfg$._base_dir <- base_dir
        cfg$._data_dir <- data_dir
        # Check for server root override from environment variable
        server_root <- Sys.getenv("MISHA_BROWSER_SERVER_ROOT", "")
        if (nzchar(server_root)) {
            cfg$._misha_root <- server_root
        } else {
            cfg$._misha_root <- profile_cfg$misha_root
        }

        # Resolve file paths in vlines
        if (!is.null(cfg$vlines)) {
            cfg$vlines <- lapply(cfg$vlines, function(vl) {
                if (!is.null(vl$file)) {
                    vl$._resolved_file <- resolve_path(data_dir, vl$file)
                }
                vl
            })
        }

        # Resolve file paths in panels (e.g., intervals panels with file source)
        if (!is.null(cfg$panels)) {
            cfg$panels <- lapply(cfg$panels, function(panel) {
                if (!is.null(panel$file)) {
                    panel$._resolved_file <- resolve_path(data_dir, panel$file)
                }
                panel
            })
        }
    }

    cfg
}

#' Validate configuration structure
#'
#' @param cfg Configuration list
#' @return Validated configuration (with defaults filled in)
#' @keywords internal
validate_config <- function(cfg) {
    # Ensure required sections exist
    cfg$panels <- cfg$panels %||% list()
    cfg$vtracks <- cfg$vtracks %||% list()
    cfg$vlines <- cfg$vlines %||% list()
    cfg$plot <- cfg$plot %||% list()
    cfg$ui <- cfg$ui %||% list()
    cfg$navigator <- cfg$navigator %||% list()
    cfg$start <- cfg$start %||% list()

    # Fill in defaults for plot settings
    cfg$plot$iterator <- cfg$plot$iterator %||% .DEFAULT_ITERATOR
    cfg$plot$expansion <- cfg$plot$expansion %||% .DEFAULT_EXPANSION
    cfg$plot$theme <- cfg$plot$theme %||% .DEFAULT_THEME
    cfg$plot$target_points <- cfg$plot$target_points %||% .DEFAULT_TARGET_POINTS

    # Fill in defaults for UI settings
    cfg$ui$title <- cfg$ui$title %||% .DEFAULT_TITLE
    cfg$ui$span_default <- cfg$ui$span_default %||% .DEFAULT_SPAN
    cfg$ui$smooth_window_default <- cfg$ui$smooth_window_default %||% .DEFAULT_SMOOTH_WINDOW
    cfg$ui$show_coordinates <- cfg$ui$show_coordinates %||% TRUE

    # Fill in defaults for navigator
    cfg$navigator$source <- cfg$navigator$source %||% .DEFAULT_NAV_SOURCE
    cfg$navigator$label_field <- cfg$navigator$label_field %||% .DEFAULT_NAV_LABEL_FIELD
    cfg$navigator$extension <- cfg$navigator$extension %||% .DEFAULT_NAV_EXTENSION

    # Validate panels
    cfg$panels <- lapply(seq_along(cfg$panels), function(i) {
        validate_panel(cfg$panels[[i]], i)
    })

    # Validate vtracks
    cfg$vtracks <- lapply(seq_along(cfg$vtracks), function(i) {
        validate_vtrack(cfg$vtracks[[i]], i)
    })

    # Validate vlines
    cfg$vlines <- lapply(seq_along(cfg$vlines), function(i) {
        validate_vline(cfg$vlines[[i]], i)
    })

    cfg
}

#' Validate a panel configuration
#'
#' @param panel Panel config list
#' @param index Panel index for error messages
#' @return Validated panel config
#' @keywords internal
validate_panel <- function(panel, index) {
    # Panel must have a name
    if (is.null(panel$name)) {
        panel$name <- paste0("panel_", index)
    }

    # Determine panel type
    panel$type <- panel$type %||% "data"

    if (panel$type == "data") {
        # Data panels need tracks
        if (is.null(panel$tracks) || length(panel$tracks) == 0) {
            cli::cli_warn("Panel '{panel$name}' has no tracks defined")
            panel$tracks <- character(0)
        }

        # Fill in grouping defaults
        if (!is.null(panel$grouping)) {
            panel$grouping$pattern <- panel$grouping$pattern %||% .DEFAULT_GROUPING_PATTERN
            panel$grouping$color_by <- panel$grouping$color_by %||% "source"
        }

        # Normalize ylim if present (YAML may parse as list)
        if (!is.null(panel$ylim) && is.list(panel$ylim)) {
            panel$ylim <- unlist(panel$ylim, use.names = FALSE)
        }

        # Fill in display defaults
        panel$plot_type <- panel$plot_type %||% "line"
        panel$alpha <- panel$alpha %||% .DEFAULT_ALPHA
        panel$linewidth <- panel$linewidth %||% .DEFAULT_LINEWIDTH
        panel$height <- panel$height %||% .DEFAULT_DATA_PANEL_HEIGHT
        panel$show_legend <- panel$show_legend %||% TRUE
        panel$show_name <- panel$show_name %||% FALSE

        # Transforms default to empty list
        panel$transforms <- panel$transforms %||% list()

        # Validate hlines if present
        if (!is.null(panel$hlines) && length(panel$hlines) > 0) {
            panel$hlines <- lapply(seq_along(panel$hlines), function(i) {
                validate_hline(panel$hlines[[i]], i, panel$name)
            })
        } else {
            panel$hlines <- list()
        }
    } else if (panel$type == "annotation") {
        panel$exon_source <- panel$exon_source %||% "intervs.global.exon"
        panel$tss_source <- panel$tss_source %||% "intervs.global.tss"
        panel$gene_label_field <- panel$gene_label_field %||% "geneSymbol"
        panel$color <- panel$color %||% "navy"
        panel$show_strand_arrows <- panel$show_strand_arrows %||% TRUE
        panel$height <- panel$height %||% .DEFAULT_ANNOTATION_HEIGHT
    } else if (panel$type == "intervals") {
        panel$source <- panel$source %||% "intervals"
        panel$intervals <- panel$intervals %||% panel$source
        panel$color <- panel$color %||% "grey60"
        panel$outline_color <- panel$outline_color %||% "grey20"
        panel$height <- panel$height %||% .DEFAULT_INTERVALS_HEIGHT
        panel$show_labels <- panel$show_labels %||% FALSE
        panel$show_direction <- panel$show_direction %||% FALSE
        panel$direction_field <- panel$direction_field %||% "strand"
    } else if (panel$type == "ideogram") {
        panel$highlight_current <- panel$highlight_current %||% TRUE
        panel$height <- panel$height %||% .DEFAULT_IDEOGRAM_HEIGHT
    } else if (panel$type == "ggplot") {
        if (is.null(panel$plot) || !inherits(panel$plot, "ggplot")) {
            cli::cli_abort(
                "Panel '{panel$name}' has type='ggplot' but `plot` is missing or not a ggplot object."
            )
        }
        panel$height <- panel$height %||% .DEFAULT_DATA_PANEL_HEIGHT
        panel$show_name <- panel$show_name %||% FALSE
    }

    # Pre-compute cache signature for data panels (avoids repeated digest calls)
    if (panel$type == "data") {
        panel$._cache_signature <- digest::digest(list(
            transforms = panel$transforms,
            grouping = panel$grouping,
            facet_by = panel$facet_by,
            plot_type = panel$plot_type,
            smooth_window = panel$smooth_window,
            raw = panel$raw
        ))
    }

    panel
}

#' Validate a horizontal line configuration
#'
#' @param hline Hline config list
#' @param index Hline index for error messages
#' @param panel_name Panel name for error messages
#' @return Validated hline config
#' @keywords internal
validate_hline <- function(hline, index, panel_name = "unknown") {
    # Must have either y or stat
    has_y <- !is.null(hline$y)
    has_stat <- !is.null(hline$stat)

    if (!has_y && !has_stat) {
        # Check for common YAML mistake: y parsed as boolean TRUE
        if (!is.null(hline$`TRUE`)) {
            cli::cli_abort(
                "Panel '{panel_name}' hline[{index}]: YAML parsed 'y:' as boolean. Use quoted key: '\"y\": {hline$`TRUE`}'"
            )
        }
        cli::cli_abort("Panel '{panel_name}' hline[{index}]: must have either 'y' or 'stat'")
    }
    if (has_y && has_stat) {
        cli::cli_abort("Panel '{panel_name}' hline[{index}]: cannot have both 'y' and 'stat'")
    }

    # Validate stat type
    if (has_stat) {
        valid_stats <- c("mean", "median", "quantile", "quantile_global")
        if (!hline$stat %in% valid_stats) {
            cli::cli_abort(
                "Panel '{panel_name}' hline[{index}]: stat must be one of: {paste(valid_stats, collapse=', ')}"
            )
        }
        if (hline$stat %in% c("quantile", "quantile_global")) {
            if (is.null(hline$q)) {
                cli::cli_abort("Panel '{panel_name}' hline[{index}]: stat='{hline$stat}' requires 'q' parameter")
            }
            if (!is.numeric(hline$q) || hline$q < 0 || hline$q > 1) {
                cli::cli_abort("Panel '{panel_name}' hline[{index}]: 'q' must be between 0 and 1")
            }
        }
    }

    # Validate y is numeric
    if (has_y && !is.numeric(hline$y)) {
        cli::cli_abort("Panel '{panel_name}' hline[{index}]: y must be numeric")
    }

    # Set defaults
    hline$color <- hline$color %||% "grey50"
    hline$linetype <- hline$linetype %||% "dashed"
    hline$linewidth <- hline$linewidth %||% .DEFAULT_HLINE_LINEWIDTH

    hline
}

#' Validate a virtual track configuration
#'
#' @param vtrack Vtrack config list
#' @param index Vtrack index for error messages
#' @return Validated vtrack config
#' @keywords internal
validate_vtrack <- function(vtrack, index) {
    if (is.null(vtrack$name)) {
        cli::cli_abort("Virtual track {index} missing 'name'")
    }

    # Support both 'src' and 'track' as source key (src is preferred)
    if (!is.null(vtrack$track) && is.null(vtrack$src)) {
        vtrack$src <- vtrack$track
        vtrack$track <- NULL
    }

    # Legacy alias: expr -> expression
    if (!is.null(vtrack$expr) && is.null(vtrack$expression)) {
        vtrack$expression <- vtrack$expr
    }
    vtrack$expr <- NULL

    # Determine vtrack type
    if (!is.null(vtrack$expression) && is.null(vtrack$src) && is.null(vtrack$func)) {
        # Pure expression-based vtrack
        vtrack$vtype <- "expr"
    } else if (is.null(vtrack$src)) {
        # Sequence-based vtrack (no source needed for kmer, pwm, masked)
        vtrack$vtype <- "sequence"
        # These functions don't need a source
        seq_funcs <- c(
            "kmer.count", "kmer.frac",
            "pwm", "pwm.max", "pwm.max.pos", "pwm.count",
            "masked.count", "masked.frac"
        )
        if (!vtrack$func %in% seq_funcs) {
            cli::cli_abort("Virtual track '{vtrack$name}' missing 'src' (required for func '{vtrack$func}')")
        }
    } else {
        # Standard vtrack with source
        vtrack$vtype <- "standard"
    }

    # Default function
    if (is.null(vtrack$func) && vtrack$vtype != "expr") {
        vtrack$func <- "sum"
    }

    # Handle iterator shifts - support both nested and flat format
    if (!is.null(vtrack$iterator)) {
        # Nested format: iterator: { sshift: -140, eshift: 140 }
        if (is.null(vtrack$sshift)) vtrack$sshift <- vtrack$iterator$sshift
        if (is.null(vtrack$eshift)) vtrack$eshift <- vtrack$iterator$eshift
        if (is.null(vtrack$dim)) vtrack$dim <- vtrack$iterator$dim
    }

    # Normalize params to list if single value
    if (!is.null(vtrack$params) && !is.list(vtrack$params)) {
        vtrack$params <- list(vtrack$params)
    }

    # Standard and sequence vtracks default to identity extraction via their name.
    if (is.null(vtrack$expression)) {
        if (vtrack$vtype == "expr") {
            cli::cli_abort("Virtual track '{vtrack$name}' missing 'expression'")
        }
        vtrack$expression <- vtrack$name
    }

    # Pre-compute transform signature for caching (avoids repeated digest calls)
    if (!is.null(vtrack$transforms) && length(vtrack$transforms) > 0) {
        vtrack$._transform_signature <- digest::digest(vtrack$transforms)
    }

    vtrack
}

#' Validate a vertical line configuration
#'
#' @param vline Vline config list
#' @param index Vline index for error messages
#' @return Validated vline config
#' @keywords internal
validate_vline <- function(vline, index) {
    if (is.null(vline$name)) {
        vline$name <- paste0("vline_", index)
    }

    vline$source <- vline$source %||% "file"
    vline$color <- vline$color %||% "grey50"
    vline$linetype <- vline$linetype %||% "dashed"
    vline$show_bounds <- vline$show_bounds %||% TRUE
    vline$enabled <- vline$enabled %||% TRUE

    # Parse inline interval strings
    if (vline$source == "inline" && !is.null(vline$intervals)) {
        vline$intervals <- lapply(vline$intervals, function(iv) {
            if (is.character(iv)) {
                parse_coords(iv)
            } else {
                iv
            }
        })
        # Remove NULL entries (failed parses)
        vline$intervals <- vline$intervals[!sapply(vline$intervals, is.null)]
    }

    vline
}

#' Create a new browser configuration programmatically
#'
#' @param misha_root Path to misha database root
#' @param title Browser title
#' @return Configuration list
#' @export
browser_create_config <- function(misha_root = NULL, title = "Genome Browser") {
    # Resolve misha_root from parameter or environment
    resolved_misha_root <- misha_root %||% Sys.getenv("MISHA_ROOT", "")

    cfg <- list(
        profiles = list(
            local = list(
                misha_root = resolved_misha_root
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
            title = title,
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

    cfg <- validate_config(cfg)

    # Set internal misha_root field (used by browser_create to call gsetroot)
    # This is normally done by resolve_profile when loading from YAML,
    # but we need to set it directly when creating config programmatically
    if (nzchar(resolved_misha_root)) {
        cfg$._misha_root <- resolved_misha_root
    }

    cfg
}

#' Save configuration to YAML file
#'
#' Saves the browser configuration to a YAML file. Internal fields (those
#' starting with `._`) are automatically removed before saving, so the
#' resulting file is clean and portable.
#'
#' @param cfg Configuration list (typically from a browser object via `browser$cfg`)
#' @param file Output file path (should end with .yaml or .yml)
#' @return Invisibly returns the original configuration (for piping)
#'
#' @details
#' The saved configuration can be loaded later with \code{\link{browser_load_config}}
#' or used directly with \code{\link{browser_create}}.
#'
#' Internal fields that are removed include:
#' \itemize{
#'   \item \code{._config_file}: Original config file path
#'   \item \code{._misha_root}: Resolved misha database path
#'   \item \code{._profile}: Active profile name
#'   \item \code{._cache_signature}: Pre-computed cache signatures
#' }
#'
#' @seealso \code{\link{browser_load_config}} to load configuration,
#'   \code{\link{browser_create_config}} to create configuration programmatically
#'
#' @export
#' @examples
#' \dontrun{
#' # Save browser configuration
#' browser_save_config(browser$cfg, "my_browser.yaml")
#'
#' # Modify and save
#' browser <- browser_add_panel(browser, name = "new_panel", tracks = "my_track")
#' browser_save_config(browser$cfg, "updated_config.yaml")
#' }
browser_save_config <- function(cfg, file) {
    # Remove internal fields before saving
    cfg_clean <- clean_config_for_export(cfg)

    yaml::write_yaml(cfg_clean, file)
    cli::cli_alert_success("Configuration saved to {file}")
    invisible(cfg)
}

#' Clean configuration for YAML export
#'
#' Removes internal fields (those starting with .) from the configuration
#' before saving to YAML.
#'
#' @param cfg Configuration list
#' @return Cleaned configuration list
#' @keywords internal
clean_config_for_export <- function(cfg) {
    # Remove internal fields that start with .
    remove_internal <- function(x) {
        if (is.list(x)) {
            # Remove fields starting with .
            to_remove <- grep("^\\.", names(x), value = TRUE)
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

#' Get panel by name
#'
#' @param cfg Configuration list
#' @param name Panel name
#' @return Panel configuration or NULL
#' @keywords internal
get_panel <- function(cfg, name) {
    for (panel in cfg$panels) {
        if (panel$name == name) {
            return(panel)
        }
    }
    NULL
}

#' Set panel by name
#'
#' @param cfg Configuration list
#' @param name Panel name
#' @param panel New panel configuration
#' @return Updated configuration
#' @keywords internal
set_panel <- function(cfg, name, panel) {
    for (i in seq_along(cfg$panels)) {
        if (cfg$panels[[i]]$name == name) {
            cfg$panels[[i]] <- panel
            return(cfg)
        }
    }
    # Panel not found, append
    cfg$panels <- c(cfg$panels, list(panel))
    cfg
}
