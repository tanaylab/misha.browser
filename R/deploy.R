# deploy.R - Local deployment helpers

#' Deploy a misha.browser app to a local directory
#'
#' Copies the bundled `app.R` and optionally a config file to a target
#' directory so it can be served by Shiny Server.
#'
#' @param dest_dir Target directory to deploy into.
#' @param config Path to a YAML config file to copy. When NULL, no config is copied.
#' @param config_name Filename to use in the destination (default: "config.yaml").
#' @param profile Profile name to set in the deployment environment (default: "server").
#' @param overwrite Logical, overwrite existing files if TRUE.
#' @param write_env Logical, write a `.Renviron` with MISHA_BROWSER_* vars.
#' @param extra_files Optional character vector of file paths to copy as-is.
#' @param touch_restart Logical, create or update `restart.txt` in destination.
#' @return Invisible list with deployed paths.
#' @export
browser_deploy_local <- function(dest_dir,
                                 config = NULL,
                                 config_name = "config.yaml",
                                 profile = "server",
                                 overwrite = FALSE,
                                 write_env = TRUE,
                                 extra_files = NULL,
                                 touch_restart = TRUE) {
    if (is.null(dest_dir) || !nzchar(dest_dir)) {
        cli::cli_abort("dest_dir must be a non-empty path.")
    }
    dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
    if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    }

    app_src <- system.file("app.R", package = "misha.browser")
    if (!nzchar(app_src) || !file.exists(app_src)) {
        # Dev fallback (source tree)
        app_src <- file.path("inst", "app.R")
    }
    if (!file.exists(app_src)) {
        cli::cli_abort("Could not locate inst/app.R to deploy.")
    }

    app_dest <- file.path(dest_dir, "app.R")
    if (file.exists(app_dest) && !isTRUE(overwrite)) {
        cli::cli_abort("app.R already exists at {app_dest}. Set overwrite=TRUE to replace.")
    }
    file.copy(app_src, app_dest, overwrite = TRUE)

    config_dest <- NULL
    if (!is.null(config)) {
        if (!file.exists(config)) {
            cli::cli_abort("Config file not found: {config}")
        }
        config_dest <- file.path(dest_dir, config_name)
        if (file.exists(config_dest) && !isTRUE(overwrite)) {
            cli::cli_abort("Config already exists at {config_dest}. Set overwrite=TRUE to replace.")
        }
        file.copy(config, config_dest, overwrite = TRUE)
    }

    extra_copied <- character(0)
    if (!is.null(extra_files)) {
        for (path in extra_files) {
            if (!file.exists(path)) {
                cli::cli_abort("Extra file not found: {path}")
            }
            dest_path <- file.path(dest_dir, basename(path))
            if (file.exists(dest_path) && !isTRUE(overwrite)) {
                cli::cli_abort("Extra file already exists at {dest_path}. Set overwrite=TRUE to replace.")
            }
            file.copy(path, dest_path, overwrite = TRUE)
            extra_copied <- c(extra_copied, dest_path)
        }
    }

    env_path <- NULL
    if (isTRUE(write_env)) {
        env_path <- file.path(dest_dir, ".Renviron")
        if (file.exists(env_path) && !isTRUE(overwrite)) {
            cli::cli_abort(".Renviron already exists at {env_path}. Set overwrite=TRUE to replace.")
        }
        env_lines <- c(
            sprintf("MISHA_BROWSER_PROFILE=%s", profile),
            sprintf("MISHA_BROWSER_CONFIG=%s", config_name)
        )
        writeLines(env_lines, env_path)
    }

    restart_path <- NULL
    if (isTRUE(touch_restart)) {
        restart_path <- file.path(dest_dir, "restart.txt")
        if (!file.exists(restart_path) || isTRUE(overwrite)) {
            writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), restart_path)
        } else {
            file.info(restart_path)
        }
    }

    invisible(list(
        app = app_dest,
        config = config_dest,
        env = env_path,
        extra = extra_copied,
        restart = restart_path
    ))
}
