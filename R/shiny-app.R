# shiny-app.R - Shiny app launcher for misha.browser

#' Run the interactive genome browser
#'
#' Launches a Shiny web application for interactive genome browsing.
#'
#' @param browser Browser object (created with browser_create)
#' @param port Port number to run on (default 8911)
#' @param host Host to bind to (default "0.0.0.0" for all interfaces)
#' @param launch.browser Whether to open browser automatically (default TRUE)
#' @return Shiny app object (runs interactively)
#' @export
#' @examples
#' \dontrun{
#' browser <- browser_create(config = "my_browser.yaml")
#' browser_run(browser)
#'
#' # Or specify port
#' browser_run(browser, port = 8080)
#' }
browser_run <- function(browser, port = .DEFAULT_PORT, host = .DEFAULT_HOST, launch.browser = TRUE) {
    if (!inherits(browser, "browser")) {
        cli::cli_abort("browser_run requires a browser object created with browser_create()")
    }

    options(shiny.maxRequestSize = .DEFAULT_MAX_UPLOAD_BYTES)

    # Create UI and server
    profile <- getOption("misha.browser.profile", FALSE)
    run_timings <- list()
    t_run <- Sys.time()

    t_ui <- Sys.time()
    ui <- browser_ui(browser)
    run_timings$ui <- as.numeric(difftime(Sys.time(), t_ui, units = "secs"))

    t_server <- Sys.time()
    server <- browser_server(browser)
    run_timings$server <- as.numeric(difftime(Sys.time(), t_server, units = "secs"))
    run_timings$total <- as.numeric(difftime(Sys.time(), t_run, units = "secs"))

    # Display startup message
    cli::cli_h1("Starting {browser$cfg$ui$title %||% 'Genome Browser'}")
    cli::cli_text("Access at: http://{host}:{port}")

    region <- browser$state$current_region
    if (!is.null(region)) {
        cli::cli_text("Initial region: {format_coords(region$chrom, region$start, region$end)}")
    }

    if (profile) {
        startup <- browser$state$startup_timings %||% list()
        cli::cli_h3("Browser Startup Profiling")
        if (!is.null(startup$total)) {
            cli::cli_text("Create total: {round(startup$total, 3)}s")
        }
        if (!is.null(startup$config)) {
            cli::cli_text("Config load: {round(startup$config, 3)}s")
        }
        if (!is.null(startup$set_root)) {
            cli::cli_text("Set root: {round(startup$set_root, 3)}s")
        }
        if (!is.null(startup$vtracks)) {
            cli::cli_text("Init vtracks: {round(startup$vtracks, 3)}s")
        }
        if (!is.null(startup$start_region)) {
            cli::cli_text("Init region: {round(startup$start_region, 3)}s")
        }
        cli::cli_text("UI: {round(run_timings$ui, 3)}s")
        cli::cli_text("Server: {round(run_timings$server, 3)}s")
        cli::cli_text("Run prep total: {round(run_timings$total, 3)}s")
    }

    # Launch app
    app <- shiny::shinyApp(ui = ui, server = server)
    shiny::runApp(app, host = host, port = port, launch.browser = launch.browser)
}

#' Quick launch browser from config file
#'
#' Convenience function to create and run a browser in one step.
#'
#' @param config Path to YAML configuration file
#' @param port Port number
#' @param host Host to bind to
#' @param profile Profile name to use
#' @param profiling Enable profiling output (default: FALSE)
#' @param disk_cache Enable disk caching (default: FALSE)
#' @param cache_dir Directory for disk cache
#' @return Shiny app object
#' @export
#' @examples
#' \dontrun{
#' browser_launch("my_browser.yaml")
#'
#' # With profiling enabled
#' browser_launch("my_browser.yaml", profiling = TRUE)
#' }
browser_launch <- function(config, port = .DEFAULT_PORT, host = .DEFAULT_HOST, profile = NULL,
                           profiling = FALSE, disk_cache = FALSE, cache_dir = NULL) {
    # Set options
    if (profiling) {
        options(misha.browser.profile = TRUE)
    }
    if (disk_cache) {
        browser_cache_config(dir = cache_dir, enabled = TRUE)
    }

    browser <- browser_create(config = config, profile = profile)
    browser_run(browser, port = port, host = host)
}
