# Shiny Server entrypoint for misha.browser

config_path <- Sys.getenv("MISHA_BROWSER_CONFIG", "config.yaml")
if (!file.exists(config_path)) {
    fallback <- file.path("inst", "examples", "silicus.yaml")
    if (file.exists(fallback)) {
        config_path <- fallback
    }
}

profile <- Sys.getenv("MISHA_BROWSER_PROFILE", "server")
port <- as.integer(Sys.getenv("SHINY_PORT", "3838"))
host <- Sys.getenv("SHINY_HOST", "0.0.0.0")

misha.browser::browser_launch(
    config = config_path,
    profile = profile,
    host = host,
    port = port
)
