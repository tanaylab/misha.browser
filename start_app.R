#!/usr/bin/env Rscript
# start_app.R - Command-line entry point for misha.browser
#
# Usage: Rscript start_app.R <config_yaml> [port] [host] [profile]
#
# Arguments:
#   config_yaml  Path to YAML configuration file (required)
#   port         Port number (default: 3838)
#   host         Host address (default: "0.0.0.0")
#   profile      Configuration profile name (default: auto-detect)
#
# Examples:
#   Rscript start_app.R dev/test_config.yaml
#   Rscript start_app.R dev/test_config.yaml 8080
#   Rscript start_app.R dev/test_config.yaml 8080 127.0.0.1 local

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
    cat("Usage: Rscript start_app.R <config_yaml> [port] [host] [profile]\n")
    cat("\n")
    cat("Arguments:\n")
    cat("  config_yaml  Path to YAML configuration file (required)\n")
    cat("  port         Port number (default: 3838)\n")
    cat("  host         Host address (default: \"0.0.0.0\")\n")
    cat("  profile      Configuration profile name (default: auto-detect)\n")
    quit(status = 1)
}

config_file <- args[1]
port <- if (length(args) >= 2) as.integer(args[2]) else 3838L
host <- if (length(args) >= 3) args[3] else "0.0.0.0"
profile <- if (length(args) >= 4) args[4] else NULL

if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file)
}

cat("Starting misha.browser\n")
cat("  Config:  ", config_file, "\n")
cat("  Port:    ", port, "\n")
cat("  Host:    ", host, "\n")
cat("  Profile: ", if (is.null(profile)) "(auto-detect)" else profile, "\n")
cat("\n")

library(misha.browser)

misha.browser::browser_launch(
    config = config_file,
    port = port,
    host = host,
    profile = profile
)
