# profile_startup.R -- Measure misha.browser startup timing breakdown
#
# Usage: Rscript dev/profile_startup.R [config_yaml] [output_json]
#
# This script instruments every major phase of misha.browser startup and writes
# a JSON timing breakdown to dev/startup_baseline.json (or a custom path).

suppressPackageStartupMessages({
    library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
config_yaml <- if (length(args) >= 1) args[1] else "dev/test_config.yaml"
output_json <- if (length(args) >= 2) args[2] else "dev/startup_baseline.json"

cat("=== misha.browser Startup Profiler ===\n")
cat("Config: ", config_yaml, "\n")
cat("Output: ", output_json, "\n\n")

if (!file.exists(config_yaml)) {
    stop("Configuration file not found: ", config_yaml)
}

timings <- list()
overall_start <- proc.time()[["elapsed"]]

# Helper: time a labelled expression, store in timings
timed <- function(label, expr) {
    cat(sprintf("[%s] starting...\n", label))
    start <- proc.time()[["elapsed"]]
    result <- tryCatch(force(expr), error = function(e) {
        cat(sprintf("[%s] ERROR: %s\n", label, conditionMessage(e)))
        NULL
    })
    elapsed_s <- proc.time()[["elapsed"]] - start
    elapsed_ms <- round(elapsed_s * 1000)
    timings[[label]] <<- elapsed_ms
    cat(sprintf("[%s] done: %d ms\n\n", label, elapsed_ms))
    invisible(result)
}

# --- Phase 1: devtools::load_all ---
timed("devtools_load_all", {
    devtools::load_all(quiet = TRUE)
})

# --- Phase 2: config_load (YAML parse + validation + profile resolution) ---
cfg <- timed("config_load", {
    misha.browser:::browser_load_config(config_yaml)
})

# --- Phase 3: gsetroot (misha database connection) ---
timed("gsetroot", {
    if (!is.null(cfg$._misha_root)) {
        misha::gsetroot(cfg$._misha_root)
    } else {
        cat("[gsetroot] SKIP: no misha_root in config\n")
    }
})

# --- Phase 4: vtrack initialization ---
# Build a minimal browser object to test init_vtracks in isolation
browser_stub <- structure(
    list(
        cfg = cfg,
        state = list(
            current_region = NULL,
            highlight = NULL,
            smooth_window = cfg$ui$smooth_window_default,
            active_tracks = NULL,
            vlines_enabled = sapply(cfg$vlines, function(v) v$enabled %||% TRUE)
        )
    ),
    class = "browser"
)

timed("vtrack_init", {
    misha.browser:::init_vtracks(browser_stub)
})

# --- Phase 5: start region setup ---
timed("start_region_setup", {
    misha.browser:::init_start_region(browser_stub)
})

# --- Phase 6: browser_create() end-to-end ---
# This re-does phases 2-5 as one unit; the total includes its own internal timing
browser_obj <- timed("browser_create_total", {
    misha.browser::browser_create(config = config_yaml)
})

# --- Phase 7: browser_ui() - UI construction ---
timed("browser_ui", {
    misha.browser:::browser_ui(browser_obj)
})

# --- Phase 8: browser_server() - server function construction ---
timed("browser_server", {
    misha.browser:::browser_server(browser_obj)
})

# --- Overall ---
overall_ms <- round((proc.time()[["elapsed"]] - overall_start) * 1000)

# Build result
result <- list(
    baseline = timings,
    totalStartupMs = overall_ms,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    config = normalizePath(config_yaml, mustWork = FALSE),
    hostname = Sys.info()[["nodename"]],
    r_version = paste(R.version$major, R.version$minor, sep = ".")
)

# Write JSON
dir.create(dirname(output_json), showWarnings = FALSE, recursive = TRUE)
writeLines(toJSON(result, auto_unbox = TRUE, pretty = TRUE), output_json)

cat("\n=== Summary ===\n")
# Sort by time descending
sorted <- sort(unlist(timings), decreasing = TRUE)
for (nm in names(sorted)) {
    cat(sprintf("  %-30s %7d ms\n", nm, sorted[[nm]]))
}
cat(sprintf("\n  %-30s %7d ms\n", "TOTAL (wall clock)", overall_ms))
cat("\nResults written to:", output_json, "\n")
