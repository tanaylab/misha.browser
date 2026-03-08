# profile_page_loads.R - Measure navigation interaction times for misha.browser
#
# Launches the misha.browser Shiny app with headless Chrome, performs
# navigation interactions (zoom, pan, etc.), and records the time from
# click to Shiny idle for each action.
# Results are saved as JSON to dev/page_load_baseline.json.
#
# Usage: Rscript dev/profile_page_loads.R
#   (Run from the package root)

library(jsonlite)

pkg_root <- "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/misha.browser"
setwd(pkg_root)

# Load package and helpers
devtools::load_all(pkg_root, quiet = TRUE)
source(file.path(pkg_root, "tests", "testthat", "helper-browser.R"))

# Output paths
output_json <- file.path(pkg_root, "dev", "page_load_baseline.json")
screenshot_dir <- .SCREENSHOT_DIR
if (!dir.exists(screenshot_dir)) {
    dir.create(screenshot_dir, recursive = TRUE)
}

# Navigation interactions to profile
# Each entry: list(name, action_type, id)
#   action_type: "click" for click_button, "coord_search" for set_shiny_input
interactions <- list(
    list(name = "Zoom In",        action = "click", id = "zoom_in"),
    list(name = "Zoom Out",       action = "click", id = "zoom_out"),
    list(name = "Move Right",     action = "click", id = "mv_right_small"),
    list(name = "Move Left",      action = "click", id = "mv_left_small"),
    list(name = "Move Right Big", action = "click", id = "mv_right_big"),
    list(name = "Move Left Big",  action = "click", id = "mv_left_big"),
    list(name = "Coord Search",   action = "coord_search", value = "chr5:1-2000000")
)

results <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    initialLoadMs = NA_real_,
    interactionTimings = list(),
    errors = list(),
    totalProfileMs = NA_real_
)

total_start <- proc.time()["elapsed"]

cat("=== misha.browser Page Load Profiler ===\n")
cat("Package root :", pkg_root, "\n")
cat("Config       :", .TEST_CONFIG, "\n")
cat("Interactions :", length(interactions), "\n\n")

# --- Launch app ---
cat("[1/4] Launching misha.browser in background...\n")
port <- httpuv::randomPort()
bg <- launch_browser_bg(.TEST_CONFIG, port, pkg_root)
cat("  Port:", port, "\n")

# --- Wait for HTTP readiness ---
cat("[2/4] Waiting for HTTP 200...\n")
http_start <- proc.time()["elapsed"]
app_ready <- wait_for_app(port, timeout = 180, bg_process = bg)
http_elapsed <- (proc.time()["elapsed"] - http_start) * 1000

if (!app_ready) {
    cat("FATAL: App did not become HTTP-ready within 180s\n")
    if (bg$is_alive()) {
        err <- tryCatch(bg$read_error(), error = function(e) "")
        cat("Background stderr:\n", err, "\n")
        bg$kill()
    }
    quit(status = 1)
}
cat("  HTTP ready in", round(http_elapsed), "ms\n")

# --- Connect browser ---
cat("[3/4] Connecting headless Chrome...\n")
session <- connect_browser(port)

# --- Wait for initial Shiny idle ---
cat("[4/4] Waiting for initial Shiny idle...\n")
idle_start <- proc.time()["elapsed"]
initial_idle <- wait_for_shiny_idle(session, timeout = 120)
idle_elapsed <- (proc.time()["elapsed"] - idle_start) * 1000

if (!initial_idle) {
    cat("WARNING: Shiny did not reach idle state within 120s\n")
    results$initialLoadMs <- -1
} else {
    results$initialLoadMs <- round(idle_elapsed)
    cat("  Initial idle in", round(idle_elapsed), "ms\n")
}

# Take screenshot of initial state
take_screenshot(session, "profile-initial")
cat("\n")

# --- Profile each interaction ---
cat("=== Profiling", length(interactions), "interactions ===\n\n")

for (interaction in interactions) {
    iname <- interaction$name
    cat(sprintf("  %-20s ... ", iname))

    action_start <- proc.time()["elapsed"]

    action_ok <- tryCatch(
        {
            if (interaction$action == "click") {
                # Check if the button exists first
                btn_exists <- element_exists(session, paste0("#", interaction$id))
                if (!btn_exists) {
                    results$errors[[iname]] <- paste("Button not found:", interaction$id)
                    FALSE
                } else {
                    clicked <- click_button(session, interaction$id)
                    isTRUE(clicked)
                }
            } else if (interaction$action == "coord_search") {
                # For coordinate search, check if the input exists
                input_exists <- element_exists(session, "#coord_search")
                if (!input_exists) {
                    results$errors[[iname]] <- "coord_search input not found"
                    FALSE
                } else {
                    # Set the input value and trigger the search
                    set_ok <- set_shiny_input(session, "coord_search", interaction$value)
                    if (isTRUE(set_ok)) {
                        # Trigger the search button event
                        click_button(session, "coord_search_search")
                    } else {
                        results$errors[[iname]] <- "Failed to set coord_search value"
                        FALSE
                    }
                }
            } else {
                results$errors[[iname]] <- paste("Unknown action type:", interaction$action)
                FALSE
            }
        },
        error = function(e) {
            results$errors[[iname]] <<- paste("Error:", e$message)
            FALSE
        }
    )

    action_elapsed <- (proc.time()["elapsed"] - action_start) * 1000

    if (!action_ok) {
        cat(sprintf("FAILED (%.0f ms)\n", action_elapsed))
        results$interactionTimings[[iname]] <- round(action_elapsed)
        results$errors[[iname]] <- results$errors[[iname]] %||% "Action returned FALSE"
        next
    }

    # Extra settle time for rendering
    Sys.sleep(1)
    action_elapsed <- (proc.time()["elapsed"] - action_start) * 1000

    # Take screenshot for verification
    screenshot_name <- paste0("profile-", gsub("[^a-zA-Z0-9]", "_", tolower(iname)))
    tryCatch(
        take_screenshot(session, screenshot_name),
        error = function(e) {
            cat("[screenshot failed] ")
        }
    )

    results$interactionTimings[[iname]] <- round(action_elapsed)
    cat(sprintf("%.0f ms\n", action_elapsed))
}

# --- Total time ---
total_elapsed <- (proc.time()["elapsed"] - total_start) * 1000
results$totalProfileMs <- round(total_elapsed)

cat(sprintf("\n=== Total profiling time: %.1f s ===\n\n", total_elapsed / 1000))

# --- Cleanup ---
cat("Cleaning up...\n")
cleanup_browser_test(session, bg)

# --- Write JSON output ---
cat("Writing results to", output_json, "\n")
writeLines(toJSON(results, auto_unbox = TRUE, pretty = TRUE), output_json)

# --- Print summary table ---
cat("\n=== RESULTS SUMMARY ===\n\n")
cat(sprintf("Initial load: %d ms\n\n", results$initialLoadMs))

timings <- results$interactionTimings
if (length(timings) > 0) {
    # Sort by time descending
    timing_df <- data.frame(
        Interaction = names(timings),
        Ms = unlist(timings),
        stringsAsFactors = FALSE
    )
    timing_df <- timing_df[order(-timing_df$Ms), ]

    cat(sprintf("%-25s %8s %s\n", "Interaction", "ms", "Status"))
    cat(paste(rep("-", 50), collapse = ""), "\n")
    for (i in seq_len(nrow(timing_df))) {
        status <- if (timing_df$Interaction[i] %in% names(results$errors)) "ERROR" else "OK"
        cat(sprintf("%-25s %8d %s\n", timing_df$Interaction[i], timing_df$Ms[i], status))
    }

    cat("\nSlowest 3:\n")
    top3 <- head(timing_df, 3)
    for (i in seq_len(nrow(top3))) {
        cat(sprintf("  %d. %s: %d ms\n", i, top3$Interaction[i], top3$Ms[i]))
    }
}

if (length(results$errors) > 0) {
    cat("\nErrors:\n")
    for (nm in names(results$errors)) {
        cat(sprintf("  %s: %s\n", nm, results$errors[[nm]]))
    }
}

cat(sprintf("\nTotal: %.1f s\n", total_elapsed / 1000))
cat("Done.\n")
