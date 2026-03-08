#!/usr/bin/env Rscript
# benchmark_extract.R - Benchmark suite for misha.browser data operations
#
# Usage:
#   Rscript benchmarks/benchmark_extract.R [config] [iterations] [output_json]
#
# Defaults:
#   config:      dev/test_config.yaml
#   iterations:  5
#   output_json: benchmarks/results/benchmark_baseline.json

# ---------------------------------------------------------------------------
# Parse CLI arguments
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

pkg_root <- tryCatch(
    normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE),
    error = function(e) getwd()
)
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    pkg_root <- getwd()
}

config_path <- if (length(args) >= 1) args[1] else file.path(pkg_root, "dev", "test_config.yaml")
iterations <- if (length(args) >= 2) as.integer(args[2]) else 5L
output_json <- if (length(args) >= 3) args[3] else file.path(pkg_root, "benchmarks", "results", "benchmark_baseline.json")

stopifnot(
    "Config file not found" = file.exists(config_path),
    "Iterations must be >= 1" = iterations >= 1L
)

# ---------------------------------------------------------------------------
# Load package
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
    if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
        devtools::load_all(pkg_root, quiet = TRUE)
    } else {
        library(misha.browser)
    }
    library(ggplot2)
    library(jsonlite)
})

# Disable disk caching so we measure real work, not I/O to tempdir
options(misha.browser.disk_cache = FALSE)

# ---------------------------------------------------------------------------
# Benchmark helper
# ---------------------------------------------------------------------------
bench <- function(label, expr, n = iterations, env = parent.frame()) {
    expr_q <- substitute(expr)
    times <- numeric(n)
    result <- NULL
    for (i in seq_len(n)) {
        # Clear in-memory cache before each iteration so we measure cold-path
        suppressMessages(misha.browser::browser_clear_cache(disk = FALSE))

        start <- proc.time()[["elapsed"]]
        result <- eval(expr_q, envir = env)
        times[i] <- (proc.time()[["elapsed"]] - start) * 1000
    }
    cat(sprintf("  %-40s  min=%.1f  median=%.1f  mean=%.1f  max=%.1f ms  (n=%d)\n",
        label, min(times), median(times), mean(times), max(times), n))
    list(
        label  = label,
        min    = round(min(times), 2),
        median = round(median(times), 2),
        mean   = round(mean(times), 2),
        max    = round(max(times), 2),
        n      = n,
        .result = result
    )
}

# ---------------------------------------------------------------------------
# Run benchmarks
# ---------------------------------------------------------------------------
cat("misha.browser benchmark suite\n")
cat("Config:     ", config_path, "\n")
cat("Iterations: ", iterations, "\n")
cat("Output:     ", output_json, "\n")
cat(rep("-", 72), sep = "")
cat("\n")

results <- list()

# 1. Config loading (YAML parse + validation) ------------------------------
cat("\n[1] Config loading\n")
results$config_load <- bench("browser_load_config (YAML parse)", {
    misha.browser:::browser_load_config(config_path)
})

# Keep the parsed config for subsequent steps
cfg <- results$config_load$.result

# 2. misha gsetroot --------------------------------------------------------
cat("\n[2] gsetroot\n")
misha_root <- cfg$._misha_root
if (is.null(misha_root) || !dir.exists(misha_root)) {
    cat("  SKIP: no valid misha_root in config\n")
    results$gsetroot <- list(label = "gsetroot", skipped = TRUE)
} else {
    results$gsetroot <- bench("misha::gsetroot", {
        misha::gsetroot(misha_root)
    })
}

# 3. Full browser_create (config + root + vtracks + start region) ----------
cat("\n[3] browser_create (full init)\n")
results$browser_create <- bench("browser_create", {
    misha.browser::browser_create(config = config_path)
})
browser_obj <- results$browser_create$.result

# 4. Virtual track creation (init_vtracks) ---------------------------------
cat("\n[4] init_vtracks\n")
# We need a bare browser without vtracks to measure init_vtracks in isolation.
# Build one by stripping vtracks from existing browser and re-initialising.
results$init_vtracks <- bench("init_vtracks", {
    b <- browser_obj
    # Remove existing vtracks so init_vtracks re-creates them
    b$state$misha_vtrack_names <- character(0)
    b$state$vtrack_expressions <- list()
    misha.browser:::init_vtracks(b)
})

# 5. Data extraction -- single panel ---------------------------------------
cat("\n[5] Single panel extraction\n")
region <- browser_obj$state$current_region
data_panels <- Filter(function(p) (p$type %||% "data") == "data", browser_obj$cfg$panels)

if (length(data_panels) == 0) {
    cat("  SKIP: no data panels in config\n")
    results$extract_single <- list(label = "extract_panel_data (single)", skipped = TRUE)
} else {
    first_panel <- data_panels[[1]]
    results$extract_single <- bench(
        paste0("extract_panel_data (", first_panel$name, ")"),
        {
            misha.browser:::extract_panel_data(browser_obj, first_panel, region, use_cache = FALSE)
        }
    )
}

# 6. Data extraction -- all panels -----------------------------------------
cat("\n[6] All panels extraction\n")
if (length(data_panels) == 0) {
    cat("  SKIP: no data panels\n")
    results$extract_all <- list(label = "extract_panels_sequential (all)", skipped = TRUE)
} else {
    results$extract_all <- bench("extract_panels_sequential (all)", {
        misha.browser:::extract_panels_sequential(browser_obj, browser_obj$cfg$panels, region, use_cache = FALSE)
    })
}

# 7. Plot rendering (browser_plot -> ggplot/patchwork object) ---------------
cat("\n[7] browser_plot (full render)\n")
results$browser_plot <- bench("browser_plot (to ggplot)", {
    misha.browser::browser_plot(browser_obj, region = region)
})

# 8. Transform: smooth -----------------------------------------------------
cat("\n[8] Transforms\n")
# Generate synthetic data similar to what extract_panel_data returns
set.seed(42)
n_points <- 4000L
synth_data <- data.frame(
    chrom = rep("chr1", n_points),
    start = seq(1e6, by = 32, length.out = n_points),
    end   = seq(1e6 + 32, by = 32, length.out = n_points),
    pos   = seq(1e6 + 16, by = 32, length.out = n_points),
    value = cumsum(rnorm(n_points, 0, 0.1)) + 5
)

results$transform_smooth <- bench("transform: smooth (window=10)", {
    misha.browser:::apply_transforms(
        synth_data, list(list(type = "smooth", window = 10)), "value"
    )
})

results$transform_log2 <- bench("transform: log2 (offset=1)", {
    misha.browser:::apply_transforms(
        synth_data, list(list(type = "log2", offset = 1)), "value"
    )
})

results$transform_zscore <- bench("transform: zscore", {
    misha.browser:::apply_transforms(
        synth_data, list(list(type = "zscore")), "value"
    )
})

results$transform_pipeline <- bench("transform: smooth + log2 (pipeline)", {
    misha.browser:::apply_transforms(
        synth_data,
        list(
            list(type = "smooth", window = 10),
            list(type = "log2", offset = 1)
        ),
        "value"
    )
})

# ---------------------------------------------------------------------------
# Assemble output
# ---------------------------------------------------------------------------
# Strip the internal .result field before serialisation
clean_results <- lapply(results, function(r) {
    r$.result <- NULL
    r
})

output <- list(
    timestamp  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    config     = normalizePath(config_path),
    iterations = iterations,
    r_version  = paste0(R.version$major, ".", R.version$minor),
    platform   = R.version$platform,
    results    = clean_results
)

# Ensure output directory exists
dir.create(dirname(output_json), recursive = TRUE, showWarnings = FALSE)
write(toJSON(output, auto_unbox = TRUE, pretty = TRUE), output_json)

cat("\n", rep("-", 72), "\n", sep = "")
cat("Results written to: ", output_json, "\n")
