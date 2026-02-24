# Tests for configuration validation functions (shiny-config-validation)

# =============================================================================
# Tests for validate_config_full
# =============================================================================

test_that("validate_config_full returns no errors for valid minimal config", {
    # Build a minimal valid config that avoids misha db lookups:
    # - no vtracks (skips track_exists / intervals_exist)
    # - no data panels requiring track validation
    # - no vlines requiring file/intervals checks
    cfg <- list(
        ui = list(title = "Test", span_default = 2e6, smooth_window_default = 10),
        plot = list(iterator = 32, expansion = 0, target_points = 4000),
        navigator = list(),
        panels = list(),
        vtracks = list(),
        vlines = list(),
        colors = list()
    )
    result <- validate_config_full(cfg)

    expect_equal(length(result), 0)
})

test_that("validate_config_full catches missing panel name", {
    cfg <- list(
        ui = list(title = "Test"),
        plot = list(),
        navigator = list(),
        panels = list(
            list(type = "data", tracks = c("track1"))
        ),
        vtracks = list(),
        vlines = list(),
        colors = list()
    )
    result <- validate_config_full(cfg)

    # Should contain an error about panel name
    error_msgs <- unlist(result)
    expect_true(any(grepl("[Nn]ame", error_msgs)))
})

test_that("validate_config_full catches invalid panel type", {
    cfg <- list(
        ui = list(),
        plot = list(),
        navigator = list(),
        panels = list(
            list(name = "bad_panel", type = "invalid_type")
        ),
        vtracks = list(),
        vlines = list(),
        colors = list()
    )
    result <- validate_config_full(cfg)

    error_msgs <- unlist(result)
    expect_true(any(grepl("type must be one of", error_msgs)))
})

# =============================================================================
# Tests for validate_panel_full
# =============================================================================

test_that("validate_panel_full catches invalid plot_type", {
    panel <- list(name = "test", type = "data", tracks = c("t1"), plot_type = "scatter")
    result <- validate_panel_full(panel, 1, vtrack_names = "t1")

    error_msgs <- unlist(result)
    expect_true(any(grepl("Plot type must be one of", error_msgs)))
})

test_that("validate_panel_full accepts valid panel types", {
    for (ptype in c("data", "annotation", "ideogram", "intervals")) {
        panel <- list(name = paste0("p_", ptype), type = ptype)
        result <- validate_panel_full(panel, 1)
        type_errors <- result[grepl("type$", names(result))]
        expect_equal(length(type_errors), 0, info = paste("Type:", ptype))
    }
})

# =============================================================================
# Tests for validate_plot_section
# =============================================================================

test_that("validate_plot_section returns no errors for NULL plot", {
    result <- validate_plot_section(NULL)
    expect_equal(length(result), 0)
})

test_that("validate_plot_section catches iterator < 1", {
    result <- validate_plot_section(list(iterator = 0))
    expect_true(any(grepl("Iterator", unlist(result))))
})

test_that("validate_plot_section catches negative expansion", {
    result <- validate_plot_section(list(expansion = -1))
    expect_true(any(grepl("Expansion", unlist(result))))
})

# =============================================================================
# Tests for validate_colors
# =============================================================================

test_that("validate_colors returns no errors for empty colors", {
    expect_equal(length(validate_colors(NULL)), 0)
    expect_equal(length(validate_colors(list())), 0)
})

test_that("validate_colors catches invalid hex color", {
    result <- validate_colors(list(bad = "#ZZZZZZ"))
    expect_true(length(result) > 0)
})

test_that("validate_colors accepts valid named colors", {
    result <- validate_colors(list(track1 = "red", track2 = "blue"))
    expect_equal(length(result), 0)
})
