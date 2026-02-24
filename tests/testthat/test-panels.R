# Tests for panel rendering helper functions

# =============================================================================
# Tests for generate_color
# =============================================================================

test_that("generate_color returns a valid hex color string", {
    result <- generate_color("test_track")

    expect_type(result, "character")
    expect_length(result, 1)
    # HCL colors are returned as hex strings like "#RRGGBB"
    expect_true(grepl("^#[0-9A-Fa-f]{6}", result))
})

test_that("generate_color is deterministic for same input", {
    result1 <- generate_color("my_track")
    result2 <- generate_color("my_track")

    expect_equal(result1, result2)
})

test_that("generate_color produces different colors for different names", {
    result_a <- generate_color("track_alpha")
    result_b <- generate_color("track_beta")

    expect_false(result_a == result_b)
})

# =============================================================================
# Tests for get_panel_colors
# =============================================================================

test_that("get_panel_colors uses explicit colors dict", {
    panel <- list(
        colors = list(track1 = "red", track2 = "blue")
    )
    data <- data.frame(
        track = c("track1", "track2", "track1"),
        value = c(1, 2, 3)
    )

    result <- get_panel_colors(panel, data, "track", config_colors = NULL)

    expect_equal(result[["track1"]], "red")
    expect_equal(result[["track2"]], "blue")
})

test_that("get_panel_colors falls back to config_colors", {
    panel <- list(colors = NULL)
    data <- data.frame(
        track = c("track1", "track2"),
        value = c(1, 2)
    )
    config_colors <- list(track1 = "green", track2 = "purple")

    result <- get_panel_colors(panel, data, "track", config_colors)

    expect_equal(result[["track1"]], "green")
    expect_equal(result[["track2"]], "purple")
})

test_that("get_panel_colors generates color for unknown tracks", {
    panel <- list(colors = NULL)
    data <- data.frame(
        track = c("unknown_track"),
        value = 1
    )

    result <- get_panel_colors(panel, data, "track", config_colors = NULL)

    expect_length(result, 1)
    expect_false(is.na(result[["unknown_track"]]))
    # Should be a valid color string
    expect_true(nchar(result[["unknown_track"]]) > 0)
})

test_that("get_panel_colors handles empty data gracefully", {
    panel <- list(colors = list(a = "red"))
    data <- data.frame(track = character(), value = numeric())

    result <- get_panel_colors(panel, data, "track", config_colors = NULL)

    expect_length(result, 0)
})
