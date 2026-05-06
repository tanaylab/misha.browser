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

# =============================================================================
# Tests for panel-name strip (Feature 1)
# =============================================================================

# Helper: minimal pre-extracted data so render_data_panel skips extraction
.panel_strip_data <- function() {
    data.frame(
        chrom = "chr1",
        start = seq(1, 1000, by = 100),
        end   = seq(100, 1099, by = 100),
        pos   = seq(50, 1049, by = 100),
        value = runif(10),
        track = rep("t1", 10),
        source = rep("src", 10),
        stringsAsFactors = FALSE
    )
}

test_that("data panel injects ._panel_name and uses FacetGrid when show_name = TRUE", {
    panel <- list(
        name = "my_signal",
        type = "data",
        tracks = "t1",
        plot_type = "line",
        show_name = TRUE
    )
    panel$._cfg_colors <- list()
    region <- data.frame(chrom = "chr1", start = 1, end = 1100)

    p <- render_data_panel(
        browser = list(cfg = list(colors = list())),
        panel = panel,
        region = region,
        vlines_data = NULL,
        pre_extracted_data = .panel_strip_data()
    )

    expect_true("._panel_name" %in% names(p$data))
    expect_equal(unique(p$data$._panel_name), "my_signal")
    expect_s3_class(p$facet, "FacetGrid")
})

test_that("data panel does NOT add ._panel_name when show_name = FALSE", {
    panel <- list(
        name = "my_signal",
        type = "data",
        tracks = "t1",
        plot_type = "line",
        show_name = FALSE
    )
    panel$._cfg_colors <- list()
    region <- data.frame(chrom = "chr1", start = 1, end = 1100)

    p <- render_data_panel(
        browser = list(cfg = list(colors = list())),
        panel = panel,
        region = region,
        vlines_data = NULL,
        pre_extracted_data = .panel_strip_data()
    )

    expect_false("._panel_name" %in% names(p$data))
    expect_s3_class(p$facet, "FacetNull")
})

test_that("apply_panel_theme sets legend title to panel name", {
    panel <- list(name = "my_signal", show_legend = TRUE)
    p <- ggplot2::ggplot()
    p <- apply_panel_theme(p, panel)
    # ggplot stores labs in p$labels; the color label should be panel$name
    expect_equal(p$labels$colour %||% p$labels$color, "my_signal")
})

# =============================================================================
# Tests for ggplot panel rendering (Feature 2)
# =============================================================================

test_that("render_ggplot_panel returns the panel's plot as-is", {
    p_in <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
    panel <- list(name = "meta", type = "ggplot", plot = p_in)
    region <- data.frame(chrom = "chr1", start = 1, end = 100)
    p_out <- render_ggplot_panel(panel, region)
    expect_identical(p_out, p_in)
})
