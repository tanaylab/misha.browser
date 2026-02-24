# Tests for migration utility functions

# =============================================================================
# Tests for convert_plot_type
# =============================================================================

test_that("convert_plot_type maps 'l' to 'line'", {
    result <- convert_plot_type("l")
    expect_equal(result, "line")
})

test_that("convert_plot_type maps 'p' to 'point'", {
    result <- convert_plot_type("p")
    expect_equal(result, "point")
})

test_that("convert_plot_type maps 'polygon' to 'area'", {
    result <- convert_plot_type("polygon")
    expect_equal(result, "area")
})

test_that("convert_plot_type maps 'histogram' to 'histogram'", {
    result <- convert_plot_type("histogram")
    expect_equal(result, "histogram")
})

test_that("convert_plot_type maps 'h' to 'histogram'", {
    result <- convert_plot_type("h")
    expect_equal(result, "histogram")
})

test_that("convert_plot_type returns 'line' for unknown type", {
    result <- convert_plot_type("scatter")
    expect_equal(result, "line")
})

test_that("convert_plot_type returns 'line' for NULL input", {
    result <- convert_plot_type(NULL)
    expect_equal(result, "line")
})
