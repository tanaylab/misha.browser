# Tests for navigation functions

# Create a mock browser for testing
create_mock_browser <- function() {
    browser <- structure(
        list(
            cfg = list(
                ui = list(smooth_window_default = 10),
                navigator = list(
                    source = "intervs.global.tss",
                    label_field = "geneSymbol",
                    extension = 1e6
                )
            ),
            state = list(
                current_region = data.frame(
                    chrom = "chr1",
                    start = 1000000,
                    end = 2000000
                )
            )
        ),
        class = "browser"
    )
    browser
}

test_that("browser_zoom_in halves the view", {
    browser <- create_mock_browser()
    result <- browser_zoom_in(browser, factor = 2)

    region <- result$state$current_region
    original_center <- 1500000
    original_width <- 1000000
    new_width <- original_width / 2

    expect_equal(get_span(region), new_width)
    expect_equal((region$start + region$end) / 2, original_center)
})

test_that("browser_zoom_in respects minimum zoom", {
    browser <- create_mock_browser()
    browser$state$current_region <- data.frame(
        chrom = "chr1", start = 1000, end = 1050
    )

    result <- browser_zoom_in(browser, factor = 2)
    region <- result$state$current_region

    # Should not go below 100bp
    expect_true(get_span(region) >= 100)
})

test_that("browser_zoom_out doubles the view", {
    browser <- create_mock_browser()
    result <- browser_zoom_out(browser, factor = 2)

    region <- result$state$current_region
    original_width <- 1000000
    new_width <- original_width * 2

    expect_equal(get_span(region), new_width)
})

test_that("browser_move_left shifts view left", {
    browser <- create_mock_browser()
    original_start <- browser$state$current_region$start

    result <- browser_move_left(browser, fraction = 0.5)
    region <- result$state$current_region

    shift <- 1000000 * 0.5 # 50% of width
    expect_equal(region$start, original_start - shift)
    expect_equal(get_span(region), 1000000) # Width unchanged
})

test_that("browser_move_right shifts view right", {
    browser <- create_mock_browser()
    original_end <- browser$state$current_region$end

    result <- browser_move_right(browser, fraction = 0.5)
    region <- result$state$current_region

    shift <- 1000000 * 0.5
    expect_equal(region$end, original_end + shift)
})

test_that("expand_interval expands correctly", {
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)

    result <- expand_interval(iv, 5000)
    # Allow for rounding in sanitize_interval
    expect_equal(get_span(result), 5000, tolerance = 2)

    # Already larger - should return unchanged
    result <- expand_interval(iv, 500)
    expect_equal(get_span(result), 1000, tolerance = 2)
})

test_that("set_interval_width centers correctly", {
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    center <- 1500

    result <- set_interval_width(iv, 4000)
    # Allow for rounding tolerance
    expect_equal(get_span(result), 4000, tolerance = 2)
    expect_equal((result$start + result$end) / 2, center, tolerance = 1)
})

test_that("set_interval_width handles invalid width", {
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)

    result <- set_interval_width(iv, 0)
    expect_equal(get_span(result), 1000) # Unchanged

    result <- set_interval_width(iv, -100)
    expect_equal(get_span(result), 1000) # Unchanged
})

test_that("navigation preserves chromosome", {
    browser <- create_mock_browser()
    browser$state$current_region$chrom <- "chr17"

    browser <- browser_zoom_in(browser)
    expect_equal(browser$state$current_region$chrom, "chr17")

    browser <- browser_move_left(browser)
    expect_equal(browser$state$current_region$chrom, "chr17")
})
