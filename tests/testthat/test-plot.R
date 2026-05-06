# Tests for main plotting helper functions

# =============================================================================
# Tests for expand_region
# =============================================================================

test_that("expand_region with zero expansion returns same region", {
    region <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    result <- expand_region(region, 0)

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("expand_region with positive expansion widens region", {
    region <- data.frame(chrom = "chr1", start = 10000, end = 20000)
    result <- expand_region(region, 2000)

    # Expansion is split evenly: 1000 on each side
    expect_equal(result$start, 9000)
    expect_equal(result$end, 21000)
    expect_equal(result$chrom, "chr1")
})

test_that("expand_region sanitizes start below 1", {
    region <- data.frame(chrom = "chr1", start = 500, end = 1500)
    result <- expand_region(region, 2000)

    # start would be 500 - 1000 = -500, sanitized to 1
    expect_equal(result$start, 1)
})

test_that("expand_region returns NULL for NULL region", {
    result <- expand_region(NULL, 1000)
    expect_null(result)
})

# =============================================================================
# Tests for add_highlight_overlay
# =============================================================================

test_that("add_highlight_overlay returns a ggplot object", {
    p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    highlight <- data.frame(start = 2, end = 4)

    result <- add_highlight_overlay(p, highlight)

    expect_s3_class(result, "ggplot")
})

test_that("add_highlight_overlay handles swapped start/end", {
    p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    highlight <- data.frame(start = 4, end = 2)

    # Should not error; it uses min/max internally
    result <- add_highlight_overlay(p, highlight)
    expect_s3_class(result, "ggplot")
})

test_that("add_highlight_overlay returns unchanged plot for non-finite values", {
    p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    highlight <- data.frame(start = NA, end = 4)

    result <- add_highlight_overlay(p, highlight)

    # Should return the plot unchanged
    expect_s3_class(result, "ggplot")
})

test_that("browser_plot handles a ggplot panel without errors", {
    p_user <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
    br <- browser_create() |>
        browser_add_panel(name = "meta", type = "ggplot", plot = p_user, height = 1)
    # Need a region to plot. Set one in state.
    br$state$current_region <- data.frame(chrom = "chr1", start = 1, end = 1000, stringsAsFactors = FALSE)
    expect_no_error(p <- browser_plot(br))
    # patchwork object
    expect_s3_class(p, "patchwork")
})

test_that("browser_plot puts x-axis on last non-ggplot panel even when ggplot is trailing", {
    skip_if_not_installed("ggplot2")
    p_user <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
    br <- browser_create() |>
        browser_add_panel(name = "meta_top", type = "ggplot", plot = p_user, height = 1) |>
        browser_add_panel(name = "sig", tracks = c("t1"), height = 2) |>
        browser_add_panel(name = "meta_bottom", type = "ggplot", plot = p_user, height = 1)
    br$state$current_region <- data.frame(chrom = "chr1", start = 1, end = 1000, stringsAsFactors = FALSE)

    # extract_panel_data will fail on a fake track without a misha db, so stub it.
    testthat::local_mocked_bindings(
        extract_panel_data = function(browser, panel, region, use_cache = TRUE) {
            data.frame(
                chrom = "chr1",
                start = seq(1, 1000, by = 100),
                end = seq(100, 1099, by = 100),
                pos = seq(50, 1049, by = 100),
                t1 = runif(10),
                track = "t1",
                source = "t1",
                stringsAsFactors = FALSE
            )
        },
        .package = "misha.browser"
    )

    expect_no_error(p <- browser_plot(br))
    expect_s3_class(p, "patchwork")
})

test_that("browser_plot does not error when all panels are ggplot", {
    skip_if_not_installed("ggplot2")
    p_user <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
    br <- browser_create() |>
        browser_add_panel(name = "a", type = "ggplot", plot = p_user, height = 1) |>
        browser_add_panel(name = "b", type = "ggplot", plot = p_user, height = 1)
    br$state$current_region <- data.frame(chrom = "chr1", start = 1, end = 1000, stringsAsFactors = FALSE)
    expect_no_error(p <- browser_plot(br))
})
