# Tests for transformation functions

test_that("transform_smooth applies rolling mean", {
    x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    result <- transform_smooth(x, list(window = 3))

    expect_length(result, 10)
    expect_true(all(is.finite(result)))
    # Center value should be smoothed
    expect_equal(result[5], mean(c(4, 5, 6)))
})

test_that("transform_smooth handles short vectors", {
    x <- c(1, 2)
    result <- transform_smooth(x, list(window = 5))
    expect_equal(result, x) # Should return unchanged
})

test_that("transform_log2 applies correctly", {
    x <- c(0, 1, 3, 7, 15)
    result <- transform_log2(x, list(offset = 1))

    expect_equal(result[1], log2(0 + 1))
    expect_equal(result[2], log2(1 + 1))
    expect_equal(result[3], log2(3 + 1))
})

test_that("transform_log2 uses default offset", {
    x <- c(0, 1, 3)
    result <- transform_log2(x, list())
    expect_equal(result, log2(x + 1))
})

test_that("transform_log10 applies correctly", {
    x <- c(0, 9, 99, 999)
    result <- transform_log10(x, list(offset = 1))
    expect_equal(result[2], log10(10))
    expect_equal(result[3], log10(100))
})

test_that("transform_sqrt handles negative values", {
    x <- c(-1, 0, 1, 4, 9)
    result <- transform_sqrt(x, list())

    expect_equal(result[1], 0) # Negative becomes 0
    expect_equal(result[4], 2)
    expect_equal(result[5], 3)
})

test_that("transform_zscore normalizes correctly", {
    x <- c(10, 20, 30, 40, 50)
    result <- transform_zscore(x, list())

    expect_equal(mean(result), 0, tolerance = 1e-10)
    expect_equal(sd(result), 1, tolerance = 1e-10)
})

test_that("transform_zscore handles constant values", {
    x <- rep(5, 10)
    result <- transform_zscore(x, list())
    expect_true(all(result == 0))
})

test_that("transform_minmax scales to 0-1", {
    x <- c(10, 20, 30, 40, 50)
    result <- transform_minmax(x, list())

    expect_equal(min(result), 0)
    expect_equal(max(result), 1)
    expect_equal(result[3], 0.5)
})

test_that("transform_minmax handles constant values", {
    x <- rep(5, 10)
    result <- transform_minmax(x, list())
    expect_true(all(result == 0.5))
})

test_that("transform_clip clips values", {
    x <- c(-5, 0, 5, 10, 15)
    result <- transform_clip(x, list(min = 0, max = 10))

    expect_equal(result[1], 0)
    expect_equal(result[2], 0)
    expect_equal(result[3], 5)
    expect_equal(result[4], 10)
    expect_equal(result[5], 10)
})

test_that("transform_clip handles missing bounds", {
    x <- c(-5, 0, 5)

    result_min <- transform_clip(x, list(min = 0))
    expect_equal(result_min, c(0, 0, 5))

    result_max <- transform_clip(x, list(max = 0))
    expect_equal(result_max, c(-5, 0, 0))
})

test_that("transform_quantile clips to quantiles", {
    set.seed(123)
    x <- rnorm(100, mean = 50, sd = 10)
    result <- transform_quantile(x, list(probs = c(0.1, 0.9)))

    q <- quantile(x, c(0.1, 0.9))
    expect_true(all(result >= q[1]))
    expect_true(all(result <= q[2]))
})

test_that("transform_expr evaluates custom expressions", {
    x <- c(1, 2, 3, 4, 5)
    pos <- 1:5

    result <- transform_expr(x, pos, list(expr = "x * 2"))
    expect_equal(result, x * 2)

    result <- transform_expr(x, pos, list(expr = "x + pos"))
    expect_equal(result, x + pos)

    result <- transform_expr(x, pos, list(expr = "pmax(x - 2, 0)"))
    expect_equal(result, c(0, 0, 1, 2, 3))
})

test_that("transform_expr handles errors gracefully", {
    x <- c(1, 2, 3)
    result <- transform_expr(x, NULL, list(expr = "undefined_function(x)"))
    expect_equal(result, x) # Returns unchanged on error
})

test_that("apply_transforms chains transforms correctly", {
    data <- data.frame(
        chrom = "chr1",
        start = 1:10,
        end = 2:11,
        pos = 1:10,
        value = c(0, 1, 3, 7, 15, 31, 63, 127, 255, 511)
    )

    transforms <- list(
        list(type = "log2", offset = 1),
        list(type = "clip", min = 0, max = 8)
    )

    result <- apply_transforms(data, transforms, "value")

    # log2(512) = 9, clipped to 8
    expect_true(max(result$value) <= 8)
    expect_true(min(result$value) >= 0)
})

test_that("apply_transforms with empty list returns unchanged", {
    data <- data.frame(pos = 1:5, value = c(1, 2, 3, 4, 5))
    result <- apply_transforms(data, list(), "value")
    expect_equal(result$value, data$value)
})

test_that("update_smooth_window updates smooth transforms", {
    transforms <- list(
        list(type = "smooth", window = 10),
        list(type = "log2", offset = 1),
        list(type = "smooth", window = 5)
    )

    result <- update_smooth_window(transforms, 20)

    expect_equal(result[[1]]$window, 20)
    expect_equal(result[[2]]$type, "log2")
    expect_equal(result[[3]]$window, 20)
})
