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
    expect_warning(result <- transform_expr(x, NULL, list(expr = "undefined_function(x)")))
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

# =============================================================================
# Additional edge case tests (Issue #18: Division by zero in z-score)
# =============================================================================

test_that("transform_zscore handles all-NA input", {
    x <- rep(NA_real_, 5)
    result <- transform_zscore(x, list())
    expect_true(all(is.na(result)))
})

test_that("transform_zscore handles mixed NA values", {
    x <- c(10, NA, 20, NA, 30)
    result <- transform_zscore(x, list())

    # Non-NA values should be normalized
    expect_false(is.na(result[1]))
    expect_false(is.na(result[3]))
    expect_false(is.na(result[5]))

    # NA values should stay NA
    expect_true(is.na(result[2]))
    expect_true(is.na(result[4]))

    # Non-NA values should have mean ~0 and sd ~1
    non_na <- result[!is.na(result)]
    expect_equal(mean(non_na), 0, tolerance = 1e-10)
    expect_equal(sd(non_na), 1, tolerance = 1e-10)
})

test_that("transform_zscore handles single non-NA value", {
    x <- c(NA, 5, NA, NA)
    result <- transform_zscore(x, list())
    
    expect_equal(result, c(NA, 0, NA, NA))
})

# =============================================================================
# Edge cases for transform_minmax
# =============================================================================

test_that("transform_minmax handles mixed NA values", {
    x <- c(0, NA, 10, NA, 5)
    result <- transform_minmax(x, list())

    expect_equal(result[1], 0) # min
    expect_equal(result[3], 1) # max
    expect_equal(result[5], 0.5) # middle
})

test_that("transform_minmax handles Inf values", {
    x <- c(0, Inf, 10)
    result <- transform_minmax(x, list())

    # With Inf, range becomes infinite - should return 0.5
    expect_true(all(result == 0.5))
})

# =============================================================================
# Edge cases for transform_smooth
# =============================================================================

test_that("transform_smooth handles all-NA input", {
    x <- rep(NA_real_, 10)
    result <- transform_smooth(x, list(window = 3))
    expect_equal(result, x) # Returns unchanged
})

test_that("transform_smooth handles mostly-NA input", {
    x <- c(NA, NA, 5, NA, NA, NA, NA, NA, NA, NA)
    result <- transform_smooth(x, list(window = 3))
    # Should handle this gracefully
    expect_length(result, 10)
})

test_that("transform_smooth uses default window of 10", {
    x <- 1:20
    result <- transform_smooth(x, list())
    expect_length(result, 20)
    expect_true(all(is.finite(result)))
})

test_that("transform_smooth handles align parameter", {
    x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    result_center <- transform_smooth(x, list(window = 3, align = "center"))
    result_left <- transform_smooth(x, list(window = 3, align = "left"))
    result_right <- transform_smooth(x, list(window = 3, align = "right"))

    # All should have same length
    expect_length(result_center, 10)
    expect_length(result_left, 10)
    expect_length(result_right, 10)

    # Values should differ based on alignment
    expect_true(!identical(result_center, result_left))
})

# =============================================================================
# Edge cases for transform_log2 and transform_log10
# =============================================================================

test_that("transform_log2 handles negative values with offset", {
    x <- c(-5, -1, 0, 1, 5)
    result <- transform_log2(x, list(offset = 10))

    # All values + 10 are positive, so should get finite results
    expect_true(all(is.finite(result)))
    expect_equal(result[4], log2(11)) # 1 + 10 = 11
})

test_that("transform_log10 handles zero with offset", {
    x <- c(0, 0, 0)
    result <- transform_log10(x, list(offset = 1))
    expect_equal(result, c(0, 0, 0)) # log10(1) = 0
})

test_that("transform_log2 handles large values", {
    x <- c(1e10, 1e15, 1e20)
    result <- transform_log2(x, list(offset = 1))
    expect_true(all(is.finite(result)))
})

# =============================================================================
# Edge cases for transform_sqrt
# =============================================================================

test_that("transform_sqrt clamps all negative values to 0", {
    x <- c(-100, -1, -0.001)
    result <- transform_sqrt(x, list())
    expect_equal(result, c(0, 0, 0))
})

test_that("transform_sqrt handles mixed positive/negative", {
    x <- c(-1, 0, 1, 4, 9, 16)
    result <- transform_sqrt(x, list())
    expect_equal(result, c(0, 0, 1, 2, 3, 4))
})

# =============================================================================
# Edge cases for transform_clip
# =============================================================================

test_that("transform_clip handles no bounds", {
    x <- c(-100, 0, 100)
    result <- transform_clip(x, list())
    expect_equal(result, x) # Unchanged
})

test_that("transform_clip handles Inf bounds", {
    x <- c(-100, 0, 100)
    result <- transform_clip(x, list(min = -Inf, max = Inf))
    expect_equal(result, x) # Unchanged
})

test_that("transform_clip handles NA in data", {
    x <- c(NA, 5, NA, 10)
    result <- transform_clip(x, list(min = 0, max = 8))

    expect_true(is.na(result[1]))
    expect_equal(result[2], 5)
    expect_true(is.na(result[3]))
    expect_equal(result[4], 8) # Clipped
})

# =============================================================================
# Edge cases for transform_quantile
# =============================================================================

test_that("transform_quantile uses default probs", {
    set.seed(123)
    x <- rnorm(100)
    result <- transform_quantile(x, list())

    # Default is c(0.01, 0.99)
    q <- quantile(x, c(0.01, 0.99))
    expect_true(all(result >= q[1]))
    expect_true(all(result <= q[2]))
})

test_that("transform_quantile handles extreme probs", {
    x <- 1:100
    result <- transform_quantile(x, list(probs = c(0.5, 0.5)))

    # All values clipped to median
    expect_true(all(result == 50.5))
})

test_that("transform_quantile handles NA values", {
    x <- c(NA, 1, 2, 3, NA, 5)
    result <- transform_quantile(x, list(probs = c(0.2, 0.8)))

    expect_true(is.na(result[1]))
    expect_true(is.na(result[5]))
})

# =============================================================================
# Edge cases for transform_expr
# =============================================================================

test_that("transform_expr handles missing expr parameter", {
    x <- c(1, 2, 3)
    expect_warning(result <- transform_expr(x, NULL, list()))
    expect_equal(result, x) # Returns unchanged with warning
})

test_that("transform_expr handles length mismatch", {
    x <- c(1, 2, 3)
    # Expression returns wrong length
    expect_warning(result <- transform_expr(x, NULL, list(expr = "c(1, 2)")))
    expect_equal(result, x) # Returns unchanged due to length mismatch
})

test_that("transform_expr has access to base R functions", {
    x <- c(-1, 0, 1, 2)
    pos <- 1:4

    # pmax
    result <- transform_expr(x, pos, list(expr = "pmax(x, 0)"))
    expect_equal(result, c(0, 0, 1, 2))

    # abs
    result <- transform_expr(x, pos, list(expr = "abs(x)"))
    expect_equal(result, c(1, 0, 1, 2))

    # sqrt (via pmax to avoid NaN)
    result <- transform_expr(x, pos, list(expr = "sqrt(pmax(x, 0))"))
    expect_equal(result, c(0, 0, 1, sqrt(2)))
})

# =============================================================================
# Tests for apply_transforms edge cases
# =============================================================================

test_that("apply_transforms handles empty value_cols", {
    data <- data.frame(chrom = "chr1", start = 1, end = 2)
    transforms <- list(list(type = "log2"))

    result <- apply_transforms(data, transforms, character(0))
    expect_equal(result, data) # Unchanged
})

test_that("apply_transforms handles non-existent columns", {
    data <- data.frame(chrom = "chr1", start = 1, end = 2, value = 5)
    transforms <- list(list(type = "log2"))

    result <- apply_transforms(data, transforms, c("nonexistent"))
    expect_equal(result, data) # Unchanged, column doesn't exist
})

test_that("apply_transforms handles NULL transform type", {
    data <- data.frame(pos = 1:5, value = 1:5)
    transforms <- list(list(type = NULL))

    result <- apply_transforms(data, transforms, "value")
    expect_equal(result$value, data$value) # Unchanged
})

test_that("apply_transforms handles unknown transform type", {
    data <- data.frame(pos = 1:5, value = 1:5)
    transforms <- list(list(type = "unknown_transform"))

    # Should warn and return unchanged
    expect_warning(
        result <- apply_transforms(data, transforms, "value"),
        "Unknown transform"
    )
    expect_equal(result$value, data$value)
})

test_that("apply_transforms applies vectorizable transforms as matrix", {
    data <- data.frame(
        chrom = "chr1",
        start = 1:3,
        end = 2:4,
        pos = 1:3,
        track1 = c(0, 1, 3),
        track2 = c(7, 15, 31)
    )

    transforms <- list(list(type = "log2", offset = 1))
    result <- apply_transforms(data, transforms, c("track1", "track2"))

    # Both columns should be transformed
    expect_equal(result$track1, log2(c(0, 1, 3) + 1))
    expect_equal(result$track2, log2(c(7, 15, 31) + 1))
})

test_that("apply_transforms chains multiple vectorizable transforms", {
    data <- data.frame(pos = 1:5, value = c(-10, 0, 10, 50, 100))

    transforms <- list(
        list(type = "clip", min = 0, max = 50),
        list(type = "sqrt")
    )

    result <- apply_transforms(data, transforms, "value")

    # First clip to [0, 50], then sqrt
    expected <- sqrt(pmax(pmin(c(-10, 0, 10, 50, 100), 50), 0))
    expect_equal(result$value, expected, ignore_attr = TRUE)
})

# =============================================================================
# Tests for get_effective_smooth_window
# =============================================================================

test_that("get_effective_smooth_window uses state value", {
    browser <- browser_create()
    browser$state$smooth_window <- 25
    browser$cfg$ui$smooth_window_default <- 10

    panel <- list(transforms = list())

    result <- get_effective_smooth_window(browser, panel)
    expect_equal(result, 25)
})

test_that("get_effective_smooth_window falls back to config default", {
    browser <- browser_create()
    browser$state$smooth_window <- NULL
    browser$cfg$ui$smooth_window_default <- 15

    panel <- list(transforms = list())

    result <- get_effective_smooth_window(browser, panel)
    expect_equal(result, 15)
})

test_that("get_effective_smooth_window falls back to 10", {
    browser <- browser_create()
    browser$state$smooth_window <- NULL
    browser$cfg$ui$smooth_window_default <- NULL

    panel <- list(transforms = list())

    result <- get_effective_smooth_window(browser, panel)
    expect_equal(result, 10)
})

test_that("get_effective_smooth_window scales panel-specific window", {
    browser <- browser_create()
    browser$state$smooth_window <- 20
    browser$cfg$ui$smooth_window_default <- 10

    panel <- list(transforms = list(
        list(type = "smooth", window = 5)
    ))

    result <- get_effective_smooth_window(browser, panel)
    # 5 * (20/10) = 10
    expect_equal(result, 10)
})
