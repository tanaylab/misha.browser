# Tests for vertical line interval functions

# =============================================================================
# Tests for load_vline_inline
# =============================================================================

test_that("load_vline_inline returns data frame for explicit positions", {
    vline <- list(
        intervals = list(
            list(chrom = "chr1", start = 1000, end = 2000),
            list(chrom = "chr1", start = 5000, end = 6000)
        )
    )
    result <- load_vline_inline(vline)

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_equal(result$chrom, c("chr1", "chr1"))
    expect_equal(result$start, c(1000, 5000))
    expect_equal(result$end, c(2000, 6000))
})

test_that("load_vline_inline uses start as end when end is missing", {
    vline <- list(
        intervals = list(
            list(chrom = "chr1", start = 5000)
        )
    )
    result <- load_vline_inline(vline)

    expect_equal(result$start, 5000)
    expect_equal(result$end, 5000)
})

test_that("load_vline_inline returns NULL for empty intervals", {
    vline <- list(intervals = list())
    result <- load_vline_inline(vline)
    expect_null(result)
})

test_that("load_vline_inline returns NULL for NULL intervals", {
    vline <- list(intervals = NULL)
    result <- load_vline_inline(vline)
    expect_null(result)
})

# =============================================================================
# Tests for get_vline_positions
# =============================================================================

test_that("get_vline_positions returns start and end when show_bounds is TRUE", {
    vline <- list(show_bounds = TRUE)
    intervals <- data.frame(
        chrom = c("chr1", "chr1"),
        start = c(100, 300),
        end = c(200, 400)
    )
    result <- get_vline_positions(vline, intervals)

    expect_equal(sort(result), c(100, 200, 300, 400))
})

test_that("get_vline_positions returns midpoints when show_bounds is FALSE", {
    vline <- list(show_bounds = FALSE)
    intervals <- data.frame(
        chrom = c("chr1", "chr1"),
        start = c(100, 300),
        end = c(200, 400)
    )
    result <- get_vline_positions(vline, intervals)

    expect_equal(result, c(150, 350))
})

test_that("get_vline_positions defaults show_bounds to TRUE", {
    vline <- list() # No show_bounds field
    intervals <- data.frame(chrom = "chr1", start = 100, end = 200)
    result <- get_vline_positions(vline, intervals)

    # Should return both start and end
    expect_equal(sort(result), c(100, 200))
})

test_that("get_vline_positions returns numeric(0) for empty intervals", {
    vline <- list(show_bounds = TRUE)
    intervals <- data.frame(chrom = character(), start = numeric(), end = numeric())
    result <- get_vline_positions(vline, intervals)

    expect_equal(result, numeric(0))
})

test_that("get_vline_positions returns numeric(0) for NULL intervals", {
    vline <- list(show_bounds = TRUE)
    result <- get_vline_positions(vline, NULL)

    expect_equal(result, numeric(0))
})
