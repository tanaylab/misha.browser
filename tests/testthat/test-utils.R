# Tests for utility functions

test_that("null coalescing operator works", {
    expect_equal(NULL %||% "default", "default")
    expect_equal("value" %||% "default", "value")
    expect_equal(character(0) %||% "default", "default")
    expect_equal(1 %||% 2, 1)
})

test_that("is_empty detects empty values", {
    expect_true(is_empty(NULL))
    expect_true(is_empty(NA))
    expect_true(is_empty(""))
    expect_true(is_empty(character(0)))
    expect_false(is_empty("value"))
    expect_false(is_empty(1))
    expect_false(is_empty(c(1, 2)))
})

test_that("parse_coords parses coordinate strings", {
    # Standard format
    result <- parse_coords("chr1:1000-2000")
    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)

    # With commas
    result <- parse_coords("chr1:1,000,000-2,000,000")
    expect_equal(result$start, 1e6)
    expect_equal(result$end, 2e6)

    # Without chr prefix
    result <- parse_coords("1:1000-2000")
    expect_equal(result$chrom, "chr1")

    # Space-separated
    result <- parse_coords("chr5 100000 200000")
    expect_equal(result$chrom, "chr5")
    expect_equal(result$start, 100000)
    expect_equal(result$end, 200000)

    # Invalid inputs
    expect_null(parse_coords(NULL))
    expect_null(parse_coords(""))
    expect_null(parse_coords("chr1:abc"))
    expect_null(parse_coords("chr1"))
})

test_that("sanitize_interval handles various inputs", {
    # Valid interval
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    result <- sanitize_interval(iv)
    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)

    # Swapped start/end
    iv <- data.frame(chrom = "chr1", start = 2000, end = 1000)
    result <- sanitize_interval(iv)
    expect_true(result$start < result$end)

    # Negative start
    iv <- data.frame(chrom = "chr1", start = -100, end = 1000)
    result <- sanitize_interval(iv)
    expect_equal(result$start, 1)

    # NULL input
    expect_null(sanitize_interval(NULL))

    # Missing columns
    iv <- data.frame(chrom = "chr1", x = 100)
    expect_null(sanitize_interval(iv))
})

test_that("calc_iterator scales correctly", {
    # Small span uses base iterator
    expect_equal(calc_iterator(1000, 32, 4000), 32)

    # Large span scales up
    expect_equal(calc_iterator(4e6, 32, 4000), 1000)

    # Very large span
    result <- calc_iterator(100e6, 32, 4000)
    expect_true(result >= 25000)
})

test_that("merge_list merges recursively", {
    base <- list(a = 1, b = list(x = 10, y = 20))
    override <- list(b = list(y = 25, z = 30), c = 3)

    result <- merge_list(base, override)
    expect_equal(result$a, 1)
    expect_equal(result$b$x, 10)
    expect_equal(result$b$y, 25)
    expect_equal(result$b$z, 30)
    expect_equal(result$c, 3)
})

test_that("get_span calculates correctly", {
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    expect_equal(get_span(iv), 1000)

    # Note: sanitize_interval adjusts start to max(1, floor(start))
    # so start=0 becomes start=1
    iv <- data.frame(chrom = "chr1", start = 1, end = 1e6)
    expect_equal(get_span(iv), 1e6 - 1, tolerance = 1)

    expect_equal(get_span(NULL), 0)
})

test_that("format_coords formats nicely", {
    result <- format_coords("chr1", 1000000, 2000000)
    expect_true(grepl("chr1", result))
    expect_true(grepl("1,000,000", result))
})

# Tests for new helper functions

test_that("interval_center calculates center correctly", {
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    expect_equal(interval_center(iv), 1500)

    iv <- data.frame(chrom = "chr1", start = 0, end = 100)
    expect_equal(interval_center(iv), 50)

    iv <- data.frame(chrom = "chr1", start = 1e6, end = 2e6)
    expect_equal(interval_center(iv), 1.5e6)
})

test_that("interval_center handles NULL", {
    expect_null(interval_center(NULL))
})

test_that("interval_width calculates width correctly", {
    iv <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    expect_equal(interval_width(iv), 1000)

    iv <- data.frame(chrom = "chr1", start = 1e6, end = 2e6)
    expect_equal(interval_width(iv), 1e6)
})

test_that("interval_width handles NULL", {
    expect_equal(interval_width(NULL), 0)
})

test_that("make_region creates valid region", {
    region <- make_region("chr1", 1000, 2000)
    expect_s3_class(region, "data.frame")
    expect_equal(region$chrom, "chr1")
    expect_equal(region$start, 1000)
    expect_equal(region$end, 2000)
})

test_that("make_region sanitizes swapped coordinates", {
    region <- make_region("chr1", 2000, 1000)
    expect_true(region$start < region$end)
})

test_that("make_region sanitizes negative start", {
    region <- make_region("chr1", -100, 1000)
    expect_equal(region$start, 1)
})

test_that("with_cache returns cached value", {
    browser_clear_cache(disk = FALSE)
    key <- cache_key("test", "value")

    result <- with_cache(key, function() "computed")
    expect_equal(result, "computed")

    # Second call should return cached
    call_count <- 0
    result2 <- with_cache(key, function() {
        call_count <<- call_count + 1
        "recomputed"
    })
    expect_equal(result2, "computed")
    expect_equal(call_count, 0) # Function not called
})

test_that("with_cache computes on miss", {
    browser_clear_cache(disk = FALSE)
    key <- cache_key("test", "new_value")

    call_count <- 0
    result <- with_cache(key, function() {
        call_count <<- call_count + 1
        "computed"
    })
    expect_equal(result, "computed")
    expect_equal(call_count, 1)
})
