# Tests for Shiny UI helper functions

# =============================================================================
# Tests for extract_track_names
# =============================================================================

test_that("extract_track_names handles simple string tracks", {
    tracks <- c("track1", "track2", "track3")
    result <- extract_track_names(tracks)

    expect_equal(result, c("track1", "track2", "track3"))
})

test_that("extract_track_names handles list tracks with name field", {
    tracks <- list(
        "track1",
        list(name = "custom_track", expr = "track2 * 2"),
        list(name = "another", src = "some_track")
    )
    result <- extract_track_names(tracks)

    expect_equal(result, c("track1", "custom_track", "another"))
})

test_that("extract_track_names skips inline expressions without name", {
    tracks <- list(
        "track1",
        list(expr = "track2 + track3"), # No name, should be skipped
        list(name = "named_expr", expr = "track4")
    )
    result <- extract_track_names(tracks)

    expect_equal(result, c("track1", "named_expr"))
})

test_that("extract_track_names handles empty input", {
    expect_equal(extract_track_names(NULL), character(0))
    expect_equal(extract_track_names(list()), character(0))
    expect_equal(extract_track_names(character(0)), character(0))
})

test_that("extract_track_names filters empty strings", {
    tracks <- c("track1", "", "track2", "")
    result <- extract_track_names(tracks)

    expect_equal(result, c("track1", "track2"))
})

test_that("extract_track_names handles mixed types", {
    tracks <- list(
        "string_track",
        list(name = "named_list_track"),
        list(src = "source_only"), # No name, has src - skipped
        list(expr = "expr_only") # No name - skipped
    )
    result <- extract_track_names(tracks)

    expect_equal(result, c("string_track", "named_list_track"))
})

# =============================================================================
# Tests for get_track_choices
# =============================================================================

test_that("get_track_choices extracts tracks from all data panels", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "panel1", type = "data", tracks = c("track1", "track2")),
        list(name = "panel2", type = "data", tracks = c("track3")),
        list(name = "genes", type = "annotation") # Should be ignored
    )

    result <- get_track_choices(browser)

    expect_true("track1" %in% result)
    expect_true("track2" %in% result)
    expect_true("track3" %in% result)
})

test_that("get_track_choices includes default_tracks", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "panel1", type = "data", tracks = c("track1"))
    )
    browser$cfg$ui$default_tracks <- c("default_track1", "default_track2")

    result <- get_track_choices(browser)

    expect_true("track1" %in% result)
    expect_true("default_track1" %in% result)
    expect_true("default_track2" %in% result)
})

test_that("get_track_choices returns unique tracks", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "panel1", type = "data", tracks = c("track1", "track2")),
        list(name = "panel2", type = "data", tracks = c("track1", "track3"))
    )

    result <- get_track_choices(browser)

    expect_equal(sum(result == "track1"), 1) # track1 appears only once
    expect_length(result, 3)
})

test_that("get_track_choices handles no data panels", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "genes", type = "annotation"),
        list(name = "ideo", type = "ideogram")
    )

    result <- get_track_choices(browser)

    expect_length(result, 0)
})

# =============================================================================
# Tests for get_default_tracks
# =============================================================================

test_that("get_default_tracks uses ui default_tracks", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "panel1", type = "data", tracks = c("track1", "track2", "track3"))
    )
    browser$cfg$ui$default_tracks <- c("track1", "track3")

    result <- get_default_tracks(browser)

    expect_equal(result, c("track1", "track3"))
})

test_that("get_default_tracks includes all defaults present in choices", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "panel1", type = "data", tracks = c("track1", "track2"))
    )
    # Since get_track_choices includes default_tracks, both will be in choices
    browser$cfg$ui$default_tracks <- c("track1", "nonexistent_track")

    result <- get_default_tracks(browser)

    # Both are returned because get_track_choices includes default_tracks
    expect_equal(result, c("track1", "nonexistent_track"))
})

test_that("get_default_tracks falls back to first panel tracks", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "panel1", type = "annotation"),
        list(name = "panel2", type = "data", tracks = c("fallback1", "fallback2"))
    )
    browser$cfg$ui$default_tracks <- NULL

    result <- get_default_tracks(browser)

    expect_equal(result, c("fallback1", "fallback2"))
})

test_that("get_default_tracks returns empty for no data panels", {
    browser <- browser_create()
    browser$cfg$panels <- list(
        list(name = "genes", type = "annotation")
    )
    browser$cfg$ui$default_tracks <- NULL

    result <- get_default_tracks(browser)

    expect_length(result, 0)
})

# =============================================================================
# Tests for utility functions (from utils.R used by UI)
# =============================================================================

test_that("parse_coords handles standard format", {
    result <- parse_coords("chr1:1000-2000")

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("parse_coords handles space-separated format", {
    result <- parse_coords("chr1 1000 2000")

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("parse_coords handles tab-separated format", {
    result <- parse_coords("chr1\t1000\t2000")

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("parse_coords adds chr prefix if missing", {
    result <- parse_coords("1:1000-2000")

    expect_equal(result$chrom, "chr1")
})

test_that("parse_coords removes commas from numbers", {
    result <- parse_coords("chr1:1,000,000-2,000,000")

    expect_equal(result$start, 1e6)
    expect_equal(result$end, 2e6)
})

test_that("parse_coords returns NULL for invalid input", {
    expect_null(parse_coords(NULL))
    expect_null(parse_coords(""))
    expect_null(parse_coords("chr1")) # Missing coordinates
    expect_null(parse_coords("chr1:abc-def")) # Non-numeric
})

test_that("format_coords formats correctly", {
    result <- format_coords("chr1", 1000000, 2000000)

    expect_true(grepl("chr1", result))
    expect_true(grepl("1,000,000", result))
    expect_true(grepl("2,000,000", result))
})

test_that("sanitize_interval handles valid interval", {
    interval <- data.frame(chrom = "chr1", start = 1000, end = 2000)
    result <- sanitize_interval(interval)

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("sanitize_interval swaps start/end if needed", {
    interval <- data.frame(chrom = "chr1", start = 2000, end = 1000)
    result <- sanitize_interval(interval)

    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("sanitize_interval ensures start >= 1", {
    interval <- data.frame(chrom = "chr1", start = -100, end = 200)
    result <- sanitize_interval(interval)

    expect_equal(result$start, 1)
})

test_that("sanitize_interval returns NULL for invalid input", {
    expect_null(sanitize_interval(NULL))
    expect_null(sanitize_interval(data.frame(x = 1))) # Missing columns
    expect_null(sanitize_interval(data.frame())) # Empty
})

test_that("sanitize_interval takes first row of multi-row input", {
    interval <- data.frame(
        chrom = c("chr1", "chr2"),
        start = c(1000, 3000),
        end = c(2000, 4000)
    )
    result <- sanitize_interval(interval)

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
})

test_that("make_region creates valid interval", {
    result <- make_region("chr1", 1000, 2000)

    expect_equal(result$chrom, "chr1")
    expect_equal(result$start, 1000)
    expect_equal(result$end, 2000)
})

test_that("make_region handles negative start", {
    result <- make_region("chr1", -500, 1000)

    expect_equal(result$start, 1)
})

# =============================================================================
# Tests for is_empty utility
# =============================================================================

test_that("is_empty handles various empty values", {
    expect_true(is_empty(NULL))
    expect_true(is_empty(NA))
    expect_true(is_empty(""))
    expect_true(is_empty(character(0)))
    expect_true(is_empty(numeric(0)))
})

test_that("is_empty returns FALSE for non-empty values", {
    expect_false(is_empty("text"))
    expect_false(is_empty(5))
    expect_false(is_empty(c(1, 2, 3)))
    expect_false(is_empty(FALSE))
    expect_false(is_empty(0))
})

# =============================================================================
# Tests for get_span
# =============================================================================

test_that("get_span calculates correctly", {
    interval <- make_region("chr1", 1000, 2000)
    result <- get_span(interval)

    expect_equal(result, 1000)
})

test_that("get_span returns 0 for NULL interval", {
    expect_equal(get_span(NULL), 0)
})

# =============================================================================
# Tests for interval_center and interval_width
# =============================================================================

test_that("interval_center calculates center position", {
    interval <- make_region("chr1", 1000, 2000)
    result <- interval_center(interval)

    expect_equal(result, 1500)
})

test_that("interval_center handles NULL", {
    expect_null(interval_center(NULL))
})

test_that("interval_width calculates width", {
    interval <- make_region("chr1", 1000, 2000)
    result <- interval_width(interval)

    expect_equal(result, 1000)
})

test_that("interval_width handles NULL", {
    expect_equal(interval_width(NULL), 0)
})

# =============================================================================
# Tests for resolve_path
# =============================================================================

test_that("resolve_path handles absolute paths", {
    result <- resolve_path("/base", "/absolute/path")

    expect_equal(result, normalizePath("/absolute/path", mustWork = FALSE))
})

test_that("resolve_path handles relative paths", {
    result <- resolve_path("/base/dir", "relative/file.txt")

    expect_true(grepl("base/dir/relative/file.txt", result))
})

test_that("resolve_path handles tilde paths", {
    result <- resolve_path("/base", "~/some/path")

    expect_true(grepl("some/path", result))
})

test_that("resolve_path handles NULL/empty input", {
    expect_null(resolve_path("/base", NULL))
    expect_null(resolve_path("/base", ""))
})

# =============================================================================
# Tests for lookup_color_value
# =============================================================================

test_that("lookup_color_value finds exact match", {
    colors <- list(red = "#FF0000", blue = "#0000FF")
    result <- lookup_color_value("red", colors)

    expect_equal(result, "#FF0000")
})

test_that("lookup_color_value falls back on .k4/.k27 suffix", {
    colors <- list(source = "purple")

    # Should find source color for source.k4
    result <- lookup_color_value("source.k4", colors)
    expect_equal(result, "purple")

    # Should find source color for source.k27
    result <- lookup_color_value("source.k27", colors)
    expect_equal(result, "purple")
})

test_that("lookup_color_value returns NULL for not found", {
    colors <- list(red = "#FF0000")
    result <- lookup_color_value("green", colors)

    expect_null(result)
})

test_that("lookup_color_value handles NULL/empty colors", {
    expect_null(lookup_color_value("red", NULL))
    expect_null(lookup_color_value("red", list()))
})

test_that("lookup_color_value handles vector colors", {
    colors <- c(red = "#FF0000", blue = "#0000FF")
    result <- lookup_color_value("red", colors)

    expect_equal(result, "#FF0000")
})

# =============================================================================
# Tests for null coalescing operator
# =============================================================================

test_that("%||% returns first non-NULL value", {
    expect_equal(NULL %||% "default", "default")
    expect_equal("value" %||% "default", "value")
})

test_that("%||% handles empty vectors", {
    expect_equal(character(0) %||% "default", "default")
    expect_equal(c("a", "b") %||% "default", c("a", "b"))
})

# =============================================================================
# Tests for expand_colors_with_tracks
# =============================================================================

test_that("expand_colors_with_tracks works with panels", {
    colors <- list(track1 = "red")
    panels <- list(
        list(type = "data", tracks = c("track1", "track2"))
    )

    result <- expand_colors_with_tracks(colors, panels)

    expect_true("track1" %in% names(result))
    expect_true("track2" %in% names(result))
    expect_equal(result[["track1"]], "red")
})

test_that("expand_colors_with_tracks handles NULL colors", {
    panels <- list(
        list(type = "data", tracks = c("track1", "track2"))
    )
    result <- expand_colors_with_tracks(NULL, panels)

    expect_true("track1" %in% names(result))
    expect_true("track2" %in% names(result))
})

test_that("expand_colors_with_tracks handles NULL panels", {
    colors <- list(track1 = "red")
    result <- expand_colors_with_tracks(colors, NULL)

    expect_true("track1" %in% names(result))
})
