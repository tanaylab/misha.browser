# Tests for intervals panel functions

# =============================================================================
# Tests for assign_interval_levels (sweep-line algorithm)
# =============================================================================

test_that("assign_interval_levels handles empty intervals", {
    intervals <- data.frame(start = integer(), end = integer())
    result <- assign_interval_levels(intervals)
    expect_equal(result, integer(0))
})

test_that("assign_interval_levels handles single interval", {
    intervals <- data.frame(start = 100, end = 200)
    result <- assign_interval_levels(intervals)
    expect_equal(result, 1L)
})

test_that("assign_interval_levels handles non-overlapping intervals", {
    # All on same level since they don't overlap
    intervals <- data.frame(
        start = c(100, 300, 500),
        end = c(150, 350, 550)
    )
    result <- assign_interval_levels(intervals)

    # All should be on level 1 since they don't overlap
    expect_equal(result, c(1L, 1L, 1L))
})

test_that("assign_interval_levels handles overlapping intervals", {
    # These intervals overlap so need different levels
    intervals <- data.frame(
        start = c(100, 120, 140),
        end = c(200, 220, 240)
    )
    result <- assign_interval_levels(intervals)

    # Each should be on different level
    expect_length(unique(result), 3)
})

test_that("assign_interval_levels handles partial overlaps correctly", {
    # First and second overlap, but third doesn't overlap with first
    intervals <- data.frame(
        start = c(100, 150, 250),
        end = c(200, 300, 350)
    )
    result <- assign_interval_levels(intervals)

    # 1 and 2 should be different levels
    expect_true(result[1] != result[2])
    # 3 can reuse level 1 since it doesn't overlap with 1
    expect_equal(result[3], result[1])
})

test_that("assign_interval_levels handles unsorted intervals", {
    # Intervals not in order by start
    intervals <- data.frame(
        start = c(300, 100, 200),
        end = c(400, 150, 250)
    )
    result <- assign_interval_levels(intervals)

    # Should handle unsorted correctly and return in original order
    expect_length(result, 3)
    # None overlap (after sorting: 100-150, 200-250, 300-400)
    expect_equal(result, c(1L, 1L, 1L))
})

test_that("assign_interval_levels handles complex overlap pattern", {
    # Pattern where some intervals can be stacked
    #    |---1---|
    #        |---2---|
    #            |---3---|
    #                |---4---|     |---5---|
    intervals <- data.frame(
        start = c(100, 150, 200, 250, 400),
        end = c(200, 250, 300, 350, 500)
    )
    result <- assign_interval_levels(intervals)

    # 1,2,3,4 form a chain, 5 can reuse earlier level
    expect_true(all(result >= 1))
    # Intervals 5 should reuse an earlier level
    expect_true(result[5] <= max(result[1:4]))
})

test_that("assign_interval_levels respects max_levels limit", {
    # Create many overlapping intervals
    n <- 30
    intervals <- data.frame(
        start = seq(100, by = 10, length.out = n),
        end = seq(200, by = 10, length.out = n) # All overlap
    )
    result <- assign_interval_levels(intervals)

    # Should not exceed max_levels (20)
    expect_true(max(result) <= 20)
})

test_that("assign_interval_levels handles touching intervals", {
    # Intervals that touch (end == start of next)
    intervals <- data.frame(
        start = c(100, 200, 300),
        end = c(200, 300, 400)
    )
    result <- assign_interval_levels(intervals)

    # Touching intervals can be on same level (no actual overlap)
    expect_equal(result, c(1L, 1L, 1L))
})

test_that("assign_interval_levels handles identical intervals", {
    intervals <- data.frame(
        start = c(100, 100, 100),
        end = c(200, 200, 200)
    )
    result <- assign_interval_levels(intervals)

    # All identical, so all need different levels
    expect_length(unique(result), 3)
})

# =============================================================================
# Tests for extract_intervals_data
# =============================================================================

test_that("extract_intervals_data returns NULL for NULL region", {
    panel <- list(source = "intervals", intervals = "test")
    result <- extract_intervals_data(panel, NULL)
    expect_null(result)
})

test_that("extract_intervals_data returns NULL for invalid region", {
    panel <- list(source = "intervals", intervals = "test")
    result <- extract_intervals_data(panel, data.frame(x = 1))
    expect_null(result)
})

test_that("extract_intervals_data returns NULL for missing file", {
    panel <- list(source = "file", file = "/nonexistent/path/file.bed")
    region <- data.frame(chrom = "chr1", start = 100, end = 200)
    result <- extract_intervals_data(panel, region)
    expect_null(result)
})

test_that("extract_intervals_data uses resolved file path", {
    # Create a temp file
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(chrom = "chr1", start = 100, end = 200),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )

    panel <- list(
        source = "file",
        file = temp_file, # Original path
        ._resolved_file = temp_file # Resolved path (same here)
    )
    region <- data.frame(chrom = "chr1", start = 50, end = 250)
    result <- extract_intervals_data(panel, region)

    expect_true(!is.null(result))
    expect_equal(nrow(result), 1)

    unlink(temp_file)
})

test_that("extract_intervals_data filters by chromosome", {
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(
            chrom = c("chr1", "chr2", "chr1"),
            start = c(100, 100, 200),
            end = c(150, 150, 250)
        ),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )

    panel <- list(source = "file", file = temp_file)
    region <- data.frame(chrom = "chr1", start = 50, end = 500)
    result <- extract_intervals_data(panel, region)

    expect_equal(nrow(result), 2) # Only chr1 intervals

    unlink(temp_file)
})

test_that("extract_intervals_data handles column name normalization", {
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(
            chr = "chr1", # 'chr' instead of 'chrom'
            chromStart = 100, # chromStart instead of start
            chromEnd = 200 # chromEnd instead of end
        ),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )

    panel <- list(source = "file", file = temp_file)
    region <- data.frame(chrom = "chr1", start = 50, end = 500)
    result <- extract_intervals_data(panel, region)

    expect_true(!is.null(result))
    expect_true("chrom" %in% names(result))
    expect_true("start" %in% names(result))
    expect_true("end" %in% names(result))

    unlink(temp_file)
})

test_that("extract_intervals_data applies value filter", {
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(
            chrom = c("chr1", "chr1", "chr1"),
            start = c(100, 200, 300),
            end = c(150, 250, 350),
            type = c("A", "B", "A")
        ),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )

    panel <- list(
        source = "file",
        file = temp_file,
        filter_field = "type",
        filter_values = c("A")
    )
    region <- data.frame(chrom = "chr1", start = 50, end = 500)
    result <- extract_intervals_data(panel, region)

    expect_equal(nrow(result), 2)
    expect_true(all(result$type == "A"))

    unlink(temp_file)
})

test_that("extract_intervals_data filters to overlapping intervals only", {
    # domain_A contains the region (HiC domain larger than view); domain_B does not overlap
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(
            chrom = c("chr1", "chr1"),
            start = c(50, 500),   # domain_B (500-600) does not overlap region (100-400)
            end = c(500, 600),
            name = c("domain_A", "domain_B")
        ),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )

    panel <- list(source = "file", file = temp_file)
    region <- data.frame(chrom = "chr1", start = 100, end = 400)
    result <- extract_intervals_data(panel, region)

    # domain_A (50-500) overlaps; domain_B (500-600) does not
    expect_equal(nrow(result), 1)
    expect_equal(result$name, "domain_A")

    unlink(temp_file)
})

test_that("render_intervals_panel clips large intervals to visible region", {
    # Interval spanning 1-1e6, region 100-400: should be clipped to 100-400 for display
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(chrom = "chr1", start = 1, end = 1e6, name = "big_domain"),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )
    panel <- list(
        source = "file",
        file = temp_file,
        name = "tads",
        type = "intervals",
        color = "coral"
    )
    region <- data.frame(chrom = "chr1", start = 100, end = 400)

    result <- render_intervals_panel(panel, region)
    expect_s3_class(result, "ggplot")

    # The geom_rect layer should have clipped coords (start=100, end=400)
    rect_layer <- result$layers[[which(sapply(result$layers, function(l) inherits(l$geom, "GeomRect")))]]
    rect_data <- rect_layer$data
    expect_true(all(rect_data$start >= 100))
    expect_true(all(rect_data$end <= 400))

    unlink(temp_file)
})

test_that("extract_intervals_data applies regex filter", {
    temp_file <- tempfile(fileext = ".tsv")
    write.table(
        data.frame(
            chrom = c("chr1", "chr1", "chr1"),
            start = c(100, 200, 300),
            end = c(150, 250, 350),
            name = c("gene_ABC", "gene_XYZ", "other_ABC")
        ),
        temp_file,
        sep = "\t",
        row.names = FALSE
    )

    panel <- list(
        source = "file",
        file = temp_file,
        filter_field = "name",
        filter_regex = "^gene_"
    )
    region <- data.frame(chrom = "chr1", start = 50, end = 500)
    result <- extract_intervals_data(panel, region)

    expect_equal(nrow(result), 2)
    expect_true(all(grepl("^gene_", result$name)))

    unlink(temp_file)
})

# =============================================================================
# Tests for resolve_interval_colors
# =============================================================================

test_that("resolve_interval_colors uses panel colors", {
    values <- c("A", "B")
    panel_colors <- list(A = "red", B = "blue")

    result <- resolve_interval_colors(values, panel_colors, list())

    expect_equal(result["A"], c(A = "red"))
    expect_equal(result["B"], c(B = "blue"))
})

test_that("resolve_interval_colors falls back to config colors", {
    values <- c("A", "B")
    panel_colors <- list(A = "red") # Only has A
    cfg_colors <- list(B = "green")

    result <- resolve_interval_colors(values, panel_colors, cfg_colors)

    expect_equal(result["A"], c(A = "red"))
    expect_equal(result["B"], c(B = "green"))
})

test_that("resolve_interval_colors handles suffix fallback", {
    values <- c("source.k4", "source.k27")
    cfg_colors <- list(source = "purple")

    result <- resolve_interval_colors(values, list(), cfg_colors)

    # Both should resolve to source color via fallback
    expect_equal(result["source.k4"], c(source.k4 = "purple"))
    expect_equal(result["source.k27"], c(source.k27 = "purple"))
})

test_that("resolve_interval_colors uses default color for unknown", {
    values <- c("unknown_value")
    result <- resolve_interval_colors(values, list(), list())

    # Should get a color (default or generated), not NA
    expect_false(is.na(result["unknown_value"]))
})

test_that("resolve_interval_colors handles empty values", {
    result <- resolve_interval_colors(character(0), list(), list())
    expect_length(result, 0)
})

test_that("resolve_interval_colors handles NULL colors lists", {
    values <- c("A", "B")
    result <- resolve_interval_colors(values, NULL, NULL)

    expect_length(result, 2)
    expect_false(any(is.na(result)))
})

# =============================================================================
# Tests for render_intervals_panel (basic, no real ggplot check)
# =============================================================================

test_that("render_intervals_panel returns ggplot for NULL region", {
    panel <- list(name = "test", type = "intervals")
    result <- render_intervals_panel(panel, NULL)

    expect_s3_class(result, "ggplot")
})

test_that("render_intervals_panel returns ggplot for invalid region", {
    panel <- list(name = "test", type = "intervals")
    result <- render_intervals_panel(panel, data.frame(x = 1))

    expect_s3_class(result, "ggplot")
})
