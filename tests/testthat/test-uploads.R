# Tests for upload functions (PSSM, intervals parsing)

# =============================================================================
# Tests for upload storage management
# =============================================================================

test_that("init_upload_storage creates empty storage", {
    # Clear storage first
    clear_upload_storage()

    init_upload_storage()

    intervals <- get_uploaded_intervals()
    pssms <- get_uploaded_pssms()

    expect_type(intervals, "list")
    expect_type(pssms, "list")
    expect_length(intervals, 0)
    expect_length(pssms, 0)
})

test_that("store and retrieve uploaded intervals", {
    clear_upload_storage()

    test_data <- data.frame(
        chrom = c("chr1", "chr1", "chr2"),
        start = c(100, 200, 300),
        end = c(150, 250, 350)
    )

    store_uploaded_intervals("test_intervals", test_data, "test.bed")

    # Retrieve all
    all_intervals <- get_uploaded_intervals()
    expect_length(all_intervals, 1)
    expect_true("test_intervals" %in% names(all_intervals))

    # Retrieve by name
    retrieved <- get_uploaded_intervals_by_name("test_intervals")
    expect_equal(nrow(retrieved$data), 3)
    expect_equal(retrieved$filename, "test.bed")
    expect_equal(retrieved$rows, 3)
})

test_that("delete uploaded intervals", {
    clear_upload_storage()

    test_data <- data.frame(chrom = "chr1", start = 100, end = 200)
    store_uploaded_intervals("to_delete", test_data)

    expect_true(!is.null(get_uploaded_intervals_by_name("to_delete")))

    delete_uploaded_intervals("to_delete")

    expect_true(is.null(get_uploaded_intervals_by_name("to_delete")))
})

test_that("get_uploaded_intervals_by_name returns NULL for non-existent", {
    clear_upload_storage()
    expect_true(is.null(get_uploaded_intervals_by_name("nonexistent")))
})

test_that("store and retrieve uploaded PSSMs", {
    clear_upload_storage()

    test_pssm <- matrix(
        c(
            0.25, 0.25, 0.25, 0.25,
            0.9, 0.03, 0.04, 0.03,
            0.1, 0.1, 0.7, 0.1
        ),
        nrow = 3, byrow = TRUE
    )
    colnames(test_pssm) <- c("A", "C", "G", "T")

    store_uploaded_pssm("test_motif", test_pssm, "motif.txt")

    # Retrieve all
    all_pssms <- get_uploaded_pssms()
    expect_length(all_pssms, 1)

    # Retrieve by name
    retrieved <- get_uploaded_pssm_by_name("test_motif")
    expect_equal(nrow(retrieved), 3)
    expect_equal(ncol(retrieved), 4)
})

test_that("delete uploaded PSSMs", {
    clear_upload_storage()

    test_pssm <- matrix(rep(0.25, 8), nrow = 2)
    colnames(test_pssm) <- c("A", "C", "G", "T")
    store_uploaded_pssm("to_delete", test_pssm)

    expect_true(!is.null(get_uploaded_pssm_by_name("to_delete")))

    delete_uploaded_pssm("to_delete")

    expect_null(get_uploaded_pssm_by_name("to_delete"))
})

test_that("clear_upload_storage clears all data", {
    # Add some data
    test_data <- data.frame(chrom = "chr1", start = 100, end = 200)
    store_uploaded_intervals("intervals1", test_data)

    test_pssm <- matrix(rep(0.25, 4), nrow = 1)
    colnames(test_pssm) <- c("A", "C", "G", "T")
    store_uploaded_pssm("pssm1", test_pssm)

    # Clear
    clear_upload_storage()

    expect_length(get_uploaded_intervals(), 0)
    expect_length(get_uploaded_pssms(), 0)
})

# =============================================================================
# Tests for intervals file parsing
# =============================================================================

test_that("detect_intervals_format detects BED files", {
    # Create a temp BED file
    temp_file <- tempfile(fileext = ".bed")
    writeLines("chr1\t100\t200", temp_file)

    result <- detect_intervals_format(temp_file)
    expect_equal(result, "bed")

    unlink(temp_file)
})

test_that("detect_intervals_format detects TSV with header", {
    temp_file <- tempfile(fileext = ".tsv")
    writeLines(c("chrom\tstart\tend", "chr1\t100\t200"), temp_file)

    result <- detect_intervals_format(temp_file)
    expect_equal(result, "tsv")

    unlink(temp_file)
})

test_that("parse_bed_file parses correctly", {
    temp_file <- tempfile(fileext = ".bed")
    writeLines(c(
        "chr1\t100\t200",
        "chr2\t300\t400",
        "chr1\t500\t600"
    ), temp_file)

    result <- parse_bed_file(temp_file)

    expect_equal(nrow(result), 3)
    expect_equal(result$chrom[1], "chr1")
    expect_equal(result$start[1], 100)
    expect_equal(result$end[1], 200)

    unlink(temp_file)
})

test_that("parse_bed_file fails with insufficient columns", {
    temp_file <- tempfile(fileext = ".bed")
    writeLines(c("chr1\t100"), temp_file)

    expect_error(parse_bed_file(temp_file), "at least 3 columns")

    unlink(temp_file)
})

test_that("parse_tsv_intervals parses TSV with header", {
    temp_file <- tempfile(fileext = ".tsv")
    writeLines(c(
        "chrom\tstart\tend",
        "chr1\t100\t200",
        "chr2\t300\t400"
    ), temp_file)

    result <- parse_tsv_intervals(temp_file)

    expect_equal(nrow(result), 2)
    expect_true(all(c("chrom", "start", "end") %in% names(result)))

    unlink(temp_file)
})

test_that("parse_tsv_intervals handles alternate column names", {
    temp_file <- tempfile(fileext = ".tsv")
    writeLines(c(
        "chromosome\tchromStart\tchromEnd",
        "chr1\t100\t200"
    ), temp_file)

    result <- parse_tsv_intervals(temp_file)

    expect_equal(nrow(result), 1)
    expect_equal(result$chrom[1], "chr1")

    unlink(temp_file)
})

test_that("parse_tsv_intervals handles CSV format", {
    temp_file <- tempfile(fileext = ".csv")
    writeLines(c(
        "chrom,start,end",
        "chr1,100,200"
    ), temp_file)

    result <- parse_tsv_intervals(temp_file)

    expect_equal(nrow(result), 1)
    expect_equal(result$chrom[1], "chr1")

    unlink(temp_file)
})

test_that("parse_intervals_file auto-detects format", {
    # BED file
    temp_bed <- tempfile(fileext = ".bed")
    writeLines("chr1\t100\t200", temp_bed)

    result <- parse_intervals_file(temp_bed, format = "auto")
    expect_equal(nrow(result), 1)

    # TSV file
    temp_tsv <- tempfile(fileext = ".tsv")
    writeLines(c("chrom\tstart\tend", "chr1\t100\t200"), temp_tsv)

    result <- parse_intervals_file(temp_tsv, format = "auto")
    expect_equal(nrow(result), 1)

    unlink(c(temp_bed, temp_tsv))
})

# =============================================================================
# Tests for intervals validation
# =============================================================================

test_that("validate_intervals_data validates required columns", {
    # Missing columns
    bad_data <- data.frame(chrom = "chr1", start = 100)
    expect_error(validate_intervals_data(bad_data), "Missing required columns")
})

test_that("validate_intervals_data rejects empty data", {
    empty_data <- data.frame(chrom = character(), start = integer(), end = integer())
    expect_error(validate_intervals_data(empty_data), "empty")
})

test_that("validate_intervals_data rejects NA values", {
    # NA in chrom
    bad_data <- data.frame(chrom = c("chr1", NA), start = c(100, 200), end = c(150, 250))
    expect_error(validate_intervals_data(bad_data), "NA values")

    # NA in coordinates
    bad_data <- data.frame(chrom = c("chr1", "chr1"), start = c(100, NA), end = c(150, 250))
    expect_error(validate_intervals_data(bad_data), "NA values")
})

test_that("validate_intervals_data rejects invalid intervals", {
    # start >= end
    bad_data <- data.frame(chrom = "chr1", start = 200, end = 100)
    expect_error(validate_intervals_data(bad_data), "start must be less than end")
})

test_that("validate_intervals_data accepts valid data", {
    good_data <- data.frame(
        chrom = c("chr1", "chr2"),
        start = c(100, 200),
        end = c(150, 250)
    )
    expect_true(validate_intervals_data(good_data))
})

# =============================================================================
# Tests for PSSM parsing
# =============================================================================

test_that("detect_pssm_format detects MEME files", {
    temp_file <- tempfile(fileext = ".meme")
    writeLines("MEME version 4", temp_file)

    result <- detect_pssm_format(temp_file)
    expect_equal(result, "meme")

    unlink(temp_file)
})

test_that("detect_pssm_format detects JASPAR files", {
    temp_file <- tempfile(fileext = ".jaspar")
    writeLines(c(">MA0001.1 AGL3"), temp_file)

    result <- detect_pssm_format(temp_file)
    expect_equal(result, "jaspar")

    unlink(temp_file)
})

test_that("detect_pssm_format defaults to TSV", {
    temp_file <- tempfile(fileext = ".txt")
    writeLines("A\tC\tG\tT", temp_file)

    result <- detect_pssm_format(temp_file)
    expect_equal(result, "tsv")

    unlink(temp_file)
})

test_that("parse_tsv_pssm parses with header", {
    temp_file <- tempfile(fileext = ".tsv")
    writeLines(c(
        "A\tC\tG\tT",
        "0.25\t0.25\t0.25\t0.25",
        "0.9\t0.03\t0.04\t0.03"
    ), temp_file)

    result <- parse_tsv_pssm(temp_file)

    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 4)
    expect_equal(colnames(result), c("A", "C", "G", "T"))

    unlink(temp_file)
})

test_that("parse_tsv_pssm parses without header", {
    temp_file <- tempfile(fileext = ".tsv")
    writeLines(c(
        "0.25\t0.25\t0.25\t0.25",
        "0.9\t0.03\t0.04\t0.03"
    ), temp_file)

    result <- parse_tsv_pssm(temp_file)

    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 4)

    unlink(temp_file)
})

test_that("parse_tsv_pssm fails with insufficient columns", {
    temp_file <- tempfile(fileext = ".tsv")
    writeLines("0.5\t0.5\t0.5", temp_file) # Only 3 columns

    expect_error(parse_tsv_pssm(temp_file), "at least 4 columns")

    unlink(temp_file)
})

test_that("parse_jaspar_pssm parses correctly", {
    temp_file <- tempfile(fileext = ".jaspar")
    writeLines(c(
        ">MA0001.1 AGL3",
        "A [ 0 3 79 40 66 48 65 11 65 0 ]",
        "C [ 94 75 4 3 1 2 5 2 3 3 ]",
        "G [ 1 0 3 4 1 0 5 3 28 88 ]",
        "T [ 2 19 11 50 29 47 22 81 1 6 ]"
    ), temp_file)

    result <- parse_jaspar_pssm(temp_file)

    expect_equal(nrow(result), 10) # 10 positions
    expect_equal(ncol(result), 4)
    expect_equal(colnames(result), c("A", "C", "G", "T"))

    unlink(temp_file)
})

test_that("parse_jaspar_pssm fails with wrong number of lines", {
    temp_file <- tempfile(fileext = ".jaspar")
    writeLines(c(
        ">MA0001.1 AGL3",
        "A [ 0 3 ]",
        "C [ 94 75 ]"
        # Missing G and T lines
    ), temp_file)

    expect_error(parse_jaspar_pssm(temp_file), "exactly 4 lines")

    unlink(temp_file)
})

# =============================================================================
# Tests for PSSM validation (Issue #23 - normalization result)
# =============================================================================

test_that("validate_pssm returns normalized matrix for counts", {
    # Create a count matrix (rows don't sum to 1)
    counts <- matrix(
        c(
            10, 10, 10, 10, # Sum = 40
            36, 4, 4, 4, # Sum = 48
            5, 5, 40, 5 # Sum = 55
        ),
        nrow = 3, byrow = TRUE
    )
    colnames(counts) <- c("A", "C", "G", "T")

    result <- validate_pssm(counts)

    # Result should be normalized (rows sum to 1)
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 4)
    row_sums <- rowSums(result)
    expect_true(all(abs(row_sums - 1) < 0.01))
})

test_that("validate_pssm preserves probability matrix", {
    # Create a probability matrix (rows sum to 1)
    probs <- matrix(
        c(
            0.25, 0.25, 0.25, 0.25,
            0.9, 0.03, 0.04, 0.03,
            0.1, 0.1, 0.7, 0.1
        ),
        nrow = 3, byrow = TRUE
    )
    colnames(probs) <- c("A", "C", "G", "T")

    result <- validate_pssm(probs)

    # Should be unchanged (already probabilities)
    expect_equal(result, probs)
})

test_that("validate_pssm rejects invalid column count", {
    bad_pssm <- matrix(1:6, nrow = 2)
    colnames(bad_pssm) <- c("A", "C", "G")

    expect_error(validate_pssm(bad_pssm), "exactly 4 columns")
})

test_that("validate_pssm rejects wrong column names", {
    bad_pssm <- matrix(rep(0.25, 8), nrow = 2)
    colnames(bad_pssm) <- c("X", "Y", "Z", "W")

    expect_error(validate_pssm(bad_pssm), "named A, C, G, T")
})

test_that("validate_pssm rejects empty matrix", {
    empty_pssm <- matrix(nrow = 0, ncol = 4)
    colnames(empty_pssm) <- c("A", "C", "G", "T")

    expect_error(validate_pssm(empty_pssm), "empty")
})

test_that("validate_pssm rejects negative values", {
    bad_pssm <- matrix(c(-0.1, 0.4, 0.4, 0.3), nrow = 1)
    colnames(bad_pssm) <- c("A", "C", "G", "T")

    expect_error(validate_pssm(bad_pssm), "negative")
})

test_that("validate_pssm rejects NA values", {
    bad_pssm <- matrix(c(0.25, NA, 0.25, 0.25), nrow = 1)
    colnames(bad_pssm) <- c("A", "C", "G", "T")

    expect_error(validate_pssm(bad_pssm), "NA")
})

# =============================================================================
# Tests for vtrack function type helpers
# =============================================================================

test_that("is_sequence_function identifies sequence functions", {
    # Sequence functions
    expect_true(is_sequence_function("pwm"))
    expect_true(is_sequence_function("pwm.max"))
    expect_true(is_sequence_function("pwm.max.pos"))
    expect_true(is_sequence_function("pwm.count"))
    expect_true(is_sequence_function("kmer.count"))
    expect_true(is_sequence_function("kmer.frac"))
    expect_true(is_sequence_function("masked.count"))
    expect_true(is_sequence_function("masked.frac"))

    # Non-sequence functions
    expect_false(is_sequence_function("sum"))
    expect_false(is_sequence_function("avg"))
    expect_false(is_sequence_function("distance"))
})

test_that("is_intervals_function identifies intervals functions", {
    expect_true(is_intervals_function("distance"))
    expect_true(is_intervals_function("distance.center"))
    expect_true(is_intervals_function("distance.edge"))
    expect_true(is_intervals_function("coverage"))
    expect_true(is_intervals_function("neighbor.count"))

    expect_false(is_intervals_function("sum"))
    expect_false(is_intervals_function("pwm"))
})

test_that("is_pwm_function identifies PWM functions", {
    expect_true(is_pwm_function("pwm"))
    expect_true(is_pwm_function("pwm.max"))
    expect_true(is_pwm_function("pwm.max.pos"))
    expect_true(is_pwm_function("pwm.count"))

    expect_false(is_pwm_function("kmer.frac"))
    expect_false(is_pwm_function("sum"))
})

test_that("is_kmer_function identifies kmer functions", {
    expect_true(is_kmer_function("kmer.count"))
    expect_true(is_kmer_function("kmer.frac"))

    expect_false(is_kmer_function("pwm"))
    expect_false(is_kmer_function("sum"))
})

test_that("requires_source_track returns correct values", {
    # Track-based functions require source
    expect_true(requires_source_track("sum"))
    expect_true(requires_source_track("avg"))
    expect_true(requires_source_track("min"))
    expect_true(requires_source_track("max"))

    # Sequence functions don't require source
    expect_false(requires_source_track("pwm"))
    expect_false(requires_source_track("kmer.frac"))

    # Intervals functions don't require source track (require intervals source)
    expect_false(requires_source_track("distance"))
})

# =============================================================================
# Tests for intervals/PSSM source resolution
# =============================================================================

test_that("resolve_intervals_source handles uploaded intervals", {
    clear_upload_storage()

    test_data <- data.frame(
        chrom = c("chr1", "chr2"),
        start = c(100, 200),
        end = c(150, 250)
    )
    store_uploaded_intervals("test_upload", test_data)

    result <- resolve_intervals_source("@uploaded:test_upload")

    expect_equal(nrow(result), 2)
    expect_equal(result$chrom[1], "chr1")
})

test_that("resolve_intervals_source fails for missing uploaded", {
    clear_upload_storage()

    expect_error(
        resolve_intervals_source("@uploaded:nonexistent"),
        "not found"
    )
})

test_that("resolve_intervals_source requires non-empty source", {
    expect_error(resolve_intervals_source(NULL), "required")
    expect_error(resolve_intervals_source(""), "required")
})

test_that("resolve_pssm_source handles uploaded PSSMs", {
    clear_upload_storage()

    test_pssm <- matrix(rep(0.25, 8), nrow = 2)
    colnames(test_pssm) <- c("A", "C", "G", "T")
    store_uploaded_pssm("test_motif", test_pssm)

    result <- resolve_pssm_source("@uploaded:test_motif")

    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 4)
})

test_that("resolve_pssm_source fails for missing uploaded", {
    clear_upload_storage()

    expect_error(
        resolve_pssm_source("@uploaded:nonexistent"),
        "not found"
    )
})

test_that("resolve_pssm_source requires non-empty source", {
    expect_error(resolve_pssm_source(NULL), "required")
    expect_error(resolve_pssm_source(""), "required")
})

# =============================================================================
# Tests for vtrack function choices
# =============================================================================

test_that("get_vtrack_function_choices returns all categories", {
    choices <- get_vtrack_function_choices()

    expect_type(choices, "list")
    expect_true("Track Summary" %in% names(choices))
    expect_true("Point Values" %in% names(choices))
    expect_true("Global Stats" %in% names(choices))
    expect_true("Position" %in% names(choices))
    expect_true("Intervals" %in% names(choices))
    expect_true("Sequence - PWM" %in% names(choices))
    expect_true("Sequence - Kmer" %in% names(choices))
    expect_true("Sequence - Masked" %in% names(choices))
})

test_that("get_vtrack_function_choices includes common functions", {
    choices <- get_vtrack_function_choices()

    all_funcs <- unlist(choices)
    expect_true("sum" %in% all_funcs)
    expect_true("avg" %in% all_funcs)
    expect_true("quantile" %in% all_funcs)
    expect_true("pwm.max" %in% all_funcs)
    expect_true("kmer.frac" %in% all_funcs)
})

# =============================================================================
# Tests for choices builders
# =============================================================================

test_that("build_intervals_choices handles empty uploaded", {
    choices <- build_intervals_choices(list())

    # Should at least have Database group or "No intervals" message
    expect_type(choices, "list")
})

test_that("build_intervals_choices includes uploaded intervals", {
    uploaded <- list(
        my_intervals = list(data = data.frame(), rows = 10, filename = "test.bed")
    )

    choices <- build_intervals_choices(uploaded)

    expect_true("Uploaded" %in% names(choices))
    expect_true(any(grepl("@uploaded:my_intervals", names(choices$Uploaded))))
})

test_that("build_pssm_choices handles empty uploaded", {
    choices <- build_pssm_choices(list())
    expect_type(choices, "list")
})

test_that("build_pssm_choices includes uploaded PSSMs", {
    uploaded <- list(
        my_motif = list(data = matrix(), dimensions = "10x4", filename = "motif.txt")
    )

    choices <- build_pssm_choices(uploaded)

    expect_true("Uploaded" %in% names(choices))
    expect_true(any(grepl("@uploaded:my_motif", names(choices$Uploaded))))
})
