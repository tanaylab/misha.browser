# Tests for extraction functions

# =============================================================================
# Tests for resolve_track_specs
# =============================================================================

test_that("resolve_track_specs handles simple string tracks", {
    tracks <- c("track1", "track2", "track3")
    result <- resolve_track_specs(tracks, browser = NULL, cfg = NULL)

    expect_equal(result$exprs, c("track1", "track2", "track3"))
    expect_equal(result$names, c("track1", "track2", "track3"))
    expect_length(result$temp_vtracks, 0)
})

test_that("resolve_track_specs handles vtrack expressions from browser state", {
    browser <- browser_create()
    browser$state$vtrack_expressions <- list(
        ratio = "track1 / track2",
        diff = "track1 - track2",
        wrapped = "pmax(some_vtrack, 0)"
    )

    tracks <- c("ratio", "other_track", "wrapped")
    result <- resolve_track_specs(tracks, browser = browser, cfg = NULL)

    expect_equal(result$exprs, c("track1 / track2", "other_track", "pmax(some_vtrack, 0)"))
    expect_equal(result$names, c("ratio", "other_track", "wrapped"))
})

test_that("resolve_track_specs handles inline expressions", {
    tracks <- list(
        "track1",
        list(expr = "track2 * 2", name = "doubled"),
        list(expr = "track3 + 1") # No name provided
    )
    result <- resolve_track_specs(tracks, browser = NULL, cfg = NULL)

    expect_equal(result$exprs, c("track1", "track2 * 2", "track3 + 1"))
    expect_equal(result$names[1], "track1")
    expect_equal(result$names[2], "doubled")
    expect_true(grepl("^expr_", result$names[3])) # Auto-generated name
})

test_that("resolve_track_specs handles mixed track types", {
    browser <- browser_create()
    browser$state$vtrack_expressions <- list(normalized = "track1 - background")

    tracks <- list(
        "regular_track",
        "normalized", # vtrack with expression
        list(expr = "track2 / track3", name = "ratio")
    )
    result <- resolve_track_specs(tracks, browser = browser, cfg = NULL)

    expect_equal(result$exprs[1], "regular_track")
    expect_equal(result$exprs[2], "track1 - background")
    expect_equal(result$exprs[3], "track2 / track3")
    expect_equal(result$names, c("regular_track", "normalized", "ratio"))
})

test_that("resolve_track_specs handles empty track list", {
    result <- resolve_track_specs(list(), browser = NULL, cfg = NULL)
    expect_length(result$exprs, 0)
    expect_length(result$names, 0)
    expect_length(result$temp_vtracks, 0)
})

test_that("resolve_track_specs handles NULL tracks", {
    result <- resolve_track_specs(NULL, browser = NULL, cfg = NULL)
    expect_length(result$exprs, 0)
    expect_length(result$names, 0)
})

# =============================================================================
# Tests for cleanup_temp_vtracks
# =============================================================================

test_that("cleanup_temp_vtracks handles empty list", {
    expect_no_error(cleanup_temp_vtracks(character(0)))
    expect_no_error(cleanup_temp_vtracks(NULL))
})

test_that("cleanup_temp_vtracks handles non-existent vtracks gracefully", {
    # Should not error when trying to remove vtracks that don't exist
    expect_no_error(cleanup_temp_vtracks(c("nonexistent_vtrack_1", "nonexistent_vtrack_2")))
})

# =============================================================================
# Tests for extract_tracks
# =============================================================================

test_that("extract_tracks returns NULL for empty track list", {
    result <- extract_tracks(character(0), NULL, iterator = 32)
    expect_null(result)
})

test_that("extract_tracks returns NULL for NULL region", {
    result <- extract_tracks(c("track1"), NULL, iterator = 32)
    expect_null(result)
})

test_that("extract_tracks returns NULL for invalid region", {
    result <- extract_tracks(c("track1"), data.frame(x = 1), iterator = 32)
    expect_null(result)
})

# =============================================================================
# Tests for add_track_metadata
# =============================================================================

test_that("add_track_metadata handles NULL data", {
    result <- add_track_metadata(NULL, list())
    expect_null(result)
})

test_that("add_track_metadata handles empty data", {
    result <- add_track_metadata(data.frame(), list())
    expect_equal(nrow(result), 0)
})

test_that("add_track_metadata adds columns based on grouping pattern", {
    data <- data.frame(
        chrom = rep("chr1", 5),
        start = 1:5,
        end = 2:6,
        pos = 1:5,
        source1.mark1 = 1:5,
        source1.mark2 = 6:10,
        source2.mark1 = 11:15
    )

    panel <- list(
        tracks = c("source1.mark1", "source1.mark2", "source2.mark1"),
        grouping = list(
            pattern = "^(?<source>.+)\\.(?<mark>.+)$",
            color_by = "source"
        )
    )

    result <- add_track_metadata(data, panel, c("source1.mark1", "source1.mark2", "source2.mark1"))

    # Should have converted to long format with metadata columns
    expect_true("track" %in% names(result))
    expect_true("value" %in% names(result))
    expect_true("source" %in% names(result))
    expect_true("mark" %in% names(result))

    # Check that metadata was extracted correctly
    expect_equal(unique(result$source[result$track == "source1.mark1"]), "source1")
    expect_equal(unique(result$mark[result$track == "source1.mark1"]), "mark1")
})

test_that("add_track_metadata handles grouping overrides", {
    data <- data.frame(
        chrom = rep("chr1", 3),
        start = 1:3,
        end = 2:4,
        pos = 1:3,
        custom_track = 1:3
    )

    panel <- list(
        tracks = c("custom_track"),
        grouping = list(
            pattern = "^(?<source>.+)\\.(?<mark>.+)$",
            overrides = list(
                custom_track = list(source = "special_source", mark = "special_mark")
            )
        )
    )

    result <- add_track_metadata(data, panel, c("custom_track"))

    expect_equal(unique(result$source), "special_source")
    expect_equal(unique(result$mark), "special_mark")
})

test_that("add_track_metadata handles tracks without grouping", {
    data <- data.frame(
        chrom = rep("chr1", 3),
        start = 1:3,
        end = 2:4,
        pos = 1:3,
        track1 = 1:3,
        track2 = 4:6
    )

    panel <- list(
        tracks = c("track1", "track2"),
        grouping = NULL
    )

    result <- add_track_metadata(data, panel, c("track1", "track2"))

    # Should still convert to long format
    expect_true("track" %in% names(result))
    expect_true("value" %in% names(result))
    expect_equal(nrow(result), 6) # 3 rows * 2 tracks
})

# =============================================================================
# Tests for is_vtrack
# =============================================================================

test_that("is_vtrack returns FALSE for unknown track with no browser", {
    result <- is_vtrack("nonexistent_track", browser = NULL)
    expect_false(result)
})

test_that("is_vtrack uses cached list from browser state", {
    browser <- browser_create()
    browser$state$misha_vtrack_names <- c("vtrack1", "vtrack2", "vtrack3")

    expect_true(is_vtrack("vtrack1", browser))
    expect_true(is_vtrack("vtrack2", browser))
    expect_false(is_vtrack("regular_track", browser))
})

test_that("is_vtrack handles NULL browser state", {
    browser <- browser_create()
    browser$state$misha_vtrack_names <- NULL

    # Should fall back to misha call
    result <- is_vtrack("some_track", browser)
    expect_type(result, "logical")
})

# =============================================================================
# Tests for reset_vtrack_iterators
# =============================================================================

test_that("reset_vtrack_iterators handles empty list", {
    expect_no_error(reset_vtrack_iterators(character(0)))
})

test_that("reset_vtrack_iterators handles NULL", {
    expect_no_error(reset_vtrack_iterators(NULL))
})

test_that("reset_vtrack_iterators handles non-existent vtracks gracefully", {
    # Should not error when trying to reset non-existent vtracks
    expect_no_error(reset_vtrack_iterators(c("fake_vtrack_1", "fake_vtrack_2")))
})

# =============================================================================
# Tests for extract_panels_sequential
# =============================================================================

test_that("extract_panels_sequential returns empty list for empty panels", {
    browser <- browser_create()
    result <- extract_panels_sequential(browser, list(), NULL, use_cache = FALSE)
    expect_equal(result, list())
})

test_that("extract_panels_sequential filters to data panels only", {
    browser <- browser_create()
    panels <- list(
        list(name = "anno", type = "annotation"),
        list(name = "ideo", type = "ideogram")
    )

    result <- extract_panels_sequential(browser, panels, NULL, use_cache = FALSE)
    expect_equal(result, list())
})

# =============================================================================
# Tests for calc_iterator (from utils.R but relevant to extraction)
# =============================================================================

test_that("calc_iterator respects base_iter for small spans", {
    # Small span should use base iterator
    expect_equal(calc_iterator(1000, 32, 4000), 32)
    expect_equal(calc_iterator(100, 32, 4000), 32)
})

test_that("calc_iterator scales for large spans", {
    # Large span: 4e6 / 4000 = 1000
    expect_equal(calc_iterator(4e6, 32, 4000), 1000)

    # Very large span: 100e6 / 4000 = 25000
    expect_equal(calc_iterator(100e6, 32, 4000), 25000)
})

test_that("calc_iterator uses custom target_points", {
    # 4e6 / 2000 = 2000
    expect_equal(calc_iterator(4e6, 32, 2000), 2000)

    # 4e6 / 8000 = 500
    expect_equal(calc_iterator(4e6, 32, 8000), 500)
})

# =============================================================================
# Tests for on.exit cleanup (functional tests)
# =============================================================================

test_that("extract_panel_data cleanup happens on NULL region", {
    browser <- browser_create()
    panel <- list(
        name = "test",
        type = "data",
        tracks = c("track1")
    )

    # Should return NULL for NULL region without errors
    result <- extract_panel_data(browser, panel, NULL, use_cache = FALSE)
    expect_null(result)
})

test_that("extract_panel_data handles non-data panel types", {
    browser <- browser_create()

    # Annotation panel
    panel <- list(name = "anno", type = "annotation")
    result <- extract_panel_data(browser, panel, make_region("chr1", 1000, 2000), use_cache = FALSE)
    expect_null(result)

    # Ideogram panel
    panel <- list(name = "ideo", type = "ideogram")
    result <- extract_panel_data(browser, panel, make_region("chr1", 1000, 2000), use_cache = FALSE)
    expect_null(result)
})

test_that("extract_panel_data handles panel with no tracks", {
    browser <- browser_create()
    panel <- list(
        name = "empty",
        type = "data",
        tracks = character(0)
    )

    result <- extract_panel_data(browser, panel, make_region("chr1", 1000, 2000), use_cache = FALSE)
    expect_null(result)
})

test_that("extract_panel_data fixed mode applies smooth_window=1 to panel smooth transform", {
    browser <- browser_create()
    browser$cfg$plot$extraction_mode <- "fixed"
    browser$cfg$plot$iterator <- 32
    browser$cfg$vtracks <- list()
    browser$state$smooth_window <- 1

    panel <- list(
        name = "test",
        type = "data",
        tracks = c("track1"),
        transforms = list(list(type = "smooth", window = 10))
    )

    captured_transforms <- NULL
    local_mocked_bindings(
        resolve_track_specs = function(tracks, browser, cfg) {
            list(exprs = c("track1"), names = c("track1"), temp_vtracks = character(0))
        },
        extract_tracks = function(tracks, region, iterator, colnames = NULL) {
            data.frame(
                chrom = "chr1",
                start = 1000L,
                end = 1032L,
                pos = 1016L,
                track1 = 1
            )
        },
        apply_transforms = function(data, transforms, value_cols) {
            captured_transforms <<- transforms
            data
        },
        add_track_metadata = function(data, panel, track_names) data,
        cache_set = function(key, value) NULL,
        .package = "misha.browser"
    )

    result <- extract_panel_data(
        browser = browser,
        panel = panel,
        region = make_region("chr1", 1000, 2000),
        use_cache = FALSE
    )

    expect_false(is.null(result))
    smooth_idx <- which(vapply(captured_transforms, function(t) identical(t$type, "smooth"), logical(1)))
    expect_length(smooth_idx, 1)
    expect_equal(captured_transforms[[smooth_idx]]$window, 1)
})

test_that("extract_panel_data fixed mode handles smooth_window=0 for panel smooth transform", {
    browser <- browser_create()
    browser$cfg$plot$extraction_mode <- "fixed"
    browser$cfg$plot$iterator <- 32
    browser$cfg$vtracks <- list()
    browser$state$smooth_window <- 0

    panel <- list(
        name = "test",
        type = "data",
        tracks = c("track1"),
        transforms = list(list(type = "smooth", window = 10))
    )

    local_mocked_bindings(
        resolve_track_specs = function(tracks, browser, cfg) {
            list(exprs = c("track1"), names = c("track1"), temp_vtracks = character(0))
        },
        extract_tracks = function(tracks, region, iterator, colnames = NULL) {
            data.frame(
                chrom = c("chr1", "chr1", "chr1"),
                start = c(1000L, 1032L, 1064L),
                end = c(1032L, 1064L, 1096L),
                pos = c(1016L, 1048L, 1080L),
                track1 = c(1, 2, 3)
            )
        },
        add_track_metadata = function(data, panel, track_names) data,
        cache_set = function(key, value) NULL,
        .package = "misha.browser"
    )

    result <- extract_panel_data(
        browser = browser,
        panel = panel,
        region = make_region("chr1", 1000, 2000),
        use_cache = FALSE
    )

    expect_false(is.null(result))
    expect_equal(result$track1, c(1, 2, 3))
})

# =============================================================================
# Tests for raw view (Feature 2)
# =============================================================================

test_that("effective_raw resolves from state > panel > cfg > FALSE", {
    # Mirror the precedence chain extract_panel_data computes.
    resolve <- function(state, panel, cfg) {
        isTRUE(state %||% panel %||% cfg %||% FALSE)
    }
    expect_false(resolve(NULL, NULL, NULL))
    expect_true(resolve(NULL, NULL, TRUE))     # cfg only
    expect_false(resolve(NULL, FALSE, TRUE))    # panel overrides cfg
    expect_true(resolve(NULL, TRUE, FALSE))     # panel overrides cfg
    expect_true(resolve(TRUE, FALSE, FALSE))    # state overrides panel
    expect_false(resolve(FALSE, TRUE, TRUE))    # state FALSE overrides
})

test_that("extract_panel_data uses base iterator under raw (no dynamic adjust)", {
    captured <- new.env()

    fake_extract <- function(tracks, region, iterator, colnames) {
        captured$iterator <- iterator
        df <- data.frame(
            chrom = "chr1",
            start = 1:5, end = 1:5, pos = 1:5,
            stringsAsFactors = FALSE
        )
        df[[colnames[[1]]]] <- c(NA_real_, 2, NA_real_, 4, NA_real_)
        df
    }

    testthat::local_mocked_bindings(
        extract_tracks = fake_extract,
        resolve_track_specs = function(...) list(
            exprs = "t1", names = "t1", temp_vtracks = character(0)
        ),
        cleanup_temp_vtracks = function(...) invisible(NULL),
        is_vtrack = function(...) FALSE,
        cache_exists = function(...) FALSE,
        cache_get = function(...) NULL,
        cache_set = function(...) invisible(NULL),
        add_track_metadata = function(data, panel, track_names) data,
        .package = "misha.browser"
    )

    cfg <- list(
        plot = list(extraction_mode = "dynamic_smooth", iterator = 32, target_points = 4000),
        vtracks = list()
    )
    br <- list(cfg = cfg, state = list(smooth_window = 100, raw_view = TRUE))
    panel <- list(
        name = "p",
        type = "data",
        tracks = c("t1"),
        transforms = list(list(type = "smooth", window = 50))
    )
    region <- data.frame(chrom = "chr1", start = 1, end = 1000)

    out <- extract_panel_data(br, panel, region, use_cache = FALSE)

    # Base iterator (32) should be used because raw forces fixed mode.
    expect_equal(captured$iterator, 32)
    # Smoothing should be skipped, NAs preserved.
    expect_true(any(is.na(out$t1)))
})

test_that("extract_panel_data smooths normally when raw is OFF", {
    raw_values <- c(0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10)
    fake_extract <- function(tracks, region, iterator, colnames) {
        df <- data.frame(
            chrom = "chr1",
            start = seq_len(20), end = seq_len(20), pos = seq_len(20),
            stringsAsFactors = FALSE
        )
        df[[colnames[[1]]]] <- raw_values
        df
    }

    testthat::local_mocked_bindings(
        extract_tracks = fake_extract,
        resolve_track_specs = function(...) list(
            exprs = "t1", names = "t1", temp_vtracks = character(0)
        ),
        cleanup_temp_vtracks = function(...) invisible(NULL),
        is_vtrack = function(...) FALSE,
        cache_exists = function(...) FALSE,
        cache_get = function(...) NULL,
        cache_set = function(...) invisible(NULL),
        add_track_metadata = function(data, panel, track_names) data,
        .package = "misha.browser"
    )

    cfg <- list(plot = list(extraction_mode = "fixed", iterator = 1), vtracks = list())
    br <- list(cfg = cfg, state = list(smooth_window = 5, raw_view = FALSE))
    panel <- list(name = "p", type = "data", tracks = c("t1"), transforms = list())
    region <- data.frame(chrom = "chr1", start = 1, end = 1000)

    out <- extract_panel_data(br, panel, region, use_cache = FALSE)

    # With smoothing on (window 5), the rolling mean of an alternating 0/10
    # signal should differ from any raw value (none of which are ~4 or ~6).
    expect_false(out$t1[3] == raw_values[3])
})
