# Tests for browser class

test_that("browser_create creates valid browser without config", {
    browser <- browser_create(misha_root = NULL, title = "Test")

    expect_s3_class(browser, "browser")
    expect_true(!is.null(browser$cfg))
    expect_true(!is.null(browser$state))
    expect_equal(browser$cfg$ui$title, "Test")
})

test_that("browser has required state fields", {
    browser <- browser_create()

    expect_true("current_region" %in% names(browser$state))
    expect_true("smooth_window" %in% names(browser$state))
    expect_true("vlines_enabled" %in% names(browser$state))
})

test_that("browser_add_panel adds panels", {
    browser <- browser_create() %>%
        browser_add_panel(
            name = "test_panel",
            tracks = c("track1", "track2"),
            plot_type = "line"
        )

    expect_length(browser$cfg$panels, 1)
    expect_equal(browser$cfg$panels[[1]]$name, "test_panel")
    expect_equal(browser$cfg$panels[[1]]$tracks, c("track1", "track2"))
})

test_that("browser_add_panel supports method chaining", {
    browser <- browser_create() %>%
        browser_add_panel(name = "panel1", tracks = "t1") %>%
        browser_add_panel(name = "panel2", tracks = "t2") %>%
        browser_add_panel(name = "panel3", tracks = "t3")

    expect_length(browser$cfg$panels, 3)
})

test_that("browser_add_panel does not create duplicate auto-vtracks for repeated tracks", {
    browser <- browser_create() %>%
        browser_add_panel(name = "signal", tracks = c("dup_track", "dup_track"))

    vtrack_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    expect_equal(sum(vtrack_names == "dup_track"), 1)
})

test_that("browser_add_panel supports inline track specs without auto-vtrack errors", {
    browser <- browser_create()
    expect_no_error(
        browser <- browser_add_panel(
            browser,
            name = "signal",
            tracks = list(list(expr = "track1 * 2", name = "double"))
        )
    )
    expect_length(browser$cfg$panels, 1)
})

test_that("browser_add_transform adds transforms to panel", {
    browser <- browser_create() %>%
        browser_add_panel(name = "signal", tracks = "track1") %>%
        browser_add_transform("signal", type = "log2", offset = 1) %>%
        browser_add_transform("signal", type = "smooth", window = 10)

    panel <- browser$cfg$panels[[1]]
    expect_length(panel$transforms, 2)
    expect_equal(panel$transforms[[1]]$type, "log2")
    expect_equal(panel$transforms[[2]]$type, "smooth")
})

test_that("browser_add_transform fails for non-existent panel", {
    browser <- browser_create()
    expect_error(browser_add_transform(browser, "nonexistent", type = "log2"))
})

test_that("browser_set_tracks updates tracks", {
    browser <- browser_create() %>%
        browser_add_panel(name = "signal", tracks = c("old1", "old2")) %>%
        browser_set_tracks("signal", c("new1", "new2", "new3"))

    expect_equal(browser$cfg$panels[[1]]$tracks, c("new1", "new2", "new3"))
})

test_that("browser_set_tracks does not create duplicate auto-vtracks for repeated tracks", {
    browser <- browser_create() %>%
        browser_add_panel(name = "signal", tracks = c("old1", "old2")) %>%
        browser_set_tracks("signal", c("dup_track", "dup_track", "other_track"))

    vtrack_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    expect_equal(sum(vtrack_names == "dup_track"), 1)
})

test_that("browser_set_tracks supports inline track specs without auto-vtrack errors", {
    browser <- browser_create() %>%
        browser_add_panel(name = "signal", tracks = "old1")

    expect_no_error(
        browser <- browser_set_tracks(
            browser,
            "signal",
            list(list(expr = "track1 * 2", name = "double"))
        )
    )
    expect_length(browser$cfg$panels[[1]]$tracks, 1)
})

test_that("browser_set_ylim updates ylim", {
    browser <- browser_create() %>%
        browser_add_panel(name = "signal", tracks = "track1") %>%
        browser_set_ylim("signal", c(0, 10))

    expect_equal(browser$cfg$panels[[1]]$ylim, c(0, 10))
})

test_that("browser_add_vlines adds vlines", {
    browser <- browser_create() %>%
        browser_add_vlines(
            name = "regions",
            source = "file",
            file = "test.csv",
            color = "red"
        )

    expect_length(browser$cfg$vlines, 1)
    expect_equal(browser$cfg$vlines[[1]]$name, "regions")
    expect_equal(browser$cfg$vlines[[1]]$color, "red")
    expect_true(browser$state$vlines_enabled[[1]])
})

test_that("browser_set_region updates region", {
    browser <- browser_create()
    browser <- browser_set_region(browser, "chr5:1000000-2000000")

    region <- browser$state$current_region
    expect_equal(region$chrom, "chr5")
    expect_equal(region$start, 1e6)
    expect_equal(region$end, 2e6)
})

test_that("browser_set_region accepts data frame", {
    browser <- browser_create()
    browser <- browser_set_region(browser, data.frame(
        chrom = "chr10",
        start = 5e6,
        end = 6e6
    ))

    region <- browser$state$current_region
    expect_equal(region$chrom, "chr10")
})

test_that("browser_get_region returns current region", {
    browser <- browser_create()
    browser <- browser_set_region(browser, "chr1:100-200")

    region <- browser_get_region(browser)
    expect_equal(region$chrom, "chr1")
    expect_equal(region$start, 100)
    expect_equal(region$end, 200)
})

test_that("print.browser works", {
    browser <- browser_create(title = "Print Test") %>%
        browser_add_panel(name = "p1", tracks = c("t1", "t2"))

    # Just check it doesn't error
    expect_no_error(print(browser))
})

# Tests for resolve_track_specs

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

test_that("cleanup_temp_vtracks handles empty list", {
    expect_no_error(cleanup_temp_vtracks(character(0)))
    expect_no_error(cleanup_temp_vtracks(NULL))
})

# --- browser_add_vtrack tests ---

test_that("browser_add_vtrack adds standard vtrack", {
    browser <- browser_create() %>%
        browser_add_vtrack("my_vt", src = "some.track", func = "avg")

    vt_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    expect_true("my_vt" %in% vt_names)

    vt <- browser$cfg$vtracks[[which(vt_names == "my_vt")]]
    expect_equal(vt$src, "some.track")
    expect_equal(vt$func, "avg")
})

test_that("browser_add_vtrack adds expression vtrack", {
    browser <- browser_create() %>%
        browser_add_vtrack("ctcf_log2", expr = "log2(1 + chipseq.ctcf)")

    vt_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    expect_true("ctcf_log2" %in% vt_names)

    vt <- browser$cfg$vtracks[[which(vt_names == "ctcf_log2")]]
    expect_equal(vt$vtype, "expr")
    expect_equal(vt$expr, "log2(1 + chipseq.ctcf)")
})

test_that("browser_add_vtrack supports pipe chaining with browser_add_panel", {
    browser <- browser_create() %>%
        browser_add_vtrack("vt1", expr = "log2(1 + track1)") %>%
        browser_add_vtrack("vt2", src = "track2", func = "sum") %>%
        browser_add_panel(name = "signal", tracks = c("vt1", "vt2"))

    vt_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    expect_true(all(c("vt1", "vt2") %in% vt_names))
    expect_length(browser$cfg$panels, 1)
})

test_that("browser_add_vtrack replaces duplicate vtrack names", {
    browser <- browser_create() %>%
        browser_add_vtrack("my_vt", src = "track1", func = "avg") %>%
        browser_add_vtrack("my_vt", src = "track2", func = "sum")

    vt_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    expect_equal(sum(vt_names == "my_vt"), 1)

    vt <- browser$cfg$vtracks[[which(vt_names == "my_vt")]]
    expect_equal(vt$src, "track2")
    expect_equal(vt$func, "sum")
})

test_that("browser_add_vtrack errors when neither src nor expr provided", {
    browser <- browser_create()
    expect_error(browser_add_vtrack(browser, "bad_vt"), "src.*expr")
})

test_that("browser_add_vtrack stores shift and dim parameters", {
    browser <- browser_create() %>%
        browser_add_vtrack("shifted", src = "track", func = "sum",
                          sshift = -500, eshift = 500)

    vt_names <- vapply(browser$cfg$vtracks, function(v) v$name, character(1))
    vt <- browser$cfg$vtracks[[which(vt_names == "shifted")]]
    expect_equal(vt$sshift, -500)
    expect_equal(vt$eshift, 500)
})
