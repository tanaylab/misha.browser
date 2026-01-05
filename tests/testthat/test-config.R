# Tests for configuration functions

test_that("browser_create_config creates valid config", {
    cfg <- browser_create_config(misha_root = "/path/to/misha", title = "Test Browser")

    expect_type(cfg, "list")
    expect_equal(cfg$ui$title, "Test Browser")
    expect_true(length(cfg$panels) == 0)
    expect_true(length(cfg$vtracks) == 0)
    expect_true(!is.null(cfg$plot$iterator))
    expect_true(!is.null(cfg$ui$span_default))
})

test_that("validate_panel fills defaults", {
    panel <- list(name = "test", tracks = c("track1"))
    result <- validate_panel(panel, 1)

    expect_equal(result$name, "test")
    expect_equal(result$type, "data")
    expect_equal(result$plot_type, "line")
    expect_equal(result$height, 2)
    expect_true(result$show_legend)
    expect_true(is.list(result$transforms))
})

test_that("validate_panel handles annotation type", {
    panel <- list(name = "genes", type = "annotation")
    result <- validate_panel(panel, 1)

    expect_equal(result$type, "annotation")
    expect_equal(result$exon_source, "intervs.global.exon")
    expect_equal(result$gene_label_field, "geneSymbol")
})

test_that("validate_panel handles ideogram type", {
    panel <- list(name = "ideo", type = "ideogram")
    result <- validate_panel(panel, 1)

    expect_equal(result$type, "ideogram")
    expect_true(result$highlight_current)
    expect_equal(result$height, 0.3)
})

test_that("validate_vtrack requires name and track", {
    vt <- list(name = "test.vt", track = "real.track")
    result <- validate_vtrack(vt, 1)
    expect_equal(result$func, "sum")

    expect_error(validate_vtrack(list(track = "x"), 1))
    expect_error(validate_vtrack(list(name = "x"), 1))
})

test_that("validate_vtrack supports src as alias for track", {
    vt <- list(name = "test.vt", src = "real.track")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$src, "real.track")
    expect_null(result$track)
    expect_equal(result$vtype, "standard")
})

test_that("validate_vtrack supports flat sshift/eshift", {
    vt <- list(name = "test.vt", src = "real.track", sshift = -100, eshift = 100)
    result <- validate_vtrack(vt, 1)

    expect_equal(result$sshift, -100)
    expect_equal(result$eshift, 100)
})

test_that("validate_vtrack supports nested iterator format", {
    vt <- list(
        name = "test.vt",
        src = "real.track",
        iterator = list(sshift = -50, eshift = 50)
    )
    result <- validate_vtrack(vt, 1)

    expect_equal(result$sshift, -50)
    expect_equal(result$eshift, 50)
})

test_that("validate_vtrack supports function parameters", {
    # Single unnamed param (e.g., quantile percentile)
    vt <- list(name = "test.vt", src = "real.track", func = "quantile", params = 0.5)
    result <- validate_vtrack(vt, 1)

    expect_true(is.list(result$params))
    expect_equal(result$params[[1]], 0.5)

    # Named params
    vt2 <- list(
        name = "test.vt2",
        src = "real.track",
        func = "exists",
        params = list(vals = c(1, 2, 3))
    )
    result2 <- validate_vtrack(vt2, 1)
    expect_equal(result2$params$vals, c(1, 2, 3))
})

test_that("validate_vtrack supports expression vtracks", {
    vt <- list(name = "ratio", expr = "track1 / track2")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$vtype, "expr")
    expect_equal(result$expr, "track1 / track2")
})

test_that("validate_vtrack supports sequence-based vtracks", {
    # kmer.frac - no src needed
    vt <- list(name = "gc", func = "kmer.frac", params = list(kmer = "GC"))
    result <- validate_vtrack(vt, 1)

    expect_equal(result$vtype, "sequence")
    expect_null(result$src)
    expect_equal(result$func, "kmer.frac")

    # pwm.max - no src needed
    vt2 <- list(name = "motif", func = "pwm.max", params = list(bidirect = TRUE))
    result2 <- validate_vtrack(vt2, 1)
    expect_equal(result2$vtype, "sequence")

    # masked.frac - no src needed
    vt3 <- list(name = "masked", func = "masked.frac")
    result3 <- validate_vtrack(vt3, 1)
    expect_equal(result3$vtype, "sequence")
})

test_that("validate_vtrack errors on non-sequence func without src", {
    # sum requires a source
    vt <- list(name = "bad", func = "sum")
    expect_error(validate_vtrack(vt, 1), "missing 'src'")

    # avg requires a source
    vt2 <- list(name = "bad2", func = "avg")
    expect_error(validate_vtrack(vt2, 1), "missing 'src'")
})

test_that("validate_vtrack supports filter parameter", {
    vt <- list(name = "filtered", src = "track", filter = "mask_intervals")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$filter, "mask_intervals")
})

test_that("validate_vtrack expression defaults to name", {
    vt <- list(name = "my_vtrack", src = "track")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$expression, "my_vtrack")
})

test_that("validate_vtrack supports custom expression", {
    vt <- list(name = "norm.k27", src = "at.EB4_norm_300", expression = "pmax(norm.k27, 0)")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$name, "norm.k27")
    expect_equal(result$expression, "pmax(norm.k27, 0)")
})

test_that("validate_vline fills defaults", {
    vl <- list(name = "test_vlines")
    result <- validate_vline(vl, 1)

    expect_equal(result$source, "file")
    expect_equal(result$color, "grey50")
    expect_equal(result$linetype, "dashed")
    expect_true(result$show_bounds)
    expect_true(result$enabled)
})

test_that("validate_vline parses inline strings", {
    vl <- list(
        name = "inline_test",
        source = "inline",
        intervals = list(
            list(chrom = "chr1", start = 100, end = 200),
            "chr2:500-600"
        )
    )
    result <- validate_vline(vl, 1)

    expect_length(result$intervals, 2)
    expect_equal(result$intervals[[2]]$chrom, "chr2")
})

test_that("validate_config adds defaults to empty config", {
    cfg <- list()
    result <- validate_config(cfg)

    expect_true(is.list(result$panels))
    expect_true(is.list(result$vtracks))
    expect_true(is.list(result$vlines))
    expect_equal(result$plot$iterator, 32)
    expect_equal(result$ui$title, "Genome Browser")
})

test_that("get_panel and set_panel work correctly", {
    cfg <- browser_create_config()
    cfg$panels <- list(
        list(name = "panel1", type = "data"),
        list(name = "panel2", type = "annotation")
    )

    panel1 <- get_panel(cfg, "panel1")
    expect_equal(panel1$name, "panel1")

    panel1$height <- 5
    cfg <- set_panel(cfg, "panel1", panel1)
    expect_equal(get_panel(cfg, "panel1")$height, 5)

    # Non-existent panel
    expect_null(get_panel(cfg, "nonexistent"))
})
