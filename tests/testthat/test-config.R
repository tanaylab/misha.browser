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
    vt <- list(name = "ratio", expression = "track1 / track2")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$vtype, "expr")
    expect_equal(result$expression, "track1 / track2")
    expect_false("expr" %in% names(result))
})

test_that("validate_vtrack normalizes legacy expr field", {
    vt <- list(name = "ratio", expr = "track1 / track2")
    result <- validate_vtrack(vt, 1)

    expect_equal(result$vtype, "expr")
    expect_equal(result$expression, "track1 / track2")
    expect_false("expr" %in% names(result))
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

# =============================================================================
# Tests for validate_hline (Issue #20: hline validation)
# =============================================================================

test_that("validate_hline requires y or stat", {
    expect_error(
        validate_hline(list(), 1, "test_panel"),
        "must have either 'y' or 'stat'"
    )
})

test_that("validate_hline rejects both y and stat", {
    expect_error(
        validate_hline(list(y = 5, stat = "mean"), 1, "test_panel"),
        "cannot have both"
    )
})

test_that("validate_hline validates stat types", {
    # Valid stats
    expect_no_error(validate_hline(list(stat = "mean"), 1))
    expect_no_error(validate_hline(list(stat = "median"), 1))
    expect_no_error(validate_hline(list(stat = "quantile", q = 0.5), 1))

    # Invalid stat
    expect_error(
        validate_hline(list(stat = "invalid_stat"), 1, "test"),
        "stat must be one of"
    )
})

test_that("validate_hline requires q for quantile stat", {
    expect_error(
        validate_hline(list(stat = "quantile"), 1, "test"),
        "requires 'q' parameter"
    )
})

test_that("validate_hline validates q range", {
    # Valid q
    expect_no_error(validate_hline(list(stat = "quantile", q = 0.5), 1))
    expect_no_error(validate_hline(list(stat = "quantile", q = 0), 1))
    expect_no_error(validate_hline(list(stat = "quantile", q = 1), 1))

    # Invalid q
    expect_error(
        validate_hline(list(stat = "quantile", q = -0.1), 1, "test"),
        "must be between 0 and 1"
    )
    expect_error(
        validate_hline(list(stat = "quantile", q = 1.5), 1, "test"),
        "must be between 0 and 1"
    )
    expect_error(
        validate_hline(list(stat = "quantile", q = "not_numeric"), 1, "test"),
        "must be between 0 and 1"
    )
})

test_that("validate_hline requires numeric y", {
    expect_error(
        validate_hline(list(y = "not_a_number"), 1, "test"),
        "y must be numeric"
    )
})

test_that("validate_hline fills in defaults", {
    result <- validate_hline(list(y = 0), 1)

    expect_equal(result$y, 0)
    expect_equal(result$color, "grey50")
    expect_equal(result$linetype, "dashed")
    expect_equal(result$linewidth, 0.5)
})

test_that("validate_hline preserves custom styling", {
    result <- validate_hline(list(
        y = 5,
        color = "red",
        linetype = "solid",
        linewidth = 2
    ), 1)

    expect_equal(result$color, "red")
    expect_equal(result$linetype, "solid")
    expect_equal(result$linewidth, 2)
})

test_that("validate_hline detects YAML y: boolean parse error", {
    # YAML parses bare 'y:' as boolean TRUE
    hline <- list(`TRUE` = 5) # Simulates 'y: 5' parsed as TRUE: 5

    expect_error(
        validate_hline(hline, 1, "test_panel"),
        "YAML parsed"
    )
})

# =============================================================================
# Tests for panel with hlines
# =============================================================================

test_that("validate_panel validates hlines", {
    panel <- list(
        name = "test",
        tracks = c("track1"),
        hlines = list(
            list(y = 0),
            list(stat = "mean", color = "blue")
        )
    )
    result <- validate_panel(panel, 1)

    expect_length(result$hlines, 2)
    expect_equal(result$hlines[[1]]$y, 0)
    expect_equal(result$hlines[[2]]$stat, "mean")
})

test_that("validate_panel handles empty hlines", {
    panel <- list(name = "test", tracks = c("track1"), hlines = list())
    result <- validate_panel(panel, 1)
    expect_length(result$hlines, 0)
})

test_that("validate_panel handles NULL hlines", {
    panel <- list(name = "test", tracks = c("track1"), hlines = NULL)
    result <- validate_panel(panel, 1)
    expect_length(result$hlines, 0)
})

# =============================================================================
# Tests for intervals panel type
# =============================================================================

test_that("validate_panel handles intervals type", {
    panel <- list(name = "peaks", type = "intervals")
    result <- validate_panel(panel, 1)

    expect_equal(result$type, "intervals")
    expect_equal(result$source, "intervals")
    expect_equal(result$color, "grey60")
    expect_equal(result$outline_color, "grey20")
    expect_equal(result$height, 1)
    expect_false(result$show_labels)
})

test_that("validate_panel preserves intervals panel options", {
    panel <- list(
        name = "peaks",
        type = "intervals",
        source = "file",
        file = "peaks.bed",
        color = "red",
        show_labels = TRUE,
        label_field = "name"
    )
    result <- validate_panel(panel, 1)

    expect_equal(result$source, "file")
    expect_equal(result$file, "peaks.bed")
    expect_equal(result$color, "red")
    expect_true(result$show_labels)
    expect_equal(result$label_field, "name")
})

# =============================================================================
# Tests for ylim normalization
# =============================================================================

test_that("validate_panel normalizes ylim from list to vector", {
    # YAML often parses [0, 10] as a list
    panel <- list(name = "test", tracks = "track1", ylim = list(0, 10))
    result <- validate_panel(panel, 1)

    expect_type(result$ylim, "double")
    expect_equal(result$ylim, c(0, 10))
})

test_that("validate_panel preserves ylim vector", {
    panel <- list(name = "test", tracks = "track1", ylim = c(-5, 5))
    result <- validate_panel(panel, 1)

    expect_equal(result$ylim, c(-5, 5))
})

# =============================================================================
# Tests for grouping defaults
# =============================================================================

test_that("validate_panel fills grouping defaults", {
    panel <- list(
        name = "test",
        tracks = c("source1.mark1", "source2.mark2"),
        grouping = list(color_by = "mark")
    )
    result <- validate_panel(panel, 1)

    expect_equal(result$grouping$color_by, "mark")
    expect_true(!is.null(result$grouping$pattern))
})

# =============================================================================
# Tests for clean_config_for_export
# =============================================================================

test_that("clean_config_for_export removes top-level internal fields", {
    cfg <- list(
        ui = list(title = "Test"),
        `.misha_root` = "/path/to/misha",
        `.config_file` = "/path/to/config.yaml"
    )

    result <- clean_config_for_export(cfg)

    expect_null(result[[".misha_root"]])
    expect_null(result[[".config_file"]])
    expect_equal(result$ui$title, "Test")
})

test_that("clean_config_for_export removes internal fields in named sublists", {
    cfg <- list(
        ui = list(
            title = "Test",
            `.internal` = "hidden"
        ),
        plot = list(
            iterator = 32,
            `.resolved` = TRUE
        )
    )

    result <- clean_config_for_export(cfg)

    expect_null(result$ui[[".internal"]])
    expect_null(result$plot[[".resolved"]])
    expect_equal(result$ui$title, "Test")
    expect_equal(result$plot$iterator, 32)
})

test_that("clean_config_for_export preserves data in named lists", {
    cfg <- list(
        panels = list(
            list(name = "panel1", type = "data"),
            list(name = "panel2", type = "annotation")
        )
    )

    result <- clean_config_for_export(cfg)

    expect_length(result$panels, 2)
    expect_equal(result$panels[[1]]$name, "panel1")
    expect_equal(result$panels[[2]]$type, "annotation")
})

# =============================================================================
# Tests for resolve_profile
# =============================================================================

test_that("resolve_profile auto-detects server profile", {
    cfg <- list(
        profiles = list(
            local = list(misha_root = "/nonexistent/local"),
            server = list(misha_root = tempdir()) # tempdir exists
        )
    )

    result <- resolve_profile(cfg, NULL, ".")

    expect_equal(result$._profile, "server")
})

test_that("resolve_profile falls back to local", {
    cfg <- list(
        profiles = list(
            local = list(misha_root = "/nonexistent/local"),
            server = list(misha_root = "/nonexistent/server")
        )
    )

    result <- resolve_profile(cfg, NULL, ".")

    expect_equal(result$._profile, "local")
})

test_that("resolve_profile respects explicit profile", {
    cfg <- list(
        profiles = list(
            local = list(misha_root = "/local"),
            server = list(misha_root = tempdir())
        )
    )

    result <- resolve_profile(cfg, "local", ".")

    expect_equal(result$._profile, "local")
    expect_equal(result$._misha_root, "/local")
})

test_that("resolve_profile handles no profiles", {
    cfg <- list(profiles = list())
    result <- resolve_profile(cfg, NULL, ".")

    expect_null(result$._profile)
})

test_that("resolve_profile resolves file paths in vlines", {
    temp_dir <- tempdir()
    cfg <- list(
        profiles = list(
            local = list(
                misha_root = temp_dir,
                data_dir = "data"
            )
        ),
        vlines = list(
            list(name = "test", file = "vlines.bed")
        )
    )

    result <- resolve_profile(cfg, "local", temp_dir)

    expect_true(!is.null(result$vlines[[1]]$._resolved_file))
    expect_true(grepl("data.*vlines.bed", result$vlines[[1]]$._resolved_file))
})

# =============================================================================
# Tests for panel name generation
# =============================================================================

test_that("validate_panel generates name if missing", {
    panel <- list(tracks = c("track1"))
    result <- validate_panel(panel, 5)

    expect_equal(result$name, "panel_5")
})

# =============================================================================
# Tests for vline name generation
# =============================================================================

test_that("validate_vline generates name if missing", {
    vl <- list(source = "file", file = "test.bed")
    result <- validate_vline(vl, 3)

    expect_equal(result$name, "vline_3")
})

# =============================================================================
# Tests for panel with no tracks warning
# =============================================================================

test_that("validate_panel warns on data panel with no tracks", {
    panel <- list(name = "empty", type = "data")

    expect_warning(
        result <- validate_panel(panel, 1),
        "no tracks defined"
    )

    expect_equal(result$tracks, character(0))
})

# =============================================================================
# Tests for set_panel append behavior
# =============================================================================

test_that("set_panel appends new panel if not found", {
    cfg <- browser_create_config()
    cfg$panels <- list(list(name = "existing", type = "data"))

    new_panel <- list(name = "new_panel", type = "annotation")
    cfg <- set_panel(cfg, "new_panel", new_panel)

    expect_length(cfg$panels, 2)
    expect_equal(cfg$panels[[2]]$name, "new_panel")
})

# =============================================================================
# Tests for cache signature pre-computation
# =============================================================================

test_that("validate_panel pre-computes cache signature for data panels", {
    panel <- list(
        name = "test",
        type = "data",
        tracks = c("track1"),
        transforms = list(list(type = "log2"))
    )

    result <- validate_panel(panel, 1)

    expect_true(!is.null(result$._cache_signature))
    expect_type(result$._cache_signature, "character")
})

test_that("validate_panel does not compute signature for non-data panels", {
    panel <- list(name = "anno", type = "annotation")
    result <- validate_panel(panel, 1)

    expect_null(result$._cache_signature)
})

test_that("validate_vtrack pre-computes transform signature", {
    vt <- list(
        name = "test_vt",
        src = "track",
        transforms = list(list(type = "zscore"))
    )

    result <- validate_vtrack(vt, 1)

    expect_true(!is.null(result$._transform_signature))
})

test_that("validate_vtrack omits signature without transforms", {
    vt <- list(name = "test_vt", src = "track")

    result <- validate_vtrack(vt, 1)

    expect_null(result$._transform_signature)
})
