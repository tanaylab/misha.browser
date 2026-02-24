# Tests for ideogram panel functions

# =============================================================================
# Tests for cytoband_colors
# =============================================================================

test_that("cytoband_colors returns a named character vector", {
    result <- cytoband_colors()

    expect_type(result, "character")
    expect_true(!is.null(names(result)))
    expect_true(length(result) > 0)
})

test_that("cytoband_colors contains expected stain names", {
    result <- cytoband_colors()
    expected_stains <- c("gneg", "gpos25", "gpos50", "gpos75", "gpos100", "acen", "gvar", "stalk")

    for (stain in expected_stains) {
        expect_true(stain %in% names(result), info = paste("Missing stain:", stain))
    }
})

test_that("cytoband_colors maps gneg to white and gpos100 to black", {
    result <- cytoband_colors()

    expect_equal(result[["gneg"]], "white")
    expect_equal(result[["gpos100"]], "black")
})

test_that("cytoband_colors maps acen to red", {
    result <- cytoband_colors()

    expect_equal(result[["acen"]], "red")
})
