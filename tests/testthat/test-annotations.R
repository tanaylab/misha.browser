# Tests for gene annotation functions (assign_gene_levels)

test_that("assign_gene_levels returns integer(0) for empty input", {
    genes <- data.frame(min_start = integer(), max_end = integer())
    result <- assign_gene_levels(genes)
    expect_equal(result, integer(0))
})

test_that("assign_gene_levels returns 1L for single gene", {
    genes <- data.frame(min_start = 1000, max_end = 2000)
    result <- assign_gene_levels(genes)
    expect_equal(result, 1L)
})

test_that("assign_gene_levels assigns same level to non-overlapping genes", {
    genes <- data.frame(
        min_start = c(100, 300, 500),
        max_end = c(200, 400, 600)
    )
    result <- assign_gene_levels(genes)

    expect_equal(result, c(1L, 1L, 1L))
})

test_that("assign_gene_levels assigns different levels to overlapping genes", {
    genes <- data.frame(
        min_start = c(100, 150, 180),
        max_end = c(250, 300, 350)
    )
    result <- assign_gene_levels(genes)

    # Each gene overlaps the previous, so all need different levels
    expect_length(unique(result), 3)
})

test_that("assign_gene_levels respects max_levels safety limit of 20", {
    # Create 25 fully overlapping genes
    n <- 25
    genes <- data.frame(
        min_start = rep(100, n),
        max_end = rep(200, n)
    )
    result <- assign_gene_levels(genes)

    expect_true(max(result) <= 20L)
    expect_length(result, n)
})

test_that("assign_gene_levels handles unsorted genes", {
    # Genes out of order by min_start
    genes <- data.frame(
        min_start = c(500, 100, 300),
        max_end = c(600, 200, 400)
    )
    result <- assign_gene_levels(genes)

    # None overlap after sorting, so all level 1
    expect_equal(result, c(1L, 1L, 1L))
})

test_that("assign_gene_levels reuses levels when possible", {
    # Gene 1 and 2 overlap; gene 3 only overlaps gene 2
    genes <- data.frame(
        min_start = c(100, 150, 250),
        max_end = c(200, 300, 350)
    )
    result <- assign_gene_levels(genes)

    # 1 and 2 must differ; 3 can reuse level 1
    expect_true(result[1] != result[2])
    expect_equal(result[3], result[1])
})
