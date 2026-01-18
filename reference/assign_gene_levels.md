# Assign Y-levels to genes using sweep-line algorithm

O(n log n) algorithm that tracks the rightmost end position per level,
avoiding the O(n²) cost of checking all previous genes at each level.

## Usage

``` r
assign_gene_levels(genes)
```

## Arguments

- genes:

  Data frame with min_start, max_end columns

## Value

Integer vector of Y-levels
