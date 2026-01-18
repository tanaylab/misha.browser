# Apply a transformation pipeline to data

Uses vectorized matrix operations where possible for performance.
Transforms that can be vectorized: log2, log10, sqrt, clip Transforms
that need per-column processing: smooth, zscore, minmax, quantile, expr

## Usage

``` r
apply_transforms(data, transforms, value_cols)
```

## Arguments

- data:

  Data frame with columns: chrom, start, end, pos, and value columns

- transforms:

  List of transform specifications

- value_cols:

  Column names containing values to transform

## Value

Transformed data frame
