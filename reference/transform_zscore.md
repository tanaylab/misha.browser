# Z-score normalization

Standardizes values to have mean 0 and standard deviation 1. If all
values are NA or standard deviation is zero, returns zeros.

## Usage

``` r
transform_zscore(x, params)
```

## Arguments

- x:

  Numeric vector

- params:

  List (unused)

## Value

Z-score normalized vector
