# Sanitize an interval

Ensures interval has valid chrom, start, end and that start \< end.

## Usage

``` r
sanitize_interval(interval)
```

## Arguments

- interval:

  Data frame with chrom, start, end

## Value

Sanitized interval or NULL if invalid
