# Parse a coordinate string

Parses strings like "chr1:1000-2000" or "chr1 1000 2000" into a data
frame.

## Usage

``` r
parse_coords(text)
```

## Arguments

- text:

  Coordinate string

## Value

Data frame with chrom, start, end columns, or NULL if parsing fails
