# Assign Y-levels to intervals using sweep-line algorithm

O(n log n) algorithm that tracks the rightmost end position per level,
avoiding the O(n²) cost of checking all previous intervals at each
level.

## Usage

``` r
assign_interval_levels(intervals)
```

## Arguments

- intervals:

  Data frame with start/end

## Value

Integer vector of Y-levels
