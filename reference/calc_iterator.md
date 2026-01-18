# Calculate dynamic iterator based on span

Ensures approximately target_points data points regardless of view span.

## Usage

``` r
calc_iterator(span, base_iter = 32, target_points = 4000)
```

## Arguments

- span:

  Viewing span in bp

- base_iter:

  Base iterator from config

- target_points:

  Target number of points (default 4000)

## Value

Calculated iterator value
