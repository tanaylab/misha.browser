# Calculate dynamic iterator based on span

Ensures approximately target_points data points regardless of view span.

## Usage

``` r
calc_iterator(
  span,
  base_iter = .DEFAULT_ITERATOR,
  target_points = .DEFAULT_TARGET_POINTS
)
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
