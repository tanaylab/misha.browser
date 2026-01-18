# Generate a consistent color for a string

Uses deterministic hash-based color generation without modifying global
random state (avoids set.seed side effects).

## Usage

``` r
generate_color(name, default = "grey50")
```

## Arguments

- name:

  String to generate color for

- default:

  Default color if generation fails

## Value

Color string
