# Compute a genome-wide quantile for a track

Extracts the specified track across the full genome, applies the panel's
transforms, and computes the requested quantile. Results are cached so
the expensive extraction only happens once per track/transform/quantile
combination.

## Usage

``` r
compute_global_quantile(browser, panel, hline)
```

## Arguments

- browser:

  Browser object

- panel:

  Panel configuration (used for transforms)

- hline:

  Hline configuration with `q` and optional `track` fields

## Value

Numeric quantile value, or NA on failure
