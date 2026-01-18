# Extract data for multiple panels in parallel

Uses the `future` package to extract data for multiple panels
concurrently. Requires `future` and `promises` packages to be installed
and a parallel plan to be set (e.g.,
`future::plan(future::multisession)`).

## Usage

``` r
extract_panels_parallel(browser, panels, region, use_cache = TRUE)
```

## Arguments

- browser:

  Browser object

- panels:

  List of panel configurations

- region:

  Viewing region

- use_cache:

  Whether to use caching

## Value

Named list of extracted data frames (keyed by panel name)

## Details

Note: This only parallelizes data extraction. ggplot rendering must
remain sequential as ggplot is not thread-safe.
