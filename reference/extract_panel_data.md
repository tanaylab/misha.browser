# Extract data for a panel

Extracts track data for a panel at the given region, applies transforms.

## Usage

``` r
extract_panel_data(browser, panel, region, use_cache = TRUE)
```

## Arguments

- browser:

  Browser object

- panel:

  Panel configuration

- region:

  Interval to extract (data frame with chrom, start, end)

- use_cache:

  Whether to use caching

## Value

Data frame with extracted and transformed data

## Details

Supports three extraction modes configured via `plot.extraction_mode`:

- "fixed" (default): Uses fixed iterator + rollmean smoothing. Best for
  vtracks with `func=sum` where values scale with bin size.

- "dynamic": Adjusts iterator based on view span (like misha.vis). The
  iterator itself provides smoothing via aggregation, so rollmean is
  skipped. More efficient for large regions but values may differ at
  different zoom levels.

- "dynamic_smooth": Adjusts iterator for resolution AND dynamically sets
  vtrack sshift/eshift for smoothing. Combines adaptive resolution with
  proper value scaling. Uses `smoothing_bp` config or state for window
  size.
