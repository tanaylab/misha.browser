# Add track metadata based on grouping pattern

Uses data.table for efficient wide-to-long transformation and joins.

## Usage

``` r
add_track_metadata(data, panel, track_names = NULL)
```

## Arguments

- data:

  Data frame with track columns

- panel:

  Panel configuration

- track_names:

  Resolved track names (column names in data)

## Value

Data frame in long format with metadata columns
