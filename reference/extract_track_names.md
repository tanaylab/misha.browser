# Extract track names from a tracks list

Handles both string tracks and list-based track specs (with expr/name
fields). Only includes character tracks and list entries with explicit
'name' field.

## Usage

``` r
extract_track_names(tracks)
```

## Arguments

- tracks:

  List or vector of track specifications

## Value

Character vector of track names
