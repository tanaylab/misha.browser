# Check if a track is a virtual track

Uses cached vtrack list from browser state for performance. Falls back
to misha::gvtrack.info() if browser not provided.

## Usage

``` r
is_vtrack(track, browser = NULL)
```

## Arguments

- track:

  Track name

- browser:

  Browser object (optional, for cached lookup)

## Value

TRUE if track is a virtual track
