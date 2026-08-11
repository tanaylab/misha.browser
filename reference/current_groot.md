# Current misha genome database root

Used to namespace cache keys so that switching genomes (gsetroot) does
not serve stale data for identically named tracks / coordinates.

## Usage

``` r
current_groot()
```

## Value

Root path as a string, or "" if misha is not initialized
