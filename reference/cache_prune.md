# Prune cache to stay within limits (LRU eviction)

Removes least-recently-used entries when cache exceeds size limits.

## Usage

``` r
cache_prune(
  max_entries = getOption("misha.browser.cache_max_entries", 100),
  max_bytes = getOption("misha.browser.cache_max_bytes", 5e+08)
)
```

## Arguments

- max_entries:

  Maximum number of entries to keep (default: 100)

- max_bytes:

  Maximum total bytes to keep (default: 500MB)

## Value

Invisibly returns number of entries pruned
