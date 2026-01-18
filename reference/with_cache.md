# Execute with caching

Returns cached value if exists, otherwise computes and caches.

## Usage

``` r
with_cache(key, compute_fn)
```

## Arguments

- key:

  Cache key

- compute_fn:

  Function to compute value if not cached

## Value

Cached or computed value
