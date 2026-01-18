# Configure disk caching

Enable or configure disk-based caching for the browser.

## Usage

``` r
browser_cache_config(dir = NULL, enabled = TRUE)
```

## Arguments

- dir:

  Directory for disk cache. NULL to use temp directory.

- enabled:

  Whether disk caching is enabled (default: TRUE when dir is set)

## Examples

``` r
if (FALSE) { # \dontrun{
# Enable disk caching with custom directory
browser_cache_config(dir = "~/.misha_browser_cache")

# Disable disk caching
browser_cache_config(enabled = FALSE)
} # }
```
