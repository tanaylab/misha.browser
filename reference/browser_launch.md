# Quick launch browser from config file

Convenience function to create and run a browser in one step.

## Usage

``` r
browser_launch(
  config,
  port = 8911,
  host = "0.0.0.0",
  profile = NULL,
  profiling = FALSE,
  disk_cache = FALSE,
  cache_dir = NULL
)
```

## Arguments

- config:

  Path to YAML configuration file

- port:

  Port number

- host:

  Host to bind to

- profile:

  Profile name to use

- profiling:

  Enable profiling output (default: FALSE)

- disk_cache:

  Enable disk caching (default: FALSE)

- cache_dir:

  Directory for disk cache

## Value

Shiny app object

## Examples

``` r
if (FALSE) { # \dontrun{
browser_launch("my_browser.yaml")

# With profiling enabled
browser_launch("my_browser.yaml", profiling = TRUE)
} # }
```
