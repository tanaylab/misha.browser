# Create a new genome browser

Creates a browser object from a configuration file or programmatically.

## Usage

``` r
browser_create(
  config = NULL,
  misha_root = NULL,
  title = "Genome Browser",
  profile = NULL
)
```

## Arguments

- config:

  Path to YAML configuration file, or NULL for programmatic setup

- misha_root:

  Path to misha database root (used if config is NULL)

- title:

  Browser title (used if config is NULL)

- profile:

  Profile name to use from configuration file

## Value

A browser object

## Examples

``` r
if (FALSE) { # \dontrun{
# From configuration file
browser <- browser_create(config = "my_browser.yaml")

# Programmatically
browser <- browser_create(misha_root = "/path/to/misha") %>%
    browser_add_panel(name = "signal", tracks = c("track1", "track2"))
} # }
```
