# Plot the genome browser

Renders all panels for the current or specified region.

## Usage

``` r
browser_plot(
  browser,
  region = NULL,
  gene = NULL,
  span = NULL,
  parallel = getOption("misha.browser.parallel", FALSE),
  profile = getOption("misha.browser.profile", FALSE)
)
```

## Arguments

- browser:

  Browser object

- region:

  Optional region to plot (uses current if NULL)

- gene:

  Optional gene name to center on

- span:

  Optional span in bp (used with gene)

- parallel:

  Logical, enable parallel panel data extraction (default: from option).
  Requires `future` package and a parallel plan to be set (e.g.,
  `future::plan(future::multisession)`).

- profile:

  Logical, enable profiling output (default: from option)

## Value

A patchwork plot object

## Examples

``` r
if (FALSE) { # \dontrun{
browser <- browser_create(config = "config.yaml")
browser_plot(browser)
browser_plot(browser, gene = "Tbx5", span = 2e6)

# Enable profiling
options(misha.browser.profile = TRUE)
browser_plot(browser)

# Enable parallel extraction
future::plan(future::multisession, workers = 4)
browser_plot(browser, parallel = TRUE)
} # }
```
