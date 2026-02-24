# Run the interactive genome browser

Launches a Shiny web application for interactive genome browsing.

## Usage

``` r
browser_run(
  browser,
  port = .DEFAULT_PORT,
  host = .DEFAULT_HOST,
  launch.browser = TRUE
)
```

## Arguments

- browser:

  Browser object (created with browser_create)

- port:

  Port number to run on (default 8911)

- host:

  Host to bind to (default "0.0.0.0" for all interfaces)

- launch.browser:

  Whether to open browser automatically (default TRUE)

## Value

Shiny app object (runs interactively)

## Examples

``` r
if (FALSE) { # \dontrun{
browser <- browser_create(config = "my_browser.yaml")
browser_run(browser)

# Or specify port
browser_run(browser, port = 8080)
} # }
```
