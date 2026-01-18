# Add vertical lines to the browser

Add vertical lines to the browser

## Usage

``` r
browser_add_vlines(
  browser,
  name,
  source = "file",
  file = NULL,
  intervals = NULL,
  color = "grey50",
  linetype = "dashed",
  show_bounds = TRUE,
  enabled = TRUE
)
```

## Arguments

- browser:

  Browser object

- name:

  Vline set name

- source:

  Source type: "file", "intervals", "inline", "current"

- file:

  File path (for source = "file")

- intervals:

  Intervals name or list (for source = "intervals" or "inline")

- color:

  Line color

- linetype:

  Line type ("solid", "dashed", "dotted")

- show_bounds:

  Whether to show both start and end of intervals

- enabled:

  Whether vlines are enabled by default

## Value

Updated browser object
