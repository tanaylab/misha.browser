# Add a data panel to the browser

Add a data panel to the browser

## Usage

``` r
browser_add_panel(
  browser,
  name,
  tracks = NULL,
  grouping = NULL,
  facet_by = NULL,
  transforms = list(),
  plot_type = "line",
  colors = NULL,
  ylim = NULL,
  height = 2,
  ...
)
```

## Arguments

- browser:

  Browser object

- name:

  Panel name

- tracks:

  Character vector of track names

- grouping:

  List with color_by, pattern, overrides

- facet_by:

  Variable to facet by (from grouping pattern)

- transforms:

  List of transforms to apply

- plot_type:

  Plot type: "line", "area", "point", "histogram"

- colors:

  Named vector of colors

- ylim:

  Y-axis limits

- height:

  Relative height

- ...:

  Additional panel options

## Value

Updated browser object
