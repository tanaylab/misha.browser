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
  show_name = FALSE,
  raw = NULL,
  type = "data",
  plot = NULL,
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

  Plot type: "line", "area", "point", "histogram", "segment"

- colors:

  Named vector of colors

- ylim:

  Y-axis limits

- height:

  Relative height

- show_name:

  Logical, render the panel name as a bold left-side strip label
  (default FALSE)

- raw:

  Logical or NULL. If TRUE, render without smoothing and with NAs as
  gaps. NULL inherits from cfg\$plot\$raw.

- type:

  Panel type. One of "data" (default), "annotation", "intervals",
  "ideogram", or "ggplot".

- plot:

  A ggplot object. Only used when `type = "ggplot"`. The plot is
  rendered as-is (static, region-independent, no vline/highlight
  overlays). Cannot be saved to YAML.

- ...:

  Additional panel options (e.g. `iterator` for per-panel iterator
  override)

## Value

Updated browser object
