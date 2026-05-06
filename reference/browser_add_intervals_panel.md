# Add an intervals panel to the browser

Adds a panel that displays genomic intervals as rectangles (or arrows
when `show_direction = TRUE`). The intervals can come from a misha
intervals set, a file (BED/TSV), or an in-memory data frame.

## Usage

``` r
browser_add_intervals_panel(
  browser,
  name,
  intervals = NULL,
  file = NULL,
  color = "grey60",
  outline_color = "grey20",
  color_by = NULL,
  colors = NULL,
  label_field = NULL,
  show_labels = FALSE,
  show_direction = FALSE,
  direction_field = "strand",
  filter_field = NULL,
  filter_values = NULL,
  filter_regex = NULL,
  height = NULL,
  y_title = NULL,
  ...
)
```

## Arguments

- browser:

  Browser object

- name:

  Panel name

- intervals:

  Intervals source. Either a character string naming a misha intervals
  set, or a data frame with `chrom`, `start`, `end` columns. When a data
  frame is supplied, the panel is runtime-only and is not serialized to
  YAML by
  [`browser_save_config()`](https://tanaylab.github.io/misha.browser/reference/browser_save_config.md).

- file:

  Path to a BED/TSV file (used when `intervals` is not given).

- color:

  Default fill color for intervals.

- outline_color:

  Border color for rectangles.

- color_by:

  Column to map to fill color.

- colors:

  Named vector/list of colors keyed by `color_by` values.

- label_field:

  Column to use for text labels.

- show_labels:

  Whether to draw labels.

- show_direction:

  Draw intervals as arrows by strand.

- direction_field:

  Column name holding strand (default `"strand"`).

- filter_field:

  Column to filter on.

- filter_values:

  Values to keep (vector).

- filter_regex:

  Regex pattern to match.

- height:

  Relative panel height.

- y_title:

  Optional y-axis title.

- ...:

  Additional panel options.

## Value

Updated browser object

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
    chrom = "chr1",
    start = c(1e6, 1.2e6),
    end = c(1.05e6, 1.25e6),
    name = c("peak_a", "peak_b")
)
browser <- browser_create() %>%
    browser_add_intervals_panel(
        "peaks",
        intervals = df,
        label_field = "name",
        show_labels = TRUE
    )
} # }
```
