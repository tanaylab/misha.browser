# Add a virtual track to the browser

Adds a virtual track definition to the browser configuration. Supports
standard vtracks (with a source track and aggregation function) and
expression vtracks (computed from a track expression).

## Usage

``` r
browser_add_vtrack(
  browser,
  name,
  src = NULL,
  func = "avg",
  vtype = NULL,
  expr = NULL,
  expression = NULL,
  sshift = NULL,
  eshift = NULL,
  dim = NULL,
  ...
)
```

## Arguments

- browser:

  Browser object

- name:

  Name for the virtual track

- src:

  Source track name (for standard vtracks)

- func:

  Aggregation function (default "avg"). Common values: "avg", "sum",
  "min", "max"

- vtype:

  Vtrack type: "standard", "expr", "sequence", "intervals".
  Auto-detected if NULL.

- expr:

  Track expression string (for expression vtracks, e.g. "log2(1 +
  trackname)")

- expression:

  Optional expression wrapping the vtrack name (e.g. "pmax(vtrack_name,
  0)")

- sshift:

  Start shift in bp

- eshift:

  End shift in bp

- dim:

  Dimension for 2D tracks

- ...:

  Additional vtrack parameters

## Value

Updated browser object

## Examples

``` r
if (FALSE) { # \dontrun{
browser <- browser_create() %>%
    browser_add_vtrack("ctcf_log2", expr = "log2(1 + chipseq.ctcf)") %>%
    browser_add_vtrack("my_signal", src = "some.track", func = "avg") %>%
    browser_add_panel(name = "signal", tracks = c("ctcf_log2", "my_signal"))
} # }
```
