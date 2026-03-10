# Add a virtual track to the browser

Adds a virtual track definition to the browser configuration. Expression
handling is inferred from the provided fields: if `src`/`func` are
present, `expression` wraps the created vtrack during extraction;
otherwise, `expression` defines a pure expression vtrack.

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

  Vtrack type: "standard", "expr", "sequence", "intervals". Usually
  inferred automatically.

- expr:

  Deprecated alias for `expression`

- expression:

  Track expression. If `src`/`func` are provided, this wraps the created
  vtrack during extraction (e.g. `pmax(vtrack_name, 0)`). If no `src` or
  `func` are provided, it defines a pure expression vtrack (e.g.
  `log2(1 + trackname)`).

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
    browser_add_vtrack("ctcf_log2", expression = "log2(1 + chipseq.ctcf)") %>%
    browser_add_vtrack("my_signal", src = "some.track", func = "avg") %>%
    browser_add_panel(name = "signal", tracks = c("ctcf_log2", "my_signal"))
} # }
```
