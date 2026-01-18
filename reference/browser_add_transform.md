# Add a transform to a panel

Add a transform to a panel

## Usage

``` r
browser_add_transform(browser, panel_name, type, ...)
```

## Arguments

- browser:

  Browser object

- panel_name:

  Name of panel to add transform to

- type:

  Transform type: "smooth", "log2", "log10", "sqrt", "zscore", "minmax",
  "clip", "expr"

- ...:

  Transform-specific parameters

## Value

Updated browser object
