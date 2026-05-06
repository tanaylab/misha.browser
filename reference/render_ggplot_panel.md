# Render a static ggplot panel

Returns the user-supplied ggplot object as-is. The plot is static, does
not depend on the genomic region, and does not get vline / highlight
overlays.

## Usage

``` r
render_ggplot_panel(panel, region)
```

## Arguments

- panel:

  Panel configuration (must have `plot` field with a ggplot)

- region:

  Viewing region (unused; accepted for signature consistency)

## Value

The ggplot object stored in `panel$plot`
