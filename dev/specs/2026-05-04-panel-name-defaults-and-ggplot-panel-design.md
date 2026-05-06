# Panel-name defaults & static ggplot panel — design

Two small follow-ups to the panel-name strip work in
`2026-05-03-panel-name-and-raw-view-design.md`:

1. The user feedback we were addressing was literally "the panel name
   doesn't show in the legend." We responded with a strip label, which
   is more visually prominent but less aligned with the literal
   complaint. Adjust:

   - Add the panel name as the **legend title**, addressing the
     original complaint directly.
   - Make the strip **opt-in** (default `show_name = FALSE`) so users
     pick the louder UCSC-style emphasis only when they want it.

2. Allow a panel to render an arbitrary user-supplied **static
   ggplot** object — useful for sample-metadata heatmaps, logos, and
   other annotations that don't depend on the genomic region.

## Feature 1 — defaults & legend title

### Behavior

- `browser_add_panel()` and `validate_panel()` change the default of
  `show_name` from `TRUE` to `FALSE`. Strip rendering only fires when
  the user explicitly sets `show_name = TRUE`.
- `apply_panel_theme()` adds the panel name to the legend title:
  `labs(y = y_title, color = panel$name)`. So a panel named `"signal"`
  shows "signal" above the track-name list in the existing legend.
- The strip styling (added in tasks 4–5 of the previous spec) is
  unchanged. It just stops being the default.

### Tradeoff acknowledged

A 4-panel browser will repeat the panel name 4 times along the bottom
(once per panel's legend). This is mild visual noise; the alternative
(strip only, no legend title) ignores the user's literal "in the
legend" complaint. We accept the noise because the literal complaint
takes priority.

If a panel has no `name` (defaulted to `panel_<index>`), the legend
title still gets that defaulted value. We could special-case empty /
defaulted names later if needed; out of scope here.

### Files touched

- `R/browser.R` — `browser_add_panel(show_name = FALSE)` (signature).
- `R/config.R` — `validate_panel()`: `panel$show_name <- panel$show_name %||% FALSE`.
- `R/panels.R` — `apply_panel_theme()`: `+ labs(y = y_title, color = panel$name)`.
- Tests in `tests/testthat/test-config.R` (default flipped) and
  `tests/testthat/test-browser.R` (no need to update — these test
  explicit values).

## Feature 2 — static ggplot panel

### Goal

Let users render an arbitrary ggplot object as a panel in the browser
layout. The plot is static: it does not re-render when the user
navigates regions, does not align with genomic x-axis, does not get
vline/highlight overlays. It just gets slotted into the patchwork
layout at the configured height and panel order.

### API

A new `type` value for `browser_add_panel()`:

```r
browser <- browser_create() %>%
    browser_add_panel(
        name = "metadata",
        type = "ggplot",
        plot = my_ggplot,   # any ggplot object
        height = 2
    ) %>%
    browser_add_panel(
        name = "signal",
        tracks = c("track1", "track2"),
        type = "data"
    )
```

`browser_add_panel()` acquires a new `plot = NULL` argument. When
`type = "ggplot"`:

- Skip the vtrack auto-creation block (it's keyed off `tracks`, which
  is `NULL` for ggplot panels — already short-circuits, but make this
  explicit by branching).
- Skip `validate_panel()`'s data-panel branch and fall into a new
  ggplot branch.

### Validation

`validate_panel()` adds:

```r
} else if (panel$type == "ggplot") {
    if (is.null(panel$plot) || !inherits(panel$plot, "ggplot")) {
        cli::cli_abort(
            "Panel '{panel$name}' has type='ggplot' but `plot` is missing or not a ggplot object."
        )
    }
    panel$height <- panel$height %||% .DEFAULT_DATA_PANEL_HEIGHT
}
```

No `._cache_signature` is computed for ggplot panels — they're not
extracted, and the cache key currently only matters for data panels.

### Render

New `render_ggplot_panel(panel, region)` in `R/panels.R`:

```r
render_ggplot_panel <- function(panel, region) {
    panel$plot
}
```

Yes, that's the whole function. The `region` argument is accepted for
signature consistency with the other render functions but unused.

Dispatch in `R/plot.R::render_panel()` (around line 149):

```r
    p <- switch(type,
        "data" = render_data_panel(...),
        "annotation" = render_annotation_panel(...),
        "ideogram" = render_ideogram_panel(...),
        "intervals" = render_intervals_panel(...),
        "ggplot" = render_ggplot_panel(panel, region),
        ...
    )
```

The post-render highlight-overlay block already at `R/plot.R:163-166`
needs to skip ggplot panels: `highlight_current` and friends only
apply when there's a genomic x-axis to draw on. Wrap that block in
`if (type != "ggplot")`.

### YAML interaction

ggplot objects are R values, not serializable to YAML.

- `browser_save_config()` (`R/config.R:517`) is modified to:
  1. Identify panels with `type == "ggplot"` in `cfg$panels`.
  2. Drop them from the saved config (filter `cfg$panels` to non-ggplot).
  3. Emit one `cli::cli_warn` listing the names that were dropped.

  The existing `clean_config_for_export()` already strips internal
  fields (anything starting with `.`); the ggplot drop happens *before*
  that, on the panel list itself.

  Rationale: drop+warn lets users save mixed configs without
  surprises. A hard error would make adding a single ggplot panel
  break their save workflow.

- YAML configs can't define ggplot panels, so loading from YAML is
  unaffected.

### Caveats documented in the help page

In the `browser_add_panel()` roxygen, document for `type = "ggplot"`:

- The plot is static and does not re-render on region navigation.
- The panel cannot be saved to YAML configs.
- vline / highlight overlays do not apply.

### Files touched

- `R/browser.R` — `plot = NULL` parameter; `@param plot` in roxygen.
- `R/config.R` — `validate_panel()` ggplot branch.
- `R/panels.R` — `render_ggplot_panel()`.
- `R/plot.R` — dispatch in `render_panel()`; skip highlight overlay on ggplot type.
- `R/config.R:517` — `browser_save_config()` drops+warns on ggplot panels.
- `man/browser_add_panel.Rd` — regenerated.
- `tests/testthat/test-config.R` — validation: missing/invalid `plot` errors; valid plot validates.
- `tests/testthat/test-panels.R` — render returns the same ggplot object.
- `tests/testthat/test-config.R` — `browser_save_config` drops ggplot panels and warns.

## Scope notes

- Region-aware ggplot panels (function `function(region) → ggplot`)
  are explicitly out of scope for this spec. If needed later, that
  becomes a separate feature with its own design — likely a different
  type (`"ggplot_fn"`) so the static and dynamic contracts stay
  distinct.
- This work goes on the same branch as the panel-name-and-raw-view PR
  (PR #1) since the two changes overlap heavily on `validate_panel`,
  `browser_add_panel`, and the panel-rendering pipeline.

## Out of scope

- Region-aware / function-based ggplot panels.
- Allowing other custom render objects (grobs, base graphics, etc.).
- Special-casing defaulted panel names ("panel_1", "panel_2") to
  suppress the legend title.
