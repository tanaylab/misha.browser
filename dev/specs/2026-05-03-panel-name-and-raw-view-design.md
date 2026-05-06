# Panel name in legend, and raw view — design

Two small UX features driven by user feedback:

1. The panel `name` argument to `browser_add_panel()` is not visible
   anywhere in the rendered plot. It should appear as a left-side
   strip-style label, in addition to the existing `y_title`.
2. There is no quick way to inspect data in raw form (no smoothing, NAs
   visible as gaps). Smoothing is desirable as a default but obscures
   sparse data.

A third item — UCSC-style barplot — was deferred and is not part of
this design.

## Feature 1 — panel name as left-side strip label

### Goal

Render the panel's `name` as a left-side strip label so users see the
identifier they assigned. `y_title` keeps its current meaning (units /
quantity on the y axis) and continues to render at its current
position.

### Behavior

- New panel option `show_name` (default `TRUE`). When `FALSE`, no strip
  is rendered.
- Annotation, ideogram, and intervals panels are unaffected — the strip
  is only added to data panels in `render_data_panel()`.
- The strip text reads horizontally (not rotated 90°), styled with a
  light gray background and bold text, matching the genome-browser
  convention.

### Implementation approach

In `R/panels.R::render_data_panel()`:

1. After data extraction, inject `data$._panel_name <- panel$name`.
2. Choose a facet expression based on whether `facet_by` is set:
   - No `facet_by` (the common case):
     `facet_grid(._panel_name ~ ., switch = "y", scales = "free_y")`.
     Single row, single strip on the left, between the y axis and the
     plot panel.
   - With `facet_by`:
     `facet_grid(<facet_by> ~ ._panel_name, switch = "y", scales = "free_y")`.
     The `facet_by` strips appear on the left as before; the panel name
     appears as a single column strip across the top. The minor change
     vs. today is the strip-position switch on the `facet_by` axis,
     which moves those labels to the left side instead of the right.
     This is acceptable because it matches the rest of the panel's new
     layout and is more compact.
3. Style strips in `apply_panel_theme()`:
   - `strip.background.y = element_rect(fill = "grey90", color = NA)`
   - `strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1)`
   - `strip.placement = "outside"` so the y axis title remains to the
     left of the strip.
4. `show_name = FALSE` skips the synthetic facet entirely.

### Why this approach

- `facet_grid` already handles strip styling, alignment with the panel
  area, and composition with other facets. No layout code needed.
- No new package dependency. `ggh4x::facet_nested` would be slightly
  cleaner for the `facet_by + name` case but is not worth a new
  dependency for this.
- `patchwork`-based side composition was rejected: it would require
  restructuring `browser_plot()`'s `wrap_plots(plots, ncol = 1, guides = "collect")`
  to handle each panel as a 2-column sub-patchwork, with high risk of
  breaking guide collection and per-panel theming.
- `labs(tag = ...)` was rejected: tags float in the plot margin and do
  not align with the panel area when patchwork stacks panels of
  different y-axis-label widths.

### API and config surface

- New panel argument: `show_name` in `browser_add_panel()`, default
  `TRUE`.
- New YAML field: `panels[].show_name`, default `TRUE`.
- `validate_panel()` must accept and pass through `show_name`.
- No change to `name` semantics elsewhere (cache keys, panel
  identification).

## Feature 2 — raw view

### Goal

Provide a "raw" rendering mode that displays the data as extracted,
with no smoothing layered on top, and with NAs surfaced as gaps. This
is for inspecting sparse data where smoothing is misleading.

### Behavior

When raw mode is active for a panel:

1. `extraction_mode` is forced to `"fixed"` for that panel render. The
   `dynamic` and `dynamic_smooth` modes use the iterator and vtrack
   `sshift`/`eshift` to smooth, which we want to skip.
2. No rollmean smoothing is applied. The UI smooth slider is ignored
   for that panel; user-defined `smooth` transforms in the panel
   config and per-vtrack `smooth` transforms are stripped from the
   transform list for that render.
3. NAs from misha extraction flow through unmodified.
   `geom_line(na.rm = TRUE)` and friends already render NAs as gaps;
   the only thing that currently fills them is `rollmean(fill = "extend")`,
   which is skipped under raw.
4. The configured base iterator (panel/global, e.g., 32 bp) is still
   honored. Going to per-bp resolution is out of scope: too expensive
   for normal use and not what the user asked for. Documented as
   "raw with respect to smoothing, not bin size".

### Where the toggle lives

Three coordinated entry points (option C):

- **Per-panel argument**: `browser_add_panel(..., raw = NULL)`,
  stored as `panel$raw`. Default is `NULL` (not `FALSE`) so that an
  unset panel inherits from the global config. `TRUE`/`FALSE` set
  explicitly override inheritance.
- **Global config**: `cfg$plot$raw` (boolean, default `FALSE`),
  inherited by panels that don't set their own.
- **Shiny UI**: a checkbox next to the smooth slider, bound to
  `br$state$raw_view`. Acts as a session-wide override.

### Precedence

```
effective_raw = state$raw_view %||% panel$raw %||% cfg$plot$raw %||% FALSE
```

UI toggle wins (one click switches everything). When unchecked, each
panel honors its own `panel$raw` if set, then the global default.

`validate_panel()` must **not** apply a `%||% FALSE` default to
`panel$raw` — leaving it `NULL` is what allows inheritance through
`%||%`. Same pattern as `panel$smooth_window` today
(`R/extract.R:107`).

The smooth-slider's UI state takes effect only when raw is off; under
raw, smoothing is skipped regardless of slider value. Document this
in the UI tooltip.

### Implementation approach

All changes in `R/extract.R::extract_panel_data()` (lines ~39–209):

1. Compute `effective_raw` once at the top of the function from the
   precedence chain above.
2. If `effective_raw`:
   - Force `extraction_mode <- "fixed"`.
   - Skip the dynamic iterator adjustment branch and the `dynamic_smooth`
     branch that sets vtrack `sshift`/`eshift`.
   - Set `smooth_window <- NULL` (skips the "add smooth transform"
     branch in `:180-189`).
   - After computing `transforms` and per-vtrack `vt_transforms`,
     filter out any element with `type == "smooth"`. The dynamic-mode
     branches in `:179` and `:203` already do exactly this — under
     raw we apply the same filter regardless of mode.
3. Cache key handling: a non-raw panel with `smooth_window = NULL`
   and a raw panel render both look like "no smoothing" to
   `extraction_mode`/`smoothing_key`, but raw additionally strips
   user-defined `smooth` transforms from the panel's transform list,
   producing different output. Add `effective_raw` to the
   `cache_key()` call so the two render paths cache separately. Same
   place: `R/extract.R:137-145`.

In `R/shiny-ui.R` (next to the smooth slider input near line 161):

- Add a `checkboxInput("raw_view", ...)` with `value = FALSE` (or
  `cfg$plot$raw` if set).

In `R/shiny-server.R` (next to the smooth observer near line 180):

- `observeEvent(input$raw_view, br$state$raw_view <- input$raw_view)`.
- The plot reactive already invalidates on state changes; verify the
  raw checkbox triggers a re-render.

### API and config surface

- New panel argument: `raw` in `browser_add_panel()`, default `NULL`
  (inherit). Documented values: `TRUE`, `FALSE`, `NULL`.
- New YAML fields: `panels[].raw` (optional, no default applied) and
  `plot.raw` (default `FALSE` at the global level).
- `validate_panel()` must accept and pass `raw` through unchanged
  (no `%||% FALSE` default — inheritance depends on `NULL`).
- `panel$raw` should be added to `panel$._cache_signature` in
  `validate_panel()` so that toggling per-panel raw invalidates that
  panel's cache. (UI override and global override change at runtime
  and are already captured by `extraction_mode`/`smoothing_key` in the
  cache key.)

### Edge cases

- A panel with a user-defined `smooth` transform (independent of the
  UI slider) gets that transform stripped under raw. This is
  documented as the intent of "raw".
- Per-vtrack `smooth` transforms in YAML (`vtracks[].transforms`) are
  similarly stripped under raw, mirroring the panel-level behavior.
- Non-data panels (annotation, ideogram, intervals) are unaffected
  because `extract_panel_data()` returns early for them.
- Caching: the existing key composition already separates raw from
  smoothed renders. Tested behavior: toggling the UI checkbox should
  not return a stale smoothed plot from cache.

## Files touched

- `R/browser.R` — add `raw` parameter to `browser_add_panel()`; pass
  through to panel object. (`name` already exists.)
- `R/panels.R` — synthetic facet, strip styling, `show_name`
  handling.
- `R/config.R` — `validate_panel()` accepts `show_name` and `raw`.
- `R/extract.R` — `effective_raw` short-circuit logic.
- `R/shiny-ui.R` — `raw_view` checkbox.
- `R/shiny-server.R` — observer for `raw_view`.
- `man/*.Rd` — regenerated by roxygen for any function with new
  arguments.
- `tests/testthat/test-*.R` — new tests for `raw` precedence and
  `show_name` rendering (presence of strip, suppression when
  `show_name = FALSE`).

## Out of scope

- UCSC-style barplot (deferred pending user clarification).
- Changing rollmean fill behavior for the non-raw case.
- Per-bp raw resolution (would require iterator override and is
  prohibitively expensive for typical regions).
