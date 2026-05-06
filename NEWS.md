# misha.browser 0.1.2

## New Features

* The panel `name` now appears as the legend title for data panels
  (e.g. a panel named `"signal"` shows "signal" above the track-name
  list in the legend).

* `browser_add_panel()` gains `show_name` (default `FALSE`), which
  renders the panel name as a bold left-side strip label (UCSC-style).
  Off by default to avoid visual noise; opt in with `show_name = TRUE`.

* `browser_add_panel()` accepts `type = "ggplot"` and a `plot`
  argument to render an arbitrary user-supplied static ggplot object
  as a panel. Useful for sample-metadata heatmaps, logos, and other
  annotations that don't depend on the genomic region. The plot is
  static (does not re-render on navigation), gets no vline/highlight
  overlay, and cannot be saved to YAML —  `browser_save_config()`
  drops ggplot panels with a warning listing the dropped names.

* `browser_add_panel()` gains `raw` (default `NULL`, inheriting from
  `cfg$plot$raw`), which renders the panel without smoothing and surfaces
  NAs as gaps. Useful for sparse data tracks where smoothing is misleading.
  Precedence at render time: Shiny `Raw view` checkbox > per-panel `raw`
  > global `cfg$plot$raw` > `FALSE`.

* Shiny UI gains a "Raw view" checkbox next to the smooth slider. When
  active, smoothing is skipped session-wide and the smooth slider is
  visually disabled.

# misha.browser 0.1.1

## New Features

* Intervals panels: `show_direction` option to plot intervals as arrows
  (triangles) by strand, for directional features like CTCF motifs. Uses
  `strand` column (BED col 6 or misha intervals). `direction_field` to
  specify custom column name.

* Simplified `browser_add_vtrack()` and YAML vtrack configuration to use a
  single inferred `expression` field. When `src`/`func` are present,
  `expression` wraps the created vtrack during extraction; otherwise it defines
  a pure expression vtrack. Legacy `expr` is still accepted as a compatibility
  alias.

# misha.browser 0.1.0

## Initial Release

### Core Features
* YAML-based genome browser configuration with profile support
* Interactive Shiny application for genome exploration
* ggplot2/patchwork-based multi-panel genome visualization
* Support for data, annotation, intervals, and ideogram panel types
* Virtual track (vtrack) system with configurable extraction functions
* Transform pipeline: smooth, log2, log10, sqrt, zscore, minmax, clip, quantile, and custom expressions
* Faceting and grouping by track metadata (e.g., source/mark)
* Vertical line overlays from files, misha tracks, or inline coordinates
* Horizontal reference lines (fixed y-value or statistical summaries)
* Color management with named palettes, auto-generation, and suffix fallbacks

### Shiny Application
* Real-time navigation: zoom, pan, go-to-gene
* In-app configuration editor for all settings
* Upload support for intervals (BED/TSV) and PSSMs (TSV/MEME/JASPAR)
* Navigator dropdown for region selection from interval sets
* Smooth window slider with live updates
* Download current plot as PNG
* Region highlight overlay

### Performance
* Two-tier LRU cache (memory + optional disk)
* Three extraction modes: fixed, dynamic, and dynamic_smooth
* Optional parallel panel extraction via `future`/`promises`

### Deployment
* `browser_deploy_local()` for Shiny Server deployment
* `browser_convert_vis_config()` for migration from misha.vis
* Multi-platform CI/CD via GitHub Actions (R-CMD-check on 5 platforms)
* pkgdown documentation site

### Programmatic API
* `browser_create()`, `browser_plot()`, `browser_run()`, `browser_launch()`
* `browser_add_panel()`, `browser_set_tracks()`, `browser_set_ylim()`
* `browser_zoom_in()`, `browser_zoom_out()`, `browser_move_left()`, `browser_move_right()`
* `browser_set_region()`, `browser_get_region()`, `browser_goto_gene()`
* `browser_load_config()`, `browser_save_config()`, `browser_create_config()`
