# Configuring misha.browser with YAML

This vignette documents the YAML configuration format used by
`misha.browser`, based on the shipped defaults (`inst/defaults.yaml`)
and the full example (`inst/examples/silicus.yaml`).

## Configuration at a glance

A minimal YAML file looks like this:

``` yaml
profiles:
  local:
    misha_root: /path/to/misha

start:
  gene: Tbx5
  span_bp: 2000000

ui:
  title: "Genome Browser"
  smooth_window_default: 10

plot:
  iterator: 32
  extraction_mode: fixed
  target_points: 4000

navigator:
  source: intervs.global.tss
  label_field: geneSymbol
  extension: 1000000

vtracks:
  - name: signal.k27
    src: chip.h3k27me3_track
    func: sum
    sshift: -140
    eshift: 140

panels:
  - name: signal
    type: data
    tracks: [signal.k27]
    plot_type: line
    height: 3

vlines:
  - name: peaks
    source: file
    file: peaks.tsv
    color: grey50
```

Unknown keys are ignored by `misha.browser`, so it is safe to add
project-specific metadata to the YAML if needed.

## Profiles and path resolution

The `profiles` section lets you define environment-specific roots:

``` yaml
profiles:
  local:
    base_dir: "../.."
    data_dir: "data"
    misha_root: /home/user/mm10
  server:
    base_dir: ".."
    data_dir: "."
    misha_root: /misha_dbs/mm10/trackdb/
```

Key behaviors:

- If you do not specify a profile, the loader auto-selects `server` when
  its `misha_root` exists, otherwise falls back to `local`.
- `base_dir` is resolved relative to the YAML file; `data_dir` is
  resolved relative to `base_dir`.
- `vlines.file` paths are resolved relative to `data_dir`.
- The selected profile is stored internally as `._profile` and the
  resolved `misha_root` is applied automatically.

## Start region

The `start` block sets the initial view:

``` yaml
start:
  gene: Tbx5
  span_bp: 2000000
```

Alternatively, you can provide explicit coordinates:

``` yaml
start:
  coords:
    chrom: chr5
    start: 118000000
    end: 121000000
```

When `gene` is provided, the navigator source is used to find the gene
and the window is centered on it with `span_bp` (or the navigator
`extension`).

## UI controls

``` yaml
ui:
  title: "Genome Browser"
  span_default: 2000000
  smooth_window_default: 10
  show_coordinates: true
```

- `span_default` controls the default span shown in the numeric input.
- `smooth_window_default` feeds the smoothing slider and is used to
  scale `smooth` transforms in fixed extraction mode.
- `show_coordinates` toggles coordinate display in the Shiny app.

## Plot extraction settings

``` yaml
plot:
  iterator: 32
  expansion: 0
  extraction_mode: fixed
  target_points: 4000
  smoothing_bp: 3200
```

Extraction modes control how misha.browser samples tracks and applies
smoothing.

### Shared formulas

- **Span**: `span = end - start` (bp)
- **Dynamic iterator**:
  `iterator = max(base_iter, ceiling(span / target_points))`
- **Smooth slider (state)**: `smooth_window = input Smooth value`
  (units: bins in fixed mode, bp in dynamic_smooth)
- **Dynamic smooth window**:
  `smoothing_bp = smooth_window if set, else plot.smoothing_bp`

### Extraction modes

#### `fixed` (default)

Step-by-step process:

1.  Set `iterator = base_iter` (from `plot.iterator`).
2.  Extract raw values with misha at that iterator (bin size).
3.  Apply panel transforms in order. If a `smooth` transform exists and
    Smooth is set, replace its window with `smooth_window` (in bins).
4.  Apply per-track transforms defined in YAML `vtracks[].transforms`
    (same rules as above).
5.  Effective smoothing in bp is `smooth_window * iterator` because the
    rolling window is measured in bins.

Best when `func=sum` values scale with bin size.

#### `dynamic`

Step-by-step process:

1.  Compute `iterator = max(base_iter, ceiling(span / target_points))`.
2.  Extract raw values with misha at the dynamic iterator.
3.  Remove any `smooth` transforms (panel + per-track YAML transforms)
    to avoid double-smoothing.
4.  Apply remaining transforms in order.

The dynamic iterator provides inherent smoothing through aggregation, so
explicit smooth transforms are skipped.

#### `dynamic_smooth`

Step-by-step process:

1.  Compute `iterator = max(base_iter, ceiling(span / target_points))`.
2.  Compute `smoothing_bp` and set vtrack iterator shifts
    `sshift = -round(smoothing_bp/2)`, `eshift = round(smoothing_bp/2)`
    for tracks in the panel.
3.  Extract raw values with misha at the dynamic iterator and shifted
    vtracks.
4.  Remove any `smooth` transforms (panel + per-track YAML transforms);
    apply remaining transforms.

Like `dynamic`, but also updates vtrack `sshift/eshift` using
`smoothing_bp` (or the current smooth window state).

### Other settings

`expansion` expands the plotting region on both sides (total expansion
in bp).

## Navigator

``` yaml
navigator:
  source: intervs.global.tss
  label_field: geneSymbol
  extension: 1000000
```

The navigator supplies the gene list for searching and determines how
gene-based navigation expands the viewing window. `source` can be a
misha intervals name or a CSV file path with `chrom`, `start`, `end`
columns.

## Global color mapping

``` yaml
colors:
  silicus: "#E41A1C"
  borzoi: "#377EB8"
  _default: grey50
```

Colors are matched by name. If a value ends with `.k4` or `.k27`, the
base name without the suffix is also checked (for example, `silicus.k27`
falls back to `silicus`). When a value is missing, a deterministic
hash-based color is generated; `_default` is used as a fallback.

## Virtual tracks (`vtracks`)

Each vtrack entry is validated before creation. Common fields:

``` yaml
vtracks:
  - name: signal.k27
    src: chip.h3k27me3_track
    func: sum
    sshift: -140
    eshift: 140
```

Supported patterns:

- Standard vtracks: `name`, `src`, `func`, optional `params`, `filter`,
  and iterator shifts (`sshift`, `eshift`, `dim`).
- Sequence-based vtracks (no `src` required) for `kmer.*`, `pwm.*`,
  `masked.*` functions.
- Expression handling uses a single `expression` field. If `src`/`func`
  are present, it wraps the created vtrack during extraction. If no
  `src` or `func` are given, it defines a pure expression vtrack.

For example, this is correct:

``` r
browser <- browser_create(misha_root = .misha$GROOT) %>%
  browser_add_vtrack(
    "chipseq_q",
    src = "chipseq.ctcf.LCL.hsa81.3",
    func = "global.percentile.max",
    expression = "-log2(1 - chipseq_q)"
  ) %>%
  browser_add_panel(
    name = "chipseq",
    tracks = "chipseq_q",
    plot_type = "line",
    height = 2
  )
```

This creates `chipseq_q` with `global.percentile.max` and then extracts
`-log2(1 - chipseq_q)`.

You can also inline vtracks or expressions directly in `panels.tracks`
using objects that include `src`/`func` or `expr`.

## Panels

Panels define what gets plotted and how they are arranged.

### Data panels

``` yaml
panels:
  - name: signal
    type: data
    tracks:
      - silicus.k27
      - silicus.k4
    plot_type: line
    height: 4
    alpha: 0.8
    linewidth: 0.7
    show_legend: true
```

Grouping and faceting (from `inst/examples/silicus.yaml`):

``` yaml
grouping:
  color_by: source
  pattern: "^(?<source>.+)\\.(?<mark>.+)$"
  overrides:
    norm.k27:
      source: norm
      mark: k27
facet_by: mark
```

- `pattern` uses named capture groups to add metadata columns.
- `color_by` controls the mapped aesthetic (`track`, `source`, or
  `mark`).
- `facet_by` uses `scales = "free_y"` so each facet can scale
  independently.

Horizontal lines can be added to data panels:

``` yaml
hlines:
  - "y": 6
    color: black
    linetype: solid
    label: "Midpoint"
  - stat: mean
    color: blue
    linetype: dashed
    label: "Mean"
```

### Annotation panels

``` yaml
panels:
  - name: genes
    type: annotation
    exon_source: intervs.global.exon
    tss_source: intervs.global.tss
    gene_label_field: geneSymbol
    color: navy
    show_strand_arrows: true
```

### Intervals panels

``` yaml
panels:
  - name: repeats
    type: intervals
    intervals: intervs.global.rmsk
    color_by: class
    filter_field: class
    filter_values: [LINE, LTR]
```

Intervals panels support `source` (`intervals` or `file`), `color_by`,
`colors`, `outline_color`, `label_field`, `show_labels`,
`show_direction` (arrows by strand for directional intervals like CTCF
motifs), `direction_field` (default `strand`), and filtering via
`filter_field` + `filter_values` or `filter_regex`.

### Ideogram panels

``` yaml
panels:
  - name: ideogram
    type: ideogram
    cytoband_track: intervs.global.cytoband
    highlight_current: true
```

Ideograms optionally load cytobands from `cytoband_track` or
`cytoband_file` and can highlight the current region.

## Vertical lines (`vlines`)

Vertical line layers can be sourced in several ways:

``` yaml
vlines:
  - name: peaks
    source: file
    file: peaks.tsv
    color: grey50
    linetype: dashed
    show_bounds: true
    enabled: true
```

Supported sources:

- `file`: CSV/TSV/BED with `chrom`, `start`, `end`.
- `intervals`: a misha interval track (`intervals: intervs.global.tss`).
- `inline`: specify `intervals` as a list of inline coordinates.
- `current`: adds lines at the current region boundaries.

`show_bounds` toggles whether both bounds are shown or just the
midpoint.

## Data transformations and scales

Transforms run in order and are applied first at the panel level and
then on each vtrack with its own `transforms` list.

Available transform types:

- `smooth`: rolling mean (`window`, optional `align`).
- `log2` / `log10`: log transform with `offset` (default 1).
- `sqrt`: square root with floor at 0.
- `zscore`: mean/SD normalization.
- `minmax`: normalize to 0..1.
- `clip`: clamp to `[min, max]`.
- `quantile`: clamp to `probs` (default `c(0.01, 0.99)`).
- `expr`: custom R expression using `x` and `pos`.

Example:

``` yaml
transforms:
  - type: smooth
    window: 10
  - type: log2
    offset: 1
  - type: clip
    min: 0
    max: 10
```

Scale controls are per panel:

``` yaml
ylim: [0, 10]
y_title: "ATAC"
y_labels: percent
```

- `ylim` uses `coord_cartesian` so data outside the range is clipped
  rather than dropped.
- `y_labels` supports `numeric` (default), `percent`, `scientific`, and
  `comma`.
- `plot_type` can be `line`, `area`, `point`, or `histogram`.

## Example: silicus config highlights

The file `inst/examples/silicus.yaml` combines all major features:

- Multiple profiles with path resolution.
- Dozens of vtracks with `func=sum` and iterator shifts.
- A faceted data panel grouped by `source` and `mark`.
- Additional panels for ATAC, GC/CpG content, repeats, and gene
  annotations.
- Vertical line overlays loaded from CSV/TSV files.

Use it as a reference when building a full browser configuration.

## Troubleshooting

### Common Issues

#### “Misha root directory does not exist”

This error occurs when the `misha_root` path in your configuration
doesn’t exist.

**Solution**: Check the path in your YAML config under the active
profile:

``` yaml
profiles:
  local:
    misha_root: /path/to/misha  # Verify this path exists
```

Ensure the directory contains a valid misha trackdb (should have
`.misha` marker files).

#### “Failed to initialize misha database”

The misha root exists but `gsetroot()` failed. This usually means the
directory is not a valid misha database.

**Solution**: Verify your misha root:

``` r
misha::gsetroot("/your/misha/root")
misha::gtrack.ls()  # Should list available tracks
```

#### Tracks not found / empty panels

Your panel references tracks that don’t exist in the misha database.

**Solution**: 1. Check track names match exactly (case-sensitive) 2.
Verify tracks exist: `misha::gtrack.exists("your.track.name")` 3. For
vtracks, ensure the source track (`src`) exists

#### Plot appears blank or shows only axes

This can happen when: - The viewing region has no data - All values are
NA - The region is outside chromosome bounds

**Solution**: 1. Navigate to a different region or gene 2. Check if the
track has data: `misha::gextract("track.name", gintervals(1, 0, 1e6))`
3. Verify chromosome names match (e.g., `chr1` vs `1`)

#### Shiny app crashes on startup

Common causes: - Invalid YAML syntax - Missing required fields - Profile
not found

**Solution**: 1. Validate YAML syntax using an online validator 2. Check
for required fields: `misha_root`, at least one panel 3. Ensure the
profile name matches what’s in `profiles:`

#### Slow performance / memory issues

**Solutions**: 1. Increase iterator size for smoother data:
`yaml plot: iterator: 64 # Default is 32` 2. Use `dynamic` extraction
mode for large regions:
`yaml plot: extraction_mode: dynamic target_points: 2000` 3. Clear the
cache periodically:
[`browser_clear_cache()`](https://tanaylab.github.io/misha.browser/reference/browser_clear_cache.md)
4. Reduce the number of tracks per panel

#### Colors not applying correctly

**Solution**: Check color mapping: - For `color_by: track`, colors
should match track names - For `color_by: source`, colors should match
extracted group names - Use `_default` for fallback:
`yaml colors: known_value: "#E41A1C" _default: grey50`

### Getting Help

If you encounter issues not covered here:

1.  Check the package site: <https://tanaylab.github.io/misha.browser/>
2.  Contact the maintainer: <aviezer.lifshitz@weizmann.ac.il>
3.  Ensure you have the latest version:
    `remotes::install_github("tanaylab/misha.browser")`
4.  Try with a minimal configuration to isolate the problem
