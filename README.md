# misha.browser

An interactive genome browser for [misha](https://github.com/tanaylab/misha) genomic tracks. Built on ggplot2 and patchwork, with YAML-based configuration and a Shiny application for interactive exploration.

## Features

- **YAML Configuration**: Define your entire browser setup in a single YAML file
- **Virtual Tracks**: Full support for misha virtual tracks with expressions, aggregation functions, and filters
- **Track Expressions**: Arithmetic operations on tracks (e.g., `"track1 / track2"`)
- **Multiple Panel Types**: Data panels, gene annotations, and ideograms
- **Data Transforms**: Smooth, log2, log10, zscore, clip, quantile normalization, and custom expressions
- **Faceting & Grouping**: Group tracks by metadata with regex pattern matching
- **Vertical Line Annotations**: Mark regions from files, misha intervals, or inline coordinates
- **Interactive Shiny App**: Navigate, zoom, highlight, and explore your data
- **Caching**: Efficient data extraction with automatic caching

## Installation

```r
# Install from GitHub
remotes::install_github("tanaylab/misha.browser")
```

## TL;DR Workflow

```r
library(misha.browser)

# 1) Create a browser rooted at the mm10 misha database
browser <- browser_create(misha_root = "/net/mraid20/export/data4/db/tgdb/mm10/trackdb/") %>%
  browser_add_panel(
    name = "signal",
    tracks = c("silicus.k27", "silicus.k4"),
    plot_type = "line",
    height = 3
  ) %>%
  browser_add_panel(
    name = "genes",
    type = "annotation"
  )

# 2) Save a YAML config for deployment
browser_save_config(browser$cfg, "config.yaml")

# 3a) Deploy to a local Shiny Server app directory
#     (creates the destination directory if needed)
browser_deploy_local(
  dest_dir = "/net/mraid20/export/tgdata/db/tgdb/tanaywiz/apps/my_user/app",
  config = "config.yaml",
  profile = "server",
  overwrite = TRUE,
  touch_restart = TRUE
)

# 3b) Or run locally (Shiny)
browser_run(browser)

# 4) Plot a region from the CLI / script
browser_plot(browser, region = "chr1:1000000-2000000")
```

## Quick Start

### From YAML Configuration

```r
library(misha.browser)

# Create browser from config file
browser <- browser_create(config = "my_browser.yaml")

# Plot current region
browser_plot(browser)

# Navigate to a gene
browser_plot(browser, gene = "Tbx5", span = 2e6)

# Launch interactive Shiny app
browser_run(browser)
```

### Programmatic Setup

```r
library(misha.browser)

# Create browser programmatically
browser <- browser_create(misha_root = "/path/to/misha") %>%
    browser_add_panel(
        name = "signal",
        tracks = c("chip.h3k27me3", "chip.h3k4me3"),
        plot_type = "line",
        colors = c(h3k27me3 = "blue", h3k4me3 = "red")
    ) %>%
    browser_add_transform("signal", type = "smooth", window = 10) %>%
    browser_add_transform("signal", type = "log2", offset = 1) %>%
    browser_set_region("chr5:118000000-121000000")

browser_plot(browser)
```

## YAML Configuration

### Basic Structure

```yaml
# Profile settings (for different environments)
profiles:
  local:
    misha_root: /path/to/local/misha
  server:
    misha_root: /path/to/server/misha

# Starting position
start:
  gene: Tbx5
  span_bp: 2000000

# UI settings
ui:
  title: "My Genome Browser"
  smooth_window_default: 10

# Plot settings
plot:
  iterator: 32
  extraction_mode: fixed  # or "dynamic" or "dynamic_smooth"
  theme: bw

# Navigator for gene selection
navigator:
  source: intervs.global.tss
  label_field: geneSymbol

# Virtual track definitions
vtracks:
  - name: signal.k27
    src: chip.h3k27me3_track
    func: sum
    sshift: -140
    eshift: 140

# Panel definitions
panels:
  - name: signal
    type: data
    tracks: [signal.k27, signal.k4]
    transforms:
      - type: smooth
        window: 10
      - type: log2
        offset: 1
    plot_type: line
    height: 3

# Vertical lines
vlines:
  - name: regions
    source: file
    file: regions.csv
    color: grey50
```

### Virtual Tracks

misha.browser supports the full misha virtual track system:

```yaml
vtracks:
  # Standard vtrack with aggregation
  - name: signal_sum
    src: chip.track
    func: sum
    sshift: -100    # Extend aggregation window
    eshift: 100

  # Quantile aggregation
  - name: signal_median
    src: chip.track
    func: quantile
    params: 0.5

  # Vtrack with expression wrapper (e.g., pmax, log2)
  # The expression is what gets extracted, with colnames mapping back to name
  - name: norm.k27
    src: at.EB4_norm_300
    func: sum
    expression: "pmax(norm.k27, 0)"  # Clamp to >= 0

  # Sequence-based vtrack (no src needed)
  - name: gc_content
    func: kmer.frac
    params:
      kmer: "GC"

  # PWM motif scoring
  - name: motif_score
    func: pwm.max
    params:
      pssm: motif.csv
      bidirect: true

  # Pure track expression (no vtrack created)
  - name: normalized
    expr: "track1 - background"

  # Vtrack with filter
  - name: unmasked
    src: chip.track
    func: avg
    filter: masked_regions
```

The `expression` field defaults to the vtrack name but can be any valid misha expression. This allows wrapping vtracks with functions like `pmax()`, `log2()`, or arithmetic operations while keeping the vtrack name for display.

### Panel Types

#### Data Panels

```yaml
panels:
  - name: chip_signal
    type: data
    tracks:
      - track1
      - track2
      # Inline expression
      - expr: "track1 / track2"
        name: ratio
    grouping:
      color_by: source
      pattern: "^(?<source>.+)\\.(?<mark>.+)$"
    facet_by: mark
    transforms:
      - type: smooth
        window: 10
      - type: log2
        offset: 1
    plot_type: line      # line, area, point, histogram
    colors:
      source1: "#E41A1C"
      source2: "#377EB8"
    ylim: [0, 10]
    height: 3
    show_legend: true
```

#### Annotation Panels

```yaml
panels:
  - name: genes
    type: annotation
    exon_source: intervs.global.exon
    tss_source: intervs.global.tss
    gene_label_field: geneSymbol
    color: navy
    show_strand_arrows: true
    height: 1
```

#### Ideogram Panels

```yaml
panels:
  - name: ideogram
    type: ideogram
    cytoband_track: intervs.cytoband
    highlight_current: true
    height: 0.3
```

### Data Transforms

Available transform types:

| Transform | Parameters | Description |
|-----------|------------|-------------|
| `smooth` | `window`, `align` | Rolling mean smoothing |
| `log2` | `offset` | Log2 transformation with offset |
| `log10` | `offset` | Log10 transformation with offset |
| `sqrt` | - | Square root transformation |
| `zscore` | - | Z-score normalization |
| `minmax` | `min`, `max` | Min-max scaling |
| `clip` | `lower`, `upper` | Clip values to range |
| `quantile` | `probs` | Quantile normalization |
| `expr` | `expr` | Custom R expression |

Example:
```yaml
transforms:
  - type: smooth
    window: 10
    align: center
  - type: log2
    offset: 1
  - type: clip
    lower: 0
    upper: 10
  - type: expr
    expr: "pmax(x, 0)"  # Custom expression (x = values)
```

### Vertical Lines

```yaml
vlines:
  # From file (CSV/TSV with chrom, start, end columns)
  - name: peaks
    source: file
    file: peaks.csv
    color: red
    linetype: solid

  # From misha intervals
  - name: genes
    source: misha
    intervals: intervs.global.tss
    color: blue

  # Inline coordinates
  - name: markers
    source: inline
    intervals:
      - chrom: chr1
        start: 1000000
        end: 1000000
      - "chr1:2000000-2000000"  # String format also works
    color: green

  # Current region (useful for highlighting)
  - name: highlight
    source: current
    color: yellow
    alpha: 0.3
```

### Extraction Modes

Control how data is extracted from misha:

```yaml
plot:
  extraction_mode: fixed  # Options: fixed, dynamic, dynamic_smooth
  iterator: 32            # Base iterator (bin size in bp)
  target_points: 4000     # Target points for dynamic modes
  smoothing_bp: 3200      # Smoothing window for dynamic_smooth mode
```

| Mode | Description |
|------|-------------|
| `fixed` | Fixed iterator + rollmean smoothing. Best for vtracks with `func=sum`. |
| `dynamic` | Adjusts iterator based on view span. More efficient for large regions. |
| `dynamic_smooth` | Dynamic iterator + dynamic vtrack sshift/eshift. Combines resolution with proper value scaling. |

## API Reference

### Browser Creation

```r
browser_create(config = NULL, misha_root = NULL, title = "Genome Browser", profile = NULL)
browser_load(file)
browser_save(browser, file)
browser_deploy_local(dest_dir, config = NULL, config_name = "config.yaml", profile = "server",
                     overwrite = FALSE, write_env = TRUE, extra_files = NULL, touch_restart = TRUE)
```

### Panel Management

```r
browser_add_panel(browser, name, type = "data", tracks = NULL, ...)
browser_add_transform(browser, panel_name, type, ...)
browser_set_tracks(browser, panel_name, tracks)
browser_set_ylim(browser, panel_name, ylim)
```

### Navigation

```r
browser_set_region(browser, region)
browser_get_region(browser)
browser_zoom_in(browser, factor = 2)
browser_zoom_out(browser, factor = 2)
browser_move_left(browser, fraction = 0.5)
browser_move_right(browser, fraction = 0.5)
```

### Visualization

```r
browser_plot(browser, region = NULL, gene = NULL, span = NULL)
browser_set_highlight(browser, start, end)
browser_clear_highlight(browser)
```

### Vertical Lines

```r
browser_add_vlines(browser, name, source, file = NULL, intervals = NULL, ...)
```

### Interactive App

```r
browser_run(browser, port = 8911, host = "0.0.0.0", launch.browser = TRUE)
browser_launch(config, port = 8911, host = "0.0.0.0", profile = NULL)
```

### Local Deployment Helper

```r
browser_deploy_local(
  dest_dir = "/srv/shiny-server/misha-browser",
  config = "inst/examples/silicus.yaml",
  profile = "server",
  overwrite = TRUE,
  extra_files = c("data/mm10_cgdom.csv", "data/ctcf_hits_1bp.tsv")
)
```

### Configuration

```r
browser_load_config(file, profile = NULL)
browser_save_config(cfg, file)
browser_create_config(misha_root = NULL, title = "Genome Browser")
browser_clear_cache()
```

### Migration

```r
# Convert misha.vis configuration to misha.browser format
browser_convert_vis_config(vis_config_file, output_file, misha_root = NULL)
```

## Examples

### Multi-track Comparison with Faceting

```yaml
panels:
  - name: histone_marks
    type: data
    tracks:
      - wt.h3k27me3
      - wt.h3k4me3
      - ko.h3k27me3
      - ko.h3k4me3
    grouping:
      color_by: condition
      pattern: "^(?<condition>\\w+)\\.(?<mark>.+)$"
    facet_by: mark
    transforms:
      - type: smooth
        window: 20
      - type: log2
        offset: 1
    colors:
      wt: black
      ko: red
    height: 4
```

### Computed Tracks

```yaml
vtracks:
  - name: ratio
    expr: "chip.signal / input.signal"
  - name: log_fc
    expr: "log2((treatment + 1) / (control + 1))"

panels:
  - name: enrichment
    type: data
    tracks: [ratio, log_fc]
    plot_type: line
```

### Custom Color Scheme

```yaml
colors:
  h3k27me3: "#1f77b4"
  h3k4me3: "#ff7f0e"
  h3k27ac: "#2ca02c"
  input: "#7f7f7f"
  _default: grey50  # Fallback color
```

Colors are matched by name. If a value ends with `.k4` or `.k27`, the base name
without the suffix is also checked (for example, `silicus.k27` falls back to
`silicus`).

## Shiny App Features

The interactive Shiny application provides:

- **Navigation**: Pan left/right, zoom in/out, history back/forward
- **Gene Search**: Quick navigation to genes via dropdown or search
- **Coordinate Input**: Direct coordinate entry (e.g., "chr5:100000-200000")
- **Brush Selection**: Zoom or highlight by brushing on the plot
- **Smooth Window Control**: Adjust smoothing in real-time
- **Span Control**: Quick span presets
- **Vertical Line Toggles**: Enable/disable vertical line layers
- **Cache Management**: Clear cache when needed

## Requirements

- R >= 4.0
- misha
- ggplot2 >= 3.4.0
- patchwork
- shiny, shinyWidgets, shinyjs, shinycssloaders
- dplyr, tidyr, zoo, yaml

## License

MIT

## Citation

If you use misha.browser in your research, please cite:

```
Lifshitz, A. (2024). misha.browser: Interactive Genome Browser for Misha Tracks.
```
