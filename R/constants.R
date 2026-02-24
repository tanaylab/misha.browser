# constants.R - Named defaults for misha.browser
#
# Centralizes all magic numbers so they are easy to find, document, and adjust.
# Values here should match inst/defaults.yaml where applicable.

# --- Plot / extraction defaults ------------------------------------------------

#' @keywords internal
.DEFAULT_ITERATOR <- 32L

#' @keywords internal
.DEFAULT_TARGET_POINTS <- 4000L

#' @keywords internal
.DEFAULT_SMOOTHING_BP <- 3200L

#' @keywords internal
.DEFAULT_EXPANSION <- 0L

#' @keywords internal
.DEFAULT_THEME <- "bw"

# --- UI defaults ---------------------------------------------------------------

#' @keywords internal
.DEFAULT_TITLE <- "Genome Browser"

#' @keywords internal
.DEFAULT_SPAN <- 2e6

#' @keywords internal
.DEFAULT_SMOOTH_WINDOW <- 10L

# --- Navigator defaults --------------------------------------------------------

#' @keywords internal
.DEFAULT_NAV_SOURCE <- "intervs.global.tss"

#' @keywords internal
.DEFAULT_NAV_LABEL_FIELD <- "geneSymbol"

#' @keywords internal
.DEFAULT_NAV_EXTENSION <- 1e6

# --- Panel defaults ------------------------------------------------------------

#' @keywords internal
.DEFAULT_DATA_PANEL_HEIGHT <- 2

#' @keywords internal
.DEFAULT_ANNOTATION_HEIGHT <- 1

#' @keywords internal
.DEFAULT_INTERVALS_HEIGHT <- 1

#' @keywords internal
.DEFAULT_IDEOGRAM_HEIGHT <- 0.3

#' @keywords internal
.DEFAULT_ALPHA <- 0.8

#' @keywords internal
.DEFAULT_LINEWIDTH <- 0.7

#' @keywords internal
.DEFAULT_HLINE_LINEWIDTH <- 0.5

#' @keywords internal
.DEFAULT_GROUPING_PATTERN <- "^(?<source>.+)\\.(?<mark>.+)$"

# --- Shiny / server defaults --------------------------------------------------

#' @keywords internal
.DEFAULT_PORT <- 8911L

#' @keywords internal
.DEFAULT_HOST <- "0.0.0.0"

#' @keywords internal
.DEFAULT_MAX_UPLOAD_BYTES <- 100 * 1024^2 # 100 MB

# --- Cache defaults ------------------------------------------------------------

#' @keywords internal
.DEFAULT_CACHE_MAX_ENTRIES <- 100L

#' @keywords internal
.DEFAULT_CACHE_MAX_BYTES <- 500e6 # 500 MB

# --- Navigation defaults -------------------------------------------------------

#' @keywords internal
.DEFAULT_ZOOM_FACTOR <- 2

#' @keywords internal
.DEFAULT_MOVE_FRACTION <- 0.5

#' @keywords internal
.MIN_REGION_BP <- 100L

#' @keywords internal
.DEFAULT_FALLBACK_SPAN <- 1e6

# --- Rendering constants -------------------------------------------------------

#' @keywords internal
.MAX_GENE_LEVELS <- 20L

#' @keywords internal
.MAX_INTERVAL_LEVELS <- 20L

#' @keywords internal
.DEFAULT_GENE_LABEL_SIZE <- 3

#' @keywords internal
.DEFAULT_TEXT_SIZE <- 3

#' @keywords internal
.HIGHLIGHT_ALPHA <- 0.15
