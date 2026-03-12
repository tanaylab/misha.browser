#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom stats setNames median quantile
#' @importFrom utils head tail object.size
## usethis namespace: end
NULL

# Suppress R CMD check notes for ggplot2 aes() variables
utils::globalVariables(c(
    "start", "end", "pos", "value", "stain",
    "y_level", "min_start", "max_end",
    "x_start", "x_end", "x_from", "x_to", "strand",
    "chrom", "track", "source", "mark",
    "intervalID", "label_x"
))
