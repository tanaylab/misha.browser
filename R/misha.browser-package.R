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
    "x_start", "x_end", "strand",
    "chrom", "track", "source", "mark",
    "intervalID", "label_x"
))

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
