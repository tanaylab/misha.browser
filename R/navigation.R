# navigation.R - Navigation helpers for misha.browser

#' Zoom in on current region
#'
#' @param browser Browser object
#' @param factor Zoom factor (2 = double resolution, default)
#' @return Updated browser object
#' @export
browser_zoom_in <- function(browser, factor = .DEFAULT_ZOOM_FACTOR) {
    region <- browser$state$current_region
    if (is.null(region)) {
        return(browser)
    }

    center <- interval_center(region)
    new_width <- max(.MIN_REGION_BP, interval_width(region) / factor)

    browser$state$current_region <- make_region(
        region$chrom, center - new_width / 2, center + new_width / 2
    )
    browser
}

#' Zoom out from current region
#'
#' @param browser Browser object
#' @param factor Zoom factor (2 = half resolution, default)
#' @return Updated browser object
#' @export
browser_zoom_out <- function(browser, factor = .DEFAULT_ZOOM_FACTOR) {
    region <- browser$state$current_region
    if (is.null(region)) {
        return(browser)
    }

    center <- interval_center(region)
    new_width <- interval_width(region) * factor

    browser$state$current_region <- make_region(
        region$chrom, center - new_width / 2, center + new_width / 2
    )
    browser
}

#' Move view left
#'
#' @param browser Browser object
#' @param fraction Fraction of view width to move (default 0.5)
#' @return Updated browser object
#' @export
browser_move_left <- function(browser, fraction = .DEFAULT_MOVE_FRACTION) {
    region <- browser$state$current_region
    if (is.null(region)) {
        return(browser)
    }

    shift <- floor(interval_width(region) * fraction)
    browser$state$current_region <- make_region(
        region$chrom, region$start - shift, region$end - shift
    )
    browser
}

#' Move view right
#'
#' @param browser Browser object
#' @param fraction Fraction of view width to move (default 0.5)
#' @return Updated browser object
#' @export
browser_move_right <- function(browser, fraction = .DEFAULT_MOVE_FRACTION) {
    region <- browser$state$current_region
    if (is.null(region)) {
        return(browser)
    }

    shift <- floor(interval_width(region) * fraction)
    browser$state$current_region <- make_region(
        region$chrom, region$start + shift, region$end + shift
    )
    browser
}

#' Expand interval to a specific width
#'
#' @param interval Interval data frame
#' @param target_width Target width in bp
#' @return Expanded interval
#' @keywords internal
expand_interval <- function(interval, target_width) {
    interval <- sanitize_interval(interval)
    if (is.null(interval)) {
        return(NULL)
    }
    if (interval_width(interval) >= target_width) {
        return(interval)
    }

    center <- interval_center(interval)
    make_region(interval$chrom, center - target_width / 2, center + target_width / 2)
}

#' Set interval to specific width (centered)
#'
#' @param interval Interval data frame
#' @param target_width Target width in bp
#' @return Resized interval
#' @keywords internal
set_interval_width <- function(interval, target_width) {
    interval <- sanitize_interval(interval)
    if (is.null(interval)) {
        return(NULL)
    }

    target_width <- suppressWarnings(as.numeric(target_width))
    if (!is.finite(target_width) || target_width <= 1) {
        return(interval)
    }

    center <- interval_center(interval)
    make_region(interval$chrom, center - target_width / 2, center + target_width / 2)
}

#' Go to gene location
#'
#' @param browser Browser object
#' @param gene Gene name
#' @param span Span around gene center
#' @return Updated browser object
#' @keywords internal
browser_goto_gene <- function(browser, gene, span = NULL) {
    nav_regions <- load_navigator_regions(browser)
    if (is.null(nav_regions)) {
        return(browser)
    }

    label_field <- browser$cfg$navigator$label_field %||% "geneSymbol"
    if (!label_field %in% names(nav_regions)) {
        return(browser)
    }

    gene_region <- nav_regions[nav_regions[[label_field]] == gene, ]
    if (nrow(gene_region) == 0) {
        cli::cli_warn("Gene '{gene}' not found")
        return(browser)
    }

    gene_region <- gene_region[1, ]
    span <- span %||% browser$cfg$navigator$extension %||% .DEFAULT_NAV_EXTENSION
    center <- interval_center(gene_region)

    browser$state$current_region <- make_region(
        gene_region$chrom, center - span / 2, center + span / 2
    )
    browser
}
