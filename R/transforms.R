# transforms.R - Data transformation pipeline for misha.browser

#' Apply a transformation pipeline to data
#'
#' Uses vectorized matrix operations where possible for performance.
#' Transforms that can be vectorized: log2, log10, sqrt, clip
#' Transforms that need per-column processing: smooth, zscore, minmax, quantile, expr
#'
#' @param data Data frame with columns: chrom, start, end, pos, and value columns
#' @param transforms List of transform specifications
#' @param value_cols Column names containing values to transform
#' @return Transformed data frame
#' @keywords internal
apply_transforms <- function(data, transforms, value_cols) {
    if (length(transforms) == 0) {
        return(data)
    }
    if (length(value_cols) == 0) {
        return(data)
    }

    # Filter to columns that exist
    value_cols <- intersect(value_cols, names(data))
    if (length(value_cols) == 0) {
        return(data)
    }

    for (transform in transforms) {
        type <- transform$type
        if (is.null(type)) next

        # Vectorizable transforms - apply as matrix operation
        if (type %in% c("log2", "log10", "sqrt", "clip")) {
            mat <- as.matrix(data[, value_cols, drop = FALSE])
            mat <- switch(type,
                "log2" = {
                    offset <- transform$offset %||% 1
                    log2(mat + offset)
                },
                "log10" = {
                    offset <- transform$offset %||% 1
                    log10(mat + offset)
                },
                "sqrt" = sqrt(pmax(mat, 0)),
                "clip" = {
                    mn <- transform$min
                    mx <- transform$max
                    if (!is.null(mn)) mat <- pmax(mat, mn)
                    if (!is.null(mx)) mat <- pmin(mat, mx)
                    mat
                }
            )
            data[, value_cols] <- mat
        } else {
            # Per-column transforms (need column-specific stats or position)
            for (col in value_cols) {
                data[[col]] <- apply_single_transform(
                    x = data[[col]],
                    pos = data$pos,
                    type = type,
                    params = transform
                )
            }
        }
    }

    data
}

#' Apply a single transform to a vector
#'
#' @param x Numeric vector to transform
#' @param pos Position vector (for position-aware transforms)
#' @param type Transform type
#' @param params Transform parameters
#' @return Transformed vector
#' @keywords internal
apply_single_transform <- function(x, pos = NULL, type, params) {
    switch(type,
        "smooth" = transform_smooth(x, params),
        "log2" = transform_log2(x, params),
        "log10" = transform_log10(x, params),
        "sqrt" = transform_sqrt(x, params),
        "zscore" = transform_zscore(x, params),
        "minmax" = transform_minmax(x, params),
        "clip" = transform_clip(x, params),
        "quantile" = transform_quantile(x, params),
        "expr" = transform_expr(x, pos, params),
        {
            cli::cli_warn("Unknown transform type: {type}")
            x
        }
    )
}

#' Smooth transform (rolling mean)
#'
#' @param x Numeric vector
#' @param params List with: window (required), align ("center", "left", "right")
#' @return Smoothed vector
#' @keywords internal
transform_smooth <- function(x, params) {
    window <- as.numeric(params$window %||% .DEFAULT_SMOOTH_WINDOW)
    align <- params$align %||% "center"
    if (!is.finite(window)) {
        return(x)
    }
    if (window > .Machine$integer.max) {
        return(x)
    }
    window <- as.integer(round(window))
    if (is.na(window)) {
        return(x)
    }
    if (window <= 1) {
        return(x)
    }

    if (length(x) < window) {
        return(x)
    }
    if (sum(!is.na(x)) < 2) {
        return(x)
    }

    zoo::rollmean(x, k = window, fill = "extend", align = align, na.rm = TRUE)
}

#' Log2 transform
#'
#' @param x Numeric vector
#' @param params List with: offset (default 1)
#' @return Log2-transformed vector
#' @keywords internal
transform_log2 <- function(x, params) {
    offset <- params$offset %||% 1
    log2(x + offset)
}

#' Log10 transform
#'
#' @param x Numeric vector
#' @param params List with: offset (default 1)
#' @return Log10-transformed vector
#' @keywords internal
transform_log10 <- function(x, params) {
    offset <- params$offset %||% 1
    log10(x + offset)
}

#' Square root transform
#'
#' @param x Numeric vector
#' @param params List (unused)
#' @return Square root of vector
#' @keywords internal
transform_sqrt <- function(x, params) {
    sqrt(pmax(x, 0))
}

#' Z-score normalization
#'
#' Standardizes values to have mean 0 and standard deviation 1.
#' If all values are NA or standard deviation is zero, returns zeros.
#'
#' @param x Numeric vector
#' @param params List (unused)
#' @return Z-score normalized vector
#' @keywords internal
transform_zscore <- function(x, params) {
    # Handle all-NA input
    if (all(is.na(x))) {
        return(x)
    }

    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)

    # Handle zero or NA standard deviation (constant values)
    if (is.na(s) || s < .Machine$double.eps) {
        # Return zeros for constant data (all values equal to mean)
        result <- rep(0, length(x))
        result[is.na(x)] <- NA
        return(result)
    }

    (x - m) / s
}

#' Min-max normalization
#'
#' Scales values to the range 0 to 1.
#'
#' @param x Numeric vector
#' @param params List (unused)
#' @return Normalized vector scaled between 0 and 1
#' @keywords internal
transform_minmax <- function(x, params) {
    mn <- min(x, na.rm = TRUE)
    mx <- max(x, na.rm = TRUE)
    if (!is.finite(mn) || !is.finite(mx) || mn == mx) {
        return(rep(0.5, length(x)))
    }
    (x - mn) / (mx - mn)
}

#' Clip values to range
#'
#' @param x Numeric vector
#' @param params List with: min, max
#' @return Clipped vector
#' @keywords internal
transform_clip <- function(x, params) {
    mn <- params$min
    mx <- params$max
    if (!is.null(mn)) x <- pmax(x, mn)
    if (!is.null(mx)) x <- pmin(x, mx)
    x
}

#' Quantile normalization
#'
#' @param x Numeric vector
#' @param params List with: probs (quantile probabilities, default c(0.01, 0.99))
#' @return Quantile-clipped vector
#' @keywords internal
transform_quantile <- function(x, params) {
    probs <- params$probs %||% c(0.01, 0.99)
    q <- stats::quantile(x, probs = probs, na.rm = TRUE)
    pmin(pmax(x, q[1]), q[2])
}

#' Safe environment for expression evaluation
#'
#' Contains only whitelisted math/vector functions. No file I/O, no system
#' calls, no environment access. This is created once and reused.
#'
#' @keywords internal
.expr_safe_env <- local({
    safe <- new.env(parent = emptyenv())
    # Arithmetic and math
    safe[["+"]] <- base::`+`
    safe[["-"]] <- base::`-`
    safe[["*"]] <- base::`*`
    safe[["/"]] <- base::`/`
    safe[["^"]] <- base::`^`
    safe[["%%"]] <- base::`%%`
    safe[["%/%"]] <- base::`%/%`
    safe[["("]] <- base::`(`
    # Comparison
    safe[[">"]] <- base::`>`
    safe[["<"]] <- base::`<`
    safe[[">="]] <- base::`>=`
    safe[["<="]] <- base::`<=`
    safe[["=="]] <- base::`==`
    safe[["!="]] <- base::`!=`
    # Logic
    safe[["&"]] <- base::`&`
    safe[["|"]] <- base::`|`
    safe[["!"]] <- base::`!`
    safe[["ifelse"]] <- base::ifelse
    # Math functions
    safe[["abs"]] <- base::abs
    safe[["sqrt"]] <- base::sqrt
    safe[["exp"]] <- base::exp
    safe[["log"]] <- base::log
    safe[["log2"]] <- base::log2
    safe[["log10"]] <- base::log10
    safe[["ceiling"]] <- base::ceiling
    safe[["floor"]] <- base::floor
    safe[["round"]] <- base::round
    safe[["sign"]] <- base::sign
    # Vector functions
    safe[["min"]] <- base::min
    safe[["max"]] <- base::max
    safe[["sum"]] <- base::sum
    safe[["mean"]] <- base::mean
    safe[["length"]] <- base::length
    safe[["pmin"]] <- base::pmin
    safe[["pmax"]] <- base::pmax
    safe[["cumsum"]] <- base::cumsum
    safe[["cummax"]] <- base::cummax
    safe[["cummin"]] <- base::cummin
    safe[["diff"]] <- base::diff
    safe[["rev"]] <- base::rev
    safe[["seq_along"]] <- base::seq_along
    safe[["seq_len"]] <- base::seq_len
    # Type checking / coercion
    safe[["is.na"]] <- base::is.na
    safe[["is.finite"]] <- base::is.finite
    safe[["as.numeric"]] <- base::as.numeric
    safe[["c"]] <- base::c
    safe[["rep"]] <- base::rep
    # Constants
    safe[["TRUE"]] <- TRUE
    safe[["FALSE"]] <- FALSE
    safe[["NA"]] <- NA
    safe[["NA_real_"]] <- NA_real_
    safe[["Inf"]] <- Inf
    safe[["pi"]] <- pi
    safe
})

#' Custom expression transform
#'
#' Evaluates a custom R expression on the data. The expression has access to
#' variables `x` (the data vector) and `pos` (position vector).
#'
#' @section Security Warning:
#' This function evaluates R expressions from YAML configuration files using
#' [eval()]. The evaluation environment is restricted to a whitelist of safe
#' math and vector functions (no file I/O, system calls, or environment access).
#' Nevertheless, **only load configuration files from trusted sources**.
#'
#' @param x Numeric vector
#' @param pos Position vector
#' @param params List with: expr (R expression as string)
#' @return Transformed vector
#' @keywords internal
transform_expr <- function(x, pos, params) {
    expr_str <- params$expr
    if (is.null(expr_str)) {
        cli::cli_warn("Transform 'expr' requires 'expr' parameter")
        return(x)
    }

    tryCatch(
        {
            # Create a child of the safe whitelist env with x and pos bound
            env <- new.env(parent = .expr_safe_env)
            env$x <- x
            env$pos <- pos

            # Evaluate expression in sandboxed environment
            result <- eval(parse(text = expr_str), envir = env)

            if (length(result) != length(x)) {
                cli::cli_warn("Transform expr result length mismatch")
                return(x)
            }

            result
        },
        error = function(e) {
            cli::cli_warn("Transform expr failed: {e$message}")
            x
        }
    )
}

#' Get smooth window from browser state
#'
#' Adjusts smooth window from config based on current state.
#'
#' @param browser Browser object
#' @param panel Panel configuration
#' @return Effective smooth window value
#' @keywords internal
get_effective_smooth_window <- function(browser, panel) {
    # Start with panel-specific or global default
    base_window <- browser$state$smooth_window %||%
        browser$cfg$ui$smooth_window_default %||%
        .DEFAULT_SMOOTH_WINDOW

    # Look for smooth transform in panel to get its window
    for (transform in panel$transforms) {
        if (transform$type == "smooth") {
            # If panel has explicit window, use that as multiplier base
            if (!is.null(transform$window)) {
                # Scale by state smooth window relative to default
                default_window <- browser$cfg$ui$smooth_window_default %||% .DEFAULT_SMOOTH_WINDOW
                scale <- base_window / default_window
                return(ceiling(transform$window * scale))
            }
        }
    }

    base_window
}

#' Update smooth transforms in panel with new window
#'
#' @param transforms List of transforms
#' @param smooth_window New smooth window value
#' @return Updated transforms list
#' @keywords internal
update_smooth_window <- function(transforms, smooth_window) {
    lapply(transforms, function(t) {
        if (t$type == "smooth") {
            t$window <- smooth_window
        }
        t
    })
}
