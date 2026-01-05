# ideogram.R - Chromosome ideogram panel for misha.browser

#' Render ideogram panel
#'
#' @param panel Panel configuration
#' @param region Viewing region
#' @return ggplot object
#' @keywords internal
render_ideogram_panel <- function(panel, region) {
    region <- sanitize_interval(region)
    if (is.null(region)) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void())
    }

    chrom <- region$chrom

    # Get chromosome size
    chrom_size <- get_chromosome_size(chrom)
    if (is.null(chrom_size)) {
        chrom_size <- get_chromosome_size_from_intervals(chrom)
    }
    if (is.null(chrom_size) || chrom_size <= 0) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void())
    }

    # Try to load cytoband data
    cytoband_data <- load_cytoband_data(panel$cytoband_file, panel$cytoband_track, chrom)

    # Create ideogram plot
    p <- ggplot2::ggplot()

    if (!is.null(cytoband_data) && nrow(cytoband_data) > 0) {
        # Full cytoband rendering
        p <- p + ggplot2::geom_rect(
            data = cytoband_data,
            ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = 1, fill = stain),
            color = NA
        ) + ggplot2::scale_fill_manual(
            values = cytoband_colors(),
            guide = "none"
        )
    } else {
        # Simple chromosome bar
        p <- p + ggplot2::geom_rect(
            data = data.frame(start = 0, end = chrom_size),
            ggplot2::aes(xmin = start, xmax = end, ymin = 0.1, ymax = 0.9),
            fill = "grey80", color = "grey40", linewidth = 0.5
        )
    }

    # Highlight current viewing region
    if (panel$highlight_current %||% TRUE) {
        p <- p + ggplot2::geom_rect(
            data = data.frame(start = region$start, end = region$end),
            ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "red", alpha = 0.4, color = "red", linewidth = 0.3
        )
    }

    # Chromosome outline
    p <- p + ggplot2::geom_rect(
        data = data.frame(start = 0, end = chrom_size),
        ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
        fill = NA, color = "black", linewidth = 0.3
    )

    # Chromosome label
    p <- p + ggplot2::annotate(
        "text",
        x = chrom_size / 2,
        y = 0.5,
        label = chrom,
        size = 3,
        fontface = "bold"
    )

    # Apply theme
    p <- p +
        ggplot2::scale_x_continuous(expand = c(0.01, 0), limits = c(0, chrom_size)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(-0.1, 1.1)) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.margin = ggplot2::margin(b = 2, t = 2, l = 5, r = 5)
        )

    p
}

#' Get chromosome size
#'
#' @param chrom Chromosome name
#' @return Chromosome size in bp, or NULL
#' @keywords internal
get_chromosome_size <- function(chrom) {
    tryCatch(
        {
            chrom_sizes <- misha::gintervals.chrom_sizes()
            chrom_names <- names(chrom_sizes)
            if (chrom %in% chrom_names) {
                return(as.numeric(chrom_sizes[[chrom]])[1])
            }

            alt_chrom <- if (grepl("^chr", chrom, ignore.case = TRUE)) {
                sub("^chr", "", chrom, ignore.case = TRUE)
            } else {
                paste0("chr", chrom)
            }
            if (alt_chrom %in% chrom_names) {
                return(as.numeric(chrom_sizes[[alt_chrom]])[1])
            }
            NULL
        },
        error = function(e) {
            NULL
        }
    )
}

#' Get chromosome size from intervals as fallback
#'
#' @param chrom Chromosome name
#' @return Chromosome size in bp, or NULL
#' @keywords internal
get_chromosome_size_from_intervals <- function(chrom) {
    tryCatch(
        {
            intervals <- misha::gintervals.all()
            if (!is.data.frame(intervals) || nrow(intervals) == 0) {
                return(NULL)
            }
            intervals <- intervals[intervals$chrom == chrom, ]
            if (nrow(intervals) == 0) {
                return(NULL)
            }
            as.numeric(max(intervals$end, na.rm = TRUE))[1]
        },
        error = function(e) {
            NULL
        }
    )
}

#' Load cytoband data
#'
#' @param cytoband_file Path to cytoband file
#' @param cytoband_track Misha intervals name for cytobands
#' @param chrom Chromosome to filter to
#' @return Data frame with cytoband data
#' @keywords internal
load_cytoband_data <- function(cytoband_file, cytoband_track, chrom) {
    if (!is.null(cytoband_file) && file.exists(cytoband_file)) {
        return(tryCatch(
            {
                cytobands <- readr::read_tsv(
                    cytoband_file,
                    col_names = c("chrom", "start", "end", "name", "stain"),
                    show_col_types = FALSE
                )
                cytobands <- cytobands[cytobands$chrom == chrom, ]
                cytobands
            },
            error = function(e) {
                NULL
            }
        ))
    }

    if (is.null(cytoband_track) || !intervals_exist(cytoband_track)) {
        return(NULL)
    }

    tryCatch(
        {
            cytobands <- misha::gintervals.load(cytoband_track)
            if (!is.data.frame(cytobands) || nrow(cytobands) == 0) {
                return(NULL)
            }

            if (!"chrom" %in% names(cytobands)) {
                return(NULL)
            }
            if (!"start" %in% names(cytobands) && "chromStart" %in% names(cytobands)) {
                cytobands$start <- cytobands$chromStart
            }
            if (!"end" %in% names(cytobands) && "chromEnd" %in% names(cytobands)) {
                cytobands$end <- cytobands$chromEnd
            }
            if (!"stain" %in% names(cytobands)) {
                if ("gieStain" %in% names(cytobands)) {
                    cytobands$stain <- cytobands$gieStain
                } else {
                    cytobands$stain <- "gneg"
                }
            }

            cytobands <- cytobands[cytobands$chrom == chrom, ]
            cytobands
        },
        error = function(e) {
            NULL
        }
    )
}

#' Cytoband stain colors
#'
#' @return Named vector of colors
#' @keywords internal
cytoband_colors <- function() {
    c(
        "gneg" = "white",
        "gpos25" = "grey75",
        "gpos50" = "grey50",
        "gpos75" = "grey25",
        "gpos100" = "black",
        "acen" = "red",
        "gvar" = "grey60",
        "stalk" = "grey80"
    )
}
