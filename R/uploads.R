# Upload Manager for Intervals and PSSMs
# Session-scoped storage for uploaded files

#' @noRd
.upload_storage <- new.env(parent = emptyenv())

#' Initialize upload storage for a session
#' @noRd
init_upload_storage <- function() {
    if (!exists("intervals", envir = .upload_storage)) {
        assign("intervals", list(), envir = .upload_storage)
    }
    if (!exists("pssms", envir = .upload_storage)) {
        assign("pssms", list(), envir = .upload_storage)
    }
}

#' Get all uploaded intervals
#' @return Named list of uploaded intervals data frames
#' @noRd
get_uploaded_intervals <- function() {
    init_upload_storage()
    get("intervals", envir = .upload_storage)
}

#' Get a specific uploaded intervals set by name
#' @param name Name of the intervals set
#' @return Data frame of intervals or NULL if not found
#' @noRd
get_uploaded_intervals_by_name <- function(name) {
    intervals <- get_uploaded_intervals()
    intervals[[name]]
}

#' Store uploaded intervals
#' @param name Name for the intervals set
#' @param data Data frame with chrom, start, end columns
#' @param filename Original filename (for display)
#' @noRd
store_uploaded_intervals <- function(name, data, filename = NULL) {
    init_upload_storage()
    intervals <- get("intervals", envir = .upload_storage)

    intervals[[name]] <- list(
        data = data,
        filename = filename %||% "unknown",
        rows = nrow(data),
        uploaded_at = Sys.time()
    )

    assign("intervals", intervals, envir = .upload_storage)
    invisible(name)
}

#' Delete uploaded intervals by name
#' @param name Name of the intervals set to delete
#' @noRd
delete_uploaded_intervals <- function(name) {
    init_upload_storage()
    intervals <- get("intervals", envir = .upload_storage)
    intervals[[name]] <- NULL
    assign("intervals", intervals, envir = .upload_storage)
    invisible(NULL)
}

#' Get all uploaded PSSMs
#' @return Named list of uploaded PSSM matrices
#' @noRd
get_uploaded_pssms <- function() {
    init_upload_storage()
    get("pssms", envir = .upload_storage)
}

#' Get a specific uploaded PSSM by name
#' @param name Name of the PSSM
#' @return Matrix with A, C, G, T columns or NULL if not found
#' @noRd
get_uploaded_pssm_by_name <- function(name) {
    pssms <- get_uploaded_pssms()
    pssm_entry <- pssms[[name]]
    if (!is.null(pssm_entry)) {
        return(pssm_entry$data)
    }
    NULL
}

#' Store uploaded PSSM
#' @param name Name for the PSSM
#' @param data Matrix with A, C, G, T columns
#' @param filename Original filename (for display)
#' @noRd
store_uploaded_pssm <- function(name, data, filename = NULL) {
    init_upload_storage()
    pssms <- get("pssms", envir = .upload_storage)

    pssms[[name]] <- list(
        data = data,
        filename = filename %||% "unknown",
        dimensions = paste0(nrow(data), "x", ncol(data)),
        uploaded_at = Sys.time()
    )

    assign("pssms", pssms, envir = .upload_storage)
    invisible(name)
}

#' Delete uploaded PSSM by name
#' @param name Name of the PSSM to delete
#' @noRd
delete_uploaded_pssm <- function(name) {
    init_upload_storage()
    pssms <- get("pssms", envir = .upload_storage)
    pssms[[name]] <- NULL
    assign("pssms", pssms, envir = .upload_storage)
    invisible(NULL)
}

#' Clear all uploaded data
#' @noRd
clear_upload_storage <- function() {
    assign("intervals", list(), envir = .upload_storage)
    assign("pssms", list(), envir = .upload_storage)
    invisible(NULL)
}

# =============================================================================
# File Parsing Functions
# =============================================================================

#' Parse intervals from uploaded file
#' @param file_path Path to the uploaded file
#' @param format File format: "auto", "bed", "tsv"
#' @return Data frame with chrom, start, end columns
#' @noRd
parse_intervals_file <- function(file_path, format = "auto") {
    if (format == "auto") {
        format <- detect_intervals_format(file_path)
    }

    data <- switch(format,
        "bed" = parse_bed_file(file_path),
        "tsv" = parse_tsv_intervals(file_path),
        stop("Unknown intervals format: ", format)
    )

    validate_intervals_data(data)
    data
}

#' Detect intervals file format from extension and content
#' @noRd
detect_intervals_format <- function(file_path) {
    ext <- tolower(tools::file_ext(file_path))

    if (ext %in% c("bed", "bed3", "bed6", "bed12")) {
        return("bed")
    }

    if (ext %in% c("tsv", "txt", "csv")) {
        # Check header to determine format
        first_line <- readLines(file_path, n = 1)
        if (grepl("chrom", first_line, ignore.case = TRUE)) {
            return("tsv")
        }
        # No header, assume BED format
        return("bed")
    }

    # Default to TSV
    "tsv"
}

#' Parse BED format file
#' @noRd
parse_bed_file <- function(file_path) {
    # BED format: chrom, start, end, [name, score, strand, ...]
    # No header, tab-separated
    data <- utils::read.table(
        file_path,
        header = FALSE,
        sep = "\t",
        stringsAsFactors = FALSE,
        comment.char = "#",
        fill = TRUE
    )

    # Take first 3 columns
    if (ncol(data) < 3) {
        stop("BED file must have at least 3 columns (chrom, start, end)")
    }

    result <- data.frame(
        chrom = as.character(data[[1]]),
        start = as.integer(data[[2]]),
        end = as.integer(data[[3]]),
        stringsAsFactors = FALSE
    )

    # BED is 0-based, half-open - convert to 1-based for misha if needed
    # Actually misha uses 0-based coordinates, so keep as-is
    result
}

#' Parse TSV intervals file with header
#' @noRd
parse_tsv_intervals <- function(file_path) {
    # Detect separator
    first_line <- readLines(file_path, n = 1)
    sep <- if (grepl("\t", first_line)) "\t" else ","

    data <- utils::read.table(
        file_path,
        header = TRUE,
        sep = sep,
        stringsAsFactors = FALSE,
        comment.char = "#"
    )

    # Normalize column names
    colnames(data) <- tolower(colnames(data))

    # Find required columns
    chrom_col <- grep("^(chrom|chr|chromosome)$", colnames(data), value = TRUE)[1]
    start_col <- grep("^(start|chromstart)$", colnames(data), value = TRUE)[1]
    end_col <- grep("^(end|chromend|stop)$", colnames(data), value = TRUE)[1]

    if (is.na(chrom_col) || is.na(start_col) || is.na(end_col)) {
        stop("Could not find required columns (chrom, start, end) in intervals file")
    }

    data.frame(
        chrom = as.character(data[[chrom_col]]),
        start = as.integer(data[[start_col]]),
        end = as.integer(data[[end_col]]),
        stringsAsFactors = FALSE
    )
}

#' Validate intervals data frame
#' @noRd
validate_intervals_data <- function(data) {
    if (!is.data.frame(data)) {
        stop("Intervals data must be a data frame")
    }

    required_cols <- c("chrom", "start", "end")
    missing <- setdiff(required_cols, colnames(data))
    if (length(missing) > 0) {
        stop("Missing required columns: ", paste(missing, collapse = ", "))
    }

    if (nrow(data) == 0) {
        stop("Intervals file is empty")
    }

    if (any(is.na(data$chrom))) {
        stop("Intervals contain NA values in chrom column")
    }

    if (any(is.na(data$start) | is.na(data$end))) {
        stop("Intervals contain NA values in start/end columns")
    }

    if (any(data$start >= data$end)) {
        stop("Invalid intervals: start must be less than end")
    }

    invisible(TRUE)
}

# =============================================================================
# PSSM Parsing Functions
# =============================================================================

#' Parse PSSM from uploaded file
#' @param file_path Path to the uploaded file
#' @param format File format: "auto", "tsv", "meme", "jaspar"
#' @return Matrix with columns A, C, G, T
#' @noRd
parse_pssm_file <- function(file_path, format = "auto") {
    if (format == "auto") {
        format <- detect_pssm_format(file_path)
    }

    pssm <- switch(format,
        "tsv" = parse_tsv_pssm(file_path),
        "meme" = parse_meme_pssm(file_path),
        "jaspar" = parse_jaspar_pssm(file_path),
        stop("Unknown PSSM format: ", format)
    )

    # Validate and normalize (returns the normalized matrix)
    pssm <- validate_pssm(pssm)
    pssm
}

#' Detect PSSM file format
#' @noRd
detect_pssm_format <- function(file_path) {
    ext <- tolower(tools::file_ext(file_path))

    if (ext == "meme") {
        return("meme")
    }

    if (ext == "jaspar") {
        return("jaspar")
    }

    # Check content
    lines <- readLines(file_path, n = 10)
    content <- paste(lines, collapse = "\n")

    if (grepl("MEME version", content, ignore.case = TRUE)) {
        return("meme")
    }

    if (grepl("^>", lines[1])) {
        return("jaspar")
    }

    # Default to TSV
    "tsv"
}

#' Parse TSV/CSV PSSM file
#' @noRd
parse_tsv_pssm <- function(file_path) {
    # Detect separator
    first_line <- readLines(file_path, n = 1)
    sep <- if (grepl("\t", first_line)) "\t" else ","

    # Try reading with header
    data <- tryCatch(
        {
            d <- utils::read.table(
                file_path,
                header = TRUE,
                sep = sep,
                stringsAsFactors = FALSE,
                comment.char = "#"
            )
            # Check if we got A, C, G, T columns
            cols <- toupper(colnames(d))
            if (all(c("A", "C", "G", "T") %in% cols)) {
                colnames(d) <- cols
                d
            } else {
                NULL
            }
        },
        error = function(e) NULL
    )

    if (is.null(data)) {
        # Try without header - assume columns are A, C, G, T in order
        data <- utils::read.table(
            file_path,
            header = FALSE,
            sep = sep,
            stringsAsFactors = FALSE,
            comment.char = "#"
        )

        if (ncol(data) < 4) {
            stop("PSSM file must have at least 4 columns (A, C, G, T)")
        }

        colnames(data)[1:4] <- c("A", "C", "G", "T")
    }

    pssm <- as.matrix(data[, c("A", "C", "G", "T")])
    rownames(pssm) <- NULL
    pssm
}

#' Parse MEME format PSSM file
#' @noRd
parse_meme_pssm <- function(file_path) {
    lines <- readLines(file_path)

    # Find the letter-probability matrix section
    matrix_start <- grep("^letter-probability matrix", lines, ignore.case = TRUE)
    if (length(matrix_start) == 0) {
        stop("Could not find 'letter-probability matrix' section in MEME file")
    }

    # Parse matrix lines (after the header line until empty line or next section)
    start_idx <- matrix_start[1] + 1
    matrix_lines <- c()

    for (i in start_idx:length(lines)) {
        line <- trimws(lines[i])
        if (line == "" || grepl("^[A-Za-z]", line)) {
            break
        }
        matrix_lines <- c(matrix_lines, line)
    }

    if (length(matrix_lines) == 0) {
        stop("No matrix data found in MEME file")
    }

    # Parse the matrix values
    pssm <- do.call(rbind, lapply(matrix_lines, function(line) {
        as.numeric(strsplit(trimws(line), "\\s+")[[1]])
    }))

    colnames(pssm) <- c("A", "C", "G", "T")
    pssm
}

#' Parse JASPAR format PSSM file
#' @noRd
parse_jaspar_pssm <- function(file_path) {
    lines <- readLines(file_path)

    # Skip header line starting with >
    data_lines <- lines[!grepl("^>", lines) & nchar(trimws(lines)) > 0]

    if (length(data_lines) != 4) {
        stop("JASPAR format expects exactly 4 lines of data (A, C, G, T)")
    }

    # Parse each line: A [ values ]
    parse_jaspar_line <- function(line) {
        # Remove letter and brackets
        values <- gsub("^[ACGT]\\s*\\[?|\\]?$", "", line)
        as.numeric(strsplit(trimws(values), "\\s+")[[1]])
    }

    values <- lapply(data_lines, parse_jaspar_line)

    # Transpose: JASPAR has bases as rows, positions as columns
    pssm <- do.call(cbind, values)
    colnames(pssm) <- c("A", "C", "G", "T")
    pssm
}

#' Validate and normalize PSSM matrix
#' @param pssm Matrix to validate
#' @return Validated (and possibly normalized) PSSM matrix
#' @noRd
validate_pssm <- function(pssm) {
    if (!is.matrix(pssm)) {
        stop("PSSM must be a matrix")
    }

    if (ncol(pssm) != 4) {
        stop("PSSM must have exactly 4 columns (A, C, G, T)")
    }

    expected_cols <- c("A", "C", "G", "T")
    if (!all(colnames(pssm) == expected_cols)) {
        stop("PSSM columns must be named A, C, G, T")
    }

    if (nrow(pssm) == 0) {
        stop("PSSM is empty")
    }

    if (!is.numeric(pssm)) {
        stop("PSSM values must be numeric")
    }

    if (any(is.na(pssm))) {
        stop("PSSM contains NA values")
    }

    # Check if values are valid probabilities (allow some tolerance)
    row_sums <- rowSums(pssm)
    if (any(pssm < 0)) {
        stop("PSSM contains negative values")
    }

    # If values sum to ~1, it's a probability matrix
    # If values are counts or log-odds, that's also fine
    # Just warn if rows don't sum to 1 and values aren't clearly log-odds
    if (any(row_sums > 1.1) && all(pssm >= 0) && max(pssm) < 100) {
        # Likely counts, normalize to probabilities
        pssm <- pssm / rowSums(pssm)
    }

    pssm
}

# =============================================================================
# Prego Motif Helpers
# =============================================================================

#' Get prego motifs grouped by dataset for selectize
#' @return Named list of motif names grouped by dataset
#' @noRd
get_prego_motif_choices <- function() {
    if (!requireNamespace("prego", quietly = TRUE)) {
        return(list())
    }

    motifs <- prego::all_motif_datasets()
    datasets <- unique(motifs$dataset)

    choices <- lapply(datasets, function(ds) {
        unique(motifs$motif[motifs$dataset == ds])
    })
    names(choices) <- datasets

    choices
}

#' Get PSSM matrix from prego motif name
#' @param motif_name Full motif name (e.g., "HOMER.AP_1")
#' @return Matrix with A, C, G, T columns
#' @noRd
get_prego_pssm <- function(motif_name) {
    if (!requireNamespace("prego", quietly = TRUE)) {
        stop("prego package is required for prego motifs")
    }

    motifs <- prego::all_motif_datasets()
    pssm_data <- motifs[motifs$motif == motif_name, ]

    if (nrow(pssm_data) == 0) {
        stop("Motif '", motif_name, "' not found in prego database")
    }

    # Sort by position
    pssm_data <- pssm_data[order(pssm_data$pos), ]

    pssm <- as.matrix(pssm_data[, c("A", "C", "G", "T")])
    rownames(pssm) <- NULL
    pssm
}

# =============================================================================
# Vtrack Function Type Helpers
# =============================================================================

#' Check if a vtrack function requires intervals source
#' @param func Function name
#' @return TRUE if function requires intervals source
#' @noRd
is_intervals_function <- function(func) {
    func %in% c("distance", "distance.center", "distance.edge", "coverage", "neighbor.count")
}

#' Check if a vtrack function is sequence-based (no source track)
#' @param func Function name
#' @return TRUE if function is sequence-based
#' @noRd
is_sequence_function <- function(func) {
    func %in% c(
        "pwm", "pwm.max", "pwm.max.pos", "pwm.count",
        "kmer.count", "kmer.frac", "masked.count", "masked.frac"
    )
}

#' Check if a vtrack function is PWM-based
#' @param func Function name
#' @return TRUE if function is PWM-based
#' @noRd
is_pwm_function <- function(func) {
    func %in% c("pwm", "pwm.max", "pwm.max.pos", "pwm.count")
}

#' Check if a vtrack function is kmer-based
#' @param func Function name
#' @return TRUE if function is kmer-based
#' @noRd
is_kmer_function <- function(func) {
    func %in% c("kmer.count", "kmer.frac")
}

#' Check if a vtrack function requires source track
#' @param func Function name
#' @return TRUE if function requires source track
#' @noRd
requires_source_track <- function(func) {
    !is_sequence_function(func) && !is_intervals_function(func)
}

#' Get all vtrack function choices grouped by category
#' @return Named list for selectInput choices
#' @noRd
get_vtrack_function_choices <- function() {
    list(
        "Track Summary" = c("avg", "sum", "min", "max", "stddev", "quantile", "nearest", "size"),
        "Point Values" = c("first", "last", "sample", "exists"),
        "Global Stats" = c("global.percentile", "global.percentile.max", "global.percentile.min"),
        "Position" = c(
            "max.pos.abs", "max.pos.relative", "min.pos.abs", "min.pos.relative",
            "first.pos.abs", "first.pos.relative", "last.pos.abs", "last.pos.relative",
            "sample.pos.abs", "sample.pos.relative"
        ),
        "Intervals" = c("distance", "distance.center", "distance.edge", "coverage", "neighbor.count"),
        "Sequence - PWM" = c("pwm", "pwm.max", "pwm.max.pos", "pwm.count"),
        "Sequence - Kmer" = c("kmer.count", "kmer.frac"),
        "Sequence - Masked" = c("masked.count", "masked.frac")
    )
}

#' Resolve intervals source to data frame
#' @param src Source specification: "@uploaded:name", intervals set name, or file path
#' @return Data frame with chrom, start, end columns
#' @noRd
resolve_intervals_source <- function(src) {
    if (is.null(src) || src == "") {
        stop("Intervals source is required")
    }

    # Check for uploaded intervals
    if (grepl("^@uploaded:", src)) {
        name <- sub("^@uploaded:", "", src)
        intervals <- get_uploaded_intervals_by_name(name)
        if (is.null(intervals)) {
            stop("Uploaded intervals '", name, "' not found")
        }
        return(intervals$data)
    }

    # Check if it's an intervals set in misha
    if (misha::gintervals.exists(src)) {
        return(misha::gintervals.load(src))
    }

    # Check if it's a file path
    if (file.exists(src)) {
        return(parse_intervals_file(src))
    }

    stop("Could not resolve intervals source: ", src)
}

#' Build intervals choices for selectize dropdown
#' @param uploaded_intervals List of uploaded intervals (from reactive value)
#' @return Named list of choices grouped by source
#' @noRd
build_intervals_choices <- function(uploaded_intervals = list()) {
    choices <- list()

    # Add uploaded intervals
    if (length(uploaded_intervals) > 0) {
        uploaded_names <- names(uploaded_intervals)
        uploaded_labels <- sapply(uploaded_names, function(name) {
            item <- uploaded_intervals[[name]]
            paste0(name, " (", item$rows, " rows)")
        })
        names(uploaded_labels) <- paste0("@uploaded:", uploaded_names)
        choices[["Uploaded"]] <- uploaded_labels
    }

    # Add database intervals sets
    db_intervals <- tryCatch(
        misha::gintervals.ls(),
        error = function(e) character(0)
    )
    if (length(db_intervals) > 0) {
        names(db_intervals) <- db_intervals
        choices[["Database"]] <- db_intervals
    }

    if (length(choices) == 0) {
        return(c("No intervals available" = ""))
    }

    choices
}

#' Build PSSM choices for selectize dropdown
#' @param uploaded_pssms List of uploaded PSSMs (from reactive value)
#' @return Named list of choices grouped by source
#' @noRd
build_pssm_choices <- function(uploaded_pssms = list()) {
    choices <- list()

    # Add uploaded PSSMs
    if (length(uploaded_pssms) > 0) {
        uploaded_names <- names(uploaded_pssms)
        uploaded_labels <- sapply(uploaded_names, function(name) {
            item <- uploaded_pssms[[name]]
            paste0(name, " (", item$dimensions, ")")
        })
        names(uploaded_labels) <- paste0("@uploaded:", uploaded_names)
        choices[["Uploaded"]] <- uploaded_labels
    }

    # Add prego motifs (grouped by dataset)
    prego_choices <- get_prego_motif_choices()
    if (length(prego_choices) > 0) {
        for (dataset in names(prego_choices)) {
            motifs <- prego_choices[[dataset]]
            names(motifs) <- motifs
            choices[[paste0("Prego: ", dataset)]] <- motifs
        }
    }

    if (length(choices) == 0) {
        return(c("No PSSMs available" = ""))
    }

    choices
}

#' Resolve PSSM source to matrix
#' @param src Source specification: "@uploaded:name", "prego:motif_name", or file path
#' @return Matrix with A, C, G, T columns
#' @noRd
resolve_pssm_source <- function(src) {
    if (is.null(src) || src == "") {
        stop("PSSM source is required")
    }

    # Check for uploaded PSSM
    if (grepl("^@uploaded:", src)) {
        name <- sub("^@uploaded:", "", src)
        pssm <- get_uploaded_pssm_by_name(name)
        if (is.null(pssm)) {
            stop("Uploaded PSSM '", name, "' not found")
        }
        return(pssm)
    }

    # Check for prego motif (format: "DATASET.motif_name" or explicit "prego:...")
    if (grepl("^prego:", src)) {
        motif_name <- sub("^prego:", "", src)
        return(get_prego_pssm(motif_name))
    }

    # Check if it looks like a prego motif name (contains a dot and no path separators)
    if (grepl("^[A-Za-z]+\\.[A-Za-z0-9_]+$", src) && !grepl("[/\\\\]", src)) {
        # Try as prego motif first
        tryCatch(
            {
                return(get_prego_pssm(src))
            },
            error = function(e) {
                # Fall through to file path check
            }
        )
    }

    # Check if it's a file path
    if (file.exists(src)) {
        return(parse_pssm_file(src))
    }

    stop("Could not resolve PSSM source: ", src)
}
