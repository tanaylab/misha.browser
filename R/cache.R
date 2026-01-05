# cache.R - Caching utilities for misha.browser

# Global cache environment (memory cache)
.browser_cache <- new.env(parent = emptyenv())

# LRU metadata: track access times for cache eviction
.browser_cache_meta <- new.env(parent = emptyenv())

# Disk cache directory (set via option or default)
.get_disk_cache_dir <- function() {
    dir <- getOption("misha.browser.cache_dir", NULL)
    if (is.null(dir)) {
        dir <- file.path(tempdir(), "misha_browser_cache")
    }
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    dir
}

#' Configure disk caching
#'
#' Enable or configure disk-based caching for the browser.
#'
#' @param dir Directory for disk cache. NULL to use temp directory.
#' @param enabled Whether disk caching is enabled (default: TRUE when dir is set)
#' @export
#' @examples
#' \dontrun{
#' # Enable disk caching with custom directory
#' browser_cache_config(dir = "~/.misha_browser_cache")
#'
#' # Disable disk caching
#' browser_cache_config(enabled = FALSE)
#' }
browser_cache_config <- function(dir = NULL, enabled = TRUE) {
    if (!is.null(dir)) {
        options(misha.browser.cache_dir = normalizePath(dir, mustWork = FALSE))
    }
    options(misha.browser.disk_cache = enabled)
    invisible(NULL)
}

#' Generate cache key
#'
#' Creates an MD5 hash key for caching extracted data.
#'
#' @param ... Objects to hash
#' @return MD5 hash string
#' @keywords internal
cache_key <- function(...) {
    digest::digest(list(...), algo = "md5")
}

#' Get value from cache (memory first, then disk)
#'
#' @param key Cache key
#' @return Cached value or NULL if not found
#' @keywords internal
cache_get <- function(key) {
    # Check memory cache first
    if (exists(key, envir = .browser_cache)) {
        # Update LRU access time
        assign(key, Sys.time(), envir = .browser_cache_meta)
        return(get(key, envir = .browser_cache))
    }

    # Check disk cache if enabled
    if (isTRUE(getOption("misha.browser.disk_cache", TRUE))) {
        disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
        if (file.exists(disk_path)) {
            tryCatch(
                {
                    value <- readRDS(disk_path)
                    # Promote to memory cache
                    assign(key, value, envir = .browser_cache)
                    return(value)
                },
                error = function(e) NULL
            )
        }
    }

    NULL
}

#' Set value in cache (memory and optionally disk)
#'
#' @param key Cache key
#' @param value Value to cache
#' @return Invisibly returns the value
#' @keywords internal
cache_set <- function(key, value) {
    # Prune cache if it's getting too large
    cache_prune()

    # Always set in memory
    assign(key, value, envir = .browser_cache)

    # Track LRU access time
    assign(key, Sys.time(), envir = .browser_cache_meta)

    # Optionally write to disk
    if (isTRUE(getOption("misha.browser.disk_cache", TRUE))) {
        disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
        tryCatch(
            {
                saveRDS(value, disk_path)
            },
            error = function(e) {
                cli::cli_warn("Failed to write disk cache: {e$message}")
            }
        )
    }

    invisible(value)
}

#' Check if key exists in cache (memory or disk)
#'
#' @param key Cache key
#' @return TRUE if key exists
#' @keywords internal
cache_exists <- function(key) {
    # Check memory first
    if (exists(key, envir = .browser_cache)) {
        return(TRUE)
    }

    # Check disk if enabled
    if (isTRUE(getOption("misha.browser.disk_cache", TRUE))) {
        disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
        if (file.exists(disk_path)) {
            return(TRUE)
        }
    }

    FALSE
}

#' Prune cache to stay within limits (LRU eviction)
#'
#' Removes least-recently-used entries when cache exceeds size limits.
#'
#' @param max_entries Maximum number of entries to keep (default: 100)
#' @param max_bytes Maximum total bytes to keep (default: 500MB)
#' @return Invisibly returns number of entries pruned
#' @keywords internal
cache_prune <- function(max_entries = getOption("misha.browser.cache_max_entries", 100),
                        max_bytes = getOption("misha.browser.cache_max_bytes", 500e6)) {
    keys <- ls(.browser_cache)
    n_entries <- length(keys)

    if (n_entries <= max_entries) {
        return(invisible(0L))
    }

    # Get access times for all keys
    access_times <- sapply(keys, function(k) {
        if (exists(k, envir = .browser_cache_meta)) {
            get(k, envir = .browser_cache_meta)
        } else {
            # Never accessed - oldest possible
            as.POSIXct("1970-01-01")
        }
    })

    # Sort by access time (oldest first)
    sorted_keys <- keys[order(access_times)]

    # Calculate how many to remove
    n_to_remove <- n_entries - max_entries

    # Also check byte size
    if (!is.null(max_bytes)) {
        total_bytes <- sum(sapply(keys, function(k) {
            object.size(get(k, envir = .browser_cache))
        }))

        if (total_bytes > max_bytes) {
            # Need to remove more - remove oldest until under limit
            cumulative_size <- 0
            for (i in seq_along(sorted_keys)) {
                k <- sorted_keys[i]
                cumulative_size <- cumulative_size + object.size(get(k, envir = .browser_cache))
                if ((total_bytes - cumulative_size) <= max_bytes && i >= n_to_remove) {
                    n_to_remove <- i
                    break
                }
            }
        }
    }

    # Remove oldest entries
    if (n_to_remove > 0) {
        to_remove <- sorted_keys[seq_len(n_to_remove)]
        for (k in to_remove) {
            if (exists(k, envir = .browser_cache)) {
                rm(list = k, envir = .browser_cache)
            }
            if (exists(k, envir = .browser_cache_meta)) {
                rm(list = k, envir = .browser_cache_meta)
            }
        }
    }

    invisible(n_to_remove)
}

#' Clear the entire cache (memory and optionally disk)
#'
#' @param disk Also clear disk cache (default: TRUE)
#' @export
browser_clear_cache <- function(disk = TRUE) {
    # Clear memory cache
    rm(list = ls(.browser_cache), envir = .browser_cache)

    # Clear LRU metadata
    rm(list = ls(.browser_cache_meta), envir = .browser_cache_meta)

    # Clear disk cache if requested
    if (disk && isTRUE(getOption("misha.browser.disk_cache", TRUE))) {
        cache_dir <- .get_disk_cache_dir()
        if (dir.exists(cache_dir)) {
            files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
            file.remove(files)
            cli::cli_alert_success("Cleared {length(files)} disk cache entries")
        }
    }

    cli::cli_alert_success("Browser cache cleared")
    invisible(NULL)
}

#' Get cache statistics
#'
#' @return List with cache size info
#' @keywords internal
cache_stats <- function() {
    # Memory cache stats
    keys <- ls(.browser_cache)
    mem_sizes <- sapply(keys, function(k) {
        object.size(get(k, envir = .browser_cache))
    })

    # Disk cache stats
    disk_entries <- 0
    disk_bytes <- 0
    if (isTRUE(getOption("misha.browser.disk_cache", TRUE))) {
        cache_dir <- .get_disk_cache_dir()
        if (dir.exists(cache_dir)) {
            files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
            disk_entries <- length(files)
            disk_bytes <- sum(file.size(files))
        }
    }

    list(
        memory = list(
            n_entries = length(keys),
            total_bytes = sum(mem_sizes)
        ),
        disk = list(
            n_entries = disk_entries,
            total_bytes = disk_bytes,
            dir = if (isTRUE(getOption("misha.browser.disk_cache", TRUE))) .get_disk_cache_dir() else NULL
        )
    )
}

#' Execute with caching
#'
#' Returns cached value if exists, otherwise computes and caches.
#'
#' @param key Cache key
#' @param compute_fn Function to compute value if not cached
#' @return Cached or computed value
#' @keywords internal
with_cache <- function(key, compute_fn) {
    if (cache_exists(key)) {
        return(cache_get(key))
    }
    result <- compute_fn()
    cache_set(key, result)
    result
}
