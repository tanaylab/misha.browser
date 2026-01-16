# Tests for cache functions

test_that("cache_key generates consistent keys", {
    key1 <- cache_key("track1", "chr1", 1000, 2000)
    key2 <- cache_key("track1", "chr1", 1000, 2000)
    key3 <- cache_key("track1", "chr1", 1000, 3000)

    expect_equal(key1, key2)
    expect_false(key1 == key3)
})

test_that("cache_set and cache_get work", {
    browser_clear_cache()

    key <- cache_key("test", 123)
    value <- data.frame(x = 1:3, y = 4:6)

    cache_set(key, value)
    expect_true(cache_exists(key))

    retrieved <- cache_get(key)
    expect_equal(retrieved, value)
})

test_that("cache_get returns NULL for missing keys", {
    browser_clear_cache()

    result <- cache_get("nonexistent_key")
    expect_null(result)
})

test_that("browser_clear_cache clears all entries", {
    key1 <- cache_key("test1")
    key2 <- cache_key("test2")

    cache_set(key1, "value1")
    cache_set(key2, "value2")

    expect_true(cache_exists(key1))
    expect_true(cache_exists(key2))

    browser_clear_cache()

    expect_false(cache_exists(key1))
    expect_false(cache_exists(key2))
})

test_that("cache_stats reports correctly", {
    browser_clear_cache()

    cache_set(cache_key("a"), 1:100)
    cache_set(cache_key("b"), 1:200)

    stats <- cache_stats()
    expect_equal(stats$memory$n_entries, 2)
    expect_true(stats$memory$total_bytes > 0)
})

# =============================================================================
# Tests for cache_key
# =============================================================================

test_that("cache_key handles various input types", {
    # Strings
    key1 <- cache_key("string1", "string2")
    expect_type(key1, "character")
    expect_true(nchar(key1) > 0)

    # Numbers
    key2 <- cache_key(123, 456.789)
    expect_type(key2, "character")

    # Mixed
    key3 <- cache_key("track", 100, 200, list(a = 1, b = 2))
    expect_type(key3, "character")

    # Lists
    key4 <- cache_key(list(x = 1:10, y = "test"))
    expect_type(key4, "character")
})

test_that("cache_key is order sensitive", {
    key1 <- cache_key("a", "b")
    key2 <- cache_key("b", "a")
    expect_false(key1 == key2)
})

test_that("cache_key handles NULL", {
    key <- cache_key(NULL, "test")
    expect_type(key, "character")
})

# =============================================================================
# Tests for LRU eviction (Issue #15, #21)
# =============================================================================

test_that("cache_prune removes oldest entries", {
    browser_clear_cache()

    # Set max entries low for testing
    old_max <- getOption("misha.browser.cache_max_entries", 100)
    options(misha.browser.cache_max_entries = 5)

    # Add entries with some delay to ensure different access times
    for (i in 1:10) {
        cache_set(cache_key("prune_test", i), i)
        Sys.sleep(0.01) # Small delay to differentiate access times
    }

    # Explicitly prune
    cache_prune(max_entries = 5)

    stats <- cache_stats()

    # Should have pruned to around max_entries (timing can cause +/- 1)
    expect_lte(stats$memory$n_entries, 6)

    # Restore option
    options(misha.browser.cache_max_entries = old_max)
    browser_clear_cache()
})

test_that("cache_prune returns correct count", {
    browser_clear_cache()

    # Add some entries
    for (i in 1:5) {
        cache_set(cache_key("count_test", i), i)
        Sys.sleep(0.01)
    }

    # Prune with small max
    pruned <- cache_prune(max_entries = 2, max_bytes = NULL)
    expect_equal(pruned, 3) # Should have pruned 3 entries
})

test_that("cache_prune handles empty cache", {
    browser_clear_cache()
    pruned <- cache_prune(max_entries = 10)
    expect_equal(pruned, 0)
})

test_that("cache_prune handles cache at max", {
    browser_clear_cache()

    # Add exactly max entries
    for (i in 1:5) {
        cache_set(cache_key("max_test", i), i)
    }

    pruned <- cache_prune(max_entries = 5)
    expect_equal(pruned, 0) # Nothing to prune
})

# =============================================================================
# Tests for disk cache (Issue #21 - disk file cleanup)
# =============================================================================

test_that("cache_set writes to disk when enabled", {
    browser_clear_cache()

    # Ensure disk cache is enabled
    old_setting <- getOption("misha.browser.disk_cache", TRUE)
    options(misha.browser.disk_cache = TRUE)

    key <- cache_key("disk_test_write")
    value <- data.frame(x = 1:100, y = 101:200)

    cache_set(key, value)

    # Check disk file exists
    disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
    expect_true(file.exists(disk_path))

    # Clean up
    options(misha.browser.disk_cache = old_setting)
    browser_clear_cache()
})

test_that("cache_get reads from disk when not in memory", {
    browser_clear_cache()

    old_setting <- getOption("misha.browser.disk_cache", TRUE)
    options(misha.browser.disk_cache = TRUE)

    key <- cache_key("disk_test_read")
    value <- data.frame(x = 1:50)

    # Write directly to disk (simulating memory eviction)
    disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
    dir.create(dirname(disk_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(value, disk_path)

    # Should retrieve from disk
    retrieved <- cache_get(key)
    expect_equal(retrieved, value)

    # Clean up
    options(misha.browser.disk_cache = old_setting)
    browser_clear_cache()
})

test_that("cache_exists checks disk when not in memory", {
    browser_clear_cache()

    old_setting <- getOption("misha.browser.disk_cache", TRUE)
    options(misha.browser.disk_cache = TRUE)

    key <- cache_key("disk_test_exists")

    # Write directly to disk
    disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
    dir.create(dirname(disk_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS("test_value", disk_path)

    expect_true(cache_exists(key))

    # Clean up
    options(misha.browser.disk_cache = old_setting)
    browser_clear_cache()
})

test_that("browser_clear_cache removes disk files", {
    old_setting <- getOption("misha.browser.disk_cache", TRUE)
    options(misha.browser.disk_cache = TRUE)

    key <- cache_key("disk_clear_test")
    cache_set(key, "test_value")

    disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
    expect_true(file.exists(disk_path))

    browser_clear_cache(disk = TRUE)

    expect_false(file.exists(disk_path))

    options(misha.browser.disk_cache = old_setting)
})

test_that("browser_clear_cache respects disk=FALSE", {
    old_setting <- getOption("misha.browser.disk_cache", TRUE)
    options(misha.browser.disk_cache = TRUE)

    key <- cache_key("disk_nodelete_test")
    cache_set(key, "test_value")

    disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
    expect_true(file.exists(disk_path))

    # Clear memory only
    browser_clear_cache(disk = FALSE)

    # Disk file should still exist
    expect_true(file.exists(disk_path))

    # Memory should be cleared
    expect_false(exists(key, envir = .browser_cache))

    # Full cleanup
    options(misha.browser.disk_cache = old_setting)
    browser_clear_cache(disk = TRUE)
})

test_that("disk cache handles corrupted files gracefully", {
    old_setting <- getOption("misha.browser.disk_cache", TRUE)
    options(misha.browser.disk_cache = TRUE)
    browser_clear_cache()

    key <- cache_key("corrupted_test")
    disk_path <- file.path(.get_disk_cache_dir(), paste0(key, ".rds"))
    dir.create(dirname(disk_path), recursive = TRUE, showWarnings = FALSE)

    # Write corrupted file
    writeLines("not valid rds data", disk_path)

    # Should return NULL without error
    result <- cache_get(key)
    expect_null(result)

    options(misha.browser.disk_cache = old_setting)
    browser_clear_cache()
})

# =============================================================================
# Tests for with_cache helper
# =============================================================================

test_that("with_cache returns cached value", {
    browser_clear_cache(disk = FALSE)
    key <- cache_key("with_cache_test")

    # First call computes
    call_count <- 0
    result1 <- with_cache(key, function() {
        call_count <<- call_count + 1
        "computed_value"
    })
    expect_equal(result1, "computed_value")
    expect_equal(call_count, 1)

    # Second call returns cached
    result2 <- with_cache(key, function() {
        call_count <<- call_count + 1
        "new_value"
    })
    expect_equal(result2, "computed_value")
    expect_equal(call_count, 1) # Function not called again
})

test_that("with_cache computes on cache miss", {
    browser_clear_cache(disk = FALSE)
    key <- cache_key("with_cache_miss_test")

    result <- with_cache(key, function() {
        list(x = 1:10, y = "test")
    })

    expect_type(result, "list")
    expect_equal(result$y, "test")

    # Value should now be cached
    expect_true(cache_exists(key))
})

# =============================================================================
# Tests for cache_stats
# =============================================================================

test_that("cache_stats returns correct structure", {
    browser_clear_cache()

    # Add at least one entry so stats work properly
    cache_set(cache_key("structure_test"), "value")

    stats <- cache_stats()

    expect_type(stats, "list")
    expect_true("memory" %in% names(stats))
    expect_true("disk" %in% names(stats))

    expect_true("n_entries" %in% names(stats$memory))
    expect_true("total_bytes" %in% names(stats$memory))
    expect_true("n_entries" %in% names(stats$disk))
    expect_true("total_bytes" %in% names(stats$disk))

    browser_clear_cache()
})

test_that("cache_stats calculates bytes correctly", {
    browser_clear_cache()

    # Add a large object
    large_vec <- 1:10000
    cache_set(cache_key("stats_test"), large_vec)

    stats <- cache_stats()

    # Should have one entry with reasonable size
    expect_equal(stats$memory$n_entries, 1)
    expect_gt(stats$memory$total_bytes, 1000) # At least 1KB for 10000 integers
})

# =============================================================================
# Tests for browser_cache_config
# =============================================================================

test_that("browser_cache_config sets custom directory", {
    temp_dir <- tempfile("cache_test")

    browser_cache_config(dir = temp_dir, enabled = TRUE)

    expect_equal(
        getOption("misha.browser.cache_dir"),
        normalizePath(temp_dir, mustWork = FALSE)
    )

    # Reset
    options(misha.browser.cache_dir = NULL)
})

test_that("browser_cache_config enables/disables caching", {
    browser_cache_config(enabled = FALSE)
    expect_false(getOption("misha.browser.disk_cache"))

    browser_cache_config(enabled = TRUE)
    expect_true(getOption("misha.browser.disk_cache"))
})

# =============================================================================
# Tests for LRU access time update
# =============================================================================

test_that("cache_get updates access time", {
    browser_clear_cache()

    key <- cache_key("access_time_test")
    cache_set(key, "value")

    # Get the value to update access time
    Sys.sleep(0.1)
    cache_get(key)

    # Access time should be recent
    access_time <- get(key, envir = .browser_cache_meta)
    expect_true(inherits(access_time, "POSIXct"))
    expect_true(difftime(Sys.time(), access_time, units = "secs") < 1)
})

test_that("cache_set sets access time", {
    browser_clear_cache()

    key <- cache_key("set_time_test")
    cache_set(key, "value")

    # Should have access time
    expect_true(exists(key, envir = .browser_cache_meta))
    access_time <- get(key, envir = .browser_cache_meta)
    expect_true(inherits(access_time, "POSIXct"))
})

# =============================================================================
# Concurrent access tests (basic, since R is single-threaded)
# =============================================================================

test_that("cache handles rapid set/get cycles", {
    browser_clear_cache()

    # Rapid set/get operations
    for (i in 1:100) {
        key <- cache_key("rapid_test", i %% 10) # Reuse some keys
        cache_set(key, i)
        result <- cache_get(key)
        expect_equal(result, i)
    }
})

test_that("cache handles large number of unique keys", {
    browser_clear_cache()

    # Temporarily increase max entries
    old_max <- getOption("misha.browser.cache_max_entries", 100)
    options(misha.browser.cache_max_entries = 200)

    # Add many entries
    for (i in 1:150) {
        cache_set(cache_key("many_keys", i), i)
    }

    stats <- cache_stats()
    expect_gt(stats$memory$n_entries, 100)

    # Restore
    options(misha.browser.cache_max_entries = old_max)
    browser_clear_cache()
})
