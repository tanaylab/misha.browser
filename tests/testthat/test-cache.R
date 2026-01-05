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
