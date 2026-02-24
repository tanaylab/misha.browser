# Tests for deployment argument validation

# =============================================================================
# Tests for browser_deploy_local argument validation
# =============================================================================

test_that("browser_deploy_local errors when dest_dir is NULL", {
    expect_error(
        browser_deploy_local(dest_dir = NULL),
        "dest_dir must be a non-empty path"
    )
})

test_that("browser_deploy_local errors when dest_dir is empty string", {
    expect_error(
        browser_deploy_local(dest_dir = ""),
        "dest_dir must be a non-empty path"
    )
})

test_that("browser_deploy_local errors when config file does not exist", {
    temp_dir <- tempfile("deploy_test_")
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    expect_error(
        browser_deploy_local(
            dest_dir = temp_dir,
            config = "/nonexistent/path/config.yaml"
        ),
        "Config file not found"
    )
})
