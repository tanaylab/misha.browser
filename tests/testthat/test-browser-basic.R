# test-browser-basic.R - Visual / integration tests for the misha.browser Shiny app
#
# These tests launch the app in a background process, connect via headless Chrome
# (chromote), and verify that core UI elements render and navigation works.
#
# Shared session: the app + browser session are launched once and reused across
# all test_that() blocks. Cleanup is registered via withr::defer() on the
# parent (file-level) environment.

# ============================================================================
# Shared session setup
# ============================================================================

# Launch app and connect browser once for all tests in this file.
# We guard everything behind browser_test_available() so the file can be
# sourced safely even when Chrome is not present.

.shared <- new.env(parent = emptyenv())
.shared$bg <- NULL
.shared$session <- NULL
.shared$ready <- FALSE

if (browser_test_available() && file.exists(.TEST_CONFIG)) {
    tryCatch(
        {
            .shared$bg <- launch_browser_bg(
                config_path = .TEST_CONFIG,
                pkg_root = .PKG_ROOT
            )
            .shared$port <- attr(.shared$bg, "port")

            app_ok <- wait_for_app(
                port = .shared$port,
                timeout = 90,
                bg_process = .shared$bg
            )

            if (app_ok) {
                .shared$session <- connect_browser(.shared$port)
                idle_ok <- wait_for_shiny_idle(.shared$session, timeout = 60)
                .shared$ready <- idle_ok
            }
        },
        error = function(e) {
            message("[browser-basic] Setup failed: ", e$message)
        }
    )

    # Register cleanup at file scope so it runs after ALL tests finish
    withr::defer(
        cleanup_browser_test(.shared$session, .shared$bg),
        envir = teardown_env()
    )
}

# Convenience: skip a test if the shared session did not initialise
skip_if_not_ready <- function() {
    skip_if_no_browser()
    if (!.shared$ready || is.null(.shared$session)) {
        skip("Shared browser session is not available")
    }
}

# ============================================================================
# 1. App starts and reaches HTTP 200
# ============================================================================

test_that("app starts and returns HTTP 200", {
    skip_if_no_browser()
    skip_if(!file.exists(.TEST_CONFIG), "Test config not found")

    # The shared setup already confirmed HTTP 200 via wait_for_app();
    # verify independently here with a fresh request.
    resp <- httr::GET(
        paste0("http://127.0.0.1:", .shared$port),
        httr::timeout(10)
    )
    expect_equal(httr::status_code(resp), 200)
})

# ============================================================================
# 2. Shiny WebSocket connects (readyState === 1)
# ============================================================================

test_that("Shiny WebSocket is connected", {
    skip_if_not_ready()

    result <- .shared$session$Runtime$evaluate(
        expression = "
        (function() {
            if (typeof Shiny !== 'undefined' && Shiny.shinyapp && Shiny.shinyapp.$socket) {
                return Shiny.shinyapp.$socket.readyState;
            }
            return -1;
        })()
        "
    )
    # readyState 1 === OPEN
    expect_equal(result$result$value, 1L)
})

# ============================================================================
# 3. Main UI elements render
# ============================================================================

test_that("genome browser plot area exists", {
    skip_if_not_ready()

    # main_plot is the plotOutput id
    expect_true(element_exists(.shared$session, "#main_plot"))
})

test_that("navigation controls exist", {
    skip_if_not_ready()

    # Navigation movement buttons
    expect_true(element_exists(.shared$session, "#mv_left_big"))
    expect_true(element_exists(.shared$session, "#mv_left_small"))
    expect_true(element_exists(.shared$session, "#mv_right_small"))
    expect_true(element_exists(.shared$session, "#mv_right_big"))

    # Zoom buttons
    expect_true(element_exists(.shared$session, "#zoom_in"))
    expect_true(element_exists(.shared$session, "#zoom_out"))

    # History navigation buttons
    expect_true(element_exists(.shared$session, "#history_back"))
    expect_true(element_exists(.shared$session, "#history_forward"))
})

test_that("coordinate display shows a valid genomic region", {
    skip_if_not_ready()

    # current_loc_text is the textOutput id
    expect_true(element_exists(.shared$session, "#current_loc_text"))

    loc_text <- get_element_text(.shared$session, "#current_loc_text")
    expect_true(!is.null(loc_text) && nzchar(loc_text))

    # Should contain "Viewing:" and a coordinate pattern like chrN:start-end
    expect_match(loc_text, "Viewing:", fixed = TRUE)
    expect_match(loc_text, "chr[0-9A-Za-z]+:")
})

test_that("additional UI controls exist", {
    skip_if_not_ready()

    # Coordinate search input
    expect_true(element_exists(.shared$session, "#coord_search"))

    # Brush mode radio buttons
    expect_true(element_exists(.shared$session, "#brush_mode"))

    # Track selection
    expect_true(element_exists(.shared$session, "#track_select"))

    # Extraction mode dropdown
    expect_true(element_exists(.shared$session, "#extraction_mode"))

    # Smooth window numeric input
    expect_true(element_exists(.shared$session, "#smooth_window"))

    # Span input and apply button
    expect_true(element_exists(.shared$session, "#span_input"))
    expect_true(element_exists(.shared$session, "#apply_span"))
})

# ============================================================================
# 4. No JavaScript errors on page
# ============================================================================

test_that("no JavaScript errors on initial load", {
    skip_if_not_ready()

    errors <- get_js_errors(.shared$session)
    expect_length(errors, 0)
})

# ============================================================================
# 5. Screenshot of initial state
# ============================================================================

test_that("screenshot of initial state is captured", {
    skip_if_not_ready()

    path <- take_screenshot(.shared$session, "01_initial_state")
    expect_true(!is.null(path) && file.exists(path))
})

# ============================================================================
# 6. Navigation: zoom in
# ============================================================================

test_that("zoom in changes the view and screenshot is captured", {
    skip_if_not_ready()

    # Record location text before zoom
    loc_before <- get_element_text(.shared$session, "#current_loc_text")

    # Click zoom-in button and wait for idle
    clicked <- click_button(.shared$session, "zoom_in")
    expect_true(clicked)

    idle <- wait_for_shiny_idle(.shared$session, timeout = 60)
    expect_true(idle)

    # Location text should have changed (smaller span after zoom in)
    loc_after <- get_element_text(.shared$session, "#current_loc_text")
    expect_true(!is.null(loc_after) && nzchar(loc_after))
    expect_false(identical(loc_before, loc_after))

    # Take screenshot
    path <- take_screenshot(.shared$session, "02_after_zoom_in")
    expect_true(!is.null(path) && file.exists(path))

    # No errors after interaction
    errors <- get_js_errors(.shared$session)
    expect_length(errors, 0)
})

# ============================================================================
# 7. Navigation: move right
# ============================================================================

test_that("move right changes the view and screenshot is captured", {
    skip_if_not_ready()

    # Record location text before move
    loc_before <- get_element_text(.shared$session, "#current_loc_text")

    # Click move-right-small button and wait for idle
    clicked <- click_button(.shared$session, "mv_right_small")
    expect_true(clicked)

    idle <- wait_for_shiny_idle(.shared$session, timeout = 60)
    expect_true(idle)

    # Location text should have changed (shifted coordinates)
    loc_after <- get_element_text(.shared$session, "#current_loc_text")
    expect_true(!is.null(loc_after) && nzchar(loc_after))
    expect_false(identical(loc_before, loc_after))

    # Take screenshot
    path <- take_screenshot(.shared$session, "03_after_move_right")
    expect_true(!is.null(path) && file.exists(path))

    # No errors after interaction
    errors <- get_js_errors(.shared$session)
    expect_length(errors, 0)
})
