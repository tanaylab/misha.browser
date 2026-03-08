# helper-browser.R - Headless Chrome testing infrastructure for misha.browser
#
# This file provides reusable functions for browser-based (chromote) testing
# of the misha.browser Shiny app. It is auto-sourced by testthat before any
# test file runs.
#
# Proven working pattern on this server:
#   1. Launch app via callr::r_bg() with devtools::load_all()
#   2. Set CHROMOTE_CHROME env var
#   3. Poll HTTP until 200
#   4. Connect chromote::ChromoteSession$new()
#   5. Set viewport with Emulation$setDeviceMetricsOverride
#   6. Navigate to app URL
#   7. Wait for shiny-busy to clear
#   8. Take screenshots, execute JS, read DOM

# ==============================================================================
# Constants
# ==============================================================================

.CHROME_PATH <- Sys.getenv(
    "CHROMOTE_CHROME",
    "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/.cache/R/chromote/chrome/145.0.7632.67/chrome-headless-shell-linux64/chrome-headless-shell"
)

.PKG_ROOT <- "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/misha.browser"

.SCREENSHOT_DIR <- file.path(.PKG_ROOT, "tests", "testthat", "screenshots")

.TEST_CONFIG <- file.path(.PKG_ROOT, "dev", "test_config.yaml")

.MISHA_ROOT <- "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/db/tgdb/mm10/trackdb"

# Library paths needed by the Chrome binary (installed via conda)
.CHROME_LIB_PATHS <- c(
    "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/tools/miniconda3/lib",
    "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/tools/miniconda3/x86_64-conda-linux-gnu/sysroot/usr/lib64"
)

# ==============================================================================
# Availability Checks
# ==============================================================================

#' Check if browser testing is available
#'
#' Returns TRUE if chromote is installed and the Chrome binary exists.
#' @return logical
browser_test_available <- function() {
    tryCatch(
        {
            has_chromote <- requireNamespace("chromote", quietly = TRUE)
            has_chrome <- file.exists(.CHROME_PATH)
            has_callr <- requireNamespace("callr", quietly = TRUE)
            has_httr <- requireNamespace("httr", quietly = TRUE)
            has_chromote && has_chrome && has_callr && has_httr
        },
        error = function(e) FALSE
    )
}

#' Skip test if browser testing is not available
#'
#' Checks for chromote, Chrome binary, callr, and httr. Also skips on CRAN.
skip_if_no_browser <- function() {
    skip_on_cran()
    if (!requireNamespace("chromote", quietly = TRUE)) {
        skip("chromote package not installed")
    }
    if (!file.exists(.CHROME_PATH)) {
        skip(paste("Chrome binary not found at:", .CHROME_PATH))
    }
    if (!requireNamespace("callr", quietly = TRUE)) {
        skip("callr package not installed")
    }
    if (!requireNamespace("httr", quietly = TRUE)) {
        skip("httr package not installed")
    }
}

# ==============================================================================
# App Launching
# ==============================================================================

#' Launch misha.browser in a background R process
#'
#' Starts the app via callr::r_bg() using devtools::load_all() for source
#' loading, then calls browser_create() + browser_run() to start the server.
#'
#' @param config_path Path to the YAML configuration file
#' @param port Port number for the Shiny server
#' @param pkg_root Path to the misha.browser package root
#' @return The callr process object (with port stored as attribute)
launch_browser_bg <- function(config_path = .TEST_CONFIG,
                              port = NULL,
                              pkg_root = .PKG_ROOT) {
    if (is.null(port)) {
        port <- if (requireNamespace("httpuv", quietly = TRUE)) {
            httpuv::randomPort()
        } else {
            19876L
        }
    }

    tryCatch(
        {
            bg <- callr::r_bg(
                function(config_path, port, pkg_root) {
                    # Load package from source in dev mode
                    devtools::load_all(pkg_root, quiet = TRUE)

                    # Create browser object from config
                    browser <- misha.browser::browser_create(
                        config = config_path,
                        profile = "local"
                    )

                    # Run the app (launch.browser = FALSE for headless testing)
                    misha.browser::browser_run(
                        browser,
                        port = port,
                        host = "127.0.0.1",
                        launch.browser = FALSE
                    )
                },
                args = list(
                    config_path = config_path,
                    port = port,
                    pkg_root = pkg_root
                )
            )

            # Attach the port as an attribute for downstream use
            attr(bg, "port") <- port
            bg
        },
        error = function(e) {
            stop("Failed to launch misha.browser background process: ", e$message)
        }
    )
}

# ==============================================================================
# App Readiness
# ==============================================================================

#' Wait for the Shiny app to become reachable via HTTP
#'
#' Polls the app URL until it returns HTTP 200 or the timeout expires.
#' Also monitors the background process for premature death.
#'
#' @param port Port number the app is running on
#' @param timeout Maximum seconds to wait (default: 60)
#' @param poll_interval Seconds between poll attempts (default: 2)
#' @param bg_process Optional callr process object to monitor for early death
#' @return TRUE if app is ready, FALSE if timeout was reached
wait_for_app <- function(port, timeout = 60, poll_interval = 2, bg_process = NULL) {
    app_url <- paste0("http://127.0.0.1:", port)

    for (elapsed in seq(0, timeout, by = poll_interval)) {
        # Check if the background process died
        if (!is.null(bg_process) && !bg_process$is_alive()) {
            err_out <- tryCatch(bg_process$read_error(), error = function(e) "")
            std_out <- tryCatch(bg_process$read_output(), error = function(e) "")
            warning(
                "misha.browser background process died before becoming ready.\n",
                "Stderr: ", err_out, "\n",
                "Stdout: ", std_out
            )
            return(FALSE)
        }

        resp <- tryCatch(
            httr::GET(app_url, httr::timeout(5)),
            error = function(e) NULL
        )

        if (!is.null(resp) && httr::status_code(resp) == 200) {
            return(TRUE)
        }

        Sys.sleep(poll_interval)
    }

    FALSE
}

# ==============================================================================
# Browser Connection
# ==============================================================================

#' Create a chromote browser session connected to the app
#'
#' Sets CHROMOTE_CHROME, creates a ChromoteSession, configures the viewport,
#' and navigates to the app URL.
#'
#' @param port Port number the app is running on
#' @param width Viewport width in pixels (default: 1400)
#' @param height Viewport height in pixels (default: 900)
#' @return A ChromoteSession object
connect_browser <- function(port, width = 1400, height = 900) {
    # Set Chrome binary path and library paths for its dependencies
    Sys.setenv(CHROMOTE_CHROME = .CHROME_PATH)
    existing_ld_path <- Sys.getenv("LD_LIBRARY_PATH", "")
    new_ld_path <- paste(c(.CHROME_LIB_PATHS, existing_ld_path), collapse = ":")
    Sys.setenv(LD_LIBRARY_PATH = new_ld_path)

    tryCatch(
        {
            # Create a new Chromote browser and session
            browser <- chromote::Chromote$new()
            session <- chromote::ChromoteSession$new(parent = browser)

            # Set viewport dimensions
            session$Emulation$setDeviceMetricsOverride(
                width = width,
                height = height,
                deviceScaleFactor = 1,
                mobile = FALSE
            )

            # Navigate to the app
            app_url <- paste0("http://127.0.0.1:", port)
            session$Page$navigate(app_url)

            # Brief pause to let the initial page load begin
            Sys.sleep(2)

            session
        },
        error = function(e) {
            stop("Failed to connect browser session: ", e$message)
        }
    )
}

# ==============================================================================
# Shiny State Helpers
# ==============================================================================

#' Wait for Shiny to become idle (no shiny-busy class on html element)
#'
#' Polls the html element's classList for the absence of "shiny-busy".
#' This indicates that all reactive outputs have finished rendering.
#'
#' @param session A ChromoteSession object
#' @param timeout Maximum seconds to wait (default: 60)
#' @param poll_interval Seconds between polls (default: 1)
#' @return TRUE if Shiny became idle, FALSE if timeout expired
wait_for_shiny_idle <- function(session, timeout = 60, poll_interval = 1) {
    tryCatch(
        {
            start_time <- Sys.time()

            while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
                result <- tryCatch(
                    session$Runtime$evaluate(
                        expression = "!document.documentElement.classList.contains('shiny-busy')"
                    ),
                    error = function(e) {
                        warning("An error occurred: ", conditionMessage(e))
                        NULL
                    }
                )

                if (!is.null(result) && isTRUE(result$result$value)) {
                    return(TRUE)
                }

                Sys.sleep(poll_interval)
            }

            FALSE
        },
        error = function(e) {
            warning("Error while waiting for Shiny idle: ", e$message)
            FALSE
        }
    )
}

# ==============================================================================
# Screenshot & DOM Inspection
# ==============================================================================

#' Take a screenshot and save it to the screenshots directory
#'
#' Creates the screenshots/ directory if it does not exist.
#' Saves the screenshot as tests/testthat/screenshots/{name}.png.
#'
#' @param session A ChromoteSession object
#' @param name Base name for the screenshot file (without extension)
#' @return The absolute path to the saved screenshot file, or NULL on error
take_screenshot <- function(session, name) {
    tryCatch(
        {
            if (!dir.exists(.SCREENSHOT_DIR)) {
                dir.create(.SCREENSHOT_DIR, recursive = TRUE, showWarnings = FALSE)
            }

            file_path <- file.path(.SCREENSHOT_DIR, paste0(name, ".png"))

            session$screenshot(filename = file_path)

            if (file.exists(file_path)) {
                message("[browser-test] Screenshot saved: ", file_path)
                file_path
            } else {
                warning("Screenshot file was not created: ", file_path)
                NULL
            }
        },
        error = function(e) {
            warning("Failed to take screenshot '", name, "': ", e$message)
            NULL
        }
    )
}

#' Check if an element exists in the DOM
#'
#' @param session A ChromoteSession object
#' @param selector A CSS selector string
#' @return TRUE if at least one matching element exists, FALSE otherwise
element_exists <- function(session, selector) {
    tryCatch(
        {
            safe_selector <- gsub("'", "\\\\'", selector)
            js <- sprintf(
                "document.querySelector('%s') !== null",
                safe_selector
            )

            result <- tryCatch(
                session$Runtime$evaluate(expression = js),
                error = function(e) {
                    # CDP transient error; retry once after a brief pause
                    Sys.sleep(1)
                    tryCatch(
                        session$Runtime$evaluate(expression = js),
                        error = function(e2) NULL
                    )
                }
            )
            if (is.null(result)) return(FALSE)
            isTRUE(result$result$value)
        },
        error = function(e) {
            warning("Failed to check element existence for '", selector, "': ", e$message)
            FALSE
        }
    )
}

#' Get the text content of a DOM element
#'
#' Uses querySelector to find an element and returns its trimmed textContent.
#'
#' @param session A ChromoteSession object
#' @param selector A CSS selector string
#' @return The text content as a string, or NULL if the element was not found
get_element_text <- function(session, selector) {
    tryCatch(
        {
            safe_selector <- gsub("'", "\\\\'", selector)

            js <- sprintf(
                "
                (function() {
                    var el = document.querySelector('%s');
                    return el ? el.textContent.trim() : null;
                })()
                ",
                safe_selector
            )

            result <- tryCatch(
                session$Runtime$evaluate(expression = js),
                error = function(e) {
                    # CDP transient error; retry once
                    Sys.sleep(2)
                    tryCatch(
                        session$Runtime$evaluate(expression = js),
                        error = function(e2) NULL
                    )
                }
            )

            if (is.null(result)) return(NULL)
            result$result$value
        },
        error = function(e) {
            warning("Error getting element text for '", selector, "': ", e$message)
            NULL
        }
    )
}

#' Wait for a DOM element to appear
#'
#' Polls until an element matching the selector exists in the DOM,
#' or until the timeout expires.
#'
#' @param session A ChromoteSession object
#' @param selector A CSS selector string
#' @param timeout Maximum seconds to wait (default: 15)
#' @param poll_interval Seconds between polls (default: 0.5)
#' @return TRUE if the element appeared, FALSE if timeout expired
wait_for_element <- function(session, selector, timeout = 15, poll_interval = 0.5) {
    tryCatch(
        {
            safe_selector <- gsub("'", "\\\\'", selector)
            js <- sprintf(
                "document.querySelector('%s') !== null",
                safe_selector
            )

            start_time <- Sys.time()

            while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
                result <- tryCatch(
                    session$Runtime$evaluate(expression = js),
                    error = function(e) {
                        # CDP transient error; retry once
                        Sys.sleep(1)
                        tryCatch(
                            session$Runtime$evaluate(expression = js),
                            error = function(e2) NULL
                        )
                    }
                )

                if (!is.null(result) && isTRUE(result$result$value)) {
                    return(TRUE)
                }

                Sys.sleep(poll_interval)
            }

            FALSE
        },
        error = function(e) {
            warning("Error waiting for element '", selector, "': ", e$message)
            FALSE
        }
    )
}

#' Check the browser console for JavaScript errors
#'
#' Checks for common Shiny error indicators in the DOM (notification errors,
#' output error classes).
#'
#' @param session A ChromoteSession object
#' @return A character vector of error messages, or character(0) if none
get_js_errors <- function(session) {
    tryCatch(
        {
            js_dom_errors <- "
            (function() {
                var errors = [];
                // Check for Shiny notification errors
                var notifications = document.querySelectorAll('.shiny-notification-error, .shiny-output-error');
                for (var i = 0; i < notifications.length; i++) {
                    var text = notifications[i].textContent.trim();
                    if (text) errors.push(text);
                }
                return errors;
            })()
            "

            result <- tryCatch(
                session$Runtime$evaluate(
                    expression = js_dom_errors,
                    returnByValue = TRUE
                ),
                error = function(e) NULL
            )

            if (!is.null(result$result$value)) {
                errors <- unlist(result$result$value)
                if (length(errors) > 0) {
                    return(as.character(errors))
                }
            }

            character(0)
        },
        error = function(e) {
            warning("Failed to check JS errors: ", e$message)
            character(0)
        }
    )
}

# ==============================================================================
# Interaction Helpers
# ==============================================================================

#' Set a Shiny input value via JavaScript
#'
#' Uses Shiny.setInputValue() to programmatically set an input, then waits
#' for Shiny to become idle.
#'
#' @param session A ChromoteSession object
#' @param input_id The Shiny input ID to set
#' @param value The value to set (will be JSON-encoded for non-scalar types)
#' @return TRUE if the value was set successfully, FALSE otherwise
set_shiny_input <- function(session, input_id, value) {
    tryCatch(
        {
            value_js <- jsonlite::toJSON(value, auto_unbox = TRUE)

            js <- sprintf(
                "
                (function() {
                    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return false;
                    Shiny.setInputValue('%s', %s, {priority: 'event'});
                    return true;
                })()
                ",
                input_id, value_js
            )

            result <- tryCatch(
                session$Runtime$evaluate(expression = js),
                error = function(e) {
                    # CDP transient error; retry once
                    Sys.sleep(2)
                    tryCatch(
                        session$Runtime$evaluate(expression = js),
                        error = function(e2) list(result = list(value = FALSE))
                    )
                }
            )

            if (!isTRUE(result$result$value)) {
                warning("Failed to set Shiny input '", input_id, "'")
                return(FALSE)
            }

            Sys.sleep(0.5)
            wait_for_shiny_idle(session)
        },
        error = function(e) {
            warning("Error setting Shiny input '", input_id, "': ", e$message)
            FALSE
        }
    )
}

#' Click a Shiny action button
#'
#' Finds the button by its ID and clicks it via JavaScript, then waits
#' for Shiny to become idle.
#'
#' @param session A ChromoteSession object
#' @param button_id The Shiny button ID to click
#' @return TRUE if the button was clicked, FALSE otherwise
click_button <- function(session, button_id) {
    tryCatch(
        {
            js <- sprintf(
                "
                (function() {
                    var btn = document.getElementById('%s');
                    if (btn) {
                        btn.click();
                        return true;
                    }
                    return false;
                })()
                ",
                button_id
            )

            result <- tryCatch(
                session$Runtime$evaluate(expression = js),
                error = function(e) {
                    # CDP transient error; retry once
                    Sys.sleep(2)
                    tryCatch(
                        session$Runtime$evaluate(expression = js),
                        error = function(e2) list(result = list(value = FALSE))
                    )
                }
            )

            if (!isTRUE(result$result$value)) {
                warning("Button '", button_id, "' not found")
                return(FALSE)
            }

            Sys.sleep(0.5)
            wait_for_shiny_idle(session)
        },
        error = function(e) {
            warning("Error clicking button '", button_id, "': ", e$message)
            FALSE
        }
    )
}

# ==============================================================================
# Cleanup
# ==============================================================================

#' Orderly cleanup of browser session and background process
#'
#' Closes the chromote session and kills the background R process.
#' Handles errors gracefully so that cleanup does not mask test failures.
#'
#' @param session A ChromoteSession object (or NULL)
#' @param bg_process A callr process object (or NULL)
#' @return Invisible NULL
cleanup_browser_test <- function(session = NULL, bg_process = NULL) {
    # Close the chromote session
    if (!is.null(session)) {
        tryCatch(
            {
                session$close()
            },
            error = function(e) {
                # Session may already be closed
            }
        )
    }

    # Kill the background process
    if (!is.null(bg_process)) {
        tryCatch(
            {
                if (bg_process$is_alive()) {
                    bg_process$kill()
                }
            },
            error = function(e) {
                # Process may already be dead
            }
        )
    }

    invisible(NULL)
}
