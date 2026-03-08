#!/usr/bin/env bash
# run-browser-tests.sh - Run misha.browser headless Chrome tests
#
# Usage:
#   ./dev/run-browser-tests.sh              # Run all browser tests
#   ./dev/run-browser-tests.sh --verbose    # Verbose output
#   ./dev/run-browser-tests.sh --filter smoke  # Custom filter pattern

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Chrome binary (matches helper-browser.R .CHROME_PATH)
export CHROMOTE_CHROME="/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/.cache/R/chromote/chrome/145.0.7632.67/chrome-headless-shell-linux64/chrome-headless-shell"

# Conda lib paths needed by Chrome (matches helper-browser.R .CHROME_LIB_PATHS)
CHROME_LIB_PATHS="/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/tools/miniconda3/lib:/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/tools/miniconda3/x86_64-conda-linux-gnu/sysroot/usr/lib64"
export LD_LIBRARY_PATH="${CHROME_LIB_PATHS}${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# Ensure browser tests are not skipped
export NOT_CRAN=true

# Defaults
FILTER="browser"
VERBOSE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --verbose|-v)
            VERBOSE="TRUE"
            shift
            ;;
        --filter|-f)
            FILTER="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --verbose, -v          Verbose test output"
            echo "  --filter, -f PATTERN   Test file filter (default: browser)"
            echo "  --help, -h             Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

echo "=== misha.browser: Browser Tests ==="
echo "Chrome:    $CHROMOTE_CHROME"
echo "Filter:    $FILTER"
echo "Verbose:   ${VERBOSE:-FALSE}"
echo "Package:   $PKG_ROOT"
echo ""

# Verify Chrome binary exists
if [[ ! -x "$CHROMOTE_CHROME" ]]; then
    echo "ERROR: Chrome binary not found or not executable at:" >&2
    echo "  $CHROMOTE_CHROME" >&2
    exit 1
fi

cd "$PKG_ROOT"

if [[ -n "$VERBOSE" ]]; then
    Rscript -e "devtools::test(filter = '${FILTER}', reporter = 'summary')"
else
    Rscript -e "devtools::test(filter = '${FILTER}')"
fi
