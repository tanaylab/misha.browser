#!/usr/bin/env bash
# run-profiling.sh - Run all misha.browser profiling scripts
#
# Usage:
#   ./dev/run-profiling.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== misha.browser: Profiling ==="
echo "Package: $PKG_ROOT"
echo ""

cd "$PKG_ROOT"

# --- Startup profiling ---
echo "--- [1/2] Startup Profiling ---"
if [[ -f "$SCRIPT_DIR/profile_startup.R" ]]; then
    Rscript "$SCRIPT_DIR/profile_startup.R"
    echo ""
    echo "Startup profiling complete."
else
    echo "SKIP: dev/profile_startup.R not found." >&2
fi

echo ""

# --- Page-load profiling ---
echo "--- [2/2] Page Load Profiling ---"
if [[ -f "$SCRIPT_DIR/profile_page_loads.R" ]]; then
    Rscript "$SCRIPT_DIR/profile_page_loads.R"
    echo ""
    echo "Page load profiling complete."
else
    echo "SKIP: dev/profile_page_loads.R not found." >&2
fi

echo ""
echo "=== All profiling runs finished ==="
