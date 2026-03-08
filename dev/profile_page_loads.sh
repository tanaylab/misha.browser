#!/usr/bin/env bash
# profile_page_loads.sh - Wrapper to run the misha.browser page load profiler
#
# Usage: ./dev/profile_page_loads.sh
#   (Run from the package root, or the script will cd there automatically)

set -euo pipefail

PKG_ROOT="/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/misha.browser"

cd "$PKG_ROOT"

echo "=== misha.browser Page Load Profiler ==="
echo "Started: $(date -Iseconds)"
echo ""

Rscript dev/profile_page_loads.R 2>&1 | tee dev/profile_page_loads.log

EXIT_CODE=${PIPESTATUS[0]}

echo ""
echo "Finished: $(date -Iseconds)"
echo "Log saved to: dev/profile_page_loads.log"

if [ -f dev/page_load_baseline.json ]; then
    echo "Results saved to: dev/page_load_baseline.json"
fi

exit "$EXIT_CODE"
