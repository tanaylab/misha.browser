#!/usr/bin/env bash
# run_benchmarks.sh - Run the misha.browser benchmark suite
#
# Usage:
#   ./benchmarks/run_benchmarks.sh [config] [iterations] [output_json]
#
# Defaults:
#   config:      dev/test_config.yaml
#   iterations:  5
#   output_json: benchmarks/results/benchmark_baseline.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

CONFIG="${1:-${PKG_ROOT}/dev/test_config.yaml}"
ITERATIONS="${2:-5}"
OUTPUT="${3:-${PKG_ROOT}/benchmarks/results/benchmark_baseline.json}"

echo "========================================"
echo " misha.browser benchmark runner"
echo "========================================"
echo "Package root: ${PKG_ROOT}"
echo "Config:       ${CONFIG}"
echo "Iterations:   ${ITERATIONS}"
echo "Output:       ${OUTPUT}"
echo ""

if [ ! -f "${CONFIG}" ]; then
    echo "ERROR: Config file not found: ${CONFIG}" >&2
    exit 1
fi

# Ensure results directory exists
mkdir -p "$(dirname "${OUTPUT}")"

# Run the R benchmark script
Rscript "${SCRIPT_DIR}/benchmark_extract.R" "${CONFIG}" "${ITERATIONS}" "${OUTPUT}"

echo ""
echo "Done. Results: ${OUTPUT}"
