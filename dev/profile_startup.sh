#!/usr/bin/env bash
# profile_startup.sh -- Wrapper for misha.browser startup profiler
#
# Usage: ./dev/profile_startup.sh [config_yaml] [output_json]
# Defaults: dev/test_config.yaml, dev/startup_baseline.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

CONFIG="${1:-dev/test_config.yaml}"
OUTPUT="${2:-dev/startup_baseline.json}"

cd "$PROJECT_DIR"

echo "misha.browser startup profiler"
echo "Working directory: $PROJECT_DIR"
echo "Config: $CONFIG"
echo "Output: $OUTPUT"
echo "---"

Rscript dev/profile_startup.R "$CONFIG" "$OUTPUT"
