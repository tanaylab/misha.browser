#!/usr/bin/env bash
# dev/start_app.sh - Development launcher for misha.browser
#
# Usage: ./dev/start_app.sh [config_yaml] [port] [host] [profile]
#
# Defaults:
#   config_yaml = dev/test_config.yaml
#   port        = 3838
#   host        = 0.0.0.0
#   profile     = local

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

CONFIG="${1:-${SCRIPT_DIR}/test_config.yaml}"
PORT="${2:-3838}"
HOST="${3:-0.0.0.0}"
PROFILE="${4:-local}"

# Resolve config path relative to project root if not absolute
if [[ "$CONFIG" != /* ]]; then
    CONFIG="${PROJECT_DIR}/${CONFIG}"
fi

if [[ ! -f "$CONFIG" ]]; then
    echo "Error: Configuration file not found: $CONFIG" >&2
    exit 1
fi

echo "=== misha.browser dev launcher ==="
echo "Project:  $PROJECT_DIR"
echo "Config:   $CONFIG"
echo "Port:     $PORT"
echo "Host:     $HOST"
echo "Profile:  $PROFILE"
echo ""

cd "$PROJECT_DIR"

Rscript -e "
devtools::load_all('${PROJECT_DIR}', quiet = TRUE)
misha.browser::browser_launch(
    config  = '${CONFIG}',
    port    = ${PORT}L,
    host    = '${HOST}',
    profile = '${PROFILE}'
)
"
