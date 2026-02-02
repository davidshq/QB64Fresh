#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [[ ! -x "$SCRIPT_DIR/layer2b" ]]; then
  echo "layer2b not found. Run build.sh first." >&2
  exit 1
fi

cd "$SCRIPT_DIR"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$QB64FRESH_ROOT/target/release"
if [[ -z "${SDL_VIDEODRIVER:-}" ]]; then
  if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then export SDL_VIDEODRIVER=wayland; else export SDL_VIDEODRIVER=x11; fi
fi
./layer2b
