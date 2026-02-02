#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
if [[ ! -f layer1_step9 ]]; then echo "Run build.sh first." >&2; exit 1; fi
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$(cd "$SCRIPT_DIR/../.." && pwd)/target/release"
if [[ -z "${SDL_VIDEODRIVER:-}" ]]; then
  if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then export SDL_VIDEODRIVER=wayland; else export SDL_VIDEODRIVER=x11; fi
fi
./layer1_step9
