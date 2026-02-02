#!/usr/bin/env bash
# Layer 2: run full QB64pe IDE (qb64pe_fresh).
# Run from QB64Fresh repo root: ide_layers/layer2/run.sh
# Or from this dir: ./run.sh
# Build first: ide_layers/layer2/build.sh
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
QB64PE_ROOT="$(cd "$QB64FRESH_ROOT/../QB64pe" && pwd)"

if [[ ! -x "$QB64PE_ROOT/qb64pe_fresh" ]]; then
  echo "qb64pe_fresh not found. Run ide_layers/layer2/build.sh first." >&2
  exit 1
fi

cd "$QB64PE_ROOT"
# Load the runtime we built (layer0/layer1 do this too; without it the wrong lib can load)
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$QB64FRESH_ROOT/target/release"
mkdir -p settings source/settings
touch settings/recent.bin source/settings/recent.bin
if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
  export SDL_VIDEODRIVER=wayland
else
  export SDL_VIDEODRIVER=x11
fi
echo "Starting IDE (qb64pe_fresh)..."
bash -c 'ulimit -v 4194304 && QB64FRESH_IDE_COMPAT=1 ./qb64pe_fresh'
