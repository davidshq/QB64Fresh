#!/usr/bin/env bash
# Layer 1 step 7 bisect: build (step6 + Batch B only).
# Run from QB64Fresh repo root: ide_layers/layer1_step7_bisect/build.sh
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$SCRIPT_DIR"
(cd "$QB64FRESH_ROOT" && cargo build -p qb64fresh-runtime --release --features graphics-sdl2) >/dev/null 2>&1 || true
gcc -I "$QB64FRESH_ROOT/runtime/include" main.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer1_step7_bisect
echo "Built: $SCRIPT_DIR/layer1_step7_bisect"
