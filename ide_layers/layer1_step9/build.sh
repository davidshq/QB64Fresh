#!/usr/bin/env bash
# Layer 1 step 9: build. Step8 + Batch G (~75% total).
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$SCRIPT_DIR"
(cd "$QB64FRESH_ROOT" && cargo build -p qb64fresh-runtime --release --features graphics-sdl2) >/dev/null 2>&1 || true
gcc -I "$QB64FRESH_ROOT/runtime/include" main.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer1_step9
echo "Built: $SCRIPT_DIR/layer1_step9"
