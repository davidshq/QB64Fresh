#!/usr/bin/env bash
# Layer 2a: compile IDE-equivalence BASIC (01_window_display.bas) to C, link with runtime.
# Run from QB64Fresh repo root: ide_layers/layer2a/build.sh
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Building QB64Fresh and runtime..."
cd "$QB64FRESH_ROOT"
cargo build --release
cargo build -p qb64fresh-runtime --release --features graphics-sdl2

echo "Compiling 01_window_display.bas to C..."
cd "$SCRIPT_DIR"
bash -c "ulimit -v 4194304 2>/dev/null || true; exec '$QB64FRESH_ROOT/target/release/qb64fresh' 01_window_display.bas --emit-c --runtime external -o 01_window_display.c"

echo "Linking layer2a..."
gcc -I "$QB64FRESH_ROOT/runtime/include" 01_window_display.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer2a

echo "Built: $SCRIPT_DIR/layer2a"
echo "Run: ide_layers/layer2a/run.sh"
