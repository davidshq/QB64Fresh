#!/usr/bin/env bash
# Layer 2b: compile graphics-focused BASIC to C, link with runtime.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Building QB64Fresh and runtime..."
cd "$QB64FRESH_ROOT"
cargo build --release 2>&1 | tail -3
cargo build -p qb64fresh-runtime --release --features graphics-sdl2 2>&1 | tail -3

echo "Compiling 01_window_graphics.bas to C..."
cd "$SCRIPT_DIR"
bash -c "ulimit -v 4194304 2>/dev/null || true; exec '$QB64FRESH_ROOT/target/release/qb64fresh' 01_window_graphics.bas --emit-c --runtime external -o 01_window_graphics.c"

echo "Generated C code snippet:"
head -100 01_window_graphics.c | tail -30

echo ""
echo "Linking layer2b..."
gcc -I "$QB64FRESH_ROOT/runtime/include" 01_window_graphics.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer2b

echo "Built: $SCRIPT_DIR/layer2b"
echo "Run: ide_layers/layer2b/run.sh"
