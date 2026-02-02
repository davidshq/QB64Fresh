#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Building runtime if needed..."
cd "$QB64FRESH_ROOT"
cargo build -p qb64fresh-runtime --release --features graphics-sdl2 2>&1 | tail -5

echo "Compiling layer2a_debug..."
cd "$SCRIPT_DIR"
gcc -I "$QB64FRESH_ROOT/runtime/include" main.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer2a_debug

echo "Built: $SCRIPT_DIR/layer2a_debug"
echo "Run: ide_layers/layer2a_debug/run.sh"
