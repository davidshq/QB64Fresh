#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Compiling 03_graphics_image.bas to C..."
cd "$SCRIPT_DIR"
bash -c "ulimit -v 4194304 2>/dev/null || true; exec '$QB64FRESH_ROOT/target/release/qb64fresh' 03_graphics_image.bas --emit-c --runtime external -o 03_graphics_image.c"

echo "Linking layer2d..."
gcc -I "$QB64FRESH_ROOT/runtime/include" 03_graphics_image.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer2d

echo "Built: $SCRIPT_DIR/layer2d"
