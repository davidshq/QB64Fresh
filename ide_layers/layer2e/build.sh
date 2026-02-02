#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Compiling 04_font_text.bas to C..."
cd "$SCRIPT_DIR"
bash -c "ulimit -v 4194304 2>/dev/null || true; exec '$QB64FRESH_ROOT/target/release/qb64fresh' 04_font_text.bas --emit-c --runtime external -o 04_font_text.c"

echo "Linking layer2e..."
gcc -I "$QB64FRESH_ROOT/runtime/include" 04_font_text.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer2e

echo "Built: $SCRIPT_DIR/layer2e"
