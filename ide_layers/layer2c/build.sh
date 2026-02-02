#!/usr/bin/env bash
# Layer 2c: compile IDE-equivalence BASIC (02_event_loop.bas) to C, link with runtime.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Compiling 02_event_loop.bas to C..."
cd "$SCRIPT_DIR"
bash -c "ulimit -v 4194304 2>/dev/null || true; exec '$QB64FRESH_ROOT/target/release/qb64fresh' 02_event_loop.bas --emit-c --runtime external -o 02_event_loop.c"

echo "Linking layer2c..."
gcc -I "$QB64FRESH_ROOT/runtime/include" 02_event_loop.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o layer2c

echo "Built: $SCRIPT_DIR/layer2c"
echo "Run: ide_layers/layer2c/run.sh"
