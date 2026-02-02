#!/usr/bin/env bash
# Layer 2: build full QB64pe IDE (qb64pe_fresh).
# Run from QB64Fresh repo root: ide_layers/layer2/build.sh
# Or from this dir: ./build.sh
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
QB64PE_ROOT="$(cd "$QB64FRESH_ROOT/../QB64pe" && pwd)"
QB64PE_SOURCE="$QB64PE_ROOT/source"

echo "Building QB64Fresh and runtime..."
cd "$QB64FRESH_ROOT"
cargo build --release
cargo build -p qb64fresh-runtime --release --features graphics-sdl2

echo "Compiling qb64pe.bas to C (may take several minutes)..."
cd "$QB64PE_SOURCE"
# Try to cap virtual memory (16GB); ignore if not permitted (e.g. in containers/CI)
bash -c "ulimit -v 16777216 2>/dev/null || true; exec '$QB64FRESH_ROOT/target/release/qb64fresh' qb64pe.bas --emit-c --runtime external -o qb64pe_fresh.c"

echo "Linking qb64pe_fresh..."
gcc -I "$QB64FRESH_ROOT/runtime/include" qb64pe_fresh.c \
  -L "$QB64FRESH_ROOT/target/release" -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client 2>/dev/null) \
  -lm -lpthread -ldl -o "$QB64PE_ROOT/qb64pe_fresh"

echo "Built: $QB64PE_ROOT/qb64pe_fresh"
echo "Run: ide_layers/layer2/run.sh (or cd $QB64PE_ROOT && ./run_qb64pe_fresh.sh)"
