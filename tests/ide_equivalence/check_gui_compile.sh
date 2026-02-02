#!/usr/bin/env bash
# Compile (--emit-c) all GUI IDE equivalence repros with QB64Fresh.
# Exit 0 if all compile; exit 1 and list failures otherwise.
# Run from QB64Fresh repo root.
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
QB64FRESH="${QB64FRESH:-$ROOT/target/release/qb64fresh}"
[ ! -x "$QB64FRESH" ] && QB64FRESH="$ROOT/target/debug/qb64fresh"
if [ ! -x "$QB64FRESH" ]; then
  echo "Error: qb64fresh not found. Build with: cargo build --release --bin qb64fresh" >&2
  exit 1
fi
failed=""
for bas in "$SCRIPT_DIR"/0{1,2,3,4}_*.bas "$SCRIPT_DIR"/07_*.bas; do
  [ -f "$bas" ] || continue
  name="$(basename "$bas" .bas)"
  if ! "$QB64FRESH" "$bas" --emit-c -o /dev/null >/dev/null 2>&1; then
    failed="$failed $name"
  fi
done
if [ -n "$failed" ]; then
  echo "GUI compile failures:$failed" >&2
  exit 1
fi
echo "All GUI IDE equivalence repros compile OK (01–04, 07)."
