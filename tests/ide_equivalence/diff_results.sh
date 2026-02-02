#!/usr/bin/env bash
# Compare results/fresh vs results/qb64pe for console-only IDE equivalence tests.
# Run after run_ide_equivalence.sh (with QB64pe) to see output differences.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS="$SCRIPT_DIR/results"
fresh="$RESULTS/fresh"
pe="$RESULTS/qb64pe"
if [ ! -d "$fresh" ] || [ ! -d "$pe" ]; then
  echo "Run run_ide_equivalence.sh first. Need both results/fresh and results/qb64pe." >&2
  exit 1
fi
echo "=== IDE equivalence: output differences (QB64Fresh vs QB64pe) ==="
for f in "$fresh"/*.txt; do
  [ -f "$f" ] || continue
  name="$(basename "$f" .txt)"
  q="$pe/$name.txt"
  if [ ! -f "$q" ]; then continue; fi
  if grep -q "^SKIP_QB64PE\|^FAIL_\|^TIMEOUT" "$f" "$q" 2>/dev/null; then continue; fi
  if ! diff -q "$f" "$q" >/dev/null 2>&1; then
    echo "--- $name ---"
    diff -u "$q" "$f" 2>/dev/null || true
    echo ""
  fi
done
echo "=== Done ==="
