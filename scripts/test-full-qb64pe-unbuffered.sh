#!/bin/bash
# Test full QB64pe with unbuffered output for real-time progress
# Usage: ./scripts/test-full-qb64pe-unbuffered.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QB64PE_SOURCE="$PROJECT_ROOT/../QB64pe/source/qb64pe.bas"
OUTPUT_FILE="/tmp/qb64pe_full_test.c"

cd "$PROJECT_ROOT"

# Memory limit
ulimit -v 16777216

echo "=========================================="
echo "Testing Full QB64pe (Unbuffered Output)"
echo "=========================================="
echo ""
echo "This will show progress in real-time..."
echo ""

# Use stdbuf to disable buffering (if available)
# This makes output appear immediately instead of being buffered
if command -v stdbuf >/dev/null 2>&1; then
    # -oL = line-buffered stdout, -eL = line-buffered stderr
    stdbuf -oL -eL cargo run --bin qb64fresh -- "$QB64PE_SOURCE" --emit-c -o "$OUTPUT_FILE" --verbose 2>&1
else
    # Fallback: use script to force flushing
    # Rust's println! should be line-buffered when connected to terminal
    # but we'll use --verbose to get progress messages
    cargo run --bin qb64fresh -- "$QB64PE_SOURCE" --emit-c -o "$OUTPUT_FILE" --verbose 2>&1
fi

EXIT_CODE=$?

echo ""
echo "=========================================="
if [ -f "$OUTPUT_FILE" ]; then
    LINES=$(wc -l < "$OUTPUT_FILE")
    echo "✅ Generated: $LINES lines"
else
    echo "❌ No output file"
fi
echo "=========================================="

exit $EXIT_CODE
