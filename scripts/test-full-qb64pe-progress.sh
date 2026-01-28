#!/bin/bash
# Test full QB64pe compilation with real-time progress output
# Usage: ./scripts/test-full-qb64pe-progress.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QB64PE_SOURCE="$PROJECT_ROOT/../QB64pe/source/qb64pe.bas"
OUTPUT_FILE="/tmp/qb64pe_full_test.c"
LOG_FILE="/tmp/qb64pe_full_test.log"

cd "$PROJECT_ROOT"

# Memory limit (16GB)
ulimit -v 16777216

echo "=========================================="
echo "Testing Full QB64pe Compilation"
echo "=========================================="
echo ""
echo "Source: $QB64PE_SOURCE"
echo "Output: $OUTPUT_FILE"
echo ""
echo "Using unbuffered output for real-time progress..."
echo ""

# Use stdbuf to disable buffering, or set RUST_BACKTRACE for more output
# Also use --verbose flag if available
export RUST_BACKTRACE=0

# Run with unbuffered output using stdbuf (if available) or python -u
if command -v stdbuf >/dev/null 2>&1; then
    # Use stdbuf to disable line buffering
    stdbuf -oL -eL cargo run --bin qb64fresh -- "$QB64PE_SOURCE" --emit-c -o "$OUTPUT_FILE" --verbose 2>&1 | tee "$LOG_FILE"
else
    # Fallback: use python -u for unbuffered output, or just run normally
    # Note: Rust's println! is line-buffered by default when connected to a terminal
    # but may be fully buffered when redirected. We'll use --verbose and hope for the best.
    cargo run --bin qb64fresh -- "$QB64PE_SOURCE" --emit-c -o "$OUTPUT_FILE" --verbose 2>&1 | tee "$LOG_FILE"
fi

EXIT_CODE=${PIPESTATUS[0]}

echo ""
echo "=========================================="
if [ -f "$OUTPUT_FILE" ]; then
    OUTPUT_SIZE=$(wc -l < "$OUTPUT_FILE")
    echo "✅ SUCCESS: Generated $OUTPUT_SIZE lines of C code"
    echo "   Output: $OUTPUT_FILE"
else
    echo "❌ FAILED: No output file generated"
    echo "   Check $LOG_FILE for errors"
fi
echo "=========================================="

# Count errors
if [ -f "$LOG_FILE" ]; then
    ERROR_COUNT=$(grep -c "error" "$LOG_FILE" 2>/dev/null || echo "0")
    WARNING_COUNT=$(grep -c "warning" "$LOG_FILE" 2>/dev/null || echo "0")
    echo ""
    echo "Errors: $ERROR_COUNT"
    echo "Warnings: $WARNING_COUNT"
fi

exit $EXIT_CODE
