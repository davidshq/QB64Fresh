#!/bin/bash
# Test full QB64pe compilation with progress tracking
# Usage: ./scripts/test-full-qb64pe.sh

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
echo "Log: $LOG_FILE"
echo ""
echo "This will take ~5+ minutes..."
echo ""

# Run with timeout, progress, and unbuffered output
# Use stdbuf if available to show progress in real-time
if command -v stdbuf >/dev/null 2>&1; then
    timeout 600 bash -c "
        stdbuf -oL -eL cargo run --bin qb64fresh -- '$QB64PE_SOURCE' --emit-c -o '$OUTPUT_FILE' --verbose 2>&1 | tee '$LOG_FILE'
    " || {
else
    timeout 600 bash -c "
        cargo run --bin qb64fresh -- '$QB64PE_SOURCE' --emit-c -o '$OUTPUT_FILE' --verbose 2>&1 | tee '$LOG_FILE'
    " || {
fi
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
        echo ""
        echo "⚠️  TIMEOUT: Compilation took longer than 10 minutes"
        echo "   This is expected for the full QB64pe (~24K lines)"
        echo "   Check $LOG_FILE for progress"
    else
        echo ""
        echo "❌ Compilation failed with exit code $EXIT_CODE"
        echo "   Check $LOG_FILE for errors"
    fi
    exit $EXIT_CODE
}

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
    ERROR_COUNT=$(grep -c "error" "$LOG_FILE" || echo "0")
    WARNING_COUNT=$(grep -c "warning" "$LOG_FILE" || echo "0")
    echo ""
    echo "Errors: $ERROR_COUNT"
    echo "Warnings: $WARNING_COUNT"
fi
