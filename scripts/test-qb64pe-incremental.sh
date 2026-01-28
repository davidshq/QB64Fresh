#!/bin/bash
# Test QB64pe incremental compilation
# Usage: ./scripts/test-qb64pe-incremental.sh [phase]
#   phase: 1-5 (default: all phases in order)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEST_DIR="$PROJECT_ROOT/tests/qb64pe_incremental"

cd "$PROJECT_ROOT"

# Memory limit (16GB)
ulimit -v 16777216

PHASE="${1:-all}"

test_phase() {
    local phase=$1
    local file="$TEST_DIR/0${phase}_*.bas"
    
    echo "=========================================="
    echo "Testing Phase $phase"
    echo "=========================================="
    
    for test_file in $file; do
        if [ -f "$test_file" ]; then
            echo ""
            echo "Testing: $(basename "$test_file")"
            echo "----------------------------------------"
            time cargo run --bin qb64fresh -- "$test_file" --emit-c -o /tmp/$(basename "$test_file" .bas).c 2>&1 | head -50
            if [ ${PIPESTATUS[0]} -eq 0 ]; then
                echo "✓ PASSED: $(basename "$test_file")"
            else
                echo "✗ FAILED: $(basename "$test_file")"
                return 1
            fi
        fi
    done
    return 0
}

case "$PHASE" in
    1|2|3|4|5)
        test_phase "$PHASE"
        ;;
    all)
        echo "Running all incremental tests..."
        echo ""
        for phase in 1 2 3 4; do
            if ! test_phase "$phase"; then
                echo ""
                echo "Phase $phase failed. Fix errors before continuing."
                exit 1
            fi
            echo ""
        done
        echo "=========================================="
        echo "All incremental tests passed!"
        echo "=========================================="
        echo ""
        echo "To test full compiler (Phase 5), run:"
        echo "  bash -c 'ulimit -v 16777216 && cargo run --bin qb64fresh -- tests/qb64pe_incremental/05_full_compiler.bas --emit-c -o /tmp/qb64pe_full.c &'"
        ;;
    *)
        echo "Usage: $0 [1|2|3|4|5|all]"
        echo ""
        echo "Phases:"
        echo "  1 - Core infrastructure (~0.5s)"
        echo "  2 - Utilities (~1s each)"
        echo "  3 - Built-in functions (~5s)"
        echo "  4 - Core compiler without IDE (~10s)"
        echo "  5 - Full compiler (~5+ min) - run manually"
        echo "  all - Run phases 1-4 in order"
        exit 1
        ;;
esac
