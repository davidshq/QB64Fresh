#!/bin/bash
# Test all major sections of QB64pe incrementally
# Usage: ./scripts/test-all-qb64pe-sections.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEST_DIR="$PROJECT_ROOT/tests/qb64pe_incremental"
SECTIONS_DIR="$TEST_DIR/sections"

cd "$PROJECT_ROOT"

# Memory limit
ulimit -v 16777216

echo "=========================================="
echo "Testing All QB64pe Sections Incrementally"
echo "=========================================="
echo ""

# Define sections to test
declare -a SECTIONS=(
    "idstruct_type:596:642"
    "ids_init:644:656"
    "clearid_sub:14476:14478"
    "regid_sub:21849:22081"
    "usedVarList_type:181:188"
    "Label_Type:468:475"
)

PASSED=0
FAILED=0
SKIPPED=0

for section in "${SECTIONS[@]}"; do
    IFS=':' read -r name start end <<< "$section"
    
    echo "Testing: $name (lines $start-$end)"
    echo "----------------------------------------"
    
    # Extract if not exists
    if [ ! -f "$SECTIONS_DIR/${name}.bas" ]; then
        echo "Extracting section..."
        ./scripts/extract-qb64pe-section.sh "$name" "$start" "$end" > /dev/null 2>&1
    fi
    
    # Test
    if cargo run --bin qb64fresh -- "$SECTIONS_DIR/${name}.bas" --emit-c 2>&1 | grep -q "Generated"; then
        echo "✅ PASSED: $name"
        ((PASSED++))
    else
        echo "❌ FAILED: $name"
        ((FAILED++))
    fi
    echo ""
done

echo "=========================================="
echo "Results:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo "  Skipped: $SKIPPED"
echo "=========================================="
