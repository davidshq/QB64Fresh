#!/bin/bash
# Run QB64pe bootstrap tests
#
# Usage:
#   ./scripts/test-bootstrap.sh           # Run all bootstrap tests
#   ./scripts/test-bootstrap.sh quick     # Run only parse test (faster)
#   ./scripts/test-bootstrap.sh full      # Run with full output

set -e

cd "$(dirname "$0")/.."

case "${1:-all}" in
    quick)
        echo "Running quick parse test..."
        cargo test --test bootstrap_tests qb64pe_parses -- --nocapture
        ;;
    full)
        echo "Running full bootstrap test suite..."
        cargo test --test bootstrap_tests -- --nocapture
        ;;
    all|*)
        echo "Running bootstrap tests..."
        cargo test --test bootstrap_tests
        ;;
esac
