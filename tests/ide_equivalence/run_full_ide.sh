#!/usr/bin/env bash
# Launch the full QB64pe IDE built with QB64Fresh (qb64pe_fresh) for manual checklist.
# Run from QB64Fresh repo root. Uses QB64pe/run_qb64pe_fresh.sh (builds then runs with ulimit).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
QB64PE_DIR="${QB64PE_DIR:-$QB64FRESH_ROOT/../QB64pe}"

if [[ ! -d "$QB64PE_DIR" ]]; then
  echo "QB64pe directory not found: $QB64PE_DIR" >&2
  echo "Set QB64PE_DIR if QB64pe is elsewhere." >&2
  exit 1
fi

if [[ ! -x "$QB64PE_DIR/run_qb64pe_fresh.sh" ]]; then
  echo "Run script not found: $QB64PE_DIR/run_qb64pe_fresh.sh" >&2
  echo "See QB64pe/BUILD_WITH_QB64FRESH.md to build qb64pe_fresh first." >&2
  exit 1
fi

echo "Launching qb64pe_fresh from $QB64PE_DIR (builds then runs with ulimit)..."
echo "After testing, fill BASELINE.md 'Full IDE checklist' table and 'Funky or broken behavior'."
echo ""

cd "$QB64PE_DIR"
exec ./run_qb64pe_fresh.sh
