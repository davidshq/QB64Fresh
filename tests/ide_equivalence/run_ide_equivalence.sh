#!/usr/bin/env bash
# Run IDE equivalence console-only tests (portions 5, 6, 9, 10).
# Compiles with QB64Fresh (and QB64pe if available), runs, captures stdout+stderr.
# GUI portions (1–4, 7) are manual; portion 8 uses tests/runtime_comparison/.
#
# Run from QB64Fresh repo root. Uses 4GB ulimit for QB64pe.
#
# Environment:
#   RUN_PE=0              Skip QB64pe (faster; use when QB64pe not built).
#   FORCE=0                If 1, always rerun. If 0 (default), skip when unchanged.
#   VERBOSE=1              Log each step (default 1).
#   FRESH_RUN_TIMEOUT      Seconds per QB64Fresh exe (default 10).
#   PE_COMPILE_TIMEOUT     Seconds for QB64pe compile per test (default 120).
#   PE_RUN_TIMEOUT         Seconds per QB64pe exe (default 10).
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RESULTS="$SCRIPT_DIR/results"
mkdir -p "$RESULTS/fresh" "$RESULTS/qb64pe"
QB64PE_DIR="${QB64PE_DIR:-$ROOT/../QB64pe}"
RUN_PE="${RUN_PE:-1}"
FORCE="${FORCE:-0}"
VERBOSE="${VERBOSE:-1}"
FRESH_RUN_TIMEOUT="${FRESH_RUN_TIMEOUT:-10}"
PE_COMPILE_TIMEOUT="${PE_COMPILE_TIMEOUT:-120}"
PE_RUN_TIMEOUT="${PE_RUN_TIMEOUT:-10}"

REPORT_DIR="$(mktemp -d)"
trap 'rm -rf "$REPORT_DIR"' EXIT
touch "$REPORT_DIR/fail_compile_fresh" "$REPORT_DIR/fail_link_fresh" \
      "$REPORT_DIR/timeout_fresh" "$REPORT_DIR/fail_compile_pe" \
      "$REPORT_DIR/timeout_pe_compile" "$REPORT_DIR/timeout_pe"

log() { [ "$VERBOSE" = "1" ] && echo "  $*" || true; }

QB64FRESH="${QB64FRESH:-$ROOT/target/release/qb64fresh}"
if [ ! -x "$QB64FRESH" ]; then
  QB64FRESH="$ROOT/target/debug/qb64fresh"
fi
if [ ! -x "$QB64FRESH" ]; then
  echo "Error: qb64fresh not found. Build with: cargo build --release --bin qb64fresh" >&2
  exit 1
fi

get_fingerprint() {
  local fresh_mtime pe_mtime
  fresh_mtime=$(stat -c %Y "$QB64FRESH" 2>/dev/null || echo "0")
  if [ "$RUN_PE" = "1" ] && [ -x "$QB64PE_DIR/qb64pe" ]; then
    pe_mtime=$(stat -c %Y "$QB64PE_DIR/qb64pe" 2>/dev/null || echo "none")
  else
    pe_mtime="none"
  fi
  echo "FRESH_MTIME=$fresh_mtime"
  echo "PE_MTIME=$pe_mtime"
}

save_report() {
  for f in fail_compile_fresh fail_link_fresh timeout_fresh fail_compile_pe timeout_pe_compile timeout_pe; do
    [ -f "$REPORT_DIR/$f" ] && cp "$REPORT_DIR/$f" "$RESULTS/${f}.txt" || true
  done
}

print_section() {
  local file="$1"
  local title="$2"
  if [ -s "$file" ]; then
    echo "$title"
    while IFS= read -r line; do echo "  - $line"; done < "$file"
    echo ""
  fi
}

run_fresh() {
  local bas="$1"
  local name="$2"
  local cfile="$RESULTS/fresh/${name}.c"
  local exe="$RESULTS/fresh/${name}.out"
  local out="$RESULTS/fresh/${name}.txt"
  log "fresh: compile $name ..."
  if ! "$QB64FRESH" "$bas" --emit-c --runtime inline -o "$cfile" 2>/dev/null; then
    echo "FAIL_COMPILE_FRESH" > "$out"
    echo "$name" >> "$REPORT_DIR/fail_compile_fresh"
    return
  fi
  log "fresh: link $name ..."
  if ! gcc -o "$exe" "$cfile" -lm 2>/dev/null; then
    echo "FAIL_LINK_FRESH" > "$out"
    echo "$name" >> "$REPORT_DIR/fail_link_fresh"
    return
  fi
  log "fresh: run $name (timeout ${FRESH_RUN_TIMEOUT}s) ..."
  if ! timeout "$FRESH_RUN_TIMEOUT" "$exe" > "$out" 2>&1; then
    rc=$?
    if [ "$rc" = 124 ]; then
      echo "TIMEOUT_FRESH" >> "$out"
      echo "$name" >> "$REPORT_DIR/timeout_fresh"
    fi
  fi
}

run_pe() {
  local bas="$1"
  local name="$2"
  local out="$RESULTS/qb64pe/${name}.txt"
  local exe_name="rt_${name}.out"
  local exe_rel="runtime_comparison/$exe_name"
  if [ "$RUN_PE" = "0" ] || [ ! -x "$QB64PE_DIR/qb64pe" ]; then
    echo "SKIP_QB64PE_NOT_BUILT" > "$out"
    return
  fi
  log "qb64pe: compile $name (timeout ${PE_COMPILE_TIMEOUT}s) ..."
  if ! (cd "$QB64PE_DIR" && mkdir -p runtime_comparison && ulimit -v 4194304 2>/dev/null; \
       timeout "$PE_COMPILE_TIMEOUT" ./qb64pe -x "$(realpath "$bas")" -o "$exe_rel" 2>/dev/null); then
    rc=$?
    echo "FAIL_COMPILE_PE" > "$out"
    if [ "$rc" = 124 ]; then
      echo "TIMEOUT_PE_COMPILE" >> "$out"
      echo "$name" >> "$REPORT_DIR/timeout_pe_compile"
    else
      echo "$name" >> "$REPORT_DIR/fail_compile_pe"
    fi
    return
  fi
  log "qb64pe: run $name (timeout ${PE_RUN_TIMEOUT}s) ..."
  if ! (cd "$QB64PE_DIR" && timeout "$PE_RUN_TIMEOUT" ./runtime_comparison/"$exe_name" > "$(realpath "$out")" 2>&1); then
    rc=$?
    if [ "$rc" = 124 ]; then
      echo "TIMEOUT_PE" >> "$out"
      echo "$name" >> "$REPORT_DIR/timeout_pe"
    fi
  fi
}

# Console-only repros (portions 5, 6, 9, 10)
CONSOLE_TESTS="05_file_io 06_filesystem 09_time_shell 10_audio"
count=0
total=4

report_from_results() {
  print_section "$RESULTS/fail_compile_fresh.txt" "QB64Fresh compile failures:"
  print_section "$RESULTS/fail_link_fresh.txt"   "QB64Fresh link failures:"
  print_section "$RESULTS/timeout_fresh.txt"     "QB64Fresh run timeouts:"
  if [ "$RUN_PE" = "1" ] && [ -x "$QB64PE_DIR/qb64pe" ]; then
    print_section "$RESULTS/fail_compile_pe.txt"    "QB64pe compile failures:"
    print_section "$RESULTS/timeout_pe_compile.txt" "QB64pe compile timeouts:"
    print_section "$RESULTS/timeout_pe.txt"         "QB64pe run timeouts:"
  fi
}

FINGERPRINT_FILE="$RESULTS/.last_run"
if [ "$FORCE" != "1" ] && [ -f "$FINGERPRINT_FILE" ]; then
  if [ -n "$(find "$RESULTS/fresh" -maxdepth 1 -name '*.txt' -type f 2>/dev/null | head -1)" ]; then
    current_fp="$(get_fingerprint | sort)"
    stored_fp="$(cat "$FINGERPRINT_FILE" | sort 2>/dev/null)"
    if [ "$current_fp" = "$stored_fp" ]; then
      echo "No code changes since last run; using existing results ($total tests)."
      echo "Use FORCE=1 to rerun. Results in $RESULTS/fresh/ and $RESULTS/qb64pe/"
      echo ""
      report_from_results
      exit 0
    fi
  fi
fi

for name in $CONSOLE_TESTS; do
  bas="$SCRIPT_DIR/${name}.bas"
  [ ! -f "$bas" ] && continue
  count=$((count + 1))
  echo "[$count/$total] $name ..."
  run_fresh "$bas" "$name"
  run_pe "$bas" "$name"
done

get_fingerprint > "$FINGERPRINT_FILE"
save_report

echo ""
echo "Done. Ran $count console-only IDE equivalence tests. Results in $RESULTS/fresh/ and $RESULTS/qb64pe/"
echo "GUI portions (01–04, 07): run .bas manually under native qb64pe and qb64pe_fresh; see BASELINE.md."
echo "Strings/console (08): use tests/runtime_comparison/run_comparison.sh."
echo ""

print_section "$REPORT_DIR/fail_compile_fresh" "QB64Fresh compile failures:"
print_section "$REPORT_DIR/fail_link_fresh"   "QB64Fresh link failures:"
print_section "$REPORT_DIR/timeout_fresh"     "QB64Fresh run timeouts:"
if [ "$RUN_PE" = "1" ] && [ -x "$QB64PE_DIR/qb64pe" ]; then
  print_section "$REPORT_DIR/fail_compile_pe"    "QB64pe compile failures:"
  print_section "$REPORT_DIR/timeout_pe_compile" "QB64pe compile timeouts:"
  print_section "$REPORT_DIR/timeout_pe"         "QB64pe run timeouts:"
fi

any_fail=
for f in "$REPORT_DIR"/*; do [ -s "$f" ] && any_fail=1 && break; done
if [ -n "$any_fail" ]; then
  exit 1
fi
