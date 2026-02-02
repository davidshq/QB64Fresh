#!/usr/bin/env bash
# Capture native baseline (BASELINE.md) by running native qb64pe and qb64pe_fresh
# for each IDE equivalence portion. Populates console portions (5,6,9,10) from
# run_ide_equivalence.sh results; records compile-only status for GUI portions
# (1–4, 7). Run from QB64Fresh repo root.
#
# Usage: ./tests/ide_equivalence/capture_baseline.sh
#
# Environment: RUN_PE=1 (default) to run QB64pe; RUN_PE=0 to skip native (only
# QB64Fresh results will be captured). Uses 4GB ulimit for QB64pe.
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RESULTS="$SCRIPT_DIR/results"
BASELINE="$SCRIPT_DIR/BASELINE.md"
QB64PE_DIR="${QB64PE_DIR:-$ROOT/../QB64pe}"
RUN_PE="${RUN_PE:-1}"
QB64FRESH="${QB64FRESH:-$ROOT/target/release/qb64fresh}"
[ -x "$QB64FRESH" ] || QB64FRESH="$ROOT/target/debug/qb64fresh"

# Ensure results exist by running console-only tests
echo "Running IDE equivalence tests (console portions) to capture output..."
FORCE=1 RUN_PE="$RUN_PE" "$ROOT/tests/ide_equivalence/run_ide_equivalence.sh" || true

mkdir -p "$RESULTS/fresh" "$RESULTS/qb64pe"

# Use ~~~ for code blocks so output containing ``` does not break the fence
write_portion_capture() {
  local name="$1"
  local title="$2"
  local native_file="$RESULTS/qb64pe/${name}.txt"
  local fresh_file="$RESULTS/fresh/${name}.txt"
  echo ""
  echo "### $title ($name)"
  echo ""
  echo "- **Native (qb64pe):**"
  if [ -f "$native_file" ]; then
    echo '~~~'
    head -n 150 < "$native_file"
    [ "$(wc -l < "$native_file" 2>/dev/null)" -gt 150 ] 2>/dev/null && echo "... (truncated)"
    echo '~~~'
  else
    echo "_（no result file; run with RUN_PE=1）_"
  fi
  echo ""
  echo "- **QB64Fresh (qb64pe_fresh):**"
  if [ -f "$fresh_file" ]; then
    echo '~~~'
    head -n 150 < "$fresh_file"
    [ "$(wc -l < "$fresh_file" 2>/dev/null)" -gt 150 ] 2>/dev/null && echo "... (truncated)"
    echo '~~~'
  else
    echo "_（no result file）_"
  fi
}

# Compile-only check for a .bas file with qb64pe; echo OK or FAIL + optional message
compile_native() {
  local bas="$1"
  local out="$2"
  if [ "$RUN_PE" != "1" ] || [ ! -x "$QB64PE_DIR/qb64pe" ]; then
    echo "SKIP (qb64pe not built)"
    return
  fi
  if (cd "$QB64PE_DIR" && ulimit -v 4194304 2>/dev/null; ./qb64pe -x "$(realpath "$bas")" -o "$out" 2>/dev/null); then
    echo "OK"
  else
    echo "FAIL"
  fi
}

# Compile-only check with qb64fresh; echo OK or FAIL
compile_fresh() {
  local bas="$1"
  local cfile="$2"
  if [ ! -x "$QB64FRESH" ]; then
    echo "SKIP (qb64fresh not built)"
    return
  fi
  if "$QB64FRESH" "$bas" --emit-c --runtime inline -o "$cfile" >/dev/null 2>/dev/null; then
    echo "OK"
  else
    echo "FAIL"
  fi
}

# Build the auto-generated section content
CAPTURED="## Captured outputs (auto-generated)

_Do not edit this section by hand. Re-run \`./tests/ide_equivalence/capture_baseline.sh\` to refresh._

- **Captured:** $(date -u '+%Y-%m-%d %H:%M UTC')
- **System:** ${OS:-unknown} / ${DISPLAY:-no DISPLAY}
- **RUN_PE:** $RUN_PE
"

# Console portions: use run_ide_equivalence.sh results
CAPTURED="$CAPTURED$(write_portion_capture "05_file_io"    "5. File I/O")"
CAPTURED="$CAPTURED$(write_portion_capture "06_filesystem" "6. Filesystem")"
CAPTURED="$CAPTURED$(write_portion_capture "09_time_shell" "9. Time and shell")"
CAPTURED="$CAPTURED$(write_portion_capture "10_audio"      "10. Audio")"

# GUI portions: compile-only status
CAPTURED="$CAPTURED

### GUI portions (compile-only)

| Portion | Repro | Native (qb64pe) compile | QB64Fresh compile |
|---------|-------|-------------------------|-------------------|
"

for entry in "01_window_display:1. Window and display" "02_event_loop:2. Event loop" "03_graphics_image:3. Graphics / image" "04_font_text:4. Font and text" "07_gui_dialogs:7. GUI dialogs"; do
  name="${entry%%:*}"
  title="${entry#*:}"
  bas="$SCRIPT_DIR/${name}.bas"
  [ ! -f "$bas" ] && continue
  native_out="$RESULTS/qb64pe/${name}.exe"
  fresh_c="$RESULTS/fresh/${name}.c"
  nat="$(compile_native "$bas" "$native_out")"
  fre="$(compile_fresh "$bas" "$fresh_c")"
  CAPTURED="$CAPTURED| $title | ${name}.bas | $nat | $fre |"$'\n'
done

CAPTURED="$CAPTURED

### 8. Strings and console

See \`tests/runtime_comparison/\` and golden \`.out\` outputs; no per-portion capture here.
"

# Update BASELINE.md: replace "## Captured outputs (auto-generated)" to EOF with new content
if grep -q "## Captured outputs (auto-generated)" "$BASELINE" 2>/dev/null; then
  # Print lines before the marker, then the new captured section
  awk '/## Captured outputs \(auto-generated\)/{exit} {print}' "$BASELINE" > "$BASELINE.new"
  echo "$CAPTURED" >> "$BASELINE.new"
  mv "$BASELINE.new" "$BASELINE"
else
  echo "" >> "$BASELINE"
  echo "$CAPTURED" >> "$BASELINE"
fi

echo "Baseline capture complete. Updated $BASELINE"
