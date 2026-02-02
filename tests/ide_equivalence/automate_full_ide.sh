#!/usr/bin/env bash
# Optional: automate Full IDE checklist by driving qb64pe_fresh (X11: xdotool; Wayland: wtype/ydotool + swaymsg/wlrctl).
# Run from QB64Fresh repo root. X11: DISPLAY + xdotool. Wayland: wtype or ydotool (ydotoold); optional swaymsg/wlrctl to focus.
# Sequence: launch IDE → wait for window "QB64" → focus (if possible) → File→New → type PRINT "hi" → close (Don't Save).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
QB64PE_DIR="${QB64PE_DIR:-$QB64FRESH_ROOT/../QB64pe}"
IDE_WINDOW_WAIT="${IDE_WINDOW_WAIT:-60}"
IDE_SETTLE_S="${IDE_SETTLE_S:-2}"
KEY_DELAY_MS="${KEY_DELAY_MS:-80}"
# Per-command timeout for xdotool (XWayland can cause xdotool to hang indefinitely)
XDOTOOL_TIMEOUT="${XDOTOOL_TIMEOUT:-5}"

WAYLAND=0
if [[ -n "${WAYLAND_DISPLAY:-}" ]] || [[ "${XDG_SESSION_TYPE:-}" == "wayland" ]]; then
  WAYLAND=1
fi

# --- Common requirements ---
if [[ ! -d "$QB64PE_DIR" ]]; then
  echo "QB64pe directory not found: $QB64PE_DIR" >&2
  exit 1
fi
if [[ ! -x "$QB64PE_DIR/qb64pe_fresh" ]]; then
  echo "qb64pe_fresh not found or not executable. Build first: ./tests/ide_equivalence/run_full_ide.sh (then cancel); or see QB64pe/BUILD_WITH_QB64FRESH.md" >&2
  exit 1
fi

# --- X11: need DISPLAY + xdotool ---
if (( ! WAYLAND )); then
  if [[ -z "${DISPLAY:-}" ]]; then
    echo "DISPLAY not set. This script needs an X11 display (e.g. :0) or run on Wayland (wtype/ydotool)." >&2
    exit 1
  fi
  if ! command -v xdotool &>/dev/null; then
    echo "xdotool not found. Install it (e.g. apt install xdotool) for X11 GUI automation." >&2
    exit 1
  fi
fi

# --- Wayland: decide key method before launching IDE (wtype, ydotool, or X11 fallback) ---
WAYLAND_USE_WTYPE=0
WAYLAND_USE_YDOTOOL=0
WAYLAND_USE_X11=0
if (( WAYLAND )); then
  HAVE_WTYPE=0
  HAVE_YDOTOOL=0
  command -v wtype &>/dev/null && HAVE_WTYPE=1
  command -v ydotool &>/dev/null && HAVE_YDOTOOL=1
  HAVE_XDOTOOL=0
  command -v xdotool &>/dev/null && [[ -n "${DISPLAY:-}" ]] && HAVE_XDOTOOL=1
  if (( ! HAVE_WTYPE && ! HAVE_YDOTOOL && ! HAVE_XDOTOOL )); then
    echo "On Wayland, install wtype (apt install wtype) or ydotool, or xdotool + DISPLAY for X11 fallback." >&2
    exit 1
  fi
  # Probe wtype first; on GNOME it fails with 'virtual keyboard protocol not supported'
  if (( HAVE_WTYPE )); then
    if wtype -d 1 " " 2>/dev/null; then
      WAYLAND_USE_WTYPE=1
      echo "Using wtype for key injection."
    else
      err=$(wtype -d 1 " " 2>&1) || true
      if [[ "$err" == *"virtual keyboard"* ]] || [[ "$err" == *"protocol"* ]] || [[ "$err" == *"not support"* ]]; then
        echo "wtype is installed but does not work on this compositor (e.g. GNOME): $err" >&2
        # Prefer X11 fallback over ydotool: no daemon (ydotoold) required, works with just xdotool + DISPLAY
        if (( HAVE_XDOTOOL )); then
          echo "Using X11 fallback: launching IDE under X11 (SDL_VIDEODRIVER=x11) and driving with xdotool."
          WAYLAND_USE_X11=1
        elif (( HAVE_YDOTOOL )); then
          echo "Using ydotool (ensure ydotoold is running: sudo ydotoold &)."
          WAYLAND_USE_YDOTOOL=1
        else
          echo "Options: (1) Manual checklist: ./tests/ide_equivalence/run_full_ide.sh then do steps by hand. (2) Install xdotool (apt install xdotool); DISPLAY is set so we can use X11 fallback next run. (3) Build ydotool from source: https://github.com/ReimuNotMoe/ydotool" >&2
          exit 1
        fi
      else
        echo "wtype failed: $err" >&2
        exit 1
      fi
    fi
  elif (( HAVE_YDOTOOL )); then
    WAYLAND_USE_YDOTOOL=1
  elif (( HAVE_XDOTOOL )); then
    WAYLAND_USE_X11=1
    echo "Using X11 fallback (SDL_VIDEODRIVER=x11 + xdotool)."
  fi
fi

# --- Ensure recent.bin so File menu doesn't Error 53 ---
mkdir -p "$QB64PE_DIR/settings" "$QB64PE_DIR/source/settings"
touch "$QB64PE_DIR/settings/recent.bin" "$QB64PE_DIR/source/settings/recent.bin"

# --- Launch IDE in background ---
cd "$QB64PE_DIR"
export QB64FRESH_IDE_COMPAT=1
if (( WAYLAND && WAYLAND_USE_X11 )); then
  export SDL_VIDEODRIVER=x11
  export GDK_BACKEND=x11
fi
(
  ulimit -v 4194304 2>/dev/null || true
  exec ./qb64pe_fresh
) &
IDE_PID=$!
echo "Launched qb64pe_fresh (PID $IDE_PID). Waiting up to ${IDE_WINDOW_WAIT}s for window..."

if (( WAYLAND )); then
  # --- Wayland: try to focus window (Sway or wlrctl), or use xdotool if X11 fallback ---
  if (( WAYLAND_USE_X11 )); then
    winid=""
    for (( i = 0; i < IDE_WINDOW_WAIT; i++ )); do
      if ! kill -0 "$IDE_PID" 2>/dev/null; then break; fi
      winid=$(xdotool search --name "QB64" 2>/dev/null | head -1)
      [[ -z "$winid" ]] && winid=$(xdotool search --name "Phoenix" 2>/dev/null | head -1)
      if [[ -n "$winid" ]]; then
        echo "Found IDE window (X11) id $winid after ${i}s."
        # Don't use windowfocus: XWayland often returns BadMatch on SetInputFocus. Send keys with --window instead.
        break
      fi
      sleep 1
    done
    if [[ -z "$winid" ]]; then
      kill "$IDE_PID" 2>/dev/null || true
      echo "Timeout: IDE X11 window did not appear. Is DISPLAY ($DISPLAY) correct for XWayland?" >&2
      exit 1
    fi
  fi
  sleep 2
  focused=0
  if (( ! WAYLAND_USE_X11 )); then
  if command -v swaymsg &>/dev/null && [[ -n "${SWAYSOCK:-}" ]]; then
    for (( i = 0; i < IDE_WINDOW_WAIT; i++ )); do
      if ! kill -0 "$IDE_PID" 2>/dev/null; then break; fi
      if command -v jq &>/dev/null; then
        con_id=$(swaymsg -t get_tree 2>/dev/null | jq -r '.. | select(.name? | test("QB64|Phoenix")) | .id' 2>/dev/null | head -1)
        if [[ -n "$con_id" && "$con_id" != "null" ]]; then
          swaymsg "[con_id=$con_id]" focus 2>/dev/null && { focused=1; echo "Focused IDE window (Sway con_id $con_id) after ${i}s."; break; }
        fi
      else
        # No jq: try exact title substring match via swaymsg
        if swaymsg '[title="QB64 Phoenix Edition (x64)"]' focus 2>/dev/null || swaymsg '[title="QB64 Phoenix Edition (x86)"]' focus 2>/dev/null; then
          focused=1
          echo "Focused IDE window (Sway) after ${i}s."
          break
        fi
      fi
      sleep 1
    done
  fi
  if (( ! focused )) && command -v wlrctl &>/dev/null; then
    for (( i = 0; i < IDE_WINDOW_WAIT; i++ )); do
      if ! kill -0 "$IDE_PID" 2>/dev/null; then break; fi
      if wlrctl window focus title:QB64 2>/dev/null || wlrctl window focus title:Phoenix 2>/dev/null; then
        focused=1
        echo "Focused IDE window (wlrctl) after ${i}s."
        break
      fi
      sleep 1
    done
  fi
  if (( ! WAYLAND_USE_X11 && ! focused )); then
    echo "Could not auto-focus IDE (no Sway/wlrctl or window not found). Click the IDE window now; continuing in 10s..."
    sleep 10
  fi
  fi
  sleep "$IDE_SETTLE_S"

  # Send keys using the method we chose before launching (wtype, ydotool, or xdotool via X11 fallback)
  if (( WAYLAND_USE_X11 )); then
    # Send keys to window by ID. XWayland can cause xdotool to hang; use timeout so script doesn't block forever.
    xdotool_fail() {
      echo "xdotool timed out or failed (XWayland often blocks/hangs xdotool). Use manual checklist: ./tests/ide_equivalence/run_full_ide.sh" >&2
      kill "$IDE_PID" 2>/dev/null || true
      exit 1
    }
    timeout "$XDOTOOL_TIMEOUT" xdotool key --window "$winid" --delay "$KEY_DELAY_MS" alt+f || xdotool_fail
    sleep 0.4
    timeout "$XDOTOOL_TIMEOUT" xdotool key --window "$winid" --delay "$KEY_DELAY_MS" n || xdotool_fail
    sleep 0.5
    timeout "$XDOTOOL_TIMEOUT" xdotool type --window "$winid" --delay "$KEY_DELAY_MS" 'PRINT "hi"' || xdotool_fail
    sleep 0.3
    timeout "$XDOTOOL_TIMEOUT" xdotool key --window "$winid" --delay "$KEY_DELAY_MS" alt+F4 || xdotool_fail
    sleep 1
    timeout "$XDOTOOL_TIMEOUT" xdotool key --window "$winid" --delay "$KEY_DELAY_MS" n || xdotool_fail
  elif (( WAYLAND_USE_YDOTOOL )); then
    ydotool key -d "$KEY_DELAY_MS" 56:1 33:1 33:0 56:0
    sleep 0.4
    ydotool key -d "$KEY_DELAY_MS" 49:1 49:0
    sleep 0.5
    ydotool type -d "$KEY_DELAY_MS" 'PRINT "hi"'
    sleep 0.3
    ydotool key -d "$KEY_DELAY_MS" 56:1 62:1 62:0 56:0
    sleep 1
    ydotool key -d "$KEY_DELAY_MS" 49:1 49:0
  else
    wtype -d "$KEY_DELAY_MS" -M alt f -m alt
    sleep 0.4
    wtype -d "$KEY_DELAY_MS" n
    sleep 0.5
    wtype -d "$KEY_DELAY_MS" 'PRINT "hi"'
    sleep 0.3
    wtype -d "$KEY_DELAY_MS" -M alt -k F4 -m alt
    sleep 1
    wtype -d "$KEY_DELAY_MS" n
  fi
  sleep 0.5
else
  # --- X11: wait for window, focus with xdotool, send keys ---
  winid=""
  for (( i = 0; i < IDE_WINDOW_WAIT; i++ )); do
    if ! kill -0 "$IDE_PID" 2>/dev/null; then
      echo "IDE process exited before window appeared." >&2
      exit 1
    fi
    winid=$(xdotool search --name "QB64" 2>/dev/null | head -1)
    [[ -z "$winid" ]] && winid=$(xdotool search --name "Phoenix" 2>/dev/null | head -1)
    if [[ -n "$winid" ]]; then
      echo "Found IDE window id $winid after ${i}s."
      break
    fi
    sleep 1
  done
  if [[ -z "$winid" ]]; then
    kill "$IDE_PID" 2>/dev/null || true
    echo "Timeout: IDE window did not appear." >&2
    exit 1
  fi
  xdotool windowfocus "$winid"
  sleep "$IDE_SETTLE_S"
  xdotool key --delay "$KEY_DELAY_MS" alt+f
  sleep 0.4
  xdotool key --delay "$KEY_DELAY_MS" n
  sleep 0.5
  xdotool type --delay "$KEY_DELAY_MS" 'PRINT "hi"'
  sleep 0.3
  xdotool key --delay "$KEY_DELAY_MS" alt+F4
  sleep 1
  xdotool key --delay "$KEY_DELAY_MS" n
  sleep 0.5
fi

# --- Wait for IDE to exit ---
wait "$IDE_PID" 2>/dev/null && true
exitcode=0
if kill -0 "$IDE_PID" 2>/dev/null; then
  echo "IDE did not exit after automation; sending SIGTERM." >&2
  kill "$IDE_PID" 2>/dev/null || true
  sleep 2
  kill -9 "$IDE_PID" 2>/dev/null || true
  exitcode=1
fi
echo "IDE exited. Automation run complete (exit $exitcode)."
exit "$exitcode"
