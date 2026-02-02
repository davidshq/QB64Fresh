# Using PyMCPAutoGUI for Full IDE Checklist

[PyMCPAutoGUI](https://github.com/kitfactory/PyMCPAutoGUI) is an MCP server that lets an AI agent (e.g. in Cursor) control mouse and keyboard via PyAutoGUI. You can use it to drive **qb64pe_fresh** for the Full IDE checklist instead of (or when) the shell script fails on Wayland.

## 1. Install PyMCPAutoGUI

Uses **uv** (recommended) or pip. One-time setup:

**With uv** (install uv first if needed: `curl -LsSf https://astral.sh/uv/install.sh | sh`):

```bash
# From QB64Fresh repo root:
./tests/ide_equivalence/setup_pymcpautogui.sh
```

That script runs `uv venv .venv` and `uv pip install --python .venv/bin/python pymcpautogui`. The MCP config should point `command` at `QB64Fresh/.venv/bin/python`.

**With pip:** If you prefer pip, install Python 3.11+ and pip (e.g. `sudo apt install python3-pip`), then `python3 -m pip install --user pymcpautogui` and set `"command": "python3"` in the MCP config.

**Linux:** PyAutoGUI may need extra deps for screenshots (e.g. `scrot`). On Wayland, key/mouse may still go through ydotool or have compositor limits—see [Wayland automation options](README.md#wayland-automation-options-why-its-hard).

## 2. Add PyMCPAutoGUI to Cursor MCP config

**Project-level config:** This repo can ship a project MCP config at `.cursor/mcp.json` (if present). It should look like:

```json
{
  "mcpServers": {
    "PyMCPAutoGUI": {
      "command": "python3",
      "args": ["-m", "pymcpautogui.server"],
      "cwd": "/absolute/path/to/QB64Fresh"
    }
  }
}
```

- If you use **uv** and ran `setup_pymcpautogui.sh`, set `"command": "/path/to/QB64Fresh/.venv/bin/python"`.
- If you use system Python with `pip install --user pymcpautogui`, `"command": "python3"` is enough; set `cwd` to your QB64Fresh repo path.
- If you already have other MCP servers (global or project), merge this `PyMCPAutoGUI` entry into your existing `mcpServers` so you don’t overwrite them.

Then save and **reload Cursor / MCP** (e.g. restart Cursor or use MCP refresh) so the server is available.

## 3. Run the Full IDE checklist with PyMCPAutoGUI

1. **Start the IDE** (so the QB64 window exists):
   ```bash
   ./tests/ide_equivalence/run_full_ide.sh
   ```
   Leave it running (or run in background). When the QB64 Phoenix Edition window is visible, continue.

2. **In Cursor chat**, use the PyMCPAutoGUI server (e.g. `@PyMCPAutoGUI` or the name you gave in mcp.json). Ask the AI to drive the Full IDE checklist, for example:

   - **Activate the IDE window:**  
     `activate_window(title="QB64")` or `get_windows_with_title("QB64")` then activate.
   - **File → New:**  
     `hotkey("alt", "f")` then `press("n")`.
   - **Type in editor:**  
     `write("PRINT \"hi\"", interval=0.05)`.
   - **Close (Don’t Save):**  
     `hotkey("alt", "f4")` then after a short wait `press("n")` for “Don’t Save” if a dialog appears.

   You can say: *“Using PyMCPAutoGUI: activate the window with title containing QB64, then do File → New (Alt+F, n), type PRINT \"hi\", then Alt+F4 and press n for Don’t Save.”*

3. **Fill BASELINE.md** with what you observed (Pass? and Notes for each step, and any “Funky or broken behavior”).

## 4. Useful PyMCPAutoGUI tools (from their README)

- **Keyboard:** `write(text, interval=0.1)`, `press(key)`, `hotkey(key1, key2, ...)`, `key_down`, `key_up`
- **Mouse:** `move_to(x, y)`, `click()`, `move_rel`, `scroll`
- **Windows:** `get_all_titles`, `get_windows_with_title(title)`, `get_active_window`, `activate_window(title="...")`, `close_window`
- **Screen:** `screenshot(filename="...")`, `locate_on_screen`, `locate_center_on_screen`

Window title for qb64pe_fresh is typically **"QB64 Phoenix Edition (x64)"** or similar (contains `QB64` or `Phoenix`).

## 5. If the server doesn’t start

- Ensure the `command` in mcp.json points to a Python that has `pymcpautogui` installed (`python -m pymcpautogui.server` runs without errors in that env).
- Check Cursor’s MCP logs for errors.
- On Linux, if key/mouse don’t work, PyAutoGUI may be using a backend that needs X11 or ydotool; see [README Wayland options](README.md#wayland-automation-options-why-its-hard).
