#!/usr/bin/env bash
# One-time setup for PyMCPAutoGUI (Full IDE checklist via MCP).
# Uses uv for venv + install. Install uv first: curl -LsSf https://astral.sh/uv/install.sh | sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QB64FRESH_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if ! command -v uv &>/dev/null; then
  echo "uv not found. Install it first: curl -LsSf https://astral.sh/uv/install.sh | sh" >&2
  echo "  (uv installs to ~/.local/bin; ensure that is in PATH)" >&2
  exit 1
fi

cd "$QB64FRESH_ROOT"
echo "Creating venv and installing pymcpautogui with uv..."
uv venv .venv
uv pip install --python .venv/bin/python pymcpautogui

echo "Verifying..."
.venv/bin/python -m pymcpautogui.server --help 2>/dev/null || true

echo "Done. MCP config should use: $QB64FRESH_ROOT/.venv/bin/python"
echo "Project config: $QB64FRESH_ROOT/.cursor/mcp.json"
echo "Reload Cursor / MCP, then use @PyMCPAutoGUI to run the Full IDE checklist."
