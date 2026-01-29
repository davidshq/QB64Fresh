#!/bin/bash
# Memory-limited runner - limits to 4GB RAM
# Note: Compiling very large files (e.g. full QB64pe) may still grow until OOM; limit caps damage.
ulimit -v 4194304  # 4GB in KB
exec "$@"
