#!/bin/bash
# Memory-limited runner - limits to 16GB RAM
ulimit -v 16777216  # 16GB in KB
exec "$@"
