# _DEFLATE$ / _INFLATE$ (Compression)

QB64Fresh supports `_DEFLATE$` and `_INFLATE$` for raw DEFLATE compression and decompression.

## Runtime modes

- **External runtime (`--runtime external`):** Real compression via the Rust runtime (miniz_oxide). Link with `libqb64fresh_rt`; `_DEFLATE$` and `_INFLATE$` work as documented.
- **Inline runtime (default):** Emitted C contains stub implementations that return an empty string. Programs compile and run but compression does nothing unless you use the optional C module below.

## Real compression with inline runtime

To get working `_DEFLATE$` / `_INFLATE$` when using the inline runtime (single emitted `.c` file):

1. Compile your program with `-DQB64FRESH_COMPRESSION_EXTERNAL` so the emitted code does **not** define the stub (the stub is wrapped in `#ifndef QB64FRESH_COMPRESSION_EXTERNAL`).
2. Compile and link `runtime/c_src/compression.c` with zlib.

**Example (from repo root):**

```bash
# Emit C (e.g. prog.c)
qb64fresh myprogram.bas --emit-c -o prog.c

# Compile with compression support
gcc -c -I runtime/include -DQB64FRESH_COMPRESSION_EXTERNAL prog.c -o prog.o
gcc -c -I runtime/include -DQB64FRESH_COMPRESSION_EXTERNAL runtime/c_src/compression.c -o compression.o
gcc -o myprogram prog.o compression.o -lz -lm
```

**Requirements:** zlib development headers and library (`libz`, e.g. `zlib1g-dev` on Debian/Ubuntu).

## Format

- **Compress:** Raw DEFLATE (no zlib header/footer), matching the behavior of the external runtime (miniz_oxide).
- **Decompress:** Raw DEFLATE input; output is limited to 64 MiB to avoid unbounded allocation.

The C module in `runtime/c_src/compression.c` uses zlib with `-MAX_WBITS` for raw deflate/inflate so the format is compatible with typical raw DEFLATE expectations.
