# Session 141: TODO Consolidated Tiers Implementation

**Date:** 2026-01-31

## Goal

Implement fully the remaining items from TODO_CONSOLIDATED.md (lines 98–104):

| Tier | Area | Remaining |
|------|------|-----------|
| 1 | qbs-compatible string system | Optional (only if QB64pe binary compatibility needed) |
| 2 | File I/O | COM: OPEN "COM1:9600,N,8,1" AS #n without FOR |
| 2 | Graphics | Advanced _DEST/_SOURCE, OpenGL stubs, some PUT modes |
| 3 | Type conversions | MKx$/CVx, MBF — verify complete |
| 4 | Advanced | Zlib: inline runtime compression |

## Decisions

1. **Tier 1 (qbs):** Document as optional; no implementation (only if binary compat needed).
2. **Tier 2 File I/O:** Parser currently requires `OPEN filename FOR mode ...`. QB45 allows `OPEN "COM1:9600,N,8,1" AS #1`. Add optional FOR: if token after filename is AS, parse `OPEN filename AS #n [LEN=reclen]` with mode implied (Binary/COM).
3. **Tier 3:** MKx$/CVx and MBF already in builtins and codegen (MKI$, CVI, MKSMBF$, etc.). Add QB45 aliases MKIS$/CVIS etc. only if in spec; else mark complete.
4. **Tier 4:** External runtime has real _DEFLATE$/_INFLATE$. Inline runtime uses stubs. Add C compression module (miniz) in runtime/c_src for linking when building with inline runtime; document build steps.

## Implementation status

- **Tier 2 File I/O:** OPEN "COM1:9600,N,8,1" AS #n (FOR optional). AST `OpenFile.mode` is `Option<FileMode>`, parser accepts AS after filename, codegen uses Binary when mode is None. Parser test added.
- **Tier 2 Graphics:** _DEST/_SOURCE and PUT stubs present in codegen/runtime (qb_gfx_source, qb_gfx_dest, PUT actions). No code changes.
- **Tier 3:** MKx$/CVx and MBF already in builtins and codegen (MKI$, CVI, MKSMBF$, CVDMBF, etc.). Verified complete.
- **Tier 4:** Inline compression stubs wrapped in `#ifndef QB64FRESH_COMPRESSION_EXTERNAL`. Added runtime/c_src/compression.c (zlib-based qb_deflate/qb_inflate). docs/COMPRESSION.md added. Golden files updated.
- **Tier 1:** Marked optional in TODO_CONSOLIDATED; no implementation (only if binary compat needed).

## Code review (bugs fixed)

- **compression.c qb_deflate:** Single deflate(Z_FINISH) call could return Z_BUF_ERROR or Z_OK when output buffer was full; we were treating Z_OK as success and returning partial/corrupt data. Fixed: loop and grow buffer until Z_STREAM_END.
- **compression.c qb_inflate:** When hitting QB_INFLATE_MAX_OUT or realloc failure we broke and returned partial output; other paths didn’t call inflateEnd. Fixed: return empty on limit/realloc failure and call inflateEnd before returning; all error paths now free and inflateEnd.
