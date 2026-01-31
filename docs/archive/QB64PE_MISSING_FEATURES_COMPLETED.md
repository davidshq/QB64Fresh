# QB64pe Missing Features — Completed Items (Archive)

**Moved:** 2026-01-30 from [docs/ThingsToDo/QB64PE_MISSING_FEATURES.md](../ThingsToDo/QB64PE_MISSING_FEATURES.md)  
**Purpose:** Record of QB64pe bootstrap / missing-features work that has been implemented. The active doc now lists only remaining gaps.

---

## Why QB64pe hung (fixed)

These causes were identified and have been addressed:

1. **Missing `DEFLNG A-Z`** — ✅ **FIXED** — QB64pe assumes all variables without suffixes are LONG
2. **Missing `_OS$` and platform detection** — ✅ **FIXED** — QB64pe uses `$IF` blocks based on `_OS$`
3. **Missing `Version$`** — ✅ **FIXED** — Used in initialization
4. **Event trapping runtime not implemented** — ✅ **FIXED** — `ON KEY`, `ON TIMER`, `ON UEVENT`, `ON STRIG` fully implemented with runtime support
5. **Graphics initialization** — ✅ **IMPLEMENTED** — Graphics initialization via `init_graphics()` function in runtime
6. **File I/O for internal files** — ✅ **IMPLEMENTED** — `resolve_file_path()` handles `internal/` directory relative to executable or CWD

---

## Priority for implementation (completed)

### High Priority (Blocks Execution)
- [x] **`DEFLNG A-Z` and DEFTYPE** — Critical for variable type inference
- [x] **`_OS$` built-in** — Required for platform detection in `$IF` blocks
- [x] **`Version$` built-in** — Used in initialization
- [x] **`$USELIBRARY` implementation** — Library system with AtTop/AfterMain/AtBottom inclusion
- [x] **Graphics initialization** — Graphics initialization via `init_graphics()` function in runtime

### Medium Priority (Causes Incorrect Behavior)
- [x] **Event trapping runtime (`ON KEY`, `ON TIMER`, `ON UEVENT`, `ON STRIG`)** — Fully implemented with runtime support
- [x] **`$INCLUDEONCE`** — Prevents duplicate includes
- [x] **`$EMBED`** — Embedded files collected and emitted as C arrays
- [x] **`_DIREXISTS` and file system functions** — Used for internal folder checks

### Low Priority (Nice to Have)
- [x] **`$COLOR` directives** — Generates comments in codegen (LSP metadata)
- [x] **`$ASSERTS`** — Emits C code to enable assertions
- [x] **`$VERSIONINFO` / `$EXEICON`** — Windows resource generation (`.rc`, `manifest.h`, `.manifest`)
- [x] **`$NOPREFIX`** — Generates comment in codegen

---

## Implementation details (former §13)

Details for features that required design decisions or user input. All items below are implemented.

### Preprocessor directives

#### `$USELIBRARY:'author/library'` — ✅ Implemented (2026-01-29)

- **What it does:** QB64pe's library system: AtTop/AfterMain/AtBottom inclusion.
- **Implementation:** File-based discovery from `libraries/descriptors/{author/library}.ini`, QB64pe format, three inclusion points, duplicate prevention, validation.
- **Files:** `src/library.rs`, `src/preprocessor.rs`, `src/lib.rs`.

#### `$EMBED:'filename'` — ✅ Implemented

- **What it does:** Embeds binary files; `_EMBEDDED$(handle$)` retrieves at runtime.
- **Implementation:** Preprocessor reads files; codegen emits C binary arrays and `qb_embedded()`; duplicate-handle and not-found errors.
- **Files:** `src/preprocessor.rs`, `src/codegen/c_backend/mod.rs`, `src/semantic/builtins.rs`, `src/codegen/c_backend/expr.rs`.

#### `$VERSIONINFO:key=value` and `$EXEICON:'filename'` — ✅ Fully implemented

- **What it does:** Windows resource files (version, icon, manifest).
- **Implementation:** `icon.rc`, `manifest.h`, `{basename}.manifest`; keys CompanyName, FileDescription, FILEVERSION#, etc.
- **Files:** `src/codegen/c_backend/resources.rs`, `src/main.rs`.

#### `$COLOR:0` and `$COLOR:32` — ✅ Implemented (LSP metadata)

- **What it does:** IDE syntax highlighting mode.
- **Implementation:** Parsed in directives, stored in `AnalysisCache.color_mode`, emitted as comment in codegen.

#### `$ASSERTS` and `$ASSERTS:CONSOLE` — ✅ Implemented

- **What it does:** Debug assertions; CONSOLE sends failures to stderr.
- **Implementation:** `qb_assert()`, preprocessor variables `_ASSERTS_`/`_CONSOLE_`, runtime flags.
- **Files:** `src/lexer/token.rs`, `src/parser/directives.rs`, `src/codegen/c_backend/runtime/error.rs`, `src/codegen/c_backend/stmt/meta.rs`, plus AST/semantic/checker updates.

#### `$STATIC` and `$DYNAMIC` — ✅ Implemented (2026-01-29)

- **What it does:** Static vs dynamic array allocation.
- **Implementation:** Static arrays as fixed-size C arrays (global/local); `array_mode_static` in checker; `is_static` on `TypedDimVariable`.
- **Files:** `src/semantic/checker/mod.rs`, `src/semantic/typed_ir.rs`, `src/codegen/c_backend/stmt/definitions.rs`, `src/codegen/c_backend/types.rs`, `src/codegen/c_backend/analysis.rs`.

### Event trapping — ✅ Implemented (2026-01-29)

- **What it does:** `ON KEY`, `ON TIMER`, `ON UEVENT` (and stubs for ON COM, ON PEN, ON STRIG).
- **Implementation:** Polling-based event queue, SDL2 keyboard, timer registry, UEVENT flag; FFI in `qb64fresh_rt.h`, `runtime/src/events.rs`, integration in `graphics/sdl2.rs` and codegen.

### ON COM / ON PEN — ✅ Implemented (runtime stubbed) (2026-01-31)

- **What it does:** `ON COM(n) GOSUB label`, `ON PEN GOSUB label` — serial port and light pen event trapping.
- **Implementation:** Parser, AST, typed IR, and codegen implemented; runtime emits fprintf "not implemented" (serial/light pen would need platform-specific hardware support). Status reflected in QB64PE_MISSING_FEATURES.md as implemented with stubbed runtime.

### Error handling (RESUME, ERL, ERR) — ✅ Implemented (2026-01-31)

- **What it does:** `RESUME`, `RESUME NEXT`, `RESUME label`; `ERL` (error line); `ERR` / `ERROR` (error code).
- **Implementation:** Full codegen in `error_jump.rs` and inline/external runtime: `_qb_error_line` for retry, `qb_clear_error()` for RESUME NEXT, `qb_err_code()`/`qb_err_line()`, `qb_error(code)`. ERL may be 0 when error originates from runtime (e.g. file I/O) if BASIC line number is not passed. Status in QB64PE_MISSING_FEATURES.md updated from "Partially implemented" to "Implemented".

### Graphics initialization — ✅ Complete

- **What it does:** Ensures graphics window init and `$SCREENHIDE` / `_SCREENSHOW` / `_SCREENHIDE` behave correctly.
- **Implementation:** `has_screen_hide()` in analysis; SCREEN codegen checks `qb_gfx_screen()` return and calls `qb_screenhide()` when requested; `screen_hide_requested` on StmtEmitter.
- **Files:** `src/codegen/c_backend/analysis.rs`, `src/codegen/c_backend/stmt/graphics.rs`. Test: `examples/test_screenhide.bas`.

### File I/O for internal files — ✅ Implemented

- **What it does:** Resolves `internal/` paths relative to executable or CWD so QB64pe can find its files.
- **Implementation:** `resolve_file_path()` in runtime; `qb_init_startdir()`; failed opens log warnings and continue.
- **Files:** `runtime/src/io/file.rs`.

---

*For current remaining items, see [QB64PE_MISSING_FEATURES.md](../ThingsToDo/QB64PE_MISSING_FEATURES.md).*
