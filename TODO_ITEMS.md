# QB64Fresh TODO Items

*Last updated: 2026-01-26*

This document lists all TODO items found in the QB64Fresh codebase, organized by category.

## Code Generation

### Graphics - Line Style Pattern
- **File:** `src/codegen/c_backend/stmt/mod.rs:1484`
- **Item:** Implement line style pattern support
- **Context:** LINE statement style parameter is currently ignored

### Array I/O - Multi-dimensional Support
- **File:** `src/codegen/c_backend/stmt/io.rs:89`
- **Item:** Handle multi-dimensional arrays in I/O statements
- **Context:** Currently only handles first index for 1D array syntax (TODO: handle multi-dim)

### String Runtime - Debug Logging
- **File:** `src/codegen/c_backend/runtime/strings.rs:111`
- **Item:** Consider adding debug logging/warning when string operations occur
- **Context:** Comment in generated C code suggesting future enhancement for string leak detection

## Runtime Library

### Graphics - Per-Image Palettes
- **File:** `runtime/src/graphics_ffi.rs:1056`
- **Item:** Support per-image palettes in `qb_palettecolor_get`
- **Context:** Currently ignores handle parameter and uses current palette

- **File:** `runtime/src/graphics_ffi.rs:1086`
- **Item:** Support per-image palettes with handle in `qb_palettecolor`
- **Context:** SET operation for palette needs handle support

### Graphics - STEP Behavior
- **File:** `runtime/src/graphics_ffi.rs:1707`
- **Item:** Track last graphics position for proper STEP behavior
- **Context:** STEP modifier in graphics commands needs last position tracking

### Graphics - Scrolling
- **File:** `runtime/src/graphics/sdl2.rs:1811`
- **Item:** Scroll if needed (text output)
- **Context:** When cursor moves past bottom, should scroll

- **File:** `runtime/src/graphics/sdl2.rs:1840`
- **Item:** Implement actual scrolling
- **Context:** Cursor row adjustment needs actual scrolling implementation

## Testing

### Bootstrap Tests - Golden File Comparison
- **File:** `tests/bootstrap_tests.rs:279`
- **Item:** Implement golden file comparison for QB64pe subset
- **Context:** Test is marked `#[ignore]` with `todo!()` macro until golden file strategy is decided

### Bootstrap Tests - Full Execution
- **File:** `tests/bootstrap_tests.rs:353` (approximate)
- **Item:** Once runtime library is built and QB64pe executable exists, implement full execution test
- **Context:** Requires building runtime with graphics support and compiling QB64pe C output

### Integration Tests - INSTR Function
- **File:** `tests/integration_tests.rs:2054`
- **Item:** Add support for 2-argument form `INSTR(string, search)`
- **Context:** Currently only supports 3-argument form `INSTR(start, string, search)`

## Debugger

### Expression Evaluation
- **File:** `tools/debug/src/server.rs:814`
- **Item:** Implement expression evaluation
- **Context:** Debug adapter protocol needs expression evaluation for watch variables

## Documentation

### Main TODO File
- **File:** `TODO.md`
- **Status:** Contains Phase 6 optimization items:
  - Dead code elimination
  - Loop optimization
  - Inline small functions

## Summary

**Total TODO Items Found:** 12 code-level TODOs + 3 from TODO.md

**By Category:**
- Code Generation: 3 items
- Runtime Library: 5 items
- Testing: 3 items
- Debugger: 1 item
- Documentation: 1 file (TODO.md with 3 Phase 6 optimization items)

**Priority Areas:**
1. Graphics features (palettes, scrolling, STEP behavior) - 5 items
2. Code generation improvements (array I/O, line style pattern) - 2 items
3. Testing infrastructure - 3 items
4. Debugger functionality - 1 item

**Note:** Line number tracking is already implemented. The `Span` structure stores actual line numbers (1-indexed) in the `line` field, and code generation uses `stmt.span.line` for debug line hooks.
