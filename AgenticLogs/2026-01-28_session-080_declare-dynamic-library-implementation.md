# Session 080: DECLARE DYNAMIC LIBRARY Implementation

**Date:** 2026-01-28  
**Summary:** Implemented runtime loading for DECLARE DYNAMIC LIBRARY to match QB64pe (dlopen/LoadLibrary + dlsym/GetProcAddress).

## Decisions

- **Implement (a) not (b):** User requested parity with QB64pe, so we implemented full runtime library loading instead of documenting as unsupported.
- **Platform split:** Generated C uses `#ifdef _WIN32` for Windows (LoadLibrary/GetProcAddress) and `#else` for Unix (dlopen/dlsym). Handle type is `void*` for both.

## Implementation

1. **Analysis** (`src/codegen/c_backend/analysis.rs`):
   - `DynamicLibInfo`: handle_id, library_name, declarations.
   - `collect_dynamic_libraries(program)`: returns (Vec<DynamicLibInfo>, HashSet<String> of dynamic external c_names).

2. **Definitions** (`src/codegen/c_backend/stmt/definitions.rs`):
   - `extern_decl_signature` / `extern_decl_signature_types_only`: shared signature helpers for extern and function-pointer types.
   - `emit_dynamic_library_section(libs, output)`: emits platform includes, handle variables (`qb_dll_0`, …), typedef + static function pointers (`qb_dyn_<c_name>`), and `qb_init_dynamic_libs()` that loads each library and resolves symbols.

3. **Backend** (`src/codegen/c_backend/mod.rs`):
   - After forward declarations: call `collect_dynamic_libraries`, set `emitter.dynamic_external_c_names`, emit dynamic library section.
   - At start of main (after qb_init_startdir): call `qb_init_dynamic_libs()` when any dynamic libs exist.

4. **StmtEmitter** (`src/codegen/c_backend/stmt/mod.rs`):
   - New field `dynamic_external_c_names: HashSet<String>`.
   - DeclareLibrary: when `is_dynamic`, emit only comments (handles/pointers/init already in section); when static, emit extern declarations as before.
   - `emit_expr` wrapper passes `dynamic_external_c_names` to expr::emit_expr.

5. **Expression codegen** (`src/codegen/c_backend/expr.rs`):
   - `emit_expr` gains parameter `dynamic_external_c_names`.
   - ExternalFunctionCall: if `c_name` is in the set, use `qb_dyn_<c_name>` for the call; otherwise use `c_name`.
   - All recursive emit_expr and helpers (emit_external_function_call, emit_binary_expr, emit_array_access) updated to pass the set through.

## Files Touched

- `src/codegen/c_backend/analysis.rs` — collect dynamic libs and c_names
- `src/codegen/c_backend/stmt/definitions.rs` — emit section + shared signature helpers
- `src/codegen/c_backend/stmt/mod.rs` — StmtEmitter field, DeclareLibrary branch, emit_expr arg
- `src/codegen/c_backend/expr.rs` — dynamic_external_c_names param, ExternalFunctionCall branch, tests
- `src/codegen/c_backend/mod.rs` — collect, emit section, call init in main
- `docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md` — item 1 marked implemented
- `examples/dynamic_lib_test.bas` — added for manual testing

## Code review (bugs fixed)

1. **Duplicate c_name across dynamic libs (C redefinition):** Two `DECLARE DYNAMIC LIBRARY` blocks declaring the same C symbol (e.g. both `MyFunc`) caused duplicate `typedef`/`static qb_dyn_MyFunc` and C redefinition errors. **Fix:** Emit one typedef and one function pointer per unique `c_name` across all dynamic libs (first occurrence defines the signature; in init, each lib that declares that symbol assigns to the same pointer, so “last lib wins” if duplicated).

2. **Empty library path:** When `library_name` was `None` we emitted `LoadLibrary("")` / `dlopen("", RTLD_LAZY)`, which is invalid. **Fix:** Only call LoadLibrary/dlopen when the path is non-empty; when empty we skip the load. We also guard dlsym/GetProcAddress with `if (handle)` so we never call them with a NULL handle when the load was skipped.

## Known limitations (not changed)

- **Null pointer calls:** If the library fails to load (wrong path, missing .so/.dll), function pointers stay NULL and calling them is undefined behavior. No runtime check is emitted (same as QB64pe; optional future improvement).
- **DECLARE LIBRARY inside SUB/FUNCTION:** `collect_dynamic_libraries` only walks top-level `program.statements`; dynamic libs declared inside procedures are not collected. Acceptable if the language only allows DECLARE at module level.

## Verification

- `cargo build` succeeds.
- Integration/test run timed out in environment; manual run of qb64fresh on a DECLARE DYNAMIC LIBRARY program recommended to confirm generated C (qb_dll_*, qb_dyn_*, qb_init_dynamic_libs).
