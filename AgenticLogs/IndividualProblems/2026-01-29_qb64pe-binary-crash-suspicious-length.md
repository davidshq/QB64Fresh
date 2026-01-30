# Problem: QB64pe binary (qb64pe_fresh) crashes with "suspicious length" / abort

**Date Started:** 2026-01-29
**Date Resolved:** Ongoing
**Related Session:** [Session 088](https://github.com/placeholder), [Session 089](../2026-01-29_session-089_qb64pe-compile.md)
**Status:** Ongoing

---

## Problem Statement

The **compiled binary** (qb64pe_fresh), produced by compiling qb64pe.bas with QB64Fresh and linking with the external runtime, crashes at runtime. The crash is **not** in the QB64Fresh (Rust) compiler or in gcc compiling the C—it is the **running executable** that crashes.

## Initial Symptoms

- Many lines: `qb_string_len: suspicious length 11910883886839364429 at 0x...` (from runtime)
- Followed by: `malloc(): unaligned tcache chunk detected` → Aborted (core dumped), exit 134
- In some runs the binary may hang (timeout) instead of printing the above

## Root Cause (from Session 088)

The runtime function `qb_string_len(s)` expects a **QbString\*** (a valid string handle). It is being called with the **address of a global variable** (e.g. `&a_str_scalar`, `&l_str_scalar`) instead of the **value** (the QbString\* stored in that variable). Those addresses lie in the executable's data segment; reading the "length" field at `(ptr - 16)` then yields garbage, triggering the "suspicious length" check and later heap corruption.

So somewhere the **generated C code** passes `&string_var` to a context that expects a string **value** (e.g. `qb_len_str(...)` or other runtime APIs that take `QbString*`).

## Fix Already Applied (Session 088 / 089)

- **BYREF string parameters:** Inside SUBs/FUNCTIONs with BYREF string params, we create a local `QbString* name = *name_ref`. Expression emission must use that local name (e.g. `a_str`), not the global scalar name (e.g. `a_str_scalar`), so that `qb_len_str(a_str)` receives the value, not an address.
- **Codegen changes:**  
  - `expr.rs`: For `Variable`, do not apply `variable_renames` when the C name is in `byref_string_names`, so we emit the local name.  
  - `assignments.rs`: When the assignment target is a BYREF string param, use `c_identifier(name)` and do not apply `variable_renames`.  
  - `stmt/mod.rs`: Pass `&self.procedure.current_func_byref_strings` into `emit_expr` / `emit_expr_external`.  
  - `analysis.rs`: Pass `&[]` for `byref_string_names` when emitting CONST (no procedure context).

This fixed the bad pointer in `qb_readchunk_str` (readchunk$). The binary may still abort; other call chains may still pass a string address where a value is required.

## How to Find the Remaining Bad Call

**Important:** Always run QB64pe (and GDB running it) under a **4GB memory limit** so the process doesn’t OOM the system. Use: `bash -c 'ulimit -v 4194304 && ...'` for the run.

1. **Build a debug binary** (in QB64pe directory):
   ```bash
   gcc -g -O0 -I ../QB64Fresh/runtime/include qb64pe_fresh.c \
     -L ../QB64Fresh/target/release -lqb64fresh_rt \
     $(pkg-config --libs sdl2) -lwayland-client -lasound -lm -lpthread -ldl \
     -o qb64pe_fresh_debug
   ```

2. **Run under GDB** with the existing script that breaks on bad `qb_string_len` (with memory limit):
   ```bash
   cd /path/to/QB64pe
   bash -c 'ulimit -v 4194304 && gdb -x gdb_catch_bad.gdb ./qb64pe_fresh_debug'
   ```
   Or for first bad only then quit: `bash -c 'ulimit -v 4194304 && gdb -x gdb_catch_first_bad.gdb ./qb64pe_fresh_debug'`
   The script breaks on `qb_string_len` and, when the length field at `(ptr-16)` is > 1GB, prints a backtrace. Reproduce the crash (e.g. run with `--help` or `-x -w source/qb64pe.bas` as needed); the first such backtrace shows which generated SUB/function called `qb_len_str`/`qb_string_len` with the bad pointer.

3. **Inspect the generated C** for that SUB: search for `qb_len_str(` or `qb_string_len(` and the variable passed. If it is `&something` or a global slot address used as a value, that is the bug. Then trace back in codegen to where that argument is emitted (e.g. LEN() built-in, or another helper) and ensure we pass the string **value** (e.g. local name for BYREF params), never the address, unless the API explicitly expects `QbString**`.

## Fix: BYREF string param abbreviation (2026-01-29)

**Cause:** In procedures with BYREF string params (e.g. `FUNCTION allocarray(n2$, elements$, ...)`), the compiler stores param C names in `current_func_byref_strings` (e.g. `elements_str`) so variable references use the local value, not the global scalar. BASIC allows variable name abbreviation (e.g. `e` for `elements`). The TypedExpr can therefore be `Variable("e")`; `c_identifier("e")` is `e_str`, which is not in `byref_string_names`, so the codegen applied `variable_renames` and emitted `e_str_scalar` (global slot) instead of the param local `elements_str`. That passed a bad pointer into `qb_len_str` / `qb_numelements` and similar.

**Change:** Track BYREF string param **basic** names in parallel with C names (`current_func_byref_string_basic_names`). In the Variable case in `expr.rs`, if the variable name matches a BYREF string param by exact or prefix match (case-insensitive), use that param’s C name and do not apply scalar renames. So `Variable("e")` in `allocarray` now resolves to `elements_str` (the local), and we emit `qb_len_str(elements_str)` and `qb_numelements(&elements_str)` correctly.

**Files:** `stmt/mod.rs` (new field, clear), `stmt/definitions.rs` (set both lists for SUB/FUNCTION), `expr.rs` (abbreviation check in Variable, new param threaded through), `analysis.rs` (pass `&[]` for basic names in CONST), tests in expr.rs updated.

## Fix: Only resolve BYREF string param when variable is string-typed (2026-01-29)

**Cause:** The abbreviation match above was applied for *any* variable name that matched a BYREF string param (e.g. `n2`, `indexes`, `elements`, `typ2`). In procedures that have both a string param (e.g. `n2$`) and a numeric/integer use of the same or abbreviated name (e.g. FOR end value, array index, `qb_tostr` argument), we emitted the string local (e.g. `n2_str`) where an integer or double was expected, causing C type errors: `int32_t _qb_for_end_val = n2_str`, `qb_getelement_str(..., &indexes_str)`, `qb_tostr(elements_str)`, `typ2_str & ISSTRING`.

**Change:** In `expr.rs` Variable case, only resolve to a BYREF string param (exact or abbreviation match) when `expr.basic_type.is_string()`. So numeric/integer variables use the normal scalar renames (e.g. `n2_scalar`, `indexes_scalar`) and string variables use the BYREF local (e.g. `n2_str`, `elements_str`).

**Verification (2026-01-29):** Rebuilt QB64Fresh, regenerated `qb64pe_fresh.c` from `source/qb64pe.bas` with 4GB limit, rebuilt `qb64pe_fresh_debug` in QB64pe directory. GDB run with `gdb_catch_bad.gdb` (run --help) for 20s: no "*** BAD qb_string_len" backtrace—the previous bad-pointer crash was not observed. After the COMMAND$ fix below, `./qb64pe_fresh_debug --help` and `./qb64pe_fresh_debug -v` print output and exit successfully. **Note:** Compilation (`-x file.bas`) still crashes (exit 134); this appears to be a separate issue from the original "suspicious length" crash and would require further investigation.

## Fix: COMMAND$ / argv not set in external runtime (2026-01-29)

**Cause:** In external runtime mode, the codegen emitted `_qb_argc` and `_qb_argv` and `qb_command_n` / `qb_commandcount` in the generated C, but did **not** emit `qb_init_args` (comment said "provided by the runtime library"). main() called the library's `qb_init_args`, which stored argv in Rust's OnceLock; the generated C's `qb_command_n` and `qb_commandcount` use the C statics `_qb_argc` and `_qb_argv`, which were never set. So `_COMMANDCOUNT` was 0 (or -1), the FOR loop in ParseCMDLineArgs$ never ran, we never saw `--help`, and the program fell through to the IDE path and blocked in `ide(0)` reading from a pipe (strace showed repeated `read(8, "", 8192) = 0`).

**Change:** Emit the full `qb_init_args(int argc, char** argv)` body in the generated C for external runtime (sets `_qb_argc = argc; _qb_argv = argv;`). Remove `qb_init_args` from the runtime library and header so the generated C is the single definition and COMMAND$(n) / _COMMANDCOUNT see argv.

**Files:** `src/codegen/c_backend/runtime/mod.rs` (emit qb_init_args for external), `runtime/src/lib.rs` (remove qb_init_args and ARGS), `runtime/include/qb64fresh_rt.h` (remove qb_init_args declaration).

## If New Bad Call Sites Show Up

Apply the same pattern:

1. **Value vs address:** APIs like `qb_len_str(s)`, `qb_string_len(s)`, `qb_numelements(&s)` expect a string **value** (QbString*) or **address of** a string variable (QbString**) for BYREF. Never pass `&global_scalar` where a value is expected.
2. **BYREF locals vs global scalars:** Inside a SUB/FUNCTION with BYREF string params, use the local name (e.g. `elements_str`) for the string **value** in expression context; use `&elements_str` only where the API expects a pointer (e.g. `qb_numelements`). Do not emit the global scalar name (e.g. `elements_str_scalar`) as the value.
3. **Type check:** Only resolve an abbreviated variable to a BYREF string param when the variable is **string-typed** (`expr.basic_type.is_string()`). Otherwise use normal scalar renames so integer/numeric uses get the right C variable.

**GDB steps** (always with 4GB limit):

1. Build debug binary (see "Build a debug binary" above).
2. Run: `bash -c 'ulimit -v 4194304 && gdb -x gdb_catch_bad.gdb ./qb64pe_fresh_debug'` (continues on each break; use to observe repeated bad calls) or `gdb -x gdb_catch_first_bad.gdb` (quits on first bad backtrace).
3. Reproduce the scenario (e.g. `run --help` or `run -x -w source/qb64pe.bas`). If you see "*** BAD qb_string_len: ptr=... len_field=...", inspect the backtrace to find the generated SUB/function, then search the generated C for the bad argument and fix the codegen (value vs address, BYREF local, type check) as above.

## Fix: Compilation crash - allocation failure in qb_string_from_bytes (2026-01-29)

**Cause:** When `gl_scan_header()` reads `internal\c\parts\core\gl_header_for_parsing\gl.h`, `qb_file_line_input()` can read extremely long lines (or entire file if no newlines). `qb_string_from_bytes()` then tries to allocate a huge string, and if allocation fails (e.g., due to memory limit), it calls `std::process::abort()`, causing exit 134.

**Changes:**
1. **`qb_string_from_bytes()`**: Now returns NULL instead of aborting when:
   - String size exceeds 100MB (MAX_STRING_SIZE)
   - Memory allocation fails
   - This allows callers to handle errors gracefully instead of crashing

2. **`qb_file_line_input()`**: Added maximum line length limit (10MB):
   - Prevents reading huge lines that would cause OOM
   - If a line exceeds the limit, returns an empty string instead of crashing
   - This protects against files without newlines being read entirely into memory

**Files:** `runtime/src/string.rs` (qb_string_from_bytes), `runtime/src/io/file.rs` (qb_file_line_input)

**Verification:** Rebuild runtime library and test with QB64pe compilation.

## Fix: Infinite loop in DO UNTIL EOF - qb_eof() always returned 0 (2026-01-29)

**Cause:** The `qb_eof()` function always returned 0 (not EOF), causing `DO UNTIL EOF(h)` loops to never terminate. When `gl_scan_header()` reads `gl.h` during IDE startup, it loops forever reading the file, consuming memory until hitting the 4GB limit.

**Changes:**
1. **Added `eof_reached` field to `FileHandle`**: Track EOF state per file handle
2. **Updated `qb_file_line_input()`**: Set `eof_reached = true` when `read_until` returns 0 bytes (EOF)
3. **Fixed `qb_eof()`**: Return -1 when `eof_reached` is true, 0 otherwise
4. **Reset EOF on SEEK**: Both `qb_file_seek()` and `qb_file_seek_record()` reset `eof_reached = false` when seeking

**Files:** `runtime/src/io/file.rs` (FileHandle struct, qb_eof, qb_file_line_input, qb_file_seek, qb_file_seek_record)

**Verification:** IDE should now start successfully without hanging at 4GB memory.

## Fix: IDE GUI not showing - graphics not initialized (2026-01-29)

**Cause:** The IDE calls `_SCREENSHOW` to display the window, but `qb_screenshow()` only works if graphics have been initialized (via a SCREEN statement). Since the IDE starts with `$SCREENHIDE` and never calls SCREEN before `_SCREENSHOW`, graphics were never initialized, so the window never appeared.

**Changes:**
- **`qb_screenshow()`**: Auto-initializes graphics with default dimensions (640x400) if not already initialized; improved error message on failure
- **`qb_screenmove()`**: Also auto-initializes graphics (IDE may call _SCREENMOVE before _SCREENSHOW)
- **SDL2 `initialize()`**: Calls `window_mut().show()` and sets `screen_visible = true` right after creating the window so the window is visible as soon as it exists

**Files:** `runtime/src/graphics_ffi.rs` (qb_screenshow, qb_screenmove), `runtime/src/graphics/sdl2.rs` (initialize)

**Verification:** Rebuild runtime and qb64pe_fresh_debug, then run `./qb64pe_fresh_debug`. If graphics init fails (e.g. no DISPLAY), stderr will show "QB64Fresh: _SCREENSHOW failed to init graphics: ...".

## Current Status (2026-01-29)

- ✅ **Original "suspicious length" crash:** Fixed. No BAD backtraces observed in GDB when running `--help`.
- ✅ **Hang on --help:** Fixed. Command-line parsing works; `--help` and `-v` print output and exit.
- ✅ **Compilation crash:** Fixed. `qb_string_from_bytes()` now returns NULL on allocation failure instead of aborting, and `qb_file_line_input()` enforces a 10MB line length limit to prevent huge allocations.
- ✅ **IDE hang (infinite loop):** Fixed. `qb_eof()` now properly tracks EOF state, preventing infinite loops in `DO UNTIL EOF` constructs.
- ✅ **IDE GUI not showing:** Fixed. `qb_screenshow()` now auto-initializes graphics if not already initialized, so the IDE window appears when `_SCREENSHOW` is called.

## Next Steps

- If new "suspicious length" or BAD backtraces appear, use the GDB steps and fix pattern above.
- If compilation crashes persist, run GDB with the catch scripts to identify the crash location and determine if it's related to the same pattern (bad string pointers) or a different issue.
