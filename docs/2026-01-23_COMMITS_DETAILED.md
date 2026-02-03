# Commits on 2026-01-23 — Detailed

This document lists every commit made on January 23, 2026, in chronological order, with full commit messages and file-change summaries. These commits continue Phase C (codegen fixes, LSP, runtime, refactors).

---

## Branch application status

**Applied to this branch:** **Commits #1–#9** have been applied. For each applied commit, **Brought** and **Not brought** are listed in that commit’s section below so partial applies are explicit.

**Not brought into this branch (commits 10–25):** The following Jan 23 commits have *not* been applied; their details remain below for reference or future cherry-pick:

| # | Hash     | Summary |
|---|----------|--------|
| 10| faf81e5  | Docs: bootstrap plan fixes |
| 11| a8d6bb0  | LSP workspace symbol search |
| 12| 18560d1  | SELECT CASE string comparison |
| 13| b8ecc12  | Lint complexity, formatter blank lines |
| 14| 0e6f494  | Docs: README and TODO status |
| 15| d8c5f3e  | REDIM SHARED, STRING$, fixed-length strings |
| 16| a438673  | Array scoping (main=globals, procedures=locals) |
| 17| 04cec5a  | Debugger scaffold (tools/debug) |
| 18| d4e02ed  | STUB_FUNCTIONS.md line counts |
| 19| bbfe484  | Variable-arg built-ins (_CONSOLE, _MAPUNICODE, etc.) |
| 20| f1a259f  | String init, stub forward declarations |
| 21| 9e0d624  | REDIM runtime expressions |
| 22| 370120c  | Docs reorganize, lint End/System pattern |
| 23| cc43b8f  | MK* memory bugs, EXIT FUNCTION return |
| 24| ca65c3e  | Testing infra (_EXIT, _MAPUNICODE, golden) |
| 25| efe10f3  | Bootstrap achievement docs and test suite |

---

## 1. `45d875d` — Refactor: split large codegen files into directory modules — **APPLIED**

**Subject:** refactor: split large codegen files into directory modules

**What changed:**

Split runtime.rs (5,660 lines) and stmt.rs (4,215 lines) into directory module structures for better maintainability and AI-assisted development.

**runtime/** (15 files, ~500–900 lines each):
- mod.rs: Public interface, emit_runtime() orchestration
- types.rs, strings.rs, io.rs, math.rs, file.rs
- error.rs, keyboard.rs, memory.rs, timing.rs
- arrays.rs, audio.rs, graphics.rs, legacy.rs, system.rs

**stmt/** (8 files):
- mod.rs: StmtEmitter struct, emit_stmt() dispatcher
- assignments.rs, control_flow.rs, data.rs, def_fn.rs
- definitions.rs, error_jump.rs, io.rs

**Stats:** 24 files changed, 8309 insertions(+), 7362 deletions(-)

**Brought:** Full refactor; all 24 file changes applied (runtime/ and stmt/ directory modules; deleted monolithic runtime.rs and stmt.rs).

**Not brought:** Nothing.

---

## 2. `23a5fa5` — Docs: update graphics documentation for external runtime mode — **APPLIED**

**Subject:** docs: update graphics documentation for external runtime mode

**What changed:**

- Fix outdated function name in GRAPHICS.md (qb_gfx_line_ex → qb_gfx_line_step)
- Add "Runtime Modes" section explaining inline vs external runtime
- Document how to build and link with external runtime library
- Document qb64fresh_rt.h header and compatibility macros
- Expand CLAUDE.md runtime modes section with build commands and key files

**Stats:** 2 files changed, 90 insertions(+), 4 deletions(-)

**Brought:** docs/GRAPHICS.md: qb_gfx_line_ex→qb_gfx_line_step in example, full "Runtime Modes" section (inline/external, build steps, qb64fresh_rt.h, compatibility macros), "Last updated" set to 2026-01-23.

**Not brought:** CLAUDE.md edits (equivalent content was already present from the earlier post-reset restore; no duplicate change applied).

---

## 3. `e52b630` — Feat: enable graphics in external runtime mode + obsolete function errors — **APPLIED (good parts)**

**Subject:** feat: enable graphics in external runtime mode + obsolete function errors

**What changed:**

**Graphics External Runtime Support:**
- Add graphics function declarations to qb64fresh_rt.h (100+ functions)
- Add qb_gfx_screen() FFI function mapping classic SCREEN modes (0–13)
- Fix codegen naming: qb_gfx_line_ex → qb_gfx_line_step, qb_gfx_box_ex → qb_gfx_box_step
- Add program initialization FFI: qb_init_args, qb_init_startdir, _qb_init_palette
- Add compatibility macros for inline runtime naming (qb__rgb32 → qb_rgb)

**Obsolete Function Errors:** FRE, SETMEM, IOCTL$, and FILEATTR are legacy BASIC functions with no meaningful purpose on modern systems. Compile-time errors match QB64pe's "Command not implemented" behavior.
- Add CommandNotImplemented error variant in semantic/error.rs
- Check for unimplemented functions in expressions.rs and statements.rs

**Stats:** 9 files changed, 314 insertions(+), 21 deletions(-)

**Brought:** Semantic: `CommandNotImplemented` in error.rs and span(); checks in expressions.rs (FRE, IOCTL$, SETMEM, FILEATTR) and statements.rs (SETMEM). Runtime: qb64fresh_rt.h (init decls, compatibility macros, full graphics block); graphics_ffi.rs `qb_gfx_screen()`; lib.rs init (qb_init_args, qb_init_startdir, _qb_init_palette). Tests: integration_tests.rs setmem_statement now expects compile error.

**Not brought:** src/codegen/c_backend/stmt.rs LINE/BOX naming (qb_gfx_line_ex→qb_gfx_line_step, qb_gfx_box_ex→qb_gfx_box_step)—already present in this branch in stmt/mod.rs from commit #1. docs/ThingsToDo/STUB_FUNCTIONS_REMAINING.md (file not present on this branch; only STUB_FUNCTIONS.md exists).

---

## 4. `d3a912f` — Feat: implement VGA palette port emulation for INP/OUT/WAIT — **APPLIED (good parts; most already present)**

**Subject:** feat: implement VGA palette port emulation for INP/OUT/WAIT

**What changed:**

Implements VGA palette port emulation matching QB64pe behavior:
- Port 0x3C7: Set palette read index
- Port 0x3C8: Set palette write index
- Port 0x3C9: Read/write RGB values (0–63 range, cycles R/G/B)
- Port 0x3DA: Vertical retrace status (bit 3)

Enables legacy BASIC programs that manipulate VGA palettes. Other ports safely return 0 or no-op.

Also: IOCTL statement compiles to stub (was erroring); STUB_FUNCTIONS docs updated; completed functions (LPOS, PEEK/POKE, core audio) moved to FULL.md.

**Stats:** 13 files changed, 3521 insertions(+), 718 deletions(-)

**Brought:** Comment in semantic/checker/statements.rs for IOCTL statement (stub / no-op, matching QB64pe). VGA palette emulation (INP/OUT/WAIT, ports 0x3C7/0x3C8/0x3C9/0x3DA), _qb_init_palette() call in main init, and IOCTL compiling to stub were already present on this branch (in runtime/legacy.rs and codegen mod.rs from the refactor).

**Not brought:** docs/ThingsToDo/STUB_FUNCTIONS_FULL.md and STUB_FUNCTIONS_REMAINING.md (STUB_FUNCTIONS_REMAINING not on branch; FULL may differ). tests/golden/*.golden (9 files; likely format/header churn, not re-applied).

---

## 5. `7994f91` — Feat: add SessionStart hook to auto-load critical AST context — **APPLIED**

**Subject:** feat: add SessionStart hook to auto-load critical AST context

**What changed:**

Adds a Claude Code hook that automatically loads key type definitions at session start:
- src/ast/expr.rs (full) — ExprKind variants
- src/ast/stmt.rs (first 200 lines) — StatementKind variants
- src/semantic/typed_ir.rs (first 250 lines) — Typed IR structure

Addresses the context recovery issue documented in CLAUDE.md.

**Stats:** 2 files changed, 55 insertions(+)

**Brought:** .claude/hooks/load-context.sh (SessionStart script that cats expr.rs, head -200 stmt.rs, head -250 typed_ir.rs); .claude/settings.json (SessionStart hook registration).

**Not brought:** Nothing.

---

## 6. `ce2fc17` — Docs: add comprehensive language reference, fix codegen for function overloads — **APPLIED (codegen only; docs not brought)**

**Subject:** docs: add comprehensive language reference, fix codegen for function overloads

**What changed:**

**Documentation:**
- Add complete LANGUAGE_REFERENCE.md (3376 lines) covering all QB64 syntax
- Reorganize stub function tracking (STUB_FUNCTIONS_REMAINING.md)
- Update ARCHITECTURE, TESTING, and BOOTSTRAP docs

**Code generation fixes:**
- Handle _RGB32 with 3 vs 4 arguments (qb__rgb32 vs qb__rgb32_4)
- Handle SCREEN function with 2 vs 3 arguments
- Fix fixed-length string conversion for built-in functions
- Add qb_file_get_string for proper string binary I/O
- Expand runtime with additional helper functions

**Stats:** 16 files changed, 4637 insertions(+), 362 deletions(-)

**Brought:** Codegen fixes were already present (see above). In a follow-up pass, docs from this commit were brought: docs/LANGUAGE_REFERENCE.md (3376 lines, full syntax reference), docs/ARCHITECTURE.md, docs/DEVELOPMENT.md, docs/TESTING.md (contents from commit ce2fc17).

**Not brought:** README.md, TODO.md; docs/ThingsToDo/BOOTSTRAP_PLAN_REMAINING.md, CODEBASE_REVIEW_CONSOLIDATED.md, FUTURE.md; STUB_FUNCTIONS→STUB_FUNCTIONS_FULL rename and STUB_FUNCTIONS_REMAINING.md; docs/archive/BOOTSTRAP_PLAN_FULL.md; commit’s runtime.rs delta (we use runtime/ directory).

---

## 7. `945c9a2` — Fix: update tests for QbString signatures, fix FOR loop variable scope — **APPLIED (good parts)**

**Subject:** fix: update tests for QbString signatures, fix FOR loop variable scope

**What changed:**

**Tests:** Update io.rs tests to use QbString instead of raw c_char pointers; regenerate golden files for codegen changes.

**FOR loops:** FOR loop variable retains value after loop ends (BASIC semantics); assign start value before loop rather than in loop initializer.

**Stats:** 10 files changed, 1877 insertions(+), 167 deletions(-)

**Brought:** FOR loop semantics were already present in stmt/control_flow.rs (assign start before loop; for (; condition; step)). Runtime tests in runtime/src/io.rs updated to use QbString: qb_string_new + qb_string_data(path) for qb_file_exists/qb_dir_exists/qb_dir calls, then qb_string_release (API kept as *const c_char, tests exercise QbString creation/release).

**Not brought:** tests/golden/*.golden (9 files); regeneration would require re-running codegen and can be done separately if desired.

---

## 8. `00ae288` — Docs: update TODO with rename symbol and path handling completion — **APPLIED**

**Subject:** docs: update TODO with rename symbol and path handling completion

**Stats:** 1 file changed, 3 insertions(+), 3 deletions(-)

**Brought:** TODO.md Low Priority: Unicode support text updated to note UCASE$/LCASE$ UTF-8 safe and char counting helpers; Windows-specific path handling marked [x] with note “fixed type mismatch in declarations”.

**Not brought:** Nothing (commit only touched TODO; we don’t have a separate “Rename symbol” checkbox line in our TODO structure).

---

## 9. `e6374bf` — Feat: add LSP rename, UTF-8 support, fix file I/O declarations — **APPLIED (LSP rename only)**

**Subject:** feat: add LSP rename, UTF-8 support, fix file I/O declarations

**What changed:**

**LSP:** textDocument/rename and textDocument/prepareRename; leverage find_references() for case-insensitive rename.

**UTF-8:** UCASE$/LCASE$ only convert ASCII a-z/A-Z (preserves UTF-8); qb_utf8_char_count(), qb_utf8_char_to_byte(), qb_strlen_chars().

**File I/O:** Fix declaration type mismatch for qb_chdir, qb_mkdir, qb_file_kill; qb_file_exists and qb_dir_exists accept QbString*; bounds registration for UBOUND/LBOUND.

**Stats:** 5 files changed, 496 insertions(+), 45 deletions(-)

**Brought:** LSP: get_identifier_at_position(), rename_provider capability (RenameOptions with prepare_provider), prepare_rename and rename handlers using find_references(); TODO.md updated per #8.

**Not brought:** UTF-8 (UCASE$/LCASE$ ASCII-only, qb_utf8_char_count, qb_utf8_char_to_byte, qb_strlen_chars). Runtime API change (qb_file_exists/qb_dir_exists/qb_dir to accept QbString*) and codegen/header updates. Codegen declaration fixes for qb_chdir, qb_mkdir, qb_file_kill and bounds registration for UBOUND/LBOUND (commit touched monolithic runtime.rs/stmt.rs; we have runtime/ and stmt/ directories—could be ported later).

---

## 10. `faf81e5` — Docs: update bootstrap plan with today's critical fixes

**Subject:** docs: update bootstrap plan with today's critical fixes

**Stats:** 1 file changed, 32 insertions(+), 3 deletions(-)

---

## 11. `a8d6bb0` — Feat(lsp): add workspace symbol search

**Subject:** feat(lsp): add workspace symbol search

**What changed:** LSP workspace/symbol support for symbol search across the workspace.

**Stats:** 2 files changed, 46 insertions(+), 3 deletions(-)

---

## 12. `18560d1` — Fix(codegen): SELECT CASE string comparison and docs update

**Subject:** fix(codegen): SELECT CASE string comparison and docs update

**What changed:**

- SELECT CASE string comparison uses qb_string_compare
- Command-line args fixed (qb_init_args); module-level scoping (NoIDEMode, etc.)
- NULL string semantics fixed (NULL == "")

QB64pe now parses command-line arguments, recognizes -c/-x flags, enters compiler mode, attempts to compile BASIC source files.

**Stats:** 1 file changed, 32 insertions(+), 3 deletions(-)

---

## 13. `b8ecc12` — Feat(tools): add complexity lint rules and formatter blank line normalization

**Subject:** feat(tools): add complexity lint rules and formatter blank line normalization

**What changed:** tools/fmt formatter blank line normalization; tools/lint complexity rules (complexity.rs) and style rules (style.rs).

**Stats:** 5 files changed, 461 insertions(+), 1 deletion(-)

---

## 14. `0e6f494` — Docs: update README and TODO with current status

**Subject:** docs: update README and TODO with current status

**Stats:** 2 files changed, 54 insertions(+), 25 deletions(-)

---

## 15. `d8c5f3e` — Fix(codegen): REDIM SHARED, STRING$ numeric form, fixed-length strings

**Subject:** fix(codegen): REDIM SHARED, STRING$ numeric form, fixed-length strings

**What changed:** REDIM SHARED handling, STRING$(n) numeric form, fixed-length string codegen fixes (expr.rs, implicit_vars.rs, runtime.rs).

**Stats:** 3 files changed, 67 insertions(+), 16 deletions(-)

---

## 16. `a438673` — Fix(codegen): array scoping — main uses globals, procedures use locals

**Subject:** fix(codegen): array scoping - main uses globals, procedures use locals

**What changed:**

Arrays declared in main program allocate to existing globals instead of shadowing locals. Main vs SUB/FUNCTION: main allocates to globals for cross-function sharing; SUB/FUNCTION always create local for DIM inside procedure.

- implicit_vars.rs: is_main_program parameter to collect_implicit_locals
- stmt.rs: emit_dim checks current_proc.is_none() for main context
- mod.rs: pass is_main_program=true for main

Before: QB64pe crashed in IdeMakeFileMenu (NULL menu_str). After: runs past menu init (fails on 'internal' folder).

**Stats:** 3 files changed (implicit_vars.rs, mod.rs, stmt.rs), plus BOOTSTRAP_PLAN_REMAINING.md

---

## 17. `04cec5a` — Feat(tools): add debugger scaffold for parallel development

**Subject:** feat(tools): add debugger scaffold for parallel development

**What changed:**

Add qb64fresh-debug as workspace member (same pattern as linter/formatter). AST-level debugger for parallel development:
- Source loading and parsing with line-to-statement mapping
- Breakpoint management (line, function, label, conditional)
- Interactive CLI with standard debugger commands
- Configuration (TOML)
- Stub execution control for future runtime integration

**Stats:** 6 files changed, 1485 insertions(+), 1 deletion(-)

---

## 18. `d4e02ed` — Docs: update STUB_FUNCTIONS.md with accurate line counts

**Subject:** docs: update STUB_FUNCTIONS.md with accurate line counts

**What changed:** Sync docs with codebase: runtime.rs, string.rs, math.rs, io.rs, graphics_ffi.rs, audio_ffi.rs line counts; external runtime total; qb64fresh_rt.h reference; graphics/font.rs in table.

**Stats:** 1 file changed, 23 insertions(+), 18 deletions(-)

---

## 19. `bbfe484` — Fix(codegen): handle variable argument count for built-in functions

**Subject:** fix(codegen): handle variable argument count for built-in functions

**What changed:**

Support for built-ins with optional/variable args:
- _CONSOLE: qb_console_get() 0 args, qb_console(mode) 1 arg
- _MAPUNICODE: qb__mapunicode1/2/3 for 1/2/3 args
- _ICON: qb_icon/icon1/icon2 for 0/1/2 args
- _ACCEPTFILEDROP: qb_acceptfiledrop/1 for 0/1 args

Also: Shell functions return int32_t when used as functions; qb_statuscode takes handle; logical_drives returns int32_t.

**Stats:** 3 files changed, 85 insertions(+), 13 deletions(-)

---

## 20. `f1a259f` — Fix: string initialization and stub forward declarations

**Subject:** fix: string initialization and stub forward declarations

**What changed:**

- Initialize global string variables to "" at program start (BASIC strings not NULL when uninitialized)
- Forward declarations for external stub functions (avoid implicit int on 64-bit)
- Declarations for: file system, console, shell, font, window, error, network, dialog, conversion

**Stats:** 2 files changed, 109 insertions(+)

---

## 21. `9e0d624` — Fix: support runtime expressions in REDIM dimensions

**Subject:** fix: support runtime expressions in REDIM dimensions

**What changed:**

- TypedRedimDimension struct with lower/upper as Option&lt;TypedExpr&gt;/TypedExpr
- TypedRedimVariable uses Vec&lt;TypedRedimDimension&gt;
- Codegen emits expressions for REDIM bounds; semantic checker produces TypedRedimDimension

Enables REDIM arr(n) where n is a variable.

**Stats:** 4 files changed, 75 insertions(+), 28 deletions(-)

---

## 22. `370120c` — Chore: reorganize docs and fix lint pattern matching

**Subject:** chore: reorganize docs and fix lint pattern matching

**What changed:**

- Move FUTURE.md and OPENGL_SUPPORT.md to docs/ThingsToDo/
- Update QB64PE_LANGUAGE_SPECIFICATION.md
- Fix lint correctness.rs: End/System use { .. } pattern (they have fields)

**Stats:** 4 files changed, 516 insertions(+), 8 deletions(-)

---

## 23. `cc43b8f` — Fix(codegen): fix memory bugs in MK* functions and EXIT FUNCTION

**Subject:** fix(codegen): fix memory bugs in MK* functions and EXIT FUNCTION

**What changed:**

**Runtime:** MKI$/MKL$/MKS$/MKD$ — data pointer set correctly (fixes segfaults); _TRIM$ same fix; qb_string_concat defensive checks for NULL/corrupt data.

**Codegen:** EXIT FUNCTION emits `return FuncName;` instead of bare `return;` (fixes undefined return when returning early).

Resolves segfaults when running QB64PE compiled by QB64Fresh.

**Stats:** 2 files changed, 45 insertions(+), 9 deletions(-)

---

## 24. `ca65c3e` — Fix: resolve testing infrastructure failures

**Subject:** fix: resolve testing infrastructure failures

**What changed:**

- _EXIT: combine function (0 args) and sub (1 arg) into single function with optional parameter
- Zero-arg calls: use required_param_count() == 0 instead of params.is_empty() (e.g. _STATUSCODE)
- _MAPUNICODE parser: accept TO and , as separators (_MAPUNICODE 8364, 128)
- test_invalid_binary_op: use STRING - INTEGER (invalid) instead of STRING + INTEGER (now valid)
- Update golden test files for current codegen

Test results: 718 integration, 10 golden, 388 unit tests passing.

**Stats:** 12 files changed, 2208 insertions(+), 91 deletions(-)

---

## 25. `efe10f3` — Docs: add bootstrap achievement documentation and test suite

**Subject:** docs: add bootstrap achievement documentation and test suite

**What changed:**

**Documentation:**
- BOOTSTRAP_ACHIEVEMENT.md: technical summary (59K lines BASIC → 2.1MB exe in 800ms)
- BEHAVIORAL_DIFFERENCES.md: QB64Fresh vs QB64pe semantics
- MIGRATION_GUIDE.md, README.md bootstrap section and metrics
- BOOTSTRAP_PLAN_REMAINING.md simplified; BOOTSTRAP_PLAN_FULL.md session history through Phase D

**Tests:** tests/bootstrap_tests.rs (compilation and regression); scripts/test-bootstrap.sh

**Stats:** 8 files changed, 996 insertions(+), 516 deletions(-)

---

## Summary

| # | Hash     | Focus |
|---|----------|--------|
| 1 | 45d875d  | Refactor: split codegen runtime.rs and stmt.rs into directory modules |
| 2 | 23a5fa5  | Docs: graphics external runtime mode |
| 3 | e52b630  | Graphics external runtime, obsolete function errors |
| 4 | d3a912f  | VGA palette port emulation (INP/OUT/WAIT) |
| 5 | 7994f91  | SessionStart hook for AST context |
| 6 | ce2fc17  | LANGUAGE_REFERENCE.md, function overload codegen |
| 7 | 945c9a2  | QbString tests, FOR loop variable scope |
| 8 | 00ae288  | Docs: TODO rename/path completion |
| 9 | e6374bf  | LSP rename, UTF-8, file I/O declarations |
| 10| faf81e5  | Docs: bootstrap plan fixes |
| 11| a8d6bb0  | LSP workspace symbol search |
| 12| 18560d1  | SELECT CASE string comparison |
| 13| b8ecc12  | Lint complexity, formatter blank lines |
| 14| 0e6f494  | Docs: README and TODO status |
| 15| d8c5f3e  | REDIM SHARED, STRING$, fixed-length strings |
| 16| a438673  | Array scoping (main=globals, procedures=locals) |
| 17| 04cec5a  | Debugger scaffold (tools/debug) |
| 18| d4e02ed  | STUB_FUNCTIONS.md line counts |
| 19| bbfe484  | Variable-arg built-ins (_CONSOLE, _MAPUNICODE, etc.) |
| 20| f1a259f  | String init, stub forward declarations |
| 21| 9e0d624  | REDIM runtime expressions |
| 22| 370120c  | Docs reorganize, lint End/System pattern |
| 23| cc43b8f  | MK* memory bugs, EXIT FUNCTION return |
| 24| ca65c3e  | Testing infrastructure (_EXIT, _MAPUNICODE, golden) |
| 25| efe10f3  | Bootstrap achievement docs and test suite |
