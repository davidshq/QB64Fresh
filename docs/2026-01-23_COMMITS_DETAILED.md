# Commits on 2026-01-23 — Detailed

This document lists every commit made on January 23, 2026, in chronological order, with full commit messages and file-change summaries. These commits continue Phase C (codegen fixes, LSP, runtime, refactors).

---

## Branch application status

**Applied to this branch:** **Commits #1–#12** have been applied. For each applied commit, **Brought** and **Not brought** are listed in that commit’s section below so partial applies are explicit.

**Not brought into this branch (commits 13–25):** The following Jan 23 commits have *not* been applied; their details remain below for reference or future cherry-pick:

| # | Hash     | Summary |
|---|----------|--------|
| 13| b8ecc12  | Lint complexity, formatter blank lines |
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

## 10. `faf81e5` — Docs: update bootstrap plan with today's critical fixes — **APPLIED**

**Subject:** docs: update bootstrap plan with today's critical fixes

**Stats:** 1 file changed, 32 insertions(+), 3 deletions(-)

**Brought:** docs/ThingsToDo/BOOTSTRAP_PLAN_REMAINING.md: added section "Command-Line & String Fixes (2026-01-23)" documenting the four fixes (qb_init_args, module-level scoping, SELECT CASE qb_string_compare, NULL string semantics) and where they were fixed in our tree.

**Not brought:** Nothing (commit only touched this doc).

---

## 11. `a8d6bb0` — Feat(lsp): add workspace symbol search — **APPLIED**

**Subject:** feat(lsp): add workspace symbol search

**What changed:** LSP workspace/symbol support for symbol search across the workspace.

**Stats:** 2 files changed, 46 insertions(+), 3 deletions(-)

**Brought:** src/lsp/mod.rs: workspace_symbol_provider capability; symbol() method that iterates open documents, calls get_document_symbols(), filters by query (case-insensitive), sets location.uri per document; module doc updated to mention workspace symbols (Ctrl+T).

**Not brought:** TODO.md checkbox updates from commit (we may not have matching items).

---

## 12. `18560d1` — Fix(codegen): SELECT CASE string comparison and docs update — **APPLIED (codegen/runtime already present)**

**Subject:** fix(codegen): SELECT CASE string comparison and docs update

**What changed:**

- SELECT CASE string comparison uses qb_string_compare
- Command-line args fixed (qb_init_args); module-level scoping (NoIDEMode, etc.)
- NULL string semantics fixed (NULL == "")

QB64pe now parses command-line arguments, recognizes -c/-x flags, enters compiler mode, attempts to compile BASIC source files.

**Stats:** 7 files changed, 512 insertions(+), 40 deletions(-)

**Brought:** SELECT CASE string comparison (qb_string_compare for Single, Range, Comparison) and NULL-as-empty in qb_string_compare were already present: stmt/control_flow.rs has emit_case_condition(..., test_type), emit_single_case_match(..., test_type) with is_string branches; runtime/strings.rs has a_data/b_data NULL handling. No code changes applied (verified present).

**Not brought:** CLAUDE.md debugger section; docs/DEBUGGING.md (354 lines); docs/ThingsToDo/FUTURE.md; commit’s implicit_vars.rs and mod.rs changes (module-level scoping / main context); commit’s monolithic runtime.rs delta (we use runtime/strings.rs).

---

## 13. `b8ecc12` — Feat(tools): add complexity lint rules and formatter blank line normalization — **APPLIED**

**Subject:** feat(tools): add complexity lint rules and formatter blank line normalization

**What changed:** tools/fmt formatter blank line normalization; tools/lint complexity rules (complexity.rs) and style rules (style.rs).

**Stats:** 5 files changed, 461 insertions(+), 1 deletion(-)

**Brought:** New `tools/lint/src/rules/complexity.rs` (LongProcedureRule, TooManyParametersRule); MagicNumberRule and tests in style.rs; mod.rs: complexity module, MagicNumberRule, DeepNestingRule, LongProcedureRule, TooManyParametersRule; formatter config `blank_lines_between_procedures` and formatter logic (just_ended_procedure, is_procedure_end, is_end_part, blank line normalization between SUB/FUNCTION); formatter tests test_blank_lines_between_procedures, test_blank_lines_preserves_existing. Fixed correctness.rs End/System pattern to use `{ .. }` (struct variants).

**Not brought:** Nothing.

---

## 14. `0e6f494` — Docs: update README and TODO with current status — **APPLIED**

**Subject:** docs: update README and TODO with current status

**Stats:** 2 files changed, 54 insertions(+), 25 deletions(-)

**Brought:** README: expanded VSCode Extension section with feature table and build command for tools. TODO: last updated 2026-01-23; Phase 8 = VSCode Extension Enhancements (formatter, linter, format on save, workspace symbol, rename, etc. with checkmarks); Phase 9 = Future Visual Designer; debugging note for tools/debug; removed long “It IS a genuine bug” debug note at end.

**Not brought:** Session number (044) kept; exact checkbox list aligned with current extension capabilities.

---

## 15. `d8c5f3e` — Fix(codegen): REDIM SHARED, STRING$ numeric form, fixed-length strings — **ALREADY PRESENT**

**Subject:** fix(codegen): REDIM SHARED, STRING$ numeric form, fixed-length strings

**What changed:** REDIM SHARED handling, STRING$(n) numeric form, fixed-length string codegen fixes (expr.rs, implicit_vars.rs, runtime.rs).

**Stats:** 3 files changed, 67 insertions(+), 16 deletions(-)

**Brought:** No code changes. Verified already in tree: expr.rs (FixedString wrap with qb_str_from_c, STRING$ two-arg numeric vs string, ByRef fixed-string not lvalue); implicit_vars.rs (REDIM uses existing global when existing_vars contains name, for both main and SUB); runtime/strings.rs (qb_string_fill_code).

**Not brought:** N/A.

---

## 16. `a438673` — Fix(codegen): array scoping — main uses globals, procedures use locals — **ALREADY PRESENT**

**Subject:** fix(codegen): array scoping - main uses globals, procedures use locals

**What changed:** implicit_vars.rs is_main_program; stmt emit_dim use_global; mod.rs pass true for main. Plus BOOTSTRAP_PLAN_REMAINING.md.

**Brought:** Doc only: added “Array scoping (2026-01-23)” paragraph to BOOTSTRAP_PLAN_REMAINING.md. Code already present: implicit_vars.rs collect_implicit_locals(..., is_main_program), collect_dims(..., existing_vars, is_main_program); stmt/definitions.rs emit_dim use_global = current_proc.is_none() && global_var_names.contains(&c_name); mod.rs collect_implicit_locals(..., true) for main.

**Not brought:** tools/debug additions from that commit (not applied).

---

## 17. `04cec5a` — Feat(tools): add debugger scaffold for parallel development — **NOT BROUGHT**

**Subject:** feat(tools): add debugger scaffold for parallel development

**What changed:** qb64fresh-debug as workspace member; AST-level debugger (breakpoints, CLI, TOML, stub execution).

**Stats:** 6 files changed, 1485 insertions(+), 1 deletion(-)

**Brought:** Nothing (tools/debug not present on this branch; can be added in a later pass if desired).

**Not brought:** Full debugger scaffold (tools/debug).

---

## 18. `d4e02ed` — Docs: update STUB_FUNCTIONS.md with accurate line counts — **NOT BROUGHT**

**Subject:** docs: update STUB_FUNCTIONS.md with accurate line counts

**What changed:** Sync docs with codebase line counts (runtime split into runtime/*.rs; STUB_FUNCTIONS.md may not exist here).

**Brought:** Nothing (file/structure differs).

**Not brought:** STUB_FUNCTIONS.md changes.

---

## 19. `bbfe484` — Fix(codegen): handle variable argument count for built-in functions — **ALREADY PRESENT**

**Subject:** fix(codegen): handle variable argument count for built-in functions

**What changed:** _CONSOLE, _MAPUNICODE, _ICON, _ACCEPTFILEDROP variants; Shell int32_t; qb_statuscode handle; logical_drives int32_t.

**Brought:** No code changes. Verified: expr.rs has _CONSOLE (qb_console_get/0, qb_console/1), _MAPUNICODE (1/2/3), _ICON (0/1/2), _ACCEPTFILEDROP (0/1); system.rs has int32_t for shell/statuscode/logical_drives.

**Not brought:** N/A.

---

## 20. `f1a259f` — Fix: string initialization and stub forward declarations — **ALREADY PRESENT**

**Subject:** fix: string initialization and stub forward declarations

**What changed:** Global string init to "" at program start; forward declarations for stub functions.

**Brought:** No code changes. Verified: analysis.rs has the loop that pushes string_const_inits for "qb_string* X = NULL;" globals; runtime/mod.rs calls system::emit_stub_declarations(output).

**Not brought:** N/A.

---

## 21. `9e0d624` — Fix: support runtime expressions in REDIM dimensions — **NOT BROUGHT**

**Subject:** fix: support runtime expressions in REDIM dimensions

**What changed:** TypedRedimDimension with TypedExpr lower/upper; codegen emits expressions for REDIM bounds.

**Brought:** Nothing (would require typed_ir, semantic checker, codegen, implicit_vars changes; deferred).

**Not brought:** TypedRedimDimension, REDIM arr(n) with variable n.

---

## 22. `370120c` — Chore: reorganize docs and fix lint pattern matching — **PARTIALLY APPLIED**

**Subject:** chore: reorganize docs and fix lint pattern matching

**What changed:** Move FUTURE.md/OPENGL_SUPPORT.md to docs/ThingsToDo/; QB64PE_LANGUAGE_SPECIFICATION.md; lint End/System { .. }.

**Brought:** Lint End/System fix was already applied in commit #13 (correctness.rs: StatementKind::End { .. } | StatementKind::System { .. }).

**Not brought:** Doc moves (FUTURE.md, OPENGL_SUPPORT.md); QB64PE_LANGUAGE_SPECIFICATION.md update.

---

## 23. `cc43b8f` — Fix(codegen): fix memory bugs in MK* functions and EXIT FUNCTION — **ALREADY PRESENT**

**Subject:** fix(codegen): fix memory bugs in MK* functions and EXIT FUNCTION

**What changed:**

**Runtime:** MKI$/MKL$/MKS$/MKD$ — data pointer set correctly (fixes segfaults); _TRIM$ same fix; qb_string_concat defensive checks for NULL/corrupt data.

**Codegen:** EXIT FUNCTION emits `return FuncName;` instead of bare `return;` (fixes undefined return when returning early).

Resolves segfaults when running QB64PE compiled by QB64Fresh.

**Stats:** 2 files changed, 45 insertions(+), 9 deletions(-)

**Brought:** No code changes. Verified: runtime/keyboard.rs MK* use separate malloc for result->data and result->capacity; runtime/keyboard.rs qb_trim same; runtime/strings.rs qb_string_concat has defensive a_valid/b_valid checks; stmt/control_flow.rs EXIT FUNCTION emits `return ret_var;`.

**Not brought:** N/A.

---

## 24. `ca65c3e` — Fix: resolve testing infrastructure failures — **APPLIED (partial)**

**Subject:** fix: resolve testing infrastructure failures

**What changed:** _EXIT combine; required_param_count() == 0; _MAPUNICODE accept TO or ,; test_invalid_binary_op; golden update.

**Brought:** expressions.rs: parameterless function call now uses `required_param_count() == 0` instead of `params.is_empty()` (so _STATUSCODE etc. can be called with no args). graphics.rs: parse_mapunicode accepts TO or Comma as separator for QB64 compatibility (_MAPUNICODE 8364, 128).

**Not brought:** _EXIT combined into single optional-param function (would need semantic + codegen; left as separate function + sub). test_invalid_binary_op (not found in tests). Golden files: run `UPDATE_GOLDEN=1 cargo test golden` to regenerate when desired.

---

## 25. `efe10f3` — Docs: add bootstrap achievement documentation and test suite — **NOT BROUGHT**

**Subject:** docs: add bootstrap achievement documentation and test suite

**What changed:** BOOTSTRAP_ACHIEVEMENT.md, BEHAVIORAL_DIFFERENCES.md, MIGRATION_GUIDE/README bootstrap section, BOOTSTRAP_PLAN simplifications, bootstrap_tests.rs, test-bootstrap.sh.

**Brought:** Nothing (docs and test script can be added in a later pass).

**Not brought:** New docs and bootstrap test script.

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
| 13| b8ecc12  | Lint complexity, formatter blank lines — APPLIED |
| 14| 0e6f494  | Docs: README and TODO status — APPLIED |
| 15| d8c5f3e  | REDIM SHARED, STRING$, fixed-length strings — ALREADY PRESENT |
| 16| a438673  | Array scoping (main=globals, procedures=locals) — ALREADY PRESENT + doc |
| 17| 04cec5a  | Debugger scaffold (tools/debug) — NOT BROUGHT |
| 18| d4e02ed  | STUB_FUNCTIONS.md line counts — NOT BROUGHT |
| 19| bbfe484  | Variable-arg built-ins — ALREADY PRESENT |
| 20| f1a259f  | String init, stub forwards — ALREADY PRESENT |
| 21| 9e0d624  | REDIM runtime expressions — NOT BROUGHT |
| 22| 370120c  | Docs reorganize, lint End/System — PARTIALLY (lint already fixed) |
| 23| cc43b8f  | MK* memory bugs, EXIT FUNCTION — ALREADY PRESENT |
| 24| ca65c3e  | Testing infra — APPLIED (required_param_count, _MAPUNICODE parser) |
| 25| efe10f3  | Bootstrap achievement docs — NOT BROUGHT |
