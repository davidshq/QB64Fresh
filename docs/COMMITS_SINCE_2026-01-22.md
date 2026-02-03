# Commits Since 2026-01-22

This document lists what was added or changed in each commit starting January 22, 2026, in chronological order. Generated from `git log --since='2026-01-22'`.

---

## 2026-01-22

| Hash | What was added / changed |
|------|---------------------------|
| `e7bbd59` | **Phase C code generation validation** – 97.8% error reduction for QB64pe bootstrap |
| `d8f974b` | **Phase C Session 3** – fix duplicate labels and static strings in codegen |
| `cafedb7` | **Phase C Session 4** – BYREF function calls and keyboard constants in codegen |
| `ef4a7a9` | **Phase C Session 5** – ASCII/CHR constants and array parameters in codegen |
| `80f0a69` | **Phase C Session 6** – fix variable name suffix mismatch in semantic checker |
| `310c344` | **Phase C Session 7** – implicit variable collection for ByRef args in codegen |
| `060d82f` | **Phase C Session 7 continuation** – implicit locals for main() in codegen |
| `c5b3cd0` | Fix macro collisions and LEN() type sizing for QB64pe bootstrap |
| `bdeb0ba` | Add suffix fallback for constant lookups in symbol table |
| `c6cd917` | Fix variable scoping and declaration issues for QB64PE bootstrap |
| `7d35e62` | Refactor(codegen): extract RESERVED_IDENTIFIERS constant |
| `322d9dc` | Refactor(codegen): extract declare_scalar_var and declare_array_var helpers |
| `503155b` | Refactor(codegen): extract implicit_vars module and fix function call names |

---

## 2026-01-23

| Hash | What was added / changed |
|------|---------------------------|
| `efe10f3` | Docs: add bootstrap achievement documentation and test suite |
| `ca65c3e` | Fix: resolve testing infrastructure failures |
| `cc43b8f` | Fix(codegen): fix memory bugs in MK* functions and EXIT FUNCTION |
| `370120c` | Chore: reorganize docs and fix lint pattern matching |
| `9e0d624` | Fix: support runtime expressions in REDIM dimensions |
| `f1a259f` | Fix: string initialization and stub forward declarations |
| `bbfe484` | Fix(codegen): handle variable argument count for built-in functions |
| `d4e02ed` | Docs: update STUB_FUNCTIONS.md with accurate line counts |
| `04cec5a` | Feat(tools): add debugger scaffold for parallel development |
| `a438673` | Fix(codegen): array scoping – main uses globals, procedures use locals |
| `d8c5f3e` | Fix(codegen): REDIM SHARED, STRING$ numeric form, fixed-length strings |
| `0e6f494` | Docs: update README and TODO with current status |
| `b8ecc12` | Feat(tools): add complexity lint rules and formatter blank line normalization |
| `18560d1` | Fix(codegen): SELECT CASE string comparison and docs update |
| `a8d6bb0` | Feat(lsp): add workspace symbol search |
| `faf81e5` | Docs: update bootstrap plan with today's critical fixes |
| `e6374bf` | Feat: add LSP rename, UTF-8 support, fix file I/O declarations |
| `00ae288` | Docs: update TODO with rename symbol and path handling completion |
| `945c9a2` | Fix: update tests for QbString signatures, fix FOR loop variable scope |
| `ce2fc17` | Docs: add comprehensive language reference, fix codegen for function overloads |
| `7994f91` | Feat: add SessionStart hook to auto-load critical AST context |
| `d3a912f` | Feat: implement VGA palette port emulation for INP/OUT/WAIT |
| `e52b630` | Feat: enable graphics in external runtime mode + obsolete function errors |
| `23a5fa5` | Docs: update graphics documentation for external runtime mode |
| `45d875d` | Refactor: split large codegen files into directory modules |

---

## 2026-01-24

| Hash | What was added / changed |
|------|---------------------------|
| `ede90e0` | Refactor: split parser/statements.rs into submodules |
| `99b3613` | Refactor: split semantic/mod.rs into submodules |
| `728d543` | Refactor: split checker/statements.rs into submodules |
| `99d5bbe` | Refactor: split lsp/mod.rs into submodules |
| `655eff5` | Refactor: extract parser tests to parser/tests.rs |
| `5cd08b6` | Feat: implement window control and alpha blending graphics commands |
| `99188b4` | Docs: update FUTURE.md with completed graphics features |
| `3b636b9` | Docs: update graphics function status in stub documentation |
| `1fb6d42` | Feat: implement full audio system with all 12 previously-stubbed functions |
| `2989037` | Docs: update FUTURE.md and STUB_FUNCTIONS_REMAINING.md for completed audio |
| `c281507` | Feat: implement INT 0x33 mouse emulation for INTERRUPT/INTERRUPTX |
| `253ffb7` | Feat: implement Windows-only desktop functions |
| `3d2fbf6` | Feat: implement _COPYPALETTE and _DISPLAYORDER commands |
| `507605f` | Feat: implement _MAPTRIANGLE with software texture mapping rasterizer |
| `3bf9956` | Docs: reorganize documentation and add Rust file size recommendations |
| `5202dad` | Docs: expand DECLARE LIBRARY documentation with limitations and workarounds |
| `ce5d388` | Feat: complete _MEM type support across all compiler phases |
| `67087f9` | Feat(header-parser): add #define constants, #ifdef conditionals, and struct parsing |
| `68b9aab` | Feat(runtime): add runtime warnings for unsupported legacy DOS functions |
| `1ea41b8` | Docs(runtime): mark legacy stubs as FINAL IMPLEMENTATION |
| `9c54b58` | Feat(runtime): implement multiple screen pages for double buffering |
| `0d01d92` | Docs: update and reorganize documentation |
| `0e5802b` | Feat(runtime): add _MAPUNICODE support and cross-platform path normalization |
| `051e9e6` | Feat(runtime): implement network PUT/GET and STRIG event handlers |
| `0c01b55` | Feat(runtime): implement FreeType font rendering integration |
| `c2cf2ee` | Feat(compiler): add DECLARE LIBRARY enhancements and platform builtins |
| `e5b5d11` | Docs: reorganize FUTURE.md DECLARE LIBRARY section |
| `fc6eb46` | Feat(parser): implement _MEMGET/_MEMPUT AS type syntax |
| `6b6dc97` | Docs: update FUTURE.md with all implemented features |
| `66eb130` | Feat(codegen): implement VARPTR/VARSEG/SADD and full callback signatures |
| `b5c5d37` | Feat(debug): implement debugger runtime integration |
| `c7597f8` | Docs: update FUTURE.md with completed debugger integration |
| `8307b7d` | Docs: reorganize feature documentation |
| `36a0fe7` | Docs: update STUB_FUNCTIONS_REMAINING.md |
| `4a854df` | Docs: create STUB_FUNCTIONS_FULL.md for implemented functions |
| `f065c32` | Docs: move compile-error functions to STUB_FUNCTIONS_FULL |
| `4222b50` | Fix: parser bugs blocking QB64pe bootstrap, add STRIG 2-arg extension |
| `c7ec6ba` | Feat: implement _PALETTECOLOR function and statement |
| `cc04e00` | Fix: resolve all 24 semantic errors blocking QB64pe bootstrap |
| `f6f601a` | Docs: clean up bootstrap plan and remove redundant FUTURE.md |
| `2cff924` | Fix: resolve local variable scoping and keyboard/graphics stubs for bootstrap |

---

## 2026-01-25

| Hash | What was added / changed |
|------|---------------------------|
| `4dd4e77` | Fix: implement reference-counted string memory management |
| `f87305c` | Fix: resolve external function call and debug output bugs for QB64pe bootstrap |
| `e191f71` | Adding cursor knowledge |
| `5c4d469` | Fix: resolve memory exhaustion in string temp pool |
| `4731873` | Feat: register 65+ missing QB64 extension functions and statements |
| `a00399f` | Feat: implement missing QB64 language features and --no-shell flag |
| `f1a9435` | Docs: condense runtime architecture and update implementation status |
| `f57fc1d` | Feat(runtime): add alpha blending and mock page support |
| `c4913cf` | Feat(runtime): add hardware-accelerated SDL2 texture rendering |
| `16142b6` | Fix(semantic): correct _OPENHOST signature and add network PUT/GET tests |

---

## 2026-01-26

| Hash | What was added / changed |
|------|---------------------------|
| `8ba10b3` | Feat(runtime): add string, memory, and networking FFI functions |
| `c5fb2b4` | Feat(runtime): add _GLRENDER and _GLCOMPAT stubs |
| `1fca4a2` | Docs: reorganize docs and update implementation plans |
| `68ceb21` | Docs(logs): add session 061 agentic log |
| `53c3929` | Docs: add QuickBASIC 4.5 language specification reference |
| `a876923` | Docs: mark hardware acceleration and network I/O as complete |
| `d4044ec` | Feat(debug): implement runtime integration for debugger |
| `219a5ae` | Add runtime initialization and fix MID$ assignment for fixed-length strings |
| `988e3e5` | Fix missing qb_dir() declaration causing pointer truncation crash |
| `849dac2` | Refactor runtime codegen: replace unwrap() with proper error handling |
| `5f60d18` | Code review findings and codebase improvements |
| `b4469bf` | Refactor semantic checker and improve error handling |
| `95af2a0` | Update graphics runtime, debug/lint tools, and documentation |

---

## 2026-01-27

| Hash | What was added / changed |
|------|---------------------------|
| `ea7080c` | Session 067: Bug review, FFI error reporting, and documentation reorganization |
| `115ae41` | Fix double-wrapping qb_str_from_c() and string conversion issues |
| `269ec2a` | Refactor codegen and add progress reporting improvements |
| `1183a3a` | Fix qbt_ParseNum* to qb_string* type compatibility issue |
| `11389a2` | Update runtime API usage and fix SELECT CASE string comparisons |
| `3fe464b` | Fix QbString type name and array variable rename bugs |
| `a436271` | Fix function signature mismatches and update architectural review |

---

## 2026-01-28

| Hash | What was added / changed |
|------|---------------------------|
| `e08be68` | Implement TypeRegistry and standardize error handling |
| `06874bb` | Replace unwrap() with expect() in test code |
| `b100e76` | Docs: add comprehensive regression test coverage analysis and tests |
| `f43535f` | Test: add string temp pool overflow regression test |
| `11d37de` | Test: add FFI declaration completeness regression test |
| `c2a7fbd` | Test: add runtime initialization order regression test |
| `6a49757` | Docs: update regression test coverage with completed tests |
| `06d25dc` | Replace expect() with advance_start() in parser/system.rs |
| `7b56549` | Fix semantic errors in QB64pe bootstrap tests |
| `5528a5e` | Add error recovery tests and update architectural review |
| `48dfa42` | LSP performance improvements and QB64pe compilation analysis |
| `fbc4d67` | Implement LSP incremental parsing and consolidate TODO files |
| `279d79f` | Rich diagnostics, BYREF/OPEN/DECLARE DYNAMIC LIBRARY, runtime LBOUND/UBOUND, LSP incremental parse |
| `727fcba` | Refactor: split large statement modules into focused submodules |

---

## 2026-01-29

| Hash | What was added / changed |
|------|---------------------------|
| `deb6ce9` | Implement Version$ and $INCLUDEONCE, update missing features docs |
| `f3baf1c` | Implement $USELIBRARY directive with library management system |
| `2a3d9c5` | Implement $EMBED directive for embedding binary files |
| `b5d9f73` | Implement $VERSIONINFO and $EXEICON resource file generation |
| `6ebf07f` | Implement $COLOR:0 and $COLOR:32 directive with LSP metadata support |
| `7af611f` | Implement $ASSERTS and $ASSERTS:CONSOLE directives |
| `574f066` | Implement $STATIC and $DYNAMIC directives for array allocation |
| `0930dfd` | Fix graphics initialization error handling to use exit() instead of return |
| `71012fd` | Implement file I/O path resolution for internal/ directory |
| `874a2da` | Fix runtime compilation errors and update documentation |
| `6bf251e` | Fix BYREF string codegen, IDE window visibility, add source size limits |

---

## 2026-01-30

| Hash | What was added / changed |
|------|---------------------------|
| `5066df4` | Add runtime comparison tests and improve file I/O, error handling, and documentation |

---

## 2026-01-31

| Hash | What was added / changed |
|------|---------------------------|
| `88fc06a` | Docs: add AgenticLogs sessions 102-147 (libqb, OpenGL, code review, parity) |
| `4d51baa` | Docs: update handbook, language reference, ADRs, archive; add OPENGL, COMPRESSION, QB64pe diffs |
| `82b92af` | Runtime: add libqb-style modules (bitops, buffer, cmem, condvar, http, logging, list, thread, gl_ffi); compression C; OpenGL build |
| `70fb0cb` | Compiler: OpenGL builtins, codegen runtime (bitops, logging), semantic/symbols/parser updates |
| `ab2883a` | Tests: qb64pe incremental (elements, phase3/4 stubs), golden/fixture updates, opengl_minimal.bas |
| `190dc67` | Chore: update .cursor rules, CI workflow, .gitignore |

---

## 2026-02-02

| Hash | What was added / changed |
|------|---------------------------|
| `eed3d21` | IDE disclaimer dismiss: push each key twice for getinput inkey+keyhit |

---

*Document generated from git history. To refresh: `git log --since='2026-01-22' --format='%h|%ad|%s' --date=short --reverse`.*

*This file is preserved after resetting the repo to the last commit before the churn (d7e5bdc, 2026-01-21: "feat(semantic): complete Phase B bootstrap - QB64pe semantic analysis passes").*
