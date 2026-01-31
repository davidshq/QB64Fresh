# QB64Fresh: Full Codebase Review Plan

**Purpose:** Systematically iterate through every code file in QB64Fresh and evaluate it for bugs, errors, bad practices, lost functionality, documentation gaps, and safety issues.

**Scope:** All Rust (`.rs`) code in QB64Fresh: compiler (`src/`), runtime (`runtime/src/`), tools (`tools/`), and tests/benches/fuzz. Excludes generated C, BASIC fixtures, and third-party code.

**Related docs:**
- [ARCHITECTURAL_REVIEW_COMPLETED.md](../archive/ARCHITECTURAL_REVIEW_COMPLETED.md) – completed items
- [PARTIAL_IMPLEMENTATIONS.md](PARTIAL_IMPLEMENTATIONS.md) – stub/partial functionality
- [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md) – known language-edge cases
- [CLAUDE.md](../../CLAUDE.md) – documentation standards, error handling, logging

---

## 1. Evaluation Dimensions

For each file (or coherent module), assess:

| Dimension | What to check | Tools / References |
|-----------|----------------|--------------------|
| **Bugs & correctness** | Logic errors, wrong types, off-by-one, misuse of APIs, panic vs `Result` | Tests, clippy, manual trace |
| **Errors & robustness** | Ignored `Result`s, missing `#[must_use]`, panics in library code, error message quality | clippy, grep for `unwrap()`/`expect()` |
| **Bad practices** | Excessive `.clone()`, `String` vs `&str`, missing docs, non-idiomatic Rust | |
| **Lost functionality** | Stubs that should be real, missing QB64pe parity, incomplete features | PARTIAL_IMPLEMENTATIONS, LIBQB_FUNCTIONALITY |
| **Documentation** | Missing `///`/`//!`, wrong sections, no examples where needed | `cargo doc`, CLAUDE.md § Documentation |
| **Safety** | `unsafe` usage, FFI contracts, resource leaks, thread safety | grep `unsafe`, runtime/include |
| **Tests** | Missing unit/integration coverage for critical paths, flaky tests | `cargo test`, coverage (optional) |

---

## 2. Phased Approach

### Phase 0: Automated baseline (before file-by-file)

Run once and fix or document findings before starting manual review:

1. **Build & test**
   - `cargo build --all-targets` (workspace)
   - `cargo test --all-targets`
   - Fix any failing tests or build errors.

2. **Clippy**
   - `cargo clippy --all-targets -- -W clippy::all -W clippy::pedantic` (or project’s existing clippy config)
   - Address or explicitly allow with justification; log in review notes.

3. **Format**
   - `cargo fmt --check`; fix or document exceptions.

4. **Documentation**
   - `cargo doc --no-deps`; fix broken links or missing docs that block doc build.

5. **Optional**
   - `cargo audit` for dependencies.
   - Count `unwrap`/`expect`/`clone` per crate to prioritize.

Output: Short “Phase 0 report” (pass/fail + list of issues or file references).

---

### Phase 1: Compiler crate (`src/`)

Order by pipeline dependency so upstream fixes don’t invalidate downstream review:

| Order | Area | Files (representative) | Focus |
|-------|------|------------------------|--------|
| 1 | Lexer | `lexer/mod.rs`, `lexer/token.rs` | Token correctness, spans, edge cases |
| 2 | AST | `ast/mod.rs`, `ast/expr.rs`, `ast/stmt.rs` | Invariants, exhaustiveness, docs |
| 3 | Parser | `parser/*.rs`, `parser/statements/*.rs` | Parse errors, recovery, consistency with AST |
| 4 | Preprocessor | `preprocessor.rs` | $INCLUDE, limits, error messages |
| 5 | Header parser | `header_parser/*.rs` | DECLARE LIBRARY parsing, C signature handling |
| 6 | Semantic | `semantic/*.rs`, `semantic/checker/*.rs`, `semantic/checker/statements/*.rs` | Types, symbols, builtins, error messages |
| 7 | Codegen | `codegen/*.rs`, `codegen/c_backend/*.rs`, `codegen/c_backend/stmt/*.rs`, `codegen/c_backend/runtime/*.rs` | C output correctness, runtime mode, stubs vs real |
| 8 | LSP | `lsp/*.rs`, `lsp/analysis/*.rs` | Incremental, diagnostics, no panics in request handling |
| 9 | Top-level | `lib.rs`, `main.rs`, `error_formatting.rs`, `library.rs` | Public API, CLI, error display |

For each **file** (or small group of related files):

- Run through the **per-file checklist** (§4).
- Note bugs, bad practices, lost functionality, doc gaps.
- Log findings in the **review log** (§5); link to IndividualProblems for complex issues.

---

### Phase 2: Runtime crate (`runtime/src/`)

Order by dependency and risk:

| Order | Area | Files (representative) | Focus |
|-------|------|------------------------|--------|
| 1 | Core / strings / types | `lib.rs`, `string.rs`, `memory.rs`, `array_registry.rs`, `qbs_compat.rs` | FFI contract, safety, docs |
| 2 | IO / console | `io/*.rs` | File handles, encoding, PRINT/INPUT behavior |
| 3 | Math / utilities | `math.rs`, `cp437.rs`, `filepath.rs`, `bitops.rs`, `buffer.rs`, `cmem.rs` | Correctness vs QB64pe, edge cases |
| 4 | System / process | `events.rs`, `thread.rs`, `mutex.rs`, `condvar.rs`, `mem_lock.rs` | Thread safety, no deadlocks |
| 5 | Graphics | `graphics/*.rs`, `graphics_ffi.rs`, `font_ffi.rs`, `font_manager.rs` | Backend trait, SDL2/mock, errors |
| 6 | Audio | `audio/*.rs`, `audio_ffi.rs` | Backend trait, rodio/mock, MIDI if present |
| 7 | Other FFI / compat | `dialogs.rs`, `logging.rs`, `logging_ffi.rs`, `http.rs`, `http_ffi.rs`, `list.rs`, `completion.rs`, `console_display_ffi.rs`, `game_controller_ffi.rs`, `condvar_ffi.rs`, `mutex_ffi.rs` | FFI safety, parity with libqb/docs |

Apply the same per-file checklist and review log.

---

### Phase 3: Tools (`tools/`)

| Crate | Files | Focus |
|-------|--------|--------|
| fmt | `fmt/src/*.rs` | Rules, config, no panics on malformed input |
| lint | `lint/src/*.rs`, `lint/src/rules/*.rs` | Rule correctness, integration with parser/semantic |
| debug | `debug/src/*.rs` | DAP protocol, symbols, no panics in server |

Same checklist and log.

---

### Phase 4: Tests, benches, fuzz

| Area | Location | Focus |
|------|----------|--------|
| Integration/other tests | `tests/*.rs` | Coverage, stability, clarity of failure messages |
| Runtime tests | `runtime/tests/*.rs` | Graphics/audio integration, mocks |
| Benches | `benches/*.rs` | Correctness of benchmarks, no accidental side effects |
| Fuzz | `fuzz/*.rs` | Harness correctness, no undefined behavior |

Ensure tests document intent and that flaky or known-failing tests are documented.

---

## 3. File Inventory (Rust only)

Approximate counts (as of plan creation):

- **Compiler `src/`:** ~105 `.rs` files (lexer, ast, parser, preprocessor, header_parser, semantic, codegen, lsp, top-level).
- **Runtime `runtime/src/`:** ~48 `.rs` files (lib, string, io, math, graphics, audio, FFI, etc.).
- **Tools:** ~28 `.rs` (fmt, lint, debug, fix_encoding).
- **Tests/benches/fuzz:** ~15+ `.rs` (tests/, runtime/tests/, benches/, fuzz/).

**Total:** ~194 `.rs` files. Review can be done in batches (e.g., one phase at a time or N files per session).

---

## 4. Per-file checklist (template)

Use this (or a shortened version) for each file or small related group:

```
File: path/from/repo/root.rs
Phase: 1 | 2 | 3 | 4
Reviewer: (optional)
Date: YYYY-MM-DD

[ ] Bugs & correctness: no logic/type errors found; OR list issues.
[ ] Errors & robustness: Results checked, panics justified or fixed; OR list.
[ ] Bad practices: clone/String/docs/idiom reviewed; OR list.
[ ] Lost functionality: stubs/parity checked against PARTIAL_IMPLEMENTATIONS; OR list.
[ ] Documentation: module + public items documented per CLAUDE.md; OR list.
[ ] Safety: unsafe/FFI reviewed; OR list.
[ ] Tests: existing tests sufficient or gaps noted.

Notes:
- (any open questions, deferred items, or links to IndividualProblems)
```

---

## 5. Review log and output

- **Review log:** Single document or spreadsheet (e.g. `docs/ThingsToDo/CODE_REVIEW_LOG.md` or `AgenticLogs/CODE_REVIEW_LOG.md`) with one row/section per file (or module), columns: file, phase, date, status (done/deferred), critical issues (Y/N), link to details.
- **Findings:** For each finding, record: file, line/area, dimension (bug/practice/doc/safety/…), severity (critical/major/minor), one-line description, optional link to IndividualProblem or ADR.
- **Session discipline:** Per CLAUDE.md, update AgenticLogs with session number and brief summary; for complex issues (3+ attempts or non-obvious root cause), add `AgenticLogs/IndividualProblems/YYYY-MM-DD_problem-name.md` and link from the review log.

---

## 6. Success criteria

- Phase 0: All automated checks pass (or documented exceptions).
- Phases 1–4: Every `.rs` file has been through the checklist and logged.
- All **critical** findings fixed or explicitly deferred with rationale.
- Review log and any new IndividualProblems committed; AgenticLogs updated.

---

## 7. Iteration and maintenance

- **Re-run Phase 0** after major refactors or before releases.
- **Re-check changed files** in Phases 1–4 when touching them (e.g., “last reviewed” date in log).
- **New code:** Apply the same checklist in PRs for new modules/files so the full sweep doesn’t regress.

---

*Last updated: 2026-01-31. Adjust file counts and paths if the tree changes.*
