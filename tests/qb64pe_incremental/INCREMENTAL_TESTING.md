# QB64pe Incremental Testing Guide

One guide for strategy, workflow, extraction, full test plan, execution, results, and progress output.

---

## Summary

### Current Status

**Working (fast iteration):**
- **Phase 1: Core Infrastructure** — 0.15s ✅ (global includes, constants, settings)
- **Phase 2: Hash utility** — 0.7s ✅ (requires hash.bi before hash.bas)
- **Phase 2: Type utility** — 0.1s ✅ (self-contained)
- **Phase 5: Full compiler** — ~10s ✅ (24,757 lines → 113,820 lines C)

**Partial:**
- **Phase 2: Const eval** — 0.1s ⚠️ (needs elements.bas; some semantic errors)
- **Phase 3: Built-in functions** — ⚠️ (4 semantic errors; dependencies resolved)
- **Phase 4: Core compiler** — ⚠️ (3 semantic errors; dependencies resolved)

**Key learnings:** Isolated utilities (Phase 1–2) iterate in 0.1–0.7s. Full QB64pe compiles in ~10s. Use Phase 1–4 for iteration; use full compile for final validation. Include order: `.bi` before `.bas`.

**Recommendations:** For rapid iteration use Phase 1–2. For compiler development extract sections from qb64pe.bas when needed. For full validation run Phase 5 when ready.

---

## Strategy

### Problem

Compiling full QB64pe (24,757 lines) takes **5+ minutes**, making rapid iteration impossible.

### Solution: Incremental Testing

Test **portions** of QB64pe for:
- **Fast iteration:** 0.1–5s per test instead of 5+ minutes
- **Focused debugging:** Test specific failing sections
- **Progressive validation:** Build from working sections to full compiler

### Quick Start

**Option 1: Pre-built test files**
```bash
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c   # ~0.15s
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c       # ~0.7s
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_type.bas --emit-c      # ~0.1s
cargo run --bin qb64fresh -- tests/qb64pe_incremental/03_builtin_functions.bas --emit-c   # ~5s
```

**Option 2: Extract specific sections**
```bash
./scripts/extract-qb64pe-section.sh idstruct_type 596 642
cargo run --bin qb64fresh -- tests/qb64pe_incremental/sections/idstruct_type.bas --emit-c
```

**Speedup:** 300–3000× faster than full QB64pe.

### Workflow: Find → Extract → Fix → Test → Repeat

1. **Run full compile (background)** — `./scripts/test-full-qb64pe-unbuffered.sh > /tmp/qb64pe_full_test.log 2>&1 &`
2. **Identify errors** — `grep "error:" /tmp/qb64pe_full_test.log | head -5`
3. **Extract problematic section** — `grep -n "^TYPE idstruct\|^SUB clearid" ../QB64pe/source/qb64pe.bas` then `./scripts/extract-qb64pe-section.sh <name> <start> <end>`
4. **Test section in isolation** — `cargo run --bin qb64fresh -- tests/qb64pe_incremental/sections/<name>.bas --emit-c`
5. **Fix and iterate** — Fix in QB64Fresh, re-test section, then re-run full when ready.

### Pre-built test files

| Phase | File | Time | Purpose |
|-------|------|------|---------|
| 1 | `01_core_infrastructure.bas` | ~0.15s | Global includes |
| 2 | `02_utilities_hash.bas`, `02_utilities_type.bas`, `02_utilities_const_eval.bas` | 0.1–0.7s | Utilities |
| 3 | `03_builtin_functions.bas` | ~5s | Built-in SUB/FUNCTION |
| 4 | `04_core_compiler.bas` | ~10s | Compiler without IDE |
| 5 | Full qb64pe.bas | ~10s | Full validation |

### Speed comparison

| Test | Lines | Time | Speedup |
|------|-------|------|---------|
| Full QB64pe | 24,757 | ~10s | 1× |
| Phase 4 (Core) | 3,500 | ~10s | — |
| Phase 3 (Built-ins) | 4,342 | ~5s | 60× |
| Phase 2 (Utilities) | 500–1500 | 0.1–0.7s | 300–3000× |
| Phase 1 (Core) | ~100 | 0.15s | 2000× |
| Extracted section | 50–200 | 0.1–0.5s | 600–3000× |

### Tips

- Start with the smallest test that reproduces the error.
- Use `--emit-c` to skip C compilation during iteration.
- Fix one error at a time; test after each fix.
- Run full compile in background; work on sections meanwhile.
- Use `--verbose` and `stdbuf -oL -eL` for real-time progress (see **Progress output** below).

### When to test full QB64pe

**Do:** When all incremental tests pass, after multiple fixes, or for final validation.  
**Don’t:** For initial debugging, single-fix verification, or rapid iteration—use phases or extracted sections.

### Scripts

```bash
./scripts/extract-qb64pe-section.sh <name> <start_line> <end_line>
./scripts/test-full-qb64pe-unbuffered.sh
./scripts/test-qb64pe-incremental.sh all   # or 1, 2, 3, 4
```

---

## Workflow Example

### Scenario: Fix a bug in hash table code

1. **Test isolated component (0.7s):** `cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c`
2. **Fix error, test again** — same command.
3. **Test related:** `02_utilities_type.bas`, `01_core_infrastructure.bas`.
4. **Larger test:** `04_core_compiler_working.bas` (~2–5s).

**Old workflow:** 5 iterations × 5+ min = 25+ min. **New:** 5 × 0.7s ≈ 3.5s → **~428× faster.**

### Scenario: Test a specific section (e.g. `clearid` SUB)

1. **Extract:** `./scripts/extract-qb64pe-section.sh clearid_sub 14476 <end_line>`
2. **Create test file** that `$INCLUDE`s core includes, type system, `sections/idstruct_type.bas`, `sections/ids_init.bas`, `sections/clearid_sub.bas`, then minimal test code.
3. **Test:** `cargo run --bin qb64fresh -- tests/qb64pe_incremental/test_clearid.bas --emit-c` (~0.2s).

### Scenario: Adding a new feature

1. Start with Phase 1; add feature to a small test file.
2. Test in isolation; fix errors quickly.
3. Integrate with Phase 4; final validation with full QB64pe when ready.

---

## Extracting Sections

Use `extract-qb64pe-section.sh` to pull sections from `qb64pe.bas` into `tests/qb64pe_incremental/sections/`.

### Usage

```bash
./scripts/extract-qb64pe-section.sh <section_name> <start_line> <end_line>
# Example:
./scripts/extract-qb64pe-section.sh idstruct_type 596 642
```

### Key sections for built-in functions

| Section | Lines | Purpose |
|---------|-------|---------|
| idstruct TYPE | 596–642 | Identifier structure for clearid/regid |
| ids_init | 644–656 | Symbol table array init |
| clearid SUB | 14476+ | Reset id structure |
| regid SUB | 21849+ | Register function/sub in symbol table |

Find line numbers:
```bash
grep -n "^TYPE \|^SUB \|^FUNCTION " ../../../QB64pe/source/qb64pe.bas
```

Workflow: identify needed sections from errors → find line numbers → extract → add `$INCLUDE` in test file → test incrementally.

---

## Full Test Plan

**Goal:** Test entire QB64pe incrementally.

1. **Phase 1:** Test isolated components (core, hash, type, const_eval).
2. **Phase 2:** Extract and test core compiler sections (TYPEs, SUBs, init).
3. **Phase 3:** Test built-in functions with minimal compiler infra.
4. **Phase 4:** Test main compiler logic (parsing, codegen).
5. **Phase 5:** Full qb64pe.bas; document errors; prioritize fixes.

**Execution:** Identify major sections → extract and test each → build up incrementally → run full compiler and collect errors.

---

## Running Full Tests

### How to run

```bash
./scripts/test-full-qb64pe-unbuffered.sh
# Or manually:
stdbuf -oL -eL cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c -o /tmp/qb64pe_full_test.c --verbose 2>&1 | tee /tmp/qb64pe_full_test.log
```

### Monitor progress

```bash
ps aux | grep qb64fresh | grep qb64pe.bas
tail -f /tmp/qb64pe_full_test.log
ls -lh /tmp/qb64pe_full_test.c
```

### After completion

```bash
grep -ci "error" /tmp/qb64pe_full_test.log
grep "error:" /tmp/qb64pe_full_test.log | head -50
```

Use `extract-qb64pe-section.sh` to isolate failing sections and fix incrementally.

---

## Results

### Full compilation (QB64pe)

- **Status:** ✅ Complete (as of 2026-01-27).
- **Source:** QB64pe/source/qb64pe.bas — 24,757 lines, 1.1MB; preprocessed ~2.6MB.
- **Output:** /tmp/qb64pe_full_test.c — 113,820 lines, 6.7MB.
- **Phases:** Lexing (400,583 tokens), Parsing (2,172 statements), Semantic (2,172 typed), Codegen (113,820 lines C).
- **Time:** ~10 seconds (after lexer performance fix).

### Section tests

- idstruct_type, ids_init, clearid_sub, usedVarList_type — passing.
- Label_Type — parse errors in some runs.

### C compilation of generated code

- **Initial:** 310 errors, 50 warnings.
- **After fixes:** Different error set (e.g. 807 incompatible pointer type errors).
- **Fixed bugs:** (1) Field access name corruption (`id2.specialformat` → `d2.specialformat`) — fixed in `src/codegen/c_backend/expr.rs`. (2) Variable shadowing (`args` parameter vs local `DIM args(5)`) — fixed via parameter tracking and local renaming (e.g. `args_local`).
- **Remaining:** Incompatible pointer types (e.g. double-wrapping); needs separate investigation.

---

## Progress Output

For real-time progress during long runs:

```bash
./scripts/test-full-qb64pe-unbuffered.sh
# Or:
stdbuf -oL -eL cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --verbose
```

**Why:** Redirected output is fully buffered; `stdbuf -oL -eL` forces line buffering so progress appears immediately.

**Progress messages:** `[1/4] Lexing...`, `Lexing complete: N tokens`, same for Parsing, Semantic, Code generation.

---

## Testing Notes and Findings

### Phase results (concise)

- **Phase 1:** ✅ Passes; global includes work.
- **Phase 2 hash:** ✅ Passes with hash.bi before hash.bas.
- **Phase 2 const_eval:** ⚠️ Partial; needs elements.bas (pushelement, getelements$); some array indexing errors.
- **Phase 2 type:** ✅ Passes; self-contained.
- **Phase 3:** Blocked in isolation; needs clearid, regid, idstruct from main compiler; test as part of Phase 4.
- **Phase 4:** Sweet spot for iteration (~10s).
- **Phase 5:** Full validation; run when ready.

### Include order

`.bi` (header) files must come before `.bas` (implementation) files.

### Dependencies to check when tests fail

Missing `.bi`, TYPE definitions, global declarations, or helpers (e.g. hash1char, hash2char).

### Common issues

- **Type/undefined errors:** Add missing `.bi`, TYPEs, or globals.
- **Undefined function:** Add utility includes or extract from qb64pe.bas.

### Revised strategy

Phase 1–2: isolated utilities ✅. Phase 3: test with Phase 4. Phase 4: extract and test core compiler sections. Phase 5: full compiler when ready.

---

*Consolidated from INCREMENTAL_TESTING_STRATEGY, WORKFLOW_EXAMPLE, EXTRACTION_GUIDE, FULL_QB64PE_TEST_PLAN, FULL_TEST_EXECUTION, FULL_TEST_RESULTS, PROGRESS_OUTPUT_GUIDE, TESTING_NOTES, C_COMPILATION_RESULTS, and SUMMARY. See README.md for quick start and file overview.*
