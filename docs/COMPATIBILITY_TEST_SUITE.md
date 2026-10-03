# Compatibility Test Suite as Behavioral Contract

**Status:** Ongoing  
**Last updated:** 2026-02-02

This document defines the **compatibility test suite** as the **behavioral contract** for QB64Fresh: what “QB64/QB4.5 compatible” means in practice, what is covered, and what is not.

---

## 1. Behavioral contract

### 1.1 What the suite defines

The compatibility test suite is the **authoritative definition** of expected behavior for:

- **Compilation:** Programs that are in scope must compile (lex → parse → semantic → codegen) without errors, or fail with documented, acceptable errors.
- **Runtime (where applicable):** Programs that compile and are run in fixtures or execution tests must produce the expected output (or match QB64pe behavior in comparison tests).

“Compatible” therefore means: **the implementation satisfies the contract expressed by these tests.** Changes that break existing compatibility tests are regressions unless the contract is explicitly updated (e.g. retiring a test or changing expected output).

### 1.2 Out of scope / explicit exclusions

The following are **not** part of the compatibility contract:

- **open_gl:** Programs under `QB64pe/tests/qbasic_testcases/open_gl/` use `_GL*` and OpenGL-specific features. QB64Fresh uses SDL2/winit; OpenGL is optional and not required for “compatibility” as defined by this suite. These tests may be skipped or run only when OpenGL support is enabled.
- **Known bugs in original code:** If a test case contains a bug (e.g. undefined behavior or logic error in the original BASIC), the contract may be “compile and run” without requiring identical wrong output; any such exceptions should be documented (e.g. in TESTING.md or this file).
- **Platform-specific behavior:** File paths, line endings, and system-dependent output may be normalized (e.g. in runtime comparison) rather than required to be byte-identical.

### 1.3 Contract levels

| Level | Meaning |
|-------|--------|
| **Must compile** | Source compiles through all stages (lex, parse, semantic, codegen). No assertion on runtime output. |
| **Must compile and run** | Compiles and, when executed, produces expected output (fixture `.output` or runtime comparison). |
| **Must fail compile** | Compilation is required to fail with an error that contains the expected substring (fixture `.err`). |

Compatibility suites can mix these levels; the important point is that each test is classified so that “pass” is well-defined.

---

## 2. Test suite inventory and coverage

### 2.1 QB64Fresh-local tests

| Location | Role | Contract level | Count (approx.) |
|----------|------|-----------------|------------------|
| `tests/fixtures/success/` | Local success cases | Compile; optional run vs `.output` | 3+ |
| `tests/fixtures/error/` | Local error cases | Must fail with `.err` substring | 4 |
| `tests/compatibility.rs` | Runs fixtures | As above | Discovered from fixtures |
| `tests/execution_tests.rs` | Compile + run | Must compile and run, output checked | 27 (4 ignored) |
| `tests/golden/` | Codegen shape | Must compile; generated C matches `.golden` | 10 |

These form the **local behavioral contract**: small, fast, and fully under QB64Fresh control.

### 2.2 QB64pe-derived compatibility suite

| Source | QB64Fresh harness | What is tested | Coverage notes |
|--------|-------------------|----------------|----------------|
| **QB64pe** `tests/qbasic_testcases/` | `tests/qb45_compat.rs` | Compile-only (all stages) | No runtime or output comparison in qb45_compat |
| **qb45com/** | `qb45com_compatibility` test | Must compile | QB4.5-oriented programs; high priority for “QB4.5 compatible” |
| **misc/** | `misc_compatibility` test | Must compile | Mixed; some use QB64 extensions |
| **n54/, pete/, thebob/** | `all_testcases_summary` test | Must compile | Contributor collections; may use extensions |

- **Total .bas in qbasic_testcases:** ~143 (across qb45com, misc, n54, pete, thebob; open_gl excluded). Exact count depends on repo snapshot; run `cargo test --test qb45_compat` for current file and pass counts.
- **Current pass rate (qb45com):** See TESTING.md and test output (e.g. 99.1% when 114/115 pass).
- **Excluded from contract:** `open_gl/` (see §1.2).

So the **external behavioral contract** is: “QB64Fresh must compile the non–open_gl qbasic_testcases programs (or fail in an acceptable way where we have agreed to exclude or relax).”

### 2.3 Runtime comparison (output contract)

| Location | Role | Contract level |
|----------|------|----------------|
| `tests/runtime_comparison/` | Compare QB64Fresh vs QB64pe runtime output | Same input → same (or normalized) output |

- **Content:** 260+ `.bas` programs and scripts (`run_comparison.sh`, `diff_results.sh`) to run under both QB64Fresh and QB64pe and compare results.
- **Contract:** For programs that both compilers accept, runtime output should match (or be normalized for known platform differences). This extends the contract from “compiles” to “compiles and behaves the same at runtime.”

### 2.4 Bootstrap and incremental tests

| Location | Role | Contract level |
|----------|------|----------------|
| `tests/bootstrap_tests.rs` | QB64pe bootstrap | QB64Fresh compiles QB64pe source and produces working build |
| `tests/qb64pe_incremental/` | Incremental QB64pe compile | Specific slices of QB64pe compile successfully |

These assert that **large, real-world QB64pe codebases** remain within the behavioral contract (compile and, where applicable, run).

---

## 3. Coverage summary

### 3.1 What is covered

- **Lexer:** All compatibility programs are lexed; failures show up as lex or parse errors.
- **Parser:** Full parse; compatibility suite exercises a wide range of QB4.5/QB64 syntax.
- **Semantic analysis:** Type checking, symbols, and control flow are exercised by every compiling test.
- **Code generation:** Every “must compile” test that passes exercises the C backend.
- **Runtime:** Execution tests and runtime_comparison cover I/O, math, strings, control flow, and other runtime behavior.
- **Regression:** Golden tests lock codegen shape; compatibility and runtime comparison lock behavior.

### 3.2 What is not covered (gaps)

- **OpenGL / _GL*:** Explicitly out of scope for the default contract (see §1.2).
- **Rare or edge syntax:** Some QB64 extensions may not appear in the current suite; adding them is ongoing.
- **Platform-specific APIs:** File paths, `SHELL`, and system-dependent behavior may be only partially or nominally tested.
- **Performance and resource use:** The contract is correctness (compile + output), not performance or memory bounds.
- **Full QB64pe parity:** The suite is a subset of QB64pe behavior; 100% parity would require broader coverage and explicit documentation of any remaining differences (see docs such as QB64Fresh_VS_QB64pe_DIFFERENCES).

### 3.3 Keeping coverage documented

- **TESTING.md:** Lists current test counts, pass rates, and how to run each suite.
- **This document:** Defines the contract and coverage at a high level; update when adding or retiring suites or when changing what “compatible” means.
- **CI:** `.github/workflows/ci.yml` runs compatibility tests (e.g. QB45 compatibility); any change to the contract (e.g. ignoring a directory or test) should be reflected in CI and in this doc.

---

## 4. Running the compatibility tests

```bash
# Local fixture compatibility
cargo test --test compatibility

# QB64pe qbasic_testcases (requires QB64pe repo at ../QB64pe or path in qb45_compat.rs)
RUST_MIN_STACK=8388608 cargo test --test qb45_compat -- --nocapture

# Runtime comparison (manual: run scripts in tests/runtime_comparison/)
# See tests/runtime_comparison/run_comparison.sh

# Bootstrap (QB64pe compile)
cargo test --test bootstrap_tests
```

Verbose failure info for qb45_compat: `VERBOSE=1 cargo test --test qb45_compat -- --nocapture`.

---

## 5. References

- [TESTING.md](TESTING.md) – Test types, counts, how to run, coverage metrics.
- [ARCHITECTURE.md](ARCHITECTURE.md) – Testing strategy overview.
- [docs/MULTI_PERSPECTIVE_CODEBASE_REVIEW.md](MULTI_PERSPECTIVE_CODEBASE_REVIEW.md) – Compatibility test suite as standard for “does this match expected behavior?”
- QB64pe: `QB64pe/tests/qbasic_testcases/README.md` – Note that test cases may not include all files needed to run (e.g. data files).
