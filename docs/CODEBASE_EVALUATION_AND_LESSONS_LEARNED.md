# QB64Fresh: Codebase Evaluation and Lessons Learned

**Purpose:** Honest evaluation of project status, concrete “where we went wrong” items drawn from AgenticLogs and the codebase, and actionable “how to do better next time” so a second pass or new contributors can avoid the same pitfalls.

---

## 1. Current Codebase and Project Status

### Pipeline and architecture

The compiler pipeline is **Lexer → Parser → AST → Semantic (two-pass) → Typed IR → CodeGen → C**, with dual runtime (inline emitted C / external `libqb64fresh_rt`). The architecture is sound. See [ARCHITECTURE.md](ARCHITECTURE.md) and [STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md](STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md) for full description.

### Refactoring state

- **Expr codegen:** Split into [src/codegen/c_backend/expr/](../src/codegen/c_backend/expr/) (no longer a single `expr.rs`).
- **Builtins:** Split into [src/semantic/builtins/](../src/semantic/builtins/) (category modules; `builtins.rs` removed).
- **Stmt codegen:** Dispatcher thinned to ~381 lines in [src/codegen/c_backend/stmt/mod.rs](../src/codegen/c_backend/stmt/mod.rs); context and helpers in [src/codegen/c_backend/stmt/state.rs](../src/codegen/c_backend/stmt/state.rs).
- **Semantic checker:** [src/semantic/checker/statements.rs](../src/semantic/checker/statements.rs) is still ~1,339 lines; thinning in progress (delegation to `statements/*.rs` submodules).
- **Compiler API:** Thin facade at [src/compiler_api.rs](../src/compiler_api.rs) exists for “parse only,” “parse + analyze,” “generate from TypedProgram.”

### IDE integration

- [ide_layers/](../ide_layers/) provides **layer0**, **layer1**, **layer2** as **separate versions** (do not patch a previous layer when adding the next).
- Layer2 builds and runs the QB64pe IDE (qb64pe.bas → C, link runtime).
- Layer 2 “Run/Make” and full IDE equivalence testing remain manual. The path is defined in [QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md](ThingsToDo/QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md) and [QB64PE_IDE_GROUND_UP_BUILD.md](ThingsToDo/QB64PE_IDE_GROUND_UP_BUILD.md).

### Tests and quality

- **1,500+ tests;** QB45 compatibility **99.1%** (114/115).
- [CODE_REVIEW_FULL.md](CODE_REVIEW_FULL.md) and [TODO_CONSOLIDATED.md](ThingsToDo/TODO_CONSOLIDATED.md) capture remaining review items and priorities (e.g. stream codegen, unwrap/expect in non-test code).

---

## 2. Where We Went Wrong

| Area | What went wrong | Source |
|------|-----------------|--------|
| **API assumptions** | Parser was written assuming `Token<'a>` and borrowed data; the actual [lexer token](../src/lexer/token.rs) owns its data. Led to 34+ compile errors (lifetimes, borrow checker, `Range` not `Copy`). | [IndividualProblems/2026-01-16_problem-parser-token-api-mismatch.md](../AgenticLogs/IndividualProblems/2026-01-16_problem-parser-token-api-mismatch.md) |
| **Refactoring scope** | writeln → writeln_code migration: changed many files at once, introduced duplicate emission, broken string literals, signature mismatches, duplicate functions. Became whack-a-mole; full revert. | [2026-01-26_refactoring-failure-analysis.md](../AgenticLogs/2026-01-26_refactoring-failure-analysis.md) |
| **IDE/runtime coupling** | IDE-compat logic was added inside the runtime (e.g. qb_screenhide, qb_gfx_screen). Had to revert and adopt “separate versions” (ide_layers) so each layer is isolated and previous layers are not patched. | [2026-01-31_session-153_ide-ground-up-build.md](../AgenticLogs/2026-01-31_session-153_ide-ground-up-build.md) |
| **Monolithic files** | Allowed stmt codegen, expr codegen, and semantic checker statements to grow very large before splitting. Reactive splitting is harder than keeping dispatchers thin from the start. | [FILE_SPLITTING_ANALYSIS.md](ThingsToDo/FILE_SPLITTING_ANALYSIS.md), [2026-01-31_session-151_stmt-codegen-file-splitting.md](../AgenticLogs/2026-01-31_session-151_stmt-codegen-file-splitting.md) |
| **No single compiler API** | Tools and LSP reached into internal modules. No thin facade for “parse only,” “parse + analyze,” “analyze from AST” until recently (compiler_api.rs). | [STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md](STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md) |
| **Big-bang and integration testing** | IDE equivalence plan explicitly warns against big-bang testing and recommends minimal repros per portion. We sometimes tested too much at once instead of one portion at a time. | [QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md](ThingsToDo/QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md) |
| **Verification gap** | Refactoring: no systematic “compile after each file” or “run tests after each step.” Parser: wrote parser without reading the actual Token definition first. | Same IndividualProblems and refactoring doc above. |

---

## 3. How to Do Better Next Time

- **Verify APIs before coding:** Read actual type definitions (e.g. Token, AST nodes) and call sites before writing dependent code; avoid assuming lifetimes or ownership.
- **Refactor in small steps:** One file (or one logical change) at a time; run `cargo build` and relevant tests after each step; avoid mass find-and-replace without verification.
- **Define a thin compiler API early:** Single entry points for “parse only,” “parse + analyze,” “generate from TypedProgram” so tools and LSP do not depend on internals; extend [src/compiler_api.rs](../src/compiler_api.rs) as the contract.
- **Layer integration with separate versions:** For IDE or any multi-component integration, build layer-by-layer with separate versions (like ide_layers); do not patch previous layers when adding the next.
- **Keep dispatchers thin from the start:** When adding new statement/expression categories, add a new submodule and one arm in the dispatcher; avoid letting a single file grow beyond ~500 lines for a match-heavy dispatcher.
- **Minimal repros per feature:** One minimal repro per IDE portion or feature; run and compare before expanding scope; avoid “test everything at once.”
- **Document the feature-add path:** Single doc (e.g. ADDING_A_LANGUAGE_FEATURE.md) with concrete file names and a minimal example; reduces context-switching and ad-hoc edits. (Not yet done; called out in strategic guidance.)
- **Consider unified diagnostics:** Parse, semantic, and codegen each have their own error types; a unified `CompilerDiagnostic` (span, severity, phase) would simplify LSP and “compile and show all diagnostics” (optional next iteration).

---

## 4. Expert Discussion: Reactions and Additions

The following is a synthesized discussion among expert personas reacting to the evaluation above and adding their own recommendations.

### Software Architect

**On the evaluation:** The “where we went wrong” table is accurate. The underlying pattern is **boundary violation**: we assumed APIs, reached into internals, and mixed concerns (IDE logic in runtime). The “how to do better” list correctly emphasizes contracts: verify APIs, define a compiler facade, keep layers separate.

**Addition:** Treat `compiler_api.rs` as the **only** public contract for the compiler library. Any tool (LSP, fmt, lint, debug) should depend only on that facade. If a tool needs something that isn’t there, add it to the API instead of importing from `parser::` or `semantic::` directly. That keeps the dependency graph acyclic and makes it obvious when a “quick fix” would break the abstraction. Also: the remaining monolithic piece is semantic `statements.rs` (~1,339 lines). Finish thinning it to a dispatcher plus submodules before adding more statement categories; otherwise every new feature touches the same giant file.

### Rust Expert

**On the evaluation:** The Token/parser story is a classic Rust lesson: **ownership and borrowing are part of the type contract**. Assuming `Token<'a>` when the type is actually owning caused lifetime and borrow errors that looked like “parser bugs” but were really “wrong type assumptions.” The refactoring failure (writeln_code) is the other side: when you change many call sites at once, you don’t get incremental compiler feedback, so mistakes compound.

**Addition:** Encode the “verify before coding” rule in practice: when touching a type you don’t own (e.g. `Token`, `TypedExpr`), open its definition first and note whether it owns or borrows, whether it’s `Copy`, and what methods exist. For large refactors, use a **branch-per-file or branch-per-module** strategy: one PR that only touches `control_flow.rs`, verify build and tests, then the next. That way the compiler is your guardrail. On unwrap/expect: the evaluation doesn’t stress it, but CODE_REVIEW_FULL does—prefer `expect("...")` with a clear invariant in non-test code so that if something breaks, the panic message tells you what contract was violated.

### Compiler Expert

**On the evaluation:** The pipeline (lex → parse → semantic → IR → codegen) is standard and well-scoped. The mistakes are less about compiler theory and more about **process**: no phase “failed” conceptually; we had API mismatches, refactor scope, and integration strategy. The “thin dispatcher” advice is exactly what you want in a codegen backend: one place that says “for this IR node, call that emitter,” and each emitter is a separate file. That’s how you keep codegen maintainable as the language grows.

**Addition:** For “add a language feature,” the doc says to have one place that lists files—good. From a compiler perspective, add one more rule: **when you add a new AST variant, add the corresponding TypedIR variant and codegen arm in the same logical change (or immediately after)**. Don’t leave “parsed but not type-checked” or “type-checked but not codegen’d” for long; it creates a matrix of partial support that’s hard to reason about. A small feature matrix (“X is parsed, Y is codegen’d”) is useful, but the real win is not letting the matrix get large. Also: the dual runtime (inline vs external) is a real design point. Document the **contract** that generated C expects from the runtime (symbols, calling convention, error handling) so that both paths stay in sync.

### Pragmatic Engineer

**On the evaluation:** I agree with all of it. The only thing I’d stress: **“refactor in small steps”** and **“minimal repro per feature”** are the same idea—reduce the blast radius. When something goes wrong, you want to know “which file” or “which test” broke, not “something in the last two weeks of changes.”

**Addition:** Turn “document the feature-add path” into a single **ADDING_A_LANGUAGE_FEATURE.md** with: (1) the list of files to touch (AST, parser, typed_ir, checker, codegen), (2) a minimal example (e.g. one new statement that does nothing in codegen at first), and (3) the order of operations (add AST, then parse, then type-check, then codegen). That way a contributor doesn’t have to infer from CLAUDE.md or the codebase. For the IDE layers: the “separate versions, don’t patch previous” rule is right. If someone asks “can we just add a flag to layer1 instead of layer2?” the answer is no—you’ll end up with one version that’s supposed to do two things and regression risk on both. Keep layers as checkpoints.

### QA / Testing Engineer

**On the evaluation:** The “big-bang testing” and “verification gap” rows are the ones I care about most. Testing too much at once means you can’t isolate failure; not running build/tests after each step means you don’t know which step introduced the bug. The IDE equivalence plan’s “minimal repro per portion” is the right discipline.

**Addition:** For each “portion” (window, events, graphics, file I/O, etc.), there should be a **single minimal repro** that is runnable and has a clear pass criterion (e.g. “window opens and closes with X”). Prefer **automated or scripted** runs where possible (e.g. run repro, check exit code or output file); for GUI, document the exact manual steps and expected result so that “did it pass?” isn’t subjective. Add a **CI job** that runs the automatable subset (e.g. console-only or file-output repros) so that regressions in runtime or codegen are caught without manual IDE testing every time.

### Language / Runtime Specialist

**On the evaluation:** The API-assumptions and monolithic-files items both affect the **surface area** of the language we support: wrong assumptions break correctness; giant files make it hard to add new statements/functions without conflicts. The compiler API and thin dispatchers reduce the cognitive load when adding a new keyword or builtin.

**Addition:** Keep a **language coverage** view: what’s implemented end-to-end (parsed + type-checked + codegen’d), what’s parsed but stubbed in codegen, and what’s explicitly out of scope. That helps contributors avoid adding “half a feature” and helps users know what to expect. For the runtime: the evaluation mentions IDE logic being reverted from runtime. Going forward, the runtime should have a **clear contract** (what the generated C calls, what it does not do). IDE-specific behavior (e.g. “hide window until first show”) belongs in the application (the IDE’s .bas or C), not in the shared runtime library, so that other consumers of the runtime don’t get unexpected behavior.

### Synthesis

The experts agree that the evaluation is on target and that the main themes are: **contracts and boundaries** (API, compiler facade, layer separation), **incremental change** (small refactors, minimal repros, verify after each step), and **documentation** (feature-add path, language coverage, runtime contract). Priorities to add to “how to do better”: (1) ADDING_A_LANGUAGE_FEATURE.md with concrete files and order of operations, (2) language/feature matrix or coverage doc, (3) runtime contract doc for generated C, (4) CI for automatable IDE-equivalence repros. The existing “unified diagnostics” and “thin dispatcher” recommendations stand.

---

## 5. References

### Key docs

- [ARCHITECTURE.md](ARCHITECTURE.md) — Pipeline and module layout
- [STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md](STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md) — Multi-perspective review and recommendations
- [FILE_SPLITTING_ANALYSIS.md](ThingsToDo/FILE_SPLITTING_ANALYSIS.md) — File size and dispatcher thinning
- [CODE_REVIEW_FULL.md](CODE_REVIEW_FULL.md) — Full codebase review (bugs, unwrap, DRY, docs)
- [TODO_CONSOLIDATED.md](ThingsToDo/TODO_CONSOLIDATED.md) — Priorities and remaining work
- [QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md](ThingsToDo/QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md) — IDE portion testing strategy
- [QB64PE_IDE_GROUND_UP_BUILD.md](ThingsToDo/QB64PE_IDE_GROUND_UP_BUILD.md) — Layer-by-layer IDE build

### AgenticLogs (lessons and problems)

- [IndividualProblems/2026-01-16_problem-parser-token-api-mismatch.md](../AgenticLogs/IndividualProblems/2026-01-16_problem-parser-token-api-mismatch.md) — Parser Token API mismatch
- [2026-01-26_refactoring-failure-analysis.md](../AgenticLogs/2026-01-26_refactoring-failure-analysis.md) — writeln_code refactoring failure
- [2026-01-31_session-153_ide-ground-up-build.md](../AgenticLogs/2026-01-31_session-153_ide-ground-up-build.md) — IDE ground-up build and ide_layers
- [2026-01-31_session-151_stmt-codegen-file-splitting.md](../AgenticLogs/2026-01-31_session-151_stmt-codegen-file-splitting.md) — Stmt codegen file splitting
