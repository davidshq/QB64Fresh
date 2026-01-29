# Item #5 Implementation Recommendation: Runtime Architecture Improvements

**Date:** 2026-01-28  
**Last Updated:** 2026-01-28 (verified against codebase)  
**Perspectives:** Software Architect, Rust Expert, Pragmatic Engineer  
**Status:** Phase 1 Complete ✅ | Phase 2-3 Pending

---

## Executive Summary

Three perspectives converge on a **phased, incremental approach** that fixes immediate issues while building toward a cleaner architecture. The recommendation balances:
- **Immediate fixes** (Pragmatic Engineer) - resolve compilation errors now
- **Type-safe abstractions** (Rust Expert) - use enums and traits for mode handling
- **Clean architecture** (Software Architect) - proper dependency management and organization

**Recommended Approach:** Incremental refactoring with three phases, starting with quick fixes and evolving to a trait-based architecture.


---

## Analysis by Topic (All Perspectives per Item)

Each item below shows **Software Architect**, **Rust Expert**, and **Pragmatic Engineer** views together so recommendations can be compared in one place.

---

### Item 1: Emission order and forward declarations

**Software Architect**
- **Emission phases:** Separate into clear phases: (1) type definitions and forward declarations, (2) function implementations in dependency order, (3) initialization code.
- **Module boundaries:** Each runtime module should declare its dependencies explicitly, provide forward declarations, and emit implementations in correct order.
- **Single source of truth:** Runtime function signatures defined once, used for both inline and external modes.
- **Benefits:** Explicit dependencies prevent ordering issues; clear separation (declarations vs implementations); self-documenting.

**Rust Expert**
- **Trait-based emission:** Traits abstract emission so signatures and order are type-driven (e.g. `emit_external_decl`, `emit_inline`).
- **Generic emission:** Use generics to avoid scattered runtime mode checks; compile-time dispatch based on mode.
- **Benefits:** Compile-time safety; zero-cost abstractions via monomorphization; type-safe function signatures.

**Pragmatic Engineer**
- **Quick fixes first:** Add forward declarations for all runtime functions; fix type mismatches (e.g. `qb_shell_hide` using `qb_string_data()`); ensure `io.rs` is emitted before `system.rs` where needed.
- **Work with existing structure:** Keep current module layout; add dependency comments; use simple ordering (e.g. emit `io` before `system`).
- **Benefits:** Immediate results; low risk; preserves working code.

---

### Item 2: Dependency tracking and module boundaries

**Software Architect**
- **Dependency graph:** Build explicit dependency tracking for runtime functions.
- **RuntimeEmitter trait:** Each emitter declares `dependencies()` and emits forward declarations and implementations; topological sort ensures correct ordering.
- **Concerns:** May be overkill for ~50 runtime functions; topological sort adds complexity.

**Rust Expert**
- **Dependencies on the trait:** e.g. `fn dependencies(&self) -> &[&'static str]` so each function declares what it depends on.
- **Type-safe registry:** Const registry of runtime functions with compile-time checks (with current workarounds for const trait objects).
- **Concerns:** May require significant refactoring; const trait objects not yet stable.

**Pragmatic Engineer**
- **Incremental:** Phase 1: fix compilation and order. Phase 2: add explicit emission order (e.g. `DEPENDENCIES` constant, manual `EMISSION_ORDER`). Phase 3: consider traits only if needed.
- **Measure first:** Profile before optimizing; only add dependency machinery if ordering causes real issues.
- **Concerns:** Manual ordering is error-prone; may not scale if runtime grows a lot.

---

### Item 3: Runtime mode abstraction

**Software Architect**
- **Single source of truth:** One definition of runtime function signatures used for both inline and external modes to avoid drift and duplication.

**Rust Expert**
- **Enum with associated data:** Implemented in `src/codegen/c_backend/mod.rs`: `RuntimeMode::Inline { type_registry }` and `RuntimeMode::External { header_path }` with `inline()`, `external()`, `is_inline()`, `is_external()`, `string_data_access()`.
- **Generic emission:** Not implemented; `fn emit_runtime<W, M: RuntimeMode>(writer: &mut W, mode: M)` would allow compile-time dispatch if adopted later.
- **Benefits:** Can’t mix modes incorrectly; clear API.

**Pragmatic Engineer**
- **Keep current structure:** Don’t rewrite mode handling in one go; keep existing module organization and add comments/documentation where useful.

---

### Item 4: Extensibility and long-term architecture

**Software Architect**
- **RuntimeEmitter / topological sort:** Makes adding new runtime functions and modules straightforward; dependencies are explicit and ordering is automated.
- **Benefits:** Easy to add new runtime functions; self-documenting dependency graph.

**Rust Expert**
- **RuntimeFunction trait + per-function structs:** Each function is a small type implementing the trait (name, signature, dependencies, `emit_inline_impl`). Registry holds all of them and emits in sorted order.
- **Benefits:** Easy to extend; type-safe signatures; optional compile-time dependency checking later.

**Pragmatic Engineer**
- **Defer heavy design:** Phase 1 fixes compilation and order. Phase 2 only if Phase 1 shows ordering is still error-prone. Phase 3 (trait-based) only if Phase 2 shows limitations or runtime grows (e.g. >100 functions).
- **YAGNI:** Avoid over-engineering; add complexity when it solves real problems.

---

### Item 5: Incremental approach and risk

**Software Architect**
- Phased rollout: fix ordering and declarations first, then add dependency metadata, then consider full trait-based emission if needed.
- Clear module boundaries and emission phases reduce risk when changing runtime code.

**Rust Expert**
- Introduce enum and traits incrementally: e.g. RuntimeMode enum first, then trait-based emission in a later phase so refactors are bounded and type-safe.

**Pragmatic Engineer**
- Phase 1: 1–2 hours, low risk. Phase 2: 1–2 days, medium risk (many files but localized). Phase 3: only if needed, higher risk but best long-term design.
- Preserve working behavior at each step; measure before optimizing.

---

## Synthesized Recommendation: Phased Approach

**Unanimous agreement:** All three perspectives support a phased, incremental approach that (1) fixes immediate issues first, (2) adds type-safe abstractions where useful, and (3) improves architecture over time.

---

### Phase 2: Dependency tracking (Short-term – 1–2 days) ⏸️ DEFERRED

**Goal:** Make dependencies explicit and prevent ordering bugs.

**Perspectives (consolidated):**
- **Architect:** Each module declares `DEPENDENCIES`; emission uses a simple ordering (or topological sort) so dependencies are explicit.
- **Rust Expert:** Same idea with type-level or const metadata (e.g. `DEPENDENCIES` on a trait or per-module constant).
- **Pragmatic:** Add `DEPENDENCIES` and a manual `EMISSION_ORDER`; consider RuntimeMode enum refinements; only do this if Phase 1 shows ordering is still fragile.

**Planned changes (when revisited):**
1. **Dependency metadata:** e.g. `pub const DEPENDENCIES: &[&str] = &["qb_print_string", "qb_print_newline"]` per runtime submodule (arrays, audio, debug, error, file, graphics, io, keyboard, legacy, math, memory, strings, system, timing, types).
2. **Explicit emission order:** e.g. `EMISSION_ORDER` list matching current order in `emit_runtime_declarations()`.
3. **RuntimeMode enum:** Already in `src/codegen/c_backend/mod.rs`; Phase 2 would use it more consistently if needed.

**Status:** Deferred – Phase 1 + manual ordering is sufficient for current runtime size. Revisit if runtime grows or ordering errors recur.

---

### Phase 3: Trait-based architecture (Future – if needed) ⏸️ FUTURE

**Goal:** Type-safe, extensible runtime emission with clear dependency and signature rules.

**Perspectives (consolidated):**
- **Architect:** Full `RuntimeEmitter`-style trait with `dependencies()`, forward declarations, and implementations; topological sort for global order.
- **Rust Expert:** `RuntimeFunction` trait, per-function structs, registry, and generic emission; optional const registry when stable.
- **Pragmatic:** Only if Phase 2 shows limitations, runtime function count grows (e.g. >100), or compile-time dependency checking becomes important.

**When to implement:** After Phase 2 (if adopted); when runtime grows significantly or maintenance cost justifies the refactor.

**Risk:** High – large refactor; best long-term design but not needed yet.

---

## Implementation Status

### Phase 1 ✅ Complete
- Forward declarations: `emit_forward_declarations()` in `src/codegen/c_backend/runtime/mod.rs`; emission order in `emit_runtime_declarations()` (io before system, etc.).
- QB64pe compilation issues addressed; manual ordering is working.
- RuntimeMode: `src/codegen/c_backend/mod.rs`; used in expr, stmt, file_io, runtime.

### Phase 2 ⏸️ Deferred
- **Rationale:** Manual emission order in `emit_runtime_declarations()` and central `emit_forward_declarations()` are enough for current runtime size; no ordering errors observed since Phase 1.
- **Revisit if:** Runtime grows a lot, ordering errors return, or many people add runtime functions frequently.

### Phase 3 ⏸️ Future consideration
- **Rationale:** Phase 1 (and optional Phase 2) meet current needs; YAGNI.
- **Consider if:** Phase 2 is insufficient, runtime function count >100, or compile-time dependency checking becomes critical.

---

## Recommended Implementation Order

1. ✅ **Phase 1** – DONE  
2. ⏸️ **Phase 2** – DEFERRED (revisit if runtime grows or ordering issues recur)  
3. ⏸️ **Phase 3** – FUTURE (only if Phase 2 shows limitations or scale demands it)

---

## Decision Criteria

**Phase 1:** ✅ Implemented – compilation errors needed fixing; quick wins were valuable.

**Phase 2:** Implement if Phase 1 ordering remains error-prone, runtime function count exceeds ~50, or multiple developers work on runtime.

**Phase 3:** Implement if Phase 2 is insufficient, compile-time dependency checking is required, runtime function count exceeds ~100, or long-term maintenance is a priority.

---

## Consensus Points

All three perspectives agree on:

1. **Incremental approach** – Avoid big-bang rewrites.
2. **Fix immediate issues first** – Compilation errors block progress.
3. **Explicit dependencies** – Prefer explicit over implicit ordering.
4. **Type safety where it helps** – Without over-engineering.
5. **Measure before optimizing** – Profile if performance becomes a concern.

---

## Next Steps

1. ✅ **Phase 1 complete** – Forward declarations and emission ordering in place.
2. ⏸️ **Phase 2 deferred** – Manual ordering sufficient for current size.
3. ⏸️ **Phase 3 future** – Revisit only if runtime architecture becomes a bottleneck.

**Monitoring:** Watch for ordering errors as runtime grows; track runtime function count; revisit Phase 2 if manual ordering becomes error-prone.

---

## Codebase Reference (2026-01-28)

| Item | Location |
|------|----------|
| RuntimeMode enum | `src/codegen/c_backend/mod.rs` (lines 98–239) |
| CBackend, emit flow | `src/codegen/c_backend/mod.rs` |
| emit_header_with_debug | `src/codegen/c_backend/runtime/mod.rs` (lines 83–161) |
| emit_runtime_declarations | `src/codegen/c_backend/runtime/mod.rs` (lines 1226–1261); calls `emit_forward_declarations` then types, strings, io, system, math, file, error, keyboard, memory, timing, arrays, audio, graphics, legacy in order |
| emit_forward_declarations | `src/codegen/c_backend/runtime/mod.rs` (lines 1100–1134) |
| Runtime submodules | arrays, audio, debug, error, file, graphics, io, keyboard, legacy, math, memory, strings, system, timing, types |
| qb_shell_hide / qb_string_data | `src/codegen/c_backend/runtime/system.rs` (lines 260, 264) |
| string_data_access usage | `expr.rs`, `stmt/mod.rs`, `stmt/assignments.rs`, `file_io.rs` (all under `src/codegen/c_backend/`) |

**Document verification:** Updated to match the codebase on 2026-01-28.
