# QB64Fresh Consolidated TODO

**Last Updated:** 2026-01-31 (Session 125)

*Consolidates priorities and architectural review. Completed review items: [ARCHITECTURAL_REVIEW_COMPLETED.md](../archive/ARCHITECTURAL_REVIEW_COMPLETED.md).*

---

## Current Codebase Snapshot (2026-01-31)

- **Compiler:** Full pipeline (lexer → parser → semantic → codegen → C). Dual runtime: `inline` (emitted C) and `external` (libqb64fresh_rt).
- **Codegen:** Still accumulates output in `String` via `write_code!`/`writeln_code!`; no streaming yet.
- **Runtime:** `runtime/` has SDL2 graphics, Rodio audio, file I/O, strings, math, memory, timing, error handling, bitops, logging, HTTP, mutex/condvar, game controller, dialogs, etc. Inline runtime in `src/codegen/c_backend/runtime/`.
- **Tools:** `qb64fresh-fmt`, `qb64fresh-lint`, `qb64fresh-lsp`; debug adapter (tools/debug) infrastructure complete. **Not yet:** `qb64fresh doctor`.
- **Tests:** 1,500+ tests; QB45 compat 99.1% (114/115). Runtime comparison vs QB64pe in `QB64pe/runtime_comparison/`.

**Architectural grade:** A- — remaining issues are optimization and organization, not fundamental architecture.

---

## Medium-term Priorities

### 3. Stream Code Generation ⚠️ **MEMORY OPTIMIZATION**

**Issue:** Codegen accumulates entire C output in `String` (e.g. 114K+ lines for full QB64pe → high memory use).  
**Current:** `write_code!`/`writeln_code!` append to `&mut String`; expression/statement emission uses `format!()`.  
**Fix:** Refactor to use `Write` trait and stream directly to file (or buffer with reuse).  
**Details:** Profile to confirm impact before optimization; consider `Cow<'static, str>` for generated code strings where appropriate.  
**Effort:** ~1 week  
**Priority:** Medium (do if memory/scale becomes an issue)

---

### 4. Runtime Mode Abstraction

**Issue:** Inline vs External runtime selected via `if`/`else` and duplicated paths in codegen.  
**Current state:** `RuntimeMode` enum in `src/codegen/c_backend/mod.rs` with `Inline { type_registry }` and `External { header_path }`. Forward declarations in `runtime/mod.rs`; manual emission ordering in `emit_runtime_declarations()`.  
**Consider:** Trait-based abstraction for “runtime provider” only if it simplifies code or adds backends; otherwise document current approach (YAGNI).  
**Deferred:** Phase 2 dependency tracking (manual ordering sufficient for 18 runtime submodules); Phase 3 trait-based architecture only if runtime grows significantly. Runtime library organization (separate crate or codegen) only if maintenance becomes an issue.  
**Effort:** 1-2 weeks  
**Priority:** Low–Medium

---

## Long-term Priorities

### 7. Phase 6: Optimization

**Remaining:**
- [ ] Dead code elimination (2–3 sessions)
- [ ] Loop optimization (2–3 sessions)
- [ ] Inline small functions (2–3 sessions)

---

### 8. Distribution and Installer

**Phases:**
1. **Minimal:** Build binaries (Windows x64, macOS x64+arm64, Linux x64), publish on GitHub Releases.
2. **Runtime:** Ship `libqb64fresh_rt.a`/`.lib` and header; document `--runtime external`.
3. **Installers:** Windows (NSIS/Inno), macOS (.pkg/.dmg), Linux (.deb/.rpm).
4. **Polish:** See *Current Codebase Snapshot* for tools status. **Not yet:** `qb64fresh doctor`, code signing.

---

### Remaining by Tier

| Tier | Area | Remaining |
|------|------|-----------|
| 1 | qbs-compatible string system | qbs* layout, tmp pool, CMEM, fixed-length (~3,000 lines) — **only if QB64pe binary compatibility needed** (optional) |
| 2 | Graphics | Advanced _DEST/_SOURCE, OpenGL stubs, some PUT modes (~1,000 lines); stubs present. |

### Implementation Phases (remaining)

| Phase | Step | Delivers | Status |
|-------|------|----------|--------|
| 8 | 8.1–8.2 | QB64pe IDE build + run + basic tests; regression/compat | **Remaining** |
| 4 | 4.1 | ERL/ERR/_ERRORLINE/_ERRORMESSAGE$ verification | **Done** — [ERL_ERR_VERIFICATION.md](../archive/ERL_ERR_VERIFICATION.md) |
| 2 | 2.1 | Codegen: emit `evnt` around statements for debugger | **Done** — evnt wrapper when `--debug` |
| 5–7 | — | CMEM, mem_lock/_MEM, qbs compatibility | Optional — [CMEM_QBS_COMPAT_STATUS.md](../archive/CMEM_QBS_COMPAT_STATUS.md) |

### Success criteria

- [ ] QB64pe compiled with QB64Fresh + our runtime: executable runs, IDE starts, basic edit/run and error handling (Phase 8.1–8.2)

---

## Related Documentation

**Planning:** [TODO-completed.md](../archive/TODO-completed.md), [ARCHITECTURAL_REVIEW_COMPLETED.md](../archive/ARCHITECTURAL_REVIEW_COMPLETED.md)  
**Status / Option B:** [PARTIAL_IMPLEMENTATIONS.md](PARTIAL_IMPLEMENTATIONS.md), [BOOTSTRAP_VALIDATION.md](../archive/BOOTSTRAP_VALIDATION.md), [RUNTIME_AND_PARITY.md](RUNTIME_AND_PARITY.md), [GENERATED_C_REVIEW.md](GENERATED_C_REVIEW.md)  
**Other:** [OPENGL_GLUT_DESIGN.md](OPENGL_GLUT_DESIGN.md), [CODE_REVIEW_PLAN.md](CODE_REVIEW_PLAN.md)
