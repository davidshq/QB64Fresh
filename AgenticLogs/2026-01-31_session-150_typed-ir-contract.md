# Session 150: Typed IR contract (formalize)

**Date:** 2026-01-31

## Goal

Implement item 1 from the strategic guidance document: formalize the “typed IR contract” between semantic and codegen.

## Outcome

Created **`docs/reference/TYPED_IR_CONTRACT.md`** and wired it into the reference index.

### Document contents

1. **Role of the typed IR** — Typed IR is the single contract; codegen is only invoked when semantic returns `Ok(TypedProgram)`.

2. **TypedExprKind variants codegen must handle** — Full table of all expression kinds (IntegerLiteral, FloatLiteral, StringLiteral, Variable, Binary, Unary, Grouped, FunctionCall, ArrayAccess, ArrayRef, Convert, FieldAccess, ExternalFunctionCall, ProcPtr, CvFunc, MkDollarFunc, CastFunc, ValWithType, MemGetTyped) with brief descriptions.

3. **TypedStatementKind and codegen dispatch** — Grouped by target module (assignments, control_flow, io, definitions, data, call, error_jump, def_fn, file_io, graphics, audio, system, misc, meta); notes that every variant must be handled (some as no-ops).

4. **Invariants semantic guarantees** — No `BasicType::Unknown` in emitted expressions; array dimensions resolved; procedure/symbol resolution; type consistency; no duplicate definitions.

5. **What codegen may assume** — Concrete types, array bounds set, control-flow labels exist, C names provided, order and structure preserved.

6. **Adding a new language feature** — Checklist: AST → Parser → Typed IR → Semantic → Codegen (expr or stmt) → update this doc.

### Other changes

- **docs/reference/README.md** — Added `TYPED_IR_CONTRACT.md` to the contents table; updated “Last updated” to 2026-01-31.

## References

- `docs/STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md` (lines 20–21) — source of the request.
- `src/semantic/typed_ir.rs` — source of truth for IR types.
- `src/codegen/c_backend/expr/mod.rs`, `src/codegen/c_backend/stmt/mod.rs` — codegen dispatch.
