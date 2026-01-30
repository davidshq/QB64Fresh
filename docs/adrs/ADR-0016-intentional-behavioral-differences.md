# ADR-0016: Intentional Behavioral Differences from QB64pe

## Status

**Accepted** - January 28, 2026

## Context

QB64Fresh aims for QB45-style compatibility and a sustainable subset of QB64 extensions, but we are not a drop-in clone of QB64pe. Some behaviors in QB64pe are permissive, implementation-specific, or undesirable for clarity and safety. We need a clear policy: when we deliberately behave differently, we document it and treat it as intentional.

Key considerations:

- Users porting from QB64pe need to know what will break or behave differently
- Stricter or different behavior can improve safety and debuggability (e.g., multiple errors, strict GOTO)
- Some differences are consequences of our architecture (e.g., C backend → different identifier mangling, different PRNG)

## Decision

**We document and enforce intentional behavioral differences from QB64pe in a single canonical document and reference it from ADRs and migration docs.**

### Policy

1. **Document**: All deliberate deviations (stricter rules, excluded features, or different-by-design behavior) are listed in [QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md](../QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md) (section “Intentional differences”).
2. **Do not emulate permissive bugs**: We do not replicate QB64pe behavior that hides programmer errors (e.g., creating a new label in the current procedure when GOTO targets a label that exists only in another procedure).
3. **Improve where justified**: We may choose better behavior (e.g., collecting and reporting multiple errors per run instead of stopping at the first).
4. **Accept implementation consequences**: Some differences follow from our design (C backend, different runtime) and are documented rather than “fixed” for parity.

### Examples (see INTENTIONAL_DIFFERENCES for full list)

| Area | QB64pe | QB64Fresh | Rationale |
|------|--------|-----------|-----------|
| Cross-procedure GOTO | May create forward-reference label in current scope | Error: label must be in current procedure | Stricter; avoids hiding typos or wrong-scope intent |
| Error reporting | Often first error only | Multiple errors per run | Better debugging; see [ADR-0011](ADR-0011-error-handling.md) |
| Raw OpenGL (`_GL*`) | Supported | Excluded | Scope; see [ADR-0014](ADR-0014-scope-and-excluded-features.md) |
| RND / RANDOMIZE | QB64pe PRNG/sequence | Different PRNG → different sequence | Implementation choice; document for compatibility |
| Identifier mangling | QB64pe C++ conventions | C-safe mangling (different names) | C backend requirement |

### Relationship to Other ADRs

- **ADR-0014** (Scope and excluded features): Defines what we exclude (e.g., `_GL*`, legacy DOS). This ADR covers *behavioral* differences (stricter or different semantics) and the policy of documenting them.
- **ADR-0011** (Error handling): Explains multi-error collection; INTENTIONAL_DIFFERENCES calls out that we do not emulate single-error-stop.

## Consequences

### Positive

- Single source of truth for “why is this different?” improves migration and support
- Stricter behavior can catch bugs that QB64pe would hide
- Clear policy reduces debate over whether a difference is a bug or by design

### Negative

- Code that relies on QB64pe’s permissive behavior will need changes
- Users must check QB64Fresh vs QB64pe differences doc when porting (see References)

## References

- [QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md](../QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md) – Canonical list: intentional and behavioral/architectural differences (single doc)
- [ADR-0011](ADR-0011-error-handling.md) – Error handling (multiple errors)
- [ADR-0014](ADR-0014-scope-and-excluded-features.md) – Scope and excluded features
