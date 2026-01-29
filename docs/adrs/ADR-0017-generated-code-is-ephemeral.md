# ADR-0017: Generated Code Is Ephemeral (Fix Code Generator, Never Patch Output)

## Status

**Accepted** - January 28, 2026

## Context

QB64Fresh emits C code from the code generator (`src/codegen/c_backend/`). Generated `.c` files are outputs of the compiler, not source artifacts. When generated code is wrong (e.g. compile error, wrong behavior), there is a temptation to patch the generated file directly to unblock work or tests.

Key considerations:

- Generated files are overwritten on every `--emit-c` run
- Patching output hides bugs in the code generator
- Contributors and automation must know that generated C is never the place to fix bugs
- The rule must be explicit so that AI assistants, new contributors, and CI follow it

## Decision

**We treat generated C files as ephemeral outputs. Bugs are fixed in the code generator, never by editing or patching generated code.**

### Rule

- **NEVER**: Edit generated `.c` files, use `sed`/scripts to patch them, or commit patched generated code
- **ALWAYS**: Fix the bug in `src/codegen/c_backend/` (or the phase that produced wrong IR), then regenerate

### Workflow

1. Identify the problem in generated C (e.g. compile error, wrong expression)
2. Trace it back to the code generator (e.g. `stmt/assignments.rs`, `expr.rs`, `runtime/strings.rs`)
3. Fix the generator so it emits correct C
4. Regenerate the C file
5. Verify compilation and behavior

### Exception

The only exception is temporary patching while debugging the code generator itself (e.g. to verify a hypothesis). Any such patch must be removed once the generator is fixed, and patched generated files must never be committed.

### Rationale

- Generated files are outputs, not sources; patching them is overwritten on next run
- Patching hides the real bug and wastes time on temporary fixes
- Enforcing "fix the generator" keeps the codebase consistent and improves the compiler for all programs

### References

- Project rule: `.cursor/rules/qb64fresh-codegen.mdc`
- Code generator: `src/codegen/c_backend/`

## Consequences

### Positive

- Single source of truth: fixes improve the compiler for all users
- No accidental commits of patched output
- Clear guidance for contributors and tooling

### Negative

- Requires discipline to trace every generated-code failure back to the generator
- Some fixes may be more involved than a one-line patch in the output

## References

- [ADR-0002](ADR-0002-code-generation-backend.md) – C backend architecture
- [reference/CODEGEN_WRITE_HELPERS.md](../reference/CODEGEN_WRITE_HELPERS.md) – Codegen write helpers
