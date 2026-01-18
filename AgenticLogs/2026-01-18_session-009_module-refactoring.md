# Session 009: Module Refactoring - Splitting Large Files

**Date:** 2026-01-18
**Focus:** Refactoring large monolithic files into focused modules

## Summary

Split three large files (~7,100 lines total) into focused, maintainable modules:
- `src/parser/mod.rs` (2,356 lines) → 7 modules
- `src/semantic/checker.rs` (2,522 lines) → 6 modules
- `src/codegen/c_backend.rs` (2,281 lines) → 6 modules

## Rationale

Large monolithic files were becoming difficult to navigate and maintain. The split follows natural functional boundaries:

1. **Parser Split** - Separates token handling, expression parsing (Pratt parser), statement dispatching, control flow, procedures, and directives
2. **Checker Split** - Separates expression type checking, statement dispatching, assignments/IO, control flow validation, definitions (SUB/FUNCTION/DIM), and constant evaluation
3. **C Backend Split** - Separates runtime generation, type mapping, expression emission, statement emission, and program analysis

## Changes Made

### Parser Module Structure
```
src/parser/
├── mod.rs          # Parser struct, entry points, Precedence enum, tests (~325 lines)
├── tokens.rs       # Token navigation (peek, advance, expect) (~140 lines)
├── expressions.rs  # Pratt parser for expressions (~340 lines)
├── statements.rs   # Statement dispatcher + simple statements (~560 lines)
├── control_flow.rs # IF/FOR/WHILE/DO/SELECT parsing (~350 lines)
├── procedures.rs   # SUB/FUNCTION/TYPE definitions (~310 lines)
├── directives.rs   # $INCLUDE, $IF meta-commands (~150 lines)
└── error.rs        # Parse error types (unchanged)
```

### Checker Module Structure
```
src/semantic/checker/
├── mod.rs          # TypeChecker struct, helpers, tests (~500 lines)
├── expressions.rs  # Expression type checking (~340 lines)
├── statements.rs   # Statement dispatcher (~300 lines)
├── assignments.rs  # Assignment & I/O checking (~220 lines)
├── control_flow.rs # Loop/branch checking (~360 lines)
├── definitions.rs  # SUB/FUNCTION/DIM/CONST (~280 lines)
└── const_eval.rs   # Constant expression evaluation (~230 lines)
```

### C Backend Module Structure
```
src/codegen/c_backend/
├── mod.rs       # CBackend struct, CodeGenerator impl (~180 lines)
├── runtime.rs   # Inline C runtime generation (~420 lines)
├── types.rs     # Type mapping utilities (~130 lines)
├── expr.rs      # Expression emission (~340 lines)
├── stmt.rs      # Statement emission (~900 lines)
└── analysis.rs  # Global/DATA collection (~180 lines)
```

## Technical Approach

Used Rust's `impl` block extension pattern - the main struct stays in `mod.rs`, and each submodule adds methods via `impl TypeChecker<'a>` blocks. This:
- Keeps related code together
- Maintains the same public API
- Allows incremental compilation benefits
- Makes navigation easier

## Verification

All verification steps passed:
- `cargo check` - Clean compilation
- `cargo test` - All 120 tests pass
- `cargo clippy` - Only pre-existing warnings (too_many_arguments)

## Key Decisions

1. **Tests stay in mod.rs** - Keeps tests close to the public API they verify
2. **Helper structs stay in mod.rs** - `LoopContext`, `ForLoopInfo` used across submodules
3. **Used `pub(super)` visibility** - Submodule methods visible to parent but not public

## Lessons Learned

1. When splitting modules, carefully trace import dependencies - easy to accidentally remove imports still needed by other code in the same file
2. Test modules need their own imports even when `use super::*` is present
3. The split preserves all functionality while improving navigability
