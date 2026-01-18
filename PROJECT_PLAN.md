# QB64Fresh - Project Plan

## Project Goal

Create a complete, ground-up rewrite of QB64 - a modern BASIC compiler that compiles QBasic/QuickBASIC-compatible code to native executables.

## Key Principles

### What We're Building
- A modern, clean BASIC compiler
- Language Server Protocol (LSP) implementation for IDE integration
- VSCode extension (and potentially other editor integrations)
- Focus on the compiler intelligence, not IDE reproduction

### What We're NOT Doing
- Not forking or patching the existing codebase
- Not reproducing the built-in IDE (QB64pe has one; we'll use LSP instead)
- Not constrained to the same implementation language
- Not constrained to the same dependencies or architecture

### Strengths to Preserve
1. Complete QB4.5/QBasic compatibility
2. Extensive QB64 extensions (_MEM, _SND*, graphics, etc.)
3. Cross-platform support (Windows, Linux, macOS)
4. Comprehensive test suite


*Document created: 2026-01-16*
*Last updated: 2026-01-16 (Phase 3.7 VSCode Extension completed)*
