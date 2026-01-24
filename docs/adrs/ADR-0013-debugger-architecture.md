# ADR-0013: Debugger Architecture

## Status

**Accepted** - January 24, 2026

## Context

QB64pe provides debugging via a TCP-based protocol (vwatch) where the BASIC program itself runs debug logic and communicates with the IDE. QB64Fresh has no built-in IDE; tooling follows an LSP-based, editor-agnostic approach (ADR-0009).

We need source-level debugging (breakpoints, stepping, variable inspection, call stack) that works with standard editors (VS Code, etc.) and does not require embedding a debugger inside the compiled BASIC program.

Key considerations:
- QB64pe: `$DEBUG`, vwatch, TCP to IDE; debug code runs in the compiled program
- Editor integration: VS Code and others expect Debug Adapter Protocol (DAP)
- Separation: Compiler and runtime are separate; debugger should not require forking the runtime
- Symbol and source mapping: Need to map generated C / execution back to BASIC source

## Decision

**We implement a DAP-oriented debugger as a separate tool (`tools/debug`) that extracts symbols from the AST and is designed to integrate with the runtime via codegen and runtime hooks.**

### Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│ Editor (e.g.    │ DAP │ qb64fresh-debug  │     │ Compiled        │
│ VS Code)        │◄───►│ (tools/debug)    │◄───►│ program +       │
│                 │     │                  │     │ runtime         │
└─────────────────┘     └────────┬────────┘     └─────────────────┘
                                  │
                    ┌─────────────┼─────────────┐
                    │             │             │
                    ▼             ▼             ▼
              symbols.rs    frames.rs     values.rs
              (from AST)   (call stack)  (value repr)
                    │             │             │
                    └─────────────┴─────────────┘
                              dap.rs (DAP types)
```

### Components (Infrastructure Complete)

| Module    | Purpose |
|----------|---------|
| `symbols` | Extract procedures, variables, scopes, types, labels from AST |
| `frames`  | Call stack representation (frames, scopes, variable refs) |
| `values`  | Variable value representation (scalars, arrays, UDTs) and display formats |
| `dap`     | Debug Adapter Protocol request/response types |
| `sources` | Multi-file source management, `$INCLUDE`-aware, source positions |
| `watch`   | Watch expression parsing (`x`, `arr(i,j)`, `player.x`) |
| `config`  | Breakpoint kinds, debug configuration, verbosity |

### DAP Choice

- **DAP** enables any DAP-capable editor (VS Code, Cursor, etc.) to support breakpoints, step, watches, and stack without a custom IDE.
- **Alternative (TCP/custom):** Would require a custom VS Code extension and reimplementation for other editors. DAP is the standard.

### Infrastructure vs. Runtime Integration

The `tools/debug` library and `qb64fresh-debug` CLI provide:

- **Done:** Symbol extraction from AST, DAP types, frames/values/sources/watch data structures, CLI scaffolding
- **Pending:** Debug info in generated C (line mappings, variable locations), runtime hooks to pause on breakpoints, and a protocol between the debugger and the running process to read memory and control execution (e.g., spawn/handle the debugee, implement `launch`/`attach`).

This “infrastructure first” approach allows the debugger’s data model and DAP surface to be designed and tested before committing to a specific runtime integration (e.g., GDB-style, custom agent, or instrumentation in generated C).

## Consequences

### Positive

- Editor-agnostic: works with any DAP client
- Clean separation: debugger is a separate process/tool, not embedded in the BASIC program
- Reuses compiler AST for symbols; no duplicated symbol logic
- DAP types and structures are in place for future runtime integration

### Negative

- Runtime integration is non-trivial: requires codegen changes and a debugee–debugger protocol
- No functioning breakpoints or stepping until that integration exists
- QB64pe’s vwatch approach is incompatible; migration means “use the new debugger” when ready

### Implementation Status

| Component           | Status |
|--------------------|--------|
| `tools/debug` lib  | Complete |
| Symbol extraction  | Complete |
| Frames, values     | Complete |
| DAP types          | Complete |
| Sources, watch     | Complete |
| CLI (`qb64fresh-debug`) | Scaffolding |
| Debug info in C output | Not started |
| Runtime hooks      | Not started |
| Launch/attach flow | Not started |

## References

- [DEBUGGING.md](../DEBUGGING.md) – QB64pe vwatch and protocol
- [FUTURE.md](../ThingsToDo/FUTURE.md) – Debugger runtime integration (High Priority)
- [CODEBASE_REVIEW_CONSOLIDATED.md](../ThingsToDo/CODEBASE_REVIEW_CONSOLIDATED.md) – Debugger subsection
- [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) – DAP specification
