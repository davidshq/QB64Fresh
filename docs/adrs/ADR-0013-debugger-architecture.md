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

### Runtime Integration (Complete)

The debugger is now fully implemented with:

**Compiler Support (`--debug` flag):**
- `qb_dbg_line()` calls emitted before each executable statement
- `qb_dbg_enter_proc()`/`qb_dbg_exit_proc()` for call stack tracking
- Named pipe IPC initialization for debugger communication

**Debug Protocol (`tools/debug/src/protocol.rs`):**
- `DebugCommand` enum: Continue, StepInto, StepOver, StepOut, Pause, Terminate, breakpoint management
- `DebugEvent` enum: Ready, Stopped, Terminated, variable values, location updates
- Text-based serialization for pipe communication

**DAP Server (`tools/debug/src/server.rs`):**
- Full Debug Adapter Protocol implementation for VS Code/Cursor
- Handles: initialize, launch, setBreakpoints, threads, stackTrace, scopes, variables, continue, step*, pause, evaluate, disconnect
- Named pipe communication with debugee process

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
| `tools/debug` lib  | ✅ Complete |
| Symbol extraction  | ✅ Complete |
| Frames, values     | ✅ Complete |
| DAP types          | ✅ Complete |
| Sources, watch     | ✅ Complete |
| CLI (`qb64fresh-debug`) | ✅ Complete |
| Debug info in C output | ✅ Complete |
| Runtime hooks      | ✅ Complete |
| Debug protocol     | ✅ Complete |
| DAP server         | ✅ Complete |
| Launch flow        | ✅ Complete |

**Tests:** 50 tests passing in the debugger crate.

## References

- [DEBUGGING.md](../DEBUGGING.md) – QB64pe vwatch and protocol
- [tools/README.md](../../tools/README.md) – Debugger tool documentation
- [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) – DAP specification
