# QB64Fresh Debugger Documentation

## Overview

The QB64Fresh debugger provides source-level debugging capabilities for BASIC programs compiled with QB64Fresh. It supports both interactive command-line debugging and IDE integration via the Debug Adapter Protocol (DAP).

### Features

- **Breakpoints**: Set line, function, label, and conditional breakpoints
- **Stepping**: Step over, step into, step out, and continue execution
- **Variable Inspection**: View variables, arrays, and user-defined types
- **Watch Expressions**: Monitor variable values and expressions
- **Call Stack**: Navigate the call stack and inspect frames
- **Source Mapping**: Map compiled code back to BASIC source lines
- **Multi-file Support**: Handle `$INCLUDE` directives and multiple source files
- **DAP Integration**: Works with VS Code, Cursor, and other DAP-compatible IDEs

## Architecture

The debugger consists of several components:

```
┌─────────────────────────────────────────────────────────────┐
│                    Debugger Components                       │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   CLI Tool   │  │  DAP Server  │  │   Library    │     │
│  │ (main.rs)    │  │  (server.rs) │  │  (lib.rs)    │     │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘     │
│         │                 │                  │              │
│         └─────────────────┼──────────────────┘              │
│                           │                                 │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Core Debugger (lib.rs)                   │  │
│  │  - Source file management                             │  │
│  │  - Breakpoint management                              │  │
│  │  - Execution state tracking                          │  │
│  └──────────────────────────────────────────────────────┘  │
│                           │                                 │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Supporting Modules                       │  │
│  │  - symbols.rs: Debug symbol extraction               │  │
│  │  - values.rs: Variable value representation          │  │
│  │  - frames.rs: Call stack management                  │  │
│  │  - watch.rs: Watch expression parsing                │  │
│  │  - sources.rs: Multi-file source management         │  │
│  │  - protocol.rs: Debug protocol (commands/events)     │  │
│  │  - dap.rs: DAP message types                         │  │
│  └──────────────────────────────────────────────────────┘  │
│                           │                                 │
│  ┌──────────────────────────────────────────────────────┐  │
│  │         Runtime Integration (Future)                 │  │
│  │  - Debug hooks in generated C code                   │  │
│  │  - Named pipe communication                           │  │
│  │  - Variable memory access                             │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Current Status

**Infrastructure Complete (44 tests passing):**
- ✅ Debug symbol extraction from AST
- ✅ Variable value representation
- ✅ Call stack structures
- ✅ Watch expression parsing
- ✅ Source file management
- ✅ Breakpoint management
- ✅ DAP protocol implementation
- ✅ CLI interface

**Runtime Integration Needed:**
- ⏳ Debug info emission in generated C code
- ⏳ Runtime hooks for breakpoints and stepping
- ⏳ Memory access for variable inspection
- ⏳ Named pipe communication with debugee

## Installation

The debugger is included in the QB64Fresh tools workspace. Build it with:

```bash
cd tools/debug
cargo build --release
```

Or build all tools:

```bash
cargo build --release --workspace
```

The binary will be available at `target/release/qb64fresh-debug`.

## Command-Line Interface

### Basic Usage

```bash
# Debug a program interactively
qb64fresh-debug myprogram.bas

# Debug with initial breakpoint
qb64fresh-debug --break 10 myprogram.bas

# Debug with breakpoint at function entry
qb64fresh-debug --break MySub myprogram.bas

# Start in DAP mode for IDE integration
qb64fresh-debug --dap

# Verbose output for troubleshooting
qb64fresh-debug --verbosity verbose myprogram.bas
```

### Command-Line Options

```
USAGE:
    qb64fresh-debug [OPTIONS] [FILE] [-- <PROGRAM_ARGS>...]

OPTIONS:
    -b, --break <LOCATION>     Set initial breakpoint (line number or function name)
    --break-on-entry          Break on program entry
    --break-on-error          Break on errors [default: true]
    --dap                     Start in DAP (Debug Adapter Protocol) mode
    --port <PORT>             Port for DAP mode [default: 4711]
    --verbosity <LEVEL>       Output verbosity [default: normal]
                              [possible values: quiet, normal, verbose, trace]
    --config <FILE>           Configuration file path
    -I, --source-path <PATH>  Source file search paths
    --list-commands           List available debugger commands
```

### Interactive Commands

Once the debugger starts, you can use these commands:

#### Execution Control

| Command | Shortcut | Description |
|---------|----------|-------------|
| `run` | `r` | Start/restart program execution |
| `continue` | `c` | Continue execution until next breakpoint |
| `step` | `s`, `next`, `n` | Step to next statement (step over) |
| `stepin` | `si` | Step into function/sub call |
| `stepout` | `so`, `finish` | Step out of current function/sub |

#### Breakpoints

| Command | Shortcut | Description |
|---------|----------|-------------|
| `break [loc]` | `b` | Set breakpoint or list all breakpoints |
| | | Location: line number, `file:line`, or function name |
| `delete [id]` | `d`, `clear` | Delete breakpoint (all if no ID given) |
| `enable <id>` | | Enable a breakpoint |
| `disable <id>` | | Disable a breakpoint |

#### Information

| Command | Shortcut | Description |
|---------|----------|-------------|
| `list [line]` | `l` | Show source code around line |
| `info breakpoints` | | List all breakpoints |
| `info sources` | | List loaded source files |

#### Other

| Command | Shortcut | Description |
|---------|----------|-------------|
| `help` | `h`, `?` | Show help |
| `quit` | `q`, `exit` | Exit the debugger |

### Example Session

```bash
$ qb64fresh-debug example.bas
QB64Fresh Debugger v0.1.0
Loaded: example.bas
Type 'help' for available commands.

(debug) break 10
Breakpoint 1 set.

(debug) run
Starting program...
State: Running
Stopped at breakpoint 1: example.bas:10

(paused) list
     5: DIM x AS INTEGER
     6: DIM y AS INTEGER
     7: 
     8: x = 5
     9: y = 10
>   10: PRINT x + y
    11: END

(paused) continue
Continuing...
Program completed normally.

(done) quit
Goodbye!
```

## Debug Adapter Protocol (DAP) Integration

The debugger implements the Debug Adapter Protocol, allowing integration with VS Code, Cursor, and other DAP-compatible IDEs.

### Starting DAP Server

```bash
qb64fresh-debug --dap
```

The server communicates via stdin/stdout (standard DAP mode). The IDE will launch the debugger automatically when you start a debug session.

### VS Code Configuration

Create `.vscode/launch.json`:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "debugadapter",
            "request": "launch",
            "name": "Debug QB64Fresh",
            "program": "${file}",
            "debugServer": 4711,
            "stopOnEntry": false,
            "console": "integratedTerminal"
        }
    ]
}
```

### DAP Features Supported

- ✅ Initialize and launch
- ✅ Set breakpoints (line, function)
- ✅ Continue, step over, step into, step out
- ✅ Stack trace
- ✅ Variable inspection
- ✅ Scope inspection
- ✅ Threads (single-threaded BASIC)
- ⏳ Expression evaluation (stub)
- ⏳ Conditional breakpoints (infrastructure ready)

## Breakpoints

### Line Breakpoints

Set a breakpoint at a specific line:

```bash
(debug) break 10
Breakpoint 1 set.

(debug) break example.bas:20
Breakpoint 2 set.
```

### Function Breakpoints

Break when entering a SUB or FUNCTION:

```bash
(debug) break MySub
Breakpoint 3 set.

(debug) break CalculateTotal
Breakpoint 4 set.
```

### Label Breakpoints

Break at a specific label (for GOTO/GOSUB):

```bash
(debug) break error_handler
Breakpoint 5 set.
```

### Conditional Breakpoints

Break when a condition is true:

```bash
(debug) break 15 when x > 100
Breakpoint 6 set.
```

### Managing Breakpoints

```bash
# List all breakpoints
(debug) break
Breakpoints:
  [1] example.bas:10 (enabled)
  [2] example.bas:20 (enabled)
  [3] function MySub (enabled)

# Disable a breakpoint
(debug) disable 2
Breakpoint 2 disabled.

# Enable a breakpoint
(debug) enable 2
Breakpoint 2 enabled.

# Delete a breakpoint
(debug) delete 2
Breakpoint 2 removed.

# Clear all breakpoints
(debug) delete
All breakpoints cleared.
```

## Stepping and Execution Control

### Step Over

Execute the current statement and stop at the next statement in the same scope:

```bash
(paused) step
At example.bas:11:1
```

Useful for stepping through code without entering procedure calls.

### Step Into

Execute the current statement, entering any procedure calls:

```bash
(paused) stepin
Entered MySub at example.bas:25:1
```

Useful for debugging inside procedures.

### Step Out

Continue execution until the current procedure returns:

```bash
(paused) stepout
Returned to example.bas:15:1
```

Useful for quickly exiting a procedure you've finished debugging.

### Continue

Resume execution until the next breakpoint or program end:

```bash
(paused) continue
Continuing...
Stopped at breakpoint 1: example.bas:10
```

## Variable Inspection

### Viewing Variables

The debugger automatically shows local and global variables when paused. In DAP mode, variables appear in the IDE's Variables panel.

### Variable Types

The debugger supports all BASIC types:

- **Primitive types**: INTEGER, LONG, SINGLE, DOUBLE, STRING, etc.
- **Arrays**: Multi-dimensional arrays with bounds
- **User-defined types**: UDTs with member access
- **Pointers**: Memory addresses and offsets

### Variable Display Formats

Variables can be displayed in different formats:

- **Decimal** (default): `42`, `-10`
- **Hexadecimal**: `0x2A`, `-0xA`
- **Binary**: `0b101010`, `-0b1010`
- **Octal**: `0o52`, `-0o12`

### Array Inspection

Arrays are displayed with their dimensions and element count:

```
arr: INTEGER(0 TO 9, 1 TO 5) [50 elements]
```

Access individual elements:

```
arr(0, 1): INTEGER = 42
arr(5, 3): INTEGER = 100
```

### UDT Inspection

User-defined types show their members:

```
player: Person {3 members}
  player.name: STRING = "Alice"
  player.age: INTEGER = 30
  player.score: DOUBLE = 1250.5
```

## Watch Expressions

Watch expressions allow you to monitor variable values and expressions during debugging.

### Adding Watches

```bash
# Watch a simple variable
(debug) watch x

# Watch an array element
(debug) watch arr(1, 2)

# Watch a UDT member
(debug) watch player.x

# Watch a complex expression (future)
(debug) watch x + y * 2
```

### Watch Expression Syntax

Supported expressions:

- **Simple variables**: `x`, `counter%`, `name$`
- **Array access**: `arr(1)`, `matrix(i, j)`
- **UDT member access**: `player.x`, `enemies(i).health`
- **Nested access**: `arr(1).member.field`

### Managing Watches

```bash
# List all watches
(debug) watch
Watches:
  [1] x = 42
  [2] arr(1, 2) = 100
  [3] player.x = 5.0

# Remove a watch
(debug) unwatch 2
Watch 2 removed.

# Clear all watches
(debug) unwatch
All watches cleared.
```

## Call Stack Navigation

The call stack shows the sequence of procedure calls that led to the current location.

### Viewing the Stack

```bash
(paused) stack
#0 MySub at example.bas:25:1
#1 ProcessData at example.bas:15:1
#2 <main> at example.bas:10:1
```

Frame #0 is the current (innermost) frame. Higher numbers are callers.

### Frame Selection

Select a frame to inspect variables in that scope:

```bash
# Select frame 1 (caller)
(paused) frame 1
Selected frame: ProcessData at example.bas:15:1

# View variables in this frame
(paused) locals
  data: INTEGER = 42
  result: DOUBLE = 3.14

# Return to current frame
(paused) frame 0
```

### Frame Information

Each frame shows:

- **Procedure name**: SUB/FUNCTION name, or `<main>` for module level
- **Source location**: File and line number
- **Local variables**: Variables in this frame's scope
- **Arguments**: Parameters passed to this procedure
- **Return location**: Where execution will return (for procedures)

## Source File Management

The debugger handles multi-file programs with `$INCLUDE` directives.

### Loading Source Files

Source files are automatically loaded when you start debugging:

```bash
qb64fresh-debug main.bas
```

The debugger will:
1. Load `main.bas`
2. Parse `$INCLUDE` directives
3. Load included files
4. Build a complete source map

### Source Search Paths

Add search paths for finding included files:

```bash
qb64fresh-debug -I ./lib -I ./include main.bas
```

Or in configuration:

```toml
source_paths = ["./lib", "./include"]
```

### Viewing Source

```bash
# Show source around current location
(paused) list

# Show source around specific line
(paused) list 20

# Show source from multiple files
(paused) info sources
Loaded sources:
  main.bas (50 lines)
  lib/utils.bas (30 lines)
```

## Configuration

### Configuration File

Create `.qb64fresh-debug.toml` or `qb64fresh-debug.toml`:

```toml
# Output verbosity: quiet, normal, verbose, trace
verbosity = "normal"

# Source file search paths
source_paths = ["./lib", "./include"]

# Break on program entry
break_on_entry = false

# Break on unhandled errors
break_on_error = true

# Default timeout for debug operations (milliseconds)
timeout_ms = 30000

# Initial breakpoints
[[breakpoints]]
type = "line"
file = "main.bas"
line = 10
enabled = true

[[breakpoints]]
type = "function"
name = "MySub"
enabled = true

# Environment variables for debug target
[environment]
DEBUG = "1"
LOG_LEVEL = "verbose"

# Working directory for debug target
working_directory = "./build"

# Arguments to pass to debug target
target_args = ["--input", "data.txt"]
```

### Configuration Discovery

The debugger automatically searches for configuration files in:

1. Current directory: `.qb64fresh-debug.toml`, `qb64fresh-debug.toml`
2. Parent directories (walking up the tree)

The first file found is used.

## Runtime Integration

**Note:** Runtime integration is not yet implemented. The following describes the planned architecture.

### Debug Hooks in Generated Code

The compiler will emit debug hooks in the generated C code:

```c
// At each statement
if (qb64fresh_debug_check_breakpoint(__LINE__, __FILE__)) {
    qb64fresh_debug_stopped(BREAKPOINT, __LINE__, __FILE__);
    qb64fresh_debug_wait_for_command();
}

// At procedure entry
void MySub(int x) {
    qb64fresh_debug_enter("MySub", __LINE__, __FILE__);
    // ... procedure body ...
    qb64fresh_debug_exit("MySub");
}
```

### Named Pipe Communication

The debugger and debugee communicate via a named pipe:

- **Unix**: `/tmp/qb64fresh_debug_<pid>`
- **Windows**: `\\.\pipe\qb64fresh_debug_<pid>`

The pipe path is passed via the `QB64FRESH_DEBUG_PIPE` environment variable.

### Protocol Messages

**Commands (Debugger → Debugee):**
- `CONTINUE` - Resume execution
- `STEP_INTO` - Step into next statement
- `STEP_OVER` - Step over next statement
- `STEP_OUT` - Step out of current procedure
- `PAUSE` - Pause execution
- `TERMINATE` - Stop program
- `BP_ADD <line>` - Add breakpoint
- `BP_REMOVE <line>` - Remove breakpoint
- `GET_VAR <name> <frame>` - Get variable value
- `GET_LOCATION` - Get current location

**Events (Debugee → Debugger):**
- `READY` - Program started
- `STOPPED <reason> <line> <file>` - Execution paused
- `TERMINATED` - Program ended
- `ENTER <proc> <line>` - Entered procedure
- `EXIT <proc>` - Exited procedure
- `VAR <name> <type> <value>` - Variable value
- `LOCATION <line> <file> <proc>` - Current location
- `OUTPUT <text>` - Program output
- `ERROR <message>` - Error message

### Debug Info Emission

The compiler will emit debug information including:

- **Line mappings**: Map C line numbers to BASIC source lines
- **Variable locations**: Memory addresses or register locations
- **Scope information**: Which variables are in which scopes
- **Type information**: Variable types for display

This information will be embedded in the generated C code or stored in a separate debug info file.

## Examples

### Example 1: Simple Debugging Session

```basic
' example.bas
DIM x AS INTEGER
DIM y AS INTEGER

x = 5
y = 10
PRINT x + y
END
```

```bash
$ qb64fresh-debug example.bas
(debug) break 5
Breakpoint 1 set.
(debug) run
Stopped at breakpoint 1: example.bas:5
(paused) list
     1: ' example.bas
     2: DIM x AS INTEGER
     3: DIM y AS INTEGER
     4: 
>    5: x = 5
     6: y = 10
     7: PRINT x + y
     8: END
(paused) step
At example.bas:6:1
(paused) step
At example.bas:7:1
(paused) continue
15
Program completed normally.
(done) quit
```

### Example 2: Procedure Debugging

```basic
' example.bas
SUB ProcessData (value AS INTEGER)
    DIM result AS DOUBLE
    result = value * 3.14
    PRINT result
END SUB

DIM x AS INTEGER
x = 10
ProcessData x
END
```

```bash
$ qb64fresh-debug example.bas
(debug) break ProcessData
Breakpoint 1 set.
(debug) run
Stopped at breakpoint 1: example.bas:2:1
(paused) stack
#0 ProcessData at example.bas:2:1
#1 <main> at example.bas:8:1
(paused) locals
  value: INTEGER = 10
  result: DOUBLE = 0.0
(paused) step
At example.bas:4:1
(paused) locals
  value: INTEGER = 10
  result: DOUBLE = 31.4
(paused) continue
31.4
Program completed normally.
(done) quit
```

### Example 3: Array and UDT Debugging

```basic
' example.bas
TYPE Point
    x AS DOUBLE
    y AS DOUBLE
END TYPE

DIM points(10) AS Point
DIM i AS INTEGER

FOR i = 0 TO 10
    points(i).x = i * 1.5
    points(i).y = i * 2.0
NEXT i

PRINT points(5).x
END
```

```bash
$ qb64fresh-debug example.bas
(debug) break 10
Breakpoint 1 set.
(debug) run
Stopped at breakpoint 1: example.bas:10:1
(paused) watch points(5)
Watch 1: points(5) = Point {2 members}
  points(5).x: DOUBLE = 7.5
  points(5).y: DOUBLE = 10.0
(paused) continue
7.5
Program completed normally.
(done) quit
```

## Troubleshooting

### Debugger Won't Start

**Problem:** `qb64fresh-debug` command not found

**Solution:** Build the debugger:
```bash
cd tools/debug
cargo build --release
```

### Source File Not Found

**Problem:** Error loading source file or included files

**Solution:** 
- Check file paths are correct
- Add source search paths: `-I ./lib -I ./include`
- Verify `$INCLUDE` paths are relative to the main file or search paths

### Breakpoints Not Working

**Problem:** Breakpoints set but program doesn't stop

**Solution:**
- Verify the line has executable code (not a comment or blank line)
- Check breakpoint is enabled: `info breakpoints`
- Ensure runtime integration is complete (currently in development)

### Variables Not Showing

**Problem:** Variables panel is empty or shows "unavailable"

**Solution:**
- Verify program is paused (not running)
- Check variable is in scope (local vs global)
- Ensure runtime integration is complete (currently in development)

### DAP Connection Issues

**Problem:** IDE can't connect to debugger

**Solution:**
- Verify debugger is started: `qb64fresh-debug --dap`
- Check launch.json configuration
- Ensure debug adapter type is correct
- Check for port conflicts (if using TCP mode)

## Future Enhancements

Planned features:

- ✅ **Infrastructure**: Complete (symbols, values, frames, watch parsing)
- ⏳ **Runtime Integration**: Debug hooks in generated code
- ⏳ **Expression Evaluation**: Evaluate complex expressions in watch
- ⏳ **Conditional Breakpoints**: Full support for breakpoint conditions
- ⏳ **Hit Counts**: Break after N hits
- ⏳ **Logpoints**: Log messages without stopping
- ⏳ **Reverse Debugging**: Step backwards through execution
- ⏳ **Memory View**: Inspect raw memory contents
- ⏳ **Disassembly View**: View generated C code

## API Reference

### Library Usage

The debugger can be used as a library:

```rust
use qb64fresh_debug::{Debugger, DebugConfig};

let config = DebugConfig::interactive();
let mut debugger = Debugger::new(config);

// Load source
debugger.load_source("myprogram.bas")?;

// Set breakpoint
debugger.add_line_breakpoint("myprogram.bas", 10)?;

// Start debugging
debugger.run()?;
```

### Key Types

- `Debugger`: Main debugger interface
- `DebugConfig`: Configuration options
- `Breakpoint`: Breakpoint definition
- `SourceLocation`: Source file position
- `ExecutionState`: Current execution state
- `DebugSymbols`: Extracted debug symbols
- `CallStack`: Call stack management
- `WatchManager`: Watch expression management

See the [API documentation](../../target/doc/qb64fresh_debug/) for complete details.

## Contributing

The debugger is part of the QB64Fresh project. See the main [README](../../README.md) for contribution guidelines.

When adding debugger features:

1. Update this documentation
2. Add tests in `tools/debug/src/*/tests.rs`
3. Update the DAP implementation if adding IDE features
4. Document any new protocol messages

## See Also

- [QB64Fresh Architecture](ARCHITECTURE.md)
- [Getting Started Guide](GETTING_STARTED.md)
- [Development Guide](DEVELOPMENT.md)
- [Debug Adapter Protocol Specification](https://microsoft.github.io/debug-adapter-protocol/)
