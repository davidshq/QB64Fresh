# Debugging Functionality in QB64pe

This document provides a comprehensive overview of the debugging capabilities available in QB64pe, including the vwatch debugger system and IDE integration.

## Overview

QB64pe includes a full-featured debugging system built around **vwatch**, which provides:
- Breakpoints and watchpoints
- Step-by-step execution
- Variable inspection and modification
- Call stack tracking
- IDE integration with visual panels

---

## Enabling Debugging

### The $DEBUG Metacommand

To enable debugging in your program, add the `$DEBUG` metacommand at the top of your source file:

```qb64
$DEBUG
' Your program code here
PRINT "Hello, Debug World!"
```

When `$DEBUG` is present, the compiler automatically includes:
- `vwatch.bi` - Variable declarations and type definitions
- `vwatch.bm` - The main debugger implementation

This sets the `_DEBUG_` precompiler flag, allowing conditional compilation:

```qb64
$IF _DEBUG_ THEN
    PRINT "Debug mode is active"
$END IF
```

---

## IDE Debugger Interface

### Starting a Debug Session

1. Add `$DEBUG` to your program (or enable "Auto-add $DEBUG" in settings)
2. Press **F7** or **F8** to start the program paused
3. Press **F5** to run normally with debugging enabled

### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| **F4** | Add variable to watch list |
| **F5** | Run / Continue execution |
| **F6** | Step out of current function |
| **F7** | Step into function/subroutine |
| **F8** | Step over (execute next line) |
| **F9** | Toggle breakpoint at cursor |
| **F10** | Clear all breakpoints |
| **Ctrl+F10** | Unskip all lines |
| **F12** | Show call stack |

### Debug Menu Options

The IDE provides a Debug menu with the following options:
- **Start Paused (F7 or F8)** - Begin debugging in paused state
- **Step Into (F7)** - Step into the next function/subroutine
- **Step Over (F8)** - Execute the current line and pause at the next
- **Toggle Breakpoint (F9)** - Set or clear a breakpoint at the cursor
- **Clear All Breakpoints (F10)** - Remove all breakpoints
- **Unskip All Lines (Ctrl+F10)** - Clear all skip-line markers
- **Call Stack (F12)** - Display the current call stack

---

## Breakpoints

### Setting Breakpoints

**In the IDE:**
- Position your cursor on a line and press **F9**
- Or use Debug menu > Toggle Breakpoint

**Programmatically:**
- Use the `STOP` statement to create a hardcoded breakpoint

### Breakpoint Behavior

- Breakpoints are stored per line number
- Only valid source lines can have breakpoints
- Execution pauses *before* the breakpointed line executes
- Breakpoints persist during the debug session

### Clearing Breakpoints

- **F9** on a breakpointed line removes it
- **F10** clears all breakpoints at once

---

## Watchpoints (Conditional Breakpoints)

Watchpoints monitor variable values and break execution when a condition is met.

### Supported Operators

| Operator | Description |
|----------|-------------|
| `=` | Equal to |
| `<>` | Not equal to |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal to |
| `>=` | Greater than or equal to |

### Example Uses

- Break when `score > 100`
- Break when `playerName$ = "Admin"`
- Break when `counter >= maxValue`

---

## Variable Watch Panel (vWatch Panel)

The vWatch panel displays monitored variables and their current values during debugging.

### Features

- **Floating panel** - Resizable and draggable
- **Scrollable** - Handles many watched variables
- **Real-time updates** - Values update as you step through code
- **Persistent layout** - Position and size saved to `debug.ini`

### Adding Variables to Watch

1. Press **F4** during debugging
2. Enter the variable name, which can be:
   - Simple variables: `myVar`, `count%`, `name$`
   - Array elements: `scores(5)`, `matrix(row, col)`
   - UDT members: `player.health`, `enemy.position.x`

### Configuration Options

In IDE settings:
- **WatchListToConsole** - Output watch values to console instead of panel
- **AutoAddDebugCommand** - Automatically add `$DEBUG` metacommand

---

## Step Execution

### Step Into (F7)

Executes the current line. If the line contains a `CALL`, `SUB`, or `FUNCTION` call, the debugger enters that routine and pauses at its first line.

### Step Over (F8)

Executes the current line completely. If the line calls a subroutine or function, it executes the entire routine and pauses at the next line in the current scope.

### Step Out (F6)

Continues execution until the current subroutine or function returns, then pauses at the calling line.

### Continue (F5)

Resumes normal execution until:
- A breakpoint is hit
- A watchpoint condition is met
- A `STOP` statement is encountered
- The program ends

---

## Call Stack

Press **F12** to view the call stack, which shows:
- The chain of subroutine/function calls leading to the current location
- The line number where each call was made
- The procedure names in display format

### Stack Information

Each stack frame includes:
- Internal procedure name
- Display name
- Entry line number

---

## Variable Inspection

### Supported Variable Types

The debugger can inspect:
- **Simple variables** - Numeric and string types
- **Arrays** - With bounds checking
- **User-defined types (UDTs)** - Including nested members
- **Bit fields** - Special bit access functions

### Reading Variables

Variables are read using memory operations (`_MEMGET`) with:
- Global vs. local scope handling
- Array index calculation
- UDT member offset calculation
- String pointer dereferencing

### Modifying Variables

During debugging, you can modify variable values to test different scenarios.

---

## Technical Architecture

### vwatch Communication Protocol

The debugger uses TCP/IP communication between the IDE and the debugged program:

1. **Protocol**: Uses `_OPENCLIENT` with "QB64IDE:" protocol
2. **Port**: Retrieved from `QB64DEBUGPORT` environment variable
3. **Message Format**: Binary protocol with 4-byte length prefix

### Protocol Commands

| Command | Description |
|---------|-------------|
| `breakpoint:` | Breakpoint operations |
| `line number:` | Current line reporting |
| `get global var` | Read global variable value |
| `set global var` | Modify global variable value |
| `watchpoint:` | Watchpoint operations |
| `set breakpoint:` | Enable a breakpoint |
| `clear breakpoint:` | Disable a breakpoint |

### Special Line Numbers

| Value | Meaning |
|-------|---------|
| `0` | Program end |
| `-1` | Runtime error |
| `-2` | SUB/FUNCTION entry |
| `-3` | STOP statement |
| `-4`, `-5` | INPUT statement handling |

### Core Files

| File | Purpose |
|------|---------|
| `internal/support/vwatch/vwatch.bi` | Type definitions and declarations |
| `internal/support/vwatch/vwatch.bm` | Main debugger implementation (~883 lines) |
| `internal/support/vwatch/vwatch_stub.bm` | Stub for when debugging disabled |
| `source/ide/ide_methods.bas` | IDE debug interface (~1500+ lines) |

---

## Configuration Files

### settings/config.ini

Contains IDE settings including:
- Debug panel preferences
- Auto-add $DEBUG setting
- Watch list output mode

### settings/debug.ini

Stores per-session debug data:
- vWatch panel position and size
- Breakpoint state (optional)

---

## Best Practices

### Performance Considerations

- **Debugging adds overhead** - Only use `$DEBUG` during development
- **vwatch() is called on every line** - This enables stepping but affects speed
- **Stub version** - When `$DEBUG` is not present, a zero-overhead stub is used

### Effective Debugging

1. **Start with breakpoints** - Set breakpoints at key locations before running
2. **Use step over for loops** - Step into only when you need to inspect a routine
3. **Watch key variables** - Add critical variables to the watch list early
4. **Check the call stack** - Use F12 to understand the execution path

### Conditional Debugging

Use precompiler directives for debug-only code:

```qb64
$IF _DEBUG_ THEN
    PRINT "Current value:"; myVariable
$END IF
```

---

## Troubleshooting

### Debugger Not Working

1. Ensure `$DEBUG` is at the top of your source file
2. Check that the program was recompiled after adding `$DEBUG`
3. Verify the IDE debug port is not blocked

### Variables Not Showing

1. Confirm the variable is in scope at the current line
2. Check spelling and case of variable name
3. For arrays, ensure indices are within bounds

### Breakpoints Not Hitting

1. Verify the breakpoint is on an executable line (not a comment or declaration)
2. Ensure the code path actually reaches that line
3. Check that the program is running in debug mode (started with F7/F8)

---

## Future: QB64Fresh Debugger

QB64Fresh includes a modern Rust-based debugger with:
- Debug Adapter Protocol (DAP) support for IDE integration
- Interactive command-line interface
- Enhanced symbol extraction from AST
- Modern source mapping

### QB64Fresh Debugger Commands

| Command | Description |
|---------|-------------|
| `run` / `r` | Start/restart execution |
| `continue` / `c` | Resume from breakpoint |
| `step` / `s` / `next` / `n` | Step over |
| `stepin` / `si` | Step into |
| `stepout` / `so` | Step out |
| `break` / `b [location]` | Set breakpoint |
| `delete` / `d [id]` | Remove breakpoint |
| `list` / `l [line]` | View source |
| `info breakpoints` | List breakpoints |
| `quit` / `exit` | Exit debugger |

---

## See Also

- [Auto-Including Documentation](auto-including.md) - How vwatch files are auto-included
- [IDE Methods Source](../source/ide/ide_methods.bas) - Full IDE implementation
- [vwatch Source](../internal/support/vwatch/) - Debugger implementation files
