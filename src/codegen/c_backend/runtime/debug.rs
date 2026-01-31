//! Debug runtime support for QB64Fresh.
//!
//! This module provides the inline C code for debugging support. When a program
//! is compiled with `--debug`, these functions are included to enable:
//!
//! - **Line tracking**: `qb_dbg_line()` called before each statement
//! - **Breakpoint checking**: Pause execution on breakpoints
//! - **Step mode**: Step into/over/out execution control
//! - **IPC communication**: Named pipe for debugger↔debugee communication
//! - **Variable inspection**: Helpers for reading variable values
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────┐     ┌──────────────────┐     ┌─────────────────────┐
//! │ VS Code /   │ DAP │ qb64fresh-debug  │pipe │ Compiled program    │
//! │ Cursor      │◄───►│ (DAP server)     │◄───►│ + debug hooks       │
//! └─────────────┘     └──────────────────┘     └─────────────────────┘
//! ```
//!
//! The debugee checks for commands from the debugger at each `qb_dbg_line()` call
//! and can be paused, stepped, or have breakpoints modified at runtime.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits the debug runtime declarations and functions.
///
/// This includes:
/// - Debug state variables (paused, stepping, current location)
/// - Breakpoint table
/// - IPC initialization and communication
/// - Line hook function
/// - Procedure entry/exit hooks
pub fn emit_debug_runtime(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Debug Runtime Support */")?;
    writeln_code!(output)?;

    // Platform-specific includes for debugging IPC
    emit_debug_includes(output)?;

    // Debug state variables
    emit_debug_state(output)?;

    // Breakpoint management
    emit_breakpoint_functions(output)?;

    // IPC communication
    emit_ipc_functions(output)?;

    // Main debug hooks
    emit_debug_hooks(output)?;

    // Variable inspection helpers
    emit_inspection_helpers(output)?;
    Ok(())
}

/// Emits platform-specific includes needed for debug IPC.
fn emit_debug_includes(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(
        output,
        "/* Windows named pipe support already from windows.h */"
    )?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "#include <unistd.h>")?;
    writeln_code!(output, "#include <fcntl.h>")?;
    writeln_code!(output, "#include <sys/stat.h>")?;
    writeln_code!(output, "#include <errno.h>")?;
    writeln_code!(output, "#include <poll.h>")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits debug state variables.
fn emit_debug_state(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Debug state */")?;
    writeln_code!(
        output,
        "static volatile int _qb_dbg_enabled = 0;     /* Debug mode active */"
    )?;
    writeln_code!(
        output,
        "static volatile int _qb_dbg_paused = 0;      /* Execution paused */"
    )?;
    writeln_code!(
        output,
        "static volatile int _qb_dbg_stepping = 0;    /* Step mode: 0=run, 1=into, 2=over, 3=out */"
    )?;
    writeln_code!(
        output,
        "static volatile int _qb_dbg_step_depth = 0;  /* Call depth for step over/out */"
    )?;
    writeln_code!(
        output,
        "static volatile int _qb_dbg_current_line = 0;/* Current source line */"
    )?;
    writeln_code!(
        output,
        "static const char* _qb_dbg_current_file = NULL; /* Current source file */"
    )?;
    writeln_code!(
        output,
        "static const char* _qb_dbg_current_proc = NULL; /* Current procedure name */"
    )?;
    writeln_code!(output)?;

    // Breakpoint table
    writeln_code!(output, "/* Breakpoint table */")?;
    writeln_code!(output, "#define QBD_MAX_BREAKPOINTS 256")?;
    writeln_code!(
        output,
        "static int _qb_dbg_bp_lines[QBD_MAX_BREAKPOINTS];  /* Line numbers */"
    )?;
    writeln_code!(
        output,
        "static int _qb_dbg_bp_enabled[QBD_MAX_BREAKPOINTS]; /* Enabled flags */"
    )?;
    writeln_code!(
        output,
        "static int _qb_dbg_bp_count = 0;                    /* Active breakpoint count */"
    )?;
    writeln_code!(output)?;

    // QB64pe-style evnt: statement boundary hook for IDE/debugger (optional break/step)
    // Types match runtime/include/qb64fresh_rt.h (uint32_t) for ABI consistency.
    writeln_code!(
        output,
        "/* evnt: 0 = no debug; non-zero = call qb_evnt at statement boundaries */"
    )?;
    writeln_code!(output, "static uint32_t qbevent = 0;")?;
    writeln_code!(
        output,
        "static void qb_evnt(uint32_t line, uint32_t incline, const char* incfile) {{"
    )?;
    writeln_code!(
        output,
        "    (void)line; (void)incline; (void)incfile; /* no-op; hook for IDE */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // IPC handle
    writeln_code!(output, "/* Debug IPC */")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "static HANDLE _qb_dbg_pipe = INVALID_HANDLE_VALUE;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "static int _qb_dbg_pipe_fd = -1;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(
        output,
        "static char _qb_dbg_cmd_buf[1024]; /* Command buffer */"
    )?;
    writeln_code!(
        output,
        "static int _qb_dbg_cmd_len = 0;    /* Bytes in buffer */"
    )?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits breakpoint management functions.
fn emit_breakpoint_functions(output: &mut String) -> Result<(), CodeGenError> {
    // Add breakpoint
    writeln_code!(output, "/* Add a breakpoint at the given line */")?;
    writeln_code!(output, "static void qb_dbg_add_bp(int line) {{")?;
    writeln_code!(
        output,
        "    if (_qb_dbg_bp_count >= QBD_MAX_BREAKPOINTS) return;"
    )?;
    writeln_code!(output, "    _qb_dbg_bp_lines[_qb_dbg_bp_count] = line;")?;
    writeln_code!(output, "    _qb_dbg_bp_enabled[_qb_dbg_bp_count] = 1;")?;
    writeln_code!(output, "    _qb_dbg_bp_count++;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Remove breakpoint
    writeln_code!(output, "/* Remove a breakpoint at the given line */")?;
    writeln_code!(output, "static void qb_dbg_remove_bp(int line) {{")?;
    writeln_code!(output, "    for (int i = 0; i < _qb_dbg_bp_count; i++) {{")?;
    writeln_code!(output, "        if (_qb_dbg_bp_lines[i] == line) {{")?;
    writeln_code!(output, "            /* Shift remaining breakpoints down */")?;
    writeln_code!(
        output,
        "            for (int j = i; j < _qb_dbg_bp_count - 1; j++) {{"
    )?;
    writeln_code!(
        output,
        "                _qb_dbg_bp_lines[j] = _qb_dbg_bp_lines[j+1];"
    )?;
    writeln_code!(
        output,
        "                _qb_dbg_bp_enabled[j] = _qb_dbg_bp_enabled[j+1];"
    )?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "            _qb_dbg_bp_count--;")?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Check if line is a breakpoint
    writeln_code!(output, "/* Check if line has an enabled breakpoint */")?;
    writeln_code!(output, "static int qb_dbg_is_bp(int line) {{")?;
    writeln_code!(output, "    for (int i = 0; i < _qb_dbg_bp_count; i++) {{")?;
    writeln_code!(
        output,
        "        if (_qb_dbg_bp_lines[i] == line && _qb_dbg_bp_enabled[i]) {{"
    )?;
    writeln_code!(output, "            return 1;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits IPC communication functions.
fn emit_ipc_functions(output: &mut String) -> Result<(), CodeGenError> {
    // Initialize debug subsystem
    writeln_code!(output, "/* Initialize debug subsystem */")?;
    writeln_code!(output, "static void qb_dbg_init(const char* pipe_path) {{")?;
    writeln_code!(output, "    _qb_dbg_enabled = 1;")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    _qb_dbg_pipe = CreateFileA(")?;
    writeln_code!(output, "        pipe_path,")?;
    writeln_code!(output, "        GENERIC_READ | GENERIC_WRITE,")?;
    writeln_code!(output, "        0, NULL, OPEN_EXISTING, 0, NULL);")?;
    writeln_code!(output, "    if (_qb_dbg_pipe != INVALID_HANDLE_VALUE) {{")?;
    writeln_code!(output, "        DWORD written;")?;
    writeln_code!(
        output,
        "        WriteFile(_qb_dbg_pipe, \"READY\\n\", 6, &written, NULL);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    _qb_dbg_pipe_fd = open(pipe_path, O_RDWR);")?;
    writeln_code!(output, "    if (_qb_dbg_pipe_fd >= 0) {{")?;
    writeln_code!(output, "        /* Set non-blocking mode for polling */")?;
    writeln_code!(
        output,
        "        int flags = fcntl(_qb_dbg_pipe_fd, F_GETFL, 0);"
    )?;
    writeln_code!(
        output,
        "        fcntl(_qb_dbg_pipe_fd, F_SETFL, flags | O_NONBLOCK);"
    )?;
    writeln_code!(output, "        write(_qb_dbg_pipe_fd, \"READY\\n\", 6);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Close debug subsystem
    writeln_code!(output, "/* Shutdown debug subsystem */")?;
    writeln_code!(output, "static void qb_dbg_shutdown(void) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    if (_qb_dbg_pipe != INVALID_HANDLE_VALUE) {{")?;
    writeln_code!(output, "        DWORD written;")?;
    writeln_code!(
        output,
        "        WriteFile(_qb_dbg_pipe, \"TERMINATED\\n\", 11, &written, NULL);"
    )?;
    writeln_code!(output, "        CloseHandle(_qb_dbg_pipe);")?;
    writeln_code!(output, "        _qb_dbg_pipe = INVALID_HANDLE_VALUE;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    if (_qb_dbg_pipe_fd >= 0) {{")?;
    writeln_code!(
        output,
        "        write(_qb_dbg_pipe_fd, \"TERMINATED\\n\", 11);"
    )?;
    writeln_code!(output, "        close(_qb_dbg_pipe_fd);")?;
    writeln_code!(output, "        _qb_dbg_pipe_fd = -1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    _qb_dbg_enabled = 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Send event to debugger
    writeln_code!(output, "/* Send an event to the debugger */")?;
    writeln_code!(output, "static void qb_dbg_send(const char* msg) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    if (_qb_dbg_pipe != INVALID_HANDLE_VALUE) {{")?;
    writeln_code!(output, "        DWORD written;")?;
    writeln_code!(
        output,
        "        WriteFile(_qb_dbg_pipe, msg, (DWORD)strlen(msg), &written, NULL);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    if (_qb_dbg_pipe_fd >= 0) {{")?;
    writeln_code!(output, "        write(_qb_dbg_pipe_fd, msg, strlen(msg));")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Process incoming commands
    emit_process_commands(output)?;
    Ok(())
}

/// Emits the command processing function.
fn emit_process_commands(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(
        output,
        "/* Process commands from debugger (non-blocking) */"
    )?;
    writeln_code!(output, "static void qb_dbg_process_commands(void) {{")?;
    writeln_code!(output, "    char buf[256];")?;
    writeln_code!(output, "    ssize_t n;")?;
    writeln_code!(output)?;

    // Read from pipe
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(
        output,
        "    if (_qb_dbg_pipe == INVALID_HANDLE_VALUE) return;"
    )?;
    writeln_code!(output, "    DWORD available = 0;")?;
    writeln_code!(
        output,
        "    if (!PeekNamedPipe(_qb_dbg_pipe, NULL, 0, NULL, &available, NULL) || available == 0) return;"
    )?;
    writeln_code!(output, "    DWORD read_bytes;")?;
    writeln_code!(
        output,
        "    if (!ReadFile(_qb_dbg_pipe, buf, sizeof(buf)-1, &read_bytes, NULL)) return;"
    )?;
    writeln_code!(output, "    n = (ssize_t)read_bytes;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    if (_qb_dbg_pipe_fd < 0) return;")?;
    writeln_code!(output, "    n = read(_qb_dbg_pipe_fd, buf, sizeof(buf)-1);")?;
    writeln_code!(output, "    if (n <= 0) return;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;
    writeln_code!(output, "    buf[n] = '\\0';")?;
    writeln_code!(output)?;

    // Parse commands
    writeln_code!(output, "    /* Parse command */")?;
    writeln_code!(output, "    char* cmd = buf;")?;
    writeln_code!(output, "    while (*cmd == ' ' || *cmd == '\\n') cmd++;")?;
    writeln_code!(output)?;

    writeln_code!(output, "    if (strncmp(cmd, \"CONTINUE\", 8) == 0) {{")?;
    writeln_code!(output, "        _qb_dbg_paused = 0;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 0;")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"STEP_INTO\", 9) == 0) {{"
    )?;
    writeln_code!(output, "        _qb_dbg_paused = 0;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 1; /* Step into */")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"STEP_OVER\", 9) == 0) {{"
    )?;
    writeln_code!(output, "        _qb_dbg_paused = 0;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 2; /* Step over */")?;
    writeln_code!(output, "        _qb_dbg_step_depth = 0;")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"STEP_OUT\", 8) == 0) {{"
    )?;
    writeln_code!(output, "        _qb_dbg_paused = 0;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 3; /* Step out */")?;
    writeln_code!(output, "        _qb_dbg_step_depth = 1;")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"PAUSE\", 5) == 0) {{"
    )?;
    writeln_code!(output, "        _qb_dbg_paused = 1;")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"TERMINATE\", 9) == 0) {{"
    )?;
    writeln_code!(output, "        qb_dbg_shutdown();")?;
    writeln_code!(output, "        exit(0);")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"BP_ADD \", 7) == 0) {{"
    )?;
    writeln_code!(output, "        int line = atoi(cmd + 7);")?;
    writeln_code!(output, "        if (line > 0) qb_dbg_add_bp(line);")?;
    writeln_code!(
        output,
        "    }} else if (strncmp(cmd, \"BP_REMOVE \", 10) == 0) {{"
    )?;
    writeln_code!(output, "        int line = atoi(cmd + 10);")?;
    writeln_code!(output, "        if (line > 0) qb_dbg_remove_bp(line);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits the main debug hook functions.
fn emit_debug_hooks(output: &mut String) -> Result<(), CodeGenError> {
    // Line hook - called before each statement
    writeln_code!(output, "/* Called before each statement */")?;
    writeln_code!(
        output,
        "static void qb_dbg_line(int line, const char* file) {{"
    )?;
    writeln_code!(output, "    if (!_qb_dbg_enabled) return;")?;
    writeln_code!(output)?;
    writeln_code!(output, "    _qb_dbg_current_line = line;")?;
    writeln_code!(output, "    _qb_dbg_current_file = file;")?;
    writeln_code!(output)?;

    // Check breakpoints
    writeln_code!(output, "    /* Check breakpoints */")?;
    writeln_code!(output, "    if (qb_dbg_is_bp(line)) {{")?;
    writeln_code!(output, "        _qb_dbg_paused = 1;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 0;")?;
    writeln_code!(output, "        char msg[128];")?;
    writeln_code!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED breakpoint %d %s\\n\", line, file ? file : \"\");"
    )?;
    writeln_code!(output, "        qb_dbg_send(msg);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;

    // Check step mode
    writeln_code!(output, "    /* Check step mode */")?;
    writeln_code!(output, "    if (_qb_dbg_stepping == 1) {{ /* Step into */")?;
    writeln_code!(output, "        _qb_dbg_paused = 1;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 0;")?;
    writeln_code!(output, "        char msg[128];")?;
    writeln_code!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED step %d %s\\n\", line, file ? file : \"\");"
    )?;
    writeln_code!(output, "        qb_dbg_send(msg);")?;
    writeln_code!(
        output,
        "    }} else if (_qb_dbg_stepping == 2 && _qb_dbg_step_depth <= 0) {{ /* Step over */"
    )?;
    writeln_code!(output, "        _qb_dbg_paused = 1;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 0;")?;
    writeln_code!(output, "        char msg[128];")?;
    writeln_code!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED step %d %s\\n\", line, file ? file : \"\");"
    )?;
    writeln_code!(output, "        qb_dbg_send(msg);")?;
    writeln_code!(
        output,
        "    }} else if (_qb_dbg_stepping == 3 && _qb_dbg_step_depth <= 0) {{ /* Step out */"
    )?;
    writeln_code!(output, "        _qb_dbg_paused = 1;")?;
    writeln_code!(output, "        _qb_dbg_stepping = 0;")?;
    writeln_code!(output, "        char msg[128];")?;
    writeln_code!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED step %d %s\\n\", line, file ? file : \"\");"
    )?;
    writeln_code!(output, "        qb_dbg_send(msg);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;

    // Wait while paused
    writeln_code!(output, "    /* Wait while paused, processing commands */")?;
    writeln_code!(output, "    while (_qb_dbg_paused) {{")?;
    writeln_code!(output, "        qb_dbg_process_commands();")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "        Sleep(1); /* 1ms poll */")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "        usleep(1000); /* 1ms poll */")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Procedure entry hook
    writeln_code!(output, "/* Called on SUB/FUNCTION entry */")?;
    writeln_code!(
        output,
        "static void qb_dbg_enter_proc(const char* name, int line) {{"
    )?;
    writeln_code!(output, "    if (!_qb_dbg_enabled) return;")?;
    writeln_code!(output, "    _qb_dbg_current_proc = name;")?;
    writeln_code!(output, "    _qb_dbg_step_depth++;")?;
    writeln_code!(output, "    char msg[128];")?;
    writeln_code!(
        output,
        "    snprintf(msg, sizeof(msg), \"ENTER %s %d\\n\", name, line);"
    )?;
    writeln_code!(output, "    qb_dbg_send(msg);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Procedure exit hook
    writeln_code!(output, "/* Called on SUB/FUNCTION exit */")?;
    writeln_code!(output, "static void qb_dbg_exit_proc(const char* name) {{")?;
    writeln_code!(output, "    if (!_qb_dbg_enabled) return;")?;
    writeln_code!(output, "    _qb_dbg_step_depth--;")?;
    writeln_code!(output, "    char msg[128];")?;
    writeln_code!(
        output,
        "    snprintf(msg, sizeof(msg), \"EXIT %s\\n\", name);"
    )?;
    writeln_code!(output, "    qb_dbg_send(msg);")?;
    writeln_code!(output, "    _qb_dbg_current_proc = NULL;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits variable inspection helper functions.
fn emit_inspection_helpers(output: &mut String) -> Result<(), CodeGenError> {
    // Send variable value to debugger
    writeln_code!(output, "/* Send a variable value to the debugger */")?;
    writeln_code!(
        output,
        "static void qb_dbg_var_int(const char* name, int64_t value) {{"
    )?;
    writeln_code!(output, "    char msg[256];")?;
    writeln_code!(
        output,
        "    snprintf(msg, sizeof(msg), \"VAR %s int %lld\\n\", name, (long long)value);"
    )?;
    writeln_code!(output, "    qb_dbg_send(msg);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "static void qb_dbg_var_float(const char* name, double value) {{"
    )?;
    writeln_code!(output, "    char msg[256];")?;
    writeln_code!(
        output,
        "    snprintf(msg, sizeof(msg), \"VAR %s float %.17g\\n\", name, value);"
    )?;
    writeln_code!(output, "    qb_dbg_send(msg);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "static void qb_dbg_var_string(const char* name, qb_string* value) {{"
    )?;
    writeln_code!(output, "    char msg[1024];")?;
    writeln_code!(output, "    if (value && value->data) {{")?;
    writeln_code!(
        output,
        "        snprintf(msg, sizeof(msg), \"VAR %s string \\\"%.*s\\\"\\n\", name, (int)(value->length < 900 ? value->length : 900), value->data);"
    )?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(
        output,
        "        snprintf(msg, sizeof(msg), \"VAR %s string \\\"\\\"\\n\", name);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    qb_dbg_send(msg);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Send current location info
    writeln_code!(output, "/* Send current location to debugger */")?;
    writeln_code!(output, "static void qb_dbg_send_location(void) {{")?;
    writeln_code!(output, "    char msg[256];")?;
    writeln_code!(
        output,
        "    snprintf(msg, sizeof(msg), \"LOCATION %d %s %s\\n\","
    )?;
    writeln_code!(output, "        _qb_dbg_current_line,")?;
    writeln_code!(
        output,
        "        _qb_dbg_current_file ? _qb_dbg_current_file : \"\","
    )?;
    writeln_code!(
        output,
        "        _qb_dbg_current_proc ? _qb_dbg_current_proc : \"main\");"
    )?;
    writeln_code!(output, "    qb_dbg_send(msg);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_debug_runtime() {
        let mut output = String::new();
        emit_debug_runtime(&mut output).unwrap();

        // Verify key components are present
        assert!(output.contains("_qb_dbg_enabled"));
        assert!(output.contains("_qb_dbg_paused"));
        assert!(output.contains("qb_dbg_line"));
        assert!(output.contains("qb_dbg_enter_proc"));
        assert!(output.contains("qb_dbg_exit_proc"));
        assert!(output.contains("qb_dbg_init"));
        assert!(output.contains("qb_dbg_shutdown"));
        assert!(output.contains("QBD_MAX_BREAKPOINTS"));
    }

    #[test]
    fn test_platform_specific_code() {
        let mut output = String::new();
        emit_debug_runtime(&mut output).unwrap();

        // Verify Windows and Unix code paths
        assert!(output.contains("#ifdef _WIN32"));
        assert!(output.contains("CreateFileA"));
        assert!(output.contains("O_RDWR"));
    }
}
