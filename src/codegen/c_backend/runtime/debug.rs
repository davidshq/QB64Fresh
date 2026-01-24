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

use std::fmt::Write;

/// Emits the debug runtime declarations and functions.
///
/// This includes:
/// - Debug state variables (paused, stepping, current location)
/// - Breakpoint table
/// - IPC initialization and communication
/// - Line hook function
/// - Procedure entry/exit hooks
pub fn emit_debug_runtime(output: &mut String) {
    writeln!(output, "/* Debug Runtime Support */").unwrap();
    writeln!(output).unwrap();

    // Platform-specific includes for debugging IPC
    emit_debug_includes(output);

    // Debug state variables
    emit_debug_state(output);

    // Breakpoint management
    emit_breakpoint_functions(output);

    // IPC communication
    emit_ipc_functions(output);

    // Main debug hooks
    emit_debug_hooks(output);

    // Variable inspection helpers
    emit_inspection_helpers(output);
}

/// Emits platform-specific includes needed for debug IPC.
fn emit_debug_includes(output: &mut String) {
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(
        output,
        "/* Windows named pipe support already from windows.h */"
    )
    .unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "#include <unistd.h>").unwrap();
    writeln!(output, "#include <fcntl.h>").unwrap();
    writeln!(output, "#include <sys/stat.h>").unwrap();
    writeln!(output, "#include <errno.h>").unwrap();
    writeln!(output, "#include <poll.h>").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();
}

/// Emits debug state variables.
fn emit_debug_state(output: &mut String) {
    writeln!(output, "/* Debug state */").unwrap();
    writeln!(
        output,
        "static volatile int _qb_dbg_enabled = 0;     /* Debug mode active */"
    )
    .unwrap();
    writeln!(
        output,
        "static volatile int _qb_dbg_paused = 0;      /* Execution paused */"
    )
    .unwrap();
    writeln!(
        output,
        "static volatile int _qb_dbg_stepping = 0;    /* Step mode: 0=run, 1=into, 2=over, 3=out */"
    )
    .unwrap();
    writeln!(
        output,
        "static volatile int _qb_dbg_step_depth = 0;  /* Call depth for step over/out */"
    )
    .unwrap();
    writeln!(
        output,
        "static volatile int _qb_dbg_current_line = 0;/* Current source line */"
    )
    .unwrap();
    writeln!(
        output,
        "static const char* _qb_dbg_current_file = NULL; /* Current source file */"
    )
    .unwrap();
    writeln!(
        output,
        "static const char* _qb_dbg_current_proc = NULL; /* Current procedure name */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Breakpoint table
    writeln!(output, "/* Breakpoint table */").unwrap();
    writeln!(output, "#define QBD_MAX_BREAKPOINTS 256").unwrap();
    writeln!(
        output,
        "static int _qb_dbg_bp_lines[QBD_MAX_BREAKPOINTS];  /* Line numbers */"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_dbg_bp_enabled[QBD_MAX_BREAKPOINTS]; /* Enabled flags */"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_dbg_bp_count = 0;                    /* Active breakpoint count */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // IPC handle
    writeln!(output, "/* Debug IPC */").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "static HANDLE _qb_dbg_pipe = INVALID_HANDLE_VALUE;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "static int _qb_dbg_pipe_fd = -1;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(
        output,
        "static char _qb_dbg_cmd_buf[1024]; /* Command buffer */"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_dbg_cmd_len = 0;    /* Bytes in buffer */"
    )
    .unwrap();
    writeln!(output).unwrap();
}

/// Emits breakpoint management functions.
fn emit_breakpoint_functions(output: &mut String) {
    // Add breakpoint
    writeln!(output, "/* Add a breakpoint at the given line */").unwrap();
    writeln!(output, "static void qb_dbg_add_bp(int line) {{").unwrap();
    writeln!(
        output,
        "    if (_qb_dbg_bp_count >= QBD_MAX_BREAKPOINTS) return;"
    )
    .unwrap();
    writeln!(output, "    _qb_dbg_bp_lines[_qb_dbg_bp_count] = line;").unwrap();
    writeln!(output, "    _qb_dbg_bp_enabled[_qb_dbg_bp_count] = 1;").unwrap();
    writeln!(output, "    _qb_dbg_bp_count++;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Remove breakpoint
    writeln!(output, "/* Remove a breakpoint at the given line */").unwrap();
    writeln!(output, "static void qb_dbg_remove_bp(int line) {{").unwrap();
    writeln!(output, "    for (int i = 0; i < _qb_dbg_bp_count; i++) {{").unwrap();
    writeln!(output, "        if (_qb_dbg_bp_lines[i] == line) {{").unwrap();
    writeln!(output, "            /* Shift remaining breakpoints down */").unwrap();
    writeln!(
        output,
        "            for (int j = i; j < _qb_dbg_bp_count - 1; j++) {{"
    )
    .unwrap();
    writeln!(
        output,
        "                _qb_dbg_bp_lines[j] = _qb_dbg_bp_lines[j+1];"
    )
    .unwrap();
    writeln!(
        output,
        "                _qb_dbg_bp_enabled[j] = _qb_dbg_bp_enabled[j+1];"
    )
    .unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "            _qb_dbg_bp_count--;").unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Check if line is a breakpoint
    writeln!(output, "/* Check if line has an enabled breakpoint */").unwrap();
    writeln!(output, "static int qb_dbg_is_bp(int line) {{").unwrap();
    writeln!(output, "    for (int i = 0; i < _qb_dbg_bp_count; i++) {{").unwrap();
    writeln!(
        output,
        "        if (_qb_dbg_bp_lines[i] == line && _qb_dbg_bp_enabled[i]) {{"
    )
    .unwrap();
    writeln!(output, "            return 1;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits IPC communication functions.
fn emit_ipc_functions(output: &mut String) {
    // Initialize debug subsystem
    writeln!(output, "/* Initialize debug subsystem */").unwrap();
    writeln!(output, "static void qb_dbg_init(const char* pipe_path) {{").unwrap();
    writeln!(output, "    _qb_dbg_enabled = 1;").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    _qb_dbg_pipe = CreateFileA(").unwrap();
    writeln!(output, "        pipe_path,").unwrap();
    writeln!(output, "        GENERIC_READ | GENERIC_WRITE,").unwrap();
    writeln!(output, "        0, NULL, OPEN_EXISTING, 0, NULL);").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe != INVALID_HANDLE_VALUE) {{").unwrap();
    writeln!(output, "        DWORD written;").unwrap();
    writeln!(
        output,
        "        WriteFile(_qb_dbg_pipe, \"READY\\n\", 6, &written, NULL);"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    _qb_dbg_pipe_fd = open(pipe_path, O_RDWR);").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe_fd >= 0) {{").unwrap();
    writeln!(output, "        /* Set non-blocking mode for polling */").unwrap();
    writeln!(
        output,
        "        int flags = fcntl(_qb_dbg_pipe_fd, F_GETFL, 0);"
    )
    .unwrap();
    writeln!(
        output,
        "        fcntl(_qb_dbg_pipe_fd, F_SETFL, flags | O_NONBLOCK);"
    )
    .unwrap();
    writeln!(output, "        write(_qb_dbg_pipe_fd, \"READY\\n\", 6);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Close debug subsystem
    writeln!(output, "/* Shutdown debug subsystem */").unwrap();
    writeln!(output, "static void qb_dbg_shutdown(void) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe != INVALID_HANDLE_VALUE) {{").unwrap();
    writeln!(output, "        DWORD written;").unwrap();
    writeln!(
        output,
        "        WriteFile(_qb_dbg_pipe, \"TERMINATED\\n\", 11, &written, NULL);"
    )
    .unwrap();
    writeln!(output, "        CloseHandle(_qb_dbg_pipe);").unwrap();
    writeln!(output, "        _qb_dbg_pipe = INVALID_HANDLE_VALUE;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe_fd >= 0) {{").unwrap();
    writeln!(
        output,
        "        write(_qb_dbg_pipe_fd, \"TERMINATED\\n\", 11);"
    )
    .unwrap();
    writeln!(output, "        close(_qb_dbg_pipe_fd);").unwrap();
    writeln!(output, "        _qb_dbg_pipe_fd = -1;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "    _qb_dbg_enabled = 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Send event to debugger
    writeln!(output, "/* Send an event to the debugger */").unwrap();
    writeln!(output, "static void qb_dbg_send(const char* msg) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe != INVALID_HANDLE_VALUE) {{").unwrap();
    writeln!(output, "        DWORD written;").unwrap();
    writeln!(
        output,
        "        WriteFile(_qb_dbg_pipe, msg, (DWORD)strlen(msg), &written, NULL);"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe_fd >= 0) {{").unwrap();
    writeln!(output, "        write(_qb_dbg_pipe_fd, msg, strlen(msg));").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Process incoming commands
    emit_process_commands(output);
}

/// Emits the command processing function.
fn emit_process_commands(output: &mut String) {
    writeln!(
        output,
        "/* Process commands from debugger (non-blocking) */"
    )
    .unwrap();
    writeln!(output, "static void qb_dbg_process_commands(void) {{").unwrap();
    writeln!(output, "    char buf[256];").unwrap();
    writeln!(output, "    ssize_t n;").unwrap();
    writeln!(output).unwrap();

    // Read from pipe
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(
        output,
        "    if (_qb_dbg_pipe == INVALID_HANDLE_VALUE) return;"
    )
    .unwrap();
    writeln!(output, "    DWORD available = 0;").unwrap();
    writeln!(
        output,
        "    if (!PeekNamedPipe(_qb_dbg_pipe, NULL, 0, NULL, &available, NULL) || available == 0) return;"
    )
    .unwrap();
    writeln!(output, "    DWORD read_bytes;").unwrap();
    writeln!(
        output,
        "    if (!ReadFile(_qb_dbg_pipe, buf, sizeof(buf)-1, &read_bytes, NULL)) return;"
    )
    .unwrap();
    writeln!(output, "    n = (ssize_t)read_bytes;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    if (_qb_dbg_pipe_fd < 0) return;").unwrap();
    writeln!(output, "    n = read(_qb_dbg_pipe_fd, buf, sizeof(buf)-1);").unwrap();
    writeln!(output, "    if (n <= 0) return;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    buf[n] = '\\0';").unwrap();
    writeln!(output).unwrap();

    // Parse commands
    writeln!(output, "    /* Parse command */").unwrap();
    writeln!(output, "    char* cmd = buf;").unwrap();
    writeln!(output, "    while (*cmd == ' ' || *cmd == '\\n') cmd++;").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "    if (strncmp(cmd, \"CONTINUE\", 8) == 0) {{").unwrap();
    writeln!(output, "        _qb_dbg_paused = 0;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 0;").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"STEP_INTO\", 9) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_paused = 0;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 1; /* Step into */").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"STEP_OVER\", 9) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_paused = 0;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 2; /* Step over */").unwrap();
    writeln!(output, "        _qb_dbg_step_depth = 0;").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"STEP_OUT\", 8) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_paused = 0;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 3; /* Step out */").unwrap();
    writeln!(output, "        _qb_dbg_step_depth = 1;").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"PAUSE\", 5) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_paused = 1;").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"TERMINATE\", 9) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        qb_dbg_shutdown();").unwrap();
    writeln!(output, "        exit(0);").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"BP_ADD \", 7) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        int line = atoi(cmd + 7);").unwrap();
    writeln!(output, "        if (line > 0) qb_dbg_add_bp(line);").unwrap();
    writeln!(
        output,
        "    }} else if (strncmp(cmd, \"BP_REMOVE \", 10) == 0) {{"
    )
    .unwrap();
    writeln!(output, "        int line = atoi(cmd + 10);").unwrap();
    writeln!(output, "        if (line > 0) qb_dbg_remove_bp(line);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits the main debug hook functions.
fn emit_debug_hooks(output: &mut String) {
    // Line hook - called before each statement
    writeln!(output, "/* Called before each statement */").unwrap();
    writeln!(
        output,
        "static void qb_dbg_line(int line, const char* file) {{"
    )
    .unwrap();
    writeln!(output, "    if (!_qb_dbg_enabled) return;").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    _qb_dbg_current_line = line;").unwrap();
    writeln!(output, "    _qb_dbg_current_file = file;").unwrap();
    writeln!(output).unwrap();

    // Check breakpoints
    writeln!(output, "    /* Check breakpoints */").unwrap();
    writeln!(output, "    if (qb_dbg_is_bp(line)) {{").unwrap();
    writeln!(output, "        _qb_dbg_paused = 1;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 0;").unwrap();
    writeln!(output, "        char msg[128];").unwrap();
    writeln!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED breakpoint %d %s\\n\", line, file ? file : \"\");"
    )
    .unwrap();
    writeln!(output, "        qb_dbg_send(msg);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();

    // Check step mode
    writeln!(output, "    /* Check step mode */").unwrap();
    writeln!(output, "    if (_qb_dbg_stepping == 1) {{ /* Step into */").unwrap();
    writeln!(output, "        _qb_dbg_paused = 1;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 0;").unwrap();
    writeln!(output, "        char msg[128];").unwrap();
    writeln!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED step %d %s\\n\", line, file ? file : \"\");"
    )
    .unwrap();
    writeln!(output, "        qb_dbg_send(msg);").unwrap();
    writeln!(
        output,
        "    }} else if (_qb_dbg_stepping == 2 && _qb_dbg_step_depth <= 0) {{ /* Step over */"
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_paused = 1;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 0;").unwrap();
    writeln!(output, "        char msg[128];").unwrap();
    writeln!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED step %d %s\\n\", line, file ? file : \"\");"
    )
    .unwrap();
    writeln!(output, "        qb_dbg_send(msg);").unwrap();
    writeln!(
        output,
        "    }} else if (_qb_dbg_stepping == 3 && _qb_dbg_step_depth <= 0) {{ /* Step out */"
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_paused = 1;").unwrap();
    writeln!(output, "        _qb_dbg_stepping = 0;").unwrap();
    writeln!(output, "        char msg[128];").unwrap();
    writeln!(
        output,
        "        snprintf(msg, sizeof(msg), \"STOPPED step %d %s\\n\", line, file ? file : \"\");"
    )
    .unwrap();
    writeln!(output, "        qb_dbg_send(msg);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();

    // Wait while paused
    writeln!(output, "    /* Wait while paused, processing commands */").unwrap();
    writeln!(output, "    while (_qb_dbg_paused) {{").unwrap();
    writeln!(output, "        qb_dbg_process_commands();").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "        Sleep(1); /* 1ms poll */").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "        usleep(1000); /* 1ms poll */").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Procedure entry hook
    writeln!(output, "/* Called on SUB/FUNCTION entry */").unwrap();
    writeln!(
        output,
        "static void qb_dbg_enter_proc(const char* name, int line) {{"
    )
    .unwrap();
    writeln!(output, "    if (!_qb_dbg_enabled) return;").unwrap();
    writeln!(output, "    _qb_dbg_current_proc = name;").unwrap();
    writeln!(output, "    _qb_dbg_step_depth++;").unwrap();
    writeln!(output, "    char msg[128];").unwrap();
    writeln!(
        output,
        "    snprintf(msg, sizeof(msg), \"ENTER %s %d\\n\", name, line);"
    )
    .unwrap();
    writeln!(output, "    qb_dbg_send(msg);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Procedure exit hook
    writeln!(output, "/* Called on SUB/FUNCTION exit */").unwrap();
    writeln!(output, "static void qb_dbg_exit_proc(const char* name) {{").unwrap();
    writeln!(output, "    if (!_qb_dbg_enabled) return;").unwrap();
    writeln!(output, "    _qb_dbg_step_depth--;").unwrap();
    writeln!(output, "    char msg[128];").unwrap();
    writeln!(
        output,
        "    snprintf(msg, sizeof(msg), \"EXIT %s\\n\", name);"
    )
    .unwrap();
    writeln!(output, "    qb_dbg_send(msg);").unwrap();
    writeln!(output, "    _qb_dbg_current_proc = NULL;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits variable inspection helper functions.
fn emit_inspection_helpers(output: &mut String) {
    // Send variable value to debugger
    writeln!(output, "/* Send a variable value to the debugger */").unwrap();
    writeln!(
        output,
        "static void qb_dbg_var_int(const char* name, int64_t value) {{"
    )
    .unwrap();
    writeln!(output, "    char msg[256];").unwrap();
    writeln!(
        output,
        "    snprintf(msg, sizeof(msg), \"VAR %s int %lld\\n\", name, (long long)value);"
    )
    .unwrap();
    writeln!(output, "    qb_dbg_send(msg);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "static void qb_dbg_var_float(const char* name, double value) {{"
    )
    .unwrap();
    writeln!(output, "    char msg[256];").unwrap();
    writeln!(
        output,
        "    snprintf(msg, sizeof(msg), \"VAR %s float %.17g\\n\", name, value);"
    )
    .unwrap();
    writeln!(output, "    qb_dbg_send(msg);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "static void qb_dbg_var_string(const char* name, qb_string* value) {{"
    )
    .unwrap();
    writeln!(output, "    char msg[1024];").unwrap();
    writeln!(output, "    if (value && value->data) {{").unwrap();
    writeln!(
        output,
        "        snprintf(msg, sizeof(msg), \"VAR %s string \\\"%.*s\\\"\\n\", name, (int)(value->length < 900 ? value->length : 900), value->data);"
    )
    .unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(
        output,
        "        snprintf(msg, sizeof(msg), \"VAR %s string \\\"\\\"\\n\", name);"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    qb_dbg_send(msg);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Send current location info
    writeln!(output, "/* Send current location to debugger */").unwrap();
    writeln!(output, "static void qb_dbg_send_location(void) {{").unwrap();
    writeln!(output, "    char msg[256];").unwrap();
    writeln!(
        output,
        "    snprintf(msg, sizeof(msg), \"LOCATION %d %s %s\\n\","
    )
    .unwrap();
    writeln!(output, "        _qb_dbg_current_line,").unwrap();
    writeln!(
        output,
        "        _qb_dbg_current_file ? _qb_dbg_current_file : \"\","
    )
    .unwrap();
    writeln!(
        output,
        "        _qb_dbg_current_proc ? _qb_dbg_current_proc : \"main\");"
    )
    .unwrap();
    writeln!(output, "    qb_dbg_send(msg);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_debug_runtime() {
        let mut output = String::new();
        emit_debug_runtime(&mut output);

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
        emit_debug_runtime(&mut output);

        // Verify Windows and Unix code paths
        assert!(output.contains("#ifdef _WIN32"));
        assert!(output.contains("CreateFileA"));
        assert!(output.contains("O_RDWR"));
    }
}
