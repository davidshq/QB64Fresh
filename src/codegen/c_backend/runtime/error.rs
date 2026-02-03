//! Error handling support for the C runtime.
//!
//! This module provides the `emit_error_handling` function which generates
//! C code for QB64 error handling infrastructure, including:
//!
//! - Error state variables (`_qb_err`, `_qb_erl`, etc.)
//! - The `qb_error()` function for simulating errors
//! - `ERR` and `ERL` functions for querying error state
//! - `_ERRORLINE` and `_ERRORMESSAGE$` for detailed error information
//! - Standard QB64 error message table

use std::fmt::Write;

/// Emits C code for QB64 error handling support.
///
/// This generates:
/// - Static error state variables for tracking current error code and line
/// - `qb_error(code)` - Simulate an error with the given code
/// - `qb_err_code()` - Returns current error code (ERR function)
/// - `qb_err_line()` - Returns error line number (ERL function)
/// - `qb_errorline()` - Returns error line as 64-bit integer (_ERRORLINE)
/// - `qb_errormessage()` - Returns error message string (_ERRORMESSAGE$)
/// - Standard error message lookup table
pub(super) fn emit_error_handling(output: &mut String) {
    writeln!(output, "/* Error Handling */").unwrap();
    writeln!(output).unwrap();

    // Error state variables
    writeln!(
        output,
        "static int32_t _qb_err = 0;           /* Current error code */"
    )
    .unwrap();
    writeln!(
        output,
        "static int32_t _qb_erl = 0;           /* Error line number */"
    )
    .unwrap();
    writeln!(
        output,
        "static void* _qb_error_handler = NULL; /* Error handler label */"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_error_resume_next = 0;  /* ON ERROR RESUME NEXT flag */"
    )
    .unwrap();
    writeln!(
        output,
        "static void* _qb_error_line = NULL;    /* Line that caused error */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // qb_error - Simulate an error
    writeln!(output, "void qb_error(int32_t code) {{").unwrap();
    writeln!(output, "    _qb_err = code;").unwrap();
    writeln!(output, "    if (_qb_error_handler) {{").unwrap();
    writeln!(
        output,
        "        /* Jump to error handler - handled by generated code */"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ERR function
    writeln!(output, "int32_t qb_err_code(void) {{ return _qb_err; }}").unwrap();
    writeln!(output).unwrap();

    // ERL function
    writeln!(output, "int32_t qb_err_line(void) {{ return _qb_erl; }}").unwrap();
    writeln!(output).unwrap();

    // _ERRORLINE - returns line number where error occurred
    writeln!(output, "int64_t qb_errorline(void) {{ return _qb_erl; }}").unwrap();
    writeln!(output).unwrap();

    // _ERRORMESSAGE$ - returns error message for current or specified error
    writeln!(output, "static const char* _qb_error_messages[] = {{").unwrap();
    writeln!(output, "    \"No error\",").unwrap();
    writeln!(output, "    \"NEXT without FOR\",").unwrap();
    writeln!(output, "    \"Syntax error\",").unwrap();
    writeln!(output, "    \"RETURN without GOSUB\",").unwrap();
    writeln!(output, "    \"Out of DATA\",").unwrap();
    writeln!(output, "    \"Illegal function call\",").unwrap();
    writeln!(output, "    \"Overflow\",").unwrap();
    writeln!(output, "    \"Out of memory\",").unwrap();
    writeln!(output, "    \"Label not defined\",").unwrap();
    writeln!(output, "    \"Subscript out of range\",").unwrap();
    writeln!(output, "    \"Duplicate definition\",").unwrap();
    writeln!(output, "    \"Division by zero\",").unwrap();
    writeln!(output, "    \"Type mismatch\",").unwrap();
    writeln!(output, "    \"Out of string space\",").unwrap();
    writeln!(output, "    \"String too long\",").unwrap();
    writeln!(output, "    \"String formula too complex\",").unwrap();
    writeln!(output, "}};").unwrap();
    writeln!(
        output,
        "#define QB_NUM_ERROR_MESSAGES (sizeof(_qb_error_messages)/sizeof(_qb_error_messages[0]))"
    )
    .unwrap();
    writeln!(output).unwrap();

    writeln!(output, "qb_string* qb_errormessage(void) {{").unwrap();
    writeln!(
        output,
        "    if (_qb_err >= 0 && (size_t)_qb_err < QB_NUM_ERROR_MESSAGES)"
    )
    .unwrap();
    writeln!(
        output,
        "        return qb_string_new(_qb_error_messages[_qb_err]);"
    )
    .unwrap();
    writeln!(output, "    char buf[64];").unwrap();
    writeln!(
        output,
        "    snprintf(buf, sizeof(buf), \"Error %d\", _qb_err);"
    )
    .unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
