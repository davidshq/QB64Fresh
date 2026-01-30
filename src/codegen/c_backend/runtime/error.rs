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

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

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
pub(super) fn emit_error_handling(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Error Handling */")?;
    writeln_code!(output)?;

    // Error state variables
    writeln_code!(
        output,
        "static int32_t _qb_err = 0;           /* Current error code */"
    )?;
    writeln_code!(
        output,
        "static int32_t _qb_erl = 0;           /* Error line number */"
    )?;
    writeln_code!(
        output,
        "static void* _qb_error_handler = NULL; /* Error handler label */"
    )?;
    writeln_code!(
        output,
        "static int _qb_error_resume_next = 0;  /* ON ERROR RESUME NEXT flag */"
    )?;
    writeln_code!(
        output,
        "static void* _qb_error_line = NULL;    /* Line that caused error */"
    )?;
    // Include file error tracking (for $INCLUDE files)
    writeln_code!(
        output,
        "static int32_t _INCLERRORLINE = 0;     /* Error line in include file */"
    )?;
    writeln_code!(
        output,
        "static qb_string* _INCLERRORFILE_str = NULL; /* Include file with error */"
    )?;
    writeln_code!(output)?;

    // qb_error - Simulate an error (ERROR statement)
    // Set _qb_err and _qb_erl so ERR/ERL are correct in the handler. Jump is emitted by codegen after this call.
    writeln_code!(output, "void qb_error(int32_t code) {{")?;
    writeln_code!(output, "    _qb_err = code;")?;
    writeln_code!(output, "    _qb_erl = 0;")?;
    writeln_code!(output, "    if (_qb_error_handler) {{")?;
    writeln_code!(
        output,
        "        /* Jump to error handler - handled by generated code */"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ERR function
    writeln_code!(output, "int32_t qb_err_code(void) {{ return _qb_err; }}")?;
    writeln_code!(output)?;

    // ERL function
    writeln_code!(output, "int32_t qb_err_line(void) {{ return _qb_erl; }}")?;
    writeln_code!(output)?;

    // _ERRORLINE - returns line number where error occurred
    writeln_code!(output, "int64_t qb_errorline(void) {{ return _qb_erl; }}")?;
    writeln_code!(output)?;

    // _ERRORMESSAGE$ - returns error message for current or specified error
    writeln_code!(output, "static const char* _qb_error_messages[] = {{")?;
    writeln_code!(output, "    \"No error\",")?;
    writeln_code!(output, "    \"NEXT without FOR\",")?;
    writeln_code!(output, "    \"Syntax error\",")?;
    writeln_code!(output, "    \"RETURN without GOSUB\",")?;
    writeln_code!(output, "    \"Out of DATA\",")?;
    writeln_code!(output, "    \"Illegal function call\",")?;
    writeln_code!(output, "    \"Overflow\",")?;
    writeln_code!(output, "    \"Out of memory\",")?;
    writeln_code!(output, "    \"Label not defined\",")?;
    writeln_code!(output, "    \"Subscript out of range\",")?;
    writeln_code!(output, "    \"Duplicate definition\",")?;
    writeln_code!(output, "    \"Division by zero\",")?;
    writeln_code!(output, "    \"Type mismatch\",")?;
    writeln_code!(output, "    \"Out of string space\",")?;
    writeln_code!(output, "    \"String too long\",")?;
    writeln_code!(output, "    \"String formula too complex\",")?;
    writeln_code!(output, "}};")?;
    writeln_code!(
        output,
        "#define QB_NUM_ERROR_MESSAGES (sizeof(_qb_error_messages)/sizeof(_qb_error_messages[0]))"
    )?;
    writeln_code!(output)?;

    writeln_code!(output, "qb_string* qb_errormessage(void) {{")?;
    writeln_code!(
        output,
        "    if (_qb_err >= 0 && (size_t)_qb_err < QB_NUM_ERROR_MESSAGES)"
    )?;
    writeln_code!(
        output,
        "        return qb_string_new(_qb_error_messages[_qb_err]);"
    )?;
    writeln_code!(output, "    char buf[64];")?;
    writeln_code!(
        output,
        "    snprintf(buf, sizeof(buf), \"Error %d\", _qb_err);"
    )?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Assertion support ($ASSERTS directive)
    // Runtime variables are set when $ASSERTS directive is encountered in codegen
    writeln_code!(output, "/* Assertion Support ($ASSERTS) */")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "/* Runtime flags for assertion checking (set by $ASSERTS directive) */"
    )?;
    writeln_code!(
        output,
        "/* These are declared as extern in generated code when $ASSERTS is used */"
    )?;
    writeln_code!(
        output,
        "int _qb_asserts_enabled = 0;  /* Set to 1 when $ASSERTS is used */"
    )?;
    writeln_code!(
        output,
        "int _qb_asserts_console = 0;  /* Set to 1 when $ASSERTS:CONSOLE is used */"
    )?;
    writeln_code!(output)?;
    writeln_code!(output, "/* qb_assert - Runtime assertion checking */")?;
    writeln_code!(
        output,
        "/* When $ASSERTS is enabled, this function checks conditions and aborts on failure */"
    )?;
    writeln_code!(
        output,
        "/* When $ASSERTS:CONSOLE is enabled, assertion failures are printed to stderr */"
    )?;
    writeln_code!(
        output,
        "void qb_assert(int condition, const char* message) {{"
    )?;
    writeln_code!(output, "    if (_qb_asserts_enabled && !condition) {{")?;
    writeln_code!(output, "        if (_qb_asserts_console) {{")?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"Assertion failed: %s\\n\", message ? message : \"(no message)\");"
    )?;
    writeln_code!(output, "            fflush(stderr);")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        abort();")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
