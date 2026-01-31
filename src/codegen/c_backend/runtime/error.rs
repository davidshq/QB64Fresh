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
/// - QB_ERROR_* constants (libqb error_handle.h compatibility)
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

    // QB_ERROR_* constants (1-76, 256-260, 270-271, 300-315, 502-518) for inline runtime
    writeln_code!(
        output,
        "/* QB_ERROR_* constants (libqb error_handle.h compatibility) */"
    )?;
    writeln_code!(output, "#define QB_ERROR_NEXT_WITHOUT_FOR 1")?;
    writeln_code!(output, "#define QB_ERROR_SYNTAX_ERROR 2")?;
    writeln_code!(output, "#define QB_ERROR_RETURN_WITHOUT_GOSUB 3")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_DATA 4")?;
    writeln_code!(output, "#define QB_ERROR_ILLEGAL_FUNCTION_CALL 5")?;
    writeln_code!(output, "#define QB_ERROR_OVERFLOW 6")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY 7")?;
    writeln_code!(output, "#define QB_ERROR_LABEL_NOT_DEFINED 8")?;
    writeln_code!(output, "#define QB_ERROR_SUBSCRIPT_OUT_OF_RANGE 9")?;
    writeln_code!(output, "#define QB_ERROR_DUPLICATE_DEFINITION 10")?;
    writeln_code!(output, "#define QB_ERROR_DIVISION_BY_ZERO 11")?;
    writeln_code!(output, "#define QB_ERROR_ILLEGAL_IN_DIRECT_MODE 12")?;
    writeln_code!(output, "#define QB_ERROR_TYPE_MISMATCH 13")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_STRING_SPACE 14")?;
    writeln_code!(output, "#define QB_ERROR_STRING_FORMULA_TOO_COMPLEX 16")?;
    writeln_code!(output, "#define QB_ERROR_CANNOT_CONTINUE 17")?;
    writeln_code!(output, "#define QB_ERROR_FUNCTION_NOT_DEFINED 18")?;
    writeln_code!(output, "#define QB_ERROR_NO_RESUME 19")?;
    writeln_code!(output, "#define QB_ERROR_RESUME_WITHOUT_ERROR 20")?;
    writeln_code!(output, "#define QB_ERROR_DEVICE_TIMEOUT 24")?;
    writeln_code!(output, "#define QB_ERROR_DEVICE_FAULT 25")?;
    writeln_code!(output, "#define QB_ERROR_FOR_WITHOUT_NEXT 26")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_PAPER 27")?;
    writeln_code!(output, "#define QB_ERROR_WHILE_WITHOUT_WEND 29")?;
    writeln_code!(output, "#define QB_ERROR_WEND_WITHOUT_WHILE 30")?;
    writeln_code!(output, "#define QB_ERROR_DUPLICATE_LABEL 33")?;
    writeln_code!(output, "#define QB_ERROR_SUBPROGRAM_NOT_DEFINED 35")?;
    writeln_code!(output, "#define QB_ERROR_ARGUMENT_COUNT_MISMATCH 37")?;
    writeln_code!(output, "#define QB_ERROR_ARRAY_NOT_DEFINED 38")?;
    writeln_code!(output, "#define QB_ERROR_VARIABLE_REQUIRED 40")?;
    writeln_code!(output, "#define QB_ERROR_FIELD_OVERFLOW 50")?;
    writeln_code!(output, "#define QB_ERROR_INTERNAL_ERROR 51")?;
    writeln_code!(output, "#define QB_ERROR_BAD_FILE_NAME_OR_NUMBER 52")?;
    writeln_code!(output, "#define QB_ERROR_FILE_NOT_FOUND 53")?;
    writeln_code!(output, "#define QB_ERROR_BAD_FILE_MODE 54")?;
    writeln_code!(output, "#define QB_ERROR_FILE_ALREADY_OPEN 55")?;
    writeln_code!(output, "#define QB_ERROR_FIELD_STATEMENT_ACTIVE 56")?;
    writeln_code!(output, "#define QB_ERROR_DEVICE_IO_ERROR 57")?;
    writeln_code!(output, "#define QB_ERROR_FILE_ALREADY_EXISTS 58")?;
    writeln_code!(output, "#define QB_ERROR_BAD_RECORD_LENGTH 59")?;
    writeln_code!(output, "#define QB_ERROR_DISK_FULL 61")?;
    writeln_code!(output, "#define QB_ERROR_INPUT_PAST_END_OF_FILE 62")?;
    writeln_code!(output, "#define QB_ERROR_BAD_RECORD_NUMBER 63")?;
    writeln_code!(output, "#define QB_ERROR_BAD_FILE_NAME 64")?;
    writeln_code!(output, "#define QB_ERROR_TOO_MANY_FILES 67")?;
    writeln_code!(output, "#define QB_ERROR_DEVICE_UNAVAILABLE 68")?;
    writeln_code!(output, "#define QB_ERROR_COMMUNICATION_BUFFER_OVERFLOW 69")?;
    writeln_code!(output, "#define QB_ERROR_PERMISSION_DENIED 70")?;
    writeln_code!(output, "#define QB_ERROR_DISK_NOT_READY 71")?;
    writeln_code!(output, "#define QB_ERROR_DISK_MEDIA_ERROR 72")?;
    writeln_code!(output, "#define QB_ERROR_FEATURE_UNAVAILABLE 73")?;
    writeln_code!(output, "#define QB_ERROR_RENAME_ACROSS_DISKS 74")?;
    writeln_code!(output, "#define QB_ERROR_PATH_FILE_ACCESS_ERROR 75")?;
    writeln_code!(output, "#define QB_ERROR_PATH_NOT_FOUND 76")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_STACK_SPACE 256")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL 257")?;
    writeln_code!(output, "#define QB_ERROR_INVALID_HANDLE 258")?;
    writeln_code!(
        output,
        "#define QB_ERROR_CANNOT_FIND_DYNAMIC_LIBRARY_FILE 259"
    )?;
    writeln_code!(
        output,
        "#define QB_ERROR_FUNCTION_NOT_FOUND_IN_DYNAMIC_LIBRARY 260"
    )?;
    writeln_code!(
        output,
        "#define QB_ERROR_FUNCTION_NOT_FOUND_IN_DYNAMIC_LIBRARY_261 261"
    )?;
    writeln_code!(
        output,
        "#define QB_ERROR_GL_COMMAND_OUTSIDE_SUB_GL_SCOPE 270"
    )?;
    writeln_code!(output, "#define QB_ERROR_END_SYSTEM_IN_SUB_GL_SCOPE 271")?;
    writeln_code!(output, "#define QB_ERROR_MEMORY_REGION_OUT_OF_RANGE 300")?;
    writeln_code!(output, "#define QB_ERROR_INVALID_SIZE 301")?;
    writeln_code!(
        output,
        "#define QB_ERROR_SOURCE_MEMORY_REGION_OUT_OF_RANGE 302"
    )?;
    writeln_code!(
        output,
        "#define QB_ERROR_DESTINATION_MEMORY_REGION_OUT_OF_RANGE 303"
    )?;
    writeln_code!(
        output,
        "#define QB_ERROR_BOTH_MEMORY_REGIONS_OUT_OF_RANGE 304"
    )?;
    writeln_code!(output, "#define QB_ERROR_SOURCE_MEMORY_FREED 305")?;
    writeln_code!(output, "#define QB_ERROR_DESTINATION_MEMORY_FREED 306")?;
    writeln_code!(output, "#define QB_ERROR_MEMORY_ALREADY_FREED 307")?;
    writeln_code!(output, "#define QB_ERROR_MEMORY_HAS_BEEN_FREED 308")?;
    writeln_code!(output, "#define QB_ERROR_MEMORY_NOT_INITIALIZED 309")?;
    writeln_code!(output, "#define QB_ERROR_SOURCE_MEMORY_NOT_INITIALIZED 310")?;
    writeln_code!(
        output,
        "#define QB_ERROR_DESTINATION_MEMORY_NOT_INITIALIZED 311"
    )?;
    writeln_code!(output, "#define QB_ERROR_BOTH_MEMORY_NOT_INITIALIZED 312")?;
    writeln_code!(output, "#define QB_ERROR_BOTH_MEMORY_FREED 313")?;
    writeln_code!(output, "#define QB_ERROR_ASSERT_FAILED 314")?;
    writeln_code!(
        output,
        "#define QB_ERROR_ASSERT_FAILED_WITH_DESCRIPTION 315"
    )?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_502 502")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_503 503")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_504 504")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_505 505")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_506 506")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_507 507")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_508 508")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_509 509")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_510 510")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_511 511")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_512 512")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_513 513")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_514 514")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_515 515")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_516 516")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_517 517")?;
    writeln_code!(output, "#define QB_ERROR_OUT_OF_MEMORY_FATAL_518 518")?;
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

    // qb_set_error - Set pending error from runtime (e.g. CSNG overflow, file I/O errors).
    // Inline runtime defines this so generated code (qb_csng_float, qb_csng_double, etc.) links.
    writeln_code!(
        output,
        "void qb_set_error(uint32_t code, int32_t line) {{ _qb_err = (int32_t)code; _qb_erl = line; }}"
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
