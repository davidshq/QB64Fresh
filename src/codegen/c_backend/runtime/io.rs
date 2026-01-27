//! I/O Functions for the C Backend Runtime
//!
//! This module contains functions that emit C code for input/output operations
//! in the generated QB64 programs. It handles:
//!
//! - **PRINT operations** - Outputting values to the console with proper formatting
//! - **INPUT operations** - Reading user input from stdin
//! - **Utility functions** - DATA/READ helpers and string conversion utilities
//!
//! These functions generate C code that implements QB64's I/O semantics,
//! including cursor positioning (TAB, SPC, POS), formatted output (PRINT USING),
//! and type-specific input handling.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits C code for PRINT-related functions.
///
/// This includes:
/// - `qb_print_int`, `qb_print_float`, `qb_print_string` - Type-specific print functions
/// - `qb_print_newline`, `qb_print_tab` - Output control
/// - `qb_tab`, `qb_spc` - Cursor positioning functions
/// - `qb_pos`, `qb_csrlin` - Cursor position queries
/// - `qb_print_using` - Formatted output with format strings
///
/// The generated code tracks cursor position for TAB/SPC/POS compatibility.
pub(super) fn emit_print_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "void qb_print_int(int64_t n) {{")?;
    writeln_code!(output, "    printf(\"%lld\", (long long)n);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_print_float(double n) {{")?;
    writeln_code!(output, "    printf(\"%g\", n);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_print_string(qb_string* s) {{")?;
    writeln_code!(output, "    if (s && s->data) printf(\"%s\", s->data);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_print_newline(void) {{")?;
    writeln_code!(output, "    printf(\"\\n\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_print_tab(void) {{")?;
    writeln_code!(output, "    printf(\"\\t\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Print formatting functions
    // Track cursor column for TAB and POS
    writeln_code!(output, "static int qb_cursor_col = 1;")?;
    writeln_code!(output, "static int qb_cursor_row = 1;")?;
    writeln_code!(output)?;

    // TAB(n) - returns string of spaces to move to column n
    writeln_code!(output, "qb_string* qb_tab(int64_t n) {{")?;
    writeln_code!(output, "    if (n < 1) n = 1;")?;
    writeln_code!(output, "    int spaces_needed = (int)(n - qb_cursor_col);")?;
    writeln_code!(output, "    if (spaces_needed < 0) spaces_needed = 0;")?;
    writeln_code!(output, "    char* buf = (char*)malloc(spaces_needed + 1);")?;
    writeln_code!(output, "    memset(buf, ' ', spaces_needed);")?;
    writeln_code!(output, "    buf[spaces_needed] = '\\0';")?;
    writeln_code!(output, "    qb_cursor_col = (int)n;")?;
    writeln_code!(output, "    qb_string* result = qb_string_new(buf);")?;
    writeln_code!(output, "    free(buf);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // SPC(n) - returns string of n spaces
    writeln_code!(output, "qb_string* qb_spc(int64_t n) {{")?;
    writeln_code!(output, "    if (n < 0) n = 0;")?;
    writeln_code!(output, "    char* buf = (char*)malloc((size_t)n + 1);")?;
    writeln_code!(output, "    memset(buf, ' ', (size_t)n);")?;
    writeln_code!(output, "    buf[n] = '\\0';")?;
    writeln_code!(output, "    qb_cursor_col += (int)n;")?;
    writeln_code!(output, "    qb_string* result = qb_string_new(buf);")?;
    writeln_code!(output, "    free(buf);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // POS(n) - returns current cursor column (n is ignored for compatibility)
    writeln_code!(output, "int qb_pos(int64_t n) {{")?;
    writeln_code!(output, "    (void)n; // unused, for compatibility")?;
    writeln_code!(output, "    return qb_cursor_col;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // CSRLIN - returns current cursor row
    writeln_code!(output, "int qb_csrlin(void) {{")?;
    writeln_code!(output, "    return qb_cursor_row;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // PRINT USING support
    // Type constants for QbPrintValue
    writeln_code!(output, "#define QB_TYPE_STRING 0")?;
    writeln_code!(output, "#define QB_TYPE_INT 1")?;
    writeln_code!(output, "#define QB_TYPE_DOUBLE 2")?;
    writeln_code!(output)?;

    // Value union for PRINT USING
    writeln_code!(output, "typedef struct {{")?;
    writeln_code!(output, "    int type;")?;
    writeln_code!(output, "    union {{")?;
    writeln_code!(output, "        qb_string* str_val;")?;
    writeln_code!(output, "        int64_t int_val;")?;
    writeln_code!(output, "        double dbl_val;")?;
    writeln_code!(output, "    }};")?;
    writeln_code!(output, "}} QbPrintValue;")?;
    writeln_code!(output)?;

    // PRINT USING implementation
    writeln_code!(
        output,
        "void qb_print_using(qb_string* fmt, QbPrintValue* values, int num_values) {{"
    )?;
    writeln_code!(output, "    if (!fmt || !fmt->data) return;")?;
    writeln_code!(output, "    const char* f = fmt->data;")?;
    writeln_code!(output, "    int val_idx = 0;")?;
    writeln_code!(output, "    while (*f) {{")?;
    writeln_code!(output, "        // Check for format specifiers")?;
    writeln_code!(
        output,
        "        if (*f == '#' || *f == '+' || *f == '-' || *f == '$' || *f == '*' || *f == '^') {{"
    )?;
    writeln_code!(
        output,
        "            // Numeric format - count consecutive format chars"
    )?;
    writeln_code!(
        output,
        "            int width = 0, decimals = 0, has_decimal = 0, has_sign = 0;"
    )?;
    writeln_code!(
        output,
        "            int has_dollar = 0, has_asterisk = 0, has_exp = 0, exp_digits = 0;"
    )?;
    writeln_code!(output, "            const char* start = f;")?;
    writeln_code!(
        output,
        "            if (*f == '+' || *f == '-') {{ has_sign = 1; f++; width++; }}"
    )?;
    writeln_code!(
        output,
        "            if (*f == '$' && *(f+1) == '$') {{ has_dollar = 1; f += 2; width += 2; }}"
    )?;
    writeln_code!(
        output,
        "            else if (*f == '*' && *(f+1) == '*') {{ has_asterisk = 1; f += 2; width += 2; }}"
    )?;
    writeln_code!(output, "            while (*f == '#') {{ width++; f++; }}")?;
    writeln_code!(
        output,
        "            if (*f == '.') {{ has_decimal = 1; f++; width++; while (*f == '#') {{ decimals++; width++; f++; }} }}"
    )?;
    writeln_code!(
        output,
        "            while (*f == '^') {{ has_exp = 1; exp_digits++; f++; width++; }}"
    )?;
    writeln_code!(output, "            // Now format the value")?;
    writeln_code!(
        output,
        "            if (val_idx < num_values && width > 0) {{"
    )?;
    writeln_code!(output, "                double val = 0;")?;
    writeln_code!(
        output,
        "                if (values[val_idx].type == QB_TYPE_INT) val = (double)values[val_idx].int_val;"
    )?;
    writeln_code!(
        output,
        "                else if (values[val_idx].type == QB_TYPE_DOUBLE) val = values[val_idx].dbl_val;"
    )?;
    writeln_code!(output, "                char buf[64];")?;
    writeln_code!(output, "                if (has_exp) {{")?;
    writeln_code!(
        output,
        "                    snprintf(buf, sizeof(buf), \"%*.*e\", width, decimals, val);"
    )?;
    writeln_code!(output, "                }} else if (has_decimal) {{")?;
    writeln_code!(
        output,
        "                    snprintf(buf, sizeof(buf), \"%*.*f\", width, decimals, val);"
    )?;
    writeln_code!(output, "                }} else {{")?;
    writeln_code!(
        output,
        "                    snprintf(buf, sizeof(buf), \"%*lld\", width, (long long)val);"
    )?;
    writeln_code!(output, "                }}")?;
    writeln_code!(output, "                // Handle asterisk fill")?;
    writeln_code!(output, "                if (has_asterisk) {{")?;
    writeln_code!(
        output,
        "                    for (int i = 0; buf[i] == ' '; i++) buf[i] = '*';"
    )?;
    writeln_code!(output, "                }}")?;
    writeln_code!(output, "                printf(\"%s\", buf);")?;
    writeln_code!(output, "                val_idx++;")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }} else if (*f == '&') {{")?;
    writeln_code!(output, "            // String - print full string")?;
    writeln_code!(output, "            f++;")?;
    writeln_code!(
        output,
        "            if (val_idx < num_values && values[val_idx].type == QB_TYPE_STRING) {{"
    )?;
    writeln_code!(
        output,
        "                if (values[val_idx].str_val && values[val_idx].str_val->data) {{"
    )?;
    writeln_code!(
        output,
        "                    printf(\"%s\", values[val_idx].str_val->data);"
    )?;
    writeln_code!(output, "                }}")?;
    writeln_code!(output, "                val_idx++;")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }} else if (*f == '!') {{")?;
    writeln_code!(output, "            // String - print first character only")?;
    writeln_code!(output, "            f++;")?;
    writeln_code!(
        output,
        "            if (val_idx < num_values && values[val_idx].type == QB_TYPE_STRING) {{"
    )?;
    writeln_code!(
        output,
        "                if (values[val_idx].str_val && values[val_idx].str_val->data && values[val_idx].str_val->data[0]) {{"
    )?;
    writeln_code!(
        output,
        "                    printf(\"%c\", values[val_idx].str_val->data[0]);"
    )?;
    writeln_code!(output, "                }}")?;
    writeln_code!(output, "                val_idx++;")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }} else if (*f == '\\\\') {{")?;
    writeln_code!(
        output,
        "            // Fixed-width string - count spaces between backslashes"
    )?;
    writeln_code!(output, "            f++;")?;
    writeln_code!(
        output,
        "            int str_width = 2; // includes both backslashes"
    )?;
    writeln_code!(
        output,
        "            while (*f && *f != '\\\\') {{ str_width++; f++; }}"
    )?;
    writeln_code!(output, "            if (*f == '\\\\') f++;")?;
    writeln_code!(
        output,
        "            if (val_idx < num_values && values[val_idx].type == QB_TYPE_STRING) {{"
    )?;
    writeln_code!(
        output,
        "                const char* s = values[val_idx].str_val ? values[val_idx].str_val->data : \"\";"
    )?;
    writeln_code!(
        output,
        "                printf(\"%-*.*s\", str_width, str_width, s ? s : \"\");"
    )?;
    writeln_code!(output, "                val_idx++;")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }} else if (*f == '_') {{")?;
    writeln_code!(output, "            // Literal next character")?;
    writeln_code!(output, "            f++;")?;
    writeln_code!(output, "            if (*f) {{ printf(\"%c\", *f); f++; }}")?;
    writeln_code!(output, "        }} else {{")?;
    writeln_code!(output, "            // Literal character")?;
    writeln_code!(output, "            printf(\"%c\", *f);")?;
    writeln_code!(output, "            f++;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits C code for INPUT-related functions.
///
/// This includes:
/// - `qb_input_string` - Read a string from stdin with optional prompt
/// - `qb_input_int` - Read an integer from stdin with optional prompt
/// - `qb_input_float` - Read a floating-point number from stdin with optional prompt
///
/// These functions handle the QB64 INPUT statement semantics, including
/// displaying prompts and parsing user input into the appropriate types.
/// The `same_line` parameter controls whether a newline is printed after input
/// (0 = print newline, non-zero = keep cursor on same line).
pub(super) fn emit_input_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(
        output,
        "void qb_input_string(const char* prompt, qb_string** var, int same_line) {{"
    )?;
    writeln_code!(output, "    char buffer[1024];")?;
    writeln_code!(output, "    if (prompt) printf(\"%s\", prompt);")?;
    writeln_code!(output, "    if (fgets(buffer, sizeof(buffer), stdin)) {{")?;
    writeln_code!(output, "        size_t len = strlen(buffer);")?;
    writeln_code!(
        output,
        "        if (len > 0 && buffer[len-1] == '\\n') buffer[len-1] = '\\0';"
    )?;
    writeln_code!(output, "        if (*var) qb_string_free(*var);")?;
    writeln_code!(output, "        *var = qb_string_new(buffer);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    if (same_line == 0) printf(\"\\n\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_input_int(const char* prompt, int32_t* var, int same_line) {{"
    )?;
    writeln_code!(output, "    if (prompt) printf(\"%s\", prompt);")?;
    writeln_code!(output, "    scanf(\"%d\", var);")?;
    writeln_code!(output, "    if (same_line == 0) printf(\"\\n\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_input_float(const char* prompt, double* var, int same_line) {{"
    )?;
    writeln_code!(output, "    if (prompt) printf(\"%s\", prompt);")?;
    writeln_code!(output, "    scanf(\"%lf\", var);")?;
    writeln_code!(output, "    if (same_line == 0) printf(\"\\n\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits C code for utility functions used by DATA/READ and string conversion.
///
/// This includes:
/// - `qb_str_from_c` - Create a qb_string from a C string (alias for qb_string_new)
/// - `qb_str_float` - Convert a float to a qb_string (used by DATA/READ)
/// - `qb_hex` - HEX$ function: convert number to hexadecimal string
/// - `qb_oct` - OCT$ function: convert number to octal string
/// - `qb_bin` - _BIN$ function: convert number to binary string
/// - `qb_tostr` - _TOSTR$ function: convert number to string without leading space
/// - `qb_iif` - _IIF function: inline conditional for numeric values
/// - `qb_iif_str` - _IIF$ function: inline conditional for string values
pub(super) fn emit_utility_functions(output: &mut String) -> Result<(), CodeGenError> {
    // qb_str_from_c - create qb_string from C string (alias for qb_string_new)
    writeln_code!(output, "qb_string* qb_str_from_c(const char* s) {{")?;
    writeln_code!(output, "    return qb_string_new(s);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_str_float - convert float to string (used by DATA/READ)
    writeln_code!(output, "qb_string* qb_str_float(double n) {{")?;
    writeln_code!(output, "    char buf[64];")?;
    writeln_code!(output, "    snprintf(buf, sizeof(buf), \"%g\", n);")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // HEX$ - convert number to hexadecimal string
    writeln_code!(output, "qb_string* qb_hex(int64_t n) {{")?;
    writeln_code!(output, "    char buf[32];")?;
    writeln_code!(
        output,
        "    snprintf(buf, sizeof(buf), \"%llX\", (unsigned long long)(uint64_t)n);"
    )?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // OCT$ - convert number to octal string
    writeln_code!(output, "qb_string* qb_oct(int64_t n) {{")?;
    writeln_code!(output, "    char buf[32];")?;
    writeln_code!(
        output,
        "    snprintf(buf, sizeof(buf), \"%llo\", (unsigned long long)(uint64_t)n);"
    )?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _BIN$ - convert number to binary string
    writeln_code!(output, "qb_string* qb_bin(int64_t n) {{")?;
    writeln_code!(output, "    char buf[65];")?;
    writeln_code!(output, "    uint64_t v = (uint64_t)n;")?;
    writeln_code!(output, "    char* p = buf + 64;")?;
    writeln_code!(output, "    *p = '\\0';")?;
    writeln_code!(output, "    if (v == 0) {{ *--p = '0'; }}")?;
    writeln_code!(
        output,
        "    else {{ while (v) {{ *--p = '0' + (v & 1); v >>= 1; }} }}"
    )?;
    writeln_code!(output, "    return qb_string_new(p);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _TOSTR$ - convert number to string without leading space
    writeln_code!(output, "qb_string* qb_tostr(double n) {{")?;
    writeln_code!(output, "    char buf[64];")?;
    writeln_code!(
        output,
        "    if (floor(n) == n && n >= -9007199254740992.0 && n <= 9007199254740992.0) {{"
    )?;
    writeln_code!(output, "        snprintf(buf, sizeof(buf), \"%.0f\", n);")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        snprintf(buf, sizeof(buf), \"%.14g\", n);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _IIF - inline conditional for numeric values
    writeln_code!(
        output,
        "double qb_iif(int64_t cond, double true_val, double false_val) {{"
    )?;
    writeln_code!(output, "    return cond ? true_val : false_val;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _IIF$ - inline conditional for string values
    writeln_code!(
        output,
        "qb_string* qb_iif_str(int64_t cond, qb_string* true_val, qb_string* false_val) {{"
    )?;
    writeln_code!(output, "    return cond ? true_val : false_val;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
