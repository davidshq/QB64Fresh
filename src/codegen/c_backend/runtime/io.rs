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

use std::fmt::Write;

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
pub(super) fn emit_print_functions(output: &mut String) {
    writeln!(output, "void qb_print_int(int64_t n) {{").unwrap();
    writeln!(output, "    printf(\"%lld\", (long long)n);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_print_float(double n) {{").unwrap();
    writeln!(output, "    printf(\"%g\", n);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_print_string(qb_string* s) {{").unwrap();
    writeln!(output, "    if (s && s->data) printf(\"%s\", s->data);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_print_newline(void) {{").unwrap();
    writeln!(output, "    printf(\"\\n\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_print_tab(void) {{").unwrap();
    writeln!(output, "    printf(\"\\t\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Print formatting functions
    // Track cursor column for TAB and POS
    writeln!(output, "static int qb_cursor_col = 1;").unwrap();
    writeln!(output, "static int qb_cursor_row = 1;").unwrap();
    writeln!(output).unwrap();

    // TAB(n) - returns string of spaces to move to column n
    writeln!(output, "qb_string* qb_tab(int64_t n) {{").unwrap();
    writeln!(output, "    if (n < 1) n = 1;").unwrap();
    writeln!(output, "    int spaces_needed = (int)(n - qb_cursor_col);").unwrap();
    writeln!(output, "    if (spaces_needed < 0) spaces_needed = 0;").unwrap();
    writeln!(output, "    char* buf = (char*)malloc(spaces_needed + 1);").unwrap();
    writeln!(output, "    memset(buf, ' ', spaces_needed);").unwrap();
    writeln!(output, "    buf[spaces_needed] = '\\0';").unwrap();
    writeln!(output, "    qb_cursor_col = (int)n;").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(buf);").unwrap();
    writeln!(output, "    free(buf);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // SPC(n) - returns string of n spaces
    writeln!(output, "qb_string* qb_spc(int64_t n) {{").unwrap();
    writeln!(output, "    if (n < 0) n = 0;").unwrap();
    writeln!(output, "    char* buf = (char*)malloc((size_t)n + 1);").unwrap();
    writeln!(output, "    memset(buf, ' ', (size_t)n);").unwrap();
    writeln!(output, "    buf[n] = '\\0';").unwrap();
    writeln!(output, "    qb_cursor_col += (int)n;").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(buf);").unwrap();
    writeln!(output, "    free(buf);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // POS(n) - returns current cursor column (n is ignored for compatibility)
    writeln!(output, "int qb_pos(int64_t n) {{").unwrap();
    writeln!(output, "    (void)n; // unused, for compatibility").unwrap();
    writeln!(output, "    return qb_cursor_col;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // CSRLIN - returns current cursor row
    writeln!(output, "int qb_csrlin(void) {{").unwrap();
    writeln!(output, "    return qb_cursor_row;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // PRINT USING support
    // Type constants for QbPrintValue
    writeln!(output, "#define QB_TYPE_STRING 0").unwrap();
    writeln!(output, "#define QB_TYPE_INT 1").unwrap();
    writeln!(output, "#define QB_TYPE_DOUBLE 2").unwrap();
    writeln!(output).unwrap();

    // Value union for PRINT USING
    writeln!(output, "typedef struct {{").unwrap();
    writeln!(output, "    int type;").unwrap();
    writeln!(output, "    union {{").unwrap();
    writeln!(output, "        qb_string* str_val;").unwrap();
    writeln!(output, "        int64_t int_val;").unwrap();
    writeln!(output, "        double dbl_val;").unwrap();
    writeln!(output, "    }};").unwrap();
    writeln!(output, "}} QbPrintValue;").unwrap();
    writeln!(output).unwrap();

    // PRINT USING implementation
    writeln!(
        output,
        "void qb_print_using(qb_string* fmt, QbPrintValue* values, int num_values) {{"
    )
    .unwrap();
    writeln!(output, "    if (!fmt || !fmt->data) return;").unwrap();
    writeln!(output, "    const char* f = fmt->data;").unwrap();
    writeln!(output, "    int val_idx = 0;").unwrap();
    writeln!(output, "    while (*f) {{").unwrap();
    writeln!(output, "        // Check for format specifiers").unwrap();
    writeln!(
        output,
        "        if (*f == '#' || *f == '+' || *f == '-' || *f == '$' || *f == '*' || *f == '^') {{"
    )
    .unwrap();
    writeln!(
        output,
        "            // Numeric format - count consecutive format chars"
    )
    .unwrap();
    writeln!(
        output,
        "            int width = 0, decimals = 0, has_decimal = 0, has_sign = 0;"
    )
    .unwrap();
    writeln!(
        output,
        "            int has_dollar = 0, has_asterisk = 0, has_exp = 0, exp_digits = 0;"
    )
    .unwrap();
    writeln!(output, "            const char* start = f;").unwrap();
    writeln!(
        output,
        "            if (*f == '+' || *f == '-') {{ has_sign = 1; f++; width++; }}"
    )
    .unwrap();
    writeln!(
        output,
        "            if (*f == '$' && *(f+1) == '$') {{ has_dollar = 1; f += 2; width += 2; }}"
    )
    .unwrap();
    writeln!(output, "            else if (*f == '*' && *(f+1) == '*') {{ has_asterisk = 1; f += 2; width += 2; }}").unwrap();
    writeln!(output, "            while (*f == '#') {{ width++; f++; }}").unwrap();
    writeln!(output, "            if (*f == '.') {{ has_decimal = 1; f++; width++; while (*f == '#') {{ decimals++; width++; f++; }} }}").unwrap();
    writeln!(
        output,
        "            while (*f == '^') {{ has_exp = 1; exp_digits++; f++; width++; }}"
    )
    .unwrap();
    writeln!(output, "            // Now format the value").unwrap();
    writeln!(
        output,
        "            if (val_idx < num_values && width > 0) {{"
    )
    .unwrap();
    writeln!(output, "                double val = 0;").unwrap();
    writeln!(output, "                if (values[val_idx].type == QB_TYPE_INT) val = (double)values[val_idx].int_val;").unwrap();
    writeln!(output, "                else if (values[val_idx].type == QB_TYPE_DOUBLE) val = values[val_idx].dbl_val;").unwrap();
    writeln!(output, "                char buf[64];").unwrap();
    writeln!(output, "                if (has_exp) {{").unwrap();
    writeln!(
        output,
        "                    snprintf(buf, sizeof(buf), \"%*.*e\", width, decimals, val);"
    )
    .unwrap();
    writeln!(output, "                }} else if (has_decimal) {{").unwrap();
    writeln!(
        output,
        "                    snprintf(buf, sizeof(buf), \"%*.*f\", width, decimals, val);"
    )
    .unwrap();
    writeln!(output, "                }} else {{").unwrap();
    writeln!(
        output,
        "                    snprintf(buf, sizeof(buf), \"%*lld\", width, (long long)val);"
    )
    .unwrap();
    writeln!(output, "                }}").unwrap();
    writeln!(output, "                // Handle asterisk fill").unwrap();
    writeln!(output, "                if (has_asterisk) {{").unwrap();
    writeln!(
        output,
        "                    for (int i = 0; buf[i] == ' '; i++) buf[i] = '*';"
    )
    .unwrap();
    writeln!(output, "                }}").unwrap();
    writeln!(output, "                printf(\"%s\", buf);").unwrap();
    writeln!(output, "                val_idx++;").unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "        }} else if (*f == '&') {{").unwrap();
    writeln!(output, "            // String - print full string").unwrap();
    writeln!(output, "            f++;").unwrap();
    writeln!(
        output,
        "            if (val_idx < num_values && values[val_idx].type == QB_TYPE_STRING) {{"
    )
    .unwrap();
    writeln!(
        output,
        "                if (values[val_idx].str_val && values[val_idx].str_val->data) {{"
    )
    .unwrap();
    writeln!(
        output,
        "                    printf(\"%s\", values[val_idx].str_val->data);"
    )
    .unwrap();
    writeln!(output, "                }}").unwrap();
    writeln!(output, "                val_idx++;").unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "        }} else if (*f == '!') {{").unwrap();
    writeln!(output, "            // String - print first character only").unwrap();
    writeln!(output, "            f++;").unwrap();
    writeln!(
        output,
        "            if (val_idx < num_values && values[val_idx].type == QB_TYPE_STRING) {{"
    )
    .unwrap();
    writeln!(output, "                if (values[val_idx].str_val && values[val_idx].str_val->data && values[val_idx].str_val->data[0]) {{").unwrap();
    writeln!(
        output,
        "                    printf(\"%c\", values[val_idx].str_val->data[0]);"
    )
    .unwrap();
    writeln!(output, "                }}").unwrap();
    writeln!(output, "                val_idx++;").unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "        }} else if (*f == '\\\\') {{").unwrap();
    writeln!(
        output,
        "            // Fixed-width string - count spaces between backslashes"
    )
    .unwrap();
    writeln!(output, "            f++;").unwrap();
    writeln!(
        output,
        "            int str_width = 2; // includes both backslashes"
    )
    .unwrap();
    writeln!(
        output,
        "            while (*f && *f != '\\\\') {{ str_width++; f++; }}"
    )
    .unwrap();
    writeln!(output, "            if (*f == '\\\\') f++;").unwrap();
    writeln!(
        output,
        "            if (val_idx < num_values && values[val_idx].type == QB_TYPE_STRING) {{"
    )
    .unwrap();
    writeln!(output, "                const char* s = values[val_idx].str_val ? values[val_idx].str_val->data : \"\";").unwrap();
    writeln!(
        output,
        "                printf(\"%-*.*s\", str_width, str_width, s ? s : \"\");"
    )
    .unwrap();
    writeln!(output, "                val_idx++;").unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "        }} else if (*f == '_') {{").unwrap();
    writeln!(output, "            // Literal next character").unwrap();
    writeln!(output, "            f++;").unwrap();
    writeln!(output, "            if (*f) {{ printf(\"%c\", *f); f++; }}").unwrap();
    writeln!(output, "        }} else {{").unwrap();
    writeln!(output, "            // Literal character").unwrap();
    writeln!(output, "            printf(\"%c\", *f);").unwrap();
    writeln!(output, "            f++;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
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
pub(super) fn emit_input_functions(output: &mut String) {
    writeln!(
        output,
        "void qb_input_string(const char* prompt, qb_string** var) {{"
    )
    .unwrap();
    writeln!(output, "    char buffer[1024];").unwrap();
    writeln!(output, "    if (prompt) printf(\"%s\", prompt);").unwrap();
    writeln!(output, "    if (fgets(buffer, sizeof(buffer), stdin)) {{").unwrap();
    writeln!(output, "        size_t len = strlen(buffer);").unwrap();
    writeln!(
        output,
        "        if (len > 0 && buffer[len-1] == '\\n') buffer[len-1] = '\\0';"
    )
    .unwrap();
    writeln!(output, "        if (*var) qb_string_free(*var);").unwrap();
    writeln!(output, "        *var = qb_string_new(buffer);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_input_int(const char* prompt, int32_t* var) {{"
    )
    .unwrap();
    writeln!(output, "    if (prompt) printf(\"%s\", prompt);").unwrap();
    writeln!(output, "    scanf(\"%d\", var);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_input_float(const char* prompt, double* var) {{"
    )
    .unwrap();
    writeln!(output, "    if (prompt) printf(\"%s\", prompt);").unwrap();
    writeln!(output, "    scanf(\"%lf\", var);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
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
pub(super) fn emit_utility_functions(output: &mut String) {
    // qb_str_from_c - create qb_string from C string (alias for qb_string_new)
    writeln!(output, "qb_string* qb_str_from_c(const char* s) {{").unwrap();
    writeln!(output, "    return qb_string_new(s);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_str_float - convert float to string (used by DATA/READ)
    writeln!(output, "qb_string* qb_str_float(double n) {{").unwrap();
    writeln!(output, "    char buf[64];").unwrap();
    writeln!(output, "    snprintf(buf, sizeof(buf), \"%g\", n);").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // HEX$ - convert number to hexadecimal string
    writeln!(output, "qb_string* qb_hex(int64_t n) {{").unwrap();
    writeln!(output, "    char buf[32];").unwrap();
    writeln!(
        output,
        "    snprintf(buf, sizeof(buf), \"%llX\", (unsigned long long)(uint64_t)n);"
    )
    .unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // OCT$ - convert number to octal string
    writeln!(output, "qb_string* qb_oct(int64_t n) {{").unwrap();
    writeln!(output, "    char buf[32];").unwrap();
    writeln!(
        output,
        "    snprintf(buf, sizeof(buf), \"%llo\", (unsigned long long)(uint64_t)n);"
    )
    .unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _BIN$ - convert number to binary string
    writeln!(output, "qb_string* qb_bin(int64_t n) {{").unwrap();
    writeln!(output, "    char buf[65];").unwrap();
    writeln!(output, "    uint64_t v = (uint64_t)n;").unwrap();
    writeln!(output, "    char* p = buf + 64;").unwrap();
    writeln!(output, "    *p = '\\0';").unwrap();
    writeln!(output, "    if (v == 0) {{ *--p = '0'; }}").unwrap();
    writeln!(
        output,
        "    else {{ while (v) {{ *--p = '0' + (v & 1); v >>= 1; }} }}"
    )
    .unwrap();
    writeln!(output, "    return qb_string_new(p);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _TOSTR$ - convert number to string without leading space
    writeln!(output, "qb_string* qb_tostr(double n) {{").unwrap();
    writeln!(output, "    char buf[64];").unwrap();
    writeln!(
        output,
        "    if (floor(n) == n && n >= -9007199254740992.0 && n <= 9007199254740992.0) {{"
    )
    .unwrap();
    writeln!(output, "        snprintf(buf, sizeof(buf), \"%.0f\", n);").unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(output, "        snprintf(buf, sizeof(buf), \"%.14g\", n);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _IIF - inline conditional for numeric values
    writeln!(
        output,
        "double qb_iif(int64_t cond, double true_val, double false_val) {{"
    )
    .unwrap();
    writeln!(output, "    return cond ? true_val : false_val;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _IIF$ - inline conditional for string values
    writeln!(
        output,
        "qb_string* qb_iif_str(int64_t cond, qb_string* true_val, qb_string* false_val) {{"
    )
    .unwrap();
    writeln!(output, "    return cond ? true_val : false_val;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
