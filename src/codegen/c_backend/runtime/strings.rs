//! String Runtime Functions
//!
//! This module contains the C code generation for QB64's string handling runtime.
//! It emits C functions for:
//!
//! - **Core string operations**: allocation, deallocation, reference counting, concatenation
//! - **String comparison**: lexicographic comparison with NULL safety
//! - **String manipulation**: LEFT$, RIGHT$, MID$, INSTR, STR$, VAL, UCASE$, LCASE$,
//!   LTRIM$, RTRIM$, SPACE$, STRING$, and UTF-8 helper functions
//!
//! All functions emit C code that operates on `qb_string*` pointers, which are
//! reference-counted string structures with the following layout:
//!
//! ```c
//! typedef struct {
//!     char* data;
//!     size_t len;
//!     size_t capacity;
//!     int refcount;
//! } qb_string;
//! ```

use std::fmt::Write;

/// Emits the temporary string pool for automatic cleanup.
///
/// This provides a mechanism similar to QB64pe's qbs_tmp_base/qbs_cleanup pattern
/// to prevent memory leaks from temporary strings created during expression evaluation.
///
/// - `qb_tmp_str_mark()` - Returns current pool position
/// - `qb_tmp_str_register()` - Registers a temp string and returns it
/// - `qb_tmp_str_cleanup()` - Frees all temps since a mark
pub(super) fn emit_temp_string_pool(output: &mut String) {
    // Static empty string - defined early so it can be referenced by temp pool
    writeln!(output, "/* Static Empty String */").unwrap();
    writeln!(output, "static char _qbs_empty_data[1] = {{'\\0'}};").unwrap();
    writeln!(
        output,
        "static qb_string _qbs_empty = {{_qbs_empty_data, 0, 1, 999999}};"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Temporary string pool for automatic cleanup
    writeln!(output, "/* Temporary String Pool */").unwrap();
    writeln!(output, "#define QBS_TMP_MAX 16384").unwrap();
    writeln!(output, "static qb_string* _qbs_tmp_pool[QBS_TMP_MAX];").unwrap();
    writeln!(output, "static uint32_t _qbs_tmp_next = 0;").unwrap();
    writeln!(output).unwrap();

    // Mark current position
    writeln!(output, "uint32_t qbs_tmp_base_get(void) {{").unwrap();
    writeln!(output, "    return _qbs_tmp_next;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Register a temporary string - strings start with refcount 1
    // When assigned to a variable, retain() increments to 2
    // When cleanup runs, release() decrements back to 1 (variable still has it)
    writeln!(output, "qb_string* qbs_tmp_register(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s == &_qbs_empty) return s;").unwrap();
    writeln!(output, "    if (_qbs_tmp_next < QBS_TMP_MAX) {{").unwrap();
    writeln!(output, "        _qbs_tmp_pool[_qbs_tmp_next++] = s;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return s;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Cleanup - release all temps since mark (only frees if refcount reaches 0)
    writeln!(output, "void qbs_cleanup(uint32_t base, int dummy) {{").unwrap();
    writeln!(output, "    (void)dummy;").unwrap();
    writeln!(output, "    while (_qbs_tmp_next > base) {{").unwrap();
    writeln!(output, "        _qbs_tmp_next--;").unwrap();
    writeln!(
        output,
        "        qb_string* s = _qbs_tmp_pool[_qbs_tmp_next];"
    )
    .unwrap();
    writeln!(output, "        _qbs_tmp_pool[_qbs_tmp_next] = NULL;").unwrap();
    writeln!(
        output,
        "        if (s && s != &_qbs_empty && s->refcount > 0) {{"
    )
    .unwrap();
    writeln!(output, "            s->refcount--;").unwrap();
    writeln!(output, "            if (s->refcount == 0) {{").unwrap();
    writeln!(output, "                free(s->data);").unwrap();
    writeln!(output, "                free(s);").unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits core string allocation and manipulation functions.
///
/// This includes:
/// - `qb_string_new` - Create string from C string
/// - `qb_string_new_len` - Create string with specified length (space-filled)
/// - `qb_string_free` - Deallocate string
/// - `qb_string_retain` - Increment reference count
/// - `qb_string_release` - Decrement reference count and free if zero
/// - `qb_string_concat` - Concatenate two strings
/// - `qb_string_data` - Get raw C string pointer
pub(super) fn emit_string_functions(output: &mut String) {
    // _qbs_empty is already defined in emit_temp_string_pool

    // String creation from null-terminated C string
    // Optimization: return static empty string for empty input
    // All new strings are registered as temps - with scoped cleanup, each loop/function
    // only cleans its own temps (from its saved base position forward), so strings
    // created by callers are preserved.
    writeln!(output, "qb_string* qb_string_new(const char* s) {{").unwrap();
    writeln!(output, "    if (!s || s[0] == '\\0') return &_qbs_empty;").unwrap();
    writeln!(output, "    qb_string* str = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    str->len = strlen(s);").unwrap();
    writeln!(output, "    str->capacity = str->len + 1;").unwrap();
    writeln!(output, "    str->data = malloc(str->capacity);").unwrap();
    writeln!(output, "    memcpy(str->data, s, str->len + 1);").unwrap();
    writeln!(output, "    str->refcount = 1;").unwrap();
    writeln!(output, "    return qbs_tmp_register(str);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // String creation with specified length (for FIELD statement)
    writeln!(output, "qb_string* qb_string_new_len(size_t len) {{").unwrap();
    writeln!(output, "    qb_string* str = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    str->len = len;").unwrap();
    writeln!(output, "    str->capacity = len + 1;").unwrap();
    writeln!(output, "    str->data = malloc(str->capacity);").unwrap();
    writeln!(output, "    memset(str->data, ' ', len);").unwrap();
    writeln!(output, "    str->data[len] = '\\0';").unwrap();
    writeln!(output, "    str->refcount = 1;").unwrap();
    writeln!(output, "    return str;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // String deallocation - skip static empty string
    writeln!(output, "void qb_string_free(qb_string* s) {{").unwrap();
    writeln!(
        output,
        "    if (s && s != &_qbs_empty) {{ free(s->data); free(s); }}"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Reference counting - retain
    // Skip static empty string as it's never freed
    writeln!(output, "qb_string* qb_string_retain(qb_string* s) {{").unwrap();
    writeln!(output, "    if (s && s != &_qbs_empty) s->refcount++;").unwrap();
    writeln!(output, "    return s;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Reference counting - release
    // Skip static empty string to avoid decrementing its refcount
    writeln!(output, "void qb_string_release(qb_string* s) {{").unwrap();
    writeln!(output, "    if (s && s != &_qbs_empty) {{").unwrap();
    writeln!(output, "        s->refcount--;").unwrap();
    writeln!(output, "        if (s->refcount <= 0) {{").unwrap();
    writeln!(output, "            free(s->data);").unwrap();
    writeln!(output, "            free(s);").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // String concatenation (with defensive checks for corrupted strings)
    writeln!(
        output,
        "qb_string* qb_string_concat(qb_string* a, qb_string* b) {{"
    )
    .unwrap();
    // Check for NULL pointers or corrupted data
    writeln!(
        output,
        "    int a_valid = a && a->data && a->len < 0x10000000;"
    )
    .unwrap();
    writeln!(
        output,
        "    int b_valid = b && b->data && b->len < 0x10000000;"
    )
    .unwrap();
    writeln!(output, "    if (!a_valid && !b_valid) return &_qbs_empty;").unwrap();
    writeln!(output, "    if (!a_valid) return qb_string_new(b->data);").unwrap();
    writeln!(output, "    if (!b_valid) return qb_string_new(a->data);").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = a->len + b->len;").unwrap();
    writeln!(output, "    result->capacity = result->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    memcpy(result->data, a->data, a->len);").unwrap();
    writeln!(
        output,
        "    memcpy(result->data + a->len, b->data, b->len + 1);"
    )
    .unwrap();
    writeln!(output, "    return qbs_tmp_register(result);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // String data accessor - returns char* for C interop
    writeln!(output, "const char* qb_string_data(qb_string* s) {{").unwrap();
    writeln!(output, "    return s ? s->data : \"\";").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits the string comparison function.
///
/// Generates `qb_string_compare` which performs lexicographic comparison
/// of two strings, treating NULL as empty string (BASIC semantics).
///
/// Uses memcmp instead of strcmp to properly handle binary strings
/// (like those created by MKL$) that may contain embedded null bytes.
///
/// Returns: negative if a < b, zero if a == b, positive if a > b
pub(super) fn emit_string_comparison(output: &mut String) {
    writeln!(
        output,
        "int qb_string_compare(qb_string* a, qb_string* b) {{"
    )
    .unwrap();
    // Treat NULL as empty string (BASIC semantics)
    // Use memcmp with actual lengths to handle binary strings with embedded nulls
    writeln!(output, "    size_t a_len = (a && a->data) ? a->len : 0;").unwrap();
    writeln!(output, "    size_t b_len = (b && b->data) ? b->len : 0;").unwrap();
    writeln!(
        output,
        "    const char* a_data = (a && a->data) ? a->data : \"\";"
    )
    .unwrap();
    writeln!(
        output,
        "    const char* b_data = (b && b->data) ? b->data : \"\";"
    )
    .unwrap();
    // Compare using the shorter length first
    writeln!(
        output,
        "    size_t min_len = (a_len < b_len) ? a_len : b_len;"
    )
    .unwrap();
    writeln!(output, "    int cmp = memcmp(a_data, b_data, min_len);").unwrap();
    // If equal for the common prefix, longer string is greater
    writeln!(output, "    if (cmp == 0) {{").unwrap();
    writeln!(output, "        if (a_len < b_len) return -1;").unwrap();
    writeln!(output, "        if (a_len > b_len) return 1;").unwrap();
    writeln!(output, "        return 0;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return cmp;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits string manipulation functions for BASIC built-ins.
///
/// This includes:
/// - `qb_left` - LEFT$(s$, n)
/// - `qb_right` - RIGHT$(s$, n)
/// - `qb_mid` - MID$(s$, start, len) - 3-argument form
/// - `qb_mid2` - MID$(s$, start) - 2-argument form
/// - `qb_mid_assign` - MID$ statement for in-place replacement
/// - `qb_instr` - INSTR(start, s$, find$)
/// - `qb_instr2` - INSTR(s$, find$)
/// - `qb_str` - STR$(n)
/// - `qb_val` - VAL(s$)
/// - `qb_ucase` - UCASE$(s$)
/// - `qb_lcase` - LCASE$(s$)
/// - `qb_ltrim` - LTRIM$(s$)
/// - `qb_rtrim` - RTRIM$(s$)
/// - `qb_space` - SPACE$(n)
/// - `qb_string_fill` - STRING$(n, char$)
/// - `qb_string_fill_code` - STRING$(n, code)
/// - UTF-8 helper functions for Unicode support
pub(super) fn emit_string_manipulation(output: &mut String) {
    // LEFT$(s$, n)
    writeln!(output, "qb_string* qb_left(qb_string* s, int32_t n) {{").unwrap();
    writeln!(output, "    if (!s || n <= 0) return &_qbs_empty;").unwrap();
    writeln!(
        output,
        "    size_t len = (size_t)n < s->len ? (size_t)n : s->len;"
    )
    .unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = len;").unwrap();
    writeln!(output, "    result->capacity = len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memcpy(result->data, s->data, len);").unwrap();
    writeln!(output, "    result->data[len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // RIGHT$(s$, n)
    writeln!(output, "qb_string* qb_right(qb_string* s, int32_t n) {{").unwrap();
    writeln!(output, "    if (!s || n <= 0) return &_qbs_empty;").unwrap();
    writeln!(
        output,
        "    size_t len = (size_t)n < s->len ? (size_t)n : s->len;"
    )
    .unwrap();
    writeln!(output, "    size_t start = s->len - len;").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = len;").unwrap();
    writeln!(output, "    result->capacity = len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memcpy(result->data, s->data + start, len);").unwrap();
    writeln!(output, "    result->data[len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MID$(s$, start, len) - 1-based index
    writeln!(
        output,
        "qb_string* qb_mid(qb_string* s, int32_t start, int32_t n) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!s || start < 1 || n <= 0 || (size_t)start > s->len) return &_qbs_empty;"
    )
    .unwrap();
    writeln!(output, "    size_t idx = (size_t)(start - 1);").unwrap();
    writeln!(
        output,
        "    size_t len = (idx + (size_t)n > s->len) ? s->len - idx : (size_t)n;"
    )
    .unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = len;").unwrap();
    writeln!(output, "    result->capacity = len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memcpy(result->data, s->data + idx, len);").unwrap();
    writeln!(output, "    result->data[len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MID$(s$, start) - 2-argument form returns from start to end
    writeln!(output, "qb_string* qb_mid2(qb_string* s, int32_t start) {{").unwrap();
    writeln!(
        output,
        "    if (!s || start < 1 || (size_t)start > s->len) return &_qbs_empty;"
    )
    .unwrap();
    writeln!(output, "    size_t idx = (size_t)(start - 1);").unwrap();
    writeln!(output, "    size_t len = s->len - idx;").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = len;").unwrap();
    writeln!(output, "    result->capacity = len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memcpy(result->data, s->data + idx, len);").unwrap();
    writeln!(output, "    result->data[len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MID$ statement - in-place substring replacement
    // MID$(str$, start [, length]) = value$
    // Replaces up to 'length' characters starting at 'start' (1-based)
    // If length is -1, replace up to min(remaining length, value length)
    writeln!(
        output,
        "void qb_mid_assign(qb_string** target, int32_t start, int32_t length, qb_string* value) {{"
    )
    .unwrap();
    writeln!(output, "    if (!target || !*target || !value) return;").unwrap();
    writeln!(output, "    qb_string* s = *target;").unwrap();
    writeln!(
        output,
        "    if (start < 1 || (size_t)start > s->len) return;"
    )
    .unwrap();
    writeln!(output, "    size_t idx = (size_t)(start - 1);").unwrap();
    writeln!(output, "    size_t max_len = s->len - idx;").unwrap();
    writeln!(output, "    size_t replace_len;").unwrap();
    writeln!(output, "    if (length < 0) {{").unwrap();
    writeln!(
        output,
        "        replace_len = (value->len < max_len) ? value->len : max_len;"
    )
    .unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(
        output,
        "        replace_len = ((size_t)length < max_len) ? (size_t)length : max_len;"
    )
    .unwrap();
    writeln!(
        output,
        "        if (replace_len > value->len) replace_len = value->len;"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    memcpy(s->data + idx, value->data, replace_len);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // INSTR(start, s$, find$) - 1-based, returns 0 if not found
    writeln!(
        output,
        "int32_t qb_instr(int32_t start, qb_string* s, qb_string* find) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!s || !find || start < 1 || (size_t)start > s->len || find->len == 0) return 0;"
    )
    .unwrap();
    writeln!(
        output,
        "    char* pos = strstr(s->data + start - 1, find->data);"
    )
    .unwrap();
    writeln!(output, "    return pos ? (int32_t)(pos - s->data + 1) : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // INSTR(s$, find$) - 2-argument form, starts at beginning
    writeln!(
        output,
        "int32_t qb_instr2(qb_string* s, qb_string* find) {{"
    )
    .unwrap();
    writeln!(output, "    return qb_instr(1, s, find);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // STR$(n) - convert number to string
    writeln!(output, "qb_string* qb_str(double n) {{").unwrap();
    writeln!(output, "    char buf[64];").unwrap();
    writeln!(output, "    snprintf(buf, sizeof(buf), \" %g\", n);").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // VAL(s$) - convert string to number
    writeln!(output, "double qb_val(qb_string* s) {{").unwrap();
    writeln!(output, "    return (s && s->data) ? atof(s->data) : 0.0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // UCASE$(s$) - UTF-8 safe: only converts ASCII a-z to A-Z
    // Multi-byte UTF-8 sequences are preserved unchanged
    writeln!(output, "qb_string* qb_ucase(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s) return &_qbs_empty;").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = s->len;").unwrap();
    writeln!(output, "    result->capacity = s->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    for (size_t i = 0; i < s->len; i++) {{").unwrap();
    writeln!(
        output,
        "        unsigned char c = (unsigned char)s->data[i];"
    )
    .unwrap();
    writeln!(
        output,
        "        // Only convert ASCII lowercase (single-byte chars)"
    )
    .unwrap();
    writeln!(output, "        if (c >= 'a' && c <= 'z') {{").unwrap();
    writeln!(output, "            result->data[i] = c - 32;").unwrap();
    writeln!(output, "        }} else {{").unwrap();
    writeln!(output, "            result->data[i] = s->data[i];").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    result->data[s->len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // LCASE$(s$) - UTF-8 safe: only converts ASCII A-Z to a-z
    // Multi-byte UTF-8 sequences are preserved unchanged
    writeln!(output, "qb_string* qb_lcase(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s) return &_qbs_empty;").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = s->len;").unwrap();
    writeln!(output, "    result->capacity = s->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    for (size_t i = 0; i < s->len; i++) {{").unwrap();
    writeln!(
        output,
        "        unsigned char c = (unsigned char)s->data[i];"
    )
    .unwrap();
    writeln!(
        output,
        "        // Only convert ASCII uppercase (single-byte chars)"
    )
    .unwrap();
    writeln!(output, "        if (c >= 'A' && c <= 'Z') {{").unwrap();
    writeln!(output, "            result->data[i] = c + 32;").unwrap();
    writeln!(output, "        }} else {{").unwrap();
    writeln!(output, "            result->data[i] = s->data[i];").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    result->data[s->len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // LTRIM$(s$)
    writeln!(output, "qb_string* qb_ltrim(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s) return &_qbs_empty;").unwrap();
    writeln!(output, "    size_t start = 0;").unwrap();
    writeln!(
        output,
        "    while (start < s->len && s->data[start] == ' ') start++;"
    )
    .unwrap();
    writeln!(output, "    return qb_right(s, (int32_t)(s->len - start));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // RTRIM$(s$)
    writeln!(output, "qb_string* qb_rtrim(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s) return &_qbs_empty;").unwrap();
    writeln!(output, "    size_t end = s->len;").unwrap();
    writeln!(
        output,
        "    while (end > 0 && s->data[end - 1] == ' ') end--;"
    )
    .unwrap();
    writeln!(output, "    return qb_left(s, (int32_t)end);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // SPACE$(n)
    writeln!(output, "qb_string* qb_space(int32_t n) {{").unwrap();
    writeln!(output, "    if (n <= 0) return &_qbs_empty;").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = (size_t)n;").unwrap();
    writeln!(output, "    result->capacity = result->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memset(result->data, ' ', result->len);").unwrap();
    writeln!(output, "    result->data[result->len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // STRING$(n, char$)
    writeln!(
        output,
        "qb_string* qb_string_fill(int32_t n, qb_string* c) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (n <= 0 || !c || c->len == 0) return &_qbs_empty;"
    )
    .unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = (size_t)n;").unwrap();
    writeln!(output, "    result->capacity = result->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memset(result->data, c->data[0], result->len);").unwrap();
    writeln!(output, "    result->data[result->len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // STRING$(n, code) - fill with ASCII code
    writeln!(
        output,
        "qb_string* qb_string_fill_code(int32_t n, int32_t code) {{"
    )
    .unwrap();
    writeln!(output, "    if (n <= 0) return &_qbs_empty;").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = (size_t)n;").unwrap();
    writeln!(output, "    result->capacity = result->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(
        output,
        "    memset(result->data, (unsigned char)code, result->len);"
    )
    .unwrap();
    writeln!(output, "    result->data[result->len] = '\\0';").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // UTF-8 helper: Count UTF-8 characters (codepoints) in a string
    // Returns the number of characters, not bytes
    writeln!(output, "// UTF-8 helper functions").unwrap();
    writeln!(output, "size_t qb_utf8_char_count(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s) return 0;").unwrap();
    writeln!(output, "    size_t count = 0;").unwrap();
    writeln!(output, "    for (size_t i = 0; i < s->len; ) {{").unwrap();
    writeln!(
        output,
        "        unsigned char c = (unsigned char)s->data[i];"
    )
    .unwrap();
    writeln!(
        output,
        "        if ((c & 0x80) == 0) i += 1;        // ASCII"
    )
    .unwrap();
    writeln!(
        output,
        "        else if ((c & 0xE0) == 0xC0) i += 2; // 2-byte"
    )
    .unwrap();
    writeln!(
        output,
        "        else if ((c & 0xF0) == 0xE0) i += 3; // 3-byte"
    )
    .unwrap();
    writeln!(
        output,
        "        else if ((c & 0xF8) == 0xF0) i += 4; // 4-byte"
    )
    .unwrap();
    writeln!(output, "        else i += 1; // Invalid UTF-8, skip byte").unwrap();
    writeln!(output, "        count++;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return count;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // UTF-8 helper: Convert character position to byte position
    // Returns byte offset for the n-th character (1-based)
    writeln!(
        output,
        "size_t qb_utf8_char_to_byte(qb_string* s, size_t char_pos) {{"
    )
    .unwrap();
    writeln!(output, "    if (!s || char_pos < 1) return 0;").unwrap();
    writeln!(output, "    size_t byte_pos = 0;").unwrap();
    writeln!(output, "    size_t char_count = 0;").unwrap();
    writeln!(
        output,
        "    while (byte_pos < s->len && char_count < char_pos - 1) {{"
    )
    .unwrap();
    writeln!(
        output,
        "        unsigned char c = (unsigned char)s->data[byte_pos];"
    )
    .unwrap();
    writeln!(output, "        if ((c & 0x80) == 0) byte_pos += 1;").unwrap();
    writeln!(
        output,
        "        else if ((c & 0xE0) == 0xC0) byte_pos += 2;"
    )
    .unwrap();
    writeln!(
        output,
        "        else if ((c & 0xF0) == 0xE0) byte_pos += 3;"
    )
    .unwrap();
    writeln!(
        output,
        "        else if ((c & 0xF8) == 0xF0) byte_pos += 4;"
    )
    .unwrap();
    writeln!(output, "        else byte_pos += 1;").unwrap();
    writeln!(output, "        char_count++;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return byte_pos;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _STRLEN - Returns character count (not byte count like LEN)
    // This is a QB64Fresh extension for Unicode-aware string length
    writeln!(output, "int32_t qb_strlen_chars(qb_string* s) {{").unwrap();
    writeln!(output, "    return (int32_t)qb_utf8_char_count(s);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
