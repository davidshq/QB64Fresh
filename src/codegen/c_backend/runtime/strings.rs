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

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits the temporary string pool for automatic cleanup.
///
/// This provides a mechanism similar to QB64pe's qbs_tmp_base/qbs_cleanup pattern
/// to prevent memory leaks from temporary strings created during expression evaluation.
///
/// - `qb_tmp_str_mark()` - Returns current pool position
/// - `qb_tmp_str_register()` - Registers a temp string and returns it
/// - `qb_tmp_str_cleanup()` - Frees all temps since a mark
pub(super) fn emit_temp_string_pool(output: &mut String) -> Result<(), CodeGenError> {
    // Static empty string - defined early so it can be referenced by temp pool
    writeln_code!(output, "/* Static Empty String */")?;
    writeln_code!(output, "static char _qbs_empty_data[1] = {{'\\0'}};")?;
    writeln_code!(
        output,
        "static qb_string _qbs_empty = {{_qbs_empty_data, 0, 1, 999999}};"
    )?;
    writeln_code!(output)?;

    // Temporary string pool for automatic cleanup
    writeln_code!(output, "/* Temporary String Pool */")?;
    writeln_code!(output, "#define QBS_TMP_MAX 16384")?;
    writeln_code!(output, "static qb_string* _qbs_tmp_pool[QBS_TMP_MAX];")?;
    writeln_code!(output, "static uint32_t _qbs_tmp_next = 0;")?;
    writeln_code!(output)?;
    writeln_code!(output, "/* Overflow tracking for when pool is full */")?;
    writeln_code!(output, "#define QBS_TMP_OVERFLOW_MAX 16384")?;
    writeln_code!(
        output,
        "static qb_string* _qbs_tmp_overflow[QBS_TMP_OVERFLOW_MAX];"
    )?;
    writeln_code!(output, "static uint32_t _qbs_tmp_overflow_count = 0;")?;
    writeln_code!(output)?;

    // Mark current position (returns packed: main pool base in lower 32 bits, overflow base in upper 32 bits)
    // This allows scoped cleanup of both main pool and overflow strings
    writeln_code!(output, "uint64_t qbs_tmp_base_get(void) {{")?;
    writeln_code!(output, "    uint64_t main_base = (uint64_t)_qbs_tmp_next;")?;
    writeln_code!(
        output,
        "    uint64_t overflow_base = (uint64_t)_qbs_tmp_overflow_count;"
    )?;
    writeln_code!(output, "    return main_base | (overflow_base << 32);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Register a temporary string - strings start with refcount 1
    // When assigned to a variable, retain() increments to 2
    // When cleanup runs, release() decrements back to 1 (variable still has it)
    // If the main pool is full, track in overflow list to prevent leaks
    writeln_code!(output, "qb_string* qbs_tmp_register(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s == &_qbs_empty) return s;")?;
    writeln_code!(output, "    if (_qbs_tmp_next < QBS_TMP_MAX) {{")?;
    writeln_code!(output, "        _qbs_tmp_pool[_qbs_tmp_next++] = s;")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(
        output,
        "        /* Pool full - track in overflow to prevent leak */"
    )?;
    writeln_code!(
        output,
        "        if (_qbs_tmp_overflow_count < QBS_TMP_OVERFLOW_MAX) {{"
    )?;
    writeln_code!(
        output,
        "            _qbs_tmp_overflow[_qbs_tmp_overflow_count++] = s;"
    )?;
    writeln_code!(output, "        }} else {{")?;
    writeln_code!(
        output,
        "            /* Overflow list also full - extremely rare edge case */"
    )?;
    writeln_code!(
        output,
        "            /* Don't track this string to preserve scoping correctness */"
    )?;
    writeln_code!(
        output,
        "            /* Overwriting any index would break nested scope cleanup */"
    )?;
    writeln_code!(
        output,
        "            /* The string will leak, but this is safer than undefined behavior */"
    )?;
    writeln_code!(
        output,
        "            /* TODO: Consider adding debug logging/warning when this occurs */"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return s;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Cleanup - release all temps since mark (only frees if refcount reaches 0)
    // Also cleans up overflow strings that were registered since the mark
    // base is packed: lower 32 bits = main pool base, upper 32 bits = overflow base
    writeln_code!(output, "void qbs_cleanup(uint64_t base, int dummy) {{")?;
    writeln_code!(output, "    (void)dummy;")?;
    writeln_code!(
        output,
        "    uint32_t main_base = (uint32_t)(base & 0xFFFFFFFF);"
    )?;
    writeln_code!(
        output,
        "    uint32_t overflow_base = (uint32_t)(base >> 32);"
    )?;
    writeln_code!(output, "    while (_qbs_tmp_next > main_base) {{")?;
    writeln_code!(output, "        _qbs_tmp_next--;")?;
    writeln_code!(
        output,
        "        qb_string* s = _qbs_tmp_pool[_qbs_tmp_next];"
    )?;
    writeln_code!(output, "        _qbs_tmp_pool[_qbs_tmp_next] = NULL;")?;
    writeln_code!(
        output,
        "        if (s && s != &_qbs_empty && s->refcount > 0) {{"
    )?;
    writeln_code!(output, "            s->refcount--;")?;
    writeln_code!(output, "            if (s->refcount == 0) {{")?;
    writeln_code!(output, "                free(s->data);")?;
    writeln_code!(output, "                free(s);")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    /* Clean up overflow strings registered since mark */"
    )?;
    writeln_code!(
        output,
        "    while (_qbs_tmp_overflow_count > overflow_base) {{"
    )?;
    writeln_code!(output, "        _qbs_tmp_overflow_count--;")?;
    writeln_code!(
        output,
        "        qb_string* s = _qbs_tmp_overflow[_qbs_tmp_overflow_count];"
    )?;
    writeln_code!(
        output,
        "        _qbs_tmp_overflow[_qbs_tmp_overflow_count] = NULL;"
    )?;
    writeln_code!(
        output,
        "        if (s && s != &_qbs_empty && s->refcount > 0) {{"
    )?;
    writeln_code!(output, "            s->refcount--;")?;
    writeln_code!(output, "            if (s->refcount == 0) {{")?;
    writeln_code!(output, "                free(s->data);")?;
    writeln_code!(output, "                free(s);")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
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
pub(super) fn emit_string_functions(output: &mut String) -> Result<(), CodeGenError> {
    // _qbs_empty is already defined in emit_temp_string_pool

    // String creation from null-terminated C string
    // Optimization: return static empty string for empty input
    // All new strings are registered as temps - with scoped cleanup, each loop/function
    // only cleans its own temps (from its saved base position forward), so strings
    // created by callers are preserved.
    writeln_code!(output, "qb_string* qb_string_new(const char* s) {{")?;
    writeln_code!(output, "    if (!s || s[0] == '\\0') return &_qbs_empty;")?;
    writeln_code!(output, "    qb_string* str = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    str->len = strlen(s);")?;
    writeln_code!(output, "    str->capacity = str->len + 1;")?;
    writeln_code!(output, "    str->data = malloc(str->capacity);")?;
    writeln_code!(output, "    memcpy(str->data, s, str->len + 1);")?;
    writeln_code!(output, "    str->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(str);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // String creation with specified length (for FIELD statement)
    writeln_code!(output, "qb_string* qb_string_new_len(size_t len) {{")?;
    writeln_code!(output, "    qb_string* str = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    str->len = len;")?;
    writeln_code!(output, "    str->capacity = len + 1;")?;
    writeln_code!(output, "    str->data = malloc(str->capacity);")?;
    writeln_code!(output, "    memset(str->data, ' ', len);")?;
    writeln_code!(output, "    str->data[len] = '\\0';")?;
    writeln_code!(output, "    str->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(str);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // String deallocation - skip static empty string
    writeln_code!(output, "void qb_string_free(qb_string* s) {{")?;
    writeln_code!(
        output,
        "    if (s && s != &_qbs_empty) {{ free(s->data); free(s); }}"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Reference counting - retain
    // Skip static empty string as it's never freed
    writeln_code!(output, "qb_string* qb_string_retain(qb_string* s) {{")?;
    writeln_code!(output, "    if (s && s != &_qbs_empty) s->refcount++;")?;
    writeln_code!(output, "    return s;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Reference counting - release
    // Skip static empty string to avoid decrementing its refcount
    writeln_code!(output, "void qb_string_release(qb_string* s) {{")?;
    writeln_code!(output, "    if (s && s != &_qbs_empty) {{")?;
    writeln_code!(output, "        s->refcount--;")?;
    writeln_code!(output, "        if (s->refcount <= 0) {{")?;
    writeln_code!(output, "            free(s->data);")?;
    writeln_code!(output, "            free(s);")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // String concatenation (with defensive checks for corrupted strings)
    writeln_code!(
        output,
        "qb_string* qb_string_concat(qb_string* a, qb_string* b) {{"
    )?;
    // Check for NULL pointers or corrupted data
    writeln_code!(
        output,
        "    int a_valid = a && a->data && a->len < 0x10000000;"
    )?;
    writeln_code!(
        output,
        "    int b_valid = b && b->data && b->len < 0x10000000;"
    )?;
    writeln_code!(output, "    if (!a_valid && !b_valid) return &_qbs_empty;")?;
    writeln_code!(output, "    if (!a_valid) return qb_string_new(b->data);")?;
    writeln_code!(output, "    if (!b_valid) return qb_string_new(a->data);")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = a->len + b->len;")?;
    writeln_code!(output, "    result->capacity = result->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    memcpy(result->data, a->data, a->len);")?;
    writeln_code!(
        output,
        "    memcpy(result->data + a->len, b->data, b->len + 1);"
    )?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // String data accessor - returns char* for C interop
    writeln_code!(output, "const char* qb_string_data(qb_string* s) {{")?;
    writeln_code!(output, "    return s ? s->data : \"\";")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
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
pub(super) fn emit_string_comparison(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(
        output,
        "int qb_string_compare(qb_string* a, qb_string* b) {{"
    )?;
    // Treat NULL as empty string (BASIC semantics)
    // Use memcmp with actual lengths to handle binary strings with embedded nulls
    writeln_code!(output, "    size_t a_len = (a && a->data) ? a->len : 0;")?;
    writeln_code!(output, "    size_t b_len = (b && b->data) ? b->len : 0;")?;
    writeln_code!(
        output,
        "    const char* a_data = (a && a->data) ? a->data : \"\";"
    )?;
    writeln_code!(
        output,
        "    const char* b_data = (b && b->data) ? b->data : \"\";"
    )?;
    // Compare using the shorter length first
    writeln_code!(
        output,
        "    size_t min_len = (a_len < b_len) ? a_len : b_len;"
    )?;
    writeln_code!(output, "    int cmp = memcmp(a_data, b_data, min_len);")?;
    // If equal for the common prefix, longer string is greater
    writeln_code!(output, "    if (cmp == 0) {{")?;
    writeln_code!(output, "        if (a_len < b_len) return -1;")?;
    writeln_code!(output, "        if (a_len > b_len) return 1;")?;
    writeln_code!(output, "        return 0;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return cmp;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
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
pub(super) fn emit_string_manipulation(output: &mut String) -> Result<(), CodeGenError> {
    // LEFT$(s$, n)
    writeln_code!(output, "qb_string* qb_left(qb_string* s, int32_t n) {{")?;
    writeln_code!(output, "    if (!s || n <= 0) return &_qbs_empty;")?;
    writeln_code!(
        output,
        "    size_t len = (size_t)n < s->len ? (size_t)n : s->len;"
    )?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = len;")?;
    writeln_code!(output, "    result->capacity = len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memcpy(result->data, s->data, len);")?;
    writeln_code!(output, "    result->data[len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // RIGHT$(s$, n)
    writeln_code!(output, "qb_string* qb_right(qb_string* s, int32_t n) {{")?;
    writeln_code!(output, "    if (!s || n <= 0) return &_qbs_empty;")?;
    writeln_code!(
        output,
        "    size_t len = (size_t)n < s->len ? (size_t)n : s->len;"
    )?;
    writeln_code!(output, "    size_t start = s->len - len;")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = len;")?;
    writeln_code!(output, "    result->capacity = len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memcpy(result->data, s->data + start, len);")?;
    writeln_code!(output, "    result->data[len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MID$(s$, start, len) - 1-based index
    writeln_code!(
        output,
        "qb_string* qb_mid(qb_string* s, int32_t start, int32_t n) {{"
    )?;
    writeln_code!(
        output,
        "    if (!s || start < 1 || n <= 0 || (size_t)start > s->len) return &_qbs_empty;"
    )?;
    writeln_code!(output, "    size_t idx = (size_t)(start - 1);")?;
    writeln_code!(
        output,
        "    size_t len = (idx + (size_t)n > s->len) ? s->len - idx : (size_t)n;"
    )?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = len;")?;
    writeln_code!(output, "    result->capacity = len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memcpy(result->data, s->data + idx, len);")?;
    writeln_code!(output, "    result->data[len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MID$(s$, start) - 2-argument form returns from start to end
    writeln_code!(output, "qb_string* qb_mid2(qb_string* s, int32_t start) {{")?;
    writeln_code!(
        output,
        "    if (!s || start < 1 || (size_t)start > s->len) return &_qbs_empty;"
    )?;
    writeln_code!(output, "    size_t idx = (size_t)(start - 1);")?;
    writeln_code!(output, "    size_t len = s->len - idx;")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = len;")?;
    writeln_code!(output, "    result->capacity = len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memcpy(result->data, s->data + idx, len);")?;
    writeln_code!(output, "    result->data[len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MID$ statement - in-place substring replacement
    // MID$(str$, start [, length]) = value$
    // Replaces up to 'length' characters starting at 'start' (1-based)
    // If length is -1, replace up to min(remaining length, value length)
    writeln_code!(
        output,
        "void qb_mid_assign(qb_string** target, int32_t start, int32_t length, qb_string* value) {{"
    )?;
    writeln_code!(output, "    if (!target || !*target || !value) return;")?;
    writeln_code!(output, "    qb_string* s = *target;")?;
    writeln_code!(
        output,
        "    if (start < 1 || (size_t)start > s->len) return;"
    )?;
    writeln_code!(output, "    size_t idx = (size_t)(start - 1);")?;
    writeln_code!(output, "    size_t max_len = s->len - idx;")?;
    writeln_code!(output, "    size_t replace_len;")?;
    writeln_code!(output, "    if (length < 0) {{")?;
    writeln_code!(
        output,
        "        replace_len = (value->len < max_len) ? value->len : max_len;"
    )?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(
        output,
        "        replace_len = ((size_t)length < max_len) ? (size_t)length : max_len;"
    )?;
    writeln_code!(
        output,
        "        if (replace_len > value->len) replace_len = value->len;"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    memcpy(s->data + idx, value->data, replace_len);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // INSTR(start, s$, find$) - 1-based, returns 0 if not found
    writeln_code!(
        output,
        "int32_t qb_instr(int32_t start, qb_string* s, qb_string* find) {{"
    )?;
    writeln_code!(
        output,
        "    if (!s || !find || start < 1 || (size_t)start > s->len || find->len == 0) return 0;"
    )?;
    writeln_code!(
        output,
        "    char* pos = strstr(s->data + start - 1, find->data);"
    )?;
    writeln_code!(output, "    return pos ? (int32_t)(pos - s->data + 1) : 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // INSTR(s$, find$) - 2-argument form, starts at beginning
    writeln_code!(
        output,
        "int32_t qb_instr2(qb_string* s, qb_string* find) {{"
    )?;
    writeln_code!(output, "    return qb_instr(1, s, find);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // STR$(n) - convert number to string
    writeln_code!(output, "qb_string* qb_str(double n) {{")?;
    writeln_code!(output, "    char buf[64];")?;
    writeln_code!(output, "    snprintf(buf, sizeof(buf), \" %g\", n);")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // VAL(s$) - convert string to number
    writeln_code!(output, "double qb_val(qb_string* s) {{")?;
    writeln_code!(output, "    return (s && s->data) ? atof(s->data) : 0.0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // UCASE$(s$) - UTF-8 safe: only converts ASCII a-z to A-Z
    // Multi-byte UTF-8 sequences are preserved unchanged
    writeln_code!(output, "qb_string* qb_ucase(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s) return &_qbs_empty;")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = s->len;")?;
    writeln_code!(output, "    result->capacity = s->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    for (size_t i = 0; i < s->len; i++) {{")?;
    writeln_code!(
        output,
        "        unsigned char c = (unsigned char)s->data[i];"
    )?;
    writeln_code!(
        output,
        "        // Only convert ASCII lowercase (single-byte chars)"
    )?;
    writeln_code!(output, "        if (c >= 'a' && c <= 'z') {{")?;
    writeln_code!(output, "            result->data[i] = c - 32;")?;
    writeln_code!(output, "        }} else {{")?;
    writeln_code!(output, "            result->data[i] = s->data[i];")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    result->data[s->len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // LCASE$(s$) - UTF-8 safe: only converts ASCII A-Z to a-z
    // Multi-byte UTF-8 sequences are preserved unchanged
    writeln_code!(output, "qb_string* qb_lcase(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s) return &_qbs_empty;")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = s->len;")?;
    writeln_code!(output, "    result->capacity = s->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    for (size_t i = 0; i < s->len; i++) {{")?;
    writeln_code!(
        output,
        "        unsigned char c = (unsigned char)s->data[i];"
    )?;
    writeln_code!(
        output,
        "        // Only convert ASCII uppercase (single-byte chars)"
    )?;
    writeln_code!(output, "        if (c >= 'A' && c <= 'Z') {{")?;
    writeln_code!(output, "            result->data[i] = c + 32;")?;
    writeln_code!(output, "        }} else {{")?;
    writeln_code!(output, "            result->data[i] = s->data[i];")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    result->data[s->len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // LTRIM$(s$)
    writeln_code!(output, "qb_string* qb_ltrim(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s) return &_qbs_empty;")?;
    writeln_code!(output, "    size_t start = 0;")?;
    writeln_code!(
        output,
        "    while (start < s->len && s->data[start] == ' ') start++;"
    )?;
    writeln_code!(output, "    return qb_right(s, (int32_t)(s->len - start));")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // RTRIM$(s$)
    writeln_code!(output, "qb_string* qb_rtrim(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s) return &_qbs_empty;")?;
    writeln_code!(output, "    size_t end = s->len;")?;
    writeln_code!(
        output,
        "    while (end > 0 && s->data[end - 1] == ' ') end--;"
    )?;
    writeln_code!(output, "    return qb_left(s, (int32_t)end);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // SPACE$(n)
    writeln_code!(output, "qb_string* qb_space(int32_t n) {{")?;
    writeln_code!(output, "    if (n <= 0) return &_qbs_empty;")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = (size_t)n;")?;
    writeln_code!(output, "    result->capacity = result->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memset(result->data, ' ', result->len);")?;
    writeln_code!(output, "    result->data[result->len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // STRING$(n, char$)
    writeln_code!(
        output,
        "qb_string* qb_string_fill(int32_t n, qb_string* c) {{"
    )?;
    writeln_code!(
        output,
        "    if (n <= 0 || !c || c->len == 0) return &_qbs_empty;"
    )?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = (size_t)n;")?;
    writeln_code!(output, "    result->capacity = result->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memset(result->data, c->data[0], result->len);")?;
    writeln_code!(output, "    result->data[result->len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // STRING$(n, code) - fill with ASCII code
    writeln_code!(
        output,
        "qb_string* qb_string_fill_code(int32_t n, int32_t code) {{"
    )?;
    writeln_code!(output, "    if (n <= 0) return &_qbs_empty;")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = (size_t)n;")?;
    writeln_code!(output, "    result->capacity = result->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(
        output,
        "    memset(result->data, (unsigned char)code, result->len);"
    )?;
    writeln_code!(output, "    result->data[result->len] = '\\0';")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    return qbs_tmp_register(result);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // UTF-8 helper: Count UTF-8 characters (codepoints) in a string
    // Returns the number of characters, not bytes
    writeln_code!(output, "// UTF-8 helper functions")?;
    writeln_code!(output, "size_t qb_utf8_char_count(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s) return 0;")?;
    writeln_code!(output, "    size_t count = 0;")?;
    writeln_code!(output, "    for (size_t i = 0; i < s->len; ) {{")?;
    writeln_code!(
        output,
        "        unsigned char c = (unsigned char)s->data[i];"
    )?;
    writeln_code!(
        output,
        "        if ((c & 0x80) == 0) i += 1;        // ASCII"
    )?;
    writeln_code!(
        output,
        "        else if ((c & 0xE0) == 0xC0) i += 2; // 2-byte"
    )?;
    writeln_code!(
        output,
        "        else if ((c & 0xF0) == 0xE0) i += 3; // 3-byte"
    )?;
    writeln_code!(
        output,
        "        else if ((c & 0xF8) == 0xF0) i += 4; // 4-byte"
    )?;
    writeln_code!(output, "        else i += 1; // Invalid UTF-8, skip byte")?;
    writeln_code!(output, "        count++;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return count;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // UTF-8 helper: Convert character position to byte position
    // Returns byte offset for the n-th character (1-based)
    writeln_code!(
        output,
        "size_t qb_utf8_char_to_byte(qb_string* s, size_t char_pos) {{"
    )?;
    writeln_code!(output, "    if (!s || char_pos < 1) return 0;")?;
    writeln_code!(output, "    size_t byte_pos = 0;")?;
    writeln_code!(output, "    size_t char_count = 0;")?;
    writeln_code!(
        output,
        "    while (byte_pos < s->len && char_count < char_pos - 1) {{"
    )?;
    writeln_code!(
        output,
        "        unsigned char c = (unsigned char)s->data[byte_pos];"
    )?;
    writeln_code!(output, "        if ((c & 0x80) == 0) byte_pos += 1;")?;
    writeln_code!(
        output,
        "        else if ((c & 0xE0) == 0xC0) byte_pos += 2;"
    )?;
    writeln_code!(
        output,
        "        else if ((c & 0xF0) == 0xE0) byte_pos += 3;"
    )?;
    writeln_code!(
        output,
        "        else if ((c & 0xF8) == 0xF0) byte_pos += 4;"
    )?;
    writeln_code!(output, "        else byte_pos += 1;")?;
    writeln_code!(output, "        char_count++;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return byte_pos;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _STRLEN - Returns character count (not byte count like LEN)
    // This is a QB64Fresh extension for Unicode-aware string length
    writeln_code!(output, "int32_t qb_strlen_chars(qb_string* s) {{")?;
    writeln_code!(output, "    return (int32_t)qb_utf8_char_count(s);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
