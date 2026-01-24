//! File I/O Operations for C Runtime
//!
//! This module contains C runtime code generation for file I/O operations,
//! including file handle management, sequential I/O (PRINT #, INPUT #, LINE INPUT #),
//! binary I/O (GET, PUT), random access (FIELD, LSET, RSET), and file functions
//! (EOF, LOF, LOC, FREEFILE).
//!
//! The generated C code implements:
//! - File handle table (up to 511 files as per QB64 convention)
//! - Path normalization for cross-platform compatibility
//! - Sequential file operations (open, close, print, input, write)
//! - Binary/random access operations (seek, get, put)
//! - FIELD statement support for random access files
//! - LSET/RSET for fixed-length string assignment

use std::fmt::Write;

/// Emits C code for file I/O runtime functions.
///
/// This generates the complete file I/O subsystem including:
/// - File handle table management
/// - Path normalization (Unix backslash-to-forward-slash conversion)
/// - File open/close operations
/// - PRINT # (formatted output)
/// - WRITE # (comma-separated, quoted output)
/// - INPUT # (formatted input)
/// - LINE INPUT # (line-based input)
/// - Binary operations (SEEK, GET, PUT)
/// - FIELD, LSET, RSET for random access
/// - EOF, LOF, LOC, FREEFILE functions
pub(super) fn emit_file_io_functions(output: &mut String) {
    writeln!(output, "/* File I/O Functions */").unwrap();
    writeln!(output).unwrap();

    // File handle table (max 511 files as per QB64)
    writeln!(output, "#define QB_MAX_FILES 512").unwrap();
    writeln!(output, "static FILE* _qb_files[QB_MAX_FILES];").unwrap();
    writeln!(output, "static int32_t _qb_file_reclen[QB_MAX_FILES];").unwrap();
    writeln!(output).unwrap();

    // Network I/O function declarations (implemented in Rust runtime)
    // Negative file numbers indicate network handles
    writeln!(
        output,
        "/* Network I/O Functions (extern - from Rust runtime) */"
    )
    .unwrap();
    writeln!(
        output,
        "extern size_t qb_net_get(int64_t handle, uint8_t* data, size_t size);"
    )
    .unwrap();
    writeln!(
        output,
        "extern size_t qb_net_put(int64_t handle, const uint8_t* data, size_t size);"
    )
    .unwrap();
    writeln!(
        output,
        "extern size_t qb_net_get_string(int64_t handle, qb_string* s);"
    )
    .unwrap();
    writeln!(
        output,
        "extern size_t qb_net_put_string(int64_t handle, const qb_string* s);"
    )
    .unwrap();
    writeln!(output, "extern int32_t qb_net_eof(int64_t handle);").unwrap();
    writeln!(output, "extern int64_t qb_net_lof(int64_t handle);").unwrap();
    writeln!(output, "extern void qb_net_close(int64_t handle);").unwrap();
    writeln!(output).unwrap();

    // _qb_file_set - internal function to set file handle
    writeln!(output, "static void _qb_file_set(int32_t fnum, FILE* f) {{").unwrap();
    writeln!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;").unwrap();
    writeln!(output, "    if (_qb_files[fnum]) fclose(_qb_files[fnum]);").unwrap();
    writeln!(output, "    _qb_files[fnum] = f;").unwrap();
    writeln!(output, "    _qb_file_reclen[fnum] = 128;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Helper to normalize path separators on non-Windows systems
    writeln!(output, "#ifndef _WIN32").unwrap();
    writeln!(
        output,
        "static char* _qb_normalize_path(const char* path) {{"
    )
    .unwrap();
    writeln!(output, "    if (!path) return NULL;").unwrap();
    writeln!(output, "    size_t len = strlen(path);").unwrap();
    writeln!(output, "    char* normalized = malloc(len + 1);").unwrap();
    writeln!(output, "    for (size_t i = 0; i <= len; i++) {{").unwrap();
    writeln!(
        output,
        "        normalized[i] = (path[i] == '\\\\') ? '/' : path[i];"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return normalized;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // qb_file_open - Open a file
    writeln!(
        output,
        "void qb_file_open(int32_t fnum, const char* filename, const char* mode) {{"
    )
    .unwrap();
    writeln!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;").unwrap();
    writeln!(output, "    if (_qb_files[fnum]) fclose(_qb_files[fnum]);").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    _qb_files[fnum] = fopen(filename, mode);").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    _qb_files[fnum] = fopen(filename, mode);").unwrap();
    writeln!(output, "    if (!_qb_files[fnum]) {{").unwrap();
    writeln!(
        output,
        "        char* normalized = _qb_normalize_path(filename);"
    )
    .unwrap();
    writeln!(
        output,
        "        if (normalized) {{ _qb_files[fnum] = fopen(normalized, mode); }}"
    )
    .unwrap();
    writeln!(output, "        free(normalized);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(
        output,
        "    _qb_file_reclen[fnum] = 128; /* default record length */"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_set_reclen - Set record length for random access
    writeln!(
        output,
        "void qb_file_set_reclen(int32_t fnum, int32_t len) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES) _qb_file_reclen[fnum] = len;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_close - Close a file or network connection
    writeln!(output, "void qb_file_close(int32_t fnum) {{").unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(output, "        qb_net_close((int64_t)fnum);").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum]) {{"
    )
    .unwrap();
    writeln!(output, "        fclose(_qb_files[fnum]);").unwrap();
    writeln!(output, "        _qb_files[fnum] = NULL;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_close_all - Close all files
    writeln!(output, "void qb_file_close_all(void) {{").unwrap();
    writeln!(output, "    for (int i = 1; i < QB_MAX_FILES; i++) {{").unwrap();
    writeln!(
        output,
        "        if (_qb_files[i]) {{ fclose(_qb_files[i]); _qb_files[i] = NULL; }}"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_print_* - Print to file
    writeln!(
        output,
        "void qb_file_print_int(int32_t fnum, int64_t val) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(
        output,
        "        fprintf(_qb_files[fnum], \"%lld\", (long long)val);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_print_float(int32_t fnum, double val) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fprintf(_qb_files[fnum], \"%g\", val);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_print_string(int32_t fnum, qb_string* s) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum] && s)"
    )
    .unwrap();
    writeln!(output, "        fprintf(_qb_files[fnum], \"%s\", s->data);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_file_print_newline(int32_t fnum) {{").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fprintf(_qb_files[fnum], \"\\n\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_file_print_tab(int32_t fnum) {{").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fprintf(_qb_files[fnum], \"\\t\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_write_* - WRITE # functions (quoted strings, comma-separated)
    writeln!(
        output,
        "void qb_file_write_string(int32_t fnum, qb_string* s) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum] && s)"
    )
    .unwrap();
    writeln!(
        output,
        "        fprintf(_qb_files[fnum], \"\\\"%s\\\"\", s->data);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_write_number(int32_t fnum, double val) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fprintf(_qb_files[fnum], \"%g\", val);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_file_write_char(int32_t fnum, char c) {{").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fputc(c, _qb_files[fnum]);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_input_* - INPUT # functions
    writeln!(
        output,
        "void qb_file_input_string(int32_t fnum, qb_string** s) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )
    .unwrap();
    writeln!(output, "    char buf[4096];").unwrap();
    writeln!(output, "    int ch, i = 0;").unwrap();
    writeln!(output, "    while ((ch = fgetc(_qb_files[fnum])) != EOF && ch != ',' && ch != '\\n' && i < 4095) {{").unwrap();
    writeln!(output, "        if (ch == '\"') {{ /* skip quotes */").unwrap();
    writeln!(
        output,
        "            while ((ch = fgetc(_qb_files[fnum])) != EOF && ch != '\"' && i < 4095)"
    )
    .unwrap();
    writeln!(output, "                buf[i++] = (char)ch;").unwrap();
    writeln!(output, "        }} else buf[i++] = (char)ch;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    buf[i] = '\\0';").unwrap();
    writeln!(output, "    *s = qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_input_int(int32_t fnum, int32_t* val) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )
    .unwrap();
    writeln!(output, "    fscanf(_qb_files[fnum], \"%d\", val);").unwrap();
    writeln!(
        output,
        "    int ch; while ((ch = fgetc(_qb_files[fnum])) == ',' || ch == ' ');"
    )
    .unwrap();
    writeln!(
        output,
        "    if (ch != EOF && ch != '\\n') ungetc(ch, _qb_files[fnum]);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_input_float(int32_t fnum, double* val) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )
    .unwrap();
    writeln!(output, "    fscanf(_qb_files[fnum], \"%lf\", val);").unwrap();
    writeln!(
        output,
        "    int ch; while ((ch = fgetc(_qb_files[fnum])) == ',' || ch == ' ');"
    )
    .unwrap();
    writeln!(
        output,
        "    if (ch != EOF && ch != '\\n') ungetc(ch, _qb_files[fnum]);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_line_input - LINE INPUT #
    writeln!(
        output,
        "void qb_file_line_input(int32_t fnum, qb_string** s) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )
    .unwrap();
    writeln!(output, "    char buf[4096];").unwrap();
    writeln!(
        output,
        "    if (fgets(buf, sizeof(buf), _qb_files[fnum])) {{"
    )
    .unwrap();
    writeln!(output, "        size_t len = strlen(buf);").unwrap();
    writeln!(
        output,
        "        if (len > 0 && buf[len-1] == '\\n') buf[--len] = '\\0';"
    )
    .unwrap();
    writeln!(output, "        *s = qb_string_new(buf);").unwrap();
    writeln!(output, "    }} else *s = qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Binary file operations
    writeln!(output, "void qb_file_seek(int32_t fnum, int64_t pos) {{").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(
        output,
        "        fseek(_qb_files[fnum], (long)(pos - 1), SEEK_SET);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_seek_record(int32_t fnum, int64_t rec) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(
        output,
        "        fseek(_qb_files[fnum], (long)((rec - 1) * _qb_file_reclen[fnum]), SEEK_SET);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_get(int32_t fnum, void* data, size_t size) {{"
    )
    .unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(
        output,
        "        qb_net_get((int64_t)fnum, (uint8_t*)data, size);"
    )
    .unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fread(data, 1, size, _qb_files[fnum]);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_get_string - Read into a string's data buffer
    writeln!(
        output,
        "void qb_file_get_string(int32_t fnum, qb_string* s) {{"
    )
    .unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(output, "        qb_net_get_string((int64_t)fnum, s);").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )
    .unwrap();
    writeln!(output, "    if (!s || !s->data || s->len == 0) return;").unwrap();
    writeln!(output, "    fread(s->data, 1, s->len, _qb_files[fnum]);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_file_put_string - Write from a string's data buffer
    writeln!(
        output,
        "void qb_file_put_string(int32_t fnum, qb_string* s) {{"
    )
    .unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(output, "        qb_net_put_string((int64_t)fnum, s);").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )
    .unwrap();
    writeln!(output, "    if (!s || !s->data || s->len == 0) return;").unwrap();
    writeln!(output, "    fwrite(s->data, 1, s->len, _qb_files[fnum]);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_file_put(int32_t fnum, const void* data, size_t size) {{"
    )
    .unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(
        output,
        "        qb_net_put((int64_t)fnum, (const uint8_t*)data, size);"
    )
    .unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        fwrite(data, 1, size, _qb_files[fnum]);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // File functions
    writeln!(output, "int32_t qb_eof(int32_t fnum) {{").unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(output, "        return qb_net_eof((int64_t)fnum);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        return feof(_qb_files[fnum]) ? -1 : 0;").unwrap();
    writeln!(output, "    return -1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int64_t qb_lof(int32_t fnum) {{").unwrap();
    writeln!(output, "    if (fnum < 0) {{").unwrap();
    writeln!(output, "        return qb_net_lof((int64_t)fnum);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return 0;"
    )
    .unwrap();
    writeln!(output, "    long pos = ftell(_qb_files[fnum]);").unwrap();
    writeln!(output, "    fseek(_qb_files[fnum], 0, SEEK_END);").unwrap();
    writeln!(output, "    long len = ftell(_qb_files[fnum]);").unwrap();
    writeln!(output, "    fseek(_qb_files[fnum], pos, SEEK_SET);").unwrap();
    writeln!(output, "    return (int64_t)len;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int64_t qb_loc(int32_t fnum) {{").unwrap();
    writeln!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )
    .unwrap();
    writeln!(
        output,
        "        return (int64_t)ftell(_qb_files[fnum]) + 1;"
    )
    .unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int32_t qb_freefile(void) {{").unwrap();
    writeln!(output, "    for (int i = 1; i < QB_MAX_FILES; i++)").unwrap();
    writeln!(output, "        if (!_qb_files[i]) return i;").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // FIELD statement support
    // Each file can have a field buffer that maps string variables to portions of the buffer
    writeln!(output, "/* FIELD Statement Support */").unwrap();
    writeln!(output, "static char* _qb_field_buffer[QB_MAX_FILES];").unwrap();
    writeln!(output, "static int32_t _qb_field_offset[QB_MAX_FILES];").unwrap();
    writeln!(output).unwrap();

    // qb_field_start - Begin a FIELD statement, allocate buffer
    writeln!(output, "void qb_field_start(int32_t fnum) {{").unwrap();
    writeln!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;").unwrap();
    writeln!(output, "    if (!_qb_field_buffer[fnum]) {{").unwrap();
    writeln!(
        output,
        "        _qb_field_buffer[fnum] = (char*)calloc(_qb_file_reclen[fnum], 1);"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    _qb_field_offset[fnum] = 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_field_add - Add a string variable to the field at current offset
    // The string variable's data pointer is set to point into the field buffer
    writeln!(
        output,
        "void qb_field_add(int32_t width, qb_string** var) {{"
    )
    .unwrap();
    writeln!(output, "    /* Note: FIELD requires special handling").unwrap();
    writeln!(
        output,
        "       In classic BASIC, FIELD maps string variables directly"
    )
    .unwrap();
    writeln!(
        output,
        "       to the file buffer. This is complex with ref-counted strings."
    )
    .unwrap();
    writeln!(
        output,
        "       For now, we create a fixed-length string. */"
    )
    .unwrap();
    writeln!(output, "    if (*var) qb_string_release(*var);").unwrap();
    writeln!(output, "    *var = qb_string_new_len(width);").unwrap();
    writeln!(output, "    memset((*var)->data, ' ', width);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_lset - Left-justify a string value into a fixed-length string variable
    writeln!(output, "void qb_lset(qb_string** var, qb_string* value) {{").unwrap();
    writeln!(output, "    if (!*var || !value) return;").unwrap();
    writeln!(output, "    int32_t var_len = (*var)->len;").unwrap();
    writeln!(output, "    int32_t val_len = value->len;").unwrap();
    writeln!(output, "    /* Fill with spaces first */").unwrap();
    writeln!(output, "    memset((*var)->data, ' ', var_len);").unwrap();
    writeln!(output, "    /* Copy value left-justified */").unwrap();
    writeln!(
        output,
        "    int32_t copy_len = val_len < var_len ? val_len : var_len;"
    )
    .unwrap();
    writeln!(output, "    memcpy((*var)->data, value->data, copy_len);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_rset - Right-justify a string value into a fixed-length string variable
    writeln!(output, "void qb_rset(qb_string** var, qb_string* value) {{").unwrap();
    writeln!(output, "    if (!*var || !value) return;").unwrap();
    writeln!(output, "    int32_t var_len = (*var)->len;").unwrap();
    writeln!(output, "    int32_t val_len = value->len;").unwrap();
    writeln!(output, "    /* Fill with spaces first */").unwrap();
    writeln!(output, "    memset((*var)->data, ' ', var_len);").unwrap();
    writeln!(output, "    /* Copy value right-justified */").unwrap();
    writeln!(
        output,
        "    int32_t copy_len = val_len < var_len ? val_len : var_len;"
    )
    .unwrap();
    writeln!(output, "    int32_t offset = var_len - copy_len;").unwrap();
    writeln!(
        output,
        "    memcpy((*var)->data + offset, value->data, copy_len);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
