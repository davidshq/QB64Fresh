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

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

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
pub(super) fn emit_file_io_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* File I/O Functions */")?;
    writeln_code!(output)?;

    // File handle table (max 511 files as per QB64)
    writeln_code!(output, "#define QB_MAX_FILES 512")?;
    writeln_code!(output, "static FILE* _qb_files[QB_MAX_FILES];")?;
    writeln_code!(output, "static int32_t _qb_file_reclen[QB_MAX_FILES];")?;
    writeln_code!(output)?;

    // Network I/O function declarations (implemented in Rust runtime)
    // Negative file numbers indicate network handles
    writeln_code!(
        output,
        "/* Network I/O Functions (extern - from Rust runtime) */"
    )?;
    writeln_code!(
        output,
        "extern size_t qb_net_get(int64_t handle, uint8_t* data, size_t size);"
    )?;
    writeln_code!(
        output,
        "extern size_t qb_net_put(int64_t handle, const uint8_t* data, size_t size);"
    )?;
    writeln_code!(
        output,
        "extern size_t qb_net_get_string(int64_t handle, qb_string* s);"
    )?;
    writeln_code!(
        output,
        "extern size_t qb_net_put_string(int64_t handle, const qb_string* s);"
    )?;
    writeln_code!(output, "extern int32_t qb_net_eof(int64_t handle);")?;
    writeln_code!(output, "extern int64_t qb_net_lof(int64_t handle);")?;
    writeln_code!(output, "extern void qb_net_close(int64_t handle);")?;
    writeln_code!(output)?;

    // _qb_file_set - internal function to set file handle
    writeln_code!(output, "static void _qb_file_set(int32_t fnum, FILE* f) {{")?;
    writeln_code!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;")?;
    writeln_code!(output, "    if (_qb_files[fnum]) fclose(_qb_files[fnum]);")?;
    writeln_code!(output, "    _qb_files[fnum] = f;")?;
    writeln_code!(output, "    _qb_file_reclen[fnum] = 128;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Helper to normalize path separators on non-Windows systems
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(
        output,
        "static char* _qb_normalize_path(const char* path) {{"
    )?;
    writeln_code!(output, "    if (!path) return NULL;")?;
    writeln_code!(output, "    size_t len = strlen(path);")?;
    writeln_code!(output, "    char* normalized = malloc(len + 1);")?;
    writeln_code!(output, "    for (size_t i = 0; i <= len; i++) {{")?;
    writeln_code!(
        output,
        "        normalized[i] = (path[i] == '\\\\') ? '/' : path[i];"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return normalized;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // qb_file_open - Open a file
    // Handles path normalization and creating files for "r+b" mode if they don't exist
    writeln_code!(
        output,
        "void qb_file_open(int32_t fnum, const char* filename, const char* mode) {{"
    )?;
    writeln_code!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;")?;
    writeln_code!(output, "    if (_qb_files[fnum]) fclose(_qb_files[fnum]);")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    _qb_files[fnum] = fopen(filename, mode);")?;
    writeln_code!(output, "#else")?;
    // On non-Windows: normalize path first, then try to open
    writeln_code!(
        output,
        "    char* normalized = _qb_normalize_path(filename);"
    )?;
    writeln_code!(
        output,
        "    const char* path_to_use = normalized ? normalized : filename;"
    )?;
    writeln_code!(output, "    _qb_files[fnum] = fopen(path_to_use, mode);")?;
    // If open failed and mode is "r+b" or "r+", create the file first
    writeln_code!(
        output,
        "    if (!_qb_files[fnum] && mode[0] == 'r' && mode[1] == '+') {{"
    )?;
    writeln_code!(
        output,
        "        FILE* tmp = fopen(path_to_use, \"w\"); if (tmp) fclose(tmp);"
    )?;
    writeln_code!(
        output,
        "        _qb_files[fnum] = fopen(path_to_use, mode);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    if (normalized) free(normalized);")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(
        output,
        "    _qb_file_reclen[fnum] = 128; /* default record length */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_set_reclen - Set record length for random access
    writeln_code!(
        output,
        "void qb_file_set_reclen(int32_t fnum, int32_t len) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES) _qb_file_reclen[fnum] = len;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_close - Close a file or network connection
    writeln_code!(output, "void qb_file_close(int32_t fnum) {{")?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(output, "        qb_net_close((int64_t)fnum);")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum]) {{"
    )?;
    writeln_code!(output, "        fclose(_qb_files[fnum]);")?;
    writeln_code!(output, "        _qb_files[fnum] = NULL;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_close_all - Close all files
    writeln_code!(output, "void qb_file_close_all(void) {{")?;
    writeln_code!(output, "    for (int i = 1; i < QB_MAX_FILES; i++) {{")?;
    writeln_code!(
        output,
        "        if (_qb_files[i]) {{ fclose(_qb_files[i]); _qb_files[i] = NULL; }}"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_print_* - Print to file
    writeln_code!(
        output,
        "void qb_file_print_int(int32_t fnum, int64_t val) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(
        output,
        "        fprintf(_qb_files[fnum], \"%lld\", (long long)val);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_print_float(int32_t fnum, double val) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fprintf(_qb_files[fnum], \"%g\", val);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_print_string(int32_t fnum, qb_string* s) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum] && s)"
    )?;
    writeln_code!(output, "        fprintf(_qb_files[fnum], \"%s\", s->data);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_file_print_newline(int32_t fnum) {{")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fprintf(_qb_files[fnum], \"\\n\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_file_print_tab(int32_t fnum) {{")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fprintf(_qb_files[fnum], \"\\t\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_write_* - WRITE # functions (quoted strings, comma-separated)
    writeln_code!(
        output,
        "void qb_file_write_string(int32_t fnum, qb_string* s) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum] && s)"
    )?;
    writeln_code!(
        output,
        "        fprintf(_qb_files[fnum], \"\\\"%s\\\"\", s->data);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_write_number(int32_t fnum, double val) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fprintf(_qb_files[fnum], \"%g\", val);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_file_write_char(int32_t fnum, char c) {{")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fputc(c, _qb_files[fnum]);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_input_* - INPUT # functions
    writeln_code!(
        output,
        "void qb_file_input_string(int32_t fnum, qb_string** s) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )?;
    writeln_code!(output, "    char buf[4096];")?;
    writeln_code!(output, "    int ch, i = 0;")?;
    writeln_code!(
        output,
        "    while ((ch = fgetc(_qb_files[fnum])) != EOF && ch != ',' && ch != '\\n' && i < 4095) {{"
    )?;
    writeln_code!(output, "        if (ch == '\"') {{ /* skip quotes */")?;
    writeln_code!(
        output,
        "            while ((ch = fgetc(_qb_files[fnum])) != EOF && ch != '\"' && i < 4095)"
    )?;
    writeln_code!(output, "                buf[i++] = (char)ch;")?;
    writeln_code!(output, "        }} else buf[i++] = (char)ch;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    buf[i] = '\\0';")?;
    writeln_code!(output, "    *s = qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_input_int(int32_t fnum, int32_t* val) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )?;
    writeln_code!(output, "    fscanf(_qb_files[fnum], \"%d\", val);")?;
    writeln_code!(
        output,
        "    int ch; while ((ch = fgetc(_qb_files[fnum])) == ',' || ch == ' ');"
    )?;
    writeln_code!(
        output,
        "    if (ch != EOF && ch != '\\n') ungetc(ch, _qb_files[fnum]);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_input_float(int32_t fnum, double* val) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )?;
    writeln_code!(output, "    fscanf(_qb_files[fnum], \"%lf\", val);")?;
    writeln_code!(
        output,
        "    int ch; while ((ch = fgetc(_qb_files[fnum])) == ',' || ch == ' ');"
    )?;
    writeln_code!(
        output,
        "    if (ch != EOF && ch != '\\n') ungetc(ch, _qb_files[fnum]);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_line_input - LINE INPUT #
    writeln_code!(
        output,
        "void qb_file_line_input(int32_t fnum, qb_string** s) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )?;
    writeln_code!(output, "    char buf[4096];")?;
    writeln_code!(
        output,
        "    if (fgets(buf, sizeof(buf), _qb_files[fnum])) {{"
    )?;
    writeln_code!(output, "        size_t len = strlen(buf);")?;
    writeln_code!(
        output,
        "        if (len > 0 && buf[len-1] == '\\n') buf[--len] = '\\0';"
    )?;
    writeln_code!(output, "        *s = qb_string_new(buf);")?;
    writeln_code!(output, "    }} else *s = qb_string_new(\"\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Binary file operations
    writeln_code!(output, "void qb_file_seek(int32_t fnum, int64_t pos) {{")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(
        output,
        "        fseek(_qb_files[fnum], (long)(pos - 1), SEEK_SET);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_seek_record(int32_t fnum, int64_t rec) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(
        output,
        "        fseek(_qb_files[fnum], (long)((rec - 1) * _qb_file_reclen[fnum]), SEEK_SET);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_get(int32_t fnum, void* data, size_t size) {{"
    )?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(
        output,
        "        qb_net_get((int64_t)fnum, (uint8_t*)data, size);"
    )?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fread(data, 1, size, _qb_files[fnum]);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_get_string - Read into a string's data buffer
    writeln_code!(
        output,
        "void qb_file_get_string(int32_t fnum, qb_string* s) {{"
    )?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(output, "        qb_net_get_string((int64_t)fnum, s);")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )?;
    writeln_code!(output, "    if (!s || !s->data || s->len == 0) return;")?;
    writeln_code!(output, "    fread(s->data, 1, s->len, _qb_files[fnum]);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_put_string - Write from a string's data buffer
    writeln_code!(
        output,
        "void qb_file_put_string(int32_t fnum, qb_string* s) {{"
    )?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(output, "        qb_net_put_string((int64_t)fnum, s);")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return;"
    )?;
    writeln_code!(output, "    if (!s || !s->data || s->len == 0) return;")?;
    writeln_code!(output, "    fwrite(s->data, 1, s->len, _qb_files[fnum]);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_file_put(int32_t fnum, const void* data, size_t size) {{"
    )?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(
        output,
        "        qb_net_put((int64_t)fnum, (const uint8_t*)data, size);"
    )?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        fwrite(data, 1, size, _qb_files[fnum]);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // File functions
    writeln_code!(output, "int32_t qb_eof(int32_t fnum) {{")?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(output, "        return qb_net_eof((int64_t)fnum);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(output, "        return feof(_qb_files[fnum]) ? -1 : 0;")?;
    writeln_code!(output, "    return -1;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int64_t qb_lof(int32_t fnum) {{")?;
    writeln_code!(output, "    if (fnum < 0) {{")?;
    writeln_code!(output, "        return qb_net_lof((int64_t)fnum);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum]) return 0;"
    )?;
    writeln_code!(output, "    long pos = ftell(_qb_files[fnum]);")?;
    writeln_code!(output, "    fseek(_qb_files[fnum], 0, SEEK_END);")?;
    writeln_code!(output, "    long len = ftell(_qb_files[fnum]);")?;
    writeln_code!(output, "    fseek(_qb_files[fnum], pos, SEEK_SET);")?;
    writeln_code!(output, "    return (int64_t)len;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int64_t qb_loc(int32_t fnum) {{")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum])"
    )?;
    writeln_code!(
        output,
        "        return (int64_t)ftell(_qb_files[fnum]) + 1;"
    )?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int32_t qb_freefile(void) {{")?;
    writeln_code!(output, "    for (int i = 1; i < QB_MAX_FILES; i++)")?;
    writeln_code!(output, "        if (!_qb_files[i]) return i;")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // FIELD statement support
    // Each file can have a field buffer that maps string variables to portions of the buffer
    writeln_code!(output, "/* FIELD Statement Support */")?;
    writeln_code!(output, "static char* _qb_field_buffer[QB_MAX_FILES];")?;
    writeln_code!(output, "static int32_t _qb_field_offset[QB_MAX_FILES];")?;
    writeln_code!(output)?;

    // qb_field_start - Begin a FIELD statement, allocate buffer
    writeln_code!(output, "void qb_field_start(int32_t fnum) {{")?;
    writeln_code!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;")?;
    writeln_code!(output, "    if (!_qb_field_buffer[fnum]) {{")?;
    writeln_code!(
        output,
        "        _qb_field_buffer[fnum] = (char*)calloc(_qb_file_reclen[fnum], 1);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    _qb_field_offset[fnum] = 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_field_add - Add a string variable to the field at current offset
    // The string variable's data pointer is set to point into the field buffer
    writeln_code!(
        output,
        "void qb_field_add(int32_t width, qb_string** var) {{"
    )?;
    writeln_code!(output, "    /* Note: FIELD requires special handling")?;
    writeln_code!(
        output,
        "       In classic BASIC, FIELD maps string variables directly"
    )?;
    writeln_code!(
        output,
        "       to the file buffer. This is complex with ref-counted strings."
    )?;
    writeln_code!(
        output,
        "       For now, we create a fixed-length string. */"
    )?;
    writeln_code!(output, "    if (*var) qb_string_release(*var);")?;
    writeln_code!(output, "    *var = qb_string_new_len(width);")?;
    writeln_code!(output, "    memset((*var)->data, ' ', width);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_lset - Left-justify a string value into a fixed-length string variable
    writeln_code!(output, "void qb_lset(qb_string** var, qb_string* value) {{")?;
    writeln_code!(output, "    if (!*var || !value) return;")?;
    writeln_code!(output, "    int32_t var_len = (*var)->len;")?;
    writeln_code!(output, "    int32_t val_len = value->len;")?;
    writeln_code!(output, "    /* Fill with spaces first */")?;
    writeln_code!(output, "    memset((*var)->data, ' ', var_len);")?;
    writeln_code!(output, "    /* Copy value left-justified */")?;
    writeln_code!(
        output,
        "    int32_t copy_len = val_len < var_len ? val_len : var_len;"
    )?;
    writeln_code!(output, "    memcpy((*var)->data, value->data, copy_len);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_rset - Right-justify a string value into a fixed-length string variable
    writeln_code!(output, "void qb_rset(qb_string** var, qb_string* value) {{")?;
    writeln_code!(output, "    if (!*var || !value) return;")?;
    writeln_code!(output, "    int32_t var_len = (*var)->len;")?;
    writeln_code!(output, "    int32_t val_len = value->len;")?;
    writeln_code!(output, "    /* Fill with spaces first */")?;
    writeln_code!(output, "    memset((*var)->data, ' ', var_len);")?;
    writeln_code!(output, "    /* Copy value right-justified */")?;
    writeln_code!(
        output,
        "    int32_t copy_len = val_len < var_len ? val_len : var_len;"
    )?;
    writeln_code!(output, "    int32_t offset = var_len - copy_len;")?;
    writeln_code!(
        output,
        "    memcpy((*var)->data + offset, value->data, copy_len);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
