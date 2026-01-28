//! System stub declarations for the C backend runtime.
//!
//! This module emits C code for system-related stub functions including:
//! - Filesystem operations (file/directory existence, paths, mkdir, rmdir, chdir, kill, rename)
//!   On non-Windows, KILL, NAME, MKDIR, RMDIR, CHDIR, and qb_file_rename normalize `\`→`/` in paths.
//! - Shell and console functions (SHELL, ECHO, CONSOLE)
//! - String manipulation helpers (ASC assignment, INSTRREV)
//! - Font functions (_FONT, _FREEFONT, _LOADFONT stubs; _MAPUNICODE functional)
//! - Window functions (stubs for _TITLE, _SCREENMOVE, _SCREENSHOW, _ICON)
//! - Environment functions (ENVIRON$)
//! - Error functions (_INCLERRORLINE, _INCLERRORFILE$, _EXIT, _STATUSCODE)
//! - Network functions (stubs - no actual network support)
//! - Drag and drop functions (stubs)
//! - Dialog functions (stubs for _MESSAGEBOX, _SAVEDIALOG$, _OPENFILEDIALOG$)
//! - Number conversion functions (VAL for 64-bit, FIX, MKQ$, CVQ)
//! - Compression functions (stubs for _DEFLATE$, _MD5$)
//! - Windows-specific functions (logical drives)
//! - Color functions (_DEFAULTCOLOR, _BACKGROUNDCOLOR)
//! - Screen functions (SCREEN for reading characters/attributes)
//! - Graphics screen mode and resize functions
//! - Palette and window management functions
//!
//! ## _MAPUNICODE Support
//!
//! The `_MAPUNICODE` statement and function are fully implemented:
//!
//! - **Statement:** `_MAPUNICODE unicode_codepoint%, ascii_position%`
//!   Sets the Unicode codepoint that should render for a given ASCII position (0-255).
//!
//! - **Function:** `_MAPUNICODE(ascii_position%)`
//!   Returns the Unicode codepoint mapped to the given ASCII position.
//!
//! The default mapping is Code Page 437 (IBM PC original character set), which includes:
//! - ASCII 0-127: Standard characters (map to same Unicode codepoints)
//! - ASCII 128-255: Extended characters (box-drawing, Greek letters, math symbols)
//!
//! This matches QB64PE's behavior where `_MAPUNICODE` is used for font rendering
//! rather than string handling. String functions (LEN, LEFT$, MID$, etc.) remain
//! byte-based in both QB64PE and QB64Fresh.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits C code for system-related stub function declarations.
///
/// This function generates C implementations for various system functions
/// that provide filesystem, shell, console, and other system-level functionality.
/// Many of these are stubs that provide minimal implementations suitable for
/// console-mode programs without full graphics or network support.
///
/// # Arguments
///
/// * `output` - A mutable string to append the generated C code to.
pub(super) fn emit_stub_declarations(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Stub function implementations */")?;
    writeln_code!(output)?;

    // File system functions - actual implementations
    writeln_code!(output, "#include <sys/stat.h>")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "#include <direct.h>")?;
    writeln_code!(output, "#define mkdir(path, mode) _mkdir(path)")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "#include <unistd.h>")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // Helper to normalize path separators on non-Windows systems
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(
        output,
        "static void _qb_normalize_path_inplace(char* path) {{"
    )?;
    writeln_code!(output, "    if (!path) return;")?;
    writeln_code!(output, "    for (char* p = path; *p; p++) {{")?;
    writeln_code!(output, "        if (*p == '\\\\') *p = '/';")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "#define _qb_normalize_path_inplace(p) ((void)0)")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    writeln_code!(output, "int32_t qb_file_exists(QbString* path) {{")?;
    writeln_code!(output, "    if (!path || !path->data) return 0;")?;
    writeln_code!(output, "    struct stat st;")?;
    writeln_code!(
        output,
        "    if (stat(path->data, &st) == 0 && S_ISREG(st.st_mode)) return -1;"
    )?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    /* Try with normalized path */")?;
    writeln_code!(output, "    char* norm = strdup(path->data);")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(norm);")?;
    writeln_code!(
        output,
        "    int result = stat(norm, &st) == 0 && S_ISREG(st.st_mode) ? -1 : 0;"
    )?;
    writeln_code!(output, "    free(norm);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;

    writeln_code!(output, "int32_t qb_dir_exists(QbString* path) {{")?;
    writeln_code!(output, "    if (!path || !path->data) return 0;")?;
    writeln_code!(output, "    struct stat st;")?;
    writeln_code!(
        output,
        "    if (stat(path->data, &st) == 0 && S_ISDIR(st.st_mode)) return -1;"
    )?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    /* Try with normalized path */")?;
    writeln_code!(output, "    char* norm = strdup(path->data);")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(norm);")?;
    writeln_code!(
        output,
        "    int result = stat(norm, &st) == 0 && S_ISDIR(st.st_mode) ? -1 : 0;"
    )?;
    writeln_code!(output, "    free(norm);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;

    writeln_code!(output, "QbString* qb_fullpath(QbString* path) {{")?;
    writeln_code!(
        output,
        "    if (!path || !path->data) return qb_string_new(\"\");"
    )?;
    writeln_code!(output, "    char resolved[4096];")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(
        output,
        "    if (_fullpath(resolved, path->data, sizeof(resolved)))"
    )?;
    writeln_code!(output, "        return qb_string_new(resolved);")?;
    writeln_code!(output, "#else")?;
    // Normalize backslashes to forward slashes before calling realpath
    writeln_code!(output, "    char* normalized = strdup(path->data);")?;
    writeln_code!(output, "    if (normalized) {{")?;
    writeln_code!(output, "        _qb_normalize_path_inplace(normalized);")?;
    writeln_code!(output, "        if (realpath(normalized, resolved)) {{")?;
    writeln_code!(output, "            free(normalized);")?;
    writeln_code!(output, "            return qb_string_new(resolved);")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        free(normalized);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(
        output,
        "    return qb_string_new(path->data ? path->data : \"\");"
    )?;
    writeln_code!(output, "}}")?;

    writeln_code!(
        output,
        "QbString* qb_dir(QbString* spec) {{ (void)spec; return qb_string_new(\"\"); }}"
    )?;

    writeln_code!(output, "int32_t qb_chdir(const char* path) {{")?;
    writeln_code!(output, "    if (!path) return -1;")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    return _chdir(path) == 0 ? 0 : -1;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    char* n = strdup(path);")?;
    writeln_code!(output, "    if (!n) return -1;")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(n);")?;
    writeln_code!(output, "    int r = chdir(n) == 0 ? 0 : -1;")?;
    writeln_code!(output, "    free(n);")?;
    writeln_code!(output, "    return r;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;

    writeln_code!(output, "int32_t qb_mkdir(const char* path) {{")?;
    writeln_code!(output, "    if (!path) return -1;")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    return mkdir(path, 0755) == 0 ? 0 : -1;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    char* n = strdup(path);")?;
    writeln_code!(output, "    if (!n) return -1;")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(n);")?;
    writeln_code!(output, "    int r = mkdir(n, 0755) == 0 ? 0 : -1;")?;
    writeln_code!(output, "    free(n);")?;
    writeln_code!(output, "    return r;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;

    writeln_code!(output, "int32_t qb_rmdir(const char* path) {{")?;
    writeln_code!(output, "    if (!path) return -1;")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    return _rmdir(path) == 0 ? 0 : -1;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    char* n = strdup(path);")?;
    writeln_code!(output, "    if (!n) return -1;")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(n);")?;
    writeln_code!(output, "    int r = rmdir(n) == 0 ? 0 : -1;")?;
    writeln_code!(output, "    free(n);")?;
    writeln_code!(output, "    return r;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;

    writeln_code!(output, "int32_t qb_file_kill(const char* filename) {{")?;
    writeln_code!(output, "    if (!filename) return -1;")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    return remove(filename) == 0 ? 0 : -1;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    char* n = strdup(filename);")?;
    writeln_code!(output, "    if (!n) return -1;")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(n);")?;
    writeln_code!(output, "    int r = remove(n) == 0 ? 0 : -1;")?;
    writeln_code!(output, "    free(n);")?;
    writeln_code!(output, "    return r;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;

    writeln_code!(
        output,
        "int32_t qb_file_rename(const char* old_name, const char* new_name) {{"
    )?;
    writeln_code!(output, "    if (!old_name || !new_name) return -1;")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(
        output,
        "    return rename(old_name, new_name) == 0 ? 0 : -1;"
    )?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    char* o = strdup(old_name);")?;
    writeln_code!(output, "    char* n = strdup(new_name);")?;
    writeln_code!(
        output,
        "    if (!o || !n) {{ free(o); free(n); return -1; }}"
    )?;
    writeln_code!(output, "    _qb_normalize_path_inplace(o);")?;
    writeln_code!(output, "    _qb_normalize_path_inplace(n);")?;
    writeln_code!(output, "    int r = rename(o, n) == 0 ? 0 : -1;")?;
    writeln_code!(output, "    free(o);")?;
    writeln_code!(output, "    free(n);")?;
    writeln_code!(output, "    return r;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Console/shell functions
    writeln_code!(output, "int32_t qb_console_get(void) {{ return 1; }}")?;
    writeln_code!(
        output,
        "int32_t qb_console(int32_t mode) {{ (void)mode; return 1; }}"
    )?;

    writeln_code!(output, "int32_t qb_shell(QbString* cmd) {{")?;
    writeln_code!(output, "    if (!cmd || !cmd->data) return -1;")?;
    writeln_code!(output, "    return system(cmd->data);")?;
    writeln_code!(output, "}}")?;

    writeln_code!(
        output,
        "int32_t qb_shell_hide(QbString* cmd) {{ return qb_shell(cmd); }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_shellhide(QbString* cmd) {{ return qb_shell(cmd); }}"
    )?;
    writeln_code!(
        output,
        "void qb_echo(QbString* text) {{ if (text) {{ qb_print_string(text); qb_print_newline(); }} }}"
    )?;
    writeln_code!(
        output,
        "void qb_controlchr(int32_t state) {{ (void)state; }}"
    )?;
    writeln_code!(output)?;

    // String functions
    writeln_code!(
        output,
        "void qb_asc_assign(QbString** s, int32_t pos, int32_t ch) {{"
    )?;
    writeln_code!(
        output,
        "    if (!s || !*s || pos < 1 || pos > (int32_t)qb_string_len(*s)) return;"
    )?;
    writeln_code!(output, "    qb_string_data(*s)[pos - 1] = (char)ch;")?;
    writeln_code!(output, "}}")?;

    writeln_code!(
        output,
        "int32_t qb_instrrev3(QbString* s, QbString* sub, int32_t start) {{"
    )?;
    writeln_code!(
        output,
        "    if (!s || !sub || !s->data || !sub->data) return 0;"
    )?;
    writeln_code!(output, "    if (sub->len == 0) return start;")?;
    writeln_code!(output, "    if (s->len < sub->len) return 0;")?;
    writeln_code!(
        output,
        "    int32_t search_start = (start < 1 || start > (int32_t)s->len) ? (int32_t)s->len : start;"
    )?;
    writeln_code!(
        output,
        "    for (int32_t i = search_start - sub->len; i >= 0; i--) {{"
    )?;
    writeln_code!(
        output,
        "        if (memcmp(s->data + i, sub->data, sub->len) == 0) return i + 1;"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Font functions (stubs)
    writeln_code!(
        output,
        "void qb_sub__font(int32_t handle) {{ (void)handle; }}"
    )?;
    writeln_code!(
        output,
        "void qb_sub__freefont(int32_t handle) {{ (void)handle; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_loadfont3(QbString* path, int32_t size, QbString* req) {{ (void)path; (void)size; (void)req; return 0; }}"
    )?;

    // _MAPUNICODE implementation - maintains CP437 to Unicode mapping table
    // QB64PE uses this for font rendering: maps ASCII positions (0-255) to Unicode codepoints
    // Default: Code Page 437 (IBM PC original character set)
    writeln_code!(
        output,
        "/* _MAPUNICODE - Code Page 437 to Unicode mapping */"
    )?;
    writeln_code!(output, "static int32_t _qb_unicode_map[256] = {{")?;
    writeln_code!(
        output,
        "    /* 0x00-0x0F */ 0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007,"
    )?;
    writeln_code!(
        output,
        "                    0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x10-0x1F */ 0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017,"
    )?;
    writeln_code!(
        output,
        "                    0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x20-0x2F */ 0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,"
    )?;
    writeln_code!(
        output,
        "                    0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x30-0x3F */ 0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,"
    )?;
    writeln_code!(
        output,
        "                    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x40-0x4F */ 0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,"
    )?;
    writeln_code!(
        output,
        "                    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x50-0x5F */ 0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,"
    )?;
    writeln_code!(
        output,
        "                    0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x60-0x6F */ 0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067,"
    )?;
    writeln_code!(
        output,
        "                    0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x70-0x7F */ 0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077,"
    )?;
    writeln_code!(
        output,
        "                    0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F,"
    )?;
    writeln_code!(
        output,
        "    /* 0x80-0x8F */ 0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,"
    )?;
    writeln_code!(
        output,
        "                    0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,"
    )?;
    writeln_code!(
        output,
        "    /* 0x90-0x9F */ 0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,"
    )?;
    writeln_code!(
        output,
        "                    0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,"
    )?;
    writeln_code!(
        output,
        "    /* 0xA0-0xAF */ 0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,"
    )?;
    writeln_code!(
        output,
        "                    0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,"
    )?;
    writeln_code!(
        output,
        "    /* 0xB0-0xBF */ 0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,"
    )?;
    writeln_code!(
        output,
        "                    0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,"
    )?;
    writeln_code!(
        output,
        "    /* 0xC0-0xCF */ 0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,"
    )?;
    writeln_code!(
        output,
        "                    0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,"
    )?;
    writeln_code!(
        output,
        "    /* 0xD0-0xDF */ 0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,"
    )?;
    writeln_code!(
        output,
        "                    0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,"
    )?;
    writeln_code!(
        output,
        "    /* 0xE0-0xEF */ 0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,"
    )?;
    writeln_code!(
        output,
        "                    0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,"
    )?;
    writeln_code!(
        output,
        "    /* 0xF0-0xFF */ 0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,"
    )?;
    writeln_code!(
        output,
        "                    0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0"
    )?;
    writeln_code!(output, "}};")?;
    // Alias for direct array access (QB64PE uses _MAPUNICODE as an array)
    writeln_code!(output, "#define _MAPUNICODE _qb_unicode_map")?;
    writeln_code!(output)?;

    // _MAPUNICODE unicode_codepoint, ascii_position - Set mapping
    writeln_code!(
        output,
        "void qb_mapunicode(int32_t unicode_code, int32_t ascii_pos) {{"
    )?;
    writeln_code!(output, "    if (ascii_pos >= 0 && ascii_pos < 256) {{")?;
    writeln_code!(output, "        _qb_unicode_map[ascii_pos] = unicode_code;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;

    // _MAPUNICODE(ascii_position) - Get Unicode codepoint for ASCII position
    writeln_code!(output, "int32_t qb__mapunicode1(int32_t ascii_pos) {{")?;
    writeln_code!(output, "    if (ascii_pos >= 0 && ascii_pos < 256) {{")?;
    writeln_code!(output, "        return _qb_unicode_map[ascii_pos];")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;

    // Additional variants with fontpage parameter (fontpage is currently ignored - single font page)
    writeln_code!(
        output,
        "int32_t qb__mapunicode2(int32_t ascii_pos, int32_t fontpage) {{ (void)fontpage; return qb__mapunicode1(ascii_pos); }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb__mapunicode(int32_t ascii_pos, int32_t fontpage, int32_t chr) {{ (void)fontpage; (void)chr; return qb__mapunicode1(ascii_pos); }}"
    )?;
    writeln_code!(output)?;

    // Window functions (stubs)
    writeln_code!(
        output,
        "void qb_sub__title(QbString* title) {{ (void)title; }}"
    )?;
    writeln_code!(
        output,
        "void qb_sub__screenmove(int32_t x, int32_t y) {{ (void)x; (void)y; }}"
    )?;
    writeln_code!(output, "void qb_sub__screenshow(void) {{ }}")?;
    writeln_code!(output, "void qb_icon(void) {{ }}")?;
    writeln_code!(output, "void qb_icon1(int32_t handle) {{ (void)handle; }}")?;
    writeln_code!(
        output,
        "void qb_icon2(int32_t handle, QbString* cmd) {{ (void)handle; (void)cmd; }}"
    )?;
    writeln_code!(output)?;

    // Environment functions
    // ENVIRON "name=value" - set environment variable
    writeln_code!(output, "void qb_sub_environ(QbString* env) {{")?;
    writeln_code!(output, "    if (!env || !env->data) return;")?;
    writeln_code!(output, "    const char* env_str = env->data;")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Parse 'name=value' format")?;
    writeln_code!(output, "    char* eq = strchr(env_str, '=');")?;
    writeln_code!(
        output,
        "    if (!eq || eq == env_str) return; // Invalid format (no '=' or name is empty)"
    )?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "    // Allocate buffer for 'name=value' (putenv requires persistent string)"
    )?;
    writeln_code!(output, "    size_t total_len = strlen(env_str);")?;
    writeln_code!(output, "    char* env_buf = (char*)malloc(total_len + 1);")?;
    writeln_code!(output, "    if (!env_buf) return;")?;
    writeln_code!(output, "    strcpy(env_buf, env_str);")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Use putenv (works on both Windows and Unix)")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    _putenv(env_buf);")?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "    putenv(env_buf); // Note: putenv takes ownership of the string"
    )?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    // Don't free env_buf - putenv takes ownership")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Error functions
    writeln_code!(output, "int32_t qb_inclerrorline(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "QbString* qb_inclerrorfile(void) {{ return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(output, "int32_t qb_exit_state(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "int32_t qb_statuscode(int32_t handle) {{ (void)handle; return 0; }}"
    )?;
    writeln_code!(output)?;

    // Network functions (stubs - no actual network support)
    // QB64pe calls this with a string argument (host:port format)
    writeln_code!(
        output,
        "static int64_t qb_net_openhost(QbString* hostport) {{ (void)hostport; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_net_openconnection(int32_t host) {{ (void)host; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_net_openclient(QbString* addr) {{ (void)addr; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_net_connected(int32_t handle) {{ (void)handle; return 0; }}"
    )?;
    // _STATUSCODE - HTTP status code for network handles (stub returns 200 OK)
    writeln_code!(
        output,
        "int64_t qb__statuscode(int64_t handle) {{ (void)handle; return 200; }}"
    )?;
    // Workaround array for legacy code that uses _STATUSCODE[handle] syntax
    // (from before _STATUSCODE was registered as a function)
    writeln_code!(
        output,
        "static int64_t _STATUSCODE[256] = {{[0 ... 255] = 200}};"
    )?;
    writeln_code!(output)?;
    // Network I/O stubs (used by file.rs for network file handles)
    writeln_code!(
        output,
        "size_t qb_net_get(int64_t handle, uint8_t* data, size_t size) {{ (void)handle; (void)data; (void)size; return 0; }}"
    )?;
    writeln_code!(
        output,
        "size_t qb_net_put(int64_t handle, const uint8_t* data, size_t size) {{ (void)handle; (void)data; (void)size; return 0; }}"
    )?;
    writeln_code!(
        output,
        "size_t qb_net_get_string(int64_t handle, QbString* s) {{ (void)handle; (void)s; return 0; }}"
    )?;
    writeln_code!(
        output,
        "size_t qb_net_put_string(int64_t handle, const QbString* s) {{ (void)handle; (void)s; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_net_eof(int64_t handle) {{ (void)handle; return -1; }}"
    )?;
    writeln_code!(
        output,
        "int64_t qb_net_lof(int64_t handle) {{ (void)handle; return 0; }}"
    )?;
    writeln_code!(
        output,
        "void qb_net_close(int64_t handle) {{ (void)handle; }}"
    )?;
    writeln_code!(output)?;

    // Drag and drop functions (stubs)
    writeln_code!(output, "int32_t qb_totaldroppedfiles(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "QbString* qb_droppedfile_str(int32_t index) {{ (void)index; return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(output, "void qb_finishdrop(void) {{ }}")?;
    writeln_code!(output, "void qb_acceptfiledrop(void) {{ }}")?;
    writeln_code!(
        output,
        "void qb_acceptfiledrop1(int32_t state) {{ (void)state; }}"
    )?;
    writeln_code!(output)?;

    // Dialog functions (stubs)
    // QB64pe calls this with 4 string arguments: title, message, buttons, icon
    writeln_code!(
        output,
        "int32_t qb_messagebox4(QbString* title, QbString* msg, QbString* btns, QbString* icon) {{ (void)title; (void)msg; (void)btns; (void)icon; return 1; }}"
    )?;
    writeln_code!(
        output,
        "QbString* qb_savefiledialog4(QbString* title, QbString* filter, QbString* def, int32_t flags) {{ (void)title; (void)filter; (void)def; (void)flags; return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(
        output,
        "QbString* qb_openfiledialog5(QbString* title, QbString* filter, QbString* def, QbString* opts, int32_t flags) {{ (void)title; (void)filter; (void)def; (void)opts; (void)flags; return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(output)?;

    // Number conversion functions
    writeln_code!(
        output,
        "int64_t qb_val_int64(QbString* s) {{ if (!s || !s->data) return 0; return strtoll(s->data, NULL, 10); }}"
    )?;
    writeln_code!(
        output,
        "uint64_t qb_val_uint64(QbString* s) {{ if (!s || !s->data) return 0; return strtoull(s->data, NULL, 10); }}"
    )?;
    writeln_code!(
        output,
        "double qb_fix(double x) {{ return x >= 0 ? floor(x) : ceil(x); }}"
    )?;
    writeln_code!(
        output,
        "QbString* qb_mkq(double val) {{ char buf[64]; snprintf(buf, sizeof(buf), \"%.17g\", val); return qb_string_new(buf); }}"
    )?;
    writeln_code!(
        output,
        "double qb_cvq(QbString* s) {{ if (!s || !s->data || s->len < 8) return 0.0; double d; memcpy(&d, s->data, 8); return d; }}"
    )?;
    writeln_code!(output)?;

    // Compression functions (stubs - no actual compression)
    writeln_code!(
        output,
        "QbString* qb_deflate(QbString* data) {{ (void)data; return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(
        output,
        "QbString* qb_md5(QbString* data) {{ (void)data; return qb_string_new(\"00000000000000000000000000000000\"); }}"
    )?;
    writeln_code!(output)?;

    // Windows specific functions
    writeln_code!(output, "int32_t logical_drives(void) {{ return 0; }}")?;

    // Additional stubs needed for QB64pe
    // _DEFAULTCOLOR and _BACKGROUNDCOLOR can be called with or without handle argument
    writeln_code!(output, "int32_t qb_defaultcolor(void) {{ return 7; }}")?;
    writeln_code!(
        output,
        "int32_t qb_defaultcolor1(int32_t handle) {{ (void)handle; return 7; }}"
    )?;
    writeln_code!(output, "int32_t qb_backgroundcolor(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "int32_t qb_backgroundcolor1(int32_t handle) {{ (void)handle; return 0; }}"
    )?;

    // SCREEN function - reads character/attribute at screen position
    // SCREEN(row, col) returns ASCII code of character
    // SCREEN(row, col, 1) returns color attribute
    writeln_code!(
        output,
        "int32_t qb_screen(int32_t row, int32_t col) {{ (void)row; (void)col; return 32; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_screen3(int32_t row, int32_t col, int32_t attr) {{ (void)row; (void)col; (void)attr; return attr ? 7 : 32; }}"
    )?;

    // qb_gfx_screen - SCREEN statement for changing screen modes (4 args)
    // Must initialize frame counter to prevent infinite loops in stub mode
    writeln_code!(
        output,
        "void qb_gfx_screen(int32_t mode, int32_t colorSwitch, int32_t activePage, int32_t visiblePage) {{"
    )?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    _qb_gfx_init_max_frames();")?;
    writeln_code!(
        output,
        "    _qb_gfx_frame_count = 0; /* Reset frame counter on SCREEN */"
    )?;
    writeln_code!(
        output,
        "    (void)mode; (void)colorSwitch; (void)activePage; (void)visiblePage;"
    )?;
    writeln_code!(output, "}}")?;

    // qb_gfx_resize - resize control (1 arg)
    writeln_code!(output, "void qb_gfx_resize(int32_t flag) {{ (void)flag; }}")?;

    // Palette and resize functions
    // _PALETTECOLOR can be both function (get) and statement (set)
    writeln_code!(
        output,
        "int32_t qb_palettecolor(int32_t attr, int32_t col, int32_t handle) {{ (void)attr; (void)col; (void)handle; return _qb_palette[attr & 255]; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_palettecolor_get(int32_t attr, int32_t handle) {{ (void)handle; return _qb_palette[attr & 255]; }}"
    )?;
    writeln_code!(output, "int32_t qb_resize(void) {{ return 0; }}")?;
    writeln_code!(output, "int32_t qb_resizewidth(void) {{ return 80; }}")?;
    writeln_code!(output, "int32_t qb_resizeheight(void) {{ return 25; }}")?;

    // Window management stub
    writeln_code!(
        output,
        "void qb_sub_set_foreground_window(intptr_t hwnd) {{ (void)hwnd; }}"
    )?;

    // qb_string_copy - create a copy of a string
    writeln_code!(output, "QbString* qb_string_copy(QbString* s) {{")?;
    writeln_code!(output, "    if (!s) return qb_string_new(\"\");")?;
    writeln_code!(
        output,
        "    return qb_string_new(s->data ? s->data : \"\");"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
