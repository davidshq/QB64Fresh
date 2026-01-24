//! System stub declarations for the C backend runtime.
//!
//! This module emits C code for system-related stub functions including:
//! - Filesystem operations (file/directory existence, paths, mkdir, chdir, kill)
//! - Shell and console functions (SHELL, ECHO, CONSOLE)
//! - String manipulation helpers (ASC assignment, INSTRREV)
//! - Font functions (stubs for _FONT, _FREEFONT, _LOADFONT, _MAPUNICODE)
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

use std::fmt::Write;

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
pub(super) fn emit_stub_declarations(output: &mut String) {
    writeln!(output, "/* Stub function implementations */").unwrap();
    writeln!(output).unwrap();

    // File system functions - actual implementations
    writeln!(output, "#include <sys/stat.h>").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "#include <direct.h>").unwrap();
    writeln!(output, "#define mkdir(path, mode) _mkdir(path)").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "#include <unistd.h>").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // Helper to normalize path separators on non-Windows systems
    writeln!(output, "#ifndef _WIN32").unwrap();
    writeln!(
        output,
        "static void _qb_normalize_path_inplace(char* path) {{"
    )
    .unwrap();
    writeln!(output, "    if (!path) return;").unwrap();
    writeln!(output, "    for (char* p = path; *p; p++) {{").unwrap();
    writeln!(output, "        if (*p == '\\\\') *p = '/';").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "#define _qb_normalize_path_inplace(p) ((void)0)").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int32_t qb_file_exists(qb_string* path) {{").unwrap();
    writeln!(output, "    if (!path || !path->data) return 0;").unwrap();
    writeln!(output, "    struct stat st;").unwrap();
    writeln!(
        output,
        "    if (stat(path->data, &st) == 0 && S_ISREG(st.st_mode)) return -1;"
    )
    .unwrap();
    writeln!(output, "#ifndef _WIN32").unwrap();
    writeln!(output, "    /* Try with normalized path */").unwrap();
    writeln!(output, "    char* norm = strdup(path->data);").unwrap();
    writeln!(output, "    _qb_normalize_path_inplace(norm);").unwrap();
    writeln!(
        output,
        "    int result = stat(norm, &st) == 0 && S_ISREG(st.st_mode) ? -1 : 0;"
    )
    .unwrap();
    writeln!(output, "    free(norm);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(output, "int32_t qb_dir_exists(qb_string* path) {{").unwrap();
    writeln!(output, "    if (!path || !path->data) return 0;").unwrap();
    writeln!(output, "    struct stat st;").unwrap();
    writeln!(
        output,
        "    if (stat(path->data, &st) == 0 && S_ISDIR(st.st_mode)) return -1;"
    )
    .unwrap();
    writeln!(output, "#ifndef _WIN32").unwrap();
    writeln!(output, "    /* Try with normalized path */").unwrap();
    writeln!(output, "    char* norm = strdup(path->data);").unwrap();
    writeln!(output, "    _qb_normalize_path_inplace(norm);").unwrap();
    writeln!(
        output,
        "    int result = stat(norm, &st) == 0 && S_ISDIR(st.st_mode) ? -1 : 0;"
    )
    .unwrap();
    writeln!(output, "    free(norm);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(output, "qb_string* qb_fullpath(qb_string* path) {{").unwrap();
    writeln!(
        output,
        "    if (!path || !path->data) return qb_string_new(\"\");"
    )
    .unwrap();
    writeln!(output, "    char resolved[4096];").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(
        output,
        "    if (_fullpath(resolved, path->data, sizeof(resolved)))"
    )
    .unwrap();
    writeln!(output, "        return qb_string_new(resolved);").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    if (realpath(path->data, resolved))").unwrap();
    writeln!(output, "        return qb_string_new(resolved);").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(
        output,
        "    return qb_string_new(path->data ? path->data : \"\");"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(
        output,
        "qb_string* qb_dir(qb_string* spec) {{ (void)spec; return qb_string_new(\"\"); }}"
    )
    .unwrap();

    writeln!(output, "int32_t qb_chdir(const char* path) {{").unwrap();
    writeln!(output, "    if (!path) return -1;").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    return _chdir(path) == 0 ? 0 : -1;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    return chdir(path) == 0 ? 0 : -1;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(output, "int32_t qb_mkdir(const char* path) {{").unwrap();
    writeln!(output, "    if (!path) return -1;").unwrap();
    writeln!(output, "    return mkdir(path, 0755) == 0 ? 0 : -1;").unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(output, "int32_t qb_file_kill(const char* filename) {{").unwrap();
    writeln!(output, "    if (!filename) return -1;").unwrap();
    writeln!(output, "    return remove(filename) == 0 ? 0 : -1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Console/shell functions
    writeln!(output, "int32_t qb_console_get(void) {{ return 1; }}").unwrap();
    writeln!(
        output,
        "int32_t qb_console(int32_t mode) {{ (void)mode; return 1; }}"
    )
    .unwrap();

    writeln!(output, "int32_t qb_shell(qb_string* cmd) {{").unwrap();
    writeln!(output, "    if (!cmd || !cmd->data) return -1;").unwrap();
    writeln!(output, "    return system(cmd->data);").unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(
        output,
        "int32_t qb_shell_hide(qb_string* cmd) {{ return qb_shell(cmd); }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_shellhide(qb_string* cmd) {{ return qb_shell(cmd); }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_echo(int32_t state) {{ (void)state; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "void qb_controlchr(int32_t state) {{ (void)state; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // String functions
    writeln!(
        output,
        "void qb_asc_assign(qb_string** s, int32_t pos, int32_t ch) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!s || !*s || pos < 1 || pos > (int32_t)(*s)->len) return;"
    )
    .unwrap();
    writeln!(output, "    (*s)->data[pos - 1] = (char)ch;").unwrap();
    writeln!(output, "}}").unwrap();

    writeln!(
        output,
        "int32_t qb_instrrev3(qb_string* s, qb_string* sub, int32_t start) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!s || !sub || !s->data || !sub->data) return 0;"
    )
    .unwrap();
    writeln!(output, "    if (sub->len == 0) return start;").unwrap();
    writeln!(output, "    if (s->len < sub->len) return 0;").unwrap();
    writeln!(output, "    int32_t search_start = (start < 1 || start > (int32_t)s->len) ? (int32_t)s->len : start;").unwrap();
    writeln!(
        output,
        "    for (int32_t i = search_start - sub->len; i >= 0; i--) {{"
    )
    .unwrap();
    writeln!(
        output,
        "        if (memcmp(s->data + i, sub->data, sub->len) == 0) return i + 1;"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Font functions (stubs)
    writeln!(
        output,
        "void qb_sub__font(int32_t handle) {{ (void)handle; }}"
    )
    .unwrap();
    writeln!(
        output,
        "void qb_sub__freefont(int32_t handle) {{ (void)handle; }}"
    )
    .unwrap();
    writeln!(output, "int32_t qb_loadfont3(qb_string* path, int32_t size, qb_string* req) {{ (void)path; (void)size; (void)req; return 0; }}").unwrap();
    writeln!(
        output,
        "void qb_mapunicode(int32_t code, int32_t chr) {{ (void)code; (void)chr; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb__mapunicode1(int32_t code) {{ (void)code; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int32_t qb__mapunicode2(int32_t code, int32_t fontpage) {{ (void)code; (void)fontpage; return 0; }}").unwrap();
    writeln!(output, "int32_t qb__mapunicode(int32_t code, int32_t fontpage, int32_t chr) {{ (void)code; (void)fontpage; (void)chr; return 0; }}").unwrap();
    writeln!(output).unwrap();

    // Window functions (stubs)
    writeln!(
        output,
        "void qb_sub__title(qb_string* title) {{ (void)title; }}"
    )
    .unwrap();
    writeln!(
        output,
        "void qb_sub__screenmove(int32_t x, int32_t y) {{ (void)x; (void)y; }}"
    )
    .unwrap();
    writeln!(output, "void qb_sub__screenshow(void) {{ }}").unwrap();
    writeln!(output, "void qb_icon(void) {{ }}").unwrap();
    writeln!(output, "void qb_icon1(int32_t handle) {{ (void)handle; }}").unwrap();
    writeln!(
        output,
        "void qb_icon2(int32_t handle, qb_string* cmd) {{ (void)handle; (void)cmd; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Environment functions
    writeln!(
        output,
        "void qb_sub_environ(qb_string* env) {{ (void)env; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Error functions
    writeln!(output, "int32_t qb_inclerrorline(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "qb_string* qb_inclerrorfile(void) {{ return qb_string_new(\"\"); }}"
    )
    .unwrap();
    writeln!(output, "int32_t qb_exit_state(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "int32_t qb_statuscode(int32_t handle) {{ (void)handle; return 0; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Network functions (stubs - no actual network support)
    writeln!(
        output,
        "int32_t qb_net_openhost(qb_string* port) {{ (void)port; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_net_openconnection(int32_t host) {{ (void)host; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_net_openclient(qb_string* addr) {{ (void)addr; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_net_connected(int32_t handle) {{ (void)handle; return 0; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Drag and drop functions (stubs)
    writeln!(output, "int32_t qb_totaldroppedfiles(void) {{ return 0; }}").unwrap();
    writeln!(output, "qb_string* qb_droppedfile_str(int32_t index) {{ (void)index; return qb_string_new(\"\"); }}").unwrap();
    writeln!(output, "void qb_finishdrop(void) {{ }}").unwrap();
    writeln!(output, "void qb_acceptfiledrop(void) {{ }}").unwrap();
    writeln!(
        output,
        "void qb_acceptfiledrop1(int32_t state) {{ (void)state; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Dialog functions (stubs)
    writeln!(output, "int32_t qb_messagebox4(qb_string* title, qb_string* msg, qb_string* btns, int32_t def) {{ (void)title; (void)msg; (void)btns; (void)def; return 1; }}").unwrap();
    writeln!(output, "qb_string* qb_savefiledialog4(qb_string* title, qb_string* filter, qb_string* def, int32_t flags) {{ (void)title; (void)filter; (void)def; (void)flags; return qb_string_new(\"\"); }}").unwrap();
    writeln!(output, "qb_string* qb_openfiledialog5(qb_string* title, qb_string* filter, qb_string* def, qb_string* opts, int32_t flags) {{ (void)title; (void)filter; (void)def; (void)opts; (void)flags; return qb_string_new(\"\"); }}").unwrap();
    writeln!(output).unwrap();

    // Number conversion functions
    writeln!(output, "int64_t qb_val_int64(qb_string* s) {{ if (!s || !s->data) return 0; return strtoll(s->data, NULL, 10); }}").unwrap();
    writeln!(output, "uint64_t qb_val_uint64(qb_string* s) {{ if (!s || !s->data) return 0; return strtoull(s->data, NULL, 10); }}").unwrap();
    writeln!(
        output,
        "double qb_fix(double x) {{ return x >= 0 ? floor(x) : ceil(x); }}"
    )
    .unwrap();
    writeln!(output, "qb_string* qb_mkq(double val) {{ char buf[64]; snprintf(buf, sizeof(buf), \"%.17g\", val); return qb_string_new(buf); }}").unwrap();
    writeln!(output, "double qb_cvq(qb_string* s) {{ if (!s || !s->data || s->len < 8) return 0.0; double d; memcpy(&d, s->data, 8); return d; }}").unwrap();
    writeln!(output).unwrap();

    // Compression functions (stubs - no actual compression)
    writeln!(
        output,
        "qb_string* qb_deflate(qb_string* data) {{ (void)data; return qb_string_new(\"\"); }}"
    )
    .unwrap();
    writeln!(output, "qb_string* qb_md5(qb_string* data) {{ (void)data; return qb_string_new(\"00000000000000000000000000000000\"); }}").unwrap();
    writeln!(output).unwrap();

    // Windows specific functions
    writeln!(output, "int32_t logical_drives(void) {{ return 0; }}").unwrap();

    // Additional stubs needed for QB64pe
    // _DEFAULTCOLOR and _BACKGROUNDCOLOR can be called with or without handle argument
    writeln!(output, "int32_t qb_defaultcolor(void) {{ return 7; }}").unwrap();
    writeln!(
        output,
        "int32_t qb_defaultcolor1(int32_t handle) {{ (void)handle; return 7; }}"
    )
    .unwrap();
    writeln!(output, "int32_t qb_backgroundcolor(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "int32_t qb_backgroundcolor1(int32_t handle) {{ (void)handle; return 0; }}"
    )
    .unwrap();

    // SCREEN function - reads character/attribute at screen position
    // SCREEN(row, col) returns ASCII code of character
    // SCREEN(row, col, 1) returns color attribute
    writeln!(
        output,
        "int32_t qb_screen(int32_t row, int32_t col) {{ (void)row; (void)col; return 32; }}"
    )
    .unwrap();
    writeln!(output, "int32_t qb_screen3(int32_t row, int32_t col, int32_t attr) {{ (void)row; (void)col; (void)attr; return attr ? 7 : 32; }}").unwrap();

    // qb_gfx_screen - SCREEN statement for changing screen modes (4 args)
    writeln!(output, "void qb_gfx_screen(int32_t mode, int32_t colorSwitch, int32_t activePage, int32_t visiblePage) {{ (void)mode; (void)colorSwitch; (void)activePage; (void)visiblePage; }}").unwrap();

    // qb_gfx_resize - resize control (1 arg)
    writeln!(output, "void qb_gfx_resize(int32_t flag) {{ (void)flag; }}").unwrap();

    // Palette and resize functions
    writeln!(output, "void qb_palettecolor(int32_t attr, int32_t col, int32_t handle) {{ (void)attr; (void)col; (void)handle; }}").unwrap();
    writeln!(output, "int32_t qb_resize(void) {{ return 0; }}").unwrap();
    writeln!(output, "int32_t qb_resizewidth(void) {{ return 80; }}").unwrap();
    writeln!(output, "int32_t qb_resizeheight(void) {{ return 25; }}").unwrap();

    // Window management stub
    writeln!(
        output,
        "void qb_sub_set_foreground_window(intptr_t hwnd) {{ (void)hwnd; }}"
    )
    .unwrap();

    // qb_string_copy - create a copy of a string
    writeln!(output, "qb_string* qb_string_copy(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s) return qb_string_new(\"\");").unwrap();
    writeln!(
        output,
        "    return qb_string_new(s->data ? s->data : \"\");"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
