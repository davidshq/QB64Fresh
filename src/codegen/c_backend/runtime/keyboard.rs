//! Keyboard input functions for the C runtime.
//!
//! This module emits C code for keyboard input and related console functions:
//! - INKEY$ - Non-blocking keyboard input
//! - INPUT$(n) - Read n characters from console or file
//! - _KEYHIT - QB64 key hit detection
//! - _KEYDOWN - QB64 key state checking
//! - _KEYCLEAR - Clear keyboard buffer
//! - _CINP - Raw console input
//! - Lock key state functions (_CAPSLOCK, _NUMLOCK, _SCROLLLOCK)
//! - Environment functions (ENVIRON$, COMMAND$, _CWD$, _OS$, _STARTDIR$)
//! - String pack/unpack functions (MKI$, MKL$, MKS$, MKD$, CVI, CVL, CVS, CVD)
//! - String functions (_INSTRREV, _TRIM$)
//! - Date/Time functions (TIMER, DATE$, TIME$, _DATE$, _TIME$)

use std::fmt::Write;

/// Emits keyboard input, environment, string, and date/time functions.
///
/// This function generates C code for various input and utility functions,
/// including platform-specific implementations for Windows and Unix systems.
pub(super) fn emit_keyboard_functions(output: &mut String) {
    writeln!(output, "/* Keyboard Input Functions */").unwrap();
    writeln!(output).unwrap();

    // Platform-specific non-blocking key check
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "#include <conio.h>").unwrap();
    writeln!(output, "qb_string* qb_inkey(void) {{").unwrap();
    writeln!(output, "    if (_kbhit()) {{").unwrap();
    writeln!(output, "        int ch = _getch();").unwrap();
    writeln!(output, "        if (ch == 0 || ch == 224) {{").unwrap();
    writeln!(
        output,
        "            char buf[3] = {{0, (char)_getch(), 0}};"
    )
    .unwrap();
    writeln!(output, "            return qb_string_new(buf);").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        char buf[2] = {{(char)ch, 0}};").unwrap();
    writeln!(output, "        return qb_string_new(buf);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "#include <termios.h>").unwrap();
    writeln!(output, "#include <unistd.h>").unwrap();
    writeln!(output, "#include <sys/select.h>").unwrap();
    writeln!(output, "qb_string* qb_inkey(void) {{").unwrap();
    writeln!(output, "    struct termios oldt, newt;").unwrap();
    writeln!(output, "    tcgetattr(STDIN_FILENO, &oldt);").unwrap();
    writeln!(output, "    newt = oldt;").unwrap();
    writeln!(output, "    newt.c_lflag &= ~(ICANON | ECHO);").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);").unwrap();
    writeln!(output, "    fd_set fds; struct timeval tv = {{0, 0}};").unwrap();
    writeln!(output, "    FD_ZERO(&fds); FD_SET(STDIN_FILENO, &fds);").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(\"\");").unwrap();
    writeln!(
        output,
        "    if (select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) > 0) {{"
    )
    .unwrap();
    writeln!(output, "        char buf[4] = {{0}};").unwrap();
    writeln!(
        output,
        "        int n = read(STDIN_FILENO, buf, sizeof(buf)-1);"
    )
    .unwrap();
    writeln!(output, "        if (n > 0) result = qb_string_new(buf);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // INPUT$(n) - read n characters
    writeln!(output, "qb_string* qb_input_chars(int32_t n) {{").unwrap();
    writeln!(output, "    if (n <= 0) return qb_string_new(\"\");").unwrap();
    writeln!(output, "    char* buf = malloc((size_t)n + 1);").unwrap();
    writeln!(
        output,
        "    for (int32_t i = 0; i < n; i++) buf[i] = (char)getchar();"
    )
    .unwrap();
    writeln!(output, "    buf[n] = '\\0';").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(buf);").unwrap();
    writeln!(output, "    free(buf);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // INPUT$(n, #fnum) - read n characters from file
    writeln!(
        output,
        "qb_string* qb_input_chars_file(int32_t n, int32_t fnum) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (n <= 0 || fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum])"
    )
    .unwrap();
    writeln!(output, "        return qb_string_new(\"\");").unwrap();
    writeln!(output, "    char* buf = malloc((size_t)n + 1);").unwrap();
    writeln!(
        output,
        "    size_t read = fread(buf, 1, (size_t)n, _qb_files[fnum]);"
    )
    .unwrap();
    writeln!(output, "    buf[read] = '\\0';").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(buf);").unwrap();
    writeln!(output, "    free(buf);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // QB64 Keyboard Extensions
    writeln!(output, "/* QB64 Keyboard Extensions */").unwrap();
    writeln!(output).unwrap();

    // Keyboard buffer for _KEYHIT
    writeln!(output, "static int64_t _qb_keybuf[256];").unwrap();
    writeln!(output, "static int _qb_keybuf_head = 0;").unwrap();
    writeln!(output, "static int _qb_keybuf_tail = 0;").unwrap();
    writeln!(output).unwrap();

    // _KEYHIT - returns key code without waiting (0 if no key)
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "int64_t qb_keyhit(void) {{").unwrap();
    writeln!(output, "    if (!_kbhit()) return 0;").unwrap();
    writeln!(output, "    int ch = _getch();").unwrap();
    writeln!(output, "    if (ch == 0 || ch == 224) {{").unwrap();
    writeln!(output, "        int ext = _getch();").unwrap();
    writeln!(
        output,
        "        return -(ext + 256); // Extended keys as negative"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return ch;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "int64_t qb_keyhit(void) {{").unwrap();
    writeln!(output, "    struct termios oldt, newt;").unwrap();
    writeln!(output, "    tcgetattr(STDIN_FILENO, &oldt);").unwrap();
    writeln!(output, "    newt = oldt;").unwrap();
    writeln!(output, "    newt.c_lflag &= ~(ICANON | ECHO);").unwrap();
    writeln!(output, "    newt.c_cc[VMIN] = 0;").unwrap();
    writeln!(output, "    newt.c_cc[VTIME] = 0;").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);").unwrap();
    writeln!(output, "    int ch = getchar();").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);").unwrap();
    writeln!(output, "    if (ch == EOF) return 0;").unwrap();
    writeln!(output, "    if (ch == 27) {{ // Escape sequence").unwrap();
    writeln!(output, "        tcsetattr(STDIN_FILENO, TCSANOW, &newt);").unwrap();
    writeln!(output, "        int next = getchar();").unwrap();
    writeln!(output, "        tcsetattr(STDIN_FILENO, TCSANOW, &oldt);").unwrap();
    writeln!(output, "        if (next != EOF) return -(next + 256);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return ch;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // _KEYDOWN - check if specific key is pressed (simplified - checks buffer)
    writeln!(output, "int64_t qb_keydown(int64_t code) {{").unwrap();
    writeln!(
        output,
        "    // Simplified implementation - always returns 0"
    )
    .unwrap();
    writeln!(
        output,
        "    // Full implementation requires platform-specific key state checking"
    )
    .unwrap();
    writeln!(output, "    (void)code;").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _KEYCLEAR - clear keyboard buffer
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "void qb_keyclear(void) {{").unwrap();
    writeln!(output, "    while (_kbhit()) _getch();").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "void qb_keyclear(void) {{").unwrap();
    writeln!(output, "    tcflush(STDIN_FILENO, TCIFLUSH);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // _CINP - raw console input (returns character code, no echo)
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "int64_t qb_cinp(void) {{").unwrap();
    writeln!(output, "    return _getch();").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "int64_t qb_cinp(void) {{").unwrap();
    writeln!(output, "    struct termios oldt, newt;").unwrap();
    writeln!(output, "    tcgetattr(STDIN_FILENO, &oldt);").unwrap();
    writeln!(output, "    newt = oldt;").unwrap();
    writeln!(output, "    newt.c_lflag &= ~(ICANON | ECHO);").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);").unwrap();
    writeln!(output, "    int ch = getchar();").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);").unwrap();
    writeln!(output, "    return (ch == EOF) ? 0 : ch;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // Lock key state functions
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "int64_t qb_capslock(void) {{").unwrap();
    writeln!(output, "    return (GetKeyState(VK_CAPITAL) & 1) ? -1 : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "int64_t qb_numlock(void) {{").unwrap();
    writeln!(output, "    return (GetKeyState(VK_NUMLOCK) & 1) ? -1 : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "int64_t qb_scrolllock(void) {{").unwrap();
    writeln!(output, "    return (GetKeyState(VK_SCROLL) & 1) ? -1 : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(
        output,
        "// On Linux/macOS, lock key states require X11 or reading /sys files"
    )
    .unwrap();
    writeln!(output, "// Simplified stub implementation").unwrap();
    writeln!(output, "int64_t qb_capslock(void) {{ return 0; }}").unwrap();
    writeln!(output, "int64_t qb_numlock(void) {{ return 0; }}").unwrap();
    writeln!(output, "int64_t qb_scrolllock(void) {{ return 0; }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // Environment functions
    writeln!(output, "/* Environment Functions */").unwrap();
    writeln!(output).unwrap();

    // ENVIRON$(var$) - get environment variable
    writeln!(output, "qb_string* qb_environ(qb_string* var) {{").unwrap();
    writeln!(
        output,
        "    if (!var || !var->data) return qb_string_new(\"\");"
    )
    .unwrap();
    writeln!(output, "    const char* val = getenv(var->data);").unwrap();
    writeln!(
        output,
        "    return val ? qb_string_new(val) : qb_string_new(\"\");"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // COMMAND$ - get command line arguments
    writeln!(output, "static int _qb_argc = 0;").unwrap();
    writeln!(output, "static char** _qb_argv = NULL;").unwrap();
    writeln!(
        output,
        "void qb_init_args(int argc, char** argv) {{ _qb_argc = argc; _qb_argv = argv; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    writeln!(output, "qb_string* qb_command(void) {{").unwrap();
    writeln!(
        output,
        "    if (_qb_argc <= 1 || !_qb_argv) return qb_string_new(\"\");"
    )
    .unwrap();
    writeln!(output, "    size_t len = 0;").unwrap();
    writeln!(
        output,
        "    for (int i = 1; i < _qb_argc; i++) len += strlen(_qb_argv[i]) + 1;"
    )
    .unwrap();
    writeln!(output, "    char* buf = malloc(len + 1);").unwrap();
    writeln!(output, "    buf[0] = '\\0';").unwrap();
    writeln!(output, "    for (int i = 1; i < _qb_argc; i++) {{").unwrap();
    writeln!(output, "        if (i > 1) strcat(buf, \" \");").unwrap();
    writeln!(output, "        strcat(buf, _qb_argv[i]);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(buf);").unwrap();
    writeln!(output, "    free(buf);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // COMMAND$(n) - get specific command-line argument (1-based)
    writeln!(output, "qb_string* qb_command_n(int64_t n) {{").unwrap();
    writeln!(
        output,
        "    if (n < 0 || n >= _qb_argc || !_qb_argv) return qb_string_new(\"\");"
    )
    .unwrap();
    writeln!(
        output,
        "    if (n == 0) return qb_command(); // Return all args"
    )
    .unwrap();
    writeln!(output, "    return qb_string_new(_qb_argv[n]);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _CWD$ - current working directory
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "#include <direct.h>").unwrap();
    writeln!(output, "#define getcwd _getcwd").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "qb_string* qb_cwd(void) {{").unwrap();
    writeln!(output, "    char buf[4096];").unwrap();
    writeln!(
        output,
        "    if (getcwd(buf, sizeof(buf))) return qb_string_new(buf);"
    )
    .unwrap();
    writeln!(output, "    return qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _OS$ - operating system (QB64 format: [PLATFORM][BITS])
    writeln!(output, "qb_string* qb_os(void) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "  #if defined(_WIN64) || defined(__x86_64__)").unwrap();
    writeln!(output, "    return qb_string_new(\"[WINDOWS][64BIT]\");").unwrap();
    writeln!(output, "  #else").unwrap();
    writeln!(output, "    return qb_string_new(\"[WINDOWS][32BIT]\");").unwrap();
    writeln!(output, "  #endif").unwrap();
    writeln!(output, "#elif defined(__APPLE__)").unwrap();
    writeln!(output, "  #if defined(__x86_64__) || defined(__aarch64__)").unwrap();
    writeln!(output, "    return qb_string_new(\"[MACOSX][64BIT]\");").unwrap();
    writeln!(output, "  #else").unwrap();
    writeln!(output, "    return qb_string_new(\"[MACOSX][32BIT]\");").unwrap();
    writeln!(output, "  #endif").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "  #if defined(__x86_64__) || defined(__aarch64__)").unwrap();
    writeln!(output, "    return qb_string_new(\"[LINUX][64BIT]\");").unwrap();
    writeln!(output, "  #else").unwrap();
    writeln!(output, "    return qb_string_new(\"[LINUX][32BIT]\");").unwrap();
    writeln!(output, "  #endif").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _STARTDIR$ - program start directory
    writeln!(output, "static char _qb_startdir[4096] = {{}};").unwrap();
    writeln!(
        output,
        "void qb_init_startdir(void) {{ getcwd(_qb_startdir, sizeof(_qb_startdir)); }}"
    )
    .unwrap();
    writeln!(
        output,
        "qb_string* qb_startdir(void) {{ return qb_string_new(_qb_startdir); }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // ==================== Phase 2: String Enhancements ====================

    // _INSTRREV(source$, search$) - find last occurrence, 1-based, 0 if not found
    writeln!(
        output,
        "int32_t qb_instrrev(qb_string* source, qb_string* search) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!source || !search || search->len == 0) return 0;"
    )
    .unwrap();
    writeln!(output, "    if (search->len > source->len) return 0;").unwrap();
    writeln!(
        output,
        "    for (int32_t i = source->len - search->len; i >= 0; i--) {{"
    )
    .unwrap();
    writeln!(
        output,
        "        if (memcmp(source->data + i, search->data, search->len) == 0) {{"
    )
    .unwrap();
    writeln!(output, "            return i + 1; // 1-based").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _TRIM$(s$) - trim whitespace from both ends
    writeln!(output, "qb_string* qb_trim(qb_string* s) {{").unwrap();
    writeln!(
        output,
        "    if (!s || s->len == 0) return qb_string_new(\"\");"
    )
    .unwrap();
    writeln!(output, "    int32_t start = 0, end = s->len - 1;").unwrap();
    writeln!(
        output,
        "    while (start <= end && (s->data[start] == ' ' || s->data[start] == '\\t')) start++;"
    )
    .unwrap();
    writeln!(
        output,
        "    while (end >= start && (s->data[end] == ' ' || s->data[end] == '\\t')) end--;"
    )
    .unwrap();
    writeln!(output, "    if (start > end) return qb_string_new(\"\");").unwrap();
    writeln!(output, "    int32_t newlen = end - start + 1;").unwrap();
    writeln!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )
    .unwrap();
    writeln!(output, "    result->data = (char*)malloc(newlen + 1);").unwrap();
    writeln!(output, "    result->len = newlen;").unwrap();
    writeln!(output, "    result->capacity = newlen + 1;").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    memcpy(result->data, s->data + start, newlen);").unwrap();
    writeln!(output, "    result->data[newlen] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MKI$(n) - pack 16-bit integer to 2-byte string
    writeln!(output, "qb_string* qb_mki(int16_t n) {{").unwrap();
    writeln!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )
    .unwrap();
    writeln!(output, "    result->data = (char*)malloc(3);").unwrap();
    writeln!(output, "    result->len = 2;").unwrap();
    writeln!(output, "    result->capacity = 3;").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    memcpy(result->data, &n, 2);").unwrap();
    writeln!(output, "    result->data[2] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MKL$(n) - pack 32-bit long to 4-byte string
    writeln!(output, "qb_string* qb_mkl(int32_t n) {{").unwrap();
    writeln!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )
    .unwrap();
    writeln!(output, "    result->data = (char*)malloc(5);").unwrap();
    writeln!(output, "    result->len = 4;").unwrap();
    writeln!(output, "    result->capacity = 5;").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    memcpy(result->data, &n, 4);").unwrap();
    writeln!(output, "    result->data[4] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MKS$(n) - pack single to 4-byte string
    writeln!(output, "qb_string* qb_mks(float n) {{").unwrap();
    writeln!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )
    .unwrap();
    writeln!(output, "    result->data = (char*)malloc(5);").unwrap();
    writeln!(output, "    result->len = 4;").unwrap();
    writeln!(output, "    result->capacity = 5;").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    memcpy(result->data, &n, 4);").unwrap();
    writeln!(output, "    result->data[4] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MKD$(n) - pack double to 8-byte string
    writeln!(output, "qb_string* qb_mkd(double n) {{").unwrap();
    writeln!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )
    .unwrap();
    writeln!(output, "    result->data = (char*)malloc(9);").unwrap();
    writeln!(output, "    result->len = 8;").unwrap();
    writeln!(output, "    result->capacity = 9;").unwrap();
    writeln!(output, "    result->refcount = 1;").unwrap();
    writeln!(output, "    memcpy(result->data, &n, 8);").unwrap();
    writeln!(output, "    result->data[8] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // CVI(s$) - unpack 2-byte string to 16-bit integer
    writeln!(output, "int16_t qb_cvi(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s->len < 2) return 0;").unwrap();
    writeln!(output, "    int16_t result;").unwrap();
    writeln!(output, "    memcpy(&result, s->data, 2);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // CVL(s$) - unpack 4-byte string to 32-bit long
    writeln!(output, "int32_t qb_cvl(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s->len < 4) return 0;").unwrap();
    writeln!(output, "    int32_t result;").unwrap();
    writeln!(output, "    memcpy(&result, s->data, 4);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // CVS(s$) - unpack 4-byte string to single
    writeln!(output, "float qb_cvs(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s->len < 4) return 0.0f;").unwrap();
    writeln!(output, "    float result;").unwrap();
    writeln!(output, "    memcpy(&result, s->data, 4);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // CVD(s$) - unpack 8-byte string to double
    writeln!(output, "double qb_cvd(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s->len < 8) return 0.0;").unwrap();
    writeln!(output, "    double result;").unwrap();
    writeln!(output, "    memcpy(&result, s->data, 8);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== Classic BASIC Date/Time Functions ====================

    // TIMER - returns seconds elapsed since midnight as a SINGLE
    // Platform-specific includes are in emit_header()
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "float qb_timer(void) {{").unwrap();
    writeln!(output, "    SYSTEMTIME st;").unwrap();
    writeln!(output, "    GetLocalTime(&st);").unwrap();
    writeln!(
        output,
        "    return st.wHour * 3600.0f + st.wMinute * 60.0f + st.wSecond + st.wMilliseconds / 1000.0f;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "float qb_timer(void) {{").unwrap();
    writeln!(output, "    struct timeval tv;").unwrap();
    writeln!(output, "    gettimeofday(&tv, NULL);").unwrap();
    writeln!(output, "    struct tm* tm = localtime(&tv.tv_sec);").unwrap();
    writeln!(
        output,
        "    return tm->tm_hour * 3600.0f + tm->tm_min * 60.0f + tm->tm_sec + tv.tv_usec / 1000000.0f;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // TIMER with accuracy parameter - same as TIMER() but parameter is ignored
    // In QB64, the accuracy parameter is a hint for timing resolution
    writeln!(output, "float qb_timer_n(double accuracy) {{").unwrap();
    writeln!(
        output,
        "    (void)accuracy; /* Ignored - max precision always used */"
    )
    .unwrap();
    writeln!(output, "    return qb_timer();").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // DATE$ - returns date in MM-DD-YYYY format (classic QBasic format)
    writeln!(output, "qb_string* qb_date(void) {{").unwrap();
    writeln!(output, "    time_t t = time(NULL);").unwrap();
    writeln!(output, "    struct tm* tm = localtime(&t);").unwrap();
    writeln!(output, "    char buf[16];").unwrap();
    writeln!(output, "    strftime(buf, sizeof(buf), \"%m-%d-%Y\", tm);").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // TIME$ - returns time in HH:MM:SS format
    writeln!(output, "qb_string* qb_time(void) {{").unwrap();
    writeln!(output, "    time_t t = time(NULL);").unwrap();
    writeln!(output, "    struct tm* tm = localtime(&t);").unwrap();
    writeln!(output, "    char buf[16];").unwrap();
    writeln!(output, "    strftime(buf, sizeof(buf), \"%H:%M:%S\", tm);").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== Phase 2: QB64 Date/Time ====================

    // _DATE$ - returns date in YYYY-MM-DD format (QB64 format)
    writeln!(output, "qb_string* qb_date64(void) {{").unwrap();
    writeln!(output, "    time_t t = time(NULL);").unwrap();
    writeln!(output, "    struct tm* tm = localtime(&t);").unwrap();
    writeln!(output, "    char buf[16];").unwrap();
    writeln!(output, "    strftime(buf, sizeof(buf), \"%Y-%m-%d\", tm);").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _TIME$ - returns time in HH:MM:SS format (same as TIME$ but for consistency)
    writeln!(output, "qb_string* qb_time64(void) {{").unwrap();
    writeln!(output, "    time_t t = time(NULL);").unwrap();
    writeln!(output, "    struct tm* tm = localtime(&t);").unwrap();
    writeln!(output, "    char buf[16];").unwrap();
    writeln!(output, "    strftime(buf, sizeof(buf), \"%H:%M:%S\", tm);").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== Phase 2: Memory Operations ====================
    emit_memory_functions(output);
}

/// Emits memory operation functions for _MEM, _MEMNEW, _MEMFREE, etc.
fn emit_memory_functions(_output: &mut String) {
    // Placeholder for memory functions - will be implemented when this module is integrated
}
