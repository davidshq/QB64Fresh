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

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits keyboard input, environment, string, and date/time functions.
///
/// This function generates C code for various input and utility functions,
/// including platform-specific implementations for Windows and Unix systems.
pub(super) fn emit_keyboard_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Keyboard Input Functions */")?;
    writeln_code!(output)?;

    // Platform-specific non-blocking key check
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "#include <conio.h>")?;
    writeln_code!(output, "qb_string* qb_inkey(void) {{")?;
    writeln_code!(output, "    if (_kbhit()) {{")?;
    writeln_code!(output, "        int ch = _getch();")?;
    writeln_code!(output, "        if (ch == 0 || ch == 224) {{")?;
    writeln_code!(
        output,
        "            char buf[3] = {{0, (char)_getch(), 0}};"
    )?;
    writeln_code!(output, "            return qb_string_new(buf);")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        char buf[2] = {{(char)ch, 0}};")?;
    writeln_code!(output, "        return qb_string_new(buf);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    return &_qbs_empty; /* No allocation for empty */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "#include <termios.h>")?;
    writeln_code!(output, "#include <unistd.h>")?;
    writeln_code!(output, "#include <sys/select.h>")?;
    writeln_code!(output, "qb_string* qb_inkey(void) {{")?;
    writeln_code!(output, "    struct termios oldt, newt;")?;
    writeln_code!(output, "    tcgetattr(STDIN_FILENO, &oldt);")?;
    writeln_code!(output, "    newt = oldt;")?;
    writeln_code!(output, "    newt.c_lflag &= ~(ICANON | ECHO);")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);")?;
    writeln_code!(output, "    fd_set fds; struct timeval tv = {{0, 0}};")?;
    writeln_code!(output, "    FD_ZERO(&fds); FD_SET(STDIN_FILENO, &fds);")?;
    writeln_code!(
        output,
        "    qb_string* result = &_qbs_empty; /* No allocation for empty */"
    )?;
    writeln_code!(
        output,
        "    if (select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) > 0) {{"
    )?;
    writeln_code!(output, "        char buf[4] = {{0}};")?;
    writeln_code!(
        output,
        "        int n = read(STDIN_FILENO, buf, sizeof(buf)-1);"
    )?;
    writeln_code!(output, "        if (n > 0) result = qb_string_new(buf);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // INPUT$(n) - read n characters
    writeln_code!(output, "qb_string* qb_input_chars(int32_t n) {{")?;
    writeln_code!(output, "    if (n <= 0) return &_qbs_empty;")?;
    writeln_code!(output, "    char* buf = malloc((size_t)n + 1);")?;
    writeln_code!(
        output,
        "    for (int32_t i = 0; i < n; i++) buf[i] = (char)getchar();"
    )?;
    writeln_code!(output, "    buf[n] = '\\0';")?;
    writeln_code!(output, "    qb_string* result = qb_string_new(buf);")?;
    writeln_code!(output, "    free(buf);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // INPUT$(n, #fnum) - read n characters from file
    writeln_code!(
        output,
        "qb_string* qb_input_chars_file(int32_t n, int32_t fnum) {{"
    )?;
    writeln_code!(
        output,
        "    if (n <= 0 || fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum])"
    )?;
    writeln_code!(output, "        return &_qbs_empty;")?;
    writeln_code!(output, "    char* buf = malloc((size_t)n + 1);")?;
    writeln_code!(
        output,
        "    size_t read = fread(buf, 1, (size_t)n, _qb_files[fnum]);"
    )?;
    writeln_code!(output, "    buf[read] = '\\0';")?;
    writeln_code!(output, "    qb_string* result = qb_string_new(buf);")?;
    writeln_code!(output, "    free(buf);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // QB64 Keyboard Extensions
    writeln_code!(output, "/* QB64 Keyboard Extensions */")?;
    writeln_code!(output)?;

    // Keyboard buffer for _KEYHIT
    writeln_code!(output, "static int64_t _qb_keybuf[256];")?;
    writeln_code!(output, "static int _qb_keybuf_head = 0;")?;
    writeln_code!(output, "static int _qb_keybuf_tail = 0;")?;
    writeln_code!(output)?;

    // _KEYHIT - returns key code without waiting (0 if no key)
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "int64_t qb_keyhit(void) {{")?;
    writeln_code!(output, "    if (!_kbhit()) return 0;")?;
    writeln_code!(output, "    int ch = _getch();")?;
    writeln_code!(output, "    if (ch == 0 || ch == 224) {{")?;
    writeln_code!(output, "        int ext = _getch();")?;
    writeln_code!(
        output,
        "        return -(ext + 256); // Extended keys as negative"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return ch;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "int64_t qb_keyhit(void) {{")?;
    writeln_code!(output, "    struct termios oldt, newt;")?;
    writeln_code!(output, "    tcgetattr(STDIN_FILENO, &oldt);")?;
    writeln_code!(output, "    newt = oldt;")?;
    writeln_code!(output, "    newt.c_lflag &= ~(ICANON | ECHO);")?;
    writeln_code!(output, "    newt.c_cc[VMIN] = 0;")?;
    writeln_code!(output, "    newt.c_cc[VTIME] = 0;")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);")?;
    writeln_code!(output, "    int ch = getchar();")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);")?;
    writeln_code!(output, "    if (ch == EOF) return 0;")?;
    writeln_code!(output, "    if (ch == 27) {{ // Escape sequence")?;
    writeln_code!(output, "        tcsetattr(STDIN_FILENO, TCSANOW, &newt);")?;
    writeln_code!(output, "        int next = getchar();")?;
    writeln_code!(output, "        tcsetattr(STDIN_FILENO, TCSANOW, &oldt);")?;
    writeln_code!(output, "        if (next != EOF) return -(next + 256);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return ch;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // _KEYDOWN - check if specific key is pressed (simplified - checks buffer)
    writeln_code!(output, "int64_t qb_keydown(int64_t code) {{")?;
    writeln_code!(
        output,
        "    // Simplified implementation - always returns 0"
    )?;
    writeln_code!(
        output,
        "    // Full implementation requires platform-specific key state checking"
    )?;
    writeln_code!(output, "    (void)code;")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _KEYCLEAR - clear keyboard buffer
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "void qb_keyclear(void) {{")?;
    writeln_code!(output, "    while (_kbhit()) _getch();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "void qb_keyclear(void) {{")?;
    writeln_code!(output, "    tcflush(STDIN_FILENO, TCIFLUSH);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // _CINP - raw console input (returns character code, no echo)
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "int64_t qb_cinp(void) {{")?;
    writeln_code!(output, "    return _getch();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "int64_t qb_cinp(void) {{")?;
    writeln_code!(output, "    struct termios oldt, newt;")?;
    writeln_code!(output, "    tcgetattr(STDIN_FILENO, &oldt);")?;
    writeln_code!(output, "    newt = oldt;")?;
    writeln_code!(output, "    newt.c_lflag &= ~(ICANON | ECHO);")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);")?;
    writeln_code!(output, "    int ch = getchar();")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);")?;
    writeln_code!(output, "    return (ch == EOF) ? 0 : ch;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // Lock key state functions
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "int64_t qb_capslock(void) {{")?;
    writeln_code!(output, "    return (GetKeyState(VK_CAPITAL) & 1) ? -1 : 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "int64_t qb_numlock(void) {{")?;
    writeln_code!(output, "    return (GetKeyState(VK_NUMLOCK) & 1) ? -1 : 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "int64_t qb_scrolllock(void) {{")?;
    writeln_code!(output, "    return (GetKeyState(VK_SCROLL) & 1) ? -1 : 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "// On Linux/macOS, lock key states require X11 or reading /sys files"
    )?;
    writeln_code!(output, "// Simplified stub implementation")?;
    writeln_code!(output, "int64_t qb_capslock(void) {{ return 0; }}")?;
    writeln_code!(output, "int64_t qb_numlock(void) {{ return 0; }}")?;
    writeln_code!(output, "int64_t qb_scrolllock(void) {{ return 0; }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // Environment functions
    writeln_code!(output, "/* Environment Functions */")?;
    writeln_code!(output)?;

    // ENVIRON$(var$) - get environment variable
    writeln_code!(output, "qb_string* qb_environ(qb_string* var) {{")?;
    writeln_code!(output, "    if (!var || !var->data) return &_qbs_empty;")?;
    writeln_code!(output, "    const char* val = getenv(var->data);")?;
    writeln_code!(output, "    return val ? qb_string_new(val) : &_qbs_empty;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // COMMAND$ - get command line arguments
    writeln_code!(output, "static int _qb_argc = 0;")?;
    writeln_code!(output, "static char** _qb_argv = NULL;")?;
    writeln_code!(
        output,
        "void qb_init_args(int argc, char** argv) {{ _qb_argc = argc; _qb_argv = argv; }}"
    )?;
    writeln_code!(output)?;

    writeln_code!(output, "qb_string* qb_command(void) {{")?;
    writeln_code!(
        output,
        "    if (_qb_argc <= 1 || !_qb_argv) return &_qbs_empty;"
    )?;
    writeln_code!(output, "    size_t len = 0;")?;
    writeln_code!(
        output,
        "    for (int i = 1; i < _qb_argc; i++) len += strlen(_qb_argv[i]) + 1;"
    )?;
    writeln_code!(output, "    char* buf = malloc(len + 1);")?;
    writeln_code!(output, "    buf[0] = '\\0';")?;
    writeln_code!(output, "    for (int i = 1; i < _qb_argc; i++) {{")?;
    writeln_code!(output, "        if (i > 1) strcat(buf, \" \");")?;
    writeln_code!(output, "        strcat(buf, _qb_argv[i]);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    qb_string* result = qb_string_new(buf);")?;
    writeln_code!(output, "    free(buf);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // COMMAND$(n) - get specific command-line argument (1-based)
    writeln_code!(output, "qb_string* qb_command_n(int64_t n) {{")?;
    writeln_code!(
        output,
        "    if (n < 0 || n >= _qb_argc || !_qb_argv) return &_qbs_empty;"
    )?;
    writeln_code!(
        output,
        "    if (n == 0) return qb_command(); // Return all args"
    )?;
    writeln_code!(output, "    return qb_string_new(_qb_argv[n]);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _CWD$ - current working directory
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "#include <direct.h>")?;
    writeln_code!(output, "#define getcwd _getcwd")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "qb_string* qb_cwd(void) {{")?;
    writeln_code!(output, "    char buf[4096];")?;
    writeln_code!(
        output,
        "    if (getcwd(buf, sizeof(buf))) return qb_string_new(buf);"
    )?;
    writeln_code!(output, "    return &_qbs_empty;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _OS$ - operating system (QB64 format: [PLATFORM][BITS])
    writeln_code!(output, "qb_string* qb_os(void) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "  #if defined(_WIN64) || defined(__x86_64__)")?;
    writeln_code!(output, "    return qb_string_new(\"[WINDOWS][64BIT]\");")?;
    writeln_code!(output, "  #else")?;
    writeln_code!(output, "    return qb_string_new(\"[WINDOWS][32BIT]\");")?;
    writeln_code!(output, "  #endif")?;
    writeln_code!(output, "#elif defined(__APPLE__)")?;
    writeln_code!(output, "  #if defined(__x86_64__) || defined(__aarch64__)")?;
    writeln_code!(output, "    return qb_string_new(\"[MACOSX][64BIT]\");")?;
    writeln_code!(output, "  #else")?;
    writeln_code!(output, "    return qb_string_new(\"[MACOSX][32BIT]\");")?;
    writeln_code!(output, "  #endif")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "  #if defined(__x86_64__) || defined(__aarch64__)")?;
    writeln_code!(output, "    return qb_string_new(\"[LINUX][64BIT]\");")?;
    writeln_code!(output, "  #else")?;
    writeln_code!(output, "    return qb_string_new(\"[LINUX][32BIT]\");")?;
    writeln_code!(output, "  #endif")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Version$ - compiler version string (QB64 compatibility)
    // Returns version in format similar to QB64pe (e.g., "QB64Fresh 0.1.0")
    writeln_code!(output, "qb_string* qb_version(void) {{")?;
    writeln_code!(output, "    return qb_string_new(\"QB64Fresh 0.1.0\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _STARTDIR$ - program start directory
    writeln_code!(output, "static char _qb_startdir[4096] = {{}};")?;
    writeln_code!(
        output,
        "void qb_init_startdir(void) {{ getcwd(_qb_startdir, sizeof(_qb_startdir)); }}"
    )?;
    writeln_code!(
        output,
        "qb_string* qb_startdir(void) {{ return qb_string_new(_qb_startdir); }}"
    )?;
    writeln_code!(output)?;

    // ==================== Phase 2: String Enhancements ====================

    // _INSTRREV(source$, search$) - find last occurrence, 1-based, 0 if not found
    writeln_code!(
        output,
        "int32_t qb_instrrev(qb_string* source, qb_string* search) {{"
    )?;
    writeln_code!(
        output,
        "    if (!source || !search || search->len == 0) return 0;"
    )?;
    writeln_code!(output, "    if (search->len > source->len) return 0;")?;
    writeln_code!(
        output,
        "    for (int32_t i = source->len - search->len; i >= 0; i--) {{"
    )?;
    writeln_code!(
        output,
        "        if (memcmp(source->data + i, search->data, search->len) == 0) {{"
    )?;
    writeln_code!(output, "            return i + 1; // 1-based")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _TRIM$(s$) - trim whitespace from both ends
    writeln_code!(output, "qb_string* qb_trim(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len == 0) return &_qbs_empty;")?;
    writeln_code!(output, "    int32_t start = 0, end = s->len - 1;")?;
    writeln_code!(
        output,
        "    while (start <= end && (s->data[start] == ' ' || s->data[start] == '\\t')) start++;"
    )?;
    writeln_code!(
        output,
        "    while (end >= start && (s->data[end] == ' ' || s->data[end] == '\\t')) end--;"
    )?;
    writeln_code!(output, "    if (start > end) return &_qbs_empty;")?;
    writeln_code!(output, "    int32_t newlen = end - start + 1;")?;
    writeln_code!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )?;
    writeln_code!(output, "    result->data = (char*)malloc(newlen + 1);")?;
    writeln_code!(output, "    result->len = newlen;")?;
    writeln_code!(output, "    result->capacity = newlen + 1;")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    memcpy(result->data, s->data + start, newlen);")?;
    writeln_code!(output, "    result->data[newlen] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MKI$(n) - pack 16-bit integer to 2-byte string
    writeln_code!(output, "qb_string* qb_mki(int16_t n) {{")?;
    writeln_code!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )?;
    writeln_code!(output, "    result->data = (char*)malloc(3);")?;
    writeln_code!(output, "    result->len = 2;")?;
    writeln_code!(output, "    result->capacity = 3;")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    memcpy(result->data, &n, 2);")?;
    writeln_code!(output, "    result->data[2] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MKL$(n) - pack 32-bit long to 4-byte string
    writeln_code!(output, "qb_string* qb_mkl(int32_t n) {{")?;
    writeln_code!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )?;
    writeln_code!(output, "    result->data = (char*)malloc(5);")?;
    writeln_code!(output, "    result->len = 4;")?;
    writeln_code!(output, "    result->capacity = 5;")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    memcpy(result->data, &n, 4);")?;
    writeln_code!(output, "    result->data[4] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MKS$(n) - pack single to 4-byte string
    writeln_code!(output, "qb_string* qb_mks(float n) {{")?;
    writeln_code!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )?;
    writeln_code!(output, "    result->data = (char*)malloc(5);")?;
    writeln_code!(output, "    result->len = 4;")?;
    writeln_code!(output, "    result->capacity = 5;")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    memcpy(result->data, &n, 4);")?;
    writeln_code!(output, "    result->data[4] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MKD$(n) - pack double to 8-byte string
    writeln_code!(output, "qb_string* qb_mkd(double n) {{")?;
    writeln_code!(
        output,
        "    qb_string* result = (qb_string*)malloc(sizeof(qb_string));"
    )?;
    writeln_code!(output, "    result->data = (char*)malloc(9);")?;
    writeln_code!(output, "    result->len = 8;")?;
    writeln_code!(output, "    result->capacity = 9;")?;
    writeln_code!(output, "    result->refcount = 1;")?;
    writeln_code!(output, "    memcpy(result->data, &n, 8);")?;
    writeln_code!(output, "    result->data[8] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // CVI(s$) - unpack 2-byte string to 16-bit integer
    writeln_code!(output, "int16_t qb_cvi(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len < 2) return 0;")?;
    writeln_code!(output, "    int16_t result;")?;
    writeln_code!(output, "    memcpy(&result, s->data, 2);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // CVL(s$) - unpack 4-byte string to 32-bit long
    writeln_code!(output, "int32_t qb_cvl(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len < 4) return 0;")?;
    writeln_code!(output, "    int32_t result;")?;
    writeln_code!(output, "    memcpy(&result, s->data, 4);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // CVS(s$) - unpack 4-byte string to single
    writeln_code!(output, "float qb_cvs(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len < 4) return 0.0f;")?;
    writeln_code!(output, "    float result;")?;
    writeln_code!(output, "    memcpy(&result, s->data, 4);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // CVD(s$) - unpack 8-byte string to double
    writeln_code!(output, "double qb_cvd(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len < 8) return 0.0;")?;
    writeln_code!(output, "    double result;")?;
    writeln_code!(output, "    memcpy(&result, s->data, 8);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== Classic BASIC Date/Time Functions ====================

    // TIMER - returns seconds elapsed since midnight as a SINGLE
    // Platform-specific includes are in emit_header()
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "float qb_timer(void) {{")?;
    writeln_code!(output, "    SYSTEMTIME st;")?;
    writeln_code!(output, "    GetLocalTime(&st);")?;
    writeln_code!(
        output,
        "    return st.wHour * 3600.0f + st.wMinute * 60.0f + st.wSecond + st.wMilliseconds / 1000.0f;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "float qb_timer(void) {{")?;
    writeln_code!(output, "    struct timeval tv;")?;
    writeln_code!(output, "    gettimeofday(&tv, NULL);")?;
    writeln_code!(output, "    struct tm* tm = localtime(&tv.tv_sec);")?;
    writeln_code!(
        output,
        "    return tm->tm_hour * 3600.0f + tm->tm_min * 60.0f + tm->tm_sec + tv.tv_usec / 1000000.0f;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // TIMER with accuracy parameter - same as TIMER() but parameter is ignored
    // In QB64, the accuracy parameter is a hint for timing resolution
    writeln_code!(output, "float qb_timer_n(double accuracy) {{")?;
    writeln_code!(
        output,
        "    (void)accuracy; /* Ignored - max precision always used */"
    )?;
    writeln_code!(output, "    return qb_timer();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // DATE$ - returns date in MM-DD-YYYY format (classic QBasic format)
    writeln_code!(output, "qb_string* qb_date(void) {{")?;
    writeln_code!(output, "    time_t t = time(NULL);")?;
    writeln_code!(output, "    struct tm* tm = localtime(&t);")?;
    writeln_code!(output, "    char buf[16];")?;
    writeln_code!(output, "    strftime(buf, sizeof(buf), \"%m-%d-%Y\", tm);")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // TIME$ - returns time in HH:MM:SS format
    writeln_code!(output, "qb_string* qb_time(void) {{")?;
    writeln_code!(output, "    time_t t = time(NULL);")?;
    writeln_code!(output, "    struct tm* tm = localtime(&t);")?;
    writeln_code!(output, "    char buf[16];")?;
    writeln_code!(output, "    strftime(buf, sizeof(buf), \"%H:%M:%S\", tm);")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== Phase 2: QB64 Date/Time ====================

    // _DATE$ - returns date in YYYY-MM-DD format (QB64 format)
    writeln_code!(output, "qb_string* qb_date64(void) {{")?;
    writeln_code!(output, "    time_t t = time(NULL);")?;
    writeln_code!(output, "    struct tm* tm = localtime(&t);")?;
    writeln_code!(output, "    char buf[16];")?;
    writeln_code!(output, "    strftime(buf, sizeof(buf), \"%Y-%m-%d\", tm);")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _TIME$ - returns time in HH:MM:SS format (same as TIME$ but for consistency)
    writeln_code!(output, "qb_string* qb_time64(void) {{")?;
    writeln_code!(output, "    time_t t = time(NULL);")?;
    writeln_code!(output, "    struct tm* tm = localtime(&t);")?;
    writeln_code!(output, "    char buf[16];")?;
    writeln_code!(output, "    strftime(buf, sizeof(buf), \"%H:%M:%S\", tm);")?;
    writeln_code!(output, "    return qb_string_new(buf);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== Phase 2: Memory Operations ====================
    emit_memory_functions(output);
    Ok(())
}

/// Emits memory operation functions for _MEM, _MEMNEW, _MEMFREE, etc.
fn emit_memory_functions(_output: &mut String) {
    // Placeholder for memory functions - will be implemented when this module is integrated
}
