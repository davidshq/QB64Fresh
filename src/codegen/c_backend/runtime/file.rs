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
//! - **COM port support**: OPEN "COM1:9600,N,8,1" AS #n opens a serial port (Windows:
//!   CreateFile + DCB; Unix: /dev/ttyS* + termios). PRINT #/INPUT # work unchanged.
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
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(
        output,
        "#include <sys/file.h>  /* flock for OPEN lock modes */"
    )?;
    writeln_code!(
        output,
        "#include <fcntl.h>  /* fcntl for LOCK/UNLOCK range locking */"
    )?;
    writeln_code!(output, "#include <unistd.h>  /* fileno */")?;
    writeln_code!(
        output,
        "#include <termios.h>  /* serial port config for OPEN COM */"
    )?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "#include <windows.h>  /* LockFile, UnlockFile, CreateFile, SetCommState */"
    )?;
    writeln_code!(
        output,
        "#include <io.h>  /* _get_osfhandle, _open_osfhandle, _fdopen, _close */"
    )?;
    writeln_code!(output, "#include <fcntl.h>  /* _O_RDWR, _O_BINARY */")?;
    writeln_code!(output, "#endif")?;
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

    // OPEN access/lock constants (match qb64fresh_rt.h)
    writeln_code!(output, "#define QB_FILE_ACCESS_DEFAULT 0")?;
    writeln_code!(output, "#define QB_FILE_ACCESS_READ    1")?;
    writeln_code!(output, "#define QB_FILE_ACCESS_WRITE   2")?;
    writeln_code!(output, "#define QB_FILE_ACCESS_READ_WRITE 3")?;
    writeln_code!(output, "#define QB_FILE_LOCK_DEFAULT    0")?;
    writeln_code!(output, "#define QB_FILE_LOCK_SHARED     1")?;
    writeln_code!(output, "#define QB_FILE_LOCK_READ       2")?;
    writeln_code!(output, "#define QB_FILE_LOCK_WRITE      3")?;
    writeln_code!(output, "#define QB_FILE_LOCK_READ_WRITE 4")?;
    writeln_code!(output, "#define QB_FILE_LOCK_ONLY       5")?;
    writeln_code!(output)?;

    // Serial COM port support: OPEN "COM1:9600,N,8,1" AS #n
    // _qb_serial_open returns FILE* (or NULL) so PRINT #/INPUT # work unchanged
    writeln_code!(
        output,
        "static FILE* _qb_serial_open(int port_num, int baud, char parity_char, int data_bits, int stop_bits) {{"
    )?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    char port_name[32];")?;
    writeln_code!(
        output,
        "    if (port_num > 9) sprintf(port_name, \"\\\\\\\\.\\\\COM%d\", port_num);"
    )?;
    writeln_code!(output, "    else sprintf(port_name, \"COM%d\", port_num);")?;
    writeln_code!(
        output,
        "    HANDLE h = CreateFileA(port_name, GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);"
    )?;
    writeln_code!(output, "    if (h == INVALID_HANDLE_VALUE) return NULL;")?;
    writeln_code!(output, "    DCB dcb = {{0}}; dcb.DCBlength = sizeof(DCB);")?;
    writeln_code!(
        output,
        "    if (!GetCommState(h, &dcb)) {{ CloseHandle(h); return NULL; }}"
    )?;
    writeln_code!(output, "    dcb.BaudRate = (DWORD)baud;")?;
    writeln_code!(output, "    dcb.ByteSize = (BYTE)data_bits;")?;
    writeln_code!(
        output,
        "    dcb.StopBits = (stop_bits == 2) ? TWOSTOPBITS : ONESTOPBIT;"
    )?;
    writeln_code!(
        output,
        "    if (parity_char == 'E' || parity_char == 'e') dcb.Parity = EVENPARITY;"
    )?;
    writeln_code!(
        output,
        "    else if (parity_char == 'O' || parity_char == 'o') dcb.Parity = ODDPARITY;"
    )?;
    writeln_code!(output, "    else dcb.Parity = NOPARITY;")?;
    writeln_code!(
        output,
        "    if (!SetCommState(h, &dcb)) {{ CloseHandle(h); return NULL; }}"
    )?;
    writeln_code!(
        output,
        "    int fd = _open_osfhandle((intptr_t)h, _O_RDWR|_O_BINARY);"
    )?;
    writeln_code!(
        output,
        "    if (fd == -1) {{ CloseHandle(h); return NULL; }}"
    )?;
    writeln_code!(output, "    FILE* f = _fdopen(fd, \"rb+\");")?;
    writeln_code!(output, "    if (!f) {{ _close(fd); return NULL; }}")?;
    writeln_code!(output, "    return f;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    char dev[64];")?;
    writeln_code!(output, "    sprintf(dev, \"/dev/ttyS%d\", port_num - 1);")?;
    writeln_code!(output, "    int fd = open(dev, O_RDWR | O_NOCTTY);")?;
    writeln_code!(output, "    if (fd < 0) return NULL;")?;
    writeln_code!(output, "    struct termios t;")?;
    writeln_code!(
        output,
        "    if (tcgetattr(fd, &t) != 0) {{ close(fd); return NULL; }}"
    )?;
    writeln_code!(output, "    speed_t speed = B9600;")?;
    writeln_code!(output, "    if (baud >= 115200) speed = B115200;")?;
    writeln_code!(output, "    else if (baud >= 57600) speed = B57600;")?;
    writeln_code!(output, "    else if (baud >= 38400) speed = B38400;")?;
    writeln_code!(output, "    else if (baud >= 19200) speed = B19200;")?;
    writeln_code!(output, "    else if (baud >= 9600) speed = B9600;")?;
    writeln_code!(output, "    else if (baud >= 4800) speed = B4800;")?;
    writeln_code!(output, "    else if (baud >= 2400) speed = B2400;")?;
    writeln_code!(output, "    else if (baud >= 1200) speed = B1200;")?;
    writeln_code!(output, "    else speed = B300;")?;
    writeln_code!(
        output,
        "    cfsetispeed(&t, speed); cfsetospeed(&t, speed);"
    )?;
    writeln_code!(output, "    t.c_cflag &= ~(CS5|CS6|CS7|CS8);")?;
    writeln_code!(output, "    if (data_bits == 5) t.c_cflag |= CS5;")?;
    writeln_code!(output, "    else if (data_bits == 6) t.c_cflag |= CS6;")?;
    writeln_code!(output, "    else if (data_bits == 7) t.c_cflag |= CS7;")?;
    writeln_code!(output, "    else t.c_cflag |= CS8;")?;
    writeln_code!(
        output,
        "    if (parity_char == 'E' || parity_char == 'e') {{ t.c_cflag |= PARENB; t.c_cflag &= ~PARODD; }}"
    )?;
    writeln_code!(
        output,
        "    else if (parity_char == 'O' || parity_char == 'o') {{ t.c_cflag |= PARENB|PARODD; }}"
    )?;
    writeln_code!(output, "    else t.c_cflag &= ~PARENB;")?;
    writeln_code!(
        output,
        "    if (stop_bits == 2) t.c_cflag |= CSTOPB; else t.c_cflag &= ~CSTOPB;"
    )?;
    writeln_code!(output, "    t.c_lflag &= ~(ICANON|ECHO|ECHOE|ISIG);")?;
    writeln_code!(output, "    t.c_iflag &= ~(IXON|IXOFF|IXANY|INLCR|ICRNL);")?;
    writeln_code!(output, "    t.c_oflag &= ~OPOST;")?;
    writeln_code!(output, "    t.c_cc[VMIN] = 0; t.c_cc[VTIME] = 10;")?;
    writeln_code!(
        output,
        "    if (tcsetattr(fd, TCSANOW, &t) != 0) {{ close(fd); return NULL; }}"
    )?;
    writeln_code!(output, "    FILE* f = fdopen(fd, \"r+\");")?;
    writeln_code!(output, "    if (!f) close(fd);")?;
    writeln_code!(output, "    return f;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_file_open - Open a file (access/lock: QB_FILE_ACCESS_*, QB_FILE_LOCK_*; 0 = default)
    // Handles path normalization, creating files for "r+b" mode if missing, flock on Unix, and COM port syntax
    writeln_code!(
        output,
        "void qb_file_open(int32_t fnum, const char* filename, const char* mode, int32_t access, int32_t lock) {{"
    )?;
    writeln_code!(output, "    (void)access;")?;
    writeln_code!(output, "    if (fnum < 1 || fnum >= QB_MAX_FILES) return;")?;
    writeln_code!(output, "    if (_qb_files[fnum]) fclose(_qb_files[fnum]);")?;
    writeln_code!(output, "    _qb_files[fnum] = NULL;")?;
    // COM port: OPEN "COM1:9600,N,8,1" AS #n (parity N/E/O, data 5-8, stop 1-2); COM10+ supported
    writeln_code!(
        output,
        "    if (filename && (filename[0]=='C'||filename[0]=='c') && (filename[1]=='O'||filename[1]=='o') && (filename[2]=='M'||filename[2]=='m') && filename[3]>='1'&&filename[3]<='9') {{"
    )?;
    writeln_code!(
        output,
        "        int port_num = 0; const char* p = filename+3;"
    )?;
    writeln_code!(
        output,
        "        while (*p >= '0' && *p <= '9') {{ int d = *p - '0'; if (port_num > 25 || (port_num == 25 && d > 6)) break; port_num = port_num*10 + d; p++; }}"
    )?;
    writeln_code!(
        output,
        "        if (port_num > 0 && port_num <= 256 && *p == ':') {{"
    )?;
    writeln_code!(
        output,
        "            int baud = 9600, data_bits = 8, stop_bits = 1;"
    )?;
    writeln_code!(output, "            char parity_char = 'N';")?;
    writeln_code!(
        output,
        "            if (p[1]) sscanf(p+1, \"%d,%c,%d,%d\", &baud, &parity_char, &data_bits, &stop_bits);"
    )?;
    writeln_code!(
        output,
        "            if (data_bits < 5) data_bits = 5; if (data_bits > 8) data_bits = 8;"
    )?;
    writeln_code!(
        output,
        "            if (stop_bits < 1) stop_bits = 1; if (stop_bits > 2) stop_bits = 2;"
    )?;
    writeln_code!(
        output,
        "            FILE* comf = _qb_serial_open(port_num, baud, parity_char, data_bits, stop_bits);"
    )?;
    writeln_code!(
        output,
        "            if (comf) {{ _qb_files[fnum] = comf; _qb_file_reclen[fnum] = 128; return; }}"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
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
    // Apply flock on Unix only for explicit lock modes (LOCK_READ, LOCK_WRITE, LOCK_READ_WRITE, ONLY)
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(
        output,
        "    if (_qb_files[fnum] && (lock == QB_FILE_LOCK_READ || lock == QB_FILE_LOCK_WRITE || lock == QB_FILE_LOCK_READ_WRITE || lock == QB_FILE_LOCK_ONLY)) {{"
    )?;
    writeln_code!(output, "        int fd = fileno(_qb_files[fnum]);")?;
    writeln_code!(output, "        if (fd >= 0) flock(fd, LOCK_EX);")?;
    writeln_code!(output, "    }}")?;
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
    writeln_code!(output, "    if (*s) qb_string_release(*s);")?;
    writeln_code!(output, "    *s = qb_string_retain(qb_string_new(buf));")?;
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
    // Assign to *s a string that survives qbs_cleanup: qb_string_new() registers in temp pool,
    // so we retain() so the variable holds a reference; release old *s first to avoid leak.
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
    writeln_code!(output, "        if (*s) qb_string_release(*s);")?;
    writeln_code!(output, "        *s = qb_string_retain(qb_string_new(buf));")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        if (*s) qb_string_release(*s);")?;
    writeln_code!(
        output,
        "        *s = qb_string_retain(qb_string_new(\"\"));"
    )?;
    writeln_code!(output, "    }}")?;
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

    // SEEK(filenum) - returns current file position (1-based), same as LOC
    writeln_code!(output, "int64_t qb_seek(int32_t fnum) {{")?;
    writeln_code!(
        output,
        "    if (fnum >= 1 && fnum < QB_MAX_FILES && _qb_files[fnum]) {{"
    )?;
    writeln_code!(
        output,
        "        return (int64_t)ftell(_qb_files[fnum]) + 1;"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int32_t qb_freefile(void) {{")?;
    writeln_code!(output, "    for (int i = 1; i < QB_MAX_FILES; i++)")?;
    writeln_code!(output, "        if (!_qb_files[i]) return i;")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // File locking functions (LOCK/UNLOCK statements)
    // LOCK #filenum - locks entire file (start=-1, end=-1 means lock all)
    // Note: Caller does not check return value; ON ERROR will not fire on lock failure (inline runtime).
    writeln_code!(
        output,
        "int32_t qb_file_lock(int32_t fnum, int64_t start, int64_t end) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum])"
    )?;
    writeln_code!(output, "        return -2; /* invalid handle */")?;
    writeln_code!(output, "    if (start == -1) start = 0;")?;
    writeln_code!(
        output,
        "    if (start < 0) return -4; /* illegal function call */"
    )?;
    writeln_code!(
        output,
        "    if (end < -1) return -4; /* illegal function call */"
    )?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    /* Unix: use flock for file locking */")?;
    writeln_code!(output, "    int fd = fileno(_qb_files[fnum]);")?;
    writeln_code!(output, "    if (fd < 0) return -2;")?;
    writeln_code!(
        output,
        "    /* Lock entire file if end == -1, otherwise lock range */"
    )?;
    writeln_code!(output, "    if (end == -1) {{")?;
    writeln_code!(output, "        if (flock(fd, LOCK_EX) != 0) {{")?;
    writeln_code!(output, "            return -7; /* permission denied */")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        /* Range locking: use fcntl F_SETLK */")?;
    writeln_code!(output, "        struct flock lock_info;")?;
    writeln_code!(output, "        lock_info.l_type = F_WRLCK;")?;
    writeln_code!(output, "        lock_info.l_whence = SEEK_SET;")?;
    writeln_code!(output, "        lock_info.l_start = (off_t)start;")?;
    writeln_code!(
        output,
        "        lock_info.l_len = (off_t)(end - start + 1);"
    )?;
    writeln_code!(output, "        lock_info.l_pid = 0;")?;
    writeln_code!(
        output,
        "        if (fcntl(fd, F_SETLK, &lock_info) != 0) {{"
    )?;
    writeln_code!(output, "            return -7; /* permission denied */")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    /* Windows: use LockFile */")?;
    writeln_code!(
        output,
        "    HANDLE hFile = (HANDLE)_get_osfhandle(fileno(_qb_files[fnum]));"
    )?;
    writeln_code!(output, "    if (hFile == INVALID_HANDLE_VALUE) return -2;")?;
    writeln_code!(output, "    DWORD nBytesLow, nBytesHigh;")?;
    writeln_code!(output, "    if (end == -1) {{")?;
    writeln_code!(
        output,
        "        /* Lock entire file: 0xFFFFFFFF,0xFFFFFFFF = to end of file */"
    )?;
    writeln_code!(output, "        nBytesLow = 0xFFFFFFFF;")?;
    writeln_code!(output, "        nBytesHigh = 0xFFFFFFFF;")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        int64_t nBytes = end - start + 1;")?;
    writeln_code!(output, "        nBytesLow = (DWORD)(nBytes & 0xFFFFFFFF);")?;
    writeln_code!(
        output,
        "        nBytesHigh = (DWORD)((nBytes >> 32) & 0xFFFFFFFF);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    DWORD dwStartLow = (DWORD)(start & 0xFFFFFFFF);"
    )?;
    writeln_code!(
        output,
        "    DWORD dwStartHigh = (DWORD)((start >> 32) & 0xFFFFFFFF);"
    )?;
    writeln_code!(
        output,
        "    if (!LockFile(hFile, dwStartLow, dwStartHigh, nBytesLow, nBytesHigh)) {{"
    )?;
    writeln_code!(output, "        DWORD e = GetLastError();")?;
    writeln_code!(
        output,
        "        if (e == ERROR_ACCESS_DENIED || e == ERROR_LOCK_VIOLATION)"
    )?;
    writeln_code!(output, "            return -7; /* permission denied */")?;
    writeln_code!(output, "        return -9; /* path/file access error */")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "int32_t qb_file_unlock(int32_t fnum, int64_t start, int64_t end) {{"
    )?;
    writeln_code!(
        output,
        "    if (fnum < 1 || fnum >= QB_MAX_FILES || !_qb_files[fnum])"
    )?;
    writeln_code!(output, "        return -2; /* invalid handle */")?;
    writeln_code!(output, "    if (start == -1) start = 0;")?;
    writeln_code!(
        output,
        "    if (start < 0) return -4; /* illegal function call */"
    )?;
    writeln_code!(
        output,
        "    if (end < -1) return -4; /* illegal function call */"
    )?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    /* Unix: use flock for file unlocking */")?;
    writeln_code!(output, "    int fd = fileno(_qb_files[fnum]);")?;
    writeln_code!(output, "    if (fd < 0) return -2;")?;
    writeln_code!(output, "    if (end == -1) {{")?;
    writeln_code!(output, "        if (flock(fd, LOCK_UN) != 0) {{")?;
    writeln_code!(output, "            return -7; /* permission denied */")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        /* Range unlocking: use fcntl F_SETLK */")?;
    writeln_code!(output, "        struct flock lock_info;")?;
    writeln_code!(output, "        lock_info.l_type = F_UNLCK;")?;
    writeln_code!(output, "        lock_info.l_whence = SEEK_SET;")?;
    writeln_code!(output, "        lock_info.l_start = (off_t)start;")?;
    writeln_code!(
        output,
        "        lock_info.l_len = (off_t)(end - start + 1);"
    )?;
    writeln_code!(output, "        lock_info.l_pid = 0;")?;
    writeln_code!(
        output,
        "        if (fcntl(fd, F_SETLK, &lock_info) != 0) {{"
    )?;
    writeln_code!(output, "            return -7; /* permission denied */")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    /* Windows: use UnlockFile */")?;
    writeln_code!(
        output,
        "    HANDLE hFile = (HANDLE)_get_osfhandle(fileno(_qb_files[fnum]));"
    )?;
    writeln_code!(output, "    if (hFile == INVALID_HANDLE_VALUE) return -2;")?;
    writeln_code!(output, "    DWORD nBytesLow, nBytesHigh;")?;
    writeln_code!(output, "    if (end == -1) {{")?;
    writeln_code!(
        output,
        "        /* Unlock entire file: must match LockFile(0,0,0xFFFFFFFF,0xFFFFFFFF) */"
    )?;
    writeln_code!(output, "        nBytesLow = 0xFFFFFFFF;")?;
    writeln_code!(output, "        nBytesHigh = 0xFFFFFFFF;")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        int64_t nBytes = end - start + 1;")?;
    writeln_code!(output, "        nBytesLow = (DWORD)(nBytes & 0xFFFFFFFF);")?;
    writeln_code!(
        output,
        "        nBytesHigh = (DWORD)((nBytes >> 32) & 0xFFFFFFFF);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    DWORD dwStartLow = (DWORD)(start & 0xFFFFFFFF);"
    )?;
    writeln_code!(
        output,
        "    DWORD dwStartHigh = (DWORD)((start >> 32) & 0xFFFFFFFF);"
    )?;
    writeln_code!(
        output,
        "    if (!UnlockFile(hFile, dwStartLow, dwStartHigh, nBytesLow, nBytesHigh)) {{"
    )?;
    writeln_code!(output, "        DWORD e = GetLastError();")?;
    writeln_code!(
        output,
        "        if (e == ERROR_ACCESS_DENIED || e == ERROR_LOCK_VIOLATION)"
    )?;
    writeln_code!(output, "            return -7; /* permission denied */")?;
    writeln_code!(output, "        return -9; /* path/file access error */")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#endif")?;
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
    writeln_code!(
        output,
        "    memset((char*)qb_string_data(*var), ' ', width);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_lset - Left-justify a string value into a fixed-length string variable
    writeln_code!(output, "void qb_lset(qb_string** var, qb_string* value) {{")?;
    writeln_code!(output, "    if (!*var || !value) return;")?;
    writeln_code!(
        output,
        "    int32_t var_len = (int32_t)qb_string_len(*var);"
    )?;
    writeln_code!(
        output,
        "    int32_t val_len = (int32_t)qb_string_len(value);"
    )?;
    writeln_code!(output, "    /* Fill with spaces first */")?;
    writeln_code!(
        output,
        "    memset((char*)qb_string_data(*var), ' ', var_len);"
    )?;
    writeln_code!(output, "    /* Copy value left-justified */")?;
    writeln_code!(
        output,
        "    int32_t copy_len = val_len < var_len ? val_len : var_len;"
    )?;
    writeln_code!(
        output,
        "    memcpy((char*)qb_string_data(*var), qb_string_data(value), copy_len);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_rset - Right-justify a string value into a fixed-length string variable
    writeln_code!(output, "void qb_rset(qb_string** var, qb_string* value) {{")?;
    writeln_code!(output, "    if (!*var || !value) return;")?;
    writeln_code!(
        output,
        "    int32_t var_len = (int32_t)qb_string_len(*var);"
    )?;
    writeln_code!(
        output,
        "    int32_t val_len = (int32_t)qb_string_len(value);"
    )?;
    writeln_code!(output, "    /* Fill with spaces first */")?;
    writeln_code!(
        output,
        "    memset((char*)qb_string_data(*var), ' ', var_len);"
    )?;
    writeln_code!(output, "    /* Copy value right-justified */")?;
    writeln_code!(
        output,
        "    int32_t copy_len = val_len < var_len ? val_len : var_len;"
    )?;
    writeln_code!(output, "    int32_t offset = var_len - copy_len;")?;
    writeln_code!(
        output,
        "    memcpy((char*)qb_string_data(*var) + offset, qb_string_data(value), copy_len);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
