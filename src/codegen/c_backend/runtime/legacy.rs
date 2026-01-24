//! Legacy DOS Functions and GOSUB Support
//!
//! This module contains runtime code emission for legacy DOS-era BASIC functions
//! and the GOSUB return address stack.
//!
//! ## Legacy DOS Functions
//!
//! These functions provide compatibility with DOS-era BASIC programs:
//!
//! - **Memory functions**: DEF SEG, PEEK, POKE, VARPTR, VARSEG, SADD, FRE
//! - **Port I/O**: INP, OUT, WAIT (with VGA palette emulation for ports 0x3C7-0x3C9, 0x3DA)
//! - **Binary file functions**: BLOAD, BSAVE
//! - **Microsoft Binary Format**: CVSMBF, CVDMBF, MKSMBF$, MKDMBF$
//! - **Joystick**: STICK, STRIG (stubs - return center position / not pressed)
//! - **Light pen**: PEN (stub with runtime warning - obsolete hardware)
//! - **Serial I/O**: ERDEV, ERDEV$, IOCTL, IOCTL$ (stubs with runtime warning)
//! - **System interrupts**: INTERRUPT, INTERRUPTX (INT 0x33 mouse only; others warn)
//! - **Event handling**: ON KEY, ON TIMER, ON STRIG, ON COM, ON PEN, ON UEVENT, ON SIGNAL
//!
//! ## Runtime Warnings
//!
//! Functions that are not supported on modern systems emit one-time warnings to stderr:
//!
//! - `PEN()` - Light pens are obsolete hardware
//! - `ERDEV/ERDEV$` - DOS device error functions
//! - `IOCTL/IOCTL$` - DOS device control functions
//! - `ON COM` - Serial port event trapping
//! - `ON PEN` - Light pen event trapping
//! - `ON UEVENT` - User event trapping
//! - `ON SIGNAL` - BASIC signal trapping
//! - `INTERRUPT/INTERRUPTX` for any interrupt other than INT 0x33 (mouse)
//!
//! ## INT 0x33 Mouse Emulation
//!
//! For compatibility with legacy mouse code, INT 0x33 is emulated:
//!
//! - AX=0: Check mouse installed (returns 0xFFFF, 2 buttons)
//! - AX=1: Show mouse cursor
//! - AX=2: Hide mouse cursor
//! - AX=3: Get position and button status
//! - AX=4: Set position (no-op)
//! - AX=5,6: Button press/release info (returns 0)
//! - AX=7,8: Set min/max range (no-op)
//!
//! ## FINAL IMPLEMENTATIONS (Not TODO Items)
//!
//! The following are **intentionally stub implementations** with no further work planned:
//!
//! | Function | Reason |
//! |----------|--------|
//! | `PEN()` | Light pens are obsolete CRT-era hardware |
//! | `ERDEV/ERDEV$` | DOS device driver errors - no modern equivalent |
//! | `IOCTL/IOCTL$` | DOS device control strings - no modern equivalent |
//! | `ON COM` | Hardware serial IRQ events - would need platform-specific async I/O |
//! | `ON PEN` | Light pen events - obsolete hardware |
//! | `ON UEVENT` | User events - rarely used, unclear modern mapping |
//! | `ON SIGNAL` | BASIC signals (not POSIX) - unclear modern mapping |
//! | `INTERRUPT` (non-0x33) | Real-mode x86 interrupts impossible on modern systems |
//!
//! These match QB64PE behavior where the functions exist but don't work on modern systems.
//!
//! ## GOSUB Support
//!
//! GOSUB is a legacy control flow statement that jumps to a label and RETURN
//! jumps back. We implement this using GCC's computed goto extension.

use std::fmt::Write;

/// Emits legacy BASIC functions for DOS compatibility.
///
/// These functions provide stubs or emulations for legacy DOS-era features:
/// - Memory functions (DEF SEG, PEEK, POKE, VARPTR, VARSEG, SADD, FRE)
/// - Port I/O with VGA palette emulation (INP, OUT, WAIT)
/// - Binary file functions (BLOAD, BSAVE)
/// - Microsoft Binary Format conversions (CVSMBF, CVDMBF, MKSMBF$, MKDMBF$)
/// - Joystick functions (STICK, STRIG)
/// - Light pen function (PEN)
/// - Serial I/O functions (ERDEV, ERDEV$, IOCTL, IOCTL$)
/// - System interrupt stubs (INTERRUPT, INTERRUPTX)
/// - Event handling stubs (ON KEY, ON TIMER, etc.)
pub(super) fn emit_legacy_functions(output: &mut String) {
    writeln!(output, "/* Legacy BASIC Functions */").unwrap();
    writeln!(output).unwrap();

    // LPOS(n) - printer position
    // Returns the current column position of the line printer
    // Tracks position for LPT output (matching QB64pe behavior)
    // lpos values: 0 = LPT1, 1 = LPT1, 2 = LPT2, 3 = LPT3
    writeln!(
        output,
        "static int qb_lpos_value = 1;  // Printer column position"
    )
    .unwrap();
    writeln!(output).unwrap();
    writeln!(output, "int qb_lpos(int64_t n) {{").unwrap();
    writeln!(
        output,
        "    if (n < 0 || n > 3) return 0;  // Invalid printer number"
    )
    .unwrap();
    writeln!(output, "    return qb_lpos_value;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
    // Helper to update lpos when printing to LPT
    writeln!(output, "void qb_lpos_update(int len, int has_newline) {{").unwrap();
    writeln!(output, "    if (has_newline) {{").unwrap();
    writeln!(output, "        qb_lpos_value = 1;").unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(output, "        qb_lpos_value += len;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // VARPTR(variable) - returns address of variable
    // In modern flat memory model, we return the actual pointer as a long
    // Note: This is typically called with a pointer to the variable
    writeln!(output, "int32_t qb_varptr(void* ptr) {{").unwrap();
    writeln!(output, "    return (int32_t)(intptr_t)ptr;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // VARPTR$(variable) - returns binary string representation of variable's address
    // Used for DRAW and PLAY statement's VARPTR$ support for accessing string data
    writeln!(output, "qb_string* qb_varptr_str(void* ptr) {{").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = sizeof(void*);").unwrap();
    writeln!(output, "    result->capacity = result->len + 1;").unwrap();
    writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
    writeln!(output, "    memcpy(result->data, &ptr, sizeof(void*));").unwrap();
    writeln!(output, "    result->data[result->len] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // VARSEG(variable) - returns segment address
    // In flat memory model, segment is meaningless, so we return 0
    writeln!(output, "int32_t qb_varseg(void* ptr) {{").unwrap();
    writeln!(output, "    (void)ptr;").unwrap();
    writeln!(output, "    return 0; // Flat memory model, no segments").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // SADD(string$) - returns address of string's data
    writeln!(output, "int32_t qb_sadd(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || !s->data) return 0;").unwrap();
    writeln!(output, "    return (int32_t)(intptr_t)(s->data);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // FILEATTR(filenum, attribute)
    // attribute = 1: returns file mode (1=INPUT, 2=OUTPUT, 4=RANDOM, 8=APPEND, 32=BINARY)
    // attribute = 2: returns DOS file handle (not meaningful in modern systems)
    writeln!(output, "int qb_fileattr(int filenum, int attribute) {{").unwrap();
    writeln!(output, "    (void)filenum; (void)attribute;").unwrap();
    writeln!(
        output,
        "    // Stub: would need to track file modes in file table"
    )
    .unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Microsoft Binary Format conversion functions
    // MBF was the floating point format used in GW-BASIC and older MS products
    // CVSMBF - Convert MBF single string to IEEE single
    writeln!(output, "float qb_cvsmbf(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s->len < 4) return 0.0f;").unwrap();
    writeln!(output, "    unsigned char* mbf = (unsigned char*)s->data;").unwrap();
    writeln!(
        output,
        "    // MBF format: mantissa(3 bytes) + exponent(1 byte)"
    )
    .unwrap();
    writeln!(
        output,
        "    // IEEE format: sign(1) + exponent(8) + mantissa(23)"
    )
    .unwrap();
    writeln!(output, "    if (mbf[3] == 0) return 0.0f; // Zero value").unwrap();
    writeln!(output, "    uint32_t ieee;").unwrap();
    writeln!(output, "    int sign = mbf[2] & 0x80;").unwrap();
    writeln!(
        output,
        "    int exp = mbf[3] - 2; // MBF to IEEE exponent adjustment"
    )
    .unwrap();
    writeln!(
        output,
        "    uint32_t mantissa = ((mbf[2] & 0x7F) << 16) | (mbf[1] << 8) | mbf[0];"
    )
    .unwrap();
    writeln!(
        output,
        "    ieee = (sign << 24) | ((exp & 0xFF) << 23) | (mantissa >> 1);"
    )
    .unwrap();
    writeln!(output, "    float result;").unwrap();
    writeln!(output, "    memcpy(&result, &ieee, 4);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // CVDMBF - Convert MBF double string to IEEE double
    writeln!(output, "double qb_cvdmbf(qb_string* s) {{").unwrap();
    writeln!(output, "    if (!s || s->len < 8) return 0.0;").unwrap();
    writeln!(output, "    unsigned char* mbf = (unsigned char*)s->data;").unwrap();
    writeln!(
        output,
        "    // MBF double: mantissa(7 bytes) + exponent(1 byte)"
    )
    .unwrap();
    writeln!(output, "    if (mbf[7] == 0) return 0.0; // Zero value").unwrap();
    writeln!(output, "    uint64_t ieee;").unwrap();
    writeln!(output, "    int sign = mbf[6] & 0x80;").unwrap();
    writeln!(
        output,
        "    int exp = mbf[7] - 2 + 1023 - 128; // MBF to IEEE exponent"
    )
    .unwrap();
    writeln!(
        output,
        "    uint64_t mantissa = ((uint64_t)(mbf[6] & 0x7F) << 48) |"
    )
    .unwrap();
    writeln!(output, "                        ((uint64_t)mbf[5] << 40) |").unwrap();
    writeln!(output, "                        ((uint64_t)mbf[4] << 32) |").unwrap();
    writeln!(output, "                        ((uint64_t)mbf[3] << 24) |").unwrap();
    writeln!(output, "                        ((uint64_t)mbf[2] << 16) |").unwrap();
    writeln!(output, "                        ((uint64_t)mbf[1] << 8) |").unwrap();
    writeln!(output, "                        (uint64_t)mbf[0];").unwrap();
    writeln!(
        output,
        "    ieee = ((uint64_t)sign << 56) | ((uint64_t)(exp & 0x7FF) << 52) | (mantissa >> 4);"
    )
    .unwrap();
    writeln!(output, "    double result;").unwrap();
    writeln!(output, "    memcpy(&result, &ieee, 8);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MKSMBF$ - Convert IEEE single to MBF string
    writeln!(output, "qb_string* qb_mksmbf(float n) {{").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = 4;").unwrap();
    writeln!(output, "    result->capacity = 5;").unwrap();
    writeln!(output, "    result->data = malloc(5);").unwrap();
    writeln!(output, "    if (n == 0.0f) {{").unwrap();
    writeln!(output, "        memset(result->data, 0, 4);").unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(output, "        uint32_t ieee;").unwrap();
    writeln!(output, "        memcpy(&ieee, &n, 4);").unwrap();
    writeln!(output, "        int sign = (ieee >> 31) & 1;").unwrap();
    writeln!(
        output,
        "        int exp = ((ieee >> 23) & 0xFF) + 2; // IEEE to MBF exponent"
    )
    .unwrap();
    writeln!(
        output,
        "        uint32_t mantissa = (ieee & 0x7FFFFF) << 1;"
    )
    .unwrap();
    writeln!(output, "        result->data[0] = mantissa & 0xFF;").unwrap();
    writeln!(output, "        result->data[1] = (mantissa >> 8) & 0xFF;").unwrap();
    writeln!(
        output,
        "        result->data[2] = ((mantissa >> 16) & 0x7F) | (sign << 7);"
    )
    .unwrap();
    writeln!(output, "        result->data[3] = exp & 0xFF;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    result->data[4] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // MKDMBF$ - Convert IEEE double to MBF string
    writeln!(output, "qb_string* qb_mkdmbf(double n) {{").unwrap();
    writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
    writeln!(output, "    result->len = 8;").unwrap();
    writeln!(output, "    result->capacity = 9;").unwrap();
    writeln!(output, "    result->data = malloc(9);").unwrap();
    writeln!(output, "    if (n == 0.0) {{").unwrap();
    writeln!(output, "        memset(result->data, 0, 8);").unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(output, "        uint64_t ieee;").unwrap();
    writeln!(output, "        memcpy(&ieee, &n, 8);").unwrap();
    writeln!(output, "        int sign = (ieee >> 63) & 1;").unwrap();
    writeln!(
        output,
        "        int exp = ((ieee >> 52) & 0x7FF) - 1023 + 128 + 2; // IEEE to MBF"
    )
    .unwrap();
    writeln!(
        output,
        "        uint64_t mantissa = (ieee & 0xFFFFFFFFFFFFFULL) << 4;"
    )
    .unwrap();
    writeln!(output, "        result->data[0] = mantissa & 0xFF;").unwrap();
    writeln!(output, "        result->data[1] = (mantissa >> 8) & 0xFF;").unwrap();
    writeln!(output, "        result->data[2] = (mantissa >> 16) & 0xFF;").unwrap();
    writeln!(output, "        result->data[3] = (mantissa >> 24) & 0xFF;").unwrap();
    writeln!(output, "        result->data[4] = (mantissa >> 32) & 0xFF;").unwrap();
    writeln!(output, "        result->data[5] = (mantissa >> 40) & 0xFF;").unwrap();
    writeln!(
        output,
        "        result->data[6] = ((mantissa >> 48) & 0x7F) | (sign << 7);"
    )
    .unwrap();
    writeln!(output, "        result->data[7] = exp & 0xFF;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    result->data[8] = '\\0';").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // BLOAD - Load binary file to memory
    // Original BSAVE format: 7-byte header with segment:offset and length
    // In modern systems, we provide a simplified version that works with arbitrary memory
    writeln!(
        output,
        "void qb_bload(const char* filename, void* address) {{"
    )
    .unwrap();
    writeln!(output, "    FILE* f = fopen(filename, \"rb\");").unwrap();
    writeln!(output, "#ifndef _WIN32").unwrap();
    writeln!(output, "    if (!f) {{").unwrap();
    writeln!(output, "        char* n = _qb_normalize_path(filename);").unwrap();
    writeln!(
        output,
        "        if (n) {{ f = fopen(n, \"rb\"); free(n); }}"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "    if (!f) return;").unwrap();
    writeln!(output, "    ").unwrap();
    writeln!(output, "    // Read BSAVE header (7 bytes)").unwrap();
    writeln!(output, "    unsigned char header[7];").unwrap();
    writeln!(output, "    if (fread(header, 1, 7, f) != 7) {{").unwrap();
    writeln!(output, "        fclose(f);").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    ").unwrap();
    writeln!(output, "    // Check magic byte (0xFD for BSAVE files)").unwrap();
    writeln!(output, "    if (header[0] != 0xFD) {{").unwrap();
    writeln!(output, "        // Not a BSAVE file, load as raw binary").unwrap();
    writeln!(output, "        fseek(f, 0, SEEK_END);").unwrap();
    writeln!(output, "        long size = ftell(f);").unwrap();
    writeln!(output, "        fseek(f, 0, SEEK_SET);").unwrap();
    writeln!(output, "        if (address) {{").unwrap();
    writeln!(output, "            fread(address, 1, (size_t)size, f);").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        fclose(f);").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    ").unwrap();
    writeln!(
        output,
        "    // Parse BSAVE header: magic(1) + segment(2) + offset(2) + length(2)"
    )
    .unwrap();
    writeln!(
        output,
        "    uint16_t length = header[5] | (header[6] << 8);"
    )
    .unwrap();
    writeln!(output, "    ").unwrap();
    writeln!(
        output,
        "    // If no address provided, we'd use the segment:offset from header"
    )
    .unwrap();
    writeln!(
        output,
        "    // In flat model, this is not directly applicable, so we require address"
    )
    .unwrap();
    writeln!(output, "    if (address) {{").unwrap();
    writeln!(output, "        fread(address, 1, length, f);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    fclose(f);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // BSAVE - Save memory to binary file
    writeln!(
        output,
        "void qb_bsave(const char* filename, void* address, size_t length) {{"
    )
    .unwrap();
    writeln!(output, "    FILE* f = fopen(filename, \"wb\");").unwrap();
    writeln!(output, "#ifndef _WIN32").unwrap();
    writeln!(output, "    if (!f) {{").unwrap();
    writeln!(output, "        char* n = _qb_normalize_path(filename);").unwrap();
    writeln!(
        output,
        "        if (n) {{ f = fopen(n, \"wb\"); free(n); }}"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "    if (!f) return;").unwrap();
    writeln!(output, "    ").unwrap();
    writeln!(output, "    // Write BSAVE header (7 bytes)").unwrap();
    writeln!(output, "    unsigned char header[7];").unwrap();
    writeln!(output, "    header[0] = 0xFD;  // Magic byte").unwrap();
    writeln!(
        output,
        "    header[1] = 0;     // Segment low (not used in flat model)"
    )
    .unwrap();
    writeln!(output, "    header[2] = 0;     // Segment high").unwrap();
    writeln!(output, "    header[3] = 0;     // Offset low (not used)").unwrap();
    writeln!(output, "    header[4] = 0;     // Offset high").unwrap();
    writeln!(
        output,
        "    header[5] = length & 0xFF;         // Length low"
    )
    .unwrap();
    writeln!(
        output,
        "    header[6] = (length >> 8) & 0xFF;  // Length high"
    )
    .unwrap();
    writeln!(output, "    ").unwrap();
    writeln!(output, "    fwrite(header, 1, 7, f);").unwrap();
    writeln!(output, "    if (address) {{").unwrap();
    writeln!(output, "        fwrite(address, 1, length, f);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    fclose(f);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 Event Handling Functions ====================
    writeln!(output, "/* QB4.5 Event Handling Functions (stubs) */").unwrap();
    writeln!(output).unwrap();

    // KEY(n) function - check key trap status
    // Returns: -1 = enabled, 0 = disabled, 1 = event pending but suspended
    writeln!(output, "int qb_key_status(int64_t n) {{").unwrap();
    writeln!(output, "    (void)n;").unwrap();
    writeln!(
        output,
        "    return 0; // Event trapping not fully implemented"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Event handler registration stubs
    // These would store the label address for calling when events occur
    writeln!(output, "void qb_on_key(int32_t key_num, void* target) {{").unwrap();
    writeln!(output, "    (void)key_num; (void)target;").unwrap();
    writeln!(
        output,
        "    // Key event trapping stub - would register handler"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_key_control(int32_t key_num, int mode) {{").unwrap();
    writeln!(output, "    (void)key_num; (void)mode;").unwrap();
    writeln!(
        output,
        "    // Key event control stub - 0=off, 1=on, 2=stop"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_on_timer(float interval, void* target) {{").unwrap();
    writeln!(output, "    (void)interval; (void)target;").unwrap();
    writeln!(output, "    // Timer event trapping stub").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_timer_control(int mode) {{").unwrap();
    writeln!(output, "    (void)mode;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_on_strig(int32_t button_num, void* target) {{"
    )
    .unwrap();
    writeln!(output, "    (void)button_num; (void)target;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_strig_control(int32_t button_num, int mode) {{"
    )
    .unwrap();
    writeln!(output, "    (void)button_num; (void)mode;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // FINAL IMPLEMENTATION: Legacy event trapping (ON COM/PEN/UEVENT/SIGNAL)
    // These are DOS-era event mechanisms that don't map well to modern systems:
    // - ON COM: Serial port interrupts (hardware IRQ-based)
    // - ON PEN: Light pen events (obsolete hardware)
    // - ON UEVENT: User-defined events (rarely used, no modern equivalent)
    // - ON SIGNAL: BASIC-specific signals (not POSIX signals)
    // These stubs with warnings are the complete implementation - no further work planned.
    // QB64PE also has these as non-functional stubs.
    // Warning flags for event handlers
    writeln!(output, "static int _qb_warned_on_com = 0;").unwrap();
    writeln!(output, "static int _qb_warned_on_pen = 0;").unwrap();
    writeln!(output, "static int _qb_warned_on_uevent = 0;").unwrap();
    writeln!(output, "static int _qb_warned_on_signal = 0;").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_on_com(int32_t port_num, void* target) {{").unwrap();
    writeln!(output, "    (void)port_num; (void)target;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_com) {{").unwrap();
    writeln!(output, "        _qb_warned_on_com = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON COM is not implemented (serial port event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_com_control(int32_t port_num, int mode) {{").unwrap();
    writeln!(output, "    (void)port_num; (void)mode;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_com) {{").unwrap();
    writeln!(output, "        _qb_warned_on_com = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON COM is not implemented (serial port event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_on_pen(void* target) {{").unwrap();
    writeln!(output, "    (void)target;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_pen) {{").unwrap();
    writeln!(output, "        _qb_warned_on_pen = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON PEN is not implemented (light pen event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_pen_control(int mode) {{").unwrap();
    writeln!(output, "    (void)mode;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_pen) {{").unwrap();
    writeln!(output, "        _qb_warned_on_pen = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON PEN is not implemented (light pen event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_on_uevent(void* target) {{").unwrap();
    writeln!(output, "    (void)target;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_uevent) {{").unwrap();
    writeln!(output, "        _qb_warned_on_uevent = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON UEVENT is not implemented (user event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_uevent_control(int mode) {{").unwrap();
    writeln!(output, "    (void)mode;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_uevent) {{").unwrap();
    writeln!(output, "        _qb_warned_on_uevent = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON UEVENT is not implemented (user event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_uevent_trigger(void) {{").unwrap();
    writeln!(output, "    if (!_qb_warned_on_uevent) {{").unwrap();
    writeln!(output, "        _qb_warned_on_uevent = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: UEVENT is not implemented (user event trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_on_signal(int32_t signal_num, void* target) {{"
    )
    .unwrap();
    writeln!(output, "    (void)signal_num; (void)target;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_signal) {{").unwrap();
    writeln!(output, "        _qb_warned_on_signal = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON SIGNAL is not implemented (BASIC signal trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_signal_control(int32_t signal_num, int mode) {{"
    )
    .unwrap();
    writeln!(output, "    (void)signal_num; (void)mode;").unwrap();
    writeln!(output, "    if (!_qb_warned_on_signal) {{").unwrap();
    writeln!(output, "        _qb_warned_on_signal = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ON SIGNAL is not implemented (BASIC signal trapping)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 Joystick Functions ====================
    writeln!(output, "/* QB4.5 Joystick Functions (stubs) */").unwrap();
    writeln!(output).unwrap();

    // STICK(n) - returns joystick position (stub returns center position)
    writeln!(output, "int qb_stick(int64_t n) {{").unwrap();
    writeln!(output, "    (void)n;").unwrap();
    writeln!(output, "    // Return center position (stub)").unwrap();
    writeln!(output, "    // n=0,2: X coordinate, n=1,3: Y coordinate").unwrap();
    writeln!(output, "    return 127; // Center of 0-255 range").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // STRIG(n) - returns joystick trigger state (stub returns 0 = not pressed)
    writeln!(output, "int qb_strig(int64_t n) {{").unwrap();
    writeln!(output, "    (void)n;").unwrap();
    writeln!(output, "    return 0; // Not pressed (stub)").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 Memory Functions ====================
    writeln!(output, "/* QB4.5 Memory Functions (stubs) */").unwrap();
    writeln!(output).unwrap();

    // FRE(n) - returns free memory
    // Modern systems have essentially unlimited memory compared to DOS
    writeln!(output, "int32_t qb_fre(int64_t n) {{").unwrap();
    writeln!(output, "    (void)n;").unwrap();
    writeln!(
        output,
        "    // Return a large value indicating plenty of memory"
    )
    .unwrap();
    writeln!(
        output,
        "    // n=-1: string space, n=-2: stack, n=0: far heap"
    )
    .unwrap();
    writeln!(
        output,
        "    return 64 * 1024 * 1024; // 64 MB (arbitrary large value)"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // FREE statement - no-op on modern systems
    writeln!(output, "void qb_free(void) {{").unwrap();
    writeln!(output, "    // String garbage collection is automatic").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 Port I/O Functions ====================
    // VGA palette port emulation matching QB64pe for legacy compatibility
    writeln!(
        output,
        "/* QB4.5 Port I/O Functions - VGA Palette Emulation */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // VGA palette state variables (matching QB64pe)
    writeln!(
        output,
        "static uint32_t _qb_palette[256];  // 256-color palette (ARGB format)"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_h3c7_read_index = 0;  // Palette read index (port 0x3C7)"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_h3c8_write_index = 0; // Palette write index (port 0x3C8)"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_h3c9_read_next = 0;   // Which RGB component to read next (0=R,1=G,2=B)"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_h3c9_write_next = 0;  // Which RGB component to write next"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_vertical_retrace = 0; // Simulated vertical retrace flag"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Initialize default VGA palette (called at program start)
    writeln!(output, "static void _qb_init_palette(void) {{").unwrap();
    writeln!(
        output,
        "    // Initialize with standard VGA palette (simplified)"
    )
    .unwrap();
    writeln!(output, "    for (int i = 0; i < 256; i++) {{").unwrap();
    writeln!(
        output,
        "        _qb_palette[i] = 0xFF000000 | (i << 16) | (i << 8) | i; // Grayscale default"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    // Standard 16-color VGA palette").unwrap();
    writeln!(output, "    _qb_palette[0] = 0xFF000000;  // Black").unwrap();
    writeln!(output, "    _qb_palette[1] = 0xFF0000AA;  // Blue").unwrap();
    writeln!(output, "    _qb_palette[2] = 0xFF00AA00;  // Green").unwrap();
    writeln!(output, "    _qb_palette[3] = 0xFF00AAAA;  // Cyan").unwrap();
    writeln!(output, "    _qb_palette[4] = 0xFFAA0000;  // Red").unwrap();
    writeln!(output, "    _qb_palette[5] = 0xFFAA00AA;  // Magenta").unwrap();
    writeln!(output, "    _qb_palette[6] = 0xFFAA5500;  // Brown").unwrap();
    writeln!(output, "    _qb_palette[7] = 0xFFAAAAAA;  // Light gray").unwrap();
    writeln!(output, "    _qb_palette[8] = 0xFF555555;  // Dark gray").unwrap();
    writeln!(output, "    _qb_palette[9] = 0xFF5555FF;  // Light blue").unwrap();
    writeln!(output, "    _qb_palette[10] = 0xFF55FF55; // Light green").unwrap();
    writeln!(output, "    _qb_palette[11] = 0xFF55FFFF; // Light cyan").unwrap();
    writeln!(output, "    _qb_palette[12] = 0xFFFF5555; // Light red").unwrap();
    writeln!(output, "    _qb_palette[13] = 0xFFFF55FF; // Light magenta").unwrap();
    writeln!(output, "    _qb_palette[14] = 0xFFFFFF55; // Yellow").unwrap();
    writeln!(output, "    _qb_palette[15] = 0xFFFFFFFF; // White").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // INP(port) - read byte from I/O port
    // Emulates VGA palette and status registers like QB64pe
    writeln!(output, "int qb_inp(int64_t port) {{").unwrap();
    writeln!(output, "    int p = (int)(port & 0xFFFF);").unwrap();
    writeln!(output, "    int value;").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    // Port 0x3C9: Read palette RGB values").unwrap();
    writeln!(output, "    if (p == 0x3C9) {{").unwrap();
    writeln!(
        output,
        "        uint32_t color = _qb_palette[_qb_h3c7_read_index];"
    )
    .unwrap();
    writeln!(output, "        if (_qb_h3c9_read_next == 0) {{ // Red").unwrap();
    writeln!(
        output,
        "            value = ((color >> 16) & 0xFF) >> 2; // Convert 0-255 to 0-63"
    )
    .unwrap();
    writeln!(
        output,
        "        }} else if (_qb_h3c9_read_next == 1) {{ // Green"
    )
    .unwrap();
    writeln!(output, "            value = ((color >> 8) & 0xFF) >> 2;").unwrap();
    writeln!(output, "        }} else {{ // Blue").unwrap();
    writeln!(output, "            value = (color & 0xFF) >> 2;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        _qb_h3c9_read_next++;").unwrap();
    writeln!(output, "        if (_qb_h3c9_read_next >= 3) {{").unwrap();
    writeln!(output, "            _qb_h3c9_read_next = 0;").unwrap();
    writeln!(
        output,
        "            _qb_h3c7_read_index = (_qb_h3c7_read_index + 1) & 0xFF;"
    )
    .unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        return value;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();
    writeln!(
        output,
        "    // Port 0x3DA: Input Status Register #1 (vertical retrace)"
    )
    .unwrap();
    writeln!(output, "    if (p == 0x3DA) {{").unwrap();
    writeln!(
        output,
        "        // Toggle vertical retrace bit to prevent infinite loops"
    )
    .unwrap();
    writeln!(
        output,
        "        _qb_vertical_retrace = !_qb_vertical_retrace;"
    )
    .unwrap();
    writeln!(
        output,
        "        return _qb_vertical_retrace ? 8 : 0; // Bit 3 = vertical retrace"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    // Unsupported port - return 0").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // OUT port, value - write byte to I/O port
    // Emulates VGA palette registers like QB64pe
    writeln!(output, "void qb_out(int32_t port, int32_t value) {{").unwrap();
    writeln!(output, "    int p = port & 0xFFFF;").unwrap();
    writeln!(output, "    int v = value & 0xFF;").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    // Port 0x3C7: Set palette read index").unwrap();
    writeln!(output, "    if (p == 0x3C7) {{").unwrap();
    writeln!(output, "        _qb_h3c7_read_index = v;").unwrap();
    writeln!(output, "        _qb_h3c9_read_next = 0;").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    // Port 0x3C8: Set palette write index").unwrap();
    writeln!(output, "    if (p == 0x3C8) {{").unwrap();
    writeln!(output, "        _qb_h3c8_write_index = v;").unwrap();
    writeln!(output, "        _qb_h3c9_write_next = 0;").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "    // Port 0x3C9: Write palette RGB values").unwrap();
    writeln!(output, "    if (p == 0x3C9) {{").unwrap();
    writeln!(
        output,
        "        int rgb = (v & 63) << 2; // Convert 0-63 to 0-252"
    )
    .unwrap();
    writeln!(output, "        if (_qb_h3c9_write_next == 0) {{ // Red").unwrap();
    writeln!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] &= 0xFF00FFFF;"
    )
    .unwrap();
    writeln!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] |= (rgb << 16);"
    )
    .unwrap();
    writeln!(
        output,
        "        }} else if (_qb_h3c9_write_next == 1) {{ // Green"
    )
    .unwrap();
    writeln!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] &= 0xFFFF00FF;"
    )
    .unwrap();
    writeln!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] |= (rgb << 8);"
    )
    .unwrap();
    writeln!(output, "        }} else {{ // Blue").unwrap();
    writeln!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] &= 0xFFFFFF00;"
    )
    .unwrap();
    writeln!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] |= rgb;"
    )
    .unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        _qb_h3c9_write_next++;").unwrap();
    writeln!(output, "        if (_qb_h3c9_write_next >= 3) {{").unwrap();
    writeln!(output, "            _qb_h3c9_write_next = 0;").unwrap();
    writeln!(
        output,
        "            _qb_h3c8_write_index = (_qb_h3c8_write_index + 1) & 0xFF;"
    )
    .unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();
    writeln!(
        output,
        "    // Port 0x3C0: Attribute controller (blink enable, etc.) - no-op"
    )
    .unwrap();
    writeln!(output, "    // Other ports: silently ignored").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // WAIT port, and_mask [, xor_mask] - wait for hardware port condition
    // Like QB64pe, returns immediately for unsupported ports so program can continue
    writeln!(
        output,
        "void qb_wait(int32_t port, int32_t and_mask, int32_t xor_mask) {{"
    )
    .unwrap();
    writeln!(output, "    int p = port & 0xFFFF;").unwrap();
    writeln!(output, "    int value;").unwrap();
    writeln!(output).unwrap();
    writeln!(
        output,
        "    // Only emulate WAIT for vertical retrace (port 0x3DA)"
    )
    .unwrap();
    writeln!(output, "    if (p == 0x3DA) {{").unwrap();
    writeln!(output, "        // Simulate waiting for vertical retrace").unwrap();
    writeln!(
        output,
        "        // Toggle the retrace flag to prevent infinite loops"
    )
    .unwrap();
    writeln!(output, "        _qb_vertical_retrace = 1;").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output).unwrap();
    writeln!(
        output,
        "    // For unsupported ports, return immediately (like QB64pe)"
    )
    .unwrap();
    writeln!(output, "    // This prevents infinite loops in legacy code").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 Light Pen Function ====================
    // FINAL IMPLEMENTATION: Light pens are obsolete hardware (CRT-era input devices).
    // This stub with warning is the complete implementation - no further work planned.
    // QB64PE also has this as a non-functional stub.
    writeln!(output, "/* QB4.5 Light Pen Function (stub with warning) */").unwrap();
    writeln!(output, "static int _qb_warned_pen = 0;").unwrap();
    writeln!(output).unwrap();

    // PEN(n) - returns light pen information (always 0 - no light pen)
    writeln!(output, "int qb_pen(int64_t n) {{").unwrap();
    writeln!(output, "    (void)n;").unwrap();
    writeln!(output, "    if (!_qb_warned_pen) {{").unwrap();
    writeln!(output, "        _qb_warned_pen = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: PEN() is not supported on modern systems (light pens are obsolete hardware)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return 0; // Light pen not present").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 Serial I/O Functions ====================
    // FINAL IMPLEMENTATION: ERDEV/IOCTL are DOS-specific device control functions.
    // They relied on DOS device drivers which don't exist on modern systems.
    // These stubs with warnings are the complete implementation - no further work planned.
    // QB64PE also has these as non-functional stubs.
    writeln!(
        output,
        "/* QB4.5 Serial I/O Functions (stubs with warnings) */"
    )
    .unwrap();
    writeln!(output, "static int _qb_warned_erdev = 0;").unwrap();
    writeln!(output, "static int _qb_warned_ioctl = 0;").unwrap();
    writeln!(output).unwrap();

    // ERDEV - device error code
    writeln!(output, "int qb_erdev(void) {{").unwrap();
    writeln!(output, "    if (!_qb_warned_erdev) {{").unwrap();
    writeln!(output, "        _qb_warned_erdev = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ERDEV/ERDEV$ are not supported (DOS device error functions)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return 0; // No device error").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ERDEV$ - device error name
    writeln!(output, "qb_string* qb_erdev_str(void) {{").unwrap();
    writeln!(output, "    if (!_qb_warned_erdev) {{").unwrap();
    writeln!(output, "        _qb_warned_erdev = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: ERDEV/ERDEV$ are not supported (DOS device error functions)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return qb_string_new(\"\"); // No device error").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // IOCTL statement
    writeln!(
        output,
        "void qb_ioctl(int32_t file_num, qb_string* control_string) {{"
    )
    .unwrap();
    writeln!(output, "    (void)file_num; (void)control_string;").unwrap();
    writeln!(output, "    if (!_qb_warned_ioctl) {{").unwrap();
    writeln!(output, "        _qb_warned_ioctl = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: IOCTL/IOCTL$ are not supported (DOS device control functions)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // IOCTL$ function - returns device status
    writeln!(output, "qb_string* qb_ioctl_str(int64_t file_num) {{").unwrap();
    writeln!(output, "    (void)file_num;").unwrap();
    writeln!(output, "    if (!_qb_warned_ioctl) {{").unwrap();
    writeln!(output, "        _qb_warned_ioctl = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: IOCTL/IOCTL$ are not supported (DOS device control functions)\\n\");").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    return qb_string_new(\"\"); // Empty status string"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ==================== QB4.5 System Interrupt Functions ====================
    // FINAL IMPLEMENTATION: INTERRUPT/INTERRUPTX with INT 0x33 mouse emulation.
    // Only INT 0x33 (mouse) is emulated - this matches QB64PE behavior.
    // Other interrupts (INT 0x10 video, INT 0x21 DOS, etc.) require real-mode x86
    // which is impossible on modern protected-mode/64-bit systems.
    // The INT 0x33 mouse emulation provides compatibility for legacy mouse code.
    // This is the complete implementation - no further work planned.
    writeln!(
        output,
        "/* QB4.5 System Interrupt Functions (INT 0x33 mouse emulation) */"
    )
    .unwrap();
    writeln!(output, "static int _qb_warned_interrupt = 0;").unwrap();
    writeln!(output).unwrap();

    // Internal function to emulate specific interrupts
    writeln!(
        output,
        "static void _qb_call_int(int32_t int_num, int16_t* regs) {{"
    )
    .unwrap();
    writeln!(output, "    /* regs: AX, BX, CX, DX, BP, SI, DI, FLAGS */").unwrap();
    writeln!(output, "    if (int_num == 0x33) {{").unwrap();
    writeln!(output, "        /* Mouse interrupt emulation */").unwrap();
    writeln!(output, "        int16_t ax = regs[0];").unwrap();
    writeln!(output, "        if (ax == 0) {{").unwrap();
    writeln!(output, "            /* Check mouse installed */").unwrap();
    writeln!(
        output,
        "            regs[0] = (int16_t)0xFFFF; /* Mouse installed */"
    )
    .unwrap();
    writeln!(output, "            regs[1] = 2; /* 2 buttons */").unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        if (ax == 1) {{").unwrap();
    writeln!(output, "            /* Show mouse cursor */").unwrap();
    writeln!(output, "            qb_mouse_show();").unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        if (ax == 2) {{").unwrap();
    writeln!(output, "            /* Hide mouse cursor */").unwrap();
    writeln!(output, "            qb_mouse_hide();").unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        if (ax == 3) {{").unwrap();
    writeln!(
        output,
        "            /* Get mouse position and button status */"
    )
    .unwrap();
    writeln!(output, "            int32_t buttons = 0;").unwrap();
    writeln!(output, "            if (qb_mouse_button(1)) buttons |= 1;").unwrap();
    writeln!(output, "            if (qb_mouse_button(2)) buttons |= 2;").unwrap();
    writeln!(output, "            if (qb_mouse_button(3)) buttons |= 4;").unwrap();
    writeln!(
        output,
        "            regs[1] = (int16_t)buttons; /* BX = buttons */"
    )
    .unwrap();
    writeln!(
        output,
        "            regs[2] = (int16_t)qb_mouse_x(); /* CX = X */"
    )
    .unwrap();
    writeln!(
        output,
        "            regs[3] = (int16_t)qb_mouse_y(); /* DX = Y */"
    )
    .unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        if (ax == 4) {{").unwrap();
    writeln!(output, "            /* Set mouse position - CX=X, DX=Y */").unwrap();
    writeln!(
        output,
        "            /* Note: qb_mouse_move may not exist in all backends */"
    )
    .unwrap();
    writeln!(
        output,
        "            /* For now, this is a no-op for compatibility */"
    )
    .unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(
        output,
        "        /* AX=5,6 (button press/release info) - no-op, returns 0 */"
    )
    .unwrap();
    writeln!(output, "        if (ax == 5 || ax == 6) {{").unwrap();
    writeln!(output, "            regs[0] = 0; /* No button info */").unwrap();
    writeln!(output, "            regs[1] = 0; /* Press count = 0 */").unwrap();
    writeln!(
        output,
        "            regs[2] = 0; regs[3] = 0; /* Position = 0,0 */"
    )
    .unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(
        output,
        "        /* AX=7,8 (min/max range) - no-op for compatibility */"
    )
    .unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    /* Unsupported interrupt - warn once */").unwrap();
    writeln!(output, "    if (!_qb_warned_interrupt) {{").unwrap();
    writeln!(output, "        _qb_warned_interrupt = 1;").unwrap();
    writeln!(output, "        fprintf(stderr, \"QB64Fresh: INTERRUPT/INTERRUPTX only supports INT 0x33 (mouse). Other interrupts (0x%02X) are ignored.\\n\", int_num);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // INTERRUPT - call system interrupt (RegType: 8 x int16)
    writeln!(
        output,
        "void qb_interrupt(int32_t int_num, void* in_regs, void* out_regs) {{"
    )
    .unwrap();
    writeln!(output, "    int16_t* in_r = (int16_t*)in_regs;").unwrap();
    writeln!(output, "    int16_t* out_r = (int16_t*)out_regs;").unwrap();
    writeln!(
        output,
        "    /* Copy input registers to output as working copy */"
    )
    .unwrap();
    writeln!(
        output,
        "    for (int i = 0; i < 8; i++) out_r[i] = in_r[i];"
    )
    .unwrap();
    writeln!(output, "    _qb_call_int(int_num, out_r);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // INTERRUPTX - extended system interrupt (RegTypeX: 10 x int16)
    writeln!(
        output,
        "void qb_interruptx(int32_t int_num, void* in_regs, void* out_regs) {{"
    )
    .unwrap();
    writeln!(output, "    int16_t* in_r = (int16_t*)in_regs;").unwrap();
    writeln!(output, "    int16_t* out_r = (int16_t*)out_regs;").unwrap();
    writeln!(
        output,
        "    /* Copy input registers to output as working copy */"
    )
    .unwrap();
    writeln!(
        output,
        "    for (int i = 0; i < 10; i++) out_r[i] = in_r[i];"
    )
    .unwrap();
    writeln!(output, "    _qb_call_int(int_num, out_r);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits the GOSUB return address stack.
///
/// GOSUB in BASIC is a "jump with return" - it jumps to a label and RETURN
/// jumps back to the statement after the GOSUB. We implement this using
/// GCC's computed goto extension (&&label gives the address of a label).
pub(super) fn emit_gosub_stack(output: &mut String) {
    writeln!(output, "/* GOSUB Return Stack */").unwrap();
    writeln!(output, "#define GOSUB_STACK_SIZE 256").unwrap();
    writeln!(output, "static void* _gosub_stack[GOSUB_STACK_SIZE];").unwrap();
    writeln!(output, "static int _gosub_sp = 0;").unwrap();
    writeln!(output).unwrap();
}
