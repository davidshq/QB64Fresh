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
//! ## FINAL IMPLEMENTATIONS
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

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

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
pub(super) fn emit_legacy_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Legacy BASIC Functions */")?;
    writeln_code!(output)?;

    // LPOS(n) - printer position
    // Returns the current column position of the line printer
    // Tracks position for LPT output (matching QB64pe behavior)
    // lpos values: 0 = LPT1, 1 = LPT1, 2 = LPT2, 3 = LPT3
    writeln_code!(
        output,
        "static int qb_lpos_value = 1;  // Printer column position"
    )?;
    writeln_code!(output)?;
    writeln_code!(output, "int qb_lpos(int64_t n) {{")?;
    writeln_code!(
        output,
        "    if (n < 0 || n > 3) return 0;  // Invalid printer number"
    )?;
    writeln_code!(output, "    return qb_lpos_value;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    // Helper to update lpos when printing to LPT
    writeln_code!(output, "void qb_lpos_update(int len, int has_newline) {{")?;
    writeln_code!(output, "    if (has_newline) {{")?;
    writeln_code!(output, "        qb_lpos_value = 1;")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        qb_lpos_value += len;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // VARPTR(variable) - returns address of variable
    // In modern flat memory model, we return the actual pointer as a long
    // Note: This is typically called with a pointer to the variable
    writeln_code!(output, "int32_t qb_varptr(void* ptr) {{")?;
    writeln_code!(output, "    return (int32_t)(intptr_t)ptr;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // VARPTR$(variable) - returns binary string representation of variable's address
    // Used for DRAW and PLAY statement's VARPTR$ support for accessing string data
    writeln_code!(output, "qb_string* qb_varptr_str(void* ptr) {{")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = sizeof(void*);")?;
    writeln_code!(output, "    result->capacity = result->len + 1;")?;
    writeln_code!(output, "    result->data = malloc(result->capacity);")?;
    writeln_code!(output, "    memcpy(result->data, &ptr, sizeof(void*));")?;
    writeln_code!(output, "    result->data[result->len] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // VARSEG(variable) - returns segment address
    // In flat memory model, segment is meaningless, so we return 0
    writeln_code!(output, "int32_t qb_varseg(void* ptr) {{")?;
    writeln_code!(output, "    (void)ptr;")?;
    writeln_code!(output, "    return 0; // Flat memory model, no segments")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // SADD(string$) - returns address of string's data
    writeln_code!(output, "int32_t qb_sadd(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || !s->data) return 0;")?;
    writeln_code!(output, "    return (int32_t)(intptr_t)(s->data);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // FILEATTR(filenum, attribute)
    // attribute = 1: returns file mode (1=INPUT, 2=OUTPUT, 4=RANDOM, 8=APPEND, 32=BINARY)
    // attribute = 2: returns DOS file handle (not meaningful in modern systems)
    writeln_code!(output, "int qb_fileattr(int filenum, int attribute) {{")?;
    writeln_code!(output, "    (void)filenum; (void)attribute;")?;
    writeln_code!(
        output,
        "    // Stub: would need to track file modes in file table"
    )?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Microsoft Binary Format conversion functions
    // MBF was the floating point format used in GW-BASIC and older MS products
    // CVSMBF - Convert MBF single string to IEEE single
    writeln_code!(output, "float qb_cvsmbf(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len < 4) return 0.0f;")?;
    writeln_code!(output, "    unsigned char* mbf = (unsigned char*)s->data;")?;
    writeln_code!(
        output,
        "    // MBF format: mantissa(3 bytes) + exponent(1 byte)"
    )?;
    writeln_code!(
        output,
        "    // IEEE format: sign(1) + exponent(8) + mantissa(23)"
    )?;
    writeln_code!(output, "    if (mbf[3] == 0) return 0.0f; // Zero value")?;
    writeln_code!(output, "    uint32_t ieee;")?;
    writeln_code!(output, "    int sign = mbf[2] & 0x80;")?;
    writeln_code!(
        output,
        "    int exp = mbf[3] - 2; // MBF to IEEE exponent adjustment"
    )?;
    writeln_code!(
        output,
        "    uint32_t mantissa = ((mbf[2] & 0x7F) << 16) | (mbf[1] << 8) | mbf[0];"
    )?;
    writeln_code!(
        output,
        "    ieee = (sign << 24) | ((exp & 0xFF) << 23) | (mantissa >> 1);"
    )?;
    writeln_code!(output, "    float result;")?;
    writeln_code!(output, "    memcpy(&result, &ieee, 4);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // CVDMBF - Convert MBF double string to IEEE double
    writeln_code!(output, "double qb_cvdmbf(qb_string* s) {{")?;
    writeln_code!(output, "    if (!s || s->len < 8) return 0.0;")?;
    writeln_code!(output, "    unsigned char* mbf = (unsigned char*)s->data;")?;
    writeln_code!(
        output,
        "    // MBF double: mantissa(7 bytes) + exponent(1 byte)"
    )?;
    writeln_code!(output, "    if (mbf[7] == 0) return 0.0; // Zero value")?;
    writeln_code!(output, "    uint64_t ieee;")?;
    writeln_code!(output, "    int sign = mbf[6] & 0x80;")?;
    writeln_code!(
        output,
        "    int exp = mbf[7] - 2 + 1023 - 128; // MBF to IEEE exponent"
    )?;
    writeln_code!(
        output,
        "    uint64_t mantissa = ((uint64_t)(mbf[6] & 0x7F) << 48) |"
    )?;
    writeln_code!(output, "                        ((uint64_t)mbf[5] << 40) |")?;
    writeln_code!(output, "                        ((uint64_t)mbf[4] << 32) |")?;
    writeln_code!(output, "                        ((uint64_t)mbf[3] << 24) |")?;
    writeln_code!(output, "                        ((uint64_t)mbf[2] << 16) |")?;
    writeln_code!(output, "                        ((uint64_t)mbf[1] << 8) |")?;
    writeln_code!(output, "                        (uint64_t)mbf[0];")?;
    writeln_code!(
        output,
        "    ieee = ((uint64_t)sign << 56) | ((uint64_t)(exp & 0x7FF) << 52) | (mantissa >> 4);"
    )?;
    writeln_code!(output, "    double result;")?;
    writeln_code!(output, "    memcpy(&result, &ieee, 8);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MKSMBF$ - Convert IEEE single to MBF string
    writeln_code!(output, "qb_string* qb_mksmbf(float n) {{")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = 4;")?;
    writeln_code!(output, "    result->capacity = 5;")?;
    writeln_code!(output, "    result->data = malloc(5);")?;
    writeln_code!(output, "    if (n == 0.0f) {{")?;
    writeln_code!(output, "        memset(result->data, 0, 4);")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        uint32_t ieee;")?;
    writeln_code!(output, "        memcpy(&ieee, &n, 4);")?;
    writeln_code!(output, "        int sign = (ieee >> 31) & 1;")?;
    writeln_code!(
        output,
        "        int exp = ((ieee >> 23) & 0xFF) + 2; // IEEE to MBF exponent"
    )?;
    writeln_code!(
        output,
        "        uint32_t mantissa = (ieee & 0x7FFFFF) << 1;"
    )?;
    writeln_code!(output, "        result->data[0] = mantissa & 0xFF;")?;
    writeln_code!(output, "        result->data[1] = (mantissa >> 8) & 0xFF;")?;
    writeln_code!(
        output,
        "        result->data[2] = ((mantissa >> 16) & 0x7F) | (sign << 7);"
    )?;
    writeln_code!(output, "        result->data[3] = exp & 0xFF;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    result->data[4] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // MKDMBF$ - Convert IEEE double to MBF string
    writeln_code!(output, "qb_string* qb_mkdmbf(double n) {{")?;
    writeln_code!(output, "    qb_string* result = malloc(sizeof(qb_string));")?;
    writeln_code!(output, "    result->len = 8;")?;
    writeln_code!(output, "    result->capacity = 9;")?;
    writeln_code!(output, "    result->data = malloc(9);")?;
    writeln_code!(output, "    if (n == 0.0) {{")?;
    writeln_code!(output, "        memset(result->data, 0, 8);")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        uint64_t ieee;")?;
    writeln_code!(output, "        memcpy(&ieee, &n, 8);")?;
    writeln_code!(output, "        int sign = (ieee >> 63) & 1;")?;
    writeln_code!(
        output,
        "        int exp = ((ieee >> 52) & 0x7FF) - 1023 + 128 + 2; // IEEE to MBF"
    )?;
    writeln_code!(
        output,
        "        uint64_t mantissa = (ieee & 0xFFFFFFFFFFFFFULL) << 4;"
    )?;
    writeln_code!(output, "        result->data[0] = mantissa & 0xFF;")?;
    writeln_code!(output, "        result->data[1] = (mantissa >> 8) & 0xFF;")?;
    writeln_code!(output, "        result->data[2] = (mantissa >> 16) & 0xFF;")?;
    writeln_code!(output, "        result->data[3] = (mantissa >> 24) & 0xFF;")?;
    writeln_code!(output, "        result->data[4] = (mantissa >> 32) & 0xFF;")?;
    writeln_code!(output, "        result->data[5] = (mantissa >> 40) & 0xFF;")?;
    writeln_code!(
        output,
        "        result->data[6] = ((mantissa >> 48) & 0x7F) | (sign << 7);"
    )?;
    writeln_code!(output, "        result->data[7] = exp & 0xFF;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    result->data[8] = '\\0';")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // BLOAD - Load binary file to memory
    // Original BSAVE format: 7-byte header with segment:offset and length
    // In modern systems, we provide a simplified version that works with arbitrary memory
    writeln_code!(
        output,
        "void qb_bload(const char* filename, void* address) {{"
    )?;
    writeln_code!(output, "    FILE* f = fopen(filename, \"rb\");")?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    if (!f) {{")?;
    writeln_code!(output, "        char* n = _qb_normalize_path(filename);")?;
    writeln_code!(
        output,
        "        if (n) {{ f = fopen(n, \"rb\"); free(n); }}"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    if (!f) return;")?;
    writeln_code!(output, "    ")?;
    writeln_code!(output, "    // Read BSAVE header (7 bytes)")?;
    writeln_code!(output, "    unsigned char header[7];")?;
    writeln_code!(output, "    if (fread(header, 1, 7, f) != 7) {{")?;
    writeln_code!(output, "        fclose(f);")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    ")?;
    writeln_code!(output, "    // Check magic byte (0xFD for BSAVE files)")?;
    writeln_code!(output, "    if (header[0] != 0xFD) {{")?;
    writeln_code!(output, "        // Not a BSAVE file, load as raw binary")?;
    writeln_code!(output, "        fseek(f, 0, SEEK_END);")?;
    writeln_code!(output, "        long size = ftell(f);")?;
    writeln_code!(output, "        fseek(f, 0, SEEK_SET);")?;
    writeln_code!(output, "        if (address) {{")?;
    writeln_code!(output, "            fread(address, 1, (size_t)size, f);")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        fclose(f);")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    ")?;
    writeln_code!(
        output,
        "    // Parse BSAVE header: magic(1) + segment(2) + offset(2) + length(2)"
    )?;
    writeln_code!(
        output,
        "    uint16_t length = header[5] | (header[6] << 8);"
    )?;
    writeln_code!(output, "    ")?;
    writeln_code!(
        output,
        "    // If no address provided, we'd use the segment:offset from header"
    )?;
    writeln_code!(
        output,
        "    // In flat model, this is not directly applicable, so we require address"
    )?;
    writeln_code!(output, "    if (address) {{")?;
    writeln_code!(output, "        fread(address, 1, length, f);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    fclose(f);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // BSAVE - Save memory to binary file
    writeln_code!(
        output,
        "void qb_bsave(const char* filename, void* address, size_t length) {{"
    )?;
    writeln_code!(output, "    FILE* f = fopen(filename, \"wb\");")?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    if (!f) {{")?;
    writeln_code!(output, "        char* n = _qb_normalize_path(filename);")?;
    writeln_code!(
        output,
        "        if (n) {{ f = fopen(n, \"wb\"); free(n); }}"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    if (!f) return;")?;
    writeln_code!(output, "    ")?;
    writeln_code!(output, "    // Write BSAVE header (7 bytes)")?;
    writeln_code!(output, "    unsigned char header[7];")?;
    writeln_code!(output, "    header[0] = 0xFD;  // Magic byte")?;
    writeln_code!(
        output,
        "    header[1] = 0;     // Segment low (not used in flat model)"
    )?;
    writeln_code!(output, "    header[2] = 0;     // Segment high")?;
    writeln_code!(output, "    header[3] = 0;     // Offset low (not used)")?;
    writeln_code!(output, "    header[4] = 0;     // Offset high")?;
    writeln_code!(
        output,
        "    header[5] = length & 0xFF;         // Length low"
    )?;
    writeln_code!(
        output,
        "    header[6] = (length >> 8) & 0xFF;  // Length high"
    )?;
    writeln_code!(output, "    ")?;
    writeln_code!(output, "    fwrite(header, 1, 7, f);")?;
    writeln_code!(output, "    if (address) {{")?;
    writeln_code!(output, "        fwrite(address, 1, length, f);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    fclose(f);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 Event Handling Functions ====================
    writeln_code!(output, "/* QB4.5 Event Handling Functions (stubs) */")?;
    writeln_code!(output)?;

    // KEY(n) function - check key trap status
    // Returns: -1 = enabled, 0 = disabled, 1 = event pending but suspended
    writeln_code!(output, "int qb_key_status(int64_t n) {{")?;
    writeln_code!(output, "    (void)n;")?;
    writeln_code!(
        output,
        "    return 0; // Event trapping not fully implemented"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Event handler registration stubs
    // These would store the label address for calling when events occur
    writeln_code!(output, "void qb_on_key(int32_t key_num, void* target) {{")?;
    writeln_code!(output, "    (void)key_num; (void)target;")?;
    writeln_code!(
        output,
        "    // Key event trapping stub - would register handler"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_key_control(int32_t key_num, int mode) {{")?;
    writeln_code!(output, "    (void)key_num; (void)mode;")?;
    writeln_code!(
        output,
        "    // Key event control stub - 0=off, 1=on, 2=stop"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_on_timer(float interval, void* target) {{")?;
    writeln_code!(output, "    (void)interval; (void)target;")?;
    writeln_code!(output, "    // Timer event trapping stub")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_timer_control(int mode) {{")?;
    writeln_code!(output, "    (void)mode;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // STRIG event handling - uses event ID-based dispatch instead of computed goto
    // The generated code calls qb_strig_check_event() at loop iterations and
    // dispatches to the appropriate GOSUB label via a switch statement
    //
    // Event dispatch infrastructure - these variables are referenced by generated code
    // but actual dispatch happens via switch statements in each procedure
    writeln_code!(output, "static uint32_t _qb_strig_event_id = 0;")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_on_strig(int32_t button_num, uint32_t event_id) {{"
    )?;
    writeln_code!(output, "    (void)button_num; (void)event_id;")?;
    writeln_code!(
        output,
        "    /* Stub - full implementation in Rust runtime */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_strig_control(int32_t button_num, int mode) {{"
    )?;
    writeln_code!(output, "    (void)button_num; (void)mode;")?;
    writeln_code!(
        output,
        "    /* Stub - full implementation in Rust runtime */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "uint32_t qb_strig_check_event(void) {{")?;
    writeln_code!(
        output,
        "    return 0; /* No events in stub implementation */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_strig_event_done(void) {{")?;
    writeln_code!(
        output,
        "    /* Stub - full implementation in Rust runtime */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // FINAL IMPLEMENTATION: Legacy event trapping (ON COM/PEN/UEVENT/SIGNAL)
    // These are DOS-era event mechanisms that don't map well to modern systems:
    // - ON COM: Serial port interrupts (hardware IRQ-based)
    // - ON PEN: Light pen events (obsolete hardware)
    // - ON UEVENT: User-defined events (rarely used, no modern equivalent)
    // - ON SIGNAL: BASIC-specific signals (not POSIX signals)
    // These stubs with warnings are the complete implementation - no further work planned.
    // QB64PE also has these as non-functional stubs.
    // Warning flags for event handlers
    writeln_code!(output, "static int _qb_warned_on_com = 0;")?;
    writeln_code!(output, "static int _qb_warned_on_pen = 0;")?;
    writeln_code!(output, "static int _qb_warned_on_uevent = 0;")?;
    writeln_code!(output, "static int _qb_warned_on_signal = 0;")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_on_com(int32_t port_num, void* target) {{")?;
    writeln_code!(output, "    (void)port_num; (void)target;")?;
    writeln_code!(output, "    if (!_qb_warned_on_com) {{")?;
    writeln_code!(output, "        _qb_warned_on_com = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON COM is not implemented (serial port event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_com_control(int32_t port_num, int mode) {{")?;
    writeln_code!(output, "    (void)port_num; (void)mode;")?;
    writeln_code!(output, "    if (!_qb_warned_on_com) {{")?;
    writeln_code!(output, "        _qb_warned_on_com = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON COM is not implemented (serial port event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_on_pen(void* target) {{")?;
    writeln_code!(output, "    (void)target;")?;
    writeln_code!(output, "    if (!_qb_warned_on_pen) {{")?;
    writeln_code!(output, "        _qb_warned_on_pen = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON PEN is not implemented (light pen event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_pen_control(int mode) {{")?;
    writeln_code!(output, "    (void)mode;")?;
    writeln_code!(output, "    if (!_qb_warned_on_pen) {{")?;
    writeln_code!(output, "        _qb_warned_on_pen = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON PEN is not implemented (light pen event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_on_uevent(void* target) {{")?;
    writeln_code!(output, "    (void)target;")?;
    writeln_code!(output, "    if (!_qb_warned_on_uevent) {{")?;
    writeln_code!(output, "        _qb_warned_on_uevent = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON UEVENT is not implemented (user event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_uevent_control(int mode) {{")?;
    writeln_code!(output, "    (void)mode;")?;
    writeln_code!(output, "    if (!_qb_warned_on_uevent) {{")?;
    writeln_code!(output, "        _qb_warned_on_uevent = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON UEVENT is not implemented (user event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_uevent_trigger(void) {{")?;
    writeln_code!(output, "    if (!_qb_warned_on_uevent) {{")?;
    writeln_code!(output, "        _qb_warned_on_uevent = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: UEVENT is not implemented (user event trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_on_signal(int32_t signal_num, void* target) {{"
    )?;
    writeln_code!(output, "    (void)signal_num; (void)target;")?;
    writeln_code!(output, "    if (!_qb_warned_on_signal) {{")?;
    writeln_code!(output, "        _qb_warned_on_signal = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON SIGNAL is not implemented (BASIC signal trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_signal_control(int32_t signal_num, int mode) {{"
    )?;
    writeln_code!(output, "    (void)signal_num; (void)mode;")?;
    writeln_code!(output, "    if (!_qb_warned_on_signal) {{")?;
    writeln_code!(output, "        _qb_warned_on_signal = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ON SIGNAL is not implemented (BASIC signal trapping)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 Joystick Functions ====================
    writeln_code!(output, "/* QB4.5 Joystick Functions (stubs) */")?;
    writeln_code!(output)?;

    // STICK(n) - returns joystick position (stub returns center position)
    writeln_code!(output, "int qb_stick(int64_t n) {{")?;
    writeln_code!(output, "    (void)n;")?;
    writeln_code!(output, "    // Return center position (stub)")?;
    writeln_code!(output, "    // n=0,2: X coordinate, n=1,3: Y coordinate")?;
    writeln_code!(output, "    return 127; // Center of 0-255 range")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // STRIG(n) - returns joystick trigger state (stub returns 0 = not pressed)
    writeln_code!(output, "int qb_strig(int64_t n) {{")?;
    writeln_code!(output, "    (void)n;")?;
    writeln_code!(output, "    return 0; // Not pressed (stub)")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // STRIG(n, controller) - QB64 extension with explicit controller (stub)
    writeln_code!(output, "int qb_strig2(int64_t n, int64_t controller) {{")?;
    writeln_code!(output, "    (void)n; (void)controller;")?;
    writeln_code!(output, "    return 0; // Not pressed (stub)")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 Memory Functions ====================
    writeln_code!(output, "/* QB4.5 Memory Functions (stubs) */")?;
    writeln_code!(output)?;

    // FRE(n) - returns free memory (Long in BASIC; int64_t in C)
    // Modern systems have essentially unlimited memory compared to DOS
    writeln_code!(output, "int64_t qb_fre(int64_t n) {{")?;
    writeln_code!(output, "    (void)n;")?;
    writeln_code!(
        output,
        "    // Return a large value indicating plenty of memory"
    )?;
    writeln_code!(
        output,
        "    // n=-1: string space, n=-2: stack, n=0: far heap"
    )?;
    writeln_code!(
        output,
        "    return (int64_t)(64 * 1024 * 1024);  /* 64 MB (arbitrary large value) */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // FREE statement - no-op on modern systems
    writeln_code!(output, "void qb_free(void) {{")?;
    writeln_code!(output, "    // String garbage collection is automatic")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 Port I/O Functions ====================
    // VGA palette port emulation matching QB64pe for legacy compatibility
    writeln_code!(
        output,
        "/* QB4.5 Port I/O Functions - VGA Palette Emulation */"
    )?;
    writeln_code!(output)?;

    // VGA palette state variables (matching QB64pe)
    // Note: _qb_palette[256] is forward-declared in mod.rs for cross-module access
    writeln_code!(
        output,
        "static int _qb_h3c7_read_index = 0;  // Palette read index (port 0x3C7)"
    )?;
    writeln_code!(
        output,
        "static int _qb_h3c8_write_index = 0; // Palette write index (port 0x3C8)"
    )?;
    writeln_code!(
        output,
        "static int _qb_h3c9_read_next = 0;   // Which RGB component to read next (0=R,1=G,2=B)"
    )?;
    writeln_code!(
        output,
        "static int _qb_h3c9_write_next = 0;  // Which RGB component to write next"
    )?;
    writeln_code!(
        output,
        "static int _qb_vertical_retrace = 0; // Simulated vertical retrace flag"
    )?;
    writeln_code!(output)?;

    // Initialize default VGA palette (called at program start)
    writeln_code!(output, "static void _qb_init_palette(void) {{")?;
    writeln_code!(
        output,
        "    // Initialize with standard VGA palette (simplified)"
    )?;
    writeln_code!(output, "    for (int i = 0; i < 256; i++) {{")?;
    writeln_code!(
        output,
        "        _qb_palette[i] = 0xFF000000 | (i << 16) | (i << 8) | i; // Grayscale default"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    // Standard 16-color VGA palette")?;
    writeln_code!(output, "    _qb_palette[0] = 0xFF000000;  // Black")?;
    writeln_code!(output, "    _qb_palette[1] = 0xFF0000AA;  // Blue")?;
    writeln_code!(output, "    _qb_palette[2] = 0xFF00AA00;  // Green")?;
    writeln_code!(output, "    _qb_palette[3] = 0xFF00AAAA;  // Cyan")?;
    writeln_code!(output, "    _qb_palette[4] = 0xFFAA0000;  // Red")?;
    writeln_code!(output, "    _qb_palette[5] = 0xFFAA00AA;  // Magenta")?;
    writeln_code!(output, "    _qb_palette[6] = 0xFFAA5500;  // Brown")?;
    writeln_code!(output, "    _qb_palette[7] = 0xFFAAAAAA;  // Light gray")?;
    writeln_code!(output, "    _qb_palette[8] = 0xFF555555;  // Dark gray")?;
    writeln_code!(output, "    _qb_palette[9] = 0xFF5555FF;  // Light blue")?;
    writeln_code!(output, "    _qb_palette[10] = 0xFF55FF55; // Light green")?;
    writeln_code!(output, "    _qb_palette[11] = 0xFF55FFFF; // Light cyan")?;
    writeln_code!(output, "    _qb_palette[12] = 0xFFFF5555; // Light red")?;
    writeln_code!(output, "    _qb_palette[13] = 0xFFFF55FF; // Light magenta")?;
    writeln_code!(output, "    _qb_palette[14] = 0xFFFFFF55; // Yellow")?;
    writeln_code!(output, "    _qb_palette[15] = 0xFFFFFFFF; // White")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // port60h_event[] / port60h_events - keyboard port 0x60 scancode queue (libqb)
    // Stub: empty buffer so INP(&H60) returns 0; programs can push scancodes for testing.
    writeln_code!(output, "uint8_t port60h_event[256];")?;
    writeln_code!(output, "int32_t port60h_events = 0;")?;
    writeln_code!(output)?;

    // INP(port) - read byte from I/O port
    // Emulates VGA palette and status registers like QB64pe; port 0x60 = keyboard data
    writeln_code!(output, "int qb_inp(int64_t port) {{")?;
    writeln_code!(output, "    int p = (int)(port & 0xFFFF);")?;
    writeln_code!(output, "    int value;")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "    // Port 0x60: Keyboard controller data (libqb port60h_event)"
    )?;
    writeln_code!(output, "    if (p == 0x60) {{")?;
    writeln_code!(output, "        if (port60h_events > 0) {{")?;
    writeln_code!(output, "            value = port60h_event[0];")?;
    writeln_code!(output, "            if (port60h_events > 1) {{")?;
    writeln_code!(
        output,
        "                memmove(port60h_event, port60h_event + 1, 255);"
    )?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "            port60h_events--;")?;
    writeln_code!(output, "            return value;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        return 0;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Port 0x3C9: Read palette RGB values")?;
    writeln_code!(output, "    if (p == 0x3C9) {{")?;
    writeln_code!(
        output,
        "        uint32_t color = _qb_palette[_qb_h3c7_read_index];"
    )?;
    writeln_code!(output, "        if (_qb_h3c9_read_next == 0) {{ // Red")?;
    writeln_code!(
        output,
        "            value = ((color >> 16) & 0xFF) >> 2; // Convert 0-255 to 0-63"
    )?;
    writeln_code!(
        output,
        "        }} else if (_qb_h3c9_read_next == 1) {{ // Green"
    )?;
    writeln_code!(output, "            value = ((color >> 8) & 0xFF) >> 2;")?;
    writeln_code!(output, "        }} else {{ // Blue")?;
    writeln_code!(output, "            value = (color & 0xFF) >> 2;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        _qb_h3c9_read_next++;")?;
    writeln_code!(output, "        if (_qb_h3c9_read_next >= 3) {{")?;
    writeln_code!(output, "            _qb_h3c9_read_next = 0;")?;
    writeln_code!(
        output,
        "            _qb_h3c7_read_index = (_qb_h3c7_read_index + 1) & 0xFF;"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        return value;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "    // Port 0x3DA: Input Status Register #1 (vertical retrace)"
    )?;
    writeln_code!(output, "    if (p == 0x3DA) {{")?;
    writeln_code!(
        output,
        "        // Toggle vertical retrace bit to prevent infinite loops"
    )?;
    writeln_code!(
        output,
        "        _qb_vertical_retrace = !_qb_vertical_retrace;"
    )?;
    writeln_code!(
        output,
        "        return _qb_vertical_retrace ? 8 : 0; // Bit 3 = vertical retrace"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Unsupported port - return 0")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // OUT port, value - write byte to I/O port
    // Emulates VGA palette registers like QB64pe
    writeln_code!(output, "void qb_out(int32_t port, int32_t value) {{")?;
    writeln_code!(output, "    int p = port & 0xFFFF;")?;
    writeln_code!(output, "    int v = value & 0xFF;")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Port 0x3C7: Set palette read index")?;
    writeln_code!(output, "    if (p == 0x3C7) {{")?;
    writeln_code!(output, "        _qb_h3c7_read_index = v;")?;
    writeln_code!(output, "        _qb_h3c9_read_next = 0;")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Port 0x3C8: Set palette write index")?;
    writeln_code!(output, "    if (p == 0x3C8) {{")?;
    writeln_code!(output, "        _qb_h3c8_write_index = v;")?;
    writeln_code!(output, "        _qb_h3c9_write_next = 0;")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(output, "    // Port 0x3C9: Write palette RGB values")?;
    writeln_code!(output, "    if (p == 0x3C9) {{")?;
    writeln_code!(
        output,
        "        int rgb = (v & 63) << 2; // Convert 0-63 to 0-252"
    )?;
    writeln_code!(output, "        if (_qb_h3c9_write_next == 0) {{ // Red")?;
    writeln_code!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] &= 0xFF00FFFF;"
    )?;
    writeln_code!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] |= (rgb << 16);"
    )?;
    writeln_code!(
        output,
        "        }} else if (_qb_h3c9_write_next == 1) {{ // Green"
    )?;
    writeln_code!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] &= 0xFFFF00FF;"
    )?;
    writeln_code!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] |= (rgb << 8);"
    )?;
    writeln_code!(output, "        }} else {{ // Blue")?;
    writeln_code!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] &= 0xFFFFFF00;"
    )?;
    writeln_code!(
        output,
        "            _qb_palette[_qb_h3c8_write_index] |= rgb;"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        _qb_h3c9_write_next++;")?;
    writeln_code!(output, "        if (_qb_h3c9_write_next >= 3) {{")?;
    writeln_code!(output, "            _qb_h3c9_write_next = 0;")?;
    writeln_code!(
        output,
        "            _qb_h3c8_write_index = (_qb_h3c8_write_index + 1) & 0xFF;"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "    // Port 0x3C0: Attribute controller (blink enable, etc.) - no-op"
    )?;
    writeln_code!(output, "    // Other ports: silently ignored")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // WAIT port, and_mask [, xor_mask] - wait for hardware port condition
    // Like QB64pe, returns immediately for unsupported ports so program can continue
    writeln_code!(
        output,
        "void qb_wait(int32_t port, int32_t and_mask, int32_t xor_mask) {{"
    )?;
    writeln_code!(output, "    int p = port & 0xFFFF;")?;
    writeln_code!(output, "    int value;")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "    // Only emulate WAIT for vertical retrace (port 0x3DA)"
    )?;
    writeln_code!(output, "    if (p == 0x3DA) {{")?;
    writeln_code!(output, "        // Simulate waiting for vertical retrace")?;
    writeln_code!(
        output,
        "        // Toggle the retrace flag to prevent infinite loops"
    )?;
    writeln_code!(output, "        _qb_vertical_retrace = 1;")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "    // For unsupported ports, return immediately (like QB64pe)"
    )?;
    writeln_code!(output, "    // This prevents infinite loops in legacy code")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 Light Pen Function ====================
    // FINAL IMPLEMENTATION: Light pens are obsolete hardware (CRT-era input devices).
    // This stub with warning is the complete implementation - no further work planned.
    // QB64PE also has this as a non-functional stub.
    writeln_code!(output, "/* QB4.5 Light Pen Function (stub with warning) */")?;
    writeln_code!(output, "static int _qb_warned_pen = 0;")?;
    writeln_code!(output)?;

    // PEN(n) - returns light pen information (always 0 - no light pen)
    writeln_code!(output, "int qb_pen(int64_t n) {{")?;
    writeln_code!(output, "    (void)n;")?;
    writeln_code!(output, "    if (!_qb_warned_pen) {{")?;
    writeln_code!(output, "        _qb_warned_pen = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: PEN() is not supported on modern systems (light pens are obsolete hardware)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0; // Light pen not present")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 Serial I/O Functions ====================
    // FINAL IMPLEMENTATION: ERDEV/IOCTL are DOS-specific device control functions.
    // They relied on DOS device drivers which don't exist on modern systems.
    // These stubs with warnings are the complete implementation - no further work planned.
    // QB64PE also has these as non-functional stubs.
    writeln_code!(
        output,
        "/* QB4.5 Serial I/O Functions (stubs with warnings) */"
    )?;
    writeln_code!(output, "static int _qb_warned_erdev = 0;")?;
    writeln_code!(output, "static int _qb_warned_ioctl = 0;")?;
    writeln_code!(output)?;

    // ERDEV - device error code
    writeln_code!(output, "int qb_erdev(void) {{")?;
    writeln_code!(output, "    if (!_qb_warned_erdev) {{")?;
    writeln_code!(output, "        _qb_warned_erdev = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ERDEV/ERDEV$ are not supported (DOS device error functions)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0; // No device error")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ERDEV$ - device error name
    writeln_code!(output, "qb_string* qb_erdev_str(void) {{")?;
    writeln_code!(output, "    if (!_qb_warned_erdev) {{")?;
    writeln_code!(output, "        _qb_warned_erdev = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: ERDEV/ERDEV$ are not supported (DOS device error functions)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return qb_string_new(\"\"); // No device error")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // IOCTL statement
    writeln_code!(
        output,
        "void qb_ioctl(int32_t file_num, qb_string* control_string) {{"
    )?;
    writeln_code!(output, "    (void)file_num; (void)control_string;")?;
    writeln_code!(output, "    if (!_qb_warned_ioctl) {{")?;
    writeln_code!(output, "        _qb_warned_ioctl = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: IOCTL/IOCTL$ are not supported (DOS device control functions)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // IOCTL$ function - returns device status
    writeln_code!(output, "qb_string* qb_ioctl_str(int64_t file_num) {{")?;
    writeln_code!(output, "    (void)file_num;")?;
    writeln_code!(output, "    if (!_qb_warned_ioctl) {{")?;
    writeln_code!(output, "        _qb_warned_ioctl = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: IOCTL/IOCTL$ are not supported (DOS device control functions)\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    return qb_string_new(\"\"); // Empty status string"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ==================== QB4.5 System Interrupt Functions ====================
    // FINAL IMPLEMENTATION: INTERRUPT/INTERRUPTX with INT 0x33 mouse emulation.
    // Only INT 0x33 (mouse) is emulated - this matches QB64PE behavior.
    // Other interrupts (INT 0x10 video, INT 0x21 DOS, etc.) require real-mode x86
    // which is impossible on modern protected-mode/64-bit systems.
    // The INT 0x33 mouse emulation provides compatibility for legacy mouse code.
    // This is the complete implementation - no further work planned.
    writeln_code!(
        output,
        "/* QB4.5 System Interrupt Functions (INT 0x33 mouse emulation) */"
    )?;
    writeln_code!(output, "static int _qb_warned_interrupt = 0;")?;
    writeln_code!(output)?;

    // Internal function to emulate specific interrupts
    writeln_code!(
        output,
        "static void _qb_call_int(int32_t int_num, int16_t* regs) {{"
    )?;
    writeln_code!(output, "    /* regs: AX, BX, CX, DX, BP, SI, DI, FLAGS */")?;
    writeln_code!(output, "    if (int_num == 0x33) {{")?;
    writeln_code!(output, "        /* Mouse interrupt emulation */")?;
    writeln_code!(output, "        int16_t ax = regs[0];")?;
    writeln_code!(output, "        if (ax == 0) {{")?;
    writeln_code!(output, "            /* Check mouse installed */")?;
    writeln_code!(
        output,
        "            regs[0] = (int16_t)0xFFFF; /* Mouse installed */"
    )?;
    writeln_code!(output, "            regs[1] = 2; /* 2 buttons */")?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        if (ax == 1) {{")?;
    writeln_code!(output, "            /* Show mouse cursor */")?;
    writeln_code!(output, "            qb_mouse_show();")?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        if (ax == 2) {{")?;
    writeln_code!(output, "            /* Hide mouse cursor */")?;
    writeln_code!(output, "            qb_mouse_hide();")?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        if (ax == 3) {{")?;
    writeln_code!(
        output,
        "            /* Get mouse position and button status */"
    )?;
    writeln_code!(output, "            int32_t buttons = 0;")?;
    writeln_code!(output, "            if (qb_mouse_button(1)) buttons |= 1;")?;
    writeln_code!(output, "            if (qb_mouse_button(2)) buttons |= 2;")?;
    writeln_code!(output, "            if (qb_mouse_button(3)) buttons |= 4;")?;
    writeln_code!(
        output,
        "            regs[1] = (int16_t)buttons; /* BX = buttons */"
    )?;
    writeln_code!(
        output,
        "            regs[2] = (int16_t)qb_mouse_x(); /* CX = X */"
    )?;
    writeln_code!(
        output,
        "            regs[3] = (int16_t)qb_mouse_y(); /* DX = Y */"
    )?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        if (ax == 4) {{")?;
    writeln_code!(output, "            /* Set mouse position - CX=X, DX=Y */")?;
    writeln_code!(
        output,
        "            /* Note: qb_mouse_move may not exist in all backends */"
    )?;
    writeln_code!(
        output,
        "            /* For now, this is a no-op for compatibility */"
    )?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(
        output,
        "        /* AX=5,6 (button press/release info) - no-op, returns 0 */"
    )?;
    writeln_code!(output, "        if (ax == 5 || ax == 6) {{")?;
    writeln_code!(output, "            regs[0] = 0; /* No button info */")?;
    writeln_code!(output, "            regs[1] = 0; /* Press count = 0 */")?;
    writeln_code!(
        output,
        "            regs[2] = 0; regs[3] = 0; /* Position = 0,0 */"
    )?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(
        output,
        "        /* AX=7,8 (min/max range) - no-op for compatibility */"
    )?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    /* Unsupported interrupt - warn once */")?;
    writeln_code!(output, "    if (!_qb_warned_interrupt) {{")?;
    writeln_code!(output, "        _qb_warned_interrupt = 1;")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"QB64Fresh: INTERRUPT/INTERRUPTX only supports INT 0x33 (mouse). Other interrupts (0x%02X) are ignored.\\n\", int_num);"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // INTERRUPT - call system interrupt (RegType: 8 x int16)
    writeln_code!(
        output,
        "void qb_interrupt(int32_t int_num, void* in_regs, void* out_regs) {{"
    )?;
    writeln_code!(output, "    int16_t* in_r = (int16_t*)in_regs;")?;
    writeln_code!(output, "    int16_t* out_r = (int16_t*)out_regs;")?;
    writeln_code!(
        output,
        "    /* Copy input registers to output as working copy */"
    )?;
    writeln_code!(
        output,
        "    for (int i = 0; i < 8; i++) out_r[i] = in_r[i];"
    )?;
    writeln_code!(output, "    _qb_call_int(int_num, out_r);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // INTERRUPTX - extended system interrupt (RegTypeX: 10 x int16)
    writeln_code!(
        output,
        "void qb_interruptx(int32_t int_num, void* in_regs, void* out_regs) {{"
    )?;
    writeln_code!(output, "    int16_t* in_r = (int16_t*)in_regs;")?;
    writeln_code!(output, "    int16_t* out_r = (int16_t*)out_regs;")?;
    writeln_code!(
        output,
        "    /* Copy input registers to output as working copy */"
    )?;
    writeln_code!(
        output,
        "    for (int i = 0; i < 10; i++) out_r[i] = in_r[i];"
    )?;
    writeln_code!(output, "    _qb_call_int(int_num, out_r);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits the GOSUB return address stack.
///
/// GOSUB in BASIC is a "jump with return" - it jumps to a label and RETURN
/// jumps back to the statement after the GOSUB. We implement this using
/// GCC's computed goto extension (&&label gives the address of a label).
pub(super) fn emit_gosub_stack(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* GOSUB Return Stack */")?;
    writeln_code!(output, "#define GOSUB_STACK_SIZE 256")?;
    writeln_code!(output, "static void* _gosub_stack[GOSUB_STACK_SIZE];")?;
    writeln_code!(output, "static int _gosub_sp = 0;")?;
    writeln_code!(output)?;
    Ok(())
}
