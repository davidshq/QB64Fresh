//! Math and built-in function code generation for the C backend.
//!
//! This module contains functions that emit C code for mathematical operations
//! and core built-in functions like LEN, CHR$, and ASC.
//!
//! ## Functions Emitted
//!
//! ### Built-in Functions (`emit_builtin_functions`)
//! - `qb_len_str` - String length (LEN for strings)
//! - `qb_chr` - Character from ASCII code (CHR$)
//! - `qb_asc` - ASCII code from character (ASC)
//! - `qb_asc2` - ASCII code at position (ASC with position argument)
//!
//! ### Math Functions (`emit_math_functions`)
//! - `qb_sgn` - Sign function (SGN)
//! - `qb_pi` - Pi constant (_PI)
//! - `qb_clamp` - Clamp value to range (_CLAMP)
//! - `qb_randomize`, `qb_randomize_timer`, `qb_rnd` - Random number generation
//! - Bitwise operations: `qb_shl`, `qb_shr`, `qb_rol`, `qb_ror`
//! - Bit manipulation: `qb_readbit`, `qb_setbit`, `qb_resetbit`, `qb_togglebit`
//! - Trigonometric: `qb_sec`, `qb_csc`, `qb_cot` and their hyperbolic/inverse variants
//! - Angle conversions: `qb_d2r`, `qb_r2d`, `qb_d2g`, `qb_g2d`, `qb_g2r`, `qb_r2g`
//! - String comparison: `qb_strcmp`, `qb_stricmp`

use std::fmt::Write;

/// Emits built-in functions for LEN, CHR$, and ASC.
///
/// These are fundamental string manipulation functions used throughout
/// QB64 programs. The generated C functions handle:
///
/// - `qb_len_str` - Returns the length of a QB string (0 for null)
/// - `qb_chr` - Creates a single-character string from an ASCII code
/// - `qb_asc` - Returns the ASCII code of the first character
/// - `qb_asc2` - Returns the ASCII code at a specific position (1-indexed)
pub(super) fn emit_builtin_functions(output: &mut String) {
    // String length function
    // Note: The code generator uses qb_len_str for strings and sizeof for numerics
    writeln!(output, "int32_t qb_len_str(qb_string* s) {{").unwrap();
    writeln!(output, "    return s ? (int32_t)s->len : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "qb_string* qb_chr(int32_t code) {{").unwrap();
    writeln!(output, "    char buf[2] = {{ (char)code, '\\0' }};").unwrap();
    writeln!(output, "    return qb_string_new(buf);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int32_t qb_asc(qb_string* s) {{").unwrap();
    writeln!(
        output,
        "    return (s && s->len > 0) ? (unsigned char)s->data[0] : 0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Two-argument version: ASC(string$, position%) - gets ASCII code at position
    writeln!(output, "int32_t qb_asc2(qb_string* s, int32_t pos) {{").unwrap();
    writeln!(
        output,
        "    if (!s || pos < 1 || pos > (int32_t)s->len) return 0;"
    )
    .unwrap();
    writeln!(output, "    return (unsigned char)s->data[pos - 1];").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}

/// Emits math helper functions.
///
/// This function generates a comprehensive set of mathematical helper functions
/// for the C runtime, including:
///
/// - Basic math: SGN, PI, CLAMP, NEGATE
/// - Random numbers: RANDOMIZE, RANDOMIZE TIMER, RND (xorshift64 algorithm)
/// - Bitwise operations: _SHL, _SHR, _ROL, _ROR
/// - Bit manipulation: _READBIT, _SETBIT, _RESETBIT, _TOGGLEBIT
/// - Reciprocal trigonometry: SEC, CSC, COT
/// - Hyperbolic reciprocals: SECH, CSCH, COTH
/// - Inverse reciprocal trig: ARCSEC, ARCCSC, ARCCOT
/// - Inverse hyperbolic reciprocals: ARCSECH, ARCCSCH, ARCCOTH
/// - Angle conversions: _D2R, _R2D, _D2G, _G2D, _G2R, _R2G
/// - String comparison: STRCMP, STRICMP (case-insensitive)
pub(super) fn emit_math_functions(output: &mut String) {
    writeln!(output, "int32_t qb_sgn(double n) {{").unwrap();
    writeln!(output, "    return (n > 0) - (n < 0);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "double qb_pi(void) {{").unwrap();
    writeln!(output, "    return 3.14159265358979323846;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "double qb_clamp(double value, double min, double max) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    return value < min ? min : (value > max ? max : value);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Random number generator state (xorshift64)
    writeln!(output, "static uint64_t qb_rng_state = 0x853c49e6748fea9b;").unwrap();
    writeln!(output).unwrap();

    // RANDOMIZE with specific seed
    writeln!(output, "void qb_randomize(double seed) {{").unwrap();
    writeln!(output, "    union {{ double d; uint64_t u; }} conv;").unwrap();
    writeln!(output, "    conv.d = seed;").unwrap();
    writeln!(output, "    qb_rng_state = conv.u;").unwrap();
    writeln!(output, "    if (qb_rng_state == 0) qb_rng_state = 1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // RANDOMIZE TIMER - seed with system time
    writeln!(output, "void qb_randomize_timer(void) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    LARGE_INTEGER pc;").unwrap();
    writeln!(output, "    QueryPerformanceCounter(&pc);").unwrap();
    writeln!(output, "    qb_rng_state = (uint64_t)pc.QuadPart;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    struct timespec ts;").unwrap();
    writeln!(output, "    clock_gettime(CLOCK_REALTIME, &ts);").unwrap();
    writeln!(
        output,
        "    qb_rng_state = (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;"
    )
    .unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "    if (qb_rng_state == 0) qb_rng_state = 1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // RND function - returns random float 0.0 to 1.0
    writeln!(output, "float qb_rnd(float n) {{").unwrap();
    writeln!(output, "    if (n < 0.0f) {{").unwrap();
    writeln!(
        output,
        "        /* Negative seed: reseed and return first value */"
    )
    .unwrap();
    writeln!(output, "        qb_randomize((double)n);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    if (n != 0.0f) {{").unwrap();
    writeln!(output, "        /* xorshift64 algorithm */").unwrap();
    writeln!(output, "        qb_rng_state ^= qb_rng_state << 13;").unwrap();
    writeln!(output, "        qb_rng_state ^= qb_rng_state >> 7;").unwrap();
    writeln!(output, "        qb_rng_state ^= qb_rng_state << 17;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(
        output,
        "    return (float)((double)qb_rng_state / (double)UINT64_MAX);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Bitwise operations
    // _SHL - shift left
    writeln!(output, "int64_t qb_shl(int64_t value, int64_t bits) {{").unwrap();
    writeln!(
        output,
        "    return (int64_t)((uint64_t)value << (bits & 63));"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SHR - shift right (arithmetic)
    writeln!(output, "int64_t qb_shr(int64_t value, int64_t bits) {{").unwrap();
    writeln!(output, "    return value >> (bits & 63);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _ROL - rotate left (64-bit)
    writeln!(output, "int64_t qb_rol(int64_t value, int64_t bits) {{").unwrap();
    writeln!(output, "    uint64_t v = (uint64_t)value;").unwrap();
    writeln!(output, "    int n = (int)(bits & 63);").unwrap();
    writeln!(output, "    return (int64_t)((v << n) | (v >> (64 - n)));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _ROR - rotate right (64-bit)
    writeln!(output, "int64_t qb_ror(int64_t value, int64_t bits) {{").unwrap();
    writeln!(output, "    uint64_t v = (uint64_t)value;").unwrap();
    writeln!(output, "    int n = (int)(bits & 63);").unwrap();
    writeln!(output, "    return (int64_t)((v >> n) | (v << (64 - n)));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _READBIT - read a specific bit (0 = rightmost)
    writeln!(output, "int64_t qb_readbit(int64_t value, int64_t bit) {{").unwrap();
    writeln!(output, "    return (value >> (bit & 63)) & 1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SETBIT - set a specific bit to 1
    writeln!(output, "int64_t qb_setbit(int64_t value, int64_t bit) {{").unwrap();
    writeln!(output, "    return value | ((int64_t)1 << (bit & 63));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _RESETBIT - clear a specific bit to 0
    writeln!(output, "int64_t qb_resetbit(int64_t value, int64_t bit) {{").unwrap();
    writeln!(output, "    return value & ~((int64_t)1 << (bit & 63));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _TOGGLEBIT - flip a specific bit
    writeln!(
        output,
        "int64_t qb_togglebit(int64_t value, int64_t bit) {{"
    )
    .unwrap();
    writeln!(output, "    return value ^ ((int64_t)1 << (bit & 63));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Reciprocal trig functions
    writeln!(output, "double qb_sec(double n) {{ return 1.0 / cos(n); }}").unwrap();
    writeln!(output, "double qb_csc(double n) {{ return 1.0 / sin(n); }}").unwrap();
    writeln!(output, "double qb_cot(double n) {{ return 1.0 / tan(n); }}").unwrap();
    writeln!(output).unwrap();

    // Hyperbolic reciprocals
    writeln!(
        output,
        "double qb_sech(double n) {{ return 1.0 / cosh(n); }}"
    )
    .unwrap();
    writeln!(
        output,
        "double qb_csch(double n) {{ return 1.0 / sinh(n); }}"
    )
    .unwrap();
    writeln!(
        output,
        "double qb_coth(double n) {{ return 1.0 / tanh(n); }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Inverse reciprocal trig: arcsec(x) = acos(1/x), arccsc(x) = asin(1/x)
    writeln!(
        output,
        "double qb_arcsec(double n) {{ return acos(1.0 / n); }}"
    )
    .unwrap();
    writeln!(
        output,
        "double qb_arccsc(double n) {{ return asin(1.0 / n); }}"
    )
    .unwrap();
    writeln!(
        output,
        "double qb_arccot(double n) {{ return atan(1.0 / n); }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Inverse hyperbolic reciprocals
    // arcsech(x) = acosh(1/x), arccsch(x) = asinh(1/x), arccoth(x) = atanh(1/x)
    writeln!(
        output,
        "double qb_arcsech(double n) {{ return acosh(1.0 / n); }}"
    )
    .unwrap();
    writeln!(
        output,
        "double qb_arccsch(double n) {{ return asinh(1.0 / n); }}"
    )
    .unwrap();
    writeln!(
        output,
        "double qb_arccoth(double n) {{ return atanh(1.0 / n); }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Angle conversions (degrees <-> radians)
    writeln!(output, "double qb_d2r(double degrees) {{").unwrap();
    writeln!(
        output,
        "    return degrees * 3.14159265358979323846 / 180.0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "double qb_r2d(double radians) {{").unwrap();
    writeln!(
        output,
        "    return radians * 180.0 / 3.14159265358979323846;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Gradian conversions (400 gradians = 360 degrees = 2*pi radians)
    writeln!(output, "double qb_d2g(double degrees) {{").unwrap();
    writeln!(output, "    return degrees * 10.0 / 9.0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "double qb_g2d(double gradians) {{").unwrap();
    writeln!(output, "    return gradians * 9.0 / 10.0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "double qb_g2r(double gradians) {{").unwrap();
    writeln!(
        output,
        "    return gradians * 3.14159265358979323846 / 200.0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "double qb_r2g(double radians) {{").unwrap();
    writeln!(
        output,
        "    return radians * 200.0 / 3.14159265358979323846;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _NEGATE - negate value
    writeln!(output, "double qb_negate(double n) {{").unwrap();
    writeln!(output, "    return -n;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // String comparison functions
    writeln!(output, "int64_t qb_strcmp(qb_string* a, qb_string* b) {{").unwrap();
    writeln!(output, "    if (!a && !b) return 0;").unwrap();
    writeln!(output, "    if (!a) return -1;").unwrap();
    writeln!(output, "    if (!b) return 1;").unwrap();
    writeln!(output, "    return strcmp(a->data, b->data);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Case-insensitive string compare
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "int64_t qb_stricmp(qb_string* a, qb_string* b) {{").unwrap();
    writeln!(output, "    if (!a && !b) return 0;").unwrap();
    writeln!(output, "    if (!a) return -1;").unwrap();
    writeln!(output, "    if (!b) return 1;").unwrap();
    writeln!(output, "    return _stricmp(a->data, b->data);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "#include <strings.h>").unwrap();
    writeln!(output, "int64_t qb_stricmp(qb_string* a, qb_string* b) {{").unwrap();
    writeln!(output, "    if (!a && !b) return 0;").unwrap();
    writeln!(output, "    if (!a) return -1;").unwrap();
    writeln!(output, "    if (!b) return 1;").unwrap();
    writeln!(output, "    return strcasecmp(a->data, b->data);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();
}
