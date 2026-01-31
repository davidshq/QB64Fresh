//! Mathematical Functions for QB64Fresh Runtime
//!
//! This module provides mathematical functions that map to BASIC built-ins.

use std::f64::consts::{E, PI};

// ============================================================================
// Basic Math Functions
// ============================================================================

/// Absolute value (ABS).
#[no_mangle]
pub extern "C" fn qb_abs_int(n: i64) -> i64 {
    n.abs()
}

/// Absolute value for floats (ABS).
#[no_mangle]
pub extern "C" fn qb_abs_float(n: f64) -> f64 {
    n.abs()
}

/// Sign function (SGN).
///
/// Returns -1 for negative, 0 for zero, 1 for positive.
#[no_mangle]
pub extern "C" fn qb_sgn_int(n: i64) -> i32 {
    match n.cmp(&0) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Sign function for floats (SGN).
#[no_mangle]
pub extern "C" fn qb_sgn_float(n: f64) -> i32 {
    if n < 0.0 {
        -1
    } else if n > 0.0 {
        1
    } else {
        0
    }
}

/// Integer part (INT) - rounds toward negative infinity.
#[no_mangle]
pub extern "C" fn qb_int(n: f64) -> f64 {
    n.floor()
}

/// Fix - truncates toward zero (unlike INT which floors).
#[no_mangle]
pub extern "C" fn qb_fix(n: f64) -> f64 {
    n.trunc()
}

/// Round to nearest integer (CINT equivalent).
#[no_mangle]
pub extern "C" fn qb_cint(n: f64) -> i16 {
    n.round() as i16
}

/// Round to nearest long integer (CLNG equivalent).
#[no_mangle]
pub extern "C" fn qb_clng(n: f64) -> i32 {
    n.round() as i32
}

// ============================================================================
// rounding.h — Type Conversion and Rounding (libqb compatibility)
// ============================================================================

/// Round to int64 (qbr). Uses round-half-away-from-zero semantics.
/// Values outside i64 range are clamped; NaN/Inf return 0.
#[no_mangle]
pub extern "C" fn qb_qbr(n: f64) -> i64 {
    if n.is_nan() || n.is_infinite() {
        return 0;
    }
    const MAX_I64: f64 = 9223372036854775807.0;
    const MIN_I64: f64 = -9223372036854775808.0;
    let r = n.round();
    if r > MAX_I64 {
        i64::MAX
    } else if r < MIN_I64 {
        i64::MIN
    } else {
        r as i64
    }
}

/// CSNG from float (C passes double): check single range, set error 6 on overflow.
#[no_mangle]
pub extern "C" fn qb_csng_float(n: f64) -> f32 {
    const MAX_SINGLE: f64 = 3.402823466e38;
    if n.is_nan() || n.is_infinite() {
        return n as f32;
    }
    if n.abs() > MAX_SINGLE {
        crate::qb_set_error(6, 0); // Overflow
        return 0.0;
    }
    n as f32
}

/// CSNG from double: check single range, set error 6 on overflow.
#[no_mangle]
pub extern "C" fn qb_csng_double(n: f64) -> f32 {
    const MAX_SINGLE: f64 = 3.402823466e38;
    if n.is_nan() || n.is_infinite() {
        return n as f32;
    }
    if n.abs() > MAX_SINGLE {
        crate::qb_set_error(6, 0); // Overflow
        return 0.0;
    }
    n as f32
}

/// CDBL from float (C passes double): convert to double (always in range).
#[no_mangle]
pub extern "C" fn qb_cdbl_float(n: f64) -> f64 {
    n
}

/// _ROUND: round to int64 (no overflow check, like libqb func_round_*).
#[no_mangle]
pub extern "C" fn qb_round_double(n: f64) -> i64 {
    qb_qbr(n)
}

/// _ROUND from float (C passes double): round to int64.
#[no_mangle]
pub extern "C" fn qb_round_float(n: f64) -> i64 {
    qb_qbr(n)
}

/// Reset FPU rounding mode (libqb fpu_reinit).
///
/// Stub for linking; real implementation would use cfenv to restore default rounding.
#[no_mangle]
pub extern "C" fn qb_fpu_reinit() {}

// ============================================================================
// Extended Math — Power-of-2 (libqb extended_math.h compatibility)
// ============================================================================
// C API uses fixed-width types; for other integral types callers cast.

/// Returns 1 if `n` is a power of 2 (or 0), 0 otherwise (Math_IsPowerOf2).
#[no_mangle]
pub extern "C" fn qb_math_is_power_of_2_u32(n: u32) -> std::ffi::c_int {
    if n == 0 {
        return 0;
    }
    if n & (n - 1) == 0 {
        1
    } else {
        0
    }
}

/// Returns 1 if `n` is a power of 2 (or 0), 0 otherwise (Math_IsPowerOf2).
#[no_mangle]
pub extern "C" fn qb_math_is_power_of_2_u64(n: u64) -> std::ffi::c_int {
    if n == 0 {
        return 0;
    }
    if n & (n - 1) == 0 {
        1
    } else {
        0
    }
}

/// Rounds `n` up to the next power of 2 (Math_RoundUpToPowerOf2). 0 rounds to 1.
#[no_mangle]
pub extern "C" fn qb_math_round_up_to_power_of_2_u32(n: u32) -> u32 {
    if n == 0 {
        return 1;
    }
    let mut u = n - 1;
    u |= u >> 1;
    u |= u >> 2;
    u |= u >> 4;
    u |= u >> 8;
    u |= u >> 16;
    u + 1
}

/// Rounds `n` up to the next power of 2 (Math_RoundUpToPowerOf2). 0 rounds to 1.
#[no_mangle]
pub extern "C" fn qb_math_round_up_to_power_of_2_u64(n: u64) -> u64 {
    if n == 0 {
        return 1;
    }
    let mut u = n - 1;
    u |= u >> 1;
    u |= u >> 2;
    u |= u >> 4;
    u |= u >> 8;
    u |= u >> 16;
    u |= u >> 32;
    u + 1
}

/// Rounds `n` down to the previous power of 2 (Math_RoundDownToPowerOf2). 0 stays 0.
#[no_mangle]
pub extern "C" fn qb_math_round_down_to_power_of_2_u32(n: u32) -> u32 {
    if n == 0 {
        return 0;
    }
    let mut u = n;
    u |= u >> 1;
    u |= u >> 2;
    u |= u >> 4;
    u |= u >> 8;
    u |= u >> 16;
    u - (u >> 1)
}

/// Rounds `n` down to the previous power of 2 (Math_RoundDownToPowerOf2). 0 stays 0.
#[no_mangle]
pub extern "C" fn qb_math_round_down_to_power_of_2_u64(n: u64) -> u64 {
    if n == 0 {
        return 0;
    }
    let mut u = n;
    u |= u >> 1;
    u |= u >> 2;
    u |= u >> 4;
    u |= u >> 8;
    u |= u >> 16;
    u |= u >> 32;
    u - (u >> 1)
}

// ============================================================================
// Trigonometric Functions
// ============================================================================

/// Sine (SIN).
#[no_mangle]
pub extern "C" fn qb_sin(n: f64) -> f64 {
    n.sin()
}

/// Cosine (COS).
#[no_mangle]
pub extern "C" fn qb_cos(n: f64) -> f64 {
    n.cos()
}

/// Tangent (TAN).
#[no_mangle]
pub extern "C" fn qb_tan(n: f64) -> f64 {
    n.tan()
}

/// Arctangent (ATN).
#[no_mangle]
pub extern "C" fn qb_atn(n: f64) -> f64 {
    n.atan()
}

/// Arcsine (_ASIN in QB64).
#[no_mangle]
pub extern "C" fn qb_asin(n: f64) -> f64 {
    n.asin()
}

/// Arccosine (_ACOS in QB64).
#[no_mangle]
pub extern "C" fn qb_acos(n: f64) -> f64 {
    n.acos()
}

/// Hyperbolic sine (_SINH in QB64).
#[no_mangle]
pub extern "C" fn qb_sinh(n: f64) -> f64 {
    n.sinh()
}

/// Hyperbolic cosine (_COSH in QB64).
#[no_mangle]
pub extern "C" fn qb_cosh(n: f64) -> f64 {
    n.cosh()
}

/// Hyperbolic tangent (_TANH in QB64).
#[no_mangle]
pub extern "C" fn qb_tanh(n: f64) -> f64 {
    n.tanh()
}

// ============================================================================
// Reciprocal Trig and Hyperbolic (extended_math — Used by QB64pe)
// ============================================================================

/// Secant: 1/cos (_SEC in QB64).
#[no_mangle]
pub extern "C" fn qb_sec(n: f64) -> f64 {
    1.0 / n.cos()
}

/// Cosecant: 1/sin (_CSC in QB64).
#[no_mangle]
pub extern "C" fn qb_csc(n: f64) -> f64 {
    1.0 / n.sin()
}

/// Cotangent: 1/tan (_COT in QB64).
#[no_mangle]
pub extern "C" fn qb_cot(n: f64) -> f64 {
    1.0 / n.tan()
}

/// Hyperbolic secant: 1/cosh (_SECH in QB64).
#[no_mangle]
pub extern "C" fn qb_sech(n: f64) -> f64 {
    1.0 / n.cosh()
}

/// Hyperbolic cosecant: 1/sinh (_CSCH in QB64).
#[no_mangle]
pub extern "C" fn qb_csch(n: f64) -> f64 {
    1.0 / n.sinh()
}

/// Hyperbolic cotangent: 1/tanh (_COTH in QB64).
#[no_mangle]
pub extern "C" fn qb_coth(n: f64) -> f64 {
    1.0 / n.tanh()
}

/// Arcsecant: acos(1/n) (_ARCSEC in QB64).
#[no_mangle]
pub extern "C" fn qb_arcsec(n: f64) -> f64 {
    (1.0 / n).acos()
}

/// Arccosecant: asin(1/n) (_ARCCSC in QB64).
#[no_mangle]
pub extern "C" fn qb_arccsc(n: f64) -> f64 {
    (1.0 / n).asin()
}

/// Arccotangent: atan(1/n) with quadrant handling (_ARCCOT in QB64).
/// Standard definition: returns value in (0, π) such that cot(result) = n.
#[no_mangle]
pub extern "C" fn qb_arccot(n: f64) -> f64 {
    // arccot(x) = atan(1/x) for x>0; π + atan(1/x) for x<0; π/2 for x=0
    if n == 0.0 {
        PI / 2.0
    } else if n > 0.0 {
        (1.0 / n).atan()
    } else {
        PI + (1.0 / n).atan()
    }
}

/// Inverse hyperbolic secant (_ARCSECH in QB64).
#[no_mangle]
pub extern "C" fn qb_arcsech(n: f64) -> f64 {
    let x = 1.0 / n;
    (x + (x * x - 1.0).sqrt()).ln()
}

/// Inverse hyperbolic cosecant (_ARCCSCH in QB64).
#[no_mangle]
pub extern "C" fn qb_arccsch(n: f64) -> f64 {
    let x = 1.0 / n;
    (x + (x * x + 1.0).sqrt()).ln()
}

/// Inverse hyperbolic cotangent (_ARCCOTH in QB64).
#[no_mangle]
pub extern "C" fn qb_arccoth(n: f64) -> f64 {
    0.5 * ((n + 1.0) / (n - 1.0)).ln()
}

/// Clamp value to [min, max] (_CLAMP in QB64).
#[no_mangle]
pub extern "C" fn qb_clamp(x: f64, min_val: f64, max_val: f64) -> f64 {
    if x < min_val {
        min_val
    } else if x > max_val {
        max_val
    } else {
        x
    }
}

// ============================================================================
// Exponential and Logarithmic Functions
// ============================================================================

/// Square root (SQR).
#[no_mangle]
pub extern "C" fn qb_sqr(n: f64) -> f64 {
    n.sqrt()
}

/// Natural logarithm (LOG).
#[no_mangle]
pub extern "C" fn qb_log(n: f64) -> f64 {
    n.ln()
}

/// Base-10 logarithm (_LOG10 in QB64).
#[no_mangle]
pub extern "C" fn qb_log10(n: f64) -> f64 {
    n.log10()
}

/// Exponential function (EXP).
#[no_mangle]
pub extern "C" fn qb_exp(n: f64) -> f64 {
    n.exp()
}

/// Power function (^).
#[no_mangle]
pub extern "C" fn qb_pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

/// Integer power (more efficient for integer exponents).
#[no_mangle]
pub extern "C" fn qb_pow_int(base: f64, exp: i32) -> f64 {
    base.powi(exp)
}

// ============================================================================
// Random Number Functions
// ============================================================================

use std::cell::RefCell;

thread_local! {
    static RNG_SEED: RefCell<u64> = RefCell::new(0x853c49e6748fea9b);
}

/// Seed the random number generator (RANDOMIZE).
#[no_mangle]
pub extern "C" fn qb_randomize(seed: f64) {
    RNG_SEED.with(|rng| {
        *rng.borrow_mut() = seed.to_bits();
    });
}

/// Seed with current time (RANDOMIZE TIMER).
#[no_mangle]
pub extern "C" fn qb_randomize_timer() {
    use std::time::{SystemTime, UNIX_EPOCH};
    let seed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    RNG_SEED.with(|rng| {
        *rng.borrow_mut() = seed;
    });
}

/// Generate a random number (RND).
///
/// # Arguments
/// * `n` - If n < 0, seeds with n and returns first value
///       - If n = 0, returns last generated value
///       - If n > 0, returns next random value (default)
#[no_mangle]
pub extern "C" fn qb_rnd(n: f64) -> f64 {
    RNG_SEED.with(|rng| {
        let mut seed = rng.borrow_mut();

        if n < 0.0 {
            // Seed with the value
            *seed = n.to_bits();
        }

        if n != 0.0 {
            // xorshift64 algorithm
            *seed ^= *seed << 13;
            *seed ^= *seed >> 7;
            *seed ^= *seed << 17;
        }

        // Convert to 0.0..1.0 range
        (*seed as f64) / (u64::MAX as f64)
    })
}

// ============================================================================
// Conversion Functions
// ============================================================================

/// Convert degrees to radians (_D2R in QB64).
#[no_mangle]
pub extern "C" fn qb_d2r(degrees: f64) -> f64 {
    degrees * PI / 180.0
}

/// Convert radians to degrees (_R2D in QB64).
#[no_mangle]
pub extern "C" fn qb_r2d(radians: f64) -> f64 {
    radians * 180.0 / PI
}

/// Convert degrees to gradians (_D2G in QB64). 400 grad = 360 deg.
#[no_mangle]
pub extern "C" fn qb_d2g(degrees: f64) -> f64 {
    degrees * 10.0 / 9.0
}

/// Convert gradians to degrees (_G2D in QB64).
#[no_mangle]
pub extern "C" fn qb_g2d(gradians: f64) -> f64 {
    gradians * 9.0 / 10.0
}

/// Convert gradians to radians (_G2R in QB64).
#[no_mangle]
pub extern "C" fn qb_g2r(gradians: f64) -> f64 {
    gradians * PI / 200.0
}

/// Convert radians to gradians (_R2G in QB64).
#[no_mangle]
pub extern "C" fn qb_r2g(radians: f64) -> f64 {
    radians * 200.0 / PI
}

// ============================================================================
// Constants
// ============================================================================

/// Return PI (_PI in QB64).
#[no_mangle]
pub extern "C" fn qb_pi() -> f64 {
    PI
}

/// Return e (Euler's number).
#[no_mangle]
pub extern "C" fn qb_e() -> f64 {
    E
}

// ============================================================================
// Bit Manipulation
// ============================================================================

/// Minimum of two integers.
#[no_mangle]
pub extern "C" fn qb_min_int(a: i64, b: i64) -> i64 {
    a.min(b)
}

/// Maximum of two integers.
#[no_mangle]
pub extern "C" fn qb_max_int(a: i64, b: i64) -> i64 {
    a.max(b)
}

/// Minimum of two floats.
#[no_mangle]
pub extern "C" fn qb_min_float(a: f64, b: f64) -> f64 {
    a.min(b)
}

/// Maximum of two floats.
#[no_mangle]
pub extern "C" fn qb_max_float(a: f64, b: f64) -> f64 {
    a.max(b)
}

// ============================================================================
// Timer
// ============================================================================

/// Get elapsed seconds since midnight (TIMER).
#[no_mangle]
pub extern "C" fn qb_timer() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};

    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();

    // Calculate seconds since midnight
    let total_secs = now.as_secs();
    let secs_today = total_secs % 86400; // Seconds in a day
    let nanos = now.subsec_nanos();

    secs_today as f64 + (nanos as f64 / 1_000_000_000.0)
}

/// Pause execution for specified seconds (SLEEP).
#[no_mangle]
pub extern "C" fn qb_sleep(seconds: f64) {
    if seconds > 0.0 {
        std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
    }
}

/// Pause execution for specified milliseconds (_DELAY in QB64).
#[no_mangle]
pub extern "C" fn qb_delay(seconds: f64) {
    qb_sleep(seconds);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abs() {
        assert_eq!(qb_abs_int(-5), 5);
        assert_eq!(qb_abs_int(5), 5);
        assert_eq!(qb_abs_float(-3.14), 3.14);
    }

    #[test]
    fn test_sgn() {
        assert_eq!(qb_sgn_int(-10), -1);
        assert_eq!(qb_sgn_int(0), 0);
        assert_eq!(qb_sgn_int(10), 1);
    }

    #[test]
    fn test_int_fix() {
        assert_eq!(qb_int(3.7), 3.0);
        assert_eq!(qb_int(-3.7), -4.0); // INT floors
        assert_eq!(qb_fix(-3.7), -3.0); // FIX truncates toward zero
    }

    #[test]
    fn test_trig() {
        assert!((qb_sin(0.0) - 0.0).abs() < 1e-10);
        assert!((qb_cos(0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_sqr_log_exp() {
        assert!((qb_sqr(4.0) - 2.0).abs() < 1e-10);
        assert!((qb_log(E) - 1.0).abs() < 1e-10);
        assert!((qb_exp(0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_pow() {
        assert!((qb_pow(2.0, 3.0) - 8.0).abs() < 1e-10);
        assert!((qb_pow_int(2.0, 3) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn test_rnd() {
        qb_randomize_timer();
        let r1 = qb_rnd(1.0);
        let r2 = qb_rnd(1.0);

        assert!(r1 >= 0.0 && r1 < 1.0);
        assert!(r2 >= 0.0 && r2 < 1.0);
        // They should be different (with very high probability)
        assert_ne!(r1, r2);
    }

    #[test]
    fn test_d2r_r2d() {
        assert!((qb_d2r(180.0) - PI).abs() < 1e-10);
        assert!((qb_r2d(PI) - 180.0).abs() < 1e-10);
    }

    #[test]
    fn test_min_max() {
        assert_eq!(qb_min_int(5, 3), 3);
        assert_eq!(qb_max_int(5, 3), 5);
        assert_eq!(qb_min_float(5.0, 3.0), 3.0);
        assert_eq!(qb_max_float(5.0, 3.0), 5.0);
    }

    // ========================================================================
    // Edge case tests
    // ========================================================================

    #[test]
    fn test_abs_edge_cases() {
        // Zero
        assert_eq!(qb_abs_int(0), 0);
        assert_eq!(qb_abs_float(0.0), 0.0);

        // Maximum values
        assert_eq!(qb_abs_int(i64::MAX), i64::MAX);
        assert_eq!(qb_abs_float(f64::MAX), f64::MAX);

        // Very small values
        assert_eq!(qb_abs_float(f64::MIN_POSITIVE), f64::MIN_POSITIVE);
        assert_eq!(qb_abs_float(-f64::MIN_POSITIVE), f64::MIN_POSITIVE);

        // Infinity
        assert!(qb_abs_float(f64::INFINITY).is_infinite());
        assert!(qb_abs_float(f64::NEG_INFINITY).is_infinite());

        // NaN (abs of NaN is NaN)
        assert!(qb_abs_float(f64::NAN).is_nan());
    }

    #[test]
    fn test_sgn_edge_cases() {
        // Maximum/minimum integers
        assert_eq!(qb_sgn_int(i64::MAX), 1);
        assert_eq!(qb_sgn_int(i64::MIN), -1);

        // Float edge cases
        assert_eq!(qb_sgn_float(f64::INFINITY), 1);
        assert_eq!(qb_sgn_float(f64::NEG_INFINITY), -1);
        assert_eq!(qb_sgn_float(f64::MIN_POSITIVE), 1);
        assert_eq!(qb_sgn_float(-f64::MIN_POSITIVE), -1);

        // NaN should return 0 (or undefined, but we test current behavior)
        // Note: NaN comparisons are always false, so SGN returns 0
        assert_eq!(qb_sgn_float(f64::NAN), 0);
    }

    #[test]
    fn test_int_fix_edge_cases() {
        // Zero
        assert_eq!(qb_int(0.0), 0.0);
        assert_eq!(qb_fix(0.0), 0.0);

        // Already integers
        assert_eq!(qb_int(5.0), 5.0);
        assert_eq!(qb_fix(5.0), 5.0);
        assert_eq!(qb_int(-5.0), -5.0);
        assert_eq!(qb_fix(-5.0), -5.0);

        // Very small decimals
        assert_eq!(qb_int(0.0001), 0.0);
        assert_eq!(qb_fix(0.0001), 0.0);
        assert_eq!(qb_int(-0.0001), -1.0); // INT floors
        assert_eq!(qb_fix(-0.0001), 0.0); // FIX truncates toward zero

        // Infinity
        assert!(qb_int(f64::INFINITY).is_infinite());
        assert!(qb_fix(f64::INFINITY).is_infinite());
    }

    #[test]
    fn test_cint_clng_edge_cases() {
        // Normal rounding (round-half-away-from-zero)
        assert_eq!(qb_cint(2.4), 2);
        assert_eq!(qb_cint(2.5), 3); // Rounds away from zero
        assert_eq!(qb_cint(2.6), 3);
        assert_eq!(qb_cint(3.5), 4); // Rounds away from zero

        // Negative values (round-half-away-from-zero)
        assert_eq!(qb_cint(-2.4), -2);
        assert_eq!(qb_cint(-2.5), -3); // Rounds away from zero
        assert_eq!(qb_cint(-2.6), -3);

        // CLNG with larger values
        assert_eq!(qb_clng(1000.5), 1001);
        assert_eq!(qb_clng(-1000.5), -1001);
    }

    #[test]
    fn test_trig_edge_cases() {
        // Common angles
        assert!((qb_sin(PI / 2.0) - 1.0).abs() < 1e-10);
        assert!((qb_cos(PI) + 1.0).abs() < 1e-10);

        // Large values (should still work due to periodicity)
        let large_angle = 100.0 * PI;
        assert!(qb_sin(large_angle).abs() < 1e-10);
        assert!((qb_cos(large_angle) - 1.0).abs() < 1e-10);

        // TAN at 45 degrees
        assert!((qb_tan(PI / 4.0) - 1.0).abs() < 1e-10);

        // ATN
        assert!((qb_atn(1.0) - PI / 4.0).abs() < 1e-10);
        assert!((qb_atn(0.0) - 0.0).abs() < 1e-10);
    }

    #[test]
    fn test_hyperbolic_edge_cases() {
        // At zero
        assert!((qb_sinh(0.0) - 0.0).abs() < 1e-10);
        assert!((qb_cosh(0.0) - 1.0).abs() < 1e-10);
        assert!((qb_tanh(0.0) - 0.0).abs() < 1e-10);

        // Large values (tanh approaches +/- 1)
        assert!((qb_tanh(100.0) - 1.0).abs() < 1e-10);
        assert!((qb_tanh(-100.0) + 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_sqr_edge_cases() {
        // Zero
        assert_eq!(qb_sqr(0.0), 0.0);

        // Perfect squares
        assert_eq!(qb_sqr(1.0), 1.0);
        assert_eq!(qb_sqr(16.0), 4.0);
        assert_eq!(qb_sqr(100.0), 10.0);

        // Non-perfect squares
        assert!((qb_sqr(2.0) - std::f64::consts::SQRT_2).abs() < 1e-10);

        // Large values
        assert!((qb_sqr(1e100) - 1e50).abs() / 1e50 < 1e-10);
    }

    #[test]
    fn test_log_edge_cases() {
        // Log of 1 is 0
        assert_eq!(qb_log(1.0), 0.0);
        assert_eq!(qb_log10(1.0), 0.0);

        // Log10 of powers of 10
        assert!((qb_log10(10.0) - 1.0).abs() < 1e-10);
        assert!((qb_log10(100.0) - 2.0).abs() < 1e-10);
        assert!((qb_log10(1000.0) - 3.0).abs() < 1e-10);

        // Natural log of e^n
        assert!((qb_log(E * E) - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_exp_edge_cases() {
        // exp(0) = 1
        assert_eq!(qb_exp(0.0), 1.0);

        // exp(1) = e
        assert!((qb_exp(1.0) - E).abs() < 1e-10);

        // Negative exponents
        assert!((qb_exp(-1.0) - 1.0 / E).abs() < 1e-10);

        // Large positive (approaches infinity)
        assert!(qb_exp(1000.0).is_infinite());

        // Large negative (approaches zero)
        assert!(qb_exp(-1000.0) < 1e-300);
    }

    #[test]
    fn test_pow_edge_cases() {
        // Anything to the 0 power is 1
        assert_eq!(qb_pow(5.0, 0.0), 1.0);
        assert_eq!(qb_pow_int(5.0, 0), 1.0);

        // Anything to the 1 power is itself
        assert_eq!(qb_pow(5.0, 1.0), 5.0);
        assert_eq!(qb_pow_int(5.0, 1), 5.0);

        // 0 to any positive power is 0
        assert_eq!(qb_pow(0.0, 5.0), 0.0);
        assert_eq!(qb_pow_int(0.0, 5), 0.0);

        // 1 to any power is 1
        assert_eq!(qb_pow(1.0, 1000.0), 1.0);
        assert_eq!(qb_pow_int(1.0, 1000), 1.0);

        // Negative exponents
        assert!((qb_pow(2.0, -1.0) - 0.5).abs() < 1e-10);
        assert!((qb_pow_int(2.0, -1) - 0.5).abs() < 1e-10);

        // Fractional exponents (square root)
        assert!((qb_pow(4.0, 0.5) - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_rnd_edge_cases() {
        // RND with negative argument should reseed (test doesn't crash)
        let _ = qb_rnd(-1.0);

        // RND with 0 should return previous value (test doesn't crash)
        let prev = qb_rnd(1.0);
        let same = qb_rnd(0.0);
        assert_eq!(prev, same);

        // Many RND calls should all be in range [0, 1)
        for _ in 0..100 {
            let r = qb_rnd(1.0);
            assert!(r >= 0.0 && r < 1.0, "RND returned out of range: {}", r);
        }
    }

    #[test]
    fn test_min_max_edge_cases() {
        // Same values
        assert_eq!(qb_min_int(5, 5), 5);
        assert_eq!(qb_max_int(5, 5), 5);
        assert_eq!(qb_min_float(5.0, 5.0), 5.0);
        assert_eq!(qb_max_float(5.0, 5.0), 5.0);

        // Extreme values
        assert_eq!(qb_min_int(i64::MIN, i64::MAX), i64::MIN);
        assert_eq!(qb_max_int(i64::MIN, i64::MAX), i64::MAX);

        // Negative values
        assert_eq!(qb_min_int(-10, -5), -10);
        assert_eq!(qb_max_int(-10, -5), -5);

        // Float infinity
        assert!(qb_min_float(0.0, f64::NEG_INFINITY).is_infinite());
        assert!(qb_max_float(0.0, f64::INFINITY).is_infinite());
    }

    #[test]
    fn test_timer_basic() {
        // Timer should return a non-negative value
        let t1 = qb_timer();
        assert!(t1 >= 0.0);

        // Timer should be less than seconds in a day (86400)
        assert!(t1 < 86400.0);

        // Two calls should return increasing values (or same if very fast)
        let t2 = qb_timer();
        assert!(t2 >= t1);
    }

    #[test]
    fn test_pi_e_constants() {
        assert!((qb_pi() - PI).abs() < 1e-15);
        assert!((qb_e() - E).abs() < 1e-15);
    }
}
