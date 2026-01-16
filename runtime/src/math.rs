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
}
