//! Helper functions for writing code to output strings.
//!
//! These functions wrap the standard `write!` and `writeln!` macros to return
//! `Result` instead of using `unwrap()`, providing consistent error handling
//! throughout the code generation codebase.
//!
//! While writing to a `String` should never fail in practice, using these
//! helpers ensures that:
//! 1. All code generation functions have consistent error handling
//! 2. Potential future changes (e.g., writing to a different target) are easier
//! 3. The code follows Rust best practices for error handling
//!
//! # Example
//!
//! ```ignore
//! use crate::codegen::c_backend::write_helpers::*;
//!
//! let mut output = String::new();
//! writeln_code(&mut output, "int x = 5;")?;
//! write_code(&mut output, "return {};", value)?;
//! ```

use std::fmt::Write;

use crate::codegen::error::CodeGenError;

/// Writes formatted data to a string, returning an error if writing fails.
///
/// This is a wrapper around `write!` that returns `Result` instead of using `unwrap()`.
/// While writing to a `String` should never fail, this provides consistent error handling.
///
/// # Arguments
///
/// * `output` - The string buffer to write to
/// * `args` - Format arguments (same as `write!` macro)
///
/// # Returns
///
/// Returns `Ok(())` on success, or `Err(CodeGenError::IoError)` if writing fails.
///
/// # Example
///
/// ```ignore
/// let mut output = String::new();
/// write_code(&mut output, "int {} = {};", var_name, value)?;
/// ```
#[inline]
pub fn write_code(output: &mut String, args: std::fmt::Arguments<'_>) -> Result<(), CodeGenError> {
    output.write_fmt(args).map_err(|e| {
        CodeGenError::new(crate::codegen::error::CodeGenErrorKind::IoError(format!(
            "failed to write to output: {}",
            e
        )))
    })
}

/// Writes formatted data followed by a newline to a string, returning an error if writing fails.
///
/// This is a wrapper around `writeln!` that returns `Result` instead of using `unwrap()`.
/// While writing to a `String` should never fail, this provides consistent error handling.
///
/// # Arguments
///
/// * `output` - The string buffer to write to
/// * `args` - Format arguments (same as `writeln!` macro)
///
/// # Returns
///
/// Returns `Ok(())` on success, or `Err(CodeGenError::IoError)` if writing fails.
///
/// # Example
///
/// ```ignore
/// let mut output = String::new();
/// writeln_code(&mut output, "int x = 5;")?;
/// ```
#[inline]
pub fn writeln_code(
    output: &mut String,
    args: std::fmt::Arguments<'_>,
) -> Result<(), CodeGenError> {
    output
        .write_fmt(args)
        .and_then(|_| output.write_char('\n'))
        .map_err(|e| {
            CodeGenError::new(crate::codegen::error::CodeGenErrorKind::IoError(format!(
                "failed to write to output: {}",
                e
            )))
        })
}

/// Macro for writing formatted code with error handling.
///
/// This macro provides a convenient way to use `write_code` with format strings,
/// similar to the standard `write!` macro but returning `Result`.
///
/// # Example
///
/// ```ignore
/// write_code!(output, "int {} = {};", var_name, value)?;
/// ```
#[macro_export]
macro_rules! write_code {
    ($output:expr, $($arg:tt)*) => {
        $crate::codegen::c_backend::write_helpers::write_code($output, format_args!($($arg)*))
    };
}

/// Macro for writing formatted code with newline and error handling.
///
/// This macro provides a convenient way to use `writeln_code` with format strings,
/// similar to the standard `writeln!` macro but returning `Result`.
///
/// # Example
///
/// ```ignore
/// writeln_code!(output, "int x = 5;")?;
/// ```
#[macro_export]
macro_rules! writeln_code {
    ($output:expr) => {
        $crate::codegen::c_backend::write_helpers::writeln_code($output, format_args!(""))
    };
    ($output:expr, $($arg:tt)*) => {
        $crate::codegen::c_backend::write_helpers::writeln_code($output, format_args!($($arg)*))
    };
}
