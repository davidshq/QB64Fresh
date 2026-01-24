//! C Header Parser for QB64Fresh
//!
//! This module provides parsing of C header files to automatically extract
//! declarations for `DECLARE LIBRARY "header.h"` support.
//!
//! # Features
//!
//! - **Function declarations** - Extract function signatures and map to BASIC types
//! - **`#define` constants** - Parse simple constant definitions
//! - **Struct definitions** - Parse `struct` and `typedef struct` into QB64 TYPE
//! - **Conditional compilation** - Handle `#ifdef`/`#ifndef` for platform-specific code
//!
//! # Limitations
//!
//! - Function-like macros (`#define FOO(x)`) are skipped
//! - Complex preprocessor expressions in `#if` are not fully evaluated
//! - Nested structs, unions inside structs, and bit fields are not supported
//!
//! # Example
//!
//! ```rust
//! use qb64fresh::header_parser::{parse_header_full, Platform};
//!
//! let header = r#"
//!     #define VERSION 100
//!     #ifdef WIN32
//!     int win_func();
//!     #endif
//!     struct Point { int x; int y; };
//!     int cross_platform();
//! "#;
//!
//! let result = parse_header_full(header, Some(Platform::Linux));
//! assert_eq!(result.constants.len(), 1);  // VERSION
//! assert_eq!(result.structs.len(), 1);    // Point
//! assert_eq!(result.functions.len(), 1);  // cross_platform (not win_func)
//! ```

mod lexer;
mod parser;

pub use lexer::PreprocessorDirective;

use crate::semantic::types::BasicType;

// ============================================================================
// Platform Detection
// ============================================================================

/// Target platform for conditional compilation.
///
/// Used to determine which platform-specific macros are defined when parsing
/// headers with `#ifdef`/`#ifndef` directives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Platform {
    /// Windows (defines WIN32, _WIN32, __WIN32__)
    Windows,
    /// Linux (defines __linux__, __unix__, __GNUC__)
    Linux,
    /// macOS (defines __APPLE__, __MACH__, __unix__)
    MacOS,
}

impl Platform {
    /// Get the current platform at runtime.
    ///
    /// Returns the platform based on the compilation target.
    #[cfg(target_os = "windows")]
    pub fn current() -> Self {
        Platform::Windows
    }

    /// Get the current platform at runtime.
    #[cfg(target_os = "linux")]
    pub fn current() -> Self {
        Platform::Linux
    }

    /// Get the current platform at runtime.
    #[cfg(target_os = "macos")]
    pub fn current() -> Self {
        Platform::MacOS
    }

    /// Get the current platform at runtime (fallback for other platforms).
    #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
    pub fn current() -> Self {
        // Default to Linux for Unix-like systems
        Platform::Linux
    }

    /// Get the predefined macros for this platform.
    ///
    /// Returns a list of macro names that are considered "defined" for
    /// conditional compilation on this platform.
    pub fn predefined_macros(&self) -> &'static [&'static str] {
        match self {
            Platform::Windows => &["WIN32", "_WIN32", "__WIN32__", "_MSC_VER"],
            Platform::Linux => &["__linux__", "__unix__", "__GNUC__", "linux", "unix"],
            Platform::MacOS => &["__APPLE__", "__MACH__", "__unix__", "__GNUC__"],
        }
    }
}

// ============================================================================
// Data Structures
// ============================================================================

/// Complete result of parsing a C header file.
///
/// Contains all extracted declarations: functions, constants, and structs.
#[derive(Debug, Clone, Default)]
pub struct HeaderParseResult {
    /// Function declarations extracted from the header.
    pub functions: Vec<CFunction>,
    /// Constant definitions from `#define` directives.
    pub constants: Vec<CConstant>,
    /// Struct/typedef struct definitions.
    pub structs: Vec<CStruct>,
}

/// Information about a function extracted from a C header.
#[derive(Debug, Clone)]
pub struct CFunction {
    /// The function name.
    pub name: String,
    /// Return type converted to BASIC.
    pub return_type: BasicType,
    /// Parameter types converted to BASIC.
    pub params: Vec<CParam>,
}

/// A parameter from a C function declaration.
#[derive(Debug, Clone)]
pub struct CParam {
    /// Parameter name (if present, otherwise empty).
    pub name: String,
    /// Type converted to BASIC.
    pub typ: BasicType,
}

/// A constant defined via `#define`.
#[derive(Debug, Clone)]
pub struct CConstant {
    /// The constant name.
    pub name: String,
    /// The constant value.
    pub value: ConstantValue,
}

/// The value of a `#define` constant.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    /// Integer value (decimal, hex, or octal).
    Integer(i64),
    /// Floating-point value.
    Float(f64),
    /// String literal.
    String(String),
    /// Expression that couldn't be evaluated (stored as string).
    Expression(String),
}

impl ConstantValue {
    /// Convert this constant value to a QB64 BASIC type.
    pub fn to_basic_type(&self) -> BasicType {
        match self {
            ConstantValue::Integer(n) => {
                // Choose smallest type that fits
                if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                    BasicType::Integer
                } else if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    BasicType::Long
                } else {
                    BasicType::Integer64
                }
            }
            ConstantValue::Float(_) => BasicType::Double,
            ConstantValue::String(_) => BasicType::String,
            ConstantValue::Expression(_) => BasicType::Long, // Default fallback
        }
    }
}

/// A C struct definition.
#[derive(Debug, Clone)]
pub struct CStruct {
    /// The struct name (from `struct Name` or `typedef struct { } Name`).
    pub name: String,
    /// The struct members in declaration order.
    pub members: Vec<CStructMember>,
}

impl CStruct {
    /// Generate QB64 TYPE declaration for this struct.
    ///
    /// # Example
    ///
    /// ```text
    /// TYPE Point
    ///     x AS LONG
    ///     y AS LONG
    /// END TYPE
    /// ```
    pub fn to_qb64_type(&self) -> String {
        let mut out = format!("TYPE {}\n", self.name);
        for member in &self.members {
            out.push_str(&format!(
                "    {} AS {}\n",
                member.name,
                member.to_qb64_type_str()
            ));
        }
        out.push_str("END TYPE");
        out
    }
}

/// A member of a C struct.
#[derive(Debug, Clone)]
pub struct CStructMember {
    /// The member name.
    pub name: String,
    /// The member type.
    pub typ: BasicType,
    /// Array size if this is a fixed-size array (e.g., `char name[64]`).
    pub array_size: Option<usize>,
}

impl CStructMember {
    /// Get the QB64 type string for this member.
    pub fn to_qb64_type_str(&self) -> String {
        if let Some(size) = self.array_size {
            // char[N] becomes STRING * N
            if matches!(self.typ, BasicType::Byte | BasicType::UnsignedByte) {
                format!("STRING * {}", size)
            } else {
                // Other arrays - QB64 doesn't directly support fixed arrays in TYPE
                // but we can represent it
                format!("{} ' ARRAY SIZE {}", self.typ, size)
            }
        } else {
            self.typ.to_string()
        }
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Parse a C header file and extract all declarations.
///
/// This is the main entry point for the enhanced header parser. It extracts
/// functions, constants, and structs, respecting conditional compilation
/// directives based on the target platform.
///
/// # Arguments
///
/// * `header_content` - The contents of the header file
/// * `platform` - Target platform for conditional compilation (uses current platform if None)
///
/// # Returns
///
/// A [`HeaderParseResult`] containing all extracted declarations.
///
/// # Example
///
/// ```rust
/// use qb64fresh::header_parser::{parse_header_full, Platform};
///
/// let header = r#"
///     #define VERSION 100
///     struct Point { int x; int y; };
///     int add(int a, int b);
/// "#;
///
/// let result = parse_header_full(header, Some(Platform::Linux));
/// println!("Found {} functions", result.functions.len());
/// println!("Found {} constants", result.constants.len());
/// println!("Found {} structs", result.structs.len());
/// ```
pub fn parse_header_full(header_content: &str, platform: Option<Platform>) -> HeaderParseResult {
    let platform = platform.unwrap_or_else(Platform::current);
    let tokens = lexer::tokenize(header_content);
    parser::parse_header_full(&tokens, platform)
}

/// Parse a C header file and extract function declarations.
///
/// This is the legacy API that only extracts functions. For new code,
/// prefer [`parse_header_full`] which also extracts constants and structs.
///
/// # Arguments
///
/// * `header_content` - The contents of the header file
///
/// # Returns
///
/// A vector of function declarations found in the header.
///
/// # Deprecated
///
/// Use [`parse_header_full`] instead for access to constants and structs.
#[deprecated(since = "0.2.0", note = "Use parse_header_full() instead")]
pub fn parse_header(header_content: &str) -> Vec<CFunction> {
    let tokens = lexer::tokenize(header_content);
    parser::parse_functions(&tokens)
}

// ============================================================================
// Type Mapping
// ============================================================================

/// Map a C type to a BASIC type.
///
/// # Arguments
///
/// * `c_type` - The C type string (e.g., "int", "unsigned long")
/// * `is_pointer` - Whether this is a pointer type
///
/// # Returns
///
/// The corresponding [`BasicType`] for use in QB64.
pub fn c_type_to_basic(c_type: &str, is_pointer: bool) -> BasicType {
    let base_type = c_type.trim();

    if is_pointer {
        // Char pointers are strings in BASIC
        if base_type == "char" || base_type == "const char" {
            return BasicType::String;
        }
        // Other pointers become _OFFSET
        return BasicType::Offset;
    }

    match base_type {
        // Signed integers
        "int" | "int32_t" | "long" => BasicType::Long,
        "short" | "int16_t" => BasicType::Integer,
        "char" | "int8_t" | "signed char" => BasicType::Byte,
        "long long" | "int64_t" => BasicType::Integer64,

        // Unsigned integers
        "unsigned int" | "uint32_t" | "unsigned long" | "unsigned" => BasicType::UnsignedLong,
        "unsigned short" | "uint16_t" => BasicType::UnsignedInteger,
        "unsigned char" | "uint8_t" => BasicType::UnsignedByte,
        "unsigned long long" | "uint64_t" => BasicType::UnsignedInteger64,

        // Floating point
        "float" => BasicType::Single,
        "double" => BasicType::Double,
        "long double" => BasicType::Float,

        // Void
        "void" => BasicType::Void,

        // Size types
        "size_t" | "ssize_t" | "intptr_t" | "uintptr_t" | "ptrdiff_t" => BasicType::Offset,

        // Unknown type - default to LONG
        _ => BasicType::Long,
    }
}

/// Parse a constant value string into a [`ConstantValue`].
///
/// Handles decimal, hexadecimal, octal, binary integers, floating-point numbers,
/// and string literals. Also handles negative numbers with a leading minus sign.
pub fn parse_constant_value(value: &str) -> ConstantValue {
    let value = value.trim();

    // String literal
    if value.starts_with('"') && value.ends_with('"') {
        let inner = &value[1..value.len() - 1];
        return ConstantValue::String(inner.to_string());
    }

    // Character literal (e.g., 'A')
    if value.starts_with('\'') && value.ends_with('\'') && value.len() >= 2 {
        let inner = &value[1..value.len() - 1];
        if let Some(c) = parse_char_escape(inner) {
            return ConstantValue::Integer(c as i64);
        }
    }

    // Handle negative numbers
    let (is_negative, value) = if value.starts_with('-') {
        (true, value[1..].trim())
    } else if value.starts_with('+') {
        (false, value[1..].trim())
    } else {
        (false, value)
    };

    let negate = |n: i64| if is_negative { -n } else { n };
    let negate_f = |f: f64| if is_negative { -f } else { f };

    // Hexadecimal
    if value.starts_with("0x") || value.starts_with("0X") {
        let hex = strip_integer_suffix(&value[2..]);
        if let Ok(n) = i64::from_str_radix(hex, 16) {
            return ConstantValue::Integer(negate(n));
        }
    }

    // Binary
    if value.starts_with("0b") || value.starts_with("0B") {
        let bin = strip_integer_suffix(&value[2..]);
        if let Ok(n) = i64::from_str_radix(bin, 2) {
            return ConstantValue::Integer(negate(n));
        }
    }

    // Octal (leading 0)
    if value.starts_with('0') && value.len() > 1 {
        let first_char = value.chars().nth(1);
        if let Some(c) = first_char {
            if c.is_ascii_digit() && c != '8' && c != '9' {
                let oct = strip_integer_suffix(&value[1..]);
                if let Ok(n) = i64::from_str_radix(oct, 8) {
                    return ConstantValue::Integer(negate(n));
                }
            }
        }
    }

    // Floating point (contains . or e/E)
    if value.contains('.') || value.contains('e') || value.contains('E') {
        let clean = strip_float_suffix(value);
        if let Ok(f) = clean.parse::<f64>() {
            return ConstantValue::Float(negate_f(f));
        }
    }

    // Decimal integer
    let clean = strip_integer_suffix(value);
    if let Ok(n) = clean.parse::<i64>() {
        return ConstantValue::Integer(negate(n));
    }

    // Couldn't parse - store as expression (restore original with sign)
    let original = if is_negative {
        format!("-{}", value)
    } else {
        value.to_string()
    };
    ConstantValue::Expression(original)
}

/// Strip integer suffixes (U, L, LL, etc.) from a number string.
fn strip_integer_suffix(s: &str) -> &str {
    let s = s.trim();
    let end = s
        .char_indices()
        .take_while(|(_, c)| c.is_ascii_hexdigit() || *c == '_')
        .last()
        .map(|(i, c)| i + c.len_utf8())
        .unwrap_or(0);
    &s[..end]
}

/// Strip float suffixes (f, F, l, L) from a number string.
fn strip_float_suffix(s: &str) -> &str {
    let s = s.trim();
    if s.ends_with('f') || s.ends_with('F') || s.ends_with('l') || s.ends_with('L') {
        &s[..s.len() - 1]
    } else {
        s
    }
}

/// Parse a character escape sequence.
fn parse_char_escape(s: &str) -> Option<char> {
    if s.is_empty() {
        return None;
    }
    if s.starts_with('\\') {
        match s.chars().nth(1) {
            Some('n') => Some('\n'),
            Some('r') => Some('\r'),
            Some('t') => Some('\t'),
            Some('\\') => Some('\\'),
            Some('\'') => Some('\''),
            Some('0') => Some('\0'),
            Some(c) => Some(c),
            None => None,
        }
    } else {
        s.chars().next()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_type_to_basic() {
        assert_eq!(c_type_to_basic("int", false), BasicType::Long);
        assert_eq!(c_type_to_basic("char", true), BasicType::String);
        assert_eq!(c_type_to_basic("double", false), BasicType::Double);
        assert_eq!(c_type_to_basic("void", true), BasicType::Offset);
    }

    #[test]
    fn test_parse_constant_integer() {
        assert_eq!(parse_constant_value("123"), ConstantValue::Integer(123));
        assert_eq!(parse_constant_value("0"), ConstantValue::Integer(0));
        assert_eq!(parse_constant_value("-42"), ConstantValue::Integer(-42));
        assert_eq!(parse_constant_value("+100"), ConstantValue::Integer(100));
    }

    #[test]
    fn test_parse_constant_hex() {
        assert_eq!(parse_constant_value("0xFF"), ConstantValue::Integer(255));
        assert_eq!(parse_constant_value("0x10"), ConstantValue::Integer(16));
        assert_eq!(
            parse_constant_value("0xDEADBEEF"),
            ConstantValue::Integer(0xDEADBEEF)
        );
    }

    #[test]
    fn test_parse_constant_float() {
        assert_eq!(parse_constant_value("3.14"), ConstantValue::Float(3.14));
        assert_eq!(parse_constant_value("1e10"), ConstantValue::Float(1e10));
        assert_eq!(parse_constant_value("2.5f"), ConstantValue::Float(2.5));
    }

    #[test]
    fn test_parse_constant_string() {
        assert_eq!(
            parse_constant_value("\"hello\""),
            ConstantValue::String("hello".to_string())
        );
    }

    #[test]
    fn test_parse_constant_char() {
        assert_eq!(parse_constant_value("'A'"), ConstantValue::Integer(65));
        assert_eq!(parse_constant_value("'\\n'"), ConstantValue::Integer(10));
    }

    #[test]
    fn test_parse_constant_expression() {
        // Expressions we can't evaluate
        assert_eq!(
            parse_constant_value("(1 << 8)"),
            ConstantValue::Expression("(1 << 8)".to_string())
        );
    }

    #[test]
    fn test_platform_macros() {
        let win = Platform::Windows;
        assert!(win.predefined_macros().contains(&"WIN32"));
        assert!(win.predefined_macros().contains(&"_WIN32"));

        let linux = Platform::Linux;
        assert!(linux.predefined_macros().contains(&"__linux__"));
        assert!(linux.predefined_macros().contains(&"__unix__"));

        let macos = Platform::MacOS;
        assert!(macos.predefined_macros().contains(&"__APPLE__"));
    }

    #[test]
    fn test_constant_value_to_basic_type() {
        assert_eq!(
            ConstantValue::Integer(42).to_basic_type(),
            BasicType::Integer
        );
        assert_eq!(
            ConstantValue::Integer(100000).to_basic_type(),
            BasicType::Long
        );
        assert_eq!(
            ConstantValue::Float(3.14).to_basic_type(),
            BasicType::Double
        );
        assert_eq!(
            ConstantValue::String("test".to_string()).to_basic_type(),
            BasicType::String
        );
    }

    #[test]
    fn test_struct_to_qb64_type() {
        let s = CStruct {
            name: "Point".to_string(),
            members: vec![
                CStructMember {
                    name: "x".to_string(),
                    typ: BasicType::Long,
                    array_size: None,
                },
                CStructMember {
                    name: "y".to_string(),
                    typ: BasicType::Long,
                    array_size: None,
                },
            ],
        };
        let qb64 = s.to_qb64_type();
        assert!(qb64.contains("TYPE Point"));
        assert!(qb64.contains("x AS LONG"));
        assert!(qb64.contains("y AS LONG"));
        assert!(qb64.contains("END TYPE"));
    }

    #[test]
    fn test_struct_member_fixed_string() {
        let member = CStructMember {
            name: "name".to_string(),
            typ: BasicType::Byte,
            array_size: Some(64),
        };
        assert_eq!(member.to_qb64_type_str(), "STRING * 64");
    }
}
