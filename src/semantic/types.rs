//! Type system for QB64Fresh semantic analysis.
//!
//! This module defines the internal type representation used during type checking.
//! It handles BASIC's type suffixes, implicit conversions, and type compatibility.
//!
//! # BASIC Type System Overview
//!
//! BASIC has several numeric types with implicit widening conversions:
//! - Small integers: _BIT, _BYTE, INTEGER
//! - Large integers: LONG, _INTEGER64, _OFFSET
//! - Floating point: SINGLE, DOUBLE, _FLOAT
//!
//! String types don't convert to/from numeric types.
//!
//! # Type Suffixes
//!
//! Variables can declare their type via suffix:
//! - `$` STRING (name$)
//! - `%` INTEGER (count%)
//! - `&` LONG (total&)
//! - `!` SINGLE (ratio!)
//! - `#` DOUBLE (pi#)
//! - QB64 extended: `%%` BYTE, `&&` INTEGER64, `##` FLOAT

use crate::ast::TypeSpec;
use std::fmt;

/// Internal type representation for semantic analysis.
///
/// This is richer than the AST's `TypeSpec` and includes array types
/// and special types like Void and Unknown.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BasicType {
    // Integer types (in order of size/widening)
    /// _BIT - Single bit (0 or -1)
    Bit,
    /// _BYTE - 8-bit signed integer
    Byte,
    /// INTEGER - 16-bit signed integer
    Integer,
    /// LONG - 32-bit signed integer
    Long,
    /// _INTEGER64 - 64-bit signed integer
    Integer64,
    /// _OFFSET - Pointer-sized integer
    Offset,

    // Floating point types
    /// SINGLE - 32-bit IEEE float
    Single,
    /// DOUBLE - 64-bit IEEE float
    Double,
    /// _FLOAT - Extended precision float
    Float,

    // String types
    /// STRING - Variable-length string
    String,
    /// STRING * n - Fixed-length string
    FixedString(usize),

    // Unsigned variants (QB64)
    /// _UNSIGNED _BIT
    UnsignedBit,
    /// _UNSIGNED _BYTE
    UnsignedByte,
    /// _UNSIGNED INTEGER
    UnsignedInteger,
    /// _UNSIGNED LONG
    UnsignedLong,
    /// _UNSIGNED _INTEGER64
    UnsignedInteger64,

    // Composite types
    /// User-defined TYPE
    UserDefined(std::string::String),
    /// Array with element type and dimension count
    Array {
        element_type: Box<BasicType>,
        dimensions: usize,
    },

    // Special types
    /// SUBs don't return values
    Void,
    /// Type not yet determined during inference
    Unknown,
}

impl BasicType {
    /// Returns true if this type can be implicitly converted to `target`.
    ///
    /// BASIC allows "widening" conversions (smaller to larger numeric types)
    /// but not "narrowing" conversions or string/numeric mixing.
    pub fn is_convertible_to(&self, target: &BasicType) -> bool {
        // Same type is always OK
        if self == target {
            return true;
        }

        // String conversions only between string types
        match (self, target) {
            (BasicType::String | BasicType::FixedString(_), BasicType::String) => return true,
            (BasicType::FixedString(_), BasicType::FixedString(_)) => return true,
            (BasicType::String | BasicType::FixedString(_), _) => return false,
            (_, BasicType::String | BasicType::FixedString(_)) => return false,
            _ => {}
        }

        // Unknown converts to anything (for inference)
        if matches!(self, BasicType::Unknown) || matches!(target, BasicType::Unknown) {
            return true;
        }

        // Numeric widening: smaller rank can convert to larger rank
        if let (Some(self_rank), Some(target_rank)) = (self.numeric_rank(), target.numeric_rank()) {
            return self_rank <= target_rank;
        }

        false
    }

    /// Returns the common type for a binary operation between two types.
    ///
    /// For numeric types, returns the "wider" type that can represent both.
    /// Returns None if types are incompatible (e.g., string + number).
    pub fn common_type(&self, other: &BasicType) -> Option<BasicType> {
        // Same type
        if self == other {
            return Some(self.clone());
        }

        // String operations
        if self.is_string() && other.is_string() {
            return Some(BasicType::String);
        }

        // Can't mix string and numeric
        if self.is_string() || other.is_string() {
            return None;
        }

        // Both numeric: pick the wider type
        let self_rank = self.numeric_rank()?;
        let other_rank = other.numeric_rank()?;

        if self_rank >= other_rank {
            Some(self.clone())
        } else {
            Some(other.clone())
        }
    }

    /// Returns true if this type is numeric (integer or floating-point).
    pub fn is_numeric(&self) -> bool {
        self.numeric_rank().is_some()
    }

    /// Returns true if this type is a string type.
    pub fn is_string(&self) -> bool {
        matches!(self, BasicType::String | BasicType::FixedString(_))
    }

    /// Returns true if this type is an integer type (not floating-point).
    pub fn is_integer(&self) -> bool {
        use BasicType::*;
        matches!(
            self,
            Bit | Byte
                | Integer
                | Long
                | Integer64
                | Offset
                | UnsignedBit
                | UnsignedByte
                | UnsignedInteger
                | UnsignedLong
                | UnsignedInteger64
        )
    }

    /// Returns true if this type is a floating-point type.
    pub fn is_float(&self) -> bool {
        matches!(
            self,
            BasicType::Single | BasicType::Double | BasicType::Float
        )
    }

    /// Returns the numeric conversion rank (higher = wider type).
    ///
    /// Used for determining widening conversions and common types.
    /// Floating-point ranks are higher than integer ranks to ensure
    /// integer-to-float promotion.
    fn numeric_rank(&self) -> Option<u8> {
        use BasicType::*;
        match self {
            Bit | UnsignedBit => Some(1),
            Byte | UnsignedByte => Some(2),
            Integer | UnsignedInteger => Some(3),
            Long | UnsignedLong => Some(4),
            Integer64 | UnsignedInteger64 => Some(5),
            Offset => Some(6),
            Single => Some(10),
            Double => Some(11),
            Float => Some(12),
            _ => None,
        }
    }
}

impl fmt::Display for BasicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BasicType::Bit => write!(f, "_BIT"),
            BasicType::Byte => write!(f, "_BYTE"),
            BasicType::Integer => write!(f, "INTEGER"),
            BasicType::Long => write!(f, "LONG"),
            BasicType::Integer64 => write!(f, "_INTEGER64"),
            BasicType::Offset => write!(f, "_OFFSET"),
            BasicType::Single => write!(f, "SINGLE"),
            BasicType::Double => write!(f, "DOUBLE"),
            BasicType::Float => write!(f, "_FLOAT"),
            BasicType::String => write!(f, "STRING"),
            BasicType::FixedString(n) => write!(f, "STRING * {}", n),
            BasicType::UnsignedBit => write!(f, "_UNSIGNED _BIT"),
            BasicType::UnsignedByte => write!(f, "_UNSIGNED _BYTE"),
            BasicType::UnsignedInteger => write!(f, "_UNSIGNED INTEGER"),
            BasicType::UnsignedLong => write!(f, "_UNSIGNED LONG"),
            BasicType::UnsignedInteger64 => write!(f, "_UNSIGNED _INTEGER64"),
            BasicType::UserDefined(name) => write!(f, "{}", name),
            BasicType::Array { element_type, .. } => write!(f, "{}()", element_type),
            BasicType::Void => write!(f, "VOID"),
            BasicType::Unknown => write!(f, "UNKNOWN"),
        }
    }
}

/// Extracts a type from an identifier's type suffix.
///
/// Returns `None` if the identifier has no type suffix, meaning the type
/// should be determined from context (DIM AS clause, DEFtype, or default).
///
/// # Examples
///
/// ```ignore
/// assert_eq!(type_from_suffix("name$"), Some(BasicType::String));
/// assert_eq!(type_from_suffix("count%"), Some(BasicType::Integer));
/// assert_eq!(type_from_suffix("total"), None);
/// ```
pub fn type_from_suffix(name: &str) -> Option<BasicType> {
    if name.len() < 2 {
        return None;
    }

    // Check for two-character QB64 suffixes first
    if name.len() >= 2 {
        let suffix2: std::string::String = name
            .chars()
            .rev()
            .take(2)
            .collect::<std::string::String>()
            .chars()
            .rev()
            .collect();
        match suffix2.as_str() {
            "%%" => return Some(BasicType::Byte),
            "&&" => return Some(BasicType::Integer64),
            "##" => return Some(BasicType::Float),
            "%&" => return Some(BasicType::Offset),
            "~%" => return Some(BasicType::UnsignedInteger),
            "~&" => return Some(BasicType::UnsignedLong),
            "~`" => return Some(BasicType::UnsignedBit),
            _ => {}
        }
    }

    // Single-character suffixes
    match name.chars().last()? {
        '$' => Some(BasicType::String),
        '%' => Some(BasicType::Integer),
        '&' => Some(BasicType::Long),
        '!' => Some(BasicType::Single),
        '#' => Some(BasicType::Double),
        '`' => Some(BasicType::Bit),
        _ => None,
    }
}

/// Strips the type suffix from an identifier name.
///
/// Returns the base name without any type suffix characters.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(strip_suffix("name$"), "name");
/// assert_eq!(strip_suffix("count%"), "count");
/// assert_eq!(strip_suffix("total"), "total");
/// ```
pub fn strip_suffix(name: &str) -> &str {
    // Two-character suffixes (must check first)
    let two_char_suffixes = ["%%", "&&", "##", "%&", "~%", "~&", "~`"];
    for suffix in &two_char_suffixes {
        if let Some(stripped) = name.strip_suffix(suffix) {
            return stripped;
        }
    }

    // Single-character suffixes
    let one_char_suffixes = ['$', '%', '&', '!', '#', '`'];
    if let Some(last) = name.chars().last()
        && one_char_suffixes.contains(&last)
    {
        return &name[..name.len() - 1];
    }

    name
}

/// Converts an AST `TypeSpec` to the internal `BasicType`.
///
/// This bridges the parser's type representation with the semantic analyzer's.
pub fn from_type_spec(spec: &TypeSpec) -> BasicType {
    match spec {
        TypeSpec::Integer => BasicType::Integer,
        TypeSpec::Long => BasicType::Long,
        TypeSpec::Single => BasicType::Single,
        TypeSpec::Double => BasicType::Double,
        TypeSpec::String => BasicType::String,
        TypeSpec::FixedString(n) => BasicType::FixedString(*n),
        TypeSpec::Byte => BasicType::Byte,
        TypeSpec::Bit => BasicType::Bit,
        TypeSpec::Integer64 => BasicType::Integer64,
        TypeSpec::Float => BasicType::Float,
        TypeSpec::Offset => BasicType::Offset,
        TypeSpec::Unsigned(inner) => {
            match from_type_spec(inner.as_ref()) {
                BasicType::Bit => BasicType::UnsignedBit,
                BasicType::Byte => BasicType::UnsignedByte,
                BasicType::Integer => BasicType::UnsignedInteger,
                BasicType::Long => BasicType::UnsignedLong,
                BasicType::Integer64 => BasicType::UnsignedInteger64,
                // Float/Double can't be unsigned, return as-is
                other => other,
            }
        }
        TypeSpec::UserDefined(name) => BasicType::UserDefined(name.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_from_suffix() {
        assert_eq!(type_from_suffix("name$"), Some(BasicType::String));
        assert_eq!(type_from_suffix("count%"), Some(BasicType::Integer));
        assert_eq!(type_from_suffix("total&"), Some(BasicType::Long));
        assert_eq!(type_from_suffix("ratio!"), Some(BasicType::Single));
        assert_eq!(type_from_suffix("pi#"), Some(BasicType::Double));
        assert_eq!(type_from_suffix("x"), None);
        assert_eq!(type_from_suffix("abc123"), None);
    }

    #[test]
    fn test_qb64_suffixes() {
        assert_eq!(type_from_suffix("b%%"), Some(BasicType::Byte));
        assert_eq!(type_from_suffix("big&&"), Some(BasicType::Integer64));
        assert_eq!(type_from_suffix("precise##"), Some(BasicType::Float));
    }

    #[test]
    fn test_strip_suffix() {
        assert_eq!(strip_suffix("name$"), "name");
        assert_eq!(strip_suffix("count%"), "count");
        assert_eq!(strip_suffix("x"), "x");
        assert_eq!(strip_suffix("big&&"), "big");
    }

    #[test]
    fn test_is_convertible_to() {
        // Integer to Long (widening)
        assert!(BasicType::Integer.is_convertible_to(&BasicType::Long));

        // Long to Integer (narrowing - not allowed)
        assert!(!BasicType::Long.is_convertible_to(&BasicType::Integer));

        // Integer to Double (widening)
        assert!(BasicType::Integer.is_convertible_to(&BasicType::Double));

        // String to Integer (not allowed)
        assert!(!BasicType::String.is_convertible_to(&BasicType::Integer));

        // Same type
        assert!(BasicType::Integer.is_convertible_to(&BasicType::Integer));
    }

    #[test]
    fn test_common_type() {
        // Integer + Long = Long
        assert_eq!(
            BasicType::Integer.common_type(&BasicType::Long),
            Some(BasicType::Long)
        );

        // Integer + Double = Double
        assert_eq!(
            BasicType::Integer.common_type(&BasicType::Double),
            Some(BasicType::Double)
        );

        // String + String = String
        assert_eq!(
            BasicType::String.common_type(&BasicType::String),
            Some(BasicType::String)
        );

        // String + Integer = None (incompatible)
        assert_eq!(BasicType::String.common_type(&BasicType::Integer), None);
    }

    #[test]
    fn test_is_numeric() {
        assert!(BasicType::Integer.is_numeric());
        assert!(BasicType::Double.is_numeric());
        assert!(!BasicType::String.is_numeric());
        assert!(!BasicType::Void.is_numeric());
    }

    #[test]
    fn test_is_integer() {
        assert!(BasicType::Integer.is_integer());
        assert!(BasicType::Long.is_integer());
        assert!(!BasicType::Single.is_integer());
        assert!(!BasicType::Double.is_integer());
    }

    #[test]
    fn test_from_type_spec() {
        assert_eq!(from_type_spec(&TypeSpec::Integer), BasicType::Integer);
        assert_eq!(from_type_spec(&TypeSpec::String), BasicType::String);
        assert_eq!(
            from_type_spec(&TypeSpec::Unsigned(Box::new(TypeSpec::Long))),
            BasicType::UnsignedLong
        );
    }

    #[test]
    fn test_display() {
        assert_eq!(BasicType::Integer.to_string(), "INTEGER");
        assert_eq!(BasicType::String.to_string(), "STRING");
        assert_eq!(BasicType::FixedString(10).to_string(), "STRING * 10");
        assert_eq!(BasicType::UnsignedLong.to_string(), "_UNSIGNED LONG");
    }
}
