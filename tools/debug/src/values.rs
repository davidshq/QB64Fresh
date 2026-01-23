//! Variable value representations for the debugger.
//!
//! This module defines how variable values are represented during debugging.
//! It provides:
//!
//! - **Value types**: Representations for all BASIC data types
//! - **Array values**: Multi-dimensional array element access
//! - **UDT values**: User-defined type member access
//! - **Formatting**: Display values in various formats (decimal, hex, binary, etc.)
//!
//! Note: This module defines the *representation* of values. The actual reading
//! of values from a running program requires runtime integration.

use crate::symbols::DebugType;
use std::collections::HashMap;
use std::fmt;

/// A debug value - the runtime value of a variable.
#[derive(Debug, Clone)]
pub enum DebugValue {
    /// Integer value (INTEGER, LONG, _BYTE, etc.)
    Integer(i64),
    /// Unsigned integer value.
    UnsignedInteger(u64),
    /// Floating-point value (SINGLE, DOUBLE, _FLOAT).
    Float(f64),
    /// String value.
    String(String),
    /// Array value.
    Array(ArrayValue),
    /// User-defined type value.
    UserDefined(UdtValue),
    /// Memory pointer/offset value.
    Pointer(usize),
    /// Value could not be read (e.g., optimized out, not in scope).
    Unavailable(String),
    /// Null/uninitialized value.
    Uninitialized,
}

impl DebugValue {
    /// Creates an integer value.
    pub fn integer(value: i64) -> Self {
        DebugValue::Integer(value)
    }

    /// Creates an unsigned integer value.
    pub fn unsigned(value: u64) -> Self {
        DebugValue::UnsignedInteger(value)
    }

    /// Creates a floating-point value.
    pub fn float(value: f64) -> Self {
        DebugValue::Float(value)
    }

    /// Creates a string value.
    pub fn string(value: impl Into<String>) -> Self {
        DebugValue::String(value.into())
    }

    /// Creates an unavailable value with a reason.
    pub fn unavailable(reason: impl Into<String>) -> Self {
        DebugValue::Unavailable(reason.into())
    }

    /// Returns true if the value is available for inspection.
    pub fn is_available(&self) -> bool {
        !matches!(self, DebugValue::Unavailable(_) | DebugValue::Uninitialized)
    }

    /// Formats the value as a string for display.
    pub fn format(&self, format: &DisplayFormat) -> String {
        match self {
            DebugValue::Integer(v) => format.format_integer(*v),
            DebugValue::UnsignedInteger(v) => format.format_unsigned(*v),
            DebugValue::Float(v) => format.format_float(*v),
            DebugValue::String(s) => format.format_string(s),
            DebugValue::Array(arr) => arr.format_summary(),
            DebugValue::UserDefined(udt) => udt.format_summary(),
            DebugValue::Pointer(addr) => format!("0x{:X}", addr),
            DebugValue::Unavailable(reason) => format!("<unavailable: {}>", reason),
            DebugValue::Uninitialized => "<uninitialized>".to_string(),
        }
    }
}

impl fmt::Display for DebugValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format(&DisplayFormat::default()))
    }
}

/// Display format options for values.
#[derive(Debug, Clone, Default)]
pub struct DisplayFormat {
    /// Number format for integers.
    pub number_format: NumberFormat,
    /// Maximum string length to display (0 = unlimited).
    pub max_string_length: usize,
    /// Maximum array elements to display.
    pub max_array_elements: usize,
}

impl DisplayFormat {
    /// Creates a default display format.
    pub fn new() -> Self {
        Self {
            number_format: NumberFormat::Decimal,
            max_string_length: 100,
            max_array_elements: 10,
        }
    }

    /// Creates a format for hexadecimal display.
    pub fn hex() -> Self {
        Self {
            number_format: NumberFormat::Hexadecimal,
            ..Self::new()
        }
    }

    /// Creates a format for binary display.
    pub fn binary() -> Self {
        Self {
            number_format: NumberFormat::Binary,
            ..Self::new()
        }
    }

    /// Formats an integer value.
    fn format_integer(&self, value: i64) -> String {
        match self.number_format {
            NumberFormat::Decimal => value.to_string(),
            NumberFormat::Hexadecimal => {
                if value < 0 {
                    format!("-0x{:X}", value.unsigned_abs())
                } else {
                    format!("0x{:X}", value)
                }
            }
            NumberFormat::Binary => {
                if value < 0 {
                    format!("-0b{:b}", value.unsigned_abs())
                } else {
                    format!("0b{:b}", value)
                }
            }
            NumberFormat::Octal => {
                if value < 0 {
                    format!("-0o{:o}", value.unsigned_abs())
                } else {
                    format!("0o{:o}", value)
                }
            }
        }
    }

    /// Formats an unsigned integer value.
    fn format_unsigned(&self, value: u64) -> String {
        match self.number_format {
            NumberFormat::Decimal => value.to_string(),
            NumberFormat::Hexadecimal => format!("0x{:X}", value),
            NumberFormat::Binary => format!("0b{:b}", value),
            NumberFormat::Octal => format!("0o{:o}", value),
        }
    }

    /// Formats a floating-point value.
    fn format_float(&self, value: f64) -> String {
        if value.fract() == 0.0 && value.abs() < 1e15 {
            format!("{:.1}", value)
        } else {
            format!("{}", value)
        }
    }

    /// Formats a string value.
    fn format_string(&self, value: &str) -> String {
        if self.max_string_length > 0 && value.len() > self.max_string_length {
            format!("\"{}\"...", &value[..self.max_string_length])
        } else {
            format!("\"{}\"", value)
        }
    }
}

/// Number format for display.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NumberFormat {
    /// Decimal (base 10).
    #[default]
    Decimal,
    /// Hexadecimal (base 16).
    Hexadecimal,
    /// Binary (base 2).
    Binary,
    /// Octal (base 8).
    Octal,
}

/// An array value with its elements.
#[derive(Debug, Clone)]
pub struct ArrayValue {
    /// Element type.
    pub element_type: DebugType,
    /// Array dimensions (bounds).
    pub dimensions: Vec<ArrayBounds>,
    /// Elements, indexed by flat index.
    pub elements: HashMap<usize, DebugValue>,
    /// Total number of elements.
    pub total_elements: usize,
}

impl ArrayValue {
    /// Creates a new array value.
    pub fn new(element_type: DebugType, dimensions: Vec<ArrayBounds>) -> Self {
        let total_elements = dimensions.iter().map(|d| d.size()).product();
        Self {
            element_type,
            dimensions,
            elements: HashMap::new(),
            total_elements,
        }
    }

    /// Gets an element by multi-dimensional indices.
    pub fn get(&self, indices: &[i64]) -> Option<&DebugValue> {
        if indices.len() != self.dimensions.len() {
            return None;
        }

        // Convert multi-dimensional indices to flat index
        let flat_index = self.indices_to_flat(indices)?;
        self.elements.get(&flat_index)
    }

    /// Sets an element by multi-dimensional indices.
    pub fn set(&mut self, indices: &[i64], value: DebugValue) -> bool {
        if indices.len() != self.dimensions.len() {
            return false;
        }

        if let Some(flat_index) = self.indices_to_flat(indices) {
            self.elements.insert(flat_index, value);
            true
        } else {
            false
        }
    }

    /// Converts multi-dimensional indices to a flat index.
    fn indices_to_flat(&self, indices: &[i64]) -> Option<usize> {
        let mut flat_index = 0usize;
        let mut multiplier = 1usize;

        for (i, (idx, dim)) in indices.iter().zip(&self.dimensions).enumerate().rev() {
            // Check bounds
            if *idx < dim.lower || *idx > dim.upper {
                return None;
            }

            let offset = (*idx - dim.lower) as usize;
            if i == indices.len() - 1 {
                flat_index = offset;
            } else {
                flat_index += offset * multiplier;
            }
            multiplier *= dim.size();
        }

        Some(flat_index)
    }

    /// Returns a summary string for display.
    fn format_summary(&self) -> String {
        let dims: Vec<String> = self
            .dimensions
            .iter()
            .map(|d| format!("{} TO {}", d.lower, d.upper))
            .collect();
        format!(
            "{}({}) [{} elements]",
            self.element_type.display_name(),
            dims.join(", "),
            self.total_elements
        )
    }
}

/// Array bounds for a single dimension.
#[derive(Debug, Clone, Copy)]
pub struct ArrayBounds {
    /// Lower bound (inclusive).
    pub lower: i64,
    /// Upper bound (inclusive).
    pub upper: i64,
}

impl ArrayBounds {
    /// Creates new array bounds.
    pub fn new(lower: i64, upper: i64) -> Self {
        Self { lower, upper }
    }

    /// Returns the size of this dimension.
    pub fn size(&self) -> usize {
        (self.upper - self.lower + 1).max(0) as usize
    }
}

/// A user-defined type value with its members.
#[derive(Debug, Clone)]
pub struct UdtValue {
    /// Type name.
    pub type_name: String,
    /// Member values, indexed by uppercase member name.
    pub members: HashMap<String, DebugValue>,
}

impl UdtValue {
    /// Creates a new UDT value.
    pub fn new(type_name: impl Into<String>) -> Self {
        Self {
            type_name: type_name.into(),
            members: HashMap::new(),
        }
    }

    /// Gets a member by name.
    pub fn get_member(&self, name: &str) -> Option<&DebugValue> {
        self.members.get(&name.to_uppercase())
    }

    /// Sets a member by name.
    pub fn set_member(&mut self, name: &str, value: DebugValue) {
        self.members.insert(name.to_uppercase(), value);
    }

    /// Returns a summary string for display.
    fn format_summary(&self) -> String {
        let member_count = self.members.len();
        format!("{} {{{} members}}", self.type_name, member_count)
    }
}

/// A variable with its name, type, and current value.
#[derive(Debug, Clone)]
pub struct VariableInfo {
    /// Variable name.
    pub name: String,
    /// Variable type.
    pub var_type: DebugType,
    /// Current value.
    pub value: DebugValue,
    /// Memory address (if available).
    pub address: Option<usize>,
}

impl VariableInfo {
    /// Creates a new variable info.
    pub fn new(name: impl Into<String>, var_type: DebugType, value: DebugValue) -> Self {
        Self {
            name: name.into(),
            var_type,
            value,
            address: None,
        }
    }

    /// Creates a variable info with address.
    pub fn with_address(
        name: impl Into<String>,
        var_type: DebugType,
        value: DebugValue,
        address: usize,
    ) -> Self {
        Self {
            name: name.into(),
            var_type,
            value,
            address: Some(address),
        }
    }
}

impl fmt::Display for VariableInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {} = {}",
            self.name,
            self.var_type.display_name(),
            self.value
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_formatting() {
        let format = DisplayFormat::default();
        assert_eq!(format.format_integer(42), "42");
        assert_eq!(format.format_integer(-42), "-42");

        let hex = DisplayFormat::hex();
        assert_eq!(hex.format_integer(255), "0xFF");
        assert_eq!(hex.format_integer(-255), "-0xFF");

        let bin = DisplayFormat::binary();
        assert_eq!(bin.format_integer(5), "0b101");
    }

    #[test]
    fn test_string_formatting() {
        let mut format = DisplayFormat::default();
        format.max_string_length = 10;

        assert_eq!(format.format_string("hello"), "\"hello\"");
        assert_eq!(
            format.format_string("hello world, this is a long string"),
            "\"hello worl\"..."
        );
    }

    #[test]
    fn test_array_value() {
        let dims = vec![ArrayBounds::new(0, 4), ArrayBounds::new(1, 3)];
        let mut arr = ArrayValue::new(DebugType::Integer, dims);

        assert_eq!(arr.total_elements, 15); // 5 * 3

        arr.set(&[0, 1], DebugValue::integer(42));
        arr.set(&[2, 2], DebugValue::integer(100));

        assert_eq!(
            arr.get(&[0, 1]).map(|v| v.to_string()),
            Some("42".to_string())
        );
        assert_eq!(
            arr.get(&[2, 2]).map(|v| v.to_string()),
            Some("100".to_string())
        );
        assert!(arr.get(&[5, 1]).is_none()); // Out of bounds
    }

    #[test]
    fn test_udt_value() {
        let mut udt = UdtValue::new("Person");
        udt.set_member("name", DebugValue::string("Alice"));
        udt.set_member("age", DebugValue::integer(30));

        assert_eq!(
            udt.get_member("name").map(|v| v.to_string()),
            Some("\"Alice\"".to_string())
        );
        assert_eq!(
            udt.get_member("AGE").map(|v| v.to_string()),
            Some("30".to_string())
        );
    }

    #[test]
    fn test_variable_info_display() {
        let var = VariableInfo::new("counter", DebugType::Integer, DebugValue::integer(42));
        assert_eq!(var.to_string(), "counter: INTEGER = 42");
    }
}
