//! Variable value representations for the debugger.
//!
//! Types for scalar, array, and UDT values as shown in the IDE
//! (e.g. in Variables view or watch expressions).

use serde::{Deserialize, Serialize};

/// A single variable or expression value for DAP.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DebugValue {
    /// Display string (e.g. "42", "hello", "Integer").
    pub value: String,

    /// Optional type string (e.g. "LONG", "STRING").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#type: Option<String>,

    /// For aggregates: child variables (stub: none until runtime integration).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub variables_reference: Option<u64>,
}

impl DebugValue {
    /// Create a simple scalar value.
    pub fn scalar(value: String, type_name: Option<String>) -> Self {
        Self {
            value,
            r#type: type_name,
            variables_reference: None,
        }
    }
}
