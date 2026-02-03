//! Call stack frame structures for the debugger.
//!
//! Represents a single frame on the call stack (main program or SUB/FUNCTION)
//! with source location and scope information.

use serde::{Deserialize, Serialize};

/// A single stack frame (e.g. main, or a SUB/FUNCTION call).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StackFrame {
    /// Frame ID for DAP (unique per frame).
    pub id: u64,

    /// Display name (e.g. "Main", "MySub", "MyFunction").
    pub name: String,

    /// Source path (e.g. "program.bas").
    pub source_path: Option<String>,

    /// 1-based line number in source.
    pub line: u32,

    /// 1-based column (optional).
    pub column: Option<u32>,
}

impl StackFrame {
    /// Create a frame for the main program.
    pub fn main(id: u64, source_path: Option<String>, line: u32) -> Self {
        Self {
            id,
            name: "Main".to_string(),
            source_path,
            line,
            column: None,
        }
    }

    /// Create a frame for a procedure (SUB or FUNCTION).
    pub fn procedure(id: u64, name: String, source_path: Option<String>, line: u32) -> Self {
        Self {
            id,
            name,
            source_path,
            line,
            column: None,
        }
    }
}
