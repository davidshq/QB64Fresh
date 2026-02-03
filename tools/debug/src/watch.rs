//! Watch expression parsing and evaluation (stub).
//!
//! Parses watch expressions (e.g. "x", "arr(i,j)", "player.x")
//! for the DAP EvaluateRequest. Evaluation requires runtime integration.

use crate::DebugResult;

/// Parsed watch expression (variable name, array indices, or field access).
#[derive(Debug, Clone)]
pub enum WatchExpr {
    /// Simple variable: "x", "count%"
    Variable(String),

    /// Array element: "arr(1)", "matrix(i, j)"
    ArrayElement { name: String, indices: Vec<String> },

    /// Field access: "player.x", "rec.name"
    FieldAccess { base: String, field: String },
}

/// Parse a watch expression string into a WatchExpr.
/// Stub: only supports simple variable names.
pub fn parse_watch(expr: &str) -> DebugResult<WatchExpr> {
    let expr = expr.trim();
    if expr.is_empty() {
        return Err(crate::DebugError::ProtocolError {
            message: "Empty watch expression".to_string(),
        });
    }
    // Simple stub: treat as variable name (no parentheses or dots).
    Ok(WatchExpr::Variable(expr.to_string()))
}
