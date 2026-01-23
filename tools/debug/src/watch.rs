//! Variable watch expression management.
//!
//! This module handles watch expressions - user-defined expressions that
//! are evaluated and displayed during debugging. It provides:
//!
//! - **Watch expression parsing**: Parse BASIC variable references
//! - **Watch management**: Add, remove, enable/disable watches
//! - **Expression evaluation**: (Stub) Evaluate expressions in context
//!
//! Watch expressions can be:
//! - Simple variables: `x`, `counter%`
//! - Array elements: `arr(1, 2)`
//! - UDT members: `player.x`, `enemies(i).health`
//! - Complex expressions: `a + b`, `LEN(s$)` (future)

use crate::frames::FrameId;
use crate::symbols::{DebugScopeId, DebugSymbols, DebugType};
use crate::values::DebugValue;
use std::collections::HashMap;

/// A unique identifier for a watch expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WatchId(pub u32);

/// A watch expression.
#[derive(Debug, Clone)]
pub struct Watch {
    /// Unique identifier.
    pub id: WatchId,
    /// The original expression text.
    pub expression: String,
    /// Parsed expression (if valid).
    pub parsed: Option<WatchExpression>,
    /// Whether this watch is enabled.
    pub enabled: bool,
    /// Last evaluated value.
    pub last_value: Option<WatchResult>,
    /// Display format override.
    pub format: Option<WatchFormat>,
}

impl Watch {
    /// Creates a new watch expression.
    pub fn new(id: WatchId, expression: impl Into<String>) -> Self {
        let expr_text = expression.into();
        let parsed = WatchExpression::parse(&expr_text);

        Self {
            id,
            expression: expr_text,
            parsed,
            enabled: true,
            last_value: None,
            format: None,
        }
    }

    /// Returns true if the expression was parsed successfully.
    pub fn is_valid(&self) -> bool {
        self.parsed.is_some()
    }
}

/// The result of evaluating a watch expression.
#[derive(Debug, Clone)]
pub enum WatchResult {
    /// Successfully evaluated to a value.
    Value {
        value: DebugValue,
        var_type: DebugType,
    },
    /// Expression refers to a variable not in scope.
    NotInScope,
    /// Expression has a syntax error.
    ParseError(String),
    /// Evaluation failed for some other reason.
    Error(String),
}

impl WatchResult {
    /// Creates a successful result.
    pub fn value(value: DebugValue, var_type: DebugType) -> Self {
        WatchResult::Value { value, var_type }
    }

    /// Creates a not-in-scope result.
    pub fn not_in_scope() -> Self {
        WatchResult::NotInScope
    }

    /// Creates a parse error result.
    pub fn parse_error(msg: impl Into<String>) -> Self {
        WatchResult::ParseError(msg.into())
    }

    /// Creates a general error result.
    pub fn error(msg: impl Into<String>) -> Self {
        WatchResult::Error(msg.into())
    }

    /// Returns a display string for this result.
    pub fn display(&self) -> String {
        match self {
            WatchResult::Value { value, .. } => value.to_string(),
            WatchResult::NotInScope => "<not in scope>".to_string(),
            WatchResult::ParseError(msg) => format!("<parse error: {}>", msg),
            WatchResult::Error(msg) => format!("<error: {}>", msg),
        }
    }
}

/// Display format for watch values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WatchFormat {
    /// Default format (decimal for numbers, quoted for strings).
    Default,
    /// Hexadecimal format for numbers.
    Hex,
    /// Binary format for numbers.
    Binary,
    /// Octal format for numbers.
    Octal,
    /// Character format (show ASCII character).
    Char,
}

/// A parsed watch expression.
#[derive(Debug, Clone)]
pub enum WatchExpression {
    /// Simple variable reference.
    Variable { name: String },
    /// Array element access.
    ArrayElement {
        name: String,
        indices: Vec<IndexExpr>,
    },
    /// UDT member access.
    MemberAccess {
        base: Box<WatchExpression>,
        member: String,
    },
}

impl WatchExpression {
    /// Parses a watch expression string.
    ///
    /// Supports:
    /// - Simple names: `x`, `counter%`, `name$`
    /// - Array access: `arr(1)`, `matrix(i, j)`
    /// - Member access: `player.x`, `arr(1).member`
    pub fn parse(expr: &str) -> Option<Self> {
        let expr = expr.trim();
        if expr.is_empty() {
            return None;
        }

        // Split by dots for member access
        let parts: Vec<&str> = expr.split('.').collect();

        if parts.len() == 1 {
            // No dots - simple variable or array
            Self::parse_base(parts[0])
        } else {
            // Member access chain
            let mut result = Self::parse_base(parts[0])?;
            for &member in &parts[1..] {
                result = WatchExpression::MemberAccess {
                    base: Box::new(result),
                    member: member.to_string(),
                };
            }
            Some(result)
        }
    }

    /// Parses a base expression (variable or array access).
    fn parse_base(expr: &str) -> Option<Self> {
        let expr = expr.trim();

        // Check for array access
        if let Some(paren_pos) = expr.find('(') {
            // Has parentheses - array access
            if !expr.ends_with(')') {
                return None; // Malformed
            }

            let name = expr[..paren_pos].trim().to_string();
            let indices_str = &expr[paren_pos + 1..expr.len() - 1];

            // Parse indices (comma-separated)
            let indices: Vec<IndexExpr> = indices_str
                .split(',')
                .map(|s| IndexExpr::parse(s.trim()))
                .collect::<Option<Vec<_>>>()?;

            Some(WatchExpression::ArrayElement { name, indices })
        } else {
            // Simple variable
            Some(WatchExpression::Variable {
                name: expr.to_string(),
            })
        }
    }

    /// Returns the base variable name.
    pub fn base_name(&self) -> &str {
        match self {
            WatchExpression::Variable { name } => name,
            WatchExpression::ArrayElement { name, .. } => name,
            WatchExpression::MemberAccess { base, .. } => base.base_name(),
        }
    }
}

/// An array index expression.
#[derive(Debug, Clone)]
pub enum IndexExpr {
    /// Literal integer index.
    Literal(i64),
    /// Variable reference as index.
    Variable(String),
}

impl IndexExpr {
    /// Parses an index expression.
    fn parse(expr: &str) -> Option<Self> {
        let expr = expr.trim();

        // Try parsing as integer
        if let Ok(n) = expr.parse::<i64>() {
            return Some(IndexExpr::Literal(n));
        }

        // Otherwise treat as variable name
        if !expr.is_empty()
            && expr.chars().all(|c| {
                c.is_alphanumeric()
                    || c == '_'
                    || c == '$'
                    || c == '%'
                    || c == '&'
                    || c == '!'
                    || c == '#'
            })
        {
            return Some(IndexExpr::Variable(expr.to_string()));
        }

        None
    }

    /// Evaluates this index to a concrete value (stub).
    pub fn evaluate(&self, _symbols: &DebugSymbols, _scope: DebugScopeId) -> Option<i64> {
        match self {
            IndexExpr::Literal(n) => Some(*n),
            IndexExpr::Variable(_name) => {
                // TODO: Look up variable value in runtime state
                None
            }
        }
    }
}

/// Manager for watch expressions.
#[derive(Debug)]
pub struct WatchManager {
    /// All watches, indexed by ID.
    watches: HashMap<WatchId, Watch>,
    /// Next watch ID.
    next_id: u32,
    /// Watches ordered by creation time.
    order: Vec<WatchId>,
}

impl WatchManager {
    /// Creates a new watch manager.
    pub fn new() -> Self {
        Self {
            watches: HashMap::new(),
            next_id: 1,
            order: Vec::new(),
        }
    }

    /// Adds a new watch expression.
    pub fn add(&mut self, expression: impl Into<String>) -> WatchId {
        let id = WatchId(self.next_id);
        self.next_id += 1;

        let watch = Watch::new(id, expression);
        self.watches.insert(id, watch);
        self.order.push(id);

        id
    }

    /// Removes a watch by ID.
    pub fn remove(&mut self, id: WatchId) -> bool {
        if self.watches.remove(&id).is_some() {
            self.order.retain(|&i| i != id);
            true
        } else {
            false
        }
    }

    /// Gets a watch by ID.
    pub fn get(&self, id: WatchId) -> Option<&Watch> {
        self.watches.get(&id)
    }

    /// Gets a mutable watch by ID.
    pub fn get_mut(&mut self, id: WatchId) -> Option<&mut Watch> {
        self.watches.get_mut(&id)
    }

    /// Returns all watches in order.
    pub fn all(&self) -> impl Iterator<Item = &Watch> {
        self.order.iter().filter_map(|id| self.watches.get(id))
    }

    /// Returns the number of watches.
    pub fn len(&self) -> usize {
        self.watches.len()
    }

    /// Returns true if there are no watches.
    pub fn is_empty(&self) -> bool {
        self.watches.is_empty()
    }

    /// Enables a watch.
    pub fn enable(&mut self, id: WatchId) -> bool {
        if let Some(watch) = self.watches.get_mut(&id) {
            watch.enabled = true;
            true
        } else {
            false
        }
    }

    /// Disables a watch.
    pub fn disable(&mut self, id: WatchId) -> bool {
        if let Some(watch) = self.watches.get_mut(&id) {
            watch.enabled = false;
            true
        } else {
            false
        }
    }

    /// Sets the display format for a watch.
    pub fn set_format(&mut self, id: WatchId, format: WatchFormat) -> bool {
        if let Some(watch) = self.watches.get_mut(&id) {
            watch.format = Some(format);
            true
        } else {
            false
        }
    }

    /// Clears all watches.
    pub fn clear(&mut self) {
        self.watches.clear();
        self.order.clear();
    }

    /// Evaluates all enabled watches in the given context.
    ///
    /// Note: This is a stub. Actual evaluation requires runtime state.
    pub fn evaluate_all(&mut self, _symbols: &DebugSymbols, _scope: DebugScopeId, _frame: FrameId) {
        for watch in self.watches.values_mut() {
            if watch.enabled {
                // TODO: Implement actual evaluation with runtime state
                watch.last_value = Some(WatchResult::error("Runtime not connected"));
            }
        }
    }

    /// Evaluates a single watch expression.
    ///
    /// Note: This is a stub. Actual evaluation requires runtime state.
    pub fn evaluate(
        &self,
        expression: &str,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
    ) -> WatchResult {
        // Try to parse
        let parsed = match WatchExpression::parse(expression) {
            Some(p) => p,
            None => return WatchResult::parse_error("Invalid expression syntax"),
        };

        // Check if the base variable exists in scope
        let base_name = parsed.base_name();
        if symbols.lookup_variable(base_name, scope).is_none() {
            return WatchResult::not_in_scope();
        }

        // TODO: Implement actual evaluation with runtime state
        WatchResult::error("Runtime not connected")
    }
}

impl Default for WatchManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_variable() {
        let expr = WatchExpression::parse("x").unwrap();
        assert!(matches!(expr, WatchExpression::Variable { name } if name == "x"));

        let expr = WatchExpression::parse("counter%").unwrap();
        assert!(matches!(expr, WatchExpression::Variable { name } if name == "counter%"));
    }

    #[test]
    fn test_parse_array_access() {
        let expr = WatchExpression::parse("arr(1)").unwrap();
        if let WatchExpression::ArrayElement { name, indices } = expr {
            assert_eq!(name, "arr");
            assert_eq!(indices.len(), 1);
            assert!(matches!(indices[0], IndexExpr::Literal(1)));
        } else {
            panic!("Expected ArrayElement");
        }

        let expr = WatchExpression::parse("matrix(i, 5)").unwrap();
        if let WatchExpression::ArrayElement { name, indices } = expr {
            assert_eq!(name, "matrix");
            assert_eq!(indices.len(), 2);
            assert!(matches!(&indices[0], IndexExpr::Variable(v) if v == "i"));
            assert!(matches!(indices[1], IndexExpr::Literal(5)));
        } else {
            panic!("Expected ArrayElement");
        }
    }

    #[test]
    fn test_parse_member_access() {
        let expr = WatchExpression::parse("player.x").unwrap();
        if let WatchExpression::MemberAccess { base, member } = expr {
            assert!(matches!(*base, WatchExpression::Variable { name } if name == "player"));
            assert_eq!(member, "x");
        } else {
            panic!("Expected MemberAccess");
        }
    }

    #[test]
    fn test_parse_complex_expression() {
        let expr = WatchExpression::parse("enemies(i).health").unwrap();
        if let WatchExpression::MemberAccess { base, member } = expr {
            assert_eq!(member, "health");
            if let WatchExpression::ArrayElement { name, indices } = *base {
                assert_eq!(name, "enemies");
                assert!(matches!(&indices[0], IndexExpr::Variable(v) if v == "i"));
            } else {
                panic!("Expected ArrayElement base");
            }
        } else {
            panic!("Expected MemberAccess");
        }
    }

    #[test]
    fn test_watch_manager() {
        let mut manager = WatchManager::new();

        let id1 = manager.add("x");
        let id2 = manager.add("arr(1)");

        assert_eq!(manager.len(), 2);
        assert!(manager.get(id1).unwrap().is_valid());
        assert!(manager.get(id2).unwrap().is_valid());

        manager.disable(id1);
        assert!(!manager.get(id1).unwrap().enabled);

        manager.remove(id1);
        assert_eq!(manager.len(), 1);
        assert!(manager.get(id1).is_none());
    }

    #[test]
    fn test_watch_format() {
        let mut manager = WatchManager::new();
        let id = manager.add("counter");

        assert!(manager.get(id).unwrap().format.is_none());

        manager.set_format(id, WatchFormat::Hex);
        assert_eq!(manager.get(id).unwrap().format, Some(WatchFormat::Hex));
    }
}
