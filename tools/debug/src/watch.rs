//! Variable watch expression management.
//!
//! This module handles watch expressions - user-defined expressions that
//! are evaluated and displayed during debugging. It provides:
//!
//! - **Watch expression parsing**: Parse BASIC variable references
//! - **Watch management**: Add, remove, enable/disable watches
//! - **Expression evaluation**: Evaluate expressions with runtime state access
//!
//! Watch expressions can be:
//! - Simple variables: `x`, `counter%`
//! - Array elements: `arr(1, 2)`, `matrix(i, j)`
//! - UDT members: `player.x`, `enemies(i).health`
//! - Nested access: `arr(1).member.field`
//!
//! ## Runtime State Access
//!
//! Evaluation requires a runtime state provider that implements the [`RuntimeState`]
//! trait. The [`CallStack`] type implements this trait, allowing variable values
//! to be read from the current call stack frames.
//!
//! ## Example
//!
//! ```no_run
//! use qb64fresh_debug::{WatchManager, CallStack, DebugSymbols, DebugScopeId, FrameId};
//!
//! let mut manager = WatchManager::new();
//! let id = manager.add("x");
//!
//! // Evaluate with runtime state
//! let symbols = DebugSymbols::new();
//! let call_stack = CallStack::new();
//! manager.evaluate_all(&symbols, DebugScopeId::GLOBAL, FrameId::MAIN, &call_stack);
//!
//! // Get result
//! if let Some(watch) = manager.get(id) {
//!     if let Some(result) = &watch.last_value {
//!         println!("Value: {}", result.display());
//!     }
//! }
//! ```

use crate::frames::{CallStack, FrameId};
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

/// Trait for accessing runtime variable values during debugging.
///
/// This trait abstracts the mechanism for looking up variable values
/// from the running program's state. Implementations can read from
/// memory, debug info, or mock data for testing.
pub trait RuntimeState {
    /// Looks up a variable value by name in the given scope.
    ///
    /// Returns `Some(value)` if the variable exists and can be read,
    /// or `None` if the variable is not found or unavailable.
    fn lookup_variable(&self, name: &str, scope: DebugScopeId) -> Option<DebugValue>;
}

impl RuntimeState for CallStack {
    /// Looks up a variable value from the call stack.
    ///
    /// Searches through frames starting from the current frame, continuing
    /// through all parent scopes up to global scope. This matches the behavior
    /// of `DebugSymbols::lookup_variable` which searches parent scopes recursively.
    ///
    /// # Arguments
    ///
    /// * `name` - Variable name to look up (case-insensitive)
    /// * `scope` - Starting scope ID (currently unused, searches all frames)
    ///
    /// # Returns
    ///
    /// `Some(DebugValue)` if found in any frame, `None` otherwise.
    fn lookup_variable(&self, name: &str, _scope: DebugScopeId) -> Option<DebugValue> {
        // Search through frames from innermost (current) to outermost (main/global)
        // This naturally follows the scope hierarchy
        for frame in self.iter() {
            // Check locals in this frame
            for local in &frame.locals {
                if local.name.eq_ignore_ascii_case(name) {
                    return Some(local.value.clone());
                }
            }

            // Check arguments in this frame
            for arg in &frame.arguments {
                if arg.name.eq_ignore_ascii_case(name) {
                    return Some(arg.value.clone());
                }
            }
        }

        // Note: Global variables should be in the outermost frame (main frame)
        // If not found in any frame, the variable is not available
        None
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

    /// Evaluates this index to a concrete integer value.
    ///
    /// For literal indices, returns the value directly.
    /// For variable indices, looks up the variable value and converts it to i64.
    ///
    /// # Arguments
    ///
    /// * `symbols` - Debug symbols for type information
    /// * `scope` - Current scope ID
    /// * `runtime` - Runtime state for variable value lookup
    ///
    /// # Returns
    ///
    /// `Some(i64)` if the index can be evaluated, `None` otherwise.
    pub fn evaluate<R: RuntimeState>(
        &self,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        runtime: &R,
    ) -> Option<i64> {
        match self {
            IndexExpr::Literal(n) => Some(*n),
            IndexExpr::Variable(name) => {
                // Look up variable value in runtime state
                let value = runtime.lookup_variable(name, scope)?;

                // Convert to i64 based on value type
                match value {
                    DebugValue::Integer(n) => Some(n),
                    DebugValue::UnsignedInteger(n) => {
                        // Clamp to i64::MAX if value exceeds signed range
                        if n > i64::MAX as u64 {
                            None
                        } else {
                            Some(n as i64)
                        }
                    }
                    DebugValue::Float(f) => Some(f as i64),
                    _ => None, // String, array, UDT, etc. cannot be used as index
                }
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
    /// Uses the provided runtime state to look up variable values.
    ///
    /// # Arguments
    ///
    /// * `symbols` - Debug symbols for type information
    /// * `scope` - Current scope ID
    /// * `frame` - Current frame ID (for scope resolution)
    /// * `runtime` - Runtime state for variable value lookup
    pub fn evaluate_all<R: RuntimeState>(
        &mut self,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        _frame: FrameId,
        runtime: &R,
    ) {
        // Collect watch IDs and expressions to avoid borrowing issues
        let watch_data: Vec<(WatchId, String, Option<WatchExpression>)> = self
            .watches
            .iter()
            .filter(|(_, w)| w.enabled)
            .map(|(id, w)| (*id, w.expression.clone(), w.parsed.clone()))
            .collect();

        // Evaluate each watch
        for (id, expr, parsed) in watch_data {
            let result = Self::evaluate_internal_static(
                &expr,
                parsed.as_ref(),
                symbols,
                scope,
                runtime,
            );
            if let Some(watch) = self.watches.get_mut(&id) {
                watch.last_value = Some(result);
            }
        }
    }

    /// Evaluates a single watch expression.
    ///
    /// Uses the provided runtime state to look up variable values.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression string to evaluate
    /// * `symbols` - Debug symbols for type information
    /// * `scope` - Current scope ID
    /// * `runtime` - Runtime state for variable value lookup
    ///
    /// # Returns
    ///
    /// A `WatchResult` containing the evaluated value or an error.
    pub fn evaluate<R: RuntimeState>(
        &self,
        expression: &str,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        runtime: &R,
    ) -> WatchResult {
        let parsed = WatchExpression::parse(expression);
        self.evaluate_internal(expression, parsed.as_ref(), symbols, scope, runtime)
    }

    /// Internal evaluation helper that works with an optional parsed expression.
    fn evaluate_internal<R: RuntimeState>(
        &self,
        expression: &str,
        parsed: Option<&WatchExpression>,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        runtime: &R,
    ) -> WatchResult {
        Self::evaluate_internal_static(expression, parsed, symbols, scope, runtime)
    }

    /// Static version of evaluate_internal to avoid borrowing issues.
    fn evaluate_internal_static<R: RuntimeState>(
        expression: &str,
        parsed: Option<&WatchExpression>,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        runtime: &R,
    ) -> WatchResult {
        // Parse if not already parsed
        let parsed = match parsed {
            Some(p) => p,
            None => {
                return match WatchExpression::parse(expression) {
                    Some(p) => Self::evaluate_expression_static(&p, symbols, scope, runtime),
                    None => WatchResult::parse_error("Invalid expression syntax"),
                };
            }
        };

        Self::evaluate_expression_static(parsed, symbols, scope, runtime)
    }

    /// Evaluates a parsed watch expression.
    #[allow(dead_code)] // May be used by external code
    fn evaluate_expression<R: RuntimeState>(
        &self,
        expr: &WatchExpression,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        runtime: &R,
    ) -> WatchResult {
        Self::evaluate_expression_static(expr, symbols, scope, runtime)
    }

    /// Static version of evaluate_expression to avoid borrowing issues.
    fn evaluate_expression_static<R: RuntimeState>(
        expr: &WatchExpression,
        symbols: &DebugSymbols,
        scope: DebugScopeId,
        runtime: &R,
    ) -> WatchResult {
        match expr {
            WatchExpression::Variable { name } => {
                // Look up variable in symbols to get type
                let var_info = match symbols.lookup_variable(name, scope) {
                    Some(v) => v,
                    None => return WatchResult::not_in_scope(),
                };

                // Look up value in runtime state
                let value = match runtime.lookup_variable(name, scope) {
                    Some(v) => v,
                    None => {
                        return WatchResult::error(format!(
                            "Variable '{}' not available in runtime state",
                            name
                        ));
                    }
                };

                WatchResult::value(value, var_info.var_type.clone())
            }
            WatchExpression::ArrayElement { name, indices } => {
                // Look up variable in symbols
                let var_info = match symbols.lookup_variable(name, scope) {
                    Some(v) => v,
                    None => return WatchResult::not_in_scope(),
                };

                // Get array type
                let element_type = match &var_info.var_type {
                    DebugType::Array { element_type, .. } => element_type.as_ref(),
                    _ => {
                        return WatchResult::error(format!(
                            "'{}' is not an array",
                            name
                        ));
                    }
                };

                // Evaluate all indices
                let mut evaluated_indices = Vec::new();
                for idx_expr in indices {
                    match idx_expr.evaluate(symbols, scope, runtime) {
                        Some(idx) => evaluated_indices.push(idx),
                        None => {
                            return WatchResult::error(format!(
                                "Could not evaluate array index"
                            ));
                        }
                    }
                }

                // Look up array value
                let array_value = match runtime.lookup_variable(name, scope) {
                    Some(v) => v,
                    None => {
                        return WatchResult::error(format!(
                            "Array '{}' not available in runtime state",
                            name
                        ));
                    }
                };

                // Access array element
                match array_value {
                    DebugValue::Array(arr) => {
                        match arr.get(&evaluated_indices) {
                            Some(element_value) => {
                                WatchResult::value(element_value.clone(), element_type.clone())
                            }
                            None => WatchResult::error(format!(
                                "Array index out of bounds: {:?}",
                                evaluated_indices
                            )),
                        }
                    }
                    _ => WatchResult::error(format!("'{}' is not an array value", name)),
                }
            }
            WatchExpression::MemberAccess { base, member } => {
                // Evaluate base expression first
                let base_result = Self::evaluate_expression_static(base, symbols, scope, runtime);
                let (base_value, base_type) = match base_result {
                    WatchResult::Value { value, var_type } => (value, var_type),
                    other => return other,
                };

                // Resolve the base type to find the member type in symbols
                // For nested member access, we already have the type from the base evaluation
                // For simple base expressions, get it from symbols (more accurate for UDTs)
                let resolved_base_type = match base.as_ref() {
                    WatchExpression::Variable { name } => {
                        // Get variable type from symbols (preferred for UDT resolution)
                        symbols
                            .lookup_variable(name, scope)
                            .map(|v| v.var_type.clone())
                            .unwrap_or(base_type)
                    }
                    WatchExpression::ArrayElement { name, .. } => {
                        // For array elements, get the element type from symbols
                        symbols
                            .lookup_variable(name, scope)
                            .and_then(|v| match &v.var_type {
                                DebugType::Array { element_type, .. } => {
                                    Some(*element_type.clone())
                                }
                                _ => None,
                            })
                            .unwrap_or(base_type)
                    }
                    WatchExpression::MemberAccess { .. } => {
                        // For nested member access, use the type from the evaluation result
                        base_type
                    }
                };

                // Access member from UDT value
                match base_value {
                    DebugValue::UserDefined(udt) => {
                        match udt.get_member(member) {
                            Some(member_value) => {
                                // Try to get member type from symbols
                                let member_type = match resolved_base_type {
                                    DebugType::UserDefined(type_name) => {
                                        symbols
                                            .lookup_type(&type_name)
                                            .and_then(|udt_type| {
                                                udt_type
                                                    .members
                                                    .iter()
                                                    .find(|m| {
                                                        m.name.eq_ignore_ascii_case(member)
                                                    })
                                                    .map(|m| m.member_type.clone())
                                            })
                                            .unwrap_or(DebugType::Unknown)
                                    }
                                    _ => DebugType::Unknown,
                                };

                                WatchResult::value(member_value.clone(), member_type)
                            }
                            None => WatchResult::error(format!(
                                "Member '{}' not found in UDT",
                                member
                            )),
                        }
                    }
                    _ => WatchResult::error(format!(
                        "Cannot access member '{}' on non-UDT value",
                        member
                    )),
                }
            }
        }
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
