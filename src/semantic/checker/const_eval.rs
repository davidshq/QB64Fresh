//! Compile-time constant expression evaluation.
//!
//! This module handles evaluation of constant expressions for:
//! - CONST statement values
//! - DIM array bounds
//! - Any other context requiring compile-time constants
//!
//! The evaluator supports:
//! - Literal values (integer, float, string)
//! - References to other constants
//! - Binary operations on constants
//! - Unary operations on constants
//! - Type conversions

use crate::ast::{BinaryOp, UnaryOp};
use crate::semantic::{
    symbols::{ConstValue, SymbolKind},
    typed_ir::{TypedExpr, TypedExprKind},
    types::BasicType,
};

use super::TypeChecker;

// ============================================================================
// Conditional Compilation Condition Evaluator
// ============================================================================
//
// Evaluates condition strings from $IF/$ELSEIF metacommands.
// Supports expressions like:
//   - Simple identifiers: WIN, LINUX, MAC
//   - Comparisons: WIN = -1, 64BIT <> 0
//   - Boolean operators: WIN AND 64BIT, NOT LINUX, WIN OR MAC
//   - Parentheses: (WIN OR MAC) AND 64BIT
//
// All identifiers are looked up as constants in the symbol table.
// In BASIC convention: -1 = TRUE, 0 = FALSE

impl<'a> TypeChecker<'a> {
    /// Attempts to evaluate a typed expression as a compile-time constant.
    ///
    /// Returns `Some(ConstValue)` if the expression can be evaluated at compile time,
    /// `None` if it contains non-constant elements (like variable references).
    pub(super) fn try_evaluate_const_expr(&self, expr: &TypedExpr) -> Option<ConstValue> {
        match &expr.kind {
            // Direct literals
            TypedExprKind::IntegerLiteral(v) => Some(ConstValue::Integer(*v)),
            TypedExprKind::FloatLiteral(v) => Some(ConstValue::Float(*v)),
            TypedExprKind::StringLiteral(v) => Some(ConstValue::String(v.clone())),

            // Grouped expressions - just unwrap
            TypedExprKind::Grouped(inner) => self.try_evaluate_const_expr(inner),

            // Binary operations on constants
            TypedExprKind::Binary { left, op, right } => {
                let left_val = self.try_evaluate_const_expr(left)?;
                let right_val = self.try_evaluate_const_expr(right)?;
                self.evaluate_binary_const(*op, left_val, right_val)
            }

            // Unary operations on constants
            TypedExprKind::Unary { op, operand } => {
                let operand_val = self.try_evaluate_const_expr(operand)?;
                self.evaluate_unary_const(*op, operand_val)
            }

            // Variable references - check if it's a constant
            TypedExprKind::Variable(name) => {
                if let Some(symbol) = self.symbols.lookup_symbol(name)
                    && let SymbolKind::Constant { value } = &symbol.kind
                {
                    return Some(value.clone());
                }
                None // Not a constant
            }

            // Type conversions - evaluate inner and convert
            TypedExprKind::Convert {
                expr: inner,
                to_type,
            } => {
                let inner_val = self.try_evaluate_const_expr(inner)?;
                self.convert_const_value(inner_val, to_type)
            }

            // Function calls, array access, array refs, and field access are not constant
            TypedExprKind::FunctionCall { .. }
            | TypedExprKind::ArrayAccess { .. }
            | TypedExprKind::ArrayRef { .. }
            | TypedExprKind::FieldAccess { .. } => None,
        }
    }

    /// Evaluates a binary operation on constant values.
    fn evaluate_binary_const(
        &self,
        op: BinaryOp,
        left: ConstValue,
        right: ConstValue,
    ) -> Option<ConstValue> {
        match (left, right) {
            // Integer operations
            (ConstValue::Integer(l), ConstValue::Integer(r)) => {
                let result = match op {
                    BinaryOp::Add => l.checked_add(r)?,
                    BinaryOp::Subtract => l.checked_sub(r)?,
                    BinaryOp::Multiply => l.checked_mul(r)?,
                    BinaryOp::Divide => l.checked_div(r)?,
                    BinaryOp::IntDivide => l.checked_div(r)?,
                    BinaryOp::Modulo => l.checked_rem(r)?,
                    BinaryOp::Power => l.checked_pow(r.try_into().ok()?)?,
                    BinaryOp::And => l & r,
                    BinaryOp::Or => l | r,
                    BinaryOp::Xor => l ^ r,
                    BinaryOp::Eqv => !(l ^ r),
                    BinaryOp::Imp => !l | r,
                    BinaryOp::Equal => {
                        if l == r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::NotEqual => {
                        if l != r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LessThan => {
                        if l < r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LessEqual => {
                        if l <= r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l > r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::GreaterEqual => {
                        if l >= r {
                            -1
                        } else {
                            0
                        }
                    }
                };
                Some(ConstValue::Integer(result))
            }

            // Float operations
            (ConstValue::Float(l), ConstValue::Float(r)) => {
                let result = match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Subtract => l - r,
                    BinaryOp::Multiply => l * r,
                    BinaryOp::Divide => l / r,
                    BinaryOp::Power => l.powf(r),
                    BinaryOp::Equal => {
                        if l == r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::NotEqual => {
                        if l != r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::LessThan => {
                        if l < r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::LessEqual => {
                        if l <= r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l > r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::GreaterEqual => {
                        if l >= r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    _ => return None, // Bitwise ops not valid on floats
                };
                Some(ConstValue::Float(result))
            }

            // Mixed int/float - promote to float
            (ConstValue::Integer(l), ConstValue::Float(r)) => {
                self.evaluate_binary_const(op, ConstValue::Float(l as f64), ConstValue::Float(r))
            }
            (ConstValue::Float(l), ConstValue::Integer(r)) => {
                self.evaluate_binary_const(op, ConstValue::Float(l), ConstValue::Float(r as f64))
            }

            // String concatenation
            (ConstValue::String(l), ConstValue::String(r)) => match op {
                BinaryOp::Add => Some(ConstValue::String(l + &r)),
                BinaryOp::Equal => Some(ConstValue::Integer(if l == r { -1 } else { 0 })),
                BinaryOp::NotEqual => Some(ConstValue::Integer(if l != r { -1 } else { 0 })),
                BinaryOp::LessThan => Some(ConstValue::Integer(if l < r { -1 } else { 0 })),
                BinaryOp::LessEqual => Some(ConstValue::Integer(if l <= r { -1 } else { 0 })),
                BinaryOp::GreaterThan => Some(ConstValue::Integer(if l > r { -1 } else { 0 })),
                BinaryOp::GreaterEqual => Some(ConstValue::Integer(if l >= r { -1 } else { 0 })),
                _ => None,
            },

            // String + non-string not allowed
            _ => None,
        }
    }

    /// Evaluates a unary operation on a constant value.
    fn evaluate_unary_const(&self, op: UnaryOp, operand: ConstValue) -> Option<ConstValue> {
        match (op, operand) {
            (UnaryOp::Negate, ConstValue::Integer(v)) => Some(ConstValue::Integer(-v)),
            (UnaryOp::Negate, ConstValue::Float(v)) => Some(ConstValue::Float(-v)),
            (UnaryOp::Not, ConstValue::Integer(v)) => Some(ConstValue::Integer(!v)),
            _ => None,
        }
    }

    /// Converts a constant value to a different type.
    fn convert_const_value(&self, value: ConstValue, to_type: &BasicType) -> Option<ConstValue> {
        match (value, to_type) {
            // Integer to float
            (ConstValue::Integer(v), BasicType::Single | BasicType::Double) => {
                Some(ConstValue::Float(v as f64))
            }
            // Float to integer (truncate)
            (ConstValue::Float(v), BasicType::Integer | BasicType::Long) => {
                Some(ConstValue::Integer(v as i64))
            }
            // Same type - no conversion needed
            (v @ ConstValue::Integer(_), BasicType::Integer | BasicType::Long) => Some(v),
            (v @ ConstValue::Float(_), BasicType::Single | BasicType::Double) => Some(v),
            (v @ ConstValue::String(_), BasicType::String) => Some(v),
            // Unsigned types
            (ConstValue::Integer(v), BasicType::UnsignedInteger | BasicType::UnsignedLong) => {
                Some(ConstValue::Integer(v))
            }
            _ => None,
        }
    }

    // ========================================================================
    // Metacommand Condition Evaluation ($IF condition THEN)
    // ========================================================================

    /// Evaluates a condition string from `$IF` / `$ELSEIF` metacommands.
    ///
    /// Returns `true` if the condition evaluates to a non-zero value (BASIC TRUE),
    /// `false` otherwise. Returns `false` for unparseable conditions.
    ///
    /// # Supported syntax
    /// - Identifiers: `WIN`, `LINUX`, `64BIT` (looked up as constants)
    /// - Comparisons: `WIN = -1`, `64BIT <> 0`
    /// - NOT operator: `NOT LINUX`
    /// - AND/OR: `WIN AND 64BIT`, `WIN OR MAC`
    /// - Parentheses: `(WIN OR MAC) AND 64BIT`
    ///
    /// # Examples
    /// ```ignore
    /// // $IF WIN THEN
    /// evaluator.evaluate_meta_condition("WIN") // true on Windows
    ///
    /// // $IF NOT LINUX THEN
    /// evaluator.evaluate_meta_condition("NOT LINUX") // true on non-Linux
    ///
    /// // $IF 64BIT AND (WIN OR MAC) THEN
    /// evaluator.evaluate_meta_condition("64BIT AND (WIN OR MAC)")
    /// ```
    pub fn evaluate_meta_condition(&self, condition: &str) -> bool {
        match self.parse_and_eval_condition(condition.trim()) {
            Some(value) => value != 0,
            None => false, // Unparseable conditions are treated as false
        }
    }

    /// Parses and evaluates a condition string, returning an integer value.
    /// In BASIC: -1 = TRUE, 0 = FALSE
    fn parse_and_eval_condition(&self, input: &str) -> Option<i64> {
        let tokens = self.tokenize_condition(input);
        if tokens.is_empty() {
            return None;
        }
        let mut pos = 0;
        self.parse_or_expr(&tokens, &mut pos)
    }

    /// Tokenizes a condition string into a list of tokens.
    fn tokenize_condition(&self, input: &str) -> Vec<CondToken> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                ' ' | '\t' => {
                    chars.next();
                }
                '(' => {
                    tokens.push(CondToken::LParen);
                    chars.next();
                }
                ')' => {
                    tokens.push(CondToken::RParen);
                    chars.next();
                }
                '=' => {
                    tokens.push(CondToken::Eq);
                    chars.next();
                }
                '<' => {
                    chars.next();
                    if chars.peek() == Some(&'>') {
                        chars.next();
                        tokens.push(CondToken::NotEq);
                    } else if chars.peek() == Some(&'=') {
                        chars.next();
                        tokens.push(CondToken::LtEq);
                    } else {
                        tokens.push(CondToken::Lt);
                    }
                }
                '>' => {
                    chars.next();
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        tokens.push(CondToken::GtEq);
                    } else {
                        tokens.push(CondToken::Gt);
                    }
                }
                '-' => {
                    // Could be negative number (possibly with spaces: "- 1" is "-1")
                    chars.next();
                    // Skip any whitespace between '-' and digits
                    while let Some(&c) = chars.peek() {
                        if c == ' ' || c == '\t' {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    let mut num_str = String::from("-");
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_digit() {
                            num_str.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if num_str.len() > 1
                        && let Ok(n) = num_str.parse::<i64>()
                    {
                        tokens.push(CondToken::Number(n));
                    }
                    // If just "-", we skip it (not a valid token)
                }
                '0'..='9' => {
                    // Could be a number or an identifier like "64BIT"
                    let mut str_val = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            str_val.push(c.to_ascii_uppercase());
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    // Check if it's purely numeric
                    if str_val.chars().all(|c| c.is_ascii_digit()) {
                        if let Ok(n) = str_val.parse::<i64>() {
                            tokens.push(CondToken::Number(n));
                        }
                    } else {
                        // It's an identifier like "64BIT"
                        tokens.push(CondToken::Ident(str_val));
                    }
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut ident = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            ident.push(c.to_ascii_uppercase());
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    // Check for keywords
                    match ident.as_str() {
                        "AND" => tokens.push(CondToken::And),
                        "OR" => tokens.push(CondToken::Or),
                        "NOT" => tokens.push(CondToken::Not),
                        "XOR" => tokens.push(CondToken::Xor),
                        _ => tokens.push(CondToken::Ident(ident)),
                    }
                }
                _ => {
                    chars.next(); // Skip unknown characters
                }
            }
        }
        tokens
    }

    /// Parses OR expressions (lowest precedence).
    fn parse_or_expr(&self, tokens: &[CondToken], pos: &mut usize) -> Option<i64> {
        let mut left = self.parse_xor_expr(tokens, pos)?;
        while *pos < tokens.len() {
            if let CondToken::Or = &tokens[*pos] {
                *pos += 1;
                let right = self.parse_xor_expr(tokens, pos)?;
                left |= right;
            } else {
                break;
            }
        }
        Some(left)
    }

    /// Parses XOR expressions.
    fn parse_xor_expr(&self, tokens: &[CondToken], pos: &mut usize) -> Option<i64> {
        let mut left = self.parse_and_expr(tokens, pos)?;
        while *pos < tokens.len() {
            if let CondToken::Xor = &tokens[*pos] {
                *pos += 1;
                let right = self.parse_and_expr(tokens, pos)?;
                left ^= right;
            } else {
                break;
            }
        }
        Some(left)
    }

    /// Parses AND expressions.
    fn parse_and_expr(&self, tokens: &[CondToken], pos: &mut usize) -> Option<i64> {
        let mut left = self.parse_not_expr(tokens, pos)?;
        while *pos < tokens.len() {
            if let CondToken::And = &tokens[*pos] {
                *pos += 1;
                let right = self.parse_not_expr(tokens, pos)?;
                left &= right;
            } else {
                break;
            }
        }
        Some(left)
    }

    /// Parses NOT expressions (unary).
    fn parse_not_expr(&self, tokens: &[CondToken], pos: &mut usize) -> Option<i64> {
        if *pos < tokens.len() && matches!(&tokens[*pos], CondToken::Not) {
            *pos += 1;
            let operand = self.parse_not_expr(tokens, pos)?;
            return Some(!operand);
        }
        self.parse_comparison(tokens, pos)
    }

    /// Parses comparison expressions.
    fn parse_comparison(&self, tokens: &[CondToken], pos: &mut usize) -> Option<i64> {
        let left = self.parse_primary(tokens, pos)?;
        if *pos < tokens.len() {
            match &tokens[*pos] {
                CondToken::Eq => {
                    *pos += 1;
                    let right = self.parse_primary(tokens, pos)?;
                    return Some(if left == right { -1 } else { 0 });
                }
                CondToken::NotEq => {
                    *pos += 1;
                    let right = self.parse_primary(tokens, pos)?;
                    return Some(if left != right { -1 } else { 0 });
                }
                CondToken::Lt => {
                    *pos += 1;
                    let right = self.parse_primary(tokens, pos)?;
                    return Some(if left < right { -1 } else { 0 });
                }
                CondToken::LtEq => {
                    *pos += 1;
                    let right = self.parse_primary(tokens, pos)?;
                    return Some(if left <= right { -1 } else { 0 });
                }
                CondToken::Gt => {
                    *pos += 1;
                    let right = self.parse_primary(tokens, pos)?;
                    return Some(if left > right { -1 } else { 0 });
                }
                CondToken::GtEq => {
                    *pos += 1;
                    let right = self.parse_primary(tokens, pos)?;
                    return Some(if left >= right { -1 } else { 0 });
                }
                _ => {}
            }
        }
        Some(left)
    }

    /// Parses primary expressions (identifiers, numbers, parenthesized expressions).
    fn parse_primary(&self, tokens: &[CondToken], pos: &mut usize) -> Option<i64> {
        if *pos >= tokens.len() {
            return None;
        }
        match &tokens[*pos] {
            CondToken::Number(n) => {
                *pos += 1;
                Some(*n)
            }
            CondToken::Ident(name) => {
                *pos += 1;
                // Look up the identifier in the symbol table
                self.lookup_constant_value(name)
            }
            CondToken::LParen => {
                *pos += 1; // consume '('
                let value = self.parse_or_expr(tokens, pos)?;
                if *pos < tokens.len() && matches!(&tokens[*pos], CondToken::RParen) {
                    *pos += 1; // consume ')'
                }
                Some(value)
            }
            _ => None,
        }
    }

    /// Looks up a constant identifier in the symbol table.
    fn lookup_constant_value(&self, name: &str) -> Option<i64> {
        if let Some(symbol) = self.symbols.lookup_symbol(name)
            && let SymbolKind::Constant { value } = &symbol.kind
        {
            return match value {
                ConstValue::Integer(v) => Some(*v),
                ConstValue::Float(v) => Some(*v as i64),
                ConstValue::String(_) => None, // Strings not valid in boolean context
            };
        }
        // Unknown identifier treated as 0 (FALSE)
        Some(0)
    }
}

/// Token type for condition parsing.
#[derive(Debug, Clone, PartialEq)]
enum CondToken {
    Ident(String),
    Number(i64),
    And,
    Or,
    Not,
    Xor,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    LParen,
    RParen,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    // Helper to create a typed integer literal expression
    fn int_lit(v: i64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::IntegerLiteral(v),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a typed float literal expression
    fn float_lit(v: f64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::FloatLiteral(v),
            basic_type: BasicType::Double,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a typed string literal expression
    fn string_lit(v: &str) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::StringLiteral(v.to_string()),
            basic_type: BasicType::String,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a binary expression
    fn binary(left: TypedExpr, op: BinaryOp, right: TypedExpr) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a unary expression
    fn unary(op: UnaryOp, operand: TypedExpr) -> TypedExpr {
        let basic_type = operand.basic_type.clone();
        TypedExpr {
            kind: TypedExprKind::Unary {
                op,
                operand: Box::new(operand),
            },
            basic_type,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a type checker
    fn checker() -> TypeChecker<'static> {
        // Create a leaked mutable symbol table for testing
        let symbols = crate::semantic::symbols::SymbolTable::new();
        let symbols_box = Box::new(symbols);
        let symbols_ref: &'static mut _ = Box::leak(symbols_box);
        TypeChecker::new(symbols_ref)
    }

    #[test]
    fn test_evaluate_integer_literal() {
        let tc = checker();
        let expr = int_lit(42);
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_evaluate_float_literal() {
        let tc = checker();
        let expr = float_lit(3.14);
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Float(3.14)));
    }

    #[test]
    fn test_evaluate_string_literal() {
        let tc = checker();
        let expr = string_lit("hello");
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::String("hello".to_string())));
    }

    #[test]
    fn test_evaluate_integer_addition() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Add, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(15)));
    }

    #[test]
    fn test_evaluate_integer_subtraction() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Subtract, int_lit(3));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(7)));
    }

    #[test]
    fn test_evaluate_integer_multiplication() {
        let tc = checker();
        let expr = binary(int_lit(6), BinaryOp::Multiply, int_lit(7));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_evaluate_integer_division() {
        let tc = checker();
        let expr = binary(int_lit(20), BinaryOp::Divide, int_lit(4));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(5)));
    }

    #[test]
    fn test_evaluate_integer_modulo() {
        let tc = checker();
        let expr = binary(int_lit(17), BinaryOp::Modulo, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(2)));
    }

    #[test]
    fn test_evaluate_integer_power() {
        let tc = checker();
        let expr = binary(int_lit(2), BinaryOp::Power, int_lit(10));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(1024)));
    }

    #[test]
    fn test_evaluate_bitwise_and() {
        let tc = checker();
        let expr = binary(int_lit(0b1100), BinaryOp::And, int_lit(0b1010));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(0b1000)));
    }

    #[test]
    fn test_evaluate_bitwise_or() {
        let tc = checker();
        let expr = binary(int_lit(0b1100), BinaryOp::Or, int_lit(0b1010));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(0b1110)));
    }

    #[test]
    fn test_evaluate_bitwise_xor() {
        let tc = checker();
        let expr = binary(int_lit(0b1100), BinaryOp::Xor, int_lit(0b1010));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(0b0110)));
    }

    #[test]
    fn test_evaluate_comparison_equal() {
        let tc = checker();
        let expr = binary(int_lit(5), BinaryOp::Equal, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // TRUE = -1 in BASIC
    }

    #[test]
    fn test_evaluate_comparison_not_equal() {
        let tc = checker();
        let expr = binary(int_lit(5), BinaryOp::NotEqual, int_lit(3));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1)));
    }

    #[test]
    fn test_evaluate_comparison_less_than() {
        let tc = checker();
        let expr = binary(int_lit(3), BinaryOp::LessThan, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1)));
    }

    #[test]
    fn test_evaluate_unary_negate() {
        let tc = checker();
        let expr = unary(UnaryOp::Negate, int_lit(42));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-42)));
    }

    #[test]
    fn test_evaluate_unary_not() {
        let tc = checker();
        let expr = unary(UnaryOp::Not, int_lit(0));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // NOT 0 = -1
    }

    #[test]
    fn test_evaluate_string_concatenation() {
        let tc = checker();
        let expr = binary(string_lit("Hello"), BinaryOp::Add, string_lit(" World"));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::String("Hello World".to_string())));
    }

    #[test]
    fn test_evaluate_string_comparison() {
        let tc = checker();
        let expr = binary(string_lit("abc"), BinaryOp::LessThan, string_lit("abd"));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1)));
    }

    #[test]
    fn test_evaluate_float_addition() {
        let tc = checker();
        let expr = binary(float_lit(1.5), BinaryOp::Add, float_lit(2.5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Float(4.0)));
    }

    #[test]
    fn test_evaluate_mixed_int_float() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Add, float_lit(0.5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Float(10.5)));
    }

    #[test]
    fn test_evaluate_grouped_expression() {
        let tc = checker();
        let inner = int_lit(42);
        let expr = TypedExpr {
            kind: TypedExprKind::Grouped(Box::new(inner)),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        };
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_evaluate_variable_is_none() {
        let tc = checker();
        let expr = TypedExpr {
            kind: TypedExprKind::Variable("unknown_var".to_string()),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        };
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, None); // Variables are not constants
    }

    #[test]
    fn test_evaluate_function_call_is_none() {
        let tc = checker();
        let expr = TypedExpr {
            kind: TypedExprKind::FunctionCall {
                name: "SIN".to_string(),
                args: vec![float_lit(0.5)],
            },
            basic_type: BasicType::Double,
            span: Span::new(0, 1),
        };
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, None); // Function calls are not constant
    }

    #[test]
    fn test_evaluate_division_by_zero() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Divide, int_lit(0));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, None); // Division by zero should return None
    }

    #[test]
    fn test_evaluate_eqv_operator() {
        let tc = checker();
        // EQV: A EQV B = NOT(A XOR B)
        let expr = binary(int_lit(-1), BinaryOp::Eqv, int_lit(-1));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // TRUE EQV TRUE = TRUE
    }

    #[test]
    fn test_evaluate_imp_operator() {
        let tc = checker();
        // IMP: A IMP B = (NOT A) OR B
        let expr = binary(int_lit(0), BinaryOp::Imp, int_lit(0));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // FALSE IMP FALSE = TRUE
    }

    // ========================================================================
    // Tests for Metacommand Condition Evaluator
    // ========================================================================

    /// Creates a checker with platform constants registered.
    fn checker_with_platform_constants() -> TypeChecker<'static> {
        use crate::semantic::symbols::{ConstValue, Symbol, SymbolKind};

        let mut symbols = crate::semantic::symbols::SymbolTable::new();

        // Register platform constants manually for testing
        let define_const =
            |symbols: &mut crate::semantic::symbols::SymbolTable, name: &str, value: i64| {
                let sym = Symbol {
                    name: name.to_string(),
                    kind: SymbolKind::Constant {
                        value: ConstValue::Integer(value),
                    },
                    basic_type: crate::semantic::types::BasicType::Long,
                    span: crate::ast::Span::new(0, 0),
                    is_mutable: false,
                };
                let _ = symbols.define_symbol(sym);
            };

        // Set up a test scenario: LINUX = TRUE, WIN = FALSE, 64BIT = TRUE
        define_const(&mut symbols, "LINUX", -1);
        define_const(&mut symbols, "WIN", 0);
        define_const(&mut symbols, "WINDOWS", 0);
        define_const(&mut symbols, "MAC", 0);
        define_const(&mut symbols, "64BIT", -1);
        define_const(&mut symbols, "32BIT", 0);
        define_const(&mut symbols, "_TRUE", -1);
        define_const(&mut symbols, "_FALSE", 0);

        let symbols_box = Box::new(symbols);
        let symbols_ref: &'static mut _ = Box::leak(symbols_box);
        TypeChecker::new(symbols_ref)
    }

    #[test]
    fn test_meta_condition_simple_true() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("LINUX"));
        assert!(tc.evaluate_meta_condition("64BIT"));
        assert!(tc.evaluate_meta_condition("_TRUE"));
    }

    #[test]
    fn test_meta_condition_simple_false() {
        let tc = checker_with_platform_constants();
        assert!(!tc.evaluate_meta_condition("WIN"));
        assert!(!tc.evaluate_meta_condition("MAC"));
        assert!(!tc.evaluate_meta_condition("32BIT"));
        assert!(!tc.evaluate_meta_condition("_FALSE"));
    }

    #[test]
    fn test_meta_condition_not_operator() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("NOT WIN"));
        assert!(tc.evaluate_meta_condition("NOT MAC"));
        assert!(!tc.evaluate_meta_condition("NOT LINUX"));
        assert!(!tc.evaluate_meta_condition("NOT 64BIT"));
    }

    #[test]
    fn test_meta_condition_and_operator() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("LINUX AND 64BIT"));
        assert!(!tc.evaluate_meta_condition("LINUX AND WIN"));
        assert!(!tc.evaluate_meta_condition("WIN AND MAC"));
    }

    #[test]
    fn test_meta_condition_or_operator() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("LINUX OR WIN"));
        assert!(tc.evaluate_meta_condition("WIN OR MAC OR LINUX"));
        assert!(!tc.evaluate_meta_condition("WIN OR MAC"));
    }

    #[test]
    fn test_meta_condition_comparison() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("LINUX = -1"));
        assert!(tc.evaluate_meta_condition("WIN = 0"));
        assert!(tc.evaluate_meta_condition("WIN <> -1"));
        assert!(!tc.evaluate_meta_condition("LINUX = 0"));
    }

    #[test]
    fn test_meta_condition_parentheses() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("(LINUX OR WIN) AND 64BIT"));
        assert!(!tc.evaluate_meta_condition("(WIN OR MAC) AND 64BIT"));
        assert!(tc.evaluate_meta_condition("LINUX AND (64BIT OR 32BIT)"));
    }

    #[test]
    fn test_meta_condition_complex() {
        let tc = checker_with_platform_constants();
        // Complex expression: 64-bit Linux or any Mac
        assert!(tc.evaluate_meta_condition("(LINUX AND 64BIT) OR MAC"));
        // Complex expression: NOT Windows and 64-bit
        assert!(tc.evaluate_meta_condition("NOT WIN AND 64BIT"));
    }

    #[test]
    fn test_meta_condition_unknown_identifier() {
        let tc = checker_with_platform_constants();
        // Unknown identifiers should be treated as 0 (FALSE)
        assert!(!tc.evaluate_meta_condition("UNKNOWN_CONSTANT"));
        assert!(tc.evaluate_meta_condition("LINUX OR UNKNOWN"));
    }

    #[test]
    fn test_meta_condition_case_insensitive() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("linux"));
        assert!(tc.evaluate_meta_condition("Linux"));
        assert!(tc.evaluate_meta_condition("LINUX"));
        assert!(tc.evaluate_meta_condition("not win"));
    }

    #[test]
    fn test_meta_condition_whitespace() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("  LINUX  "));
        assert!(tc.evaluate_meta_condition("LINUX  AND  64BIT"));
        assert!(tc.evaluate_meta_condition("  NOT  WIN  "));
    }

    #[test]
    fn test_meta_condition_numeric_literal() {
        let tc = checker_with_platform_constants();
        assert!(tc.evaluate_meta_condition("-1"));
        assert!(!tc.evaluate_meta_condition("0"));
        assert!(tc.evaluate_meta_condition("LINUX = -1"));
        assert!(tc.evaluate_meta_condition("64BIT <> 0"));
    }
}
