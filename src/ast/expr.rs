//! Expression AST nodes.
//!
//! Expressions are constructs that evaluate to a value. In BASIC, expressions
//! include literals, variables, arithmetic operations, comparisons, and function calls.
//!
//! # Expression Precedence
//!
//! From highest to lowest (as implemented in the parser):
//!
//! 1. Primary: literals, identifiers, parenthesized expressions
//! 2. Unary: `-x`, `NOT x`
//! 3. Exponentiation: `^` (right-associative)
//! 4. Multiplicative: `*`, `/`, `\`, `MOD`
//! 5. Additive: `+`, `-`
//! 6. Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=`
//! 7. Logical NOT: `NOT`
//! 8. Logical AND: `AND`
//! 9. Logical OR/XOR: `OR`, `XOR`

use super::Span;

/// An expression with its source location.
#[derive(Debug, Clone)]
pub struct Expr {
    /// The kind of expression.
    pub kind: ExprKind,
    /// Source location of this expression.
    pub span: Span,
}

impl Expr {
    /// Creates a new expression with the given kind and span.
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// The different kinds of expressions in BASIC.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Integer literal: `42`, `-17`, `&HFF`
    IntegerLiteral(i64),

    /// Floating-point literal: `3.14`, `1.5E10`
    FloatLiteral(f64),

    /// String literal: `"Hello, World!"`
    StringLiteral(String),

    /// Variable or constant reference: `x`, `name$`, `PI`
    Identifier(String),

    /// Binary operation: `left op right`
    ///
    /// Examples: `1 + 2`, `x * y`, `a > b`, `flag AND mask`
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },

    /// Unary operation: `op operand`
    ///
    /// Examples: `-x`, `NOT flag`
    Unary { op: UnaryOp, operand: Box<Expr> },

    /// Parenthesized expression: `(expr)`
    ///
    /// Preserves the parentheses in the AST for accurate source reconstruction.
    Grouped(Box<Expr>),

    /// Function call: `FunctionName(arg1, arg2, ...)`
    ///
    /// Also used for array indexing since syntax is identical: `array(index)`
    FunctionCall { name: String, args: Vec<Expr> },
}

/// Binary operators.
///
/// Organized roughly by precedence (though precedence is handled by the parser).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic (higher precedence)
    /// `^` - Exponentiation (right-associative)
    Power,
    /// `*` - Multiplication
    Multiply,
    /// `/` - Floating-point division
    Divide,
    /// `\` - Integer division
    IntDivide,
    /// `MOD` - Modulo (remainder)
    Modulo,
    /// `+` - Addition (also string concatenation)
    Add,
    /// `-` - Subtraction
    Subtract,

    // Comparison
    /// `=` - Equality (also assignment in statement context)
    Equal,
    /// `<>` - Not equal
    NotEqual,
    /// `<` - Less than
    LessThan,
    /// `<=` - Less than or equal
    LessEqual,
    /// `>` - Greater than
    GreaterThan,
    /// `>=` - Greater than or equal
    GreaterEqual,

    // Logical (lower precedence)
    /// `AND` - Logical/bitwise AND
    And,
    /// `OR` - Logical/bitwise OR
    Or,
    /// `XOR` - Logical/bitwise exclusive OR
    Xor,
    /// `EQV` - Logical equivalence
    Eqv,
    /// `IMP` - Logical implication
    Imp,
}

impl BinaryOp {
    /// Returns true if this operator is right-associative.
    ///
    /// In BASIC, only exponentiation (`^`) is right-associative:
    /// `2 ^ 3 ^ 4` is parsed as `2 ^ (3 ^ 4)`, not `(2 ^ 3) ^ 4`.
    pub fn is_right_associative(&self) -> bool {
        matches!(self, BinaryOp::Power)
    }

    /// Returns a string representation of the operator for display.
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Power => "^",
            BinaryOp::Multiply => "*",
            BinaryOp::Divide => "/",
            BinaryOp::IntDivide => "\\",
            BinaryOp::Modulo => "MOD",
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Equal => "=",
            BinaryOp::NotEqual => "<>",
            BinaryOp::LessThan => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::GreaterThan => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::And => "AND",
            BinaryOp::Or => "OR",
            BinaryOp::Xor => "XOR",
            BinaryOp::Eqv => "EQV",
            BinaryOp::Imp => "IMP",
        }
    }
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `-` - Numeric negation
    Negate,
    /// `NOT` - Logical/bitwise NOT
    Not,
}

impl UnaryOp {
    /// Returns a string representation of the operator for display.
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "NOT",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_op_right_associative() {
        assert!(BinaryOp::Power.is_right_associative());
        assert!(!BinaryOp::Add.is_right_associative());
        assert!(!BinaryOp::Multiply.is_right_associative());
    }

    #[test]
    fn test_binary_op_as_str() {
        assert_eq!(BinaryOp::Add.as_str(), "+");
        assert_eq!(BinaryOp::Modulo.as_str(), "MOD");
        assert_eq!(BinaryOp::NotEqual.as_str(), "<>");
    }

    #[test]
    fn test_create_integer_literal() {
        let expr = Expr::new(ExprKind::IntegerLiteral(42), Span::new(0, 2));
        assert!(matches!(expr.kind, ExprKind::IntegerLiteral(42)));
    }

    #[test]
    fn test_create_binary_expr() {
        let left = Box::new(Expr::new(ExprKind::IntegerLiteral(1), Span::new(0, 1)));
        let right = Box::new(Expr::new(ExprKind::IntegerLiteral(2), Span::new(4, 5)));
        let expr = Expr::new(
            ExprKind::Binary {
                left,
                op: BinaryOp::Add,
                right,
            },
            Span::new(0, 5),
        );
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        ));
    }
}
