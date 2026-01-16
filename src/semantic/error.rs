//! Semantic analysis error types.
//!
//! These errors represent problems found during type checking and symbol resolution
//! that aren't syntax errors. Each error includes source location information
//! for accurate diagnostics.

use crate::ast::Span;
use thiserror::Error;

/// A semantic analysis error with location and description.
#[derive(Debug, Error, Clone)]
pub enum SemanticError {
    // === Variable/Symbol Errors ===
    /// Reference to a variable that hasn't been declared.
    #[error("undefined variable `{name}`")]
    UndefinedVariable { name: String, span: Span },

    /// Reference to a label that doesn't exist.
    #[error("undefined label `{name}`")]
    UndefinedLabel { name: String, span: Span },

    /// Call to a procedure that hasn't been defined.
    #[error("undefined procedure `{name}`")]
    UndefinedProcedure { name: String, span: Span },

    /// Variable declared more than once in the same scope.
    #[error("variable `{name}` already defined")]
    DuplicateVariable {
        name: String,
        original_span: Span,
        duplicate_span: Span,
    },

    /// Label defined more than once.
    #[error("label `{name}` already defined")]
    DuplicateLabel {
        name: String,
        original_span: Span,
        duplicate_span: Span,
    },

    /// Procedure defined more than once.
    #[error("procedure `{name}` already defined")]
    DuplicateProcedure {
        name: String,
        original_span: Span,
        duplicate_span: Span,
    },

    // === Type Errors ===
    /// Expected one type but found another.
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    /// Cannot convert between these types.
    #[error("cannot convert {from} to {to}")]
    InvalidConversion {
        from: String,
        to: String,
        span: Span,
    },

    /// Binary operator applied to incompatible types.
    #[error("operator `{op}` cannot be applied to types {left_type} and {right_type}")]
    InvalidBinaryOp {
        op: String,
        left_type: String,
        right_type: String,
        span: Span,
    },

    /// Unary operator applied to incompatible type.
    #[error("operator `{op}` cannot be applied to type {operand_type}")]
    InvalidUnaryOp {
        op: String,
        operand_type: String,
        span: Span,
    },

    // === Procedure/Function Errors ===
    /// Function called with wrong number of arguments.
    #[error("function `{name}` called with {found} arguments, expected {expected}")]
    ArgumentCountMismatch {
        name: String,
        expected: usize,
        found: usize,
        span: Span,
    },

    /// Argument type doesn't match parameter type.
    #[error("argument {position} type mismatch: expected {expected}, found {found}")]
    ArgumentTypeMismatch {
        position: usize,
        expected: String,
        found: String,
        span: Span,
    },

    /// Trying to call something that isn't a procedure.
    #[error("cannot call `{name}` - it is not a procedure")]
    NotCallable { name: String, span: Span },

    /// SUB used where a value is expected (SUBs don't return values).
    #[error("SUB `{name}` does not return a value")]
    SubUsedAsFunction { name: String, span: Span },

    // === Control Flow Errors ===
    /// EXIT statement outside its corresponding loop/procedure.
    #[error("EXIT {exit_type} outside of {exit_type}")]
    ExitOutsideLoop { exit_type: String, span: Span },

    /// RETURN statement outside of GOSUB or FUNCTION.
    #[error("RETURN outside of GOSUB or FUNCTION")]
    ReturnOutsideContext { span: Span },

    /// NEXT variable doesn't match FOR variable.
    #[error("FOR loop variable `{expected}` does not match NEXT variable `{found}`")]
    ForNextMismatch {
        expected: String,
        found: String,
        for_span: Span,
        next_span: Span,
    },

    // === Array Errors ===
    /// Trying to index something that isn't an array.
    #[error("`{name}` is not an array")]
    NotAnArray { name: String, span: Span },

    /// Array indexed with wrong number of dimensions.
    #[error("array `{name}` indexed with {found} dimensions, expected {expected}")]
    ArrayDimensionMismatch {
        name: String,
        expected: usize,
        found: usize,
        span: Span,
    },

    /// Array index must be a numeric type.
    #[error("array index must be numeric, found {found}")]
    NonNumericIndex { found: String, span: Span },

    // === Constant Errors ===
    /// Attempting to assign to a CONST.
    #[error("cannot assign to constant `{name}`")]
    AssignmentToConst { name: String, span: Span },

    /// CONST initializer isn't a compile-time constant.
    #[error("CONST value must be a compile-time constant")]
    NonConstantExpression { span: Span },

    // === SHARED Errors ===
    /// SHARED used outside a procedure.
    #[error("SHARED can only be used inside SUB or FUNCTION")]
    SharedOutsideProcedure { span: Span },

    /// SHARED references a variable not defined at module level.
    #[error("SHARED variable `{name}` not defined at module level")]
    SharedVariableNotFound { name: String, span: Span },
}

impl SemanticError {
    /// Returns the primary span of this error.
    pub fn span(&self) -> Span {
        match self {
            SemanticError::UndefinedVariable { span, .. } => *span,
            SemanticError::UndefinedLabel { span, .. } => *span,
            SemanticError::UndefinedProcedure { span, .. } => *span,
            SemanticError::DuplicateVariable { duplicate_span, .. } => *duplicate_span,
            SemanticError::DuplicateLabel { duplicate_span, .. } => *duplicate_span,
            SemanticError::DuplicateProcedure { duplicate_span, .. } => *duplicate_span,
            SemanticError::TypeMismatch { span, .. } => *span,
            SemanticError::InvalidConversion { span, .. } => *span,
            SemanticError::InvalidBinaryOp { span, .. } => *span,
            SemanticError::InvalidUnaryOp { span, .. } => *span,
            SemanticError::ArgumentCountMismatch { span, .. } => *span,
            SemanticError::ArgumentTypeMismatch { span, .. } => *span,
            SemanticError::NotCallable { span, .. } => *span,
            SemanticError::SubUsedAsFunction { span, .. } => *span,
            SemanticError::ExitOutsideLoop { span, .. } => *span,
            SemanticError::ReturnOutsideContext { span } => *span,
            SemanticError::ForNextMismatch { next_span, .. } => *next_span,
            SemanticError::NotAnArray { span, .. } => *span,
            SemanticError::ArrayDimensionMismatch { span, .. } => *span,
            SemanticError::NonNumericIndex { span, .. } => *span,
            SemanticError::AssignmentToConst { span, .. } => *span,
            SemanticError::NonConstantExpression { span } => *span,
            SemanticError::SharedOutsideProcedure { span } => *span,
            SemanticError::SharedVariableNotFound { span, .. } => *span,
        }
    }

    /// Creates an undefined variable error.
    pub fn undefined_variable(name: impl Into<String>, span: Span) -> Self {
        SemanticError::UndefinedVariable {
            name: name.into(),
            span,
        }
    }

    /// Creates a type mismatch error.
    pub fn type_mismatch(
        expected: impl Into<String>,
        found: impl Into<String>,
        span: Span,
    ) -> Self {
        SemanticError::TypeMismatch {
            expected: expected.into(),
            found: found.into(),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_span() {
        let err = SemanticError::undefined_variable("x", Span::new(10, 11));
        assert_eq!(err.span(), Span::new(10, 11));
    }

    #[test]
    fn test_type_mismatch_message() {
        let err = SemanticError::type_mismatch("INTEGER", "STRING", Span::new(0, 5));
        assert!(err.to_string().contains("INTEGER"));
        assert!(err.to_string().contains("STRING"));
    }

    #[test]
    fn test_duplicate_variable_spans() {
        let err = SemanticError::DuplicateVariable {
            name: "x".to_string(),
            original_span: Span::new(0, 5),
            duplicate_span: Span::new(10, 15),
        };
        // Primary span should be the duplicate
        assert_eq!(err.span(), Span::new(10, 15));
    }
}
