//! Semantic analysis error types for QB64Fresh.
//!
//! This module defines errors that occur during type checking and symbol resolution -
//! problems that are valid syntax but semantically incorrect. Each error includes
//! source location information for accurate diagnostics.
//!
//! ## Error Categories
//!
//! Semantic errors are organized into categories:
//!
//! - **Symbol errors** - Undefined or duplicate variables, labels, procedures
//! - **Type errors** - Type mismatches, invalid conversions, operator type errors
//! - **Procedure errors** - Wrong argument counts, argument type mismatches
//! - **Control flow errors** - EXIT/CONTINUE outside loops, mismatched FOR/NEXT
//! - **Array errors** - Wrong dimensions, non-numeric indices
//! - **Constant errors** - Assigning to constants, non-constant initializers
//! - **Scope errors** - SHARED outside procedures, invalid OPTION BASE
//!
//! ## Example
//!
//! ```ignore
//! use qb64fresh::semantic::error::SemanticError;
//!
//! let err = SemanticError::type_mismatch("INTEGER", "STRING", span);
//! println!("{}", err); // "type mismatch: expected INTEGER, found STRING"
//! ```

use crate::ast::Span;
use thiserror::Error;

/// Errors that occur during semantic analysis of QB64 BASIC programs.
///
/// These errors represent problems found after parsing - the code is syntactically
/// valid but violates semantic rules (type safety, scope rules, etc.). Each variant
/// includes location information via [`Span`] for accurate error reporting.
///
/// # Example
///
/// ```ignore
/// use qb64fresh::semantic::error::SemanticError;
/// use qb64fresh::ast::Span;
///
/// let err = SemanticError::undefined_variable("count", Span::new(10, 15, 1));
/// assert_eq!(err.span(), Span::new(10, 15, 1));
/// ```
#[derive(Debug, Error, Clone)]
pub enum SemanticError {
    // ========================================================================
    // Variable/Symbol Errors
    // ========================================================================
    /// Reference to a variable that hasn't been declared.
    ///
    /// In QB64, variables can be implicitly declared by assignment, but referencing
    /// a variable before any assignment is an error. Check for typos in the variable
    /// name or add a DIM statement.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// PRINT counter   ' Error: undefined variable `counter`
    /// ```
    #[error("undefined variable `{name}`")]
    UndefinedVariable {
        /// The name of the undefined variable.
        name: String,
        /// Source location where the variable was referenced.
        span: Span,
    },

    /// Reference to a label that doesn't exist.
    ///
    /// GOTO and GOSUB require the target label to be defined somewhere in the
    /// program. Check for typos in the label name.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// GOTO finish     ' Error: undefined label `finish`
    /// ```
    #[error("undefined label `{name}`")]
    UndefinedLabel {
        /// The name of the undefined label.
        name: String,
        /// Source location where the label was referenced.
        span: Span,
    },

    /// Call to a SUB or FUNCTION that hasn't been defined.
    ///
    /// Ensure the procedure is defined (with SUB...END SUB or FUNCTION...END FUNCTION)
    /// or declared (with DECLARE SUB/FUNCTION for external procedures).
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// CALL ProcessData   ' Error: undefined procedure `ProcessData`
    /// ```
    #[error("undefined procedure `{name}`")]
    UndefinedProcedure {
        /// The name of the undefined SUB or FUNCTION.
        name: String,
        /// Source location where the procedure was called.
        span: Span,
    },

    /// Variable declared more than once in the same scope.
    ///
    /// Each variable name can only be declared once per scope. Remove the
    /// duplicate DIM statement or use a different variable name.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM x AS INTEGER
    /// DIM x AS STRING    ' Error: variable `x` already defined
    /// ```
    #[error("variable `{name}` already defined")]
    DuplicateVariable {
        /// Name of the duplicate variable.
        name: String,
        /// Location of the original declaration.
        original_span: Span,
        /// Location of the duplicate declaration.
        duplicate_span: Span,
    },

    /// Label defined more than once.
    ///
    /// Each label name must be unique within its scope.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// start:
    ///     PRINT "first"
    /// start:             ' Error: label `start` already defined
    ///     PRINT "second"
    /// ```
    #[error("label `{name}` already defined")]
    DuplicateLabel {
        /// Name of the duplicate label.
        name: String,
        /// Location of the original declaration.
        original_span: Span,
        /// Location of the duplicate declaration.
        duplicate_span: Span,
    },

    /// SUB or FUNCTION defined more than once.
    ///
    /// Each procedure name must be unique. Rename one of the procedures.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SUB Init
    /// END SUB
    ///
    /// SUB Init           ' Error: procedure `Init` already defined
    /// END SUB
    /// ```
    #[error("procedure `{name}` already defined")]
    DuplicateProcedure {
        /// Name of the duplicate procedure.
        name: String,
        /// Location of the original declaration.
        original_span: Span,
        /// Location of the duplicate declaration.
        duplicate_span: Span,
    },

    /// User-defined TYPE declared more than once.
    ///
    /// Each TYPE name must be unique. Rename one of the types.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// TYPE Point
    ///     x AS INTEGER
    /// END TYPE
    ///
    /// TYPE Point         ' Error: TYPE `Point` already defined
    ///     y AS INTEGER
    /// END TYPE
    /// ```
    #[error("TYPE `{name}` already defined")]
    DuplicateType {
        /// Name of the duplicate TYPE.
        name: String,
        /// Location of the original declaration.
        original_span: Span,
        /// Location of the duplicate declaration.
        duplicate_span: Span,
    },

    // ========================================================================
    // Type Errors
    // ========================================================================
    /// Expected one type but found another.
    ///
    /// This occurs when an expression's type doesn't match what's required.
    /// Common cases: assigning STRING to INTEGER, passing wrong argument types.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM count AS INTEGER
    /// count = "hello"    ' Error: type mismatch: expected INTEGER, found STRING
    /// ```
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        /// Expected type name.
        expected: String,
        /// Found type name.
        found: String,
        /// Location of the type mismatch.
        span: Span,
    },

    /// Cannot convert between these types.
    ///
    /// Some type conversions are not allowed, such as converting a STRING
    /// to a numeric type without using VAL().
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM n AS INTEGER
    /// n = CINT("123")    ' Error: cannot convert STRING to INTEGER (use VAL first)
    /// ```
    #[error("cannot convert {from} to {to}")]
    InvalidConversion {
        /// Source type name.
        from: String,
        /// Target type name.
        to: String,
        /// Location of the conversion.
        span: Span,
    },

    /// Binary operator applied to incompatible types.
    ///
    /// Operators like +, -, *, / require compatible operand types.
    /// String concatenation uses + but both operands must be strings.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// PRINT "Count: " - 5   ' Error: operator `-` cannot be applied to STRING and INTEGER
    /// ```
    #[error("operator `{op}` cannot be applied to types {left_type} and {right_type}")]
    InvalidBinaryOp {
        /// Operator symbol (e.g., "+", "-", "*").
        op: String,
        /// Type of the left operand.
        left_type: String,
        /// Type of the right operand.
        right_type: String,
        /// Location of the operator.
        span: Span,
    },

    /// Unary operator applied to incompatible type.
    ///
    /// Unary operators like - (negation) and NOT require specific operand types.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// PRINT -"hello"     ' Error: operator `-` cannot be applied to STRING
    /// ```
    #[error("operator `{op}` cannot be applied to type {operand_type}")]
    InvalidUnaryOp {
        /// Operator symbol (e.g., "-", "NOT").
        op: String,
        /// Type of the operand.
        operand_type: String,
        /// Location of the operator.
        span: Span,
    },

    // ========================================================================
    // Procedure/Function Errors
    // ========================================================================
    /// Function or SUB called with wrong number of arguments.
    ///
    /// Check the procedure definition for the correct number of parameters.
    /// Some built-in functions have optional parameters.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// PRINT MID$("hello")   ' Error: function `MID$` called with 1 arguments, expected 2 to 3
    /// ```
    #[error("function `{name}` called with {found} arguments, expected {expected_min}{}", if *.expected_min == *.expected_max { "".to_string() } else { format!(" to {}", .expected_max) })]
    ArgumentCountMismatch {
        /// Name of the function or SUB.
        name: String,
        /// Minimum number of arguments required.
        expected_min: usize,
        /// Maximum number of arguments allowed.
        expected_max: usize,
        /// Number of arguments provided.
        found: usize,
        /// Location of the function call.
        span: Span,
    },

    /// Argument type doesn't match parameter type.
    ///
    /// The type of an argument must be compatible with the parameter type
    /// declared in the procedure definition.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SUB PrintNum(n AS INTEGER)
    /// END SUB
    ///
    /// PrintNum "hello"   ' Error: argument 1 type mismatch: expected INTEGER, found STRING
    /// ```
    #[error("argument {position} type mismatch: expected {expected}, found {found}")]
    ArgumentTypeMismatch {
        /// 1-based argument position.
        position: usize,
        /// Expected type name.
        expected: String,
        /// Found type name.
        found: String,
        /// Location of the argument.
        span: Span,
    },

    /// _IIF true and false parts have incompatible types.
    ///
    /// In _IIF(condition, true_value, false_value), both values must be
    /// either both strings or both numeric types. Mixing string and numeric
    /// types is not allowed.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// result = _IIF(x > 0, "positive", 123)  ' Error: types must match
    /// ```
    #[error("_IIF true and false parts have incompatible types: {true_type} vs {false_type}")]
    IifTypeMismatch {
        /// Type of the true branch value.
        true_type: String,
        /// Type of the false branch value.
        false_type: String,
        /// Location of the _IIF expression.
        span: Span,
    },

    /// Trying to call something that isn't a SUB or FUNCTION.
    ///
    /// Only procedures (SUB/FUNCTION) can be called. Variables and constants
    /// cannot be called.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM myVar AS INTEGER
    /// CALL myVar         ' Error: cannot call `myVar` - it is not a procedure
    /// ```
    #[error("cannot call `{name}` - it is not a procedure")]
    NotCallable {
        /// Name of the non-callable item.
        name: String,
        /// Location of the call attempt.
        span: Span,
    },

    /// SUB used in an expression where a return value is expected.
    ///
    /// SUBs don't return values - use a FUNCTION instead, or call the SUB
    /// as a statement rather than in an expression.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SUB DoWork
    /// END SUB
    ///
    /// x = DoWork         ' Error: SUB `DoWork` does not return a value
    /// ```
    #[error("SUB `{name}` does not return a value")]
    SubUsedAsFunction {
        /// Name of the SUB being used incorrectly.
        name: String,
        /// Location where the SUB is used as a function.
        span: Span,
    },

    // ========================================================================
    // Control Flow Errors
    // ========================================================================
    /// EXIT statement used outside its corresponding construct.
    ///
    /// EXIT FOR must be inside a FOR loop, EXIT DO inside a DO loop, etc.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// EXIT FOR           ' Error: EXIT FOR outside of FOR
    /// ```
    #[error("EXIT {exit_type} outside of {exit_type}")]
    ExitOutsideLoop {
        /// Type of EXIT statement (e.g., "FOR", "DO", "SUB").
        exit_type: String,
        /// Location of the EXIT statement.
        span: Span,
    },

    /// CONTINUE statement used outside its corresponding loop.
    ///
    /// CONTINUE FOR must be inside a FOR loop, CONTINUE DO inside a DO loop.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// CONTINUE FOR       ' Error: CONTINUE FOR outside of FOR loop
    /// ```
    #[error("CONTINUE {loop_type} outside of {loop_type} loop")]
    ContinueOutsideLoop {
        /// Type of CONTINUE statement (e.g., "FOR", "DO").
        loop_type: String,
        /// Location of the CONTINUE statement.
        span: Span,
    },

    /// RETURN statement outside of a GOSUB routine or FUNCTION.
    ///
    /// RETURN is used to return from GOSUB or to return a value from FUNCTION.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// RETURN             ' Error: RETURN outside of GOSUB or FUNCTION
    /// ```
    #[error("RETURN outside of GOSUB or FUNCTION")]
    ReturnOutsideContext {
        /// Location of the RETURN statement.
        span: Span,
    },

    /// NEXT variable doesn't match the FOR loop variable.
    ///
    /// When NEXT specifies a variable, it must match the corresponding FOR.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// FOR i = 1 TO 10
    ///     PRINT i
    /// NEXT j             ' Error: FOR loop variable `i` does not match NEXT variable `j`
    /// ```
    #[error("FOR loop variable `{expected}` does not match NEXT variable `{found}`")]
    ForNextMismatch {
        /// Variable name from the FOR statement.
        expected: String,
        /// Variable name from the NEXT statement.
        found: String,
        /// Location of the FOR statement.
        for_span: Span,
        /// Location of the NEXT statement.
        next_span: Span,
    },

    // ========================================================================
    // Array Errors
    // ========================================================================
    /// Trying to index something that isn't an array.
    ///
    /// Array subscript syntax can only be used on arrays. Make sure the
    /// variable was declared as an array with DIM.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM x AS INTEGER
    /// PRINT x(1)         ' Error: `x` is not an array
    /// ```
    #[error("`{name}` is not an array")]
    NotAnArray {
        /// Name of the variable that was indexed.
        name: String,
        /// Location of the array access.
        span: Span,
    },

    /// Array accessed with wrong number of subscripts.
    ///
    /// The number of indices must match the array's dimensions.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM arr(10, 10) AS INTEGER
    /// PRINT arr(5)       ' Error: array `arr` indexed with 1 dimensions, expected 2
    /// ```
    #[error("array `{name}` indexed with {found} dimensions, expected {expected}")]
    ArrayDimensionMismatch {
        /// Name of the array variable.
        name: String,
        /// Expected number of dimensions.
        expected: usize,
        /// Number of dimensions provided.
        found: usize,
        /// Location of the array access.
        span: Span,
    },

    /// Array index is not a numeric type.
    ///
    /// Array subscripts must be numeric (INTEGER, LONG, etc.), not STRING.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM arr(10) AS INTEGER
    /// PRINT arr("five")  ' Error: array index must be numeric, found STRING
    /// ```
    #[error("array index must be numeric, found {found}")]
    NonNumericIndex {
        /// Type of the index expression.
        found: String,
        /// Location of the array access.
        span: Span,
    },

    // ========================================================================
    // Constant Errors
    // ========================================================================
    /// Attempting to assign a new value to a CONST.
    ///
    /// Constants cannot be modified after declaration. Use a variable instead
    /// if the value needs to change.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// CONST PI = 3.14159
    /// PI = 3.0           ' Error: cannot assign to constant `PI`
    /// ```
    #[error("cannot assign to constant `{name}`")]
    AssignmentToConst {
        /// Name of the constant being assigned to.
        name: String,
        /// Location of the assignment.
        span: Span,
    },

    /// CONST initializer is not a compile-time constant expression.
    ///
    /// CONST values must be known at compile time - literals, other constants,
    /// or simple constant expressions. Variables and function calls are not allowed.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DIM x AS INTEGER
    /// CONST Y = x + 1    ' Error: CONST value must be a compile-time constant
    /// ```
    #[error("CONST value must be a compile-time constant")]
    NonConstantExpression {
        /// Location of the non-constant expression.
        span: Span,
    },

    // ========================================================================
    // SHARED Errors
    // ========================================================================
    /// SHARED statement used outside a SUB or FUNCTION.
    ///
    /// SHARED is used inside procedures to access module-level variables.
    /// At module level, variables are already accessible.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SHARED globalVar   ' Error: SHARED can only be used inside SUB or FUNCTION
    /// ```
    #[error("SHARED can only be used inside SUB or FUNCTION")]
    SharedOutsideProcedure {
        /// Location of the SHARED statement.
        span: Span,
    },

    /// SHARED references a variable that doesn't exist at module level.
    ///
    /// The variable must be declared or used at module level before it can
    /// be accessed via SHARED.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SUB Test
    ///     SHARED noSuchVar   ' Error: SHARED variable `noSuchVar` not defined at module level
    /// END SUB
    /// ```
    #[error("SHARED variable `{name}` not defined at module level")]
    SharedVariableNotFound {
        /// Name of the variable referenced in SHARED.
        name: String,
        /// Location of the SHARED statement.
        span: Span,
    },

    // ========================================================================
    // OPTION Errors
    // ========================================================================
    /// OPTION BASE value is not 0 or 1.
    ///
    /// OPTION BASE sets the default lower bound for arrays. Only 0 and 1
    /// are valid values.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// OPTION BASE 5      ' Error: OPTION BASE must be 0 or 1, found 5
    /// ```
    #[error("OPTION BASE must be 0 or 1, found {value}")]
    InvalidOptionBase {
        /// Invalid BASE value provided.
        value: i64,
        /// Location of the OPTION BASE statement.
        span: Span,
    },

    // ========================================================================
    // Unimplemented Feature Errors
    // ========================================================================
    /// Command is not implemented (matching QB64pe behavior).
    ///
    /// Some legacy BASIC commands are intentionally not implemented because they
    /// have no meaningful function on modern systems. QB64pe also throws
    /// "Command not implemented" for these functions.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// PRINT FRE(0)    ' Error: Command not implemented
    /// ```
    #[error("Command not implemented: `{name}`")]
    CommandNotImplemented {
        /// Name of the unimplemented command.
        name: String,
        /// Location of the command.
        span: Span,
    },
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
            SemanticError::DuplicateType { duplicate_span, .. } => *duplicate_span,
            SemanticError::TypeMismatch { span, .. } => *span,
            SemanticError::InvalidConversion { span, .. } => *span,
            SemanticError::InvalidBinaryOp { span, .. } => *span,
            SemanticError::InvalidUnaryOp { span, .. } => *span,
            SemanticError::ArgumentCountMismatch { span, .. } => *span,
            SemanticError::ArgumentTypeMismatch { span, .. } => *span,
            SemanticError::IifTypeMismatch { span, .. } => *span,
            SemanticError::NotCallable { span, .. } => *span,
            SemanticError::SubUsedAsFunction { span, .. } => *span,
            SemanticError::ExitOutsideLoop { span, .. } => *span,
            SemanticError::ContinueOutsideLoop { span, .. } => *span,
            SemanticError::ReturnOutsideContext { span } => *span,
            SemanticError::ForNextMismatch { next_span, .. } => *next_span,
            SemanticError::NotAnArray { span, .. } => *span,
            SemanticError::ArrayDimensionMismatch { span, .. } => *span,
            SemanticError::NonNumericIndex { span, .. } => *span,
            SemanticError::AssignmentToConst { span, .. } => *span,
            SemanticError::NonConstantExpression { span } => *span,
            SemanticError::SharedOutsideProcedure { span } => *span,
            SemanticError::SharedVariableNotFound { span, .. } => *span,
            SemanticError::InvalidOptionBase { span, .. } => *span,
            SemanticError::CommandNotImplemented { span, .. } => *span,
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
        let err = SemanticError::undefined_variable("x", Span::new(10, 11, 1));
        assert_eq!(err.span(), Span::new(10, 11, 1));
    }

    #[test]
    fn test_type_mismatch_message() {
        let err = SemanticError::type_mismatch("INTEGER", "STRING", Span::new(0, 5, 1));
        assert!(err.to_string().contains("INTEGER"));
        assert!(err.to_string().contains("STRING"));
    }

    #[test]
    fn test_duplicate_variable_spans() {
        let err = SemanticError::DuplicateVariable {
            name: "x".to_string(),
            original_span: Span::new(0, 5, 1),
            duplicate_span: Span::new(10, 15, 1),
        };
        // Primary span should be the duplicate
        assert_eq!(err.span(), Span::new(10, 15, 1));
    }
}
