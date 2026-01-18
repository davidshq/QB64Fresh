//! Typed intermediate representation for code generation.
//!
//! This module defines the output of semantic analysis - an AST annotated with
//! type information. Each expression carries its inferred type, and the IR is
//! ready for consumption by the code generator.
//!
//! # Design Principles
//!
//! - **Mirrors the AST structure**: Easy to traverse similarly to the parser output
//! - **Type-annotated**: Every expression knows its type
//! - **Explicit conversions**: Implicit conversions become explicit `Convert` nodes
//! - **Ready for codegen**: No further analysis needed

use crate::ast::{BinaryOp, ContinueType, ExitType, PrintSeparator, Span, UnaryOp};
use crate::semantic::types::BasicType;

/// A type-annotated expression.
///
/// Every expression in the typed IR carries its inferred type and source location.
#[derive(Debug, Clone)]
pub struct TypedExpr {
    /// The expression kind with typed sub-expressions.
    pub kind: TypedExprKind,
    /// The type of this expression's value.
    pub basic_type: BasicType,
    /// Source location for error reporting and debugging.
    pub span: Span,
}

/// The different kinds of typed expressions.
#[derive(Debug, Clone)]
pub enum TypedExprKind {
    /// Integer literal: `42`, `&HFF`
    IntegerLiteral(i64),

    /// Floating-point literal: `3.14`
    FloatLiteral(f64),

    /// String literal: `"Hello"`
    StringLiteral(String),

    /// Variable reference (resolved to a symbol).
    Variable(String),

    /// Binary operation with typed operands.
    Binary {
        left: Box<TypedExpr>,
        op: BinaryOp,
        right: Box<TypedExpr>,
    },

    /// Unary operation with typed operand.
    Unary {
        op: UnaryOp,
        operand: Box<TypedExpr>,
    },

    /// Parenthesized expression (preserved for source fidelity).
    Grouped(Box<TypedExpr>),

    /// Function call with typed arguments.
    FunctionCall { name: String, args: Vec<TypedExpr> },

    /// Array access with typed indices.
    ///
    /// For multi-dimensional arrays, the dimensions field contains the
    /// bounds of each dimension, which is needed for calculating the
    /// linear index in row-major order.
    ArrayAccess {
        name: String,
        indices: Vec<TypedExpr>,
        /// Dimension bounds for index calculation (empty for 1D arrays).
        dimensions: Vec<TypedArrayDimension>,
    },

    /// Explicit type conversion (inserted by type checker for implicit conversions).
    ///
    /// The inner expression's type is converted to `to_type`.
    Convert {
        expr: Box<TypedExpr>,
        to_type: BasicType,
    },

    /// Field access on a user-defined type.
    ///
    /// Example: `person.name`, `record.data.value`
    FieldAccess {
        /// The object expression being accessed.
        object: Box<TypedExpr>,
        /// The field name.
        field: String,
    },
}

impl TypedExpr {
    /// Creates a new typed expression.
    pub fn new(kind: TypedExprKind, basic_type: BasicType, span: Span) -> Self {
        Self {
            kind,
            basic_type,
            span,
        }
    }

    /// Creates an integer literal expression.
    pub fn integer(value: i64, span: Span) -> Self {
        Self::new(TypedExprKind::IntegerLiteral(value), BasicType::Long, span)
    }

    /// Creates a float literal expression.
    pub fn float(value: f64, span: Span) -> Self {
        Self::new(TypedExprKind::FloatLiteral(value), BasicType::Double, span)
    }

    /// Creates a string literal expression.
    pub fn string(value: String, span: Span) -> Self {
        Self::new(TypedExprKind::StringLiteral(value), BasicType::String, span)
    }

    /// Wraps this expression in a conversion node if needed.
    pub fn convert_to(self, target: BasicType) -> Self {
        if self.basic_type == target {
            self
        } else {
            let span = self.span;
            Self::new(
                TypedExprKind::Convert {
                    expr: Box::new(self),
                    to_type: target.clone(),
                },
                target,
                span,
            )
        }
    }
}

/// A type-checked statement.
#[derive(Debug, Clone)]
pub struct TypedStatement {
    /// The statement kind with typed sub-expressions.
    pub kind: TypedStatementKind,
    /// Source location.
    pub span: Span,
}

/// The different kinds of typed statements.
#[derive(Debug, Clone)]
pub enum TypedStatementKind {
    /// Variable assignment with target type.
    Assignment {
        name: String,
        value: TypedExpr,
        /// The type of the target variable (for codegen to emit conversion).
        target_type: BasicType,
    },

    /// Array element assignment with target type.
    ArrayAssignment {
        name: String,
        indices: Vec<TypedExpr>,
        value: TypedExpr,
        /// Dimension info for linear index calculation.
        dimensions: Vec<TypedArrayDimension>,
        /// The element type.
        element_type: BasicType,
    },

    /// PRINT statement with typed items.
    Print {
        items: Vec<TypedPrintItem>,
        newline: bool,
    },

    /// INPUT statement with typed variables.
    Input {
        prompt: Option<String>,
        show_question_mark: bool,
        /// Variables paired with their types.
        variables: Vec<(String, BasicType)>,
    },

    /// LINE INPUT statement.
    LineInput {
        prompt: Option<String>,
        variable: String,
    },

    /// IF/ELSEIF/ELSE statement.
    If {
        condition: TypedExpr,
        then_branch: Vec<TypedStatement>,
        elseif_branches: Vec<(TypedExpr, Vec<TypedStatement>)>,
        else_branch: Option<Vec<TypedStatement>>,
    },

    /// SELECT CASE statement.
    SelectCase {
        test_expr: TypedExpr,
        cases: Vec<TypedCaseClause>,
        case_else: Option<Vec<TypedStatement>>,
    },

    /// FOR/NEXT loop.
    For {
        variable: String,
        var_type: BasicType,
        start: TypedExpr,
        end: TypedExpr,
        step: Option<TypedExpr>,
        body: Vec<TypedStatement>,
    },

    /// WHILE/WEND loop.
    While {
        condition: TypedExpr,
        body: Vec<TypedStatement>,
    },

    /// DO/LOOP.
    DoLoop {
        pre_condition: Option<TypedDoCondition>,
        body: Vec<TypedStatement>,
        post_condition: Option<TypedDoCondition>,
    },

    /// GOTO statement.
    Goto { target: String },

    /// GOSUB statement.
    Gosub { target: String },

    /// RETURN statement.
    Return,

    /// EXIT statement.
    Exit { exit_type: ExitType },

    /// END statement.
    End,

    /// STOP statement.
    Stop,

    /// SUB procedure call.
    Call { name: String, args: Vec<TypedExpr> },

    /// SUB definition.
    SubDefinition {
        name: String,
        params: Vec<TypedParameter>,
        body: Vec<TypedStatement>,
        is_static: bool,
    },

    /// FUNCTION definition.
    FunctionDefinition {
        name: String,
        params: Vec<TypedParameter>,
        return_type: BasicType,
        body: Vec<TypedStatement>,
        is_static: bool,
    },

    /// DIM statement (variable/array declaration).
    Dim {
        name: String,
        basic_type: BasicType,
        dimensions: Vec<TypedArrayDimension>,
        shared: bool,
    },

    /// CONST statement.
    Const {
        name: String,
        value: TypedExpr,
        basic_type: BasicType,
    },

    /// Label definition.
    Label { name: String },

    /// Comment (preserved for documentation).
    Comment(String),

    /// Expression used as statement (e.g., bare function call).
    Expression(TypedExpr),

    // ==================== Preprocessor Directives ====================
    /// $INCLUDE directive (file inclusion).
    IncludeDirective {
        /// Path to include.
        path: String,
    },

    /// $IF conditional compilation block.
    ConditionalBlock {
        /// The compile-time condition.
        condition: String,
        /// Statements in the $IF block.
        then_branch: Vec<TypedStatement>,
        /// $ELSEIF clauses.
        elseif_branches: Vec<(String, Vec<TypedStatement>)>,
        /// $ELSE block.
        else_branch: Option<Vec<TypedStatement>>,
    },

    /// Other meta-command directive.
    MetaCommand {
        /// Command name without $.
        command: String,
        /// Command arguments.
        args: Option<String>,
    },

    /// SWAP statement - exchange values of two variables.
    Swap {
        /// First variable/expression to swap.
        left: TypedExpr,
        /// Second variable/expression to swap.
        right: TypedExpr,
    },

    /// _CONTINUE statement - skip to next loop iteration.
    Continue {
        /// The type of loop to continue.
        continue_type: ContinueType,
    },

    /// TYPE definition - user-defined type.
    TypeDefinition {
        /// The type name.
        name: String,
        /// The type members with their types.
        members: Vec<TypedMember>,
    },

    /// DATA statement - compile-time data values.
    ///
    /// DATA values are collected into a global pool that READ consumes.
    Data {
        /// The literal values in this DATA statement.
        values: Vec<TypedDataValue>,
    },

    /// READ statement - read from DATA pool.
    ///
    /// Each READ consumes values from the DATA pool in order.
    Read {
        /// Variables to read into, paired with their types.
        variables: Vec<(String, BasicType)>,
    },

    /// RESTORE statement - reset DATA pointer.
    ///
    /// RESTORE resets the DATA read position to the beginning
    /// or to a labeled DATA statement.
    Restore {
        /// Optional label to restore to.
        label: Option<String>,
    },
}

/// A typed member of a TYPE definition.
#[derive(Debug, Clone)]
pub struct TypedMember {
    /// Member name.
    pub name: String,
    /// Member type.
    pub basic_type: BasicType,
}

/// A typed value from a DATA statement.
///
/// DATA values are typed during semantic analysis based on their literal form.
#[derive(Debug, Clone)]
pub enum TypedDataValue {
    /// An integer literal.
    Integer(i64),
    /// A floating-point literal.
    Float(f64),
    /// A string literal.
    String(String),
}

impl TypedStatement {
    /// Creates a new typed statement.
    pub fn new(kind: TypedStatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// A typed print item with its separator.
#[derive(Debug, Clone)]
pub struct TypedPrintItem {
    /// The expression to print.
    pub expr: TypedExpr,
    /// Separator after this item (Semicolon, Comma, or None).
    pub separator: Option<PrintSeparator>,
}

/// A typed DO loop condition.
#[derive(Debug, Clone)]
pub struct TypedDoCondition {
    /// True for WHILE, false for UNTIL.
    pub is_while: bool,
    /// The condition expression.
    pub condition: TypedExpr,
}

/// A typed CASE clause.
#[derive(Debug, Clone)]
pub struct TypedCaseClause {
    /// Match conditions.
    pub matches: Vec<TypedCaseMatch>,
    /// Body statements.
    pub body: Vec<TypedStatement>,
}

/// A typed CASE match condition.
#[derive(Debug, Clone)]
pub enum TypedCaseMatch {
    /// Single value: `CASE 1`
    Single(TypedExpr),
    /// Range: `CASE 1 TO 10`
    Range { from: TypedExpr, to: TypedExpr },
    /// Comparison: `CASE IS > 5`
    Comparison {
        op: TypedCaseCompareOp,
        value: TypedExpr,
    },
}

/// Comparison operators for CASE IS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedCaseCompareOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// A typed procedure parameter.
#[derive(Debug, Clone)]
pub struct TypedParameter {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub basic_type: BasicType,
    /// Whether BYVAL was specified.
    pub by_val: bool,
}

/// A typed array dimension.
#[derive(Debug, Clone)]
pub struct TypedArrayDimension {
    /// Lower bound (evaluated to constant).
    pub lower: i64,
    /// Upper bound (evaluated to constant).
    pub upper: i64,
}

/// The complete typed program - output of semantic analysis.
///
/// This structure is ready for code generation.
#[derive(Debug)]
pub struct TypedProgram {
    /// The type-checked statements.
    pub statements: Vec<TypedStatement>,
}

impl TypedProgram {
    /// Creates a new typed program.
    pub fn new(statements: Vec<TypedStatement>) -> Self {
        Self { statements }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typed_expr_creation() {
        let expr = TypedExpr::integer(42, Span::new(0, 2));
        assert_eq!(expr.basic_type, BasicType::Long);
        assert!(matches!(expr.kind, TypedExprKind::IntegerLiteral(42)));
    }

    #[test]
    fn test_convert_to() {
        let int_expr = TypedExpr::integer(42, Span::new(0, 2));
        let converted = int_expr.convert_to(BasicType::Double);

        assert_eq!(converted.basic_type, BasicType::Double);
        assert!(matches!(converted.kind, TypedExprKind::Convert { .. }));
    }

    #[test]
    fn test_convert_to_same_type() {
        let int_expr = TypedExpr::integer(42, Span::new(0, 2));
        let not_converted = int_expr.convert_to(BasicType::Long);

        // Should NOT wrap in Convert since types match
        assert!(matches!(
            not_converted.kind,
            TypedExprKind::IntegerLiteral(42)
        ));
    }

    #[test]
    fn test_typed_statement() {
        let stmt = TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "x".to_string(),
                value: TypedExpr::integer(5, Span::new(4, 5)),
                target_type: BasicType::Integer,
            },
            Span::new(0, 5),
        );

        assert!(matches!(stmt.kind, TypedStatementKind::Assignment { .. }));
    }
}
