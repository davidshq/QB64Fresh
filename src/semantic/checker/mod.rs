//! Type checker for QB64Fresh semantic analysis.
//!
//! The type checker validates expressions and statements, ensuring type
//! compatibility and producing a typed IR. It handles:
//!
//! - **Expression type inference**: Determining the type of each expression
//! - **Binary/unary operation validation**: Checking operator applicability
//! - **Assignment type checking**: Ensuring values match target types
//! - **Function call validation**: Argument count and type checking
//! - **Control flow validation**: EXIT context, FOR/NEXT matching
//!
//! # Module Structure
//!
//! The type checker is split into focused modules:
//! - [`expressions`] - Expression type checking (literals, binary/unary ops, calls)
//! - [`statements`] - Statement dispatcher
//! - [`assignments`] - Assignment and I/O statement checking
//! - [`control_flow`] - Loop and branch checking (IF, FOR, WHILE, SELECT CASE)
//! - [`definitions`] - SUB/FUNCTION/DIM/CONST definitions
//! - [`const_eval`] - Compile-time constant expression evaluation
//!
//! # Error Recovery
//!
//! The type checker continues after errors when possible, collecting all
//! errors for batch reporting rather than stopping at the first problem.

mod assignments;
mod const_eval;
mod control_flow;
mod definitions;
mod expressions;
mod statements;

use crate::ast::{Expr, Span, Statement};
use crate::semantic::{error::SemanticError, symbols::SymbolTable, typed_ir::*, types::BasicType};

/// Tracks loop nesting for EXIT validation.
#[derive(Debug, Clone, Default)]
pub(crate) struct LoopContext {
    /// Depth of FOR loops.
    pub for_depth: usize,
    /// Depth of WHILE loops.
    pub while_depth: usize,
    /// Depth of DO loops.
    pub do_depth: usize,
}

/// Bundles FOR loop components for type checking.
///
/// This struct groups the related parts of a FOR statement to reduce
/// the number of parameters passed to `check_for`.
pub(crate) struct ForLoopInfo<'a> {
    /// Loop counter variable name.
    pub variable: &'a str,
    /// Start value expression.
    pub start: &'a Expr,
    /// End value expression.
    pub end: &'a Expr,
    /// Optional step value expression.
    pub step: &'a Option<Expr>,
    /// Loop body statements.
    pub body: &'a [Statement],
    /// Optional variable name after NEXT (for validation).
    pub next_variable: &'a Option<String>,
    /// Source span for error reporting.
    pub span: Span,
}

/// The type checker validates and annotates the AST with types.
pub struct TypeChecker<'a> {
    /// Reference to the symbol table for lookups and definitions.
    pub(crate) symbols: &'a mut SymbolTable,
    /// Accumulated errors.
    pub errors: Vec<SemanticError>,
    /// Current loop nesting context.
    pub(crate) loop_context: LoopContext,
    /// Whether we're inside a FUNCTION (for EXIT FUNCTION / return checking).
    pub(crate) in_function: bool,
    /// Whether we're inside a SUB (for EXIT SUB).
    pub(crate) in_sub: bool,
    /// Current function name (for assigning return value).
    pub(crate) current_function_name: Option<String>,
}

impl<'a> TypeChecker<'a> {
    /// Creates a new type checker.
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            errors: Vec::new(),
            loop_context: LoopContext::default(),
            in_function: false,
            in_sub: false,
            current_function_name: None,
        }
    }

    /// Type checks a list of statements.
    pub fn check_statements(&mut self, statements: &[Statement]) -> Vec<TypedStatement> {
        statements
            .iter()
            .map(|stmt| self.check_statement(stmt))
            .collect()
    }

    /// Checks if two types are comparable (for comparison operators).
    pub(crate) fn types_comparable(&self, a: &BasicType, b: &BasicType) -> bool {
        (a.is_numeric() && b.is_numeric()) || (a.is_string() && b.is_string())
    }

    /// Looks up a field's type in a user-defined TYPE.
    ///
    /// Returns None if the type or field doesn't exist.
    pub(crate) fn lookup_type_field(&self, type_name: &str, field: &str) -> Option<BasicType> {
        self.symbols.lookup_type_member(type_name, field)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, ExprKind, Span, StatementKind};
    use crate::semantic::symbols::{Symbol, SymbolKind};

    fn make_int_expr(val: i64) -> Expr {
        Expr::new(ExprKind::IntegerLiteral(val), Span::new(0, 1))
    }

    fn make_str_expr(val: &str) -> Expr {
        Expr::new(ExprKind::StringLiteral(val.to_string()), Span::new(0, 1))
    }

    fn make_ident_expr(name: &str) -> Expr {
        Expr::new(ExprKind::Identifier(name.to_string()), Span::new(0, 1))
    }

    #[test]
    fn test_const_with_literal() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // CONST X = 42 should succeed
        let stmt = Statement::new(
            StatementKind::Const {
                name: "X".to_string(),
                value: make_int_expr(42),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CONST with literal should not error"
        );
    }

    #[test]
    fn test_const_with_variable_errors() {
        let mut symbols = SymbolTable::new();

        // First define a variable before creating the checker
        let _ = symbols.define_symbol(Symbol {
            name: "someVar".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Long,
            span: Span::new(0, 7),
            is_mutable: true,
        });

        let mut checker = TypeChecker::new(&mut symbols);

        // CONST X = someVar should error
        let stmt = Statement::new(
            StatementKind::Const {
                name: "X".to_string(),
                value: make_ident_expr("someVar"),
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            !checker.errors.is_empty(),
            "CONST with variable should error"
        );
        assert!(
            matches!(
                checker.errors[0],
                SemanticError::NonConstantExpression { .. }
            ),
            "Expected NonConstantExpression error"
        );
    }

    #[test]
    fn test_const_with_constant_expr() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // CONST X = 1 + 2 should succeed (constant expression)
        let stmt = Statement::new(
            StatementKind::Const {
                name: "X".to_string(),
                value: Expr::new(
                    ExprKind::Binary {
                        left: Box::new(make_int_expr(1)),
                        op: BinaryOp::Add,
                        right: Box::new(make_int_expr(2)),
                    },
                    Span::new(0, 5),
                ),
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CONST with constant expression should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_const_referencing_other_const() {
        let mut symbols = SymbolTable::new();

        // First define a constant A = 10
        let _ = symbols.define_symbol(Symbol {
            name: "A".to_string(),
            kind: SymbolKind::Constant {
                value: crate::semantic::symbols::ConstValue::Integer(10),
            },
            basic_type: BasicType::Long,
            span: Span::new(0, 10),
            is_mutable: false,
        });

        let mut checker = TypeChecker::new(&mut symbols);

        // CONST B = A should succeed (reference to another constant)
        let stmt = Statement::new(
            StatementKind::Const {
                name: "B".to_string(),
                value: make_ident_expr("A"),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CONST referencing another CONST should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_dim_with_constant_bounds() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(10) should succeed
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: None,
                    upper: make_int_expr(10),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 15),
        );

        let typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DIM with constant bounds should not error: {:?}",
            checker.errors
        );

        // Verify the dimensions were correctly evaluated
        if let TypedStatementKind::Dim { dimensions, .. } = &typed.kind {
            assert_eq!(dimensions.len(), 1);
            assert_eq!(dimensions[0].lower, 0); // Default OPTION BASE
            assert_eq!(dimensions[0].upper, 10);
        } else {
            panic!("Expected Dim statement");
        }
    }

    #[test]
    fn test_dim_with_expression_bounds() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(5 + 5) should succeed and evaluate to 10
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: None,
                    upper: Expr::new(
                        ExprKind::Binary {
                            left: Box::new(make_int_expr(5)),
                            op: BinaryOp::Add,
                            right: Box::new(make_int_expr(5)),
                        },
                        Span::new(0, 5),
                    ),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 20),
        );

        let typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DIM with constant expression bounds should not error: {:?}",
            checker.errors
        );

        // Verify the dimensions were correctly evaluated
        if let TypedStatementKind::Dim { dimensions, .. } = &typed.kind {
            assert_eq!(dimensions[0].upper, 10);
        } else {
            panic!("Expected Dim statement");
        }
    }

    #[test]
    fn test_dim_with_lower_and_upper_bounds() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(1 TO 10) should succeed
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: Some(make_int_expr(1)),
                    upper: make_int_expr(10),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 20),
        );

        let typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DIM with lower TO upper should not error: {:?}",
            checker.errors
        );

        // Verify both bounds
        if let TypedStatementKind::Dim { dimensions, .. } = &typed.kind {
            assert_eq!(dimensions[0].lower, 1);
            assert_eq!(dimensions[0].upper, 10);
        } else {
            panic!("Expected Dim statement");
        }
    }

    #[test]
    fn test_dim_with_variable_bound_errors() {
        let mut symbols = SymbolTable::new();

        // Define a variable (not a constant)
        let _ = symbols.define_symbol(Symbol {
            name: "size".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Long,
            span: Span::new(0, 4),
            is_mutable: true,
        });

        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(size) should error - variable bounds not allowed
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: None,
                    upper: make_ident_expr("size"),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            !checker.errors.is_empty(),
            "DIM with variable bound should error"
        );
        assert!(
            matches!(
                checker.errors[0],
                SemanticError::NonConstantExpression { .. }
            ),
            "Expected NonConstantExpression error"
        );
    }

    #[test]
    fn test_check_integer_literal() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = make_int_expr(42);
        let typed = checker.check_expr(&expr);

        assert_eq!(typed.basic_type, BasicType::Long);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_binary_add() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(make_int_expr(1)),
                op: BinaryOp::Add,
                right: Box::new(make_int_expr(2)),
            },
            Span::new(0, 5),
        );

        let typed = checker.check_expr(&expr);
        assert_eq!(typed.basic_type, BasicType::Long);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_string_concat() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(make_str_expr("hello")),
                op: BinaryOp::Add,
                right: Box::new(make_str_expr(" world")),
            },
            Span::new(0, 15),
        );

        let typed = checker.check_expr(&expr);
        assert_eq!(typed.basic_type, BasicType::String);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_invalid_binary_op() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // String + Integer should error
        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(make_str_expr("hello")),
                op: BinaryOp::Add,
                right: Box::new(make_int_expr(42)),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_expr(&expr);
        assert!(!checker.errors.is_empty());
        assert!(matches!(
            checker.errors[0],
            SemanticError::InvalidBinaryOp { .. }
        ));
    }

    #[test]
    fn test_implicit_variable() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // Reference to undeclared variable should implicitly declare it
        let expr = Expr::new(ExprKind::Identifier("x".to_string()), Span::new(0, 1));

        let typed = checker.check_expr(&expr);
        // Default type is SINGLE
        assert_eq!(typed.basic_type, BasicType::Single);
        assert!(checker.errors.is_empty());

        // Variable should now exist
        assert!(symbols.lookup_symbol("x").is_some());
    }

    #[test]
    fn test_variable_with_suffix() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = Expr::new(ExprKind::Identifier("name$".to_string()), Span::new(0, 5));

        let typed = checker.check_expr(&expr);
        assert_eq!(typed.basic_type, BasicType::String);
    }

    #[test]
    fn test_for_next_variable_mismatch() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // FOR i = 1 TO 10 ... NEXT j (should error - j != i)
        let stmt = Statement::new(
            StatementKind::For {
                variable: "i".to_string(),
                start: make_int_expr(1),
                end: make_int_expr(10),
                step: None,
                body: vec![],
                next_variable: Some("j".to_string()), // Mismatched!
            },
            Span::new(0, 20),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            !checker.errors.is_empty(),
            "FOR/NEXT variable mismatch should error"
        );
        assert!(
            matches!(checker.errors[0], SemanticError::ForNextMismatch { .. }),
            "Expected ForNextMismatch error"
        );
    }

    #[test]
    fn test_for_next_variable_match() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // FOR i = 1 TO 10 ... NEXT i (should be OK)
        let stmt = Statement::new(
            StatementKind::For {
                variable: "i".to_string(),
                start: make_int_expr(1),
                end: make_int_expr(10),
                step: None,
                body: vec![],
                next_variable: Some("i".to_string()), // Matches
            },
            Span::new(0, 20),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "FOR/NEXT with matching variable should not error"
        );
    }

    #[test]
    fn test_for_next_variable_case_insensitive() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // FOR i = 1 TO 10 ... NEXT I (should be OK - case insensitive)
        let stmt = Statement::new(
            StatementKind::For {
                variable: "counter".to_string(),
                start: make_int_expr(1),
                end: make_int_expr(10),
                step: None,
                body: vec![],
                next_variable: Some("COUNTER".to_string()), // Different case
            },
            Span::new(0, 20),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "FOR/NEXT with case-different variable should not error"
        );
    }

    // ========================================================
    // Phase 1 Feature Tests: File I/O Type Checking
    // ========================================================

    #[test]
    fn test_file_close_accepts_numeric() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // CLOSE #1 - file number should be accepted as numeric
        let stmt = Statement::new(
            StatementKind::CloseFile {
                file_nums: vec![make_int_expr(1)],
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CLOSE with integer file number should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_open_file_basic() {
        use crate::ast::FileMode;

        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // OPEN "test.txt" FOR INPUT AS #1
        let stmt = Statement::new(
            StatementKind::OpenFile {
                filename: make_str_expr("test.txt"),
                mode: FileMode::Input,
                access: None,
                lock: None,
                file_num: make_int_expr(1),
                record_len: None,
            },
            Span::new(0, 30),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "OPEN with valid arguments should not error: {:?}",
            checker.errors
        );
    }

    // ========================================================
    // Phase 1 Feature Tests: Error Handling Type Checking
    // ========================================================

    #[test]
    fn test_on_error_goto() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // ON ERROR GOTO errorHandler
        let stmt = Statement::new(
            StatementKind::OnErrorGoto {
                target: "errorHandler".to_string(),
            },
            Span::new(0, 25),
        );

        let _typed = checker.check_statement(&stmt);
        // ON ERROR GOTO doesn't validate label existence at type check time
        assert!(
            checker.errors.is_empty(),
            "ON ERROR GOTO should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_error_statement_numeric() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // ERROR 53 - should accept numeric error code
        let stmt = Statement::new(
            StatementKind::ErrorStmt {
                code: make_int_expr(53),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "ERROR with integer code should not error: {:?}",
            checker.errors
        );
    }

    // ========================================================
    // Phase 1 Feature Tests: Computed Control Flow
    // ========================================================

    #[test]
    fn test_on_goto_numeric_selector() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // ON choice GOTO label1, label2
        let stmt = Statement::new(
            StatementKind::OnGoto {
                selector: make_int_expr(1),
                targets: vec!["label1".to_string(), "label2".to_string()],
            },
            Span::new(0, 30),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "ON GOTO with integer selector should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_on_gosub_numeric_selector() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // ON menu GOSUB sub1, sub2
        let stmt = Statement::new(
            StatementKind::OnGosub {
                selector: make_int_expr(2),
                targets: vec!["sub1".to_string(), "sub2".to_string()],
            },
            Span::new(0, 25),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "ON GOSUB with integer selector should not error: {:?}",
            checker.errors
        );
    }

    // ========================================================
    // Phase 1 Feature Tests: DEF FN
    // ========================================================

    #[test]
    fn test_def_fn_basic() {
        use crate::ast::Parameter;

        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DEF FNdouble(x) = x * 2
        let stmt = Statement::new(
            StatementKind::DefFn {
                name: "FNdouble".to_string(),
                params: vec![Parameter {
                    name: "x".to_string(),
                    type_spec: None,
                    by_val: false,
                }],
                body: Expr::new(
                    ExprKind::Binary {
                        left: Box::new(make_ident_expr("x")),
                        op: BinaryOp::Multiply,
                        right: Box::new(make_int_expr(2)),
                    },
                    Span::new(0, 5),
                ),
            },
            Span::new(0, 25),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DEF FN should not error: {:?}",
            checker.errors
        );
    }

    // ========================================================
    // Phase 1 Feature Tests: COMMON / REDIM
    // ========================================================

    #[test]
    fn test_common_statement() {
        use crate::ast::CommonVariable;

        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // COMMON x, y$
        let stmt = Statement::new(
            StatementKind::CommonStmt {
                shared: false,
                variables: vec![
                    CommonVariable {
                        name: "x".to_string(),
                        dimensions: vec![],
                        type_spec: None,
                    },
                    CommonVariable {
                        name: "y$".to_string(),
                        dimensions: vec![],
                        type_spec: None,
                    },
                ],
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "COMMON should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_redim_statement() {
        use crate::ast::ArrayDimension;

        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // REDIM array(100)
        let stmt = Statement::new(
            StatementKind::Redim {
                preserve: false,
                name: "array".to_string(),
                dimensions: vec![ArrayDimension {
                    lower: None,
                    upper: make_int_expr(100),
                }],
                type_spec: None,
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "REDIM should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_redim_preserve() {
        use crate::ast::ArrayDimension;

        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // REDIM _PRESERVE buffer$(50)
        let stmt = Statement::new(
            StatementKind::Redim {
                preserve: true,
                name: "buffer$".to_string(),
                dimensions: vec![ArrayDimension {
                    lower: None,
                    upper: make_int_expr(50),
                }],
                type_spec: None,
            },
            Span::new(0, 25),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "REDIM _PRESERVE should not error: {:?}",
            checker.errors
        );
    }
}
