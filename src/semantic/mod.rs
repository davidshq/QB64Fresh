//! Semantic analysis for QB64Fresh.
//!
//! This module performs the semantic analysis phase of compilation, which occurs
//! after parsing and before code generation. It handles:
//!
//! - **Symbol resolution**: Linking identifier uses to their declarations
//! - **Type checking**: Ensuring operations have compatible types
//! - **Type inference**: Determining types for expressions and implicit variables
//! - **Validation**: Enforcing language rules beyond syntax
//!
//! # Architecture
//!
//! The semantic analyzer uses a **two-pass** approach:
//!
//! 1. **Pass 1 (Declaration Collection)**: Scans for all SUB/FUNCTION definitions
//!    and labels. This enables forward references - you can call a SUB before
//!    its definition appears in the source.
//!
//! 2. **Pass 2 (Type Checking)**: Processes all statements, resolving types
//!    and validating operations using the declarations from Pass 1.
//!
//! # Output
//!
//! The output is a `TypedProgram` - the AST annotated with type information,
//! ready for code generation.
//!
//! # Usage
//!
//! ```ignore
//! use qb64fresh::semantic::SemanticAnalyzer;
//!
//! let program = parser.parse()?;
//! let typed_program = SemanticAnalyzer::new().analyze(&program)?;
//! // typed_program is ready for codegen
//! ```

pub mod checker;
pub mod error;
pub mod symbols;
pub mod typed_ir;
pub mod types;

pub use error::SemanticError;
pub use symbols::{ParameterInfo, ProcedureEntry, ProcedureKind, ScopeKind, Symbol, SymbolTable};
pub use typed_ir::TypedProgram;
pub use types::BasicType;

use crate::ast::{Program, Statement, StatementKind};
use checker::TypeChecker;
use types::from_type_spec;

/// Main entry point for semantic analysis.
///
/// The `SemanticAnalyzer` transforms an untyped AST into a typed IR,
/// performing symbol resolution and type checking along the way.
pub struct SemanticAnalyzer {
    /// The symbol table for this compilation unit.
    symbols: SymbolTable,
    /// Accumulated errors.
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    /// Creates a new semantic analyzer.
    ///
    /// The analyzer starts with built-in functions pre-registered.
    pub fn new() -> Self {
        let mut analyzer = Self {
            symbols: SymbolTable::new(),
            errors: Vec::new(),
        };
        analyzer.register_builtins();
        analyzer
    }

    /// Analyzes a program and produces a typed IR.
    ///
    /// Returns `Ok(TypedProgram)` if analysis succeeds, or `Err(Vec<SemanticError>)`
    /// if any semantic errors were found.
    pub fn analyze(&mut self, program: &Program) -> Result<TypedProgram, Vec<SemanticError>> {
        // Pass 1: Collect all declarations (enables forward references)
        self.collect_declarations(&program.statements);

        // Pass 2: Type check all statements
        let mut checker = TypeChecker::new(&mut self.symbols);
        let typed_statements = checker.check_statements(&program.statements);

        // Collect errors from checker
        self.errors.append(&mut checker.errors);

        if self.errors.is_empty() {
            Ok(TypedProgram::new(typed_statements))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Pass 1: Collects all declarations without analyzing bodies.
    ///
    /// This enables forward references - procedures and labels can be used
    /// before their definitions appear in the source.
    fn collect_declarations(&mut self, statements: &[Statement]) {
        for stmt in statements {
            match &stmt.kind {
                StatementKind::SubDefinition {
                    name,
                    params,
                    is_static,
                    ..
                } => {
                    self.register_sub(name, params, *is_static, stmt.span);
                }

                StatementKind::FunctionDefinition {
                    name,
                    params,
                    return_type,
                    is_static,
                    ..
                } => {
                    self.register_function(name, params, return_type, *is_static, stmt.span);
                }

                StatementKind::Label { name } => {
                    if let Err(existing) = self.symbols.define_label(name.clone(), stmt.span) {
                        self.errors.push(SemanticError::DuplicateLabel {
                            name: name.clone(),
                            original_span: existing.span,
                            duplicate_span: stmt.span,
                        });
                    }
                }

                // Recursively collect from nested blocks
                StatementKind::If {
                    then_branch,
                    elseif_branches,
                    else_branch,
                    ..
                } => {
                    self.collect_declarations(then_branch);
                    for (_, branch) in elseif_branches {
                        self.collect_declarations(branch);
                    }
                    if let Some(eb) = else_branch {
                        self.collect_declarations(eb);
                    }
                }

                StatementKind::For { body, .. }
                | StatementKind::While { body, .. }
                | StatementKind::DoLoop { body, .. } => {
                    self.collect_declarations(body);
                }

                StatementKind::SelectCase {
                    cases, case_else, ..
                } => {
                    for case in cases {
                        self.collect_declarations(&case.body);
                    }
                    if let Some(ce) = case_else {
                        self.collect_declarations(ce);
                    }
                }

                _ => {}
            }
        }
    }

    /// Registers a SUB in the symbol table.
    fn register_sub(
        &mut self,
        name: &str,
        params: &[crate::ast::Parameter],
        is_static: bool,
        span: crate::ast::Span,
    ) {
        let param_infos: Vec<ParameterInfo> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| types::type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                ParameterInfo {
                    name: p.name.clone(),
                    basic_type,
                    by_val: p.by_val,
                }
            })
            .collect();

        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::Sub,
            params: param_infos,
            return_type: None,
            span,
            is_static,
        };

        if let Err(existing) = self.symbols.define_procedure(entry) {
            self.errors.push(SemanticError::DuplicateProcedure {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }
    }

    /// Registers a FUNCTION in the symbol table.
    fn register_function(
        &mut self,
        name: &str,
        params: &[crate::ast::Parameter],
        return_type: &Option<crate::ast::TypeSpec>,
        is_static: bool,
        span: crate::ast::Span,
    ) {
        let param_infos: Vec<ParameterInfo> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| types::type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                ParameterInfo {
                    name: p.name.clone(),
                    basic_type,
                    by_val: p.by_val,
                }
            })
            .collect();

        // Return type from AS clause, suffix on name, or default
        let ret_type = return_type
            .as_ref()
            .map(from_type_spec)
            .or_else(|| types::type_from_suffix(name))
            .unwrap_or_else(|| self.symbols.default_type_for(name));

        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::Function,
            params: param_infos,
            return_type: Some(ret_type),
            span,
            is_static,
        };

        if let Err(existing) = self.symbols.define_procedure(entry) {
            self.errors.push(SemanticError::DuplicateProcedure {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }
    }

    /// Registers all built-in functions.
    fn register_builtins(&mut self) {
        // String functions
        self.register_builtin_function("LEN", &[("s", BasicType::String)], BasicType::Long);
        self.register_builtin_function("CHR$", &[("n", BasicType::Integer)], BasicType::String);
        self.register_builtin_function("ASC", &[("s", BasicType::String)], BasicType::Integer);
        self.register_builtin_function(
            "LEFT$",
            &[("s", BasicType::String), ("n", BasicType::Integer)],
            BasicType::String,
        );
        self.register_builtin_function(
            "RIGHT$",
            &[("s", BasicType::String), ("n", BasicType::Integer)],
            BasicType::String,
        );
        self.register_builtin_function(
            "MID$",
            &[
                ("s", BasicType::String),
                ("start", BasicType::Integer),
                ("len", BasicType::Integer),
            ],
            BasicType::String,
        );
        self.register_builtin_function(
            "INSTR",
            &[
                ("start", BasicType::Integer),
                ("s", BasicType::String),
                ("find", BasicType::String),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("UCASE$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("LCASE$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("LTRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("RTRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("STR$", &[("n", BasicType::Double)], BasicType::String);
        self.register_builtin_function("VAL", &[("s", BasicType::String)], BasicType::Double);
        self.register_builtin_function(
            "STRING$",
            &[("n", BasicType::Integer), ("c", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function("SPACE$", &[("n", BasicType::Integer)], BasicType::String);

        // Math functions
        self.register_builtin_function("ABS", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("SGN", &[("n", BasicType::Double)], BasicType::Integer);
        self.register_builtin_function("INT", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("FIX", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("CINT", &[("n", BasicType::Double)], BasicType::Integer);
        self.register_builtin_function("CLNG", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("CSNG", &[("n", BasicType::Double)], BasicType::Single);
        self.register_builtin_function("CDBL", &[("n", BasicType::Single)], BasicType::Double);
        self.register_builtin_function("SQR", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("LOG", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("EXP", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("SIN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("COS", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("TAN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("ATN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("RND", &[("n", BasicType::Single)], BasicType::Single);

        // QB64 extended math functions
        self.register_builtin_function("_PI", &[], BasicType::Double);
        self.register_builtin_function("_ASIN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ACOS", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function(
            "_ATAN2",
            &[("y", BasicType::Double), ("x", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_HYPOT",
            &[("x", BasicType::Double), ("y", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function("_CEIL", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("_ROUND", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function(
            "_MIN",
            &[("a", BasicType::Double), ("b", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_MAX",
            &[("a", BasicType::Double), ("b", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_CLAMP",
            &[
                ("value", BasicType::Double),
                ("min", BasicType::Double),
                ("max", BasicType::Double),
            ],
            BasicType::Double,
        );

        // Type conversion
        self.register_builtin_function("HEX$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("OCT$", &[("n", BasicType::Long)], BasicType::String);

        // Array functions
        self.register_builtin_function("LBOUND", &[("arr", BasicType::Unknown)], BasicType::Long);
        self.register_builtin_function("UBOUND", &[("arr", BasicType::Unknown)], BasicType::Long);

        // Timer/Date
        self.register_builtin_function("TIMER", &[], BasicType::Single);
        self.register_builtin_function("DATE$", &[], BasicType::String);
        self.register_builtin_function("TIME$", &[], BasicType::String);

        // File I/O functions
        self.register_builtin_function("EOF", &[("fnum", BasicType::Integer)], BasicType::Integer);
        self.register_builtin_function("LOF", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("LOC", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("FREEFILE", &[], BasicType::Integer);

        // Keyboard input functions
        self.register_builtin_function("INKEY$", &[], BasicType::String);
        self.register_builtin_function("INPUT$", &[("n", BasicType::Integer)], BasicType::String);

        // Error handling functions
        self.register_builtin_function("ERR", &[], BasicType::Integer);
        self.register_builtin_function("ERL", &[], BasicType::Integer);

        // Environment functions
        self.register_builtin_function(
            "ENVIRON$",
            &[("var", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function("COMMAND$", &[], BasicType::String);
        self.register_builtin_function("_CWD$", &[], BasicType::String);
        self.register_builtin_function("_OS$", &[], BasicType::String);
        self.register_builtin_function("_STARTDIR$", &[], BasicType::String);

        // Additional utility functions
        self.register_builtin_function(
            "STRING$",
            &[("n", BasicType::Integer), ("char", BasicType::Integer)],
            BasicType::String,
        );
    }

    /// Registers a single built-in function.
    fn register_builtin_function(
        &mut self,
        name: &str,
        params: &[(&str, BasicType)],
        return_type: BasicType,
    ) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                })
                .collect(),
            return_type: Some(return_type),
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, ExprKind, Span};

    fn make_program(statements: Vec<Statement>) -> Program {
        Program { statements }
    }

    fn make_print_stmt(value: i64) -> Statement {
        Statement::new(
            StatementKind::Print {
                values: vec![crate::ast::PrintItem {
                    expr: Expr::new(ExprKind::IntegerLiteral(value), Span::new(6, 8)),
                    separator: None,
                }],
                newline: true,
            },
            Span::new(0, 8),
        )
    }

    fn make_assignment(name: &str, value: i64) -> Statement {
        Statement::new(
            StatementKind::Let {
                name: name.to_string(),
                value: Expr::new(ExprKind::IntegerLiteral(value), Span::new(4, 5)),
            },
            Span::new(0, 5),
        )
    }

    #[test]
    fn test_analyze_simple_program() {
        let program = make_program(vec![make_print_stmt(42)]);
        let result = SemanticAnalyzer::new().analyze(&program);

        assert!(result.is_ok());
        let typed = result.unwrap();
        assert_eq!(typed.statements.len(), 1);
    }

    #[test]
    fn test_implicit_variable_declaration() {
        let program = make_program(vec![make_assignment("x", 5)]);
        let result = SemanticAnalyzer::new().analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_builtin_function_exists() {
        let analyzer = SemanticAnalyzer::new();

        // Check some built-in functions exist
        assert!(analyzer.symbols.lookup_procedure("LEN").is_some());
        assert!(analyzer.symbols.lookup_procedure("CHR$").is_some());
        assert!(analyzer.symbols.lookup_procedure("SIN").is_some());
        assert!(analyzer.symbols.lookup_procedure("TIMER").is_some());
    }

    #[test]
    fn test_type_mismatch_error() {
        // Assign string to integer variable
        let program = make_program(vec![
            Statement::new(
                StatementKind::Dim {
                    name: "x".to_string(),
                    dimensions: vec![],
                    type_spec: Some(crate::ast::TypeSpec::Integer),
                    shared: false,
                },
                Span::new(0, 15),
            ),
            Statement::new(
                StatementKind::Let {
                    name: "x".to_string(),
                    value: Expr::new(
                        ExprKind::StringLiteral("hello".to_string()),
                        Span::new(4, 11),
                    ),
                },
                Span::new(0, 11),
            ),
        ]);

        let result = SemanticAnalyzer::new().analyze(&program);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemanticError::TypeMismatch { .. }))
        );
    }
}
