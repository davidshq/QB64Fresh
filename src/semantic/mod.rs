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

mod builtins;
mod builtins_opengl;
mod builtins_opengl_functions;
mod collect;

pub mod checker;
pub mod error;
pub mod suggestions;
pub mod symbols;
pub mod typed_ir;
pub mod types;

pub use error::SemanticError;
pub use symbols::{
    ParameterInfo, ProcedureEntry, ProcedureKind, ScopeKind, Symbol, SymbolKind, SymbolTable,
    UserTypeDefinition,
};
pub use typed_ir::TypedProgram;
pub use types::BasicType;

use crate::ast::{Program, Span};
use checker::TypeChecker;

/// Information about a document symbol for LSP.
#[derive(Debug, Clone)]
pub struct DocumentSymbolInfo {
    /// The symbol's name.
    pub name: String,
    /// What kind of symbol this is.
    pub kind: DocumentSymbolKind,
    /// Where the symbol is defined.
    pub span: Span,
}

/// The kind of document symbol for LSP.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentSymbolKind {
    /// SUB (subroutine)
    Sub,
    /// FUNCTION
    Function,
    /// User-defined TYPE
    Type,
    /// CONST constant
    Constant,
    /// Array variable
    Array,
    /// Regular variable
    Variable,
}

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

    /// Finds the definition span of a named symbol.
    ///
    /// Used by the LSP server for "Go to Definition". Searches for:
    /// - Variables (local and global)
    /// - Procedures (SUB and FUNCTION)
    /// - Labels
    /// - User-defined TYPEs
    ///
    /// Returns `None` if the name doesn't match any defined symbol.
    pub fn find_definition(&self, name: &str) -> Option<crate::ast::Span> {
        // Try procedures first (SUB, FUNCTION)
        if let Some(proc) = self.symbols.lookup_procedure(name) {
            return Some(proc.span);
        }

        // Try variables (local scope first, then global)
        if let Some(sym) = self.symbols.lookup_symbol(name) {
            return Some(sym.span);
        }

        // Try global symbols specifically
        if let Some(sym) = self.symbols.lookup_global_symbol(name) {
            return Some(sym.span);
        }

        // Try labels
        if let Some(label) = self.symbols.lookup_label(name) {
            return Some(label.span);
        }

        // Try user-defined types
        if let Some(udt) = self.symbols.lookup_user_type(name) {
            return Some(udt.span);
        }

        None
    }

    /// Gets detailed hover information about a symbol.
    ///
    /// Used by the LSP server for enhanced hover. Returns a markdown-formatted
    /// string describing the symbol including its type, kind, and signature.
    ///
    /// Returns `None` if the name doesn't match any defined symbol.
    pub fn get_hover_info(&self, name: &str) -> Option<String> {
        // Try procedures first (SUB, FUNCTION)
        if let Some(proc) = self.symbols.lookup_procedure(name) {
            let kind = match proc.kind {
                symbols::ProcedureKind::Sub => "SUB",
                symbols::ProcedureKind::Function => "FUNCTION",
                symbols::ProcedureKind::BuiltIn => "Built-in",
                symbols::ProcedureKind::External => "External",
            };

            let params: Vec<String> = proc
                .params
                .iter()
                .map(|p| {
                    let type_str = format_type(&p.basic_type);
                    if p.by_val {
                        format!("BYVAL {} AS {}", p.name, type_str)
                    } else {
                        format!("{} AS {}", p.name, type_str)
                    }
                })
                .collect();

            let params_str = params.join(", ");

            let mut info = if proc.kind == symbols::ProcedureKind::Function {
                let ret = proc
                    .return_type
                    .as_ref()
                    .map(format_type)
                    .unwrap_or_else(|| "SINGLE".to_string());
                format!(
                    "```basic\n{} {}({}) AS {}\n```\n\n",
                    kind, proc.name, params_str, ret
                )
            } else {
                format!("```basic\n{} {}({})\n```\n\n", kind, proc.name, params_str)
            };

            info.push_str(&format!("**{}**", kind));
            if proc.is_static {
                info.push_str(" (STATIC)");
            }

            return Some(info);
        }

        // Try variables
        if let Some(sym) = self.symbols.lookup_symbol(name) {
            return Some(format_symbol_hover(sym));
        }

        // Try global symbols
        if let Some(sym) = self.symbols.lookup_global_symbol(name) {
            return Some(format_symbol_hover(sym));
        }

        // Try labels
        if let Some(label) = self.symbols.lookup_label(name) {
            return Some(format!("**Label:** `{}`", label.name));
        }

        // Try user-defined types
        if let Some(udt) = self.symbols.lookup_user_type(name) {
            let members: Vec<String> = udt
                .members
                .iter()
                .map(|m| format!("    {} AS {}", m.name, format_type(&m.basic_type)))
                .collect();
            let members_str = members.join("\n");
            return Some(format!(
                "```basic\nTYPE {}\n{}\nEND TYPE\n```",
                udt.name, members_str
            ));
        }

        None
    }

    /// Returns all document symbols for the LSP outline view.
    ///
    /// Collects procedures (SUB/FUNCTION), user-defined TYPEs, and
    /// global variables. Returns tuples of (name, kind, span) where
    /// kind is a string identifying the symbol type.
    pub fn get_document_symbols(&self) -> Vec<DocumentSymbolInfo> {
        let mut symbols = Vec::new();

        // Add procedures
        for proc in self.symbols.iter_procedures() {
            // Skip built-in functions (they have span 0..0)
            if proc.span.start == 0 && proc.span.end == 0 {
                continue;
            }
            let kind = match proc.kind {
                symbols::ProcedureKind::Sub => DocumentSymbolKind::Sub,
                symbols::ProcedureKind::Function => DocumentSymbolKind::Function,
                symbols::ProcedureKind::BuiltIn => continue, // Skip built-ins
                symbols::ProcedureKind::External => DocumentSymbolKind::Function, // External functions
            };
            symbols.push(DocumentSymbolInfo {
                name: proc.name.clone(),
                kind,
                span: proc.span,
            });
        }

        // Add user-defined types
        for udt in self.symbols.iter_user_types() {
            symbols.push(DocumentSymbolInfo {
                name: udt.name.clone(),
                kind: DocumentSymbolKind::Type,
                span: udt.span,
            });
        }

        // Add global variables (skip built-in constants with span 0..0)
        for sym in self.symbols.iter_global_symbols() {
            // Skip built-in constants (they have span 0..0)
            if sym.span.start == 0 && sym.span.end == 0 {
                continue;
            }
            let kind = match &sym.kind {
                symbols::SymbolKind::Constant { .. } => DocumentSymbolKind::Constant,
                symbols::SymbolKind::ArrayVariable { .. } => DocumentSymbolKind::Array,
                symbols::SymbolKind::Variable => DocumentSymbolKind::Variable,
                _ => continue, // Skip parameters and external functions
            };
            symbols.push(DocumentSymbolInfo {
                name: sym.name.clone(),
                kind,
                span: sym.span,
            });
        }

        // Sort by position in file
        symbols.sort_by_key(|s| s.span.start);
        symbols
    }

    /// Finds detailed symbol information for a named symbol.
    ///
    /// Used by the LSP server for inlay hints. Returns the Symbol struct
    /// which contains type information.
    ///
    /// Searches global symbols first (where variables and constants are stored).
    pub fn find_symbol_info(&self, name: &str) -> Option<&Symbol> {
        // Try global symbols (where DIM'd variables live)
        if let Some(sym) = self.symbols.lookup_global_symbol(name) {
            return Some(sym);
        }

        // Try local scope
        if let Some(sym) = self.symbols.lookup_symbol(name) {
            return Some(sym);
        }

        None
    }
}

// The collect_declarations and related methods are in collect.rs
// The register_builtins and related methods are in builtins.rs

// Note: SemanticAnalyzer methods from collect.rs and builtins.rs are automatically
// available via Rust's impl block resolution across module files.

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Formats a BasicType for display in hover information.
fn format_type(typ: &BasicType) -> String {
    match typ {
        BasicType::Integer => "INTEGER".to_string(),
        BasicType::Long => "LONG".to_string(),
        BasicType::Single => "SINGLE".to_string(),
        BasicType::Double => "DOUBLE".to_string(),
        BasicType::String => "STRING".to_string(),
        BasicType::Integer64 => "_INTEGER64".to_string(),
        BasicType::Bit => "_BIT".to_string(),
        BasicType::Byte => "_BYTE".to_string(),
        BasicType::Offset => "_OFFSET".to_string(),
        BasicType::Float => "_FLOAT".to_string(),
        BasicType::FixedString(length) => format!("STRING * {}", length),
        BasicType::UnsignedBit => "_UNSIGNED _BIT".to_string(),
        BasicType::UnsignedByte => "_UNSIGNED _BYTE".to_string(),
        BasicType::UnsignedInteger => "_UNSIGNED INTEGER".to_string(),
        BasicType::UnsignedLong => "_UNSIGNED LONG".to_string(),
        BasicType::UnsignedInteger64 => "_UNSIGNED _INTEGER64".to_string(),
        BasicType::UserDefined(name) => name.clone(),
        BasicType::Array {
            element_type,
            dimensions,
        } => {
            format!("{}() x {}", format_type(element_type), dimensions)
        }
        BasicType::Mem => "_MEM".to_string(),
        BasicType::Void => "VOID".to_string(),
        BasicType::Unknown => "UNKNOWN".to_string(),
    }
}

/// Formats a Symbol for hover information.
fn format_symbol_hover(sym: &Symbol) -> String {
    let type_str = format_type(&sym.basic_type);

    match &sym.kind {
        symbols::SymbolKind::Variable => {
            format!(
                "```basic\nDIM {} AS {}\n```\n\n**Variable**",
                sym.name, type_str
            )
        }
        symbols::SymbolKind::Constant { value } => {
            let val_str = match value {
                symbols::ConstValue::Integer(i) => i.to_string(),
                symbols::ConstValue::Float(f) => f.to_string(),
                symbols::ConstValue::String(s) => format!("\"{}\"", s),
            };
            format!(
                "```basic\nCONST {} = {}\n```\n\n**Constant** ({})",
                sym.name, val_str, type_str
            )
        }
        symbols::SymbolKind::Parameter { by_val } => {
            let modifier = if *by_val { "BYVAL " } else { "" };
            format!(
                "```basic\n{}{} AS {}\n```\n\n**Parameter**",
                modifier, sym.name, type_str
            )
        }
        symbols::SymbolKind::ArrayVariable { dimensions, .. } => {
            let dims: Vec<String> = dimensions
                .iter()
                .map(|d| {
                    if d.lower_bound == 0 {
                        format!("{}", d.upper_bound)
                    } else {
                        format!("{} TO {}", d.lower_bound, d.upper_bound)
                    }
                })
                .collect();
            let dims_str = dims.join(", ");
            format!(
                "```basic\nDIM {}({}) AS {}\n```\n\n**Array**",
                sym.name, dims_str, type_str
            )
        }
        symbols::SymbolKind::ExternalFunction {
            c_name,
            params,
            return_type,
        } => {
            let param_types: Vec<String> = params.iter().map(format_type).collect();
            let params_str = param_types.join(", ");
            let ret_str = format_type(return_type);
            format!(
                "```basic\nDECLARE FUNCTION {}({}) AS {}\n' C name: {}\n```\n\n**External Function**",
                sym.name, params_str, ret_str, c_name
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, ExprKind, Span, Statement, StatementKind};

    fn make_program(statements: Vec<Statement>) -> Program {
        Program { statements }
    }

    fn make_print_stmt(value: i64) -> Statement {
        Statement::new(
            StatementKind::Print {
                values: vec![crate::ast::PrintItem {
                    expr: Expr::new(ExprKind::IntegerLiteral(value), Span::new(6, 8, 1)),
                    separator: None,
                }],
                newline: true,
            },
            Span::new(0, 8, 1),
        )
    }

    fn make_assignment(name: &str, value: i64) -> Statement {
        Statement::new(
            StatementKind::Let {
                name: name.to_string(),
                value: Expr::new(ExprKind::IntegerLiteral(value), Span::new(4, 5, 1)),
            },
            Span::new(0, 5, 1),
        )
    }

    #[test]
    fn test_analyze_simple_program() {
        let program = make_program(vec![make_print_stmt(42)]);
        let result = SemanticAnalyzer::new().analyze(&program);

        assert!(result.is_ok());
        let typed = result.expect("analyzing simple program should succeed");
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
    fn test_builtin_constants_exist() {
        use symbols::{ConstValue, SymbolKind};

        let analyzer = SemanticAnalyzer::new();

        // Check _TRUE constant exists and has correct value (-1)
        let true_sym = analyzer.symbols.lookup_symbol("_TRUE");
        assert!(
            true_sym.is_some(),
            "_TRUE should exist as built-in constant"
        );
        let true_sym = true_sym.expect("_TRUE should exist as built-in constant");
        assert!(!true_sym.is_mutable, "_TRUE should be immutable");
        assert!(
            matches!(
                &true_sym.kind,
                SymbolKind::Constant {
                    value: ConstValue::Integer(-1)
                }
            ),
            "_TRUE should be an integer constant with value -1, got {:?}",
            true_sym.kind
        );

        // Check _FALSE constant exists and has correct value (0)
        let false_sym = analyzer.symbols.lookup_symbol("_FALSE");
        assert!(
            false_sym.is_some(),
            "_FALSE should exist as built-in constant"
        );
        let false_sym = false_sym.expect("_FALSE should exist as built-in constant");
        assert!(!false_sym.is_mutable, "_FALSE should be immutable");
        assert!(
            matches!(
                &false_sym.kind,
                SymbolKind::Constant {
                    value: ConstValue::Integer(0)
                }
            ),
            "_FALSE should be an integer constant with value 0, got {:?}",
            false_sym.kind
        );

        // Case-insensitive lookup should work
        assert!(analyzer.symbols.lookup_symbol("_true").is_some());
        assert!(analyzer.symbols.lookup_symbol("_false").is_some());
    }

    #[test]
    fn test_error_code_constants_exist() {
        use symbols::{ConstValue, SymbolKind};

        let analyzer = SemanticAnalyzer::new();

        // Helper to check error constant
        let check_error_const = |name: &str, expected_value: i64| {
            let sym = analyzer.symbols.lookup_symbol(name);
            assert!(sym.is_some(), "{} should exist as built-in constant", name);
            let sym = sym.unwrap_or_else(|| panic!("{} should exist as built-in constant", name));
            assert!(!sym.is_mutable, "{} should be immutable", name);
            assert!(
                matches!(
                    &sym.kind,
                    SymbolKind::Constant {
                        value: ConstValue::Integer(_)
                    }
                ),
                "{} should be an integer constant, got {:?}",
                name,
                sym.kind
            );
            if let SymbolKind::Constant {
                value: ConstValue::Integer(v),
            } = &sym.kind
            {
                assert_eq!(*v, expected_value, "{} should be {}", name, expected_value);
            }
        };

        // Check a selection of error constants
        check_error_const("_ERR_NEXT_WITHOUT_FOR", 1);
        check_error_const("_ERR_SYNTAX_ERROR", 2);
        check_error_const("_ERR_RETURN_WITHOUT_GOSUB", 3);
        check_error_const("_ERR_ILLEGAL_FUNCTION_CALL", 5);
        check_error_const("_ERR_OVERFLOW", 6);
        check_error_const("_ERR_SUBSCRIPT_OUT_OF_RANGE", 9);
        check_error_const("_ERR_DIVISION_BY_ZERO", 11);
        check_error_const("_ERR_TYPE_MISMATCH", 13);
        check_error_const("_ERR_FILE_NOT_FOUND", 53);
        check_error_const("_ERR_PATH_NOT_FOUND", 76);

        // Case-insensitive lookup should work
        assert!(analyzer.symbols.lookup_symbol("_err_overflow").is_some());
        assert!(analyzer.symbols.lookup_symbol("_ERR_OVERFLOW").is_some());
    }

    #[test]
    fn test_type_mismatch_error() {
        // Assign string to integer variable
        let program = make_program(vec![
            Statement::new(
                StatementKind::Dim {
                    variables: vec![crate::ast::DimVariable {
                        name: "x".to_string(),
                        dimensions: vec![],
                        type_spec: Some(crate::ast::TypeSpec::Integer),
                        is_dynamic_array: false,
                    }],
                    shared: false,
                },
                Span::new(0, 15, 1),
            ),
            Statement::new(
                StatementKind::Let {
                    name: "x".to_string(),
                    value: Expr::new(
                        ExprKind::StringLiteral("hello".to_string()),
                        Span::new(4, 11, 1),
                    ),
                },
                Span::new(0, 11, 1),
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
