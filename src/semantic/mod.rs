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
use types::{from_type_spec, type_from_name};

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

                // DECLARE SUB - forward declaration of a subroutine
                StatementKind::DeclareSub { name, params } => {
                    self.register_declared_sub(name, params, stmt.span);
                }

                // DECLARE FUNCTION - forward declaration of a function
                StatementKind::DeclareFunction { name, params } => {
                    self.register_declared_function(name, params, stmt.span);
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
                    is_optional: false,
                    is_array: p.is_array,
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

        // Use allow_redef since a DECLARE SUB may have come first
        if let Err(existing) = self.symbols.define_procedure_allow_redef(entry) {
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
                    is_optional: false,
                    is_array: p.is_array,
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

        // Use allow_redef since a DECLARE FUNCTION may have come first
        if let Err(existing) = self.symbols.define_procedure_allow_redef(entry) {
            self.errors.push(SemanticError::DuplicateProcedure {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }
    }

    /// Registers a DECLARE SUB in the symbol table.
    ///
    /// This handles forward declarations from `DECLARE SUB name (params)`.
    /// The params come as `DeclareParam` which has string type names.
    fn register_declared_sub(
        &mut self,
        name: &str,
        params: &[crate::ast::DeclareParam],
        span: crate::ast::Span,
    ) {
        let param_infos: Vec<ParameterInfo> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .param_type
                    .as_ref()
                    .and_then(|t| type_from_name(t))
                    .or_else(|| types::type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                ParameterInfo {
                    name: p.name.clone(),
                    basic_type,
                    by_val: false, // DECLARE doesn't specify BYVAL
                    is_optional: false,
                    is_array: p.is_array,
                }
            })
            .collect();

        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::Sub,
            params: param_infos,
            return_type: None,
            span,
            is_static: false,
        };

        // For DECLARE, we don't error on duplicates - the actual definition
        // will be registered later and should match
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a DECLARE FUNCTION in the symbol table.
    ///
    /// This handles forward declarations from `DECLARE FUNCTION name (params)`.
    fn register_declared_function(
        &mut self,
        name: &str,
        params: &[crate::ast::DeclareParam],
        span: crate::ast::Span,
    ) {
        let param_infos: Vec<ParameterInfo> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .param_type
                    .as_ref()
                    .and_then(|t| type_from_name(t))
                    .or_else(|| types::type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                ParameterInfo {
                    name: p.name.clone(),
                    basic_type,
                    by_val: false,
                    is_optional: false,
                    is_array: p.is_array,
                }
            })
            .collect();

        // Return type from name suffix or default
        let ret_type =
            types::type_from_suffix(name).unwrap_or_else(|| self.symbols.default_type_for(name));

        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::Function,
            params: param_infos,
            return_type: Some(ret_type),
            span,
            is_static: false,
        };

        // For DECLARE, we don't error on duplicates
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers all built-in functions.
    fn register_builtins(&mut self) {
        // Register built-in constants first
        self.register_builtin_constants();

        // String/UDT functions
        // Note: Using Long for integer parameters since integer literals default to Long in QB64
        // LEN() can return the length of a string OR the size of a UDT/fixed-length type
        self.register_builtin_function("LEN", &[("s", BasicType::Unknown)], BasicType::Long);
        self.register_builtin_function("CHR$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("ASC", &[("s", BasicType::String)], BasicType::Long);
        self.register_builtin_function(
            "LEFT$",
            &[("s", BasicType::String), ("n", BasicType::Long)],
            BasicType::String,
        );
        self.register_builtin_function(
            "RIGHT$",
            &[("s", BasicType::String), ("n", BasicType::Long)],
            BasicType::String,
        );
        // MID$ can be called with 2 or 3 arguments: MID$(s$, start) or MID$(s$, start, len)
        self.register_builtin_function_with_optionals(
            "MID$",
            &[
                ("s", BasicType::String, false),
                ("start", BasicType::Long, false),
                ("len", BasicType::Long, true), // optional - if omitted, returns rest of string
            ],
            BasicType::String,
        );
        // INSTR can be called with 2 or 3 arguments:
        // INSTR(string, substring) - search from beginning
        // INSTR(start, string, substring) - search from position
        self.register_builtin_function_with_optionals(
            "INSTR",
            &[
                ("start_or_string", BasicType::Unknown, false), // can be Long or String
                ("string_or_find", BasicType::String, false),
                ("find", BasicType::String, true), // optional - if omitted, arg1 is string, arg2 is find
            ],
            BasicType::Long,
        );
        self.register_builtin_function("UCASE$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("LCASE$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("LTRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("RTRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("TRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("STR$", &[("n", BasicType::Double)], BasicType::String);
        self.register_builtin_function("VAL", &[("s", BasicType::String)], BasicType::Double);
        // STRING$ can take either a character code (integer) or a single-char string:
        // STRING$(n, charcode%) or STRING$(n, char$)
        // Using Unknown for the second parameter allows both
        self.register_builtin_function(
            "STRING$",
            &[("n", BasicType::Long), ("c", BasicType::Unknown)],
            BasicType::String,
        );
        self.register_builtin_function("SPACE$", &[("n", BasicType::Long)], BasicType::String);

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
        // RND can be called with 0 or 1 arguments: RND or RND(n)
        // RND with no args or RND(1) returns next random number
        // RND(0) returns the last random number generated
        // RND(negative) reseeds the generator
        self.register_builtin_function_with_optionals(
            "RND",
            &[("n", BasicType::Single, true)], // optional seed/mode parameter
            BasicType::Single,
        );

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

        // Hyperbolic functions
        self.register_builtin_function("_SINH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_COSH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_TANH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ASINH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ACOSH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ATANH", &[("n", BasicType::Double)], BasicType::Double);

        // Reciprocal trig functions (sec, csc, cot)
        self.register_builtin_function("_SEC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_CSC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_COT", &[("n", BasicType::Double)], BasicType::Double);

        // Hyperbolic reciprocals
        self.register_builtin_function("_SECH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_CSCH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_COTH", &[("n", BasicType::Double)], BasicType::Double);

        // Inverse reciprocal trig (arcsec, arccsc, arccot)
        self.register_builtin_function("_ARCSEC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCSC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCOT", &[("n", BasicType::Double)], BasicType::Double);

        // Inverse hyperbolic reciprocals
        self.register_builtin_function("_ARCSECH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCSCH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCOTH", &[("n", BasicType::Double)], BasicType::Double);

        // Angle conversions (degrees <-> radians)
        self.register_builtin_function(
            "_D2R",
            &[("degrees", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_R2D",
            &[("radians", BasicType::Double)],
            BasicType::Double,
        );

        // Gradian conversions
        self.register_builtin_function(
            "_D2G",
            &[("degrees", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_G2D",
            &[("gradians", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_G2R",
            &[("gradians", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_R2G",
            &[("radians", BasicType::Double)],
            BasicType::Double,
        );

        // Negate
        self.register_builtin_function("_NEGATE", &[("n", BasicType::Double)], BasicType::Double);

        // String comparison
        self.register_builtin_function(
            "_STRCMP",
            &[("a", BasicType::String), ("b", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_STRICMP",
            &[("a", BasicType::String), ("b", BasicType::String)],
            BasicType::Long,
        );

        // Bitwise operations
        self.register_builtin_function(
            "_SHL",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_SHR",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_ROL",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_ROR",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_READBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_SETBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_RESETBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_TOGGLEBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );

        // Type conversion
        self.register_builtin_function("HEX$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("OCT$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("_BIN$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("_TOSTR$", &[("n", BasicType::Double)], BasicType::String);

        // Inline conditional
        self.register_builtin_function(
            "_IIF",
            &[
                ("cond", BasicType::Long),
                ("true_val", BasicType::Double),
                ("false_val", BasicType::Double),
            ],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_IIF$",
            &[
                ("cond", BasicType::Long),
                ("true_val", BasicType::String),
                ("false_val", BasicType::String),
            ],
            BasicType::String,
        );

        // Array functions
        self.register_builtin_function("LBOUND", &[("arr", BasicType::Unknown)], BasicType::Long);
        self.register_builtin_function("UBOUND", &[("arr", BasicType::Unknown)], BasicType::Long);

        // Timer/Date
        self.register_builtin_function("TIMER", &[], BasicType::Single);
        self.register_builtin_function("DATE$", &[], BasicType::String);
        self.register_builtin_function("TIME$", &[], BasicType::String);

        // Print formatting functions
        // Use Long for parameters since integer literals default to Long
        self.register_builtin_function("TAB", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("SPC", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("POS", &[("n", BasicType::Long)], BasicType::Integer);
        self.register_builtin_function("CSRLIN", &[], BasicType::Integer);

        // SCREEN function - read character/attribute from text screen
        // SCREEN(row, col) - returns ASCII value of character
        // SCREEN(row, col, flag) - if flag<>0, returns color attribute instead
        self.register_builtin_function_with_optionals(
            "SCREEN",
            &[
                ("row", BasicType::Integer, false),
                ("col", BasicType::Integer, false),
                ("flag", BasicType::Integer, true), // optional - if non-zero, return color attribute
            ],
            BasicType::Integer,
        );

        // File I/O functions
        self.register_builtin_function("EOF", &[("fnum", BasicType::Integer)], BasicType::Integer);
        self.register_builtin_function("LOF", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("LOC", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("SEEK", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("FREEFILE", &[], BasicType::Integer);

        // Keyboard input functions
        self.register_builtin_function("INKEY$", &[], BasicType::String);
        // INPUT$(n) - read n chars from keyboard
        // INPUT$(n, filenum) - read n chars from file
        self.register_builtin_function_with_optionals(
            "INPUT$",
            &[
                ("n", BasicType::Integer, false),
                ("filenum", BasicType::Integer, true),
            ],
            BasicType::String,
        );

        // QB64 keyboard extensions
        self.register_builtin_function("_KEYHIT", &[], BasicType::Long);
        self.register_builtin_function("_KEYDOWN", &[("code", BasicType::Long)], BasicType::Long);
        self.register_builtin_function("_CINP", &[], BasicType::Long);
        // _KEYCLEAR is a statement, not a function - handled separately

        // Lock key state functions
        self.register_builtin_function("_CAPSLOCK", &[], BasicType::Long);
        self.register_builtin_function("_NUMLOCK", &[], BasicType::Long);
        self.register_builtin_function("_SCROLLLOCK", &[], BasicType::Long);

        // Error handling functions
        self.register_builtin_function("ERR", &[], BasicType::Integer);
        self.register_builtin_function("ERL", &[], BasicType::Integer);
        // QB64 error handling extensions
        self.register_builtin_function("_ERRORLINE", &[], BasicType::Long);
        self.register_builtin_function("_ERRORMESSAGE$", &[], BasicType::String);

        // Utility functions
        self.register_builtin_function("_COMMANDCOUNT", &[], BasicType::Long);
        self.register_builtin_function("_ENVIRONCOUNT", &[], BasicType::Long);

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

        // Phase 2: String Enhancements
        self.register_builtin_function(
            "_INSTRREV",
            &[("source", BasicType::String), ("search", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function("_TRIM$", &[("s", BasicType::String)], BasicType::String);

        // Binary string packing/unpacking functions
        self.register_builtin_function("MKI$", &[("n", BasicType::Integer)], BasicType::String);
        self.register_builtin_function("MKL$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("MKS$", &[("n", BasicType::Single)], BasicType::String);
        self.register_builtin_function("MKD$", &[("n", BasicType::Double)], BasicType::String);
        self.register_builtin_function("CVI", &[("s", BasicType::String)], BasicType::Integer);
        self.register_builtin_function("CVL", &[("s", BasicType::String)], BasicType::Long);
        self.register_builtin_function("CVS", &[("s", BasicType::String)], BasicType::Single);
        self.register_builtin_function("CVD", &[("s", BasicType::String)], BasicType::Double);

        // Phase 2: QB64 Date/Time enhancements
        self.register_builtin_function("_DATE$", &[], BasicType::String);
        self.register_builtin_function("_TIME$", &[], BasicType::String);

        // Phase 2: Memory operations
        self.register_builtin_function("_MEMNEW", &[("size", BasicType::Offset)], BasicType::Mem);
        self.register_builtin_function("_MEMFREE", &[("mem", BasicType::Mem)], BasicType::Void);
        self.register_builtin_function(
            "_MEMGET",
            &[("mem", BasicType::Mem), ("offset", BasicType::Offset)],
            BasicType::Unknown, // Return type depends on context
        );
        self.register_builtin_function(
            "_MEMPUT",
            &[
                ("mem", BasicType::Mem),
                ("offset", BasicType::Offset),
                ("value", BasicType::Unknown),
            ],
            BasicType::Void,
        );
        self.register_builtin_function(
            "_MEMCOPY",
            &[
                ("source", BasicType::Mem),
                ("src_offset", BasicType::Offset),
                ("size", BasicType::Offset),
                ("dest", BasicType::Mem),
                ("dest_offset", BasicType::Offset),
            ],
            BasicType::Void,
        );
        self.register_builtin_function(
            "_MEMFILL",
            &[
                ("mem", BasicType::Mem),
                ("offset", BasicType::Offset),
                ("size", BasicType::Offset),
                ("value", BasicType::Unknown),
            ],
            BasicType::Void,
        );
        self.register_builtin_function(
            "_OFFSET",
            &[("variable", BasicType::Unknown)],
            BasicType::Offset,
        );
        self.register_builtin_function("_MEM", &[("variable", BasicType::Unknown)], BasicType::Mem);

        // Phase 5: System Integration
        self.register_builtin_function(
            "_FILEEXISTS",
            &[("path", BasicType::String)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_DIREXISTS",
            &[("path", BasicType::String)],
            BasicType::Integer,
        );
        self.register_builtin_function("_DIR$", &[("spec", BasicType::String)], BasicType::String);
        // File content helpers
        self.register_builtin_function(
            "_READFILE$",
            &[("path", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_sub(
            "_WRITEFILE",
            &[("path", BasicType::String), ("content", BasicType::String)],
        );

        // Phase 5: Mouse Input
        self.register_builtin_function("_MOUSEX", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEY", &[], BasicType::Integer);
        self.register_builtin_function(
            "_MOUSEBUTTON",
            &[("button", BasicType::Integer)],
            BasicType::Integer,
        );
        self.register_builtin_function("_MOUSEINPUT", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEMOVEMENTX", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEMOVEMENTY", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEWHEEL", &[], BasicType::Integer);

        // Phase 5: Clipboard
        self.register_builtin_function("_CLIPBOARD$", &[], BasicType::String);

        // Sound functions
        // _SNDOPEN can have optional mode/requirements string
        self.register_builtin_function_with_optionals(
            "_SNDOPEN",
            &[
                ("file", BasicType::String, false),
                ("mode", BasicType::String, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("_SNDOPENRAW", &[], BasicType::Long);
        self.register_builtin_function("_SNDCOPY", &[("handle", BasicType::Long)], BasicType::Long);
        self.register_builtin_function(
            "_SNDPLAYING",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_SNDPAUSED",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_SNDGETPOS",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_SNDLEN",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );

        // Font support
        self.register_builtin_function(
            "_LOADFONT",
            &[("file", BasicType::String), ("size", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function("_FONTHEIGHT", &[], BasicType::Long);
        self.register_builtin_function("_FONTWIDTH", &[], BasicType::Long);
        self.register_builtin_function(
            "_PRINTWIDTH",
            &[("text", BasicType::String)],
            BasicType::Long,
        ); // Get pixel width of text
        self.register_builtin_function("_FONT", &[("handle", BasicType::Long)], BasicType::Long); // Sets current font, returns previous handle
        self.register_builtin_function(
            "_FREEFONT",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );

        // Desktop/Window functions
        self.register_builtin_function("_DESKTOPWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_DESKTOPHEIGHT", &[], BasicType::Long);
        self.register_builtin_function("_SCREENX", &[], BasicType::Long);
        self.register_builtin_function("_SCREENY", &[], BasicType::Long);
        self.register_builtin_function("_TITLE$", &[], BasicType::String);
        self.register_builtin_function("_WINDOWHANDLE", &[], BasicType::Long);
        self.register_builtin_function("_WINDOWHASFOCUS", &[], BasicType::Long);

        // Window control functions (also used as statements)
        self.register_builtin_function(
            "_SCREENMOVE",
            &[("x", BasicType::Long), ("y", BasicType::Long)],
            BasicType::Long, // Returns 0 for success
        );
        self.register_builtin_function("_SCREENHIDE", &[], BasicType::Long);
        self.register_builtin_function("_SCREENSHOW", &[], BasicType::Long);
        self.register_builtin_function("_FULLSCREEN", &[], BasicType::Long); // Returns/toggles fullscreen mode
        self.register_builtin_function("_SCREENCLICK", &[], BasicType::Long);

        // Dialog boxes
        self.register_builtin_function(
            "_MESSAGEBOX",
            &[("title", BasicType::String), ("message", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_INPUTBOX$",
            &[("prompt", BasicType::String), ("title", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_OPENFILEDIALOG$",
            &[("title", BasicType::String), ("filter", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_SAVEFILEDIALOG$",
            &[("title", BasicType::String), ("filter", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_SELECTFOLDERDIALOG$",
            &[("title", BasicType::String)],
            BasicType::String,
        );

        // Phase 5: Networking
        self.register_builtin_function("_OPENHOST", &[("port", BasicType::Long)], BasicType::Long);
        self.register_builtin_function(
            "_OPENCONNECTION",
            &[("host_handle", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_OPENCLIENT",
            &[("connection_string", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_CONNECTED",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );

        // Image buffer functions
        self.register_builtin_function(
            "_NEWIMAGE",
            &[
                ("width", BasicType::Long),
                ("height", BasicType::Long),
                ("mode", BasicType::Long),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_LOADIMAGE",
            &[("filename", BasicType::String), ("mode", BasicType::Long)],
            BasicType::Long,
        );
        // Image dimension functions (take handle, return dimension)
        self.register_builtin_function("_WIDTH", &[("handle", BasicType::Long)], BasicType::Long);
        self.register_builtin_function("_HEIGHT", &[("handle", BasicType::Long)], BasicType::Long);

        // Coordinate mapping function
        // PMAP(coordinate, function_code)
        // function_code: 0 = world X to screen X, 1 = world Y to screen Y,
        //                2 = screen X to world X, 3 = screen Y to world Y
        self.register_builtin_function(
            "PMAP",
            &[
                ("coordinate", BasicType::Double),
                ("function_code", BasicType::Long),
            ],
            BasicType::Double,
        );

        // POINT function - get pixel color or cursor coordinates
        // POINT(x, y) - returns color attribute of pixel at (x, y) - returns LONG
        // POINT(function) - returns cursor coordinates:
        //   0 = current logical X, 1 = current logical Y,
        //   2 = current physical X, 3 = current physical Y
        // When called with one argument, returns Double (for world coordinates)
        // When called with two arguments, returns Long (color value)
        self.register_builtin_function_with_optionals(
            "POINT",
            &[
                ("x_or_function", BasicType::Long, false), // X coordinate or function code
                ("y", BasicType::Long, true),              // Y coordinate (optional)
            ],
            BasicType::Long, // Returns color (Long) or coordinate (also Long for physical coords)
        );

        // Print formatting functions (legacy)
        // LPOS returns the current position of the line printer
        self.register_builtin_function("LPOS", &[("n", BasicType::Long)], BasicType::Integer);

        // Event Handling Functions (QB4.5)
        // KEY(n) function - check key trap status (returns event status: -1=enabled, 0=disabled, 1=event pending)
        self.register_builtin_function("KEY", &[("n", BasicType::Long)], BasicType::Integer);

        // Joystick Functions (QB4.5)
        // STICK(n) - returns joystick position
        // n=0: returns X coordinate of joystick A (and latches Y)
        // n=1: returns Y coordinate of joystick A
        // n=2: returns X coordinate of joystick B (and latches Y)
        // n=3: returns Y coordinate of joystick B
        self.register_builtin_function("STICK", &[("n", BasicType::Long)], BasicType::Integer);
        // STRIG(n) - returns joystick trigger state
        // n=0: lower trigger A pressed since last STRIG(0)
        // n=1: lower trigger A currently pressed
        // n=2: lower trigger B pressed since last STRIG(2)
        // n=3: lower trigger B currently pressed
        // n=4: upper trigger A pressed since last STRIG(4)
        // n=5: upper trigger A currently pressed
        // n=6: upper trigger B pressed since last STRIG(6)
        // n=7: upper trigger B currently pressed
        self.register_builtin_function("STRIG", &[("n", BasicType::Long)], BasicType::Integer);

        // Memory Functions (QB4.5)
        // FRE(n) - returns free memory
        // n=-1: largest block of free string space
        // n=-2: available stack space
        // n=0 or "string": free string space
        // n=any other: far heap space (legacy, returns large number on modern systems)
        self.register_builtin_function("FRE", &[("n", BasicType::Long)], BasicType::Long);

        // PEEK(address) - reads a byte from memory address within current DEF SEG segment
        // Returns a value from 0-255. In modern QB64, this uses an emulated memory model.
        self.register_builtin_function("PEEK", &[("address", BasicType::Long)], BasicType::Integer);

        // Port I/O Functions (QB4.5 - may be sandboxed)
        // INP(port) - reads a byte from hardware I/O port
        self.register_builtin_function("INP", &[("port", BasicType::Long)], BasicType::Integer);

        // Light Pen Functions (QB4.5 legacy)
        // PEN(n) - returns light pen information (stub - returns 0)
        self.register_builtin_function("PEN", &[("n", BasicType::Long)], BasicType::Integer);

        // Serial I/O Functions (QB4.5)
        // ERDEV - returns device error code
        self.register_builtin_function("ERDEV", &[], BasicType::Integer);
        // ERDEV$ - returns device error name
        self.register_builtin_function("ERDEV$", &[], BasicType::String);
        // IOCTL$(filenum) - returns device control string from driver
        self.register_builtin_function(
            "IOCTL$",
            &[("filenum", BasicType::Long)],
            BasicType::String,
        );

        // Memory/Legacy functions
        // VARPTR returns the offset address of a variable within its segment
        self.register_builtin_function(
            "VARPTR",
            &[("variable", BasicType::Unknown)],
            BasicType::Long,
        );
        // VARPTR$ returns a binary string representation of a variable's address
        self.register_builtin_function(
            "VARPTR$",
            &[("variable", BasicType::Unknown)],
            BasicType::String,
        );
        // VARSEG returns the segment address of a variable (returns 0 in flat memory model)
        self.register_builtin_function(
            "VARSEG",
            &[("variable", BasicType::Unknown)],
            BasicType::Long,
        );
        // SADD returns the address of a string's data
        self.register_builtin_function("SADD", &[("s", BasicType::String)], BasicType::Long);

        // File System functions
        // FILEATTR returns file mode or handle attributes
        self.register_builtin_function(
            "FILEATTR",
            &[
                ("filenum", BasicType::Integer),
                ("attribute", BasicType::Integer),
            ],
            BasicType::Integer,
        );

        // Type Conversion (Microsoft Binary Format)
        // CVSMBF converts a 4-byte MBF string to a SINGLE
        self.register_builtin_function("CVSMBF", &[("s", BasicType::String)], BasicType::Single);
        // CVDMBF converts an 8-byte MBF string to a DOUBLE
        self.register_builtin_function("CVDMBF", &[("s", BasicType::String)], BasicType::Double);
        // MKSMBF$ converts a SINGLE to a 4-byte MBF string
        self.register_builtin_function("MKSMBF$", &[("n", BasicType::Single)], BasicType::String);
        // MKDMBF$ converts a DOUBLE to an 8-byte MBF string
        self.register_builtin_function("MKDMBF$", &[("n", BasicType::Double)], BasicType::String);

        // QB64 Audio Functions
        // _SNDOPEN loads a sound file and returns a handle
        // Optional second param: mode ("SYNC", "STREAM", "VOL", "PAUSE", "NODECODE")
        self.register_builtin_function_with_optionals(
            "_SNDOPEN",
            &[
                ("filename", BasicType::String, false),
                ("mode", BasicType::String, true),
            ],
            BasicType::Long,
        );
        // _SNDOPENRAW opens a raw sound buffer for audio output
        self.register_builtin_function("_SNDOPENRAW", &[], BasicType::Long);
        // _SNDCOPY creates a copy of a sound handle for independent playback
        self.register_builtin_function("_SNDCOPY", &[("handle", BasicType::Long)], BasicType::Long);
        // _SNDPLAYING returns -1 if sound is currently playing, 0 otherwise
        self.register_builtin_function(
            "_SNDPLAYING",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        // _SNDPAUSED returns -1 if sound is paused, 0 otherwise
        self.register_builtin_function(
            "_SNDPAUSED",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        // _SNDGETPOS returns the current playback position in seconds
        self.register_builtin_function(
            "_SNDGETPOS",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );
        // _SNDLEN returns the total length of the sound in seconds
        self.register_builtin_function(
            "_SNDLEN",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );
        // _SNDRATE returns the sample rate of a sound (usually 44100)
        self.register_builtin_function("_SNDRATE", &[("handle", BasicType::Long)], BasicType::Long);
        // _SNDRAWLEN returns the amount of queued raw sound data in seconds
        self.register_builtin_function("_SNDRAWLEN", &[], BasicType::Double);
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
                    is_optional: false,
                    is_array: false,
                })
                .collect(),
            return_type: Some(return_type),
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in function with optional parameters.
    /// Parameters are specified as (name, type, is_optional).
    fn register_builtin_function_with_optionals(
        &mut self,
        name: &str,
        params: &[(&str, BasicType, bool)],
        return_type: BasicType,
    ) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t, opt)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                    is_optional: *opt,
                    is_array: false,
                })
                .collect(),
            return_type: Some(return_type),
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in SUB (procedure with no return value).
    fn register_builtin_sub(&mut self, name: &str, params: &[(&str, BasicType)]) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                    is_optional: false,
                    is_array: false,
                })
                .collect(),
            return_type: None, // SUBs have no return type
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers built-in constants (_TRUE, _FALSE, etc.).
    ///
    /// In QB64, _TRUE is -1 and _FALSE is 0 (following BASIC tradition where
    /// boolean true is all bits set, i.e., -1 in two's complement).
    fn register_builtin_constants(&mut self) {
        use symbols::{ConstValue, Symbol, SymbolKind};

        // _TRUE = -1 (all bits set, standard BASIC convention)
        let true_symbol = Symbol {
            name: "_TRUE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(-1),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(true_symbol);

        // _FALSE = 0
        let false_symbol = Symbol {
            name: "_FALSE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(0),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(false_symbol);

        // Register platform/architecture constants for conditional compilation
        // These follow BASIC convention: -1 for true, 0 for false
        self.register_platform_constants();

        // Register standard QB error code constants
        // These match the ERR values returned by the ERR function
        self.register_error_constants();
    }

    /// Registers standard QBasic/QB64 error code constants.
    ///
    /// These constants correspond to the ERR values returned when runtime errors occur.
    /// Using named constants improves code readability over magic numbers.
    fn register_error_constants(&mut self) {
        use symbols::{ConstValue, Symbol, SymbolKind};

        // Helper to create an error constant
        let mut define_error = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // Standard QBasic/QB64 error codes (ERR values)
        // See: https://qb64phoenix.com/qb64wiki/index.php/ERROR_Codes

        // Core runtime errors (1-20)
        define_error("_ERR_NEXT_WITHOUT_FOR", 1);
        define_error("_ERR_SYNTAX_ERROR", 2);
        define_error("_ERR_RETURN_WITHOUT_GOSUB", 3);
        define_error("_ERR_OUT_OF_DATA", 4);
        define_error("_ERR_ILLEGAL_FUNCTION_CALL", 5);
        define_error("_ERR_OVERFLOW", 6);
        define_error("_ERR_OUT_OF_MEMORY", 7);
        define_error("_ERR_LABEL_NOT_DEFINED", 8);
        define_error("_ERR_SUBSCRIPT_OUT_OF_RANGE", 9);
        define_error("_ERR_DUPLICATE_DEFINITION", 10);
        define_error("_ERR_DIVISION_BY_ZERO", 11);
        define_error("_ERR_ILLEGAL_IN_DIRECT_MODE", 12);
        define_error("_ERR_TYPE_MISMATCH", 13);
        define_error("_ERR_OUT_OF_STRING_SPACE", 14);
        // Note: 15 is not used in standard QBasic
        define_error("_ERR_STRING_FORMULA_TOO_COMPLEX", 16);
        define_error("_ERR_CANNOT_CONTINUE", 17);
        define_error("_ERR_FUNCTION_NOT_DEFINED", 18);
        define_error("_ERR_NO_RESUME", 19);
        define_error("_ERR_RESUME_WITHOUT_ERROR", 20);

        // Device/timeout errors (24-27)
        define_error("_ERR_DEVICE_TIMEOUT", 24);
        define_error("_ERR_DEVICE_FAULT", 25);
        define_error("_ERR_FOR_WITHOUT_NEXT", 26);
        define_error("_ERR_OUT_OF_PAPER", 27);

        // Additional runtime errors (29-40)
        define_error("_ERR_WHILE_WITHOUT_WEND", 29);
        define_error("_ERR_WEND_WITHOUT_WHILE", 30);
        define_error("_ERR_DUPLICATE_LABEL", 33);
        define_error("_ERR_SUBPROGRAM_NOT_DEFINED", 35);
        define_error("_ERR_ARGUMENT_COUNT_MISMATCH", 37);
        define_error("_ERR_ARRAY_NOT_DEFINED", 38);
        define_error("_ERR_VARIABLE_REQUIRED", 40);

        // File I/O errors (50-76)
        define_error("_ERR_FIELD_OVERFLOW", 50);
        define_error("_ERR_INTERNAL_ERROR", 51);
        define_error("_ERR_BAD_FILE_NAME_OR_NUMBER", 52);
        define_error("_ERR_FILE_NOT_FOUND", 53);
        define_error("_ERR_BAD_FILE_MODE", 54);
        define_error("_ERR_FILE_ALREADY_OPEN", 55);
        define_error("_ERR_FIELD_STATEMENT_ACTIVE", 56);
        define_error("_ERR_DEVICE_IO_ERROR", 57);
        define_error("_ERR_FILE_ALREADY_EXISTS", 58);
        define_error("_ERR_BAD_RECORD_LENGTH", 59);
        define_error("_ERR_DISK_FULL", 61);
        define_error("_ERR_INPUT_PAST_END_OF_FILE", 62);
        define_error("_ERR_BAD_RECORD_NUMBER", 63);
        define_error("_ERR_BAD_FILE_NAME", 64);
        define_error("_ERR_TOO_MANY_FILES", 67);
        define_error("_ERR_DEVICE_UNAVAILABLE", 68);
        define_error("_ERR_COMM_BUFFER_OVERFLOW", 69);
        define_error("_ERR_PERMISSION_DENIED", 70);
        define_error("_ERR_DISK_NOT_READY", 71);
        define_error("_ERR_DISK_MEDIA_ERROR", 72);
        define_error("_ERR_FEATURE_UNAVAILABLE", 73);
        define_error("_ERR_RENAME_ACROSS_DISKS", 74);
        define_error("_ERR_PATH_FILE_ACCESS_ERROR", 75);
        define_error("_ERR_PATH_NOT_FOUND", 76);
    }

    /// Registers platform and architecture constants for conditional compilation.
    ///
    /// These constants allow BASIC code to use `$IF WIN THEN` style conditional
    /// compilation. Values follow BASIC convention: -1 for true, 0 for false.
    ///
    /// Constants registered:
    /// - `WIN` / `WINDOWS` - True on Windows
    /// - `LINUX` - True on Linux
    /// - `MAC` - True on macOS
    /// - `32BIT` - True on 32-bit architecture
    /// - `64BIT` - True on 64-bit architecture
    fn register_platform_constants(&mut self) {
        use symbols::{ConstValue, Symbol, SymbolKind};

        // Helper to create a platform constant (-1 for true, 0 for false)
        let mut define_platform = |name: &str, is_true: bool| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(if is_true { -1 } else { 0 }),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // Operating system constants (QB64 convention: underscore prefix)
        // These are determined at compile time based on the host platform
        define_platform("_WIN", cfg!(target_os = "windows"));
        define_platform(
            "_WIN64",
            cfg!(target_os = "windows") && cfg!(target_pointer_width = "64"),
        );
        define_platform("_WINDOWS", cfg!(target_os = "windows"));
        define_platform("_LINUX", cfg!(target_os = "linux"));
        define_platform("_MAC", cfg!(target_os = "macos"));

        // Architecture constants
        // 32-bit vs 64-bit based on pointer width
        define_platform("_32BIT", cfg!(target_pointer_width = "32"));
        define_platform("_64BIT", cfg!(target_pointer_width = "64"));
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
    fn test_builtin_constants_exist() {
        use symbols::{ConstValue, SymbolKind};

        let analyzer = SemanticAnalyzer::new();

        // Check _TRUE constant exists and has correct value (-1)
        let true_sym = analyzer.symbols.lookup_symbol("_TRUE");
        assert!(
            true_sym.is_some(),
            "_TRUE should exist as built-in constant"
        );
        let true_sym = true_sym.unwrap();
        assert!(!true_sym.is_mutable, "_TRUE should be immutable");
        match &true_sym.kind {
            SymbolKind::Constant { value } => match value {
                ConstValue::Integer(v) => assert_eq!(*v, -1, "_TRUE should be -1"),
                _ => panic!("_TRUE should be an integer constant"),
            },
            _ => panic!("_TRUE should be a constant"),
        }

        // Check _FALSE constant exists and has correct value (0)
        let false_sym = analyzer.symbols.lookup_symbol("_FALSE");
        assert!(
            false_sym.is_some(),
            "_FALSE should exist as built-in constant"
        );
        let false_sym = false_sym.unwrap();
        assert!(!false_sym.is_mutable, "_FALSE should be immutable");
        match &false_sym.kind {
            SymbolKind::Constant { value } => match value {
                ConstValue::Integer(v) => assert_eq!(*v, 0, "_FALSE should be 0"),
                _ => panic!("_FALSE should be an integer constant"),
            },
            _ => panic!("_FALSE should be a constant"),
        }

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
            let sym = sym.unwrap();
            assert!(!sym.is_mutable, "{} should be immutable", name);
            match &sym.kind {
                SymbolKind::Constant { value } => match value {
                    ConstValue::Integer(v) => {
                        assert_eq!(*v, expected_value, "{} should be {}", name, expected_value)
                    }
                    _ => panic!("{} should be an integer constant", name),
                },
                _ => panic!("{} should be a constant", name),
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
                    }],
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
