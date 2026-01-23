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
pub use symbols::{
    ParameterInfo, ProcedureEntry, ProcedureKind, ScopeKind, Symbol, SymbolKind, SymbolTable,
    UserTypeDefinition,
};
pub use typed_ir::TypedProgram;
pub use types::BasicType;

use crate::ast::{Program, Span, Statement, StatementKind};
use checker::TypeChecker;
use types::{from_type_spec, type_from_name};

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

    /// Pass 1: Collects all declarations without analyzing bodies.
    ///
    /// This enables forward references - procedures and labels can be used
    /// before their definitions appear in the source.
    ///
    /// This method uses a two-pass approach:
    /// 1. First, process DEFTYPE statements (DEFINT, DEFLNG, etc.) to set default types
    /// 2. Then, collect SUB/FUNCTION definitions with correct parameter types
    fn collect_declarations(&mut self, statements: &[Statement]) {
        // Pass 1a: Process DEFTYPE statements first so default types are set
        // before we register procedures (which need correct default types for params)
        self.collect_deftype_declarations(statements);

        // Pass 1b: Now collect procedure declarations with correct types
        self.collect_procedure_declarations(statements);
    }

    /// Collects DEFTYPE statements (DEFINT, DEFLNG, DEFSNG, DEFDBL, DEFSTR, _DEFINE).
    ///
    /// These must be processed before procedure declarations because they affect
    /// the default type of untyped parameters.
    fn collect_deftype_declarations(&mut self, statements: &[Statement]) {
        for stmt in statements {
            match &stmt.kind {
                StatementKind::DefType { type_kind, ranges } => {
                    use crate::ast::DefTypeKind;
                    let basic_type = match type_kind {
                        DefTypeKind::Integer => BasicType::Integer,
                        DefTypeKind::Long => BasicType::Long,
                        DefTypeKind::Single => BasicType::Single,
                        DefTypeKind::Double => BasicType::Double,
                        DefTypeKind::String => BasicType::String,
                    };
                    for &(start, end) in ranges {
                        self.symbols
                            .set_default_type(start, end, basic_type.clone());
                    }
                }
                StatementKind::Define { type_spec, ranges } => {
                    // Handle _DEFINE A-Z AS type
                    let upper = type_spec.to_uppercase();
                    let (is_unsigned, base) = if upper.starts_with("_UNSIGNED ") {
                        (true, upper.trim_start_matches("_UNSIGNED "))
                    } else {
                        (false, upper.as_str())
                    };

                    let base_type = match base {
                        "INTEGER" => BasicType::Integer,
                        "LONG" => BasicType::Long,
                        "SINGLE" => BasicType::Single,
                        "DOUBLE" => BasicType::Double,
                        "STRING" => BasicType::String,
                        "_BYTE" => BasicType::Byte,
                        "_BIT" => BasicType::Bit,
                        "_INTEGER64" => BasicType::Integer64,
                        "_FLOAT" => BasicType::Float,
                        "_OFFSET" => BasicType::Offset,
                        _ => BasicType::Single, // fallback
                    };

                    let basic_type = if is_unsigned {
                        match base_type {
                            BasicType::Bit => BasicType::UnsignedBit,
                            BasicType::Byte => BasicType::UnsignedByte,
                            BasicType::Integer => BasicType::UnsignedInteger,
                            BasicType::Long => BasicType::UnsignedLong,
                            BasicType::Integer64 => BasicType::UnsignedInteger64,
                            other => other,
                        }
                    } else {
                        base_type
                    };

                    for &(start, end) in ranges {
                        self.symbols
                            .set_default_type(start, end, basic_type.clone());
                    }
                }
                // Recursively collect from nested blocks
                StatementKind::If {
                    then_branch,
                    elseif_branches,
                    else_branch,
                    ..
                } => {
                    self.collect_deftype_declarations(then_branch);
                    for (_, branch) in elseif_branches {
                        self.collect_deftype_declarations(branch);
                    }
                    if let Some(eb) = else_branch {
                        self.collect_deftype_declarations(eb);
                    }
                }
                StatementKind::For { body, .. }
                | StatementKind::While { body, .. }
                | StatementKind::DoLoop { body, .. } => {
                    self.collect_deftype_declarations(body);
                }
                StatementKind::SelectCase {
                    cases, case_else, ..
                } => {
                    for case in cases {
                        self.collect_deftype_declarations(&case.body);
                    }
                    if let Some(ce) = case_else {
                        self.collect_deftype_declarations(ce);
                    }
                }
                _ => {}
            }
        }
    }

    /// Collects procedure and label declarations.
    fn collect_procedure_declarations(&mut self, statements: &[Statement]) {
        for stmt in statements {
            match &stmt.kind {
                StatementKind::SubDefinition {
                    name,
                    params,
                    is_static,
                    body,
                } => {
                    self.register_sub(name, params, *is_static, stmt.span);
                    // Also collect DECLARE LIBRARY inside SUB bodies (external functions
                    // declared inside a SUB are typically available program-wide in QB64)
                    self.collect_declarations(body);
                }

                StatementKind::FunctionDefinition {
                    name,
                    params,
                    return_type,
                    is_static,
                    body,
                } => {
                    self.register_function(name, params, return_type, *is_static, stmt.span);
                    // Also collect DECLARE LIBRARY inside FUNCTION bodies
                    self.collect_declarations(body);
                }

                StatementKind::Label { name } => {
                    // Silently ignore duplicate labels at the top level.
                    // This is common in QB64 codebases where $INCLUDE files may each
                    // have line numbers like `1 END` for error handling. The first
                    // definition wins, which matches GOTO behavior.
                    let _ = self.symbols.define_label(name.clone(), stmt.span);
                }

                // DECLARE SUB - forward declaration of a subroutine
                StatementKind::DeclareSub { name, params } => {
                    self.register_declared_sub(name, params, stmt.span);
                }

                // DECLARE FUNCTION - forward declaration of a function
                StatementKind::DeclareFunction { name, params, .. } => {
                    self.register_declared_function(name, params, stmt.span);
                }

                // DECLARE LIBRARY - register external functions/subs
                StatementKind::DeclareLibrary { declarations, .. } => {
                    for decl in declarations {
                        self.register_external_declaration(decl, stmt.span);
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

    /// Registers an external function/sub from DECLARE LIBRARY.
    ///
    /// This makes C library functions callable from BASIC code.
    fn register_external_declaration(
        &mut self,
        decl: &crate::ast::ExternalDeclaration,
        span: crate::ast::Span,
    ) {
        use crate::semantic::types::from_type_spec;

        let param_infos: Vec<ParameterInfo> = decl
            .params
            .iter()
            .map(|p| ParameterInfo {
                name: p.name.clone(),
                basic_type: from_type_spec(&p.type_spec),
                by_val: p.is_byval,
                is_optional: false,
                is_array: false,
            })
            .collect();

        let return_type = if decl.is_function {
            decl.return_type.as_ref().map(from_type_spec)
        } else {
            None
        };

        let entry = ProcedureEntry {
            name: decl.name.clone(),
            kind: ProcedureKind::External,
            params: param_infos,
            return_type,
            span,
            is_static: false,
        };

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
        // ASC can be called with 1 or 2 arguments:
        // ASC(s$) - returns ASCII of first character
        // ASC(s$, position%) - returns ASCII of character at position
        self.register_builtin_function_with_optionals(
            "ASC",
            &[
                ("s", BasicType::String, false),
                ("position", BasicType::Long, true),
            ],
            BasicType::Long,
        );
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
        // LBOUND/UBOUND can be called with 1 or 2 arguments:
        // LBOUND(arr) - returns lower bound of first dimension
        // LBOUND(arr, dimension) - returns lower bound of specified dimension
        self.register_builtin_function_with_optionals(
            "LBOUND",
            &[
                ("arr", BasicType::Unknown, false),
                ("dimension", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "UBOUND",
            &[
                ("arr", BasicType::Unknown, false),
                ("dimension", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // Timer/Date
        // TIMER can be called with 0 or 1 argument:
        // TIMER - returns seconds since midnight as single
        // TIMER(accuracy!) - QB64 extension with optional accuracy parameter
        self.register_builtin_function_with_optionals(
            "TIMER",
            &[("accuracy", BasicType::Single, true)],
            BasicType::Single,
        );
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
        // COMMAND$ can be called with 0 or 1 argument:
        // COMMAND$ - returns entire command line
        // COMMAND$(n) - returns nth command line argument
        self.register_builtin_function_with_optionals(
            "COMMAND$",
            &[("index", BasicType::Long, true)],
            BasicType::String,
        );
        self.register_builtin_function("_CWD$", &[], BasicType::String);
        self.register_builtin_function("_OS$", &[], BasicType::String);
        self.register_builtin_function("_STARTDIR$", &[], BasicType::String);

        // Phase 2: String Enhancements
        // _INSTRREV can be called with 2 or 3 arguments:
        // _INSTRREV(source$, search$) - search from end
        // _INSTRREV(start, source$, search$) - search from position
        self.register_builtin_function_with_optionals(
            "_INSTRREV",
            &[
                ("start_or_source", BasicType::Unknown, false), // can be Long or String
                ("source_or_search", BasicType::String, false),
                ("search", BasicType::String, true),
            ],
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

        // SHELL function form: ret% = SHELL(command$)
        // Returns the exit code of the command (0 = success)
        self.register_builtin_function("SHELL", &[("command", BasicType::String)], BasicType::Long);

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
        // _LOADFONT can be called with 2 or 3 arguments:
        // _LOADFONT(file$, size%) - load font
        // _LOADFONT(file$, size%, style$) - load font with style ("BOLD,ITALIC,etc")
        self.register_builtin_function_with_optionals(
            "_LOADFONT",
            &[
                ("file", BasicType::String, false),
                ("size", BasicType::Long, false),
                ("style", BasicType::String, true),
            ],
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
        // _TITLE as function can take an optional string argument:
        // _TITLE - no effect as expression, exists for statement dual-use
        // _TITLE(title$) - sets the window title (returns void)
        self.register_builtin_function_with_optionals(
            "_TITLE",
            &[("title", BasicType::String, true)],
            BasicType::Long, // Returns 0 as placeholder
        );
        // _ICON as function can take 0 or 1 argument:
        // _ICON - returns current icon handle
        // _ICON(handle&) - sets window icon (also a statement)
        self.register_builtin_function_with_optionals(
            "_ICON",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
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
        // _MESSAGEBOX can be called with 0-5 arguments (all optional):
        // _MESSAGEBOX() - simple message box with defaults
        // _MESSAGEBOX(title$) - with title
        // _MESSAGEBOX(title$, message$) - with message
        // _MESSAGEBOX(title$, message$, dialogType$) - with OK/Cancel etc.
        // _MESSAGEBOX(title$, message$, dialogType$, iconType$) - with icon
        // _MESSAGEBOX(title$, message$, dialogType$, iconType$, defaultButton&) - full
        self.register_builtin_function_with_optionals(
            "_MESSAGEBOX",
            &[
                ("title", BasicType::String, true),
                ("message", BasicType::String, true),
                ("dialogType", BasicType::String, true),
                ("iconType", BasicType::String, true),
                ("defaultButton", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_INPUTBOX$",
            &[("prompt", BasicType::String), ("title", BasicType::String)],
            BasicType::String,
        );
        // _OPENFILEDIALOG$ can take 2-5 arguments
        self.register_builtin_function_with_optionals(
            "_OPENFILEDIALOG$",
            &[
                ("title", BasicType::String, false),
                ("filter", BasicType::String, false),
                ("defaultDir", BasicType::String, true),
                ("defaultFile", BasicType::String, true),
                ("flags", BasicType::Long, true),
            ],
            BasicType::String,
        );
        // _SAVEFILEDIALOG$ can take 2-4 arguments
        self.register_builtin_function_with_optionals(
            "_SAVEFILEDIALOG$",
            &[
                ("title", BasicType::String, false),
                ("filter", BasicType::String, false),
                ("defaultDir", BasicType::String, true),
                ("defaultFile", BasicType::String, true),
            ],
            BasicType::String,
        );
        self.register_builtin_function(
            "_SELECTFOLDERDIALOG$",
            &[("title", BasicType::String)],
            BasicType::String,
        );

        // Phase 5: Networking
        // _OPENHOST takes a connection string like "TCP/IP:port", not a numeric port
        self.register_builtin_function(
            "_OPENHOST",
            &[("connection_string", BasicType::String)],
            BasicType::Long,
        );
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
        self.register_builtin_function(
            "_COPYIMAGE",
            &[("source", BasicType::Long), ("mode", BasicType::Long)],
            BasicType::Long,
        );
        // Image dimension functions (take handle, return dimension)
        // _WIDTH can be called with 0 or 1 argument:
        // _WIDTH - returns width of current screen/image
        // _WIDTH(handle&) - returns width of specified image
        self.register_builtin_function_with_optionals(
            "_WIDTH",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        // _HEIGHT can take 0 or 1 argument: _HEIGHT or _HEIGHT(handle)
        self.register_builtin_function_with_optionals(
            "_HEIGHT",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

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
        // _SNDRAWDONE returns -1 if raw sound buffer is empty, 0 otherwise
        self.register_builtin_function("_SNDRAWDONE", &[], BasicType::Integer);

        // ==========================================
        // QB64 Extension Functions (Session 031+)
        // ==========================================

        // Color creation functions
        // _RGB(r, g, b) or _RGB(r, g, b, handle) - creates a color value
        // _RGB32 has variants: _RGB32(i), _RGB32(i, a), _RGB32(r, g, b), _RGB32(r, g, b, a)
        self.register_builtin_function_with_optionals(
            "_RGB",
            &[
                ("red", BasicType::Long, false),
                ("green", BasicType::Long, false),
                ("blue", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // _RGB32 is complex - can take 1, 2, 3, or 4 arguments
        // For now, register the 3-4 argument variant (most common)
        self.register_builtin_function_with_optionals(
            "_RGB32",
            &[
                ("red", BasicType::Long, false),
                ("green", BasicType::Long, false),
                ("blue", BasicType::Long, false),
                ("alpha", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // _RGBA and _RGBA32 - explicit alpha channel
        self.register_builtin_function_with_optionals(
            "_RGBA",
            &[
                ("red", BasicType::Long, false),
                ("green", BasicType::Long, false),
                ("blue", BasicType::Long, false),
                ("alpha", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_RGBA32",
            &[
                ("red", BasicType::Long),
                ("green", BasicType::Long),
                ("blue", BasicType::Long),
                ("alpha", BasicType::Long),
            ],
            BasicType::Long,
        );

        // Color component extraction functions
        // _RED, _GREEN, _BLUE, _ALPHA extract color components (0-255)
        // Can take 1 or 2 arguments: _RED(color) or _RED(color, imagehandle)
        self.register_builtin_function_with_optionals(
            "_RED",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_GREEN",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_BLUE",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_ALPHA",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // 32-bit variants (same functionality, for explicitness)
        self.register_builtin_function_with_optionals(
            "_RED32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_GREEN32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_BLUE32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_ALPHA32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // _PIXELSIZE returns bytes per pixel for current screen/image
        // 0 = text mode, 1 = 256 color, 4 = 32-bit color
        self.register_builtin_function_with_optionals(
            "_PIXELSIZE",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

        // _SCREENEXISTS returns -1 if graphics window exists, 0 otherwise
        self.register_builtin_function("_SCREENEXISTS", &[], BasicType::Integer);

        // _EXIT - dual purpose:
        // - As function with 0 args: returns exit request state (non-zero if user requested exit)
        // - As statement with 1 arg: exits program with specific return code
        self.register_builtin_function_with_optionals(
            "_EXIT",
            &[("code", BasicType::Long, true)], // optional code parameter
            BasicType::Long,
        );

        // _DEFAULTCOLOR returns the default foreground color for the current _DEST
        self.register_builtin_function("_DEFAULTCOLOR", &[], BasicType::Long);
        // _BACKGROUNDCOLOR returns the background color of the current _DEST
        self.register_builtin_function("_BACKGROUNDCOLOR", &[], BasicType::Long);

        // _FULLPATH$ returns the full absolute path of a file/directory
        self.register_builtin_function(
            "_FULLPATH$",
            &[("path", BasicType::String)],
            BasicType::String,
        );

        // _FPS returns/sets frame rate limit (when called as function, returns current FPS)
        self.register_builtin_function("_FPS", &[], BasicType::Double);

        // Hash and encoding functions
        self.register_builtin_function("_CRC32", &[("data", BasicType::String)], BasicType::Long);
        self.register_builtin_function("_MD5$", &[("data", BasicType::String)], BasicType::String);
        self.register_builtin_function("_ADLER32", &[("data", BasicType::String)], BasicType::Long);

        // Base64 encoding/decoding
        self.register_builtin_function(
            "_BASE64ENCODE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_BASE64DECODE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );

        // URL encoding/decoding
        self.register_builtin_function(
            "_ENCODEURL$",
            &[("url", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_DECODEURL$",
            &[("url", BasicType::String)],
            BasicType::String,
        );

        // Compression (zlib deflate/inflate)
        self.register_builtin_function(
            "_DEFLATE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_INFLATE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );

        // Memory extended functions
        self.register_builtin_function(
            "_MEMEXISTS",
            &[("mem", BasicType::Offset)],
            BasicType::Integer,
        );

        // Default color functions
        self.register_builtin_function_with_optionals(
            "_DEFAULTCOLOR",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_BACKGROUNDCOLOR",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

        // Short-circuit logical operators (functions)
        // _ANDALSO returns second argument only if first is true
        self.register_builtin_function(
            "_ANDALSO",
            &[("a", BasicType::Long), ("b", BasicType::Long)],
            BasicType::Long,
        );
        // _ORELSE returns second argument only if first is false
        self.register_builtin_function(
            "_ORELSE",
            &[("a", BasicType::Long), ("b", BasicType::Long)],
            BasicType::Long,
        );

        // _FREETIMER - free a timer resource
        self.register_builtin_function("_FREETIMER", &[], BasicType::Long);

        // Console mode functions
        self.register_builtin_function("_CONSOLEINPUT", &[], BasicType::Long);
        self.register_builtin_function("_ECHO", &[("text", BasicType::String)], BasicType::Long);

        // Mouse extended
        self.register_builtin_function("_MOUSEHIDDEN", &[], BasicType::Integer);

        // Clipboard extended - get image from clipboard
        self.register_builtin_function("_CLIPBOARDIMAGE", &[], BasicType::Long);

        // Device input functions (gamepad/joystick)
        self.register_builtin_function("_DEVICES", &[], BasicType::Long);
        self.register_builtin_function("_DEVICE$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("_DEVICEINPUT", &[], BasicType::Long);
        self.register_builtin_function(
            "_LASTAXIS",
            &[("device", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_LASTBUTTON",
            &[("device", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_LASTWHEEL",
            &[("device", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_AXIS",
            &[("device", BasicType::Long), ("axis", BasicType::Long)],
            BasicType::Single,
        );
        self.register_builtin_function(
            "_BUTTON",
            &[("device", BasicType::Long), ("button", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_BUTTONCHANGE",
            &[("device", BasicType::Long), ("button", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_WHEEL",
            &[("device", BasicType::Long), ("wheel", BasicType::Long)],
            BasicType::Single,
        );

        // Drag and drop functions
        self.register_builtin_function("_TOTALDROPPEDFILES", &[], BasicType::Long);
        self.register_builtin_function(
            "_DROPPEDFILE",
            &[("index", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_DROPPEDFILE$",
            &[("index", BasicType::Long)],
            BasicType::String,
        );

        // Resize event functions
        self.register_builtin_function("_RESIZE", &[], BasicType::Integer);
        self.register_builtin_function("_RESIZEWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_RESIZEHEIGHT", &[], BasicType::Long);
        self.register_builtin_function("_SCALEDWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_SCALEDHEIGHT", &[], BasicType::Long);

        // Dialog extended
        self.register_builtin_function(
            "_COLORCHOOSERDIALOG",
            &[("initialColor", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_NOTIFYPOPUP",
            &[("title", BasicType::String), ("message", BasicType::String)],
            BasicType::Long,
        );

        // ==========================================
        // QB64 Extension SUBs (statements)
        // ==========================================

        // Drag and drop control
        // _ACCEPTFILEDROP can be called with 0 or 1 argument as a statement:
        // _ACCEPTFILEDROP - enable file drop with default settings
        // _ACCEPTFILEDROP ON/OFF - explicitly enable/disable
        self.register_builtin_sub_with_optionals(
            "_ACCEPTFILEDROP",
            &[("enable", BasicType::Integer, true)],
        );
        self.register_builtin_sub("_FINISHDROP", &[]);

        // Console mode statements
        self.register_builtin_sub("_CONSOLECURSOR", &[("visible", BasicType::Integer)]);
        self.register_builtin_sub(
            "_CONSOLEFONT",
            &[("font", BasicType::String), ("size", BasicType::Long)],
        );
        self.register_builtin_sub("_CONTROLCHR", &[("mode", BasicType::Integer)]);

        // Graphics alpha/blending
        self.register_builtin_sub(
            "_SETALPHA",
            &[
                ("alpha", BasicType::Long),
                ("color1", BasicType::Long),
                ("color2", BasicType::Long),
            ],
        );
        // _PALETTECOLOR SUB form is handled by the FUNCTION registration with optionals
        // (registered in the function section with 1-3 optional args)
        self.register_builtin_sub(
            "_COPYPALETTE",
            &[
                ("srcHandle", BasicType::Long),
                ("destHandle", BasicType::Long),
            ],
        );
        self.register_builtin_sub("_BLEND", &[("handle", BasicType::Long)]);
        self.register_builtin_sub("_DONTBLEND", &[("handle", BasicType::Long)]);
        self.register_builtin_sub(
            "_CLEARCOLOR",
            &[("color", BasicType::Long), ("handle", BasicType::Long)],
        );
        self.register_builtin_sub("_DEPTHBUFFER", &[("mode", BasicType::Integer)]);
        self.register_builtin_sub(
            "_DISPLAYORDER",
            &[
                ("layer1", BasicType::Long),
                ("layer2", BasicType::Long),
                ("layer3", BasicType::Long),
                ("layer4", BasicType::Long),
            ],
        );

        // Sound extended statement
        self.register_builtin_sub(
            "_SNDLIMIT",
            &[("handle", BasicType::Long), ("seconds", BasicType::Single)],
        );

        // Icon statement (set window icon)
        self.register_builtin_sub("_ICON", &[("handle", BasicType::Long)]);

        // Window visibility
        self.register_builtin_sub("_HIDE", &[]);
        self.register_builtin_sub("_SHOW", &[]);
        self.register_builtin_sub("_ONTOP", &[("mode", BasicType::Integer)]);

        // Print mode
        self.register_builtin_sub("_PRINTMODE", &[("mode", BasicType::Integer)]);

        // ==========================================
        // QB64 Extension Functions (Session 032+)
        // ==========================================

        // Error handling extended - include file info
        self.register_builtin_function("_INCLERRORFILE$", &[], BasicType::String);
        self.register_builtin_function("_INCLERRORLINE", &[], BasicType::Long);

        // Utility function - status code from last operation
        // Can take 0 or 1 argument: _STATUSCODE or _STATUSCODE(handle)
        self.register_builtin_function_with_optionals(
            "_STATUSCODE",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

        // Networking extended - connection address info
        self.register_builtin_function(
            "_CONNECTIONADDRESS",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_CONNECTIONADDRESS$",
            &[("handle", BasicType::Long)],
            BasicType::String,
        );

        // HSB color functions (Hue, Saturation, Brightness)
        // _HSB32 creates a 32-bit color from HSB values (0-360, 0-100, 0-100)
        self.register_builtin_function(
            "_HSB32",
            &[
                ("hue", BasicType::Single),
                ("saturation", BasicType::Single),
                ("brightness", BasicType::Single),
            ],
            BasicType::Long,
        );
        // _HSBA32 creates a 32-bit color from HSBA values
        self.register_builtin_function(
            "_HSBA32",
            &[
                ("hue", BasicType::Single),
                ("saturation", BasicType::Single),
                ("brightness", BasicType::Single),
                ("alpha", BasicType::Single),
            ],
            BasicType::Long,
        );
        // Extract HSB components from a color
        self.register_builtin_function("_HUE32", &[("color", BasicType::Long)], BasicType::Single);
        self.register_builtin_function(
            "_SATURATION32",
            &[("color", BasicType::Long)],
            BasicType::Single,
        );
        self.register_builtin_function(
            "_BRIGHTNESS32",
            &[("color", BasicType::Long)],
            BasicType::Single,
        );

        // Memory extended functions
        // _MEMELEMENT returns offset of array element in memory block
        self.register_builtin_function(
            "_MEMELEMENT",
            &[("mem", BasicType::Offset), ("index", BasicType::Long)],
            BasicType::Offset,
        );
        // _MEMIMAGE returns memory block for an image handle
        self.register_builtin_function(
            "_MEMIMAGE",
            &[("handle", BasicType::Long)],
            BasicType::Offset,
        );
        // _MEMSOUND returns memory block for a sound handle
        self.register_builtin_function(
            "_MEMSOUND",
            &[("handle", BasicType::Long)],
            BasicType::Offset,
        );

        // Sound extended - create new sound buffer
        self.register_builtin_function(
            "_SNDNEW",
            &[
                ("frames", BasicType::Long),
                ("channels", BasicType::Long),
                ("bits", BasicType::Long),
            ],
            BasicType::Long,
        );

        // File I/O extended - file listing iterator
        self.register_builtin_function(
            "_FILES$",
            &[("pattern", BasicType::String)],
            BasicType::String,
        );

        // Device input extended
        self.register_builtin_function("_LASTHANDLER", &[], BasicType::Long);

        // Unicode font functions
        self.register_builtin_function(
            "_UCHARPOS",
            &[("text", BasicType::String), ("pos", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_UFONTHEIGHT",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function("_ULINESPACING", &[], BasicType::Long);
        self.register_builtin_function(
            "_UPRINTWIDTH",
            &[("text", BasicType::String)],
            BasicType::Long,
        );

        // ==========================================
        // QB64 Extension SUBs (Session 032+)
        // ==========================================

        // Graphics extended - save image to file
        self.register_builtin_sub(
            "_SAVEIMAGE",
            &[("filename", BasicType::String), ("handle", BasicType::Long)],
        );

        // Graphics - print screen contents
        self.register_builtin_sub("_SCREENPRINT", &[("text", BasicType::String)]);

        // Unicode font statements
        self.register_builtin_sub(
            "_UPRINTSTRING",
            &[
                ("x", BasicType::Long),
                ("y", BasicType::Long),
                ("text", BasicType::String),
            ],
        );
        // _MAPUNICODE is dual-use:
        // - As function: _MAPUNICODE(charcode) returns Unicode codepoint (1 arg)
        // - As statement: _MAPUNICODE codepoint TO charcode (2 args, TO stripped by parser)
        // Use optional params to allow 1-2 arguments
        self.register_builtin_function_with_optionals(
            "_MAPUNICODE",
            &[
                ("charcode", BasicType::Long, false),     // required
                ("codepoint_set", BasicType::Long, true), // optional (only for statement form)
            ],
            BasicType::Long,
        );

        // Logging statements
        self.register_builtin_sub("_LOGTRACE", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGINFO", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGWARN", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGERROR", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGMINLEVEL", &[("level", BasicType::Long)]);

        // Sound extended - batch raw samples
        self.register_builtin_sub(
            "_SNDRAWBATCH",
            &[
                ("handle", BasicType::Long),
                ("samples", BasicType::Offset),
                ("count", BasicType::Long),
            ],
        );

        // MIDI soundbank
        self.register_builtin_sub("_MIDISOUNDBANK", &[("filename", BasicType::String)]);

        // Device input extended
        self.register_builtin_sub("_NEWHANDLER", &[("callback", BasicType::Offset)]);

        // ==========================================
        // QB64 Extension Functions (Session 033+)
        // ==========================================

        // File I/O extended - get embedded file data
        self.register_builtin_function(
            "_EMBEDDED$",
            &[("name", BasicType::String)],
            BasicType::String,
        );

        // Graphics rendering mode functions (return current mode when called without args)
        // These can also be used as keywords in _PUTIMAGE, but work as functions too
        self.register_builtin_function("_SMOOTH", &[], BasicType::Long);
        self.register_builtin_function("_SMOOTHSHRUNK", &[], BasicType::Long);
        self.register_builtin_function("_SMOOTHSTRETCHED", &[], BasicType::Long);
        self.register_builtin_function("_HARDWARE", &[], BasicType::Long);
        self.register_builtin_function("_HARDWARE1", &[], BasicType::Long);
        self.register_builtin_function("_SOFTWARE", &[], BasicType::Long);

        // Graphics direction constants/functions
        self.register_builtin_function("_ANTICLOCKWISE", &[], BasicType::Long);
        self.register_builtin_function("_CLOCKWISE", &[], BasicType::Long);

        // Print mode constants/functions
        self.register_builtin_function("_KEEPBACKGROUND", &[], BasicType::Long);
        self.register_builtin_function("_FILLBACKGROUND", &[], BasicType::Long);
        self.register_builtin_function("_ONLYBACKGROUND", &[], BasicType::Long);

        // Alignment constants
        self.register_builtin_function("_MIDDLE", &[], BasicType::Long);

        // Auto display mode
        self.register_builtin_function("_AUTO", &[], BasicType::Long);

        // ==========================================
        // QB64 Extension SUBs (Session 033+)
        // ==========================================

        // Graphics extended - print to image
        self.register_builtin_sub("_PRINTIMAGE", &[("handle", BasicType::Long)]);

        // Clear specific resource
        self.register_builtin_sub("_CLEAR", &[("resource", BasicType::Long)]);

        // Toggle a setting
        self.register_builtin_sub("_TOGGLE", &[("setting", BasicType::Long)]);

        // 3D triangle mapping (simplified - full version needs vertex arrays)
        self.register_builtin_sub(
            "_MAPTRIANGLE",
            &[
                ("src_x1", BasicType::Single),
                ("src_y1", BasicType::Single),
                ("src_x2", BasicType::Single),
                ("src_y2", BasicType::Single),
                ("src_x3", BasicType::Single),
                ("src_y3", BasicType::Single),
                ("dst_x1", BasicType::Single),
                ("dst_y1", BasicType::Single),
                ("dst_x2", BasicType::Single),
                ("dst_y2", BasicType::Single),
                ("dst_x3", BasicType::Single),
                ("dst_y3", BasicType::Single),
            ],
        );

        // OpenGL render mode (stub - we use SDL2)
        self.register_builtin_sub("_GLRENDER", &[("mode", BasicType::Long)]);

        // ==========================================
        // QB64 Extension Functions (Session 034+)
        // ==========================================

        // Graphics keyword constants (used as mode flags)
        self.register_builtin_function("_CLIP", &[], BasicType::Long);
        self.register_builtin_function("_STRETCH", &[], BasicType::Long);
        self.register_builtin_function("_SEAMLESS", &[], BasicType::Long);
        self.register_builtin_function("_SQUAREPIXELS", &[], BasicType::Long);
        self.register_builtin_function("_BEHIND", &[], BasicType::Long);

        // Type/mode keywords as functions
        self.register_builtin_function("_ALL", &[], BasicType::Long);
        self.register_builtin_function("_BLINK", &[], BasicType::Long);
        self.register_builtin_function("_OFF", &[], BasicType::Long);
        self.register_builtin_function("_ONLY", &[], BasicType::Long);

        // Sound keyword
        self.register_builtin_function("_WAVE", &[], BasicType::Long);

        // Network keyword
        self.register_builtin_function("_DONTWAIT", &[], BasicType::Long);

        // Console functions
        self.register_builtin_function("_CONSOLEINPUT", &[], BasicType::Long);
        self.register_builtin_function("_CINP", &[], BasicType::Long);
        self.register_builtin_function("_CONSOLETITLE$", &[], BasicType::String);
        self.register_builtin_function("_CONSOLE", &[], BasicType::Long);

        // Environment functions
        // _SHELLHIDE is both a statement and a function; as function takes command, returns exit code
        self.register_builtin_function(
            "_SHELLHIDE",
            &[("command", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function("_STARTDIR$", &[], BasicType::String);
        // _ACCEPTFILEDROP can be called with 0 or 1 argument:
        // _ACCEPTFILEDROP - returns whether drop was accepted
        // _ACCEPTFILEDROP(mode) - QB64 extension (enable=ON/OFF)
        self.register_builtin_function_with_optionals(
            "_ACCEPTFILEDROP",
            &[("mode", BasicType::Long, true)],
            BasicType::Long,
        );
        self.register_builtin_function("_TOTALDROPPEDFILES", &[], BasicType::Long);
        self.register_builtin_function("_DROPPEDFILE$", &[], BasicType::String);
        self.register_builtin_function(
            "_DROPPEDFILE",
            &[("index", BasicType::Long)],
            BasicType::String,
        );
        self.register_builtin_function("_FINISHDROP", &[], BasicType::Long);

        // Graphics info functions
        self.register_builtin_function("_DEPTHBUFFER", &[], BasicType::Long);
        self.register_builtin_function("_ANTIALIASING", &[], BasicType::Long);
        self.register_builtin_function("_DISPLAYORDER", &[], BasicType::Long);
        self.register_builtin_function("_GLCOMPAT", &[], BasicType::Long);

        // Debug/assert functions
        self.register_builtin_function(
            "_ASSERT",
            &[("condition", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function("_ASSERTERROR$", &[], BasicType::String);

        // Input functions
        self.register_builtin_function(
            "_KEYDOWN",
            &[("keycode", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function("_KEYHIT", &[], BasicType::Long);
        self.register_builtin_function("_KEYCLEAR", &[], BasicType::Long);

        // Clipboard functions
        self.register_builtin_function("_CLIPBOARD$", &[], BasicType::String);
        self.register_builtin_function("_CLIPBOARDIMAGE", &[], BasicType::Long);

        // Display extended functions
        self.register_builtin_function("_FULLSCREENSMOOTH", &[], BasicType::Long);
        self.register_builtin_function("_ALLOWFULLSCREEN", &[], BasicType::Long);
        self.register_builtin_function("_DISPLAYWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_DISPLAYHEIGHT", &[], BasicType::Long);

        // Color utility functions
        // _PALETTECOLOR can be called with 1-3 arguments (as function):
        // _PALETTECOLOR(attr) - get palette color from current screen
        // _PALETTECOLOR(attr, handle&) - get palette color from specified image
        // _PALETTECOLOR(attr, value, handle&) - set palette color (also a statement)
        self.register_builtin_function_with_optionals(
            "_PALETTECOLOR",
            &[
                ("attr", BasicType::Long, false),
                ("value_or_handle", BasicType::Long, true),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("_DEFAULTCOLOR", &[], BasicType::Long);
        self.register_builtin_function("_BACKGROUNDCOLOR", &[], BasicType::Long);

        // Timer extended functions
        self.register_builtin_function("_FREETIMER", &[], BasicType::Long);

        // ==========================================
        // QB64 Extension SUBs (Session 034+)
        // Note: Only new SUBs not already registered earlier
        // ==========================================

        // Console statements (NEW)
        self.register_builtin_sub("_ECHO", &[("text", BasicType::String)]);
        self.register_builtin_sub("_CONSOLETITLE", &[("title", BasicType::String)]);

        // Clipboard write statement (NEW - _CLIPBOARD$ for read is a function)
        self.register_builtin_sub("_CLIPBOARD", &[("text", BasicType::String)]);

        // Delay/timing statements (NEW)
        self.register_builtin_sub("_DELAY", &[("seconds", BasicType::Single)]);

        // ==========================================
        // QB64 Extension Functions (Session 035+)
        // ==========================================

        // Memory block functions (Session 035)
        self.register_builtin_function(
            "_MEMGET",
            &[("block", BasicType::Offset), ("offset", BasicType::Offset)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_MEMEXISTS",
            &[("block", BasicType::Offset)],
            BasicType::Long,
        );

        // String utility functions (QB64 underscore versions - Session 035)
        self.register_builtin_function("_TRIM$", &[("text", BasicType::String)], BasicType::String);

        // Console extended (Session 035)
        self.register_builtin_function("_SCREENBUFFER", &[], BasicType::Long);
        self.register_builtin_function("_SCINKEY$", &[], BasicType::String);

        // Date/time extended (Session 035)
        self.register_builtin_function("_YEAR", &[], BasicType::Long);
        self.register_builtin_function("_MONTH", &[], BasicType::Long);
        self.register_builtin_function("_DAY", &[], BasicType::Long);
        self.register_builtin_function("_WEEKDAY", &[], BasicType::Long);
        self.register_builtin_function("_HOUR", &[], BasicType::Long);
        self.register_builtin_function("_MINUTE", &[], BasicType::Long);
        self.register_builtin_function("_SECOND", &[], BasicType::Long);

        // Image functions (Session 035)
        self.register_builtin_function(
            "_PIXELSIZE",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );

        // Window icon state (Session 035)
        self.register_builtin_function("_SCREENICON", &[], BasicType::Long);

        // ==========================================
        // QB64 Extension SUBs (Session 035+)
        // ==========================================

        // Memory statements
        self.register_builtin_sub(
            "_MEMPUT",
            &[
                ("block", BasicType::Offset),
                ("offset", BasicType::Offset),
                ("value", BasicType::Long),
            ],
        );
        self.register_builtin_sub(
            "_MEMFILL",
            &[
                ("block", BasicType::Offset),
                ("offset", BasicType::Offset),
                ("size", BasicType::Long),
                ("value", BasicType::Long),
            ],
        );
        self.register_builtin_sub(
            "_MEMCOPY",
            &[
                ("src", BasicType::Offset),
                ("srcoff", BasicType::Offset),
                ("bytes", BasicType::Long),
                ("dst", BasicType::Offset),
                ("dstoff", BasicType::Offset),
            ],
        );
        self.register_builtin_sub("_MEMFREE", &[("block", BasicType::Offset)]);

        // Window icon statement (Session 035)
        self.register_builtin_sub("_SCREENICON", &[]);
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

    /// Registers a built-in SUB with optional parameters.
    fn register_builtin_sub_with_optionals(
        &mut self,
        name: &str,
        params: &[(&str, BasicType, bool)],
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

        // _NONE = 0 (null/none value for handles, modes, etc.)
        let none_symbol = Symbol {
            name: "_NONE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(0),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(none_symbol);

        // Register platform/architecture constants for conditional compilation
        // These follow BASIC convention: -1 for true, 0 for false
        self.register_platform_constants();

        // Register standard QB error code constants
        // These match the ERR values returned by the ERR function
        self.register_error_constants();

        // Register character constant strings
        self.register_character_constants();

        // Register keyboard scan code constants
        self.register_keyboard_constants();
    }

    /// Registers QB64 character constant strings (_CHR_CR, _CHR_LF, etc.).
    ///
    /// These are single-character string constants for common control characters.
    fn register_character_constants(&mut self) {
        use symbols::{ConstValue, Symbol, SymbolKind};

        let char_constants = [
            ("_CHR_CR", "\r"),     // Carriage return (ASCII 13)
            ("_CHR_LF", "\n"),     // Line feed (ASCII 10)
            ("_CHR_SUB", "\x1A"),  // Substitute/EOF (ASCII 26)
            ("_CHR_QUOTE", "\""),  // Double quote (ASCII 34)
            ("_CHR_TAB", "\t"),    // Tab (ASCII 9)
            ("_CHR_HT", "\t"),     // Horizontal Tab (alias for _CHR_TAB)
            ("_CHR_ESC", "\x1B"),  // Escape (ASCII 27)
            ("_CHR_BELL", "\x07"), // Bell (ASCII 7)
            ("_CHR_BS", "\x08"),   // Backspace (ASCII 8)
            ("_CHR_NUL", "\x00"),  // Null (ASCII 0)
            // Multi-character string constants
            ("_STR_CRLF", "\r\n"), // Windows line ending (CR+LF)
            ("_STR_LF", "\n"),     // Unix line ending (LF only)
            ("_STR_CR", "\r"),     // Classic Mac line ending (CR only)
            ("_STR_EMPTY", ""),    // Empty string
        ];

        for (name, value) in char_constants {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::String(value.to_string()),
                },
                basic_type: BasicType::String,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        }

        // Register _ASC_* numeric constants (ASCII codes)
        let asc_constants: &[(&str, i64)] = &[
            ("_ASC_CR", 13),    // Carriage return
            ("_ASC_LF", 10),    // Line feed
            ("_ASC_SUB", 26),   // Substitute/EOF
            ("_ASC_QUOTE", 34), // Double quote
            ("_ASC_TAB", 9),    // Tab
            ("_ASC_HT", 9),     // Horizontal Tab (alias)
            ("_ASC_ESC", 27),   // Escape
            ("_ASC_BELL", 7),   // Bell
            ("_ASC_BS", 8),     // Backspace
            ("_ASC_NUL", 0),    // Null
        ];

        for (name, value) in asc_constants {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(*value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        }
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

    /// Registers keyboard scan code constants for _KEYHIT and _KEYDOWN.
    ///
    /// These constants represent keyboard keys and can be compared against
    /// the values returned by _KEYHIT and _KEYDOWN functions.
    fn register_keyboard_constants(&mut self) {
        use symbols::{ConstValue, Symbol, SymbolKind};

        // Helper to create a keyboard constant
        let mut define_key = |name: &str, value: i64| {
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

        // Special keys (extended ASCII / scan codes)
        define_key("_KEY_ESC", 27);
        define_key("_KEY_BACKSPACE", 8);
        define_key("_KEY_TAB", 9);
        define_key("_KEY_ENTER", 13);

        // Arrow keys
        define_key("_KEY_UP", 18432);
        define_key("_KEY_DOWN", 20480);
        define_key("_KEY_LEFT", 19200);
        define_key("_KEY_RIGHT", 19712);

        // Navigation keys
        define_key("_KEY_INSERT", 20992);
        define_key("_KEY_DELETE", 21248);
        define_key("_KEY_HOME", 18176);
        define_key("_KEY_END", 20224);
        define_key("_KEY_PAGEUP", 18688);
        define_key("_KEY_PAGEDOWN", 20736);

        // Function keys
        define_key("_KEY_F1", 15104);
        define_key("_KEY_F2", 15360);
        define_key("_KEY_F3", 15616);
        define_key("_KEY_F4", 15872);
        define_key("_KEY_F5", 16128);
        define_key("_KEY_F6", 16384);
        define_key("_KEY_F7", 16640);
        define_key("_KEY_F8", 16896);
        define_key("_KEY_F9", 17152);
        define_key("_KEY_F10", 17408);
        define_key("_KEY_F11", 34048);
        define_key("_KEY_F12", 34304);

        // Modifier keys (use SDL scan codes + 100000 offset for QB64 compatibility)
        define_key("_KEY_LSHIFT", 100304);
        define_key("_KEY_RSHIFT", 100303);
        define_key("_KEY_LCTRL", 100306);
        define_key("_KEY_RCTRL", 100305);
        define_key("_KEY_LALT", 100308);
        define_key("_KEY_RALT", 100307);
        define_key("_KEY_CAPSLOCK", 100301);
        define_key("_KEY_NUMLOCK", 100300);
        define_key("_KEY_SCROLLLOCK", 100302);

        // Print Screen and Pause
        define_key("_KEY_PRINT", 100316);
        define_key("_KEY_PAUSE", 100319);
    }
}

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
        symbols::SymbolKind::ArrayVariable { dimensions } => {
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
        let false_sym = false_sym.unwrap();
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
            let sym = sym.unwrap();
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
