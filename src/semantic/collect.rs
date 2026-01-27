//! Declaration collection for semantic analysis.
//!
//! This module handles the first pass of semantic analysis: collecting all
//! declarations (SUB, FUNCTION, labels, DECLARE statements) before type checking.
//! This enables forward references - you can call a SUB before its definition
//! appears in the source.
//!
//! # Organization
//!
//! - `collect_declarations` - Main entry point for declaration collection
//! - `collect_deftype_declarations` - Process DEFINT, DEFLNG, etc.
//! - `collect_procedure_declarations` - Collect SUB/FUNCTION definitions
//! - `register_sub` - Register a SUB in the symbol table
//! - `register_function` - Register a FUNCTION in the symbol table
//! - `register_declared_sub` - Register a DECLARE SUB
//! - `register_declared_function` - Register a DECLARE FUNCTION
//! - `register_external_declaration` - Register DECLARE LIBRARY items

use crate::ast::{Statement, StatementKind};

use super::SemanticAnalyzer;
use super::error::SemanticError;
use super::symbols::{ParameterInfo, ProcedureEntry, ProcedureKind};
use super::types::{self, BasicType, from_type_spec, type_from_name};

impl SemanticAnalyzer {
    /// Collects all declarations from the program.
    ///
    /// This is the first pass of semantic analysis, run before type checking.
    /// It scans for SUB/FUNCTION definitions, labels, and DECLARE statements
    /// before their definitions appear in the source.
    ///
    /// This method uses a two-pass approach:
    /// 1. First, process DEFTYPE statements (DEFINT, DEFLNG, etc.) to set default types
    /// 2. Then, collect SUB/FUNCTION definitions with correct parameter types
    pub(super) fn collect_declarations(&mut self, statements: &[Statement]) {
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
        // will be registered later and should match. However, if there are
        // multiple DECLARE statements for the same procedure, that's an error.
        if let Err(existing) = self.symbols.define_procedure(entry.clone()) {
            // Only error if both are DECLARE statements (not a DECLARE followed by definition)
            // Since we're in register_declared_sub, this is a duplicate DECLARE
            self.errors.push(SemanticError::DuplicateProcedure {
                name: entry.name.clone(),
                original_span: existing.span,
                duplicate_span: entry.span,
            });
        }
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

        // For DECLARE, we don't error on duplicates - the actual definition
        // will be registered later and should match. However, if there are
        // multiple DECLARE statements for the same procedure, that's an error.
        if let Err(existing) = self.symbols.define_procedure(entry.clone()) {
            // Only error if both are DECLARE statements (not a DECLARE followed by definition)
            // Since we're in register_declared_function, this is a duplicate DECLARE
            self.errors.push(SemanticError::DuplicateProcedure {
                name: entry.name.clone(),
                original_span: existing.span,
                duplicate_span: entry.span,
            });
        }
    }

    /// Registers an external function/sub from DECLARE LIBRARY.
    ///
    /// This makes C library functions callable from BASIC code.
    fn register_external_declaration(
        &mut self,
        decl: &crate::ast::ExternalDeclaration,
        span: crate::ast::Span,
    ) {
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

        // Duplicate DECLARE LIBRARY declarations should be reported as errors
        if let Err(existing) = self.symbols.define_procedure(entry.clone()) {
            self.errors.push(SemanticError::DuplicateProcedure {
                name: entry.name.clone(),
                original_span: existing.span,
                duplicate_span: entry.span,
            });
        }
    }
}
