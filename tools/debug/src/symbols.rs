//! Debug symbol extraction from QB64Fresh AST.
//!
//! This module extracts debugging information from the parsed AST and semantic
//! analysis results. It builds a debug symbol table that maps:
//!
//! - Variables to their types and scopes
//! - Procedures (SUB/FUNCTION) to their signatures and locations
//! - User-defined types to their member layouts
//! - Labels to their source locations
//!
//! This information is used by the debugger for:
//! - Variable inspection (what variables exist and their types)
//! - Breakpoint validation (is this a valid location?)
//! - Call stack display (what procedure are we in?)
//! - Watch expressions (parse and evaluate variable references)

use qb64fresh::ast::{
    ArrayDimension, Parameter, Program, Span, Statement, StatementKind, TypeSpec,
};
use std::collections::HashMap;
use std::path::PathBuf;

/// A debug symbol representing a variable, constant, or parameter.
#[derive(Debug, Clone)]
pub struct DebugVariable {
    /// Variable name (original casing preserved).
    pub name: String,
    /// The variable's type.
    pub var_type: DebugType,
    /// What kind of variable this is.
    pub kind: DebugVariableKind,
    /// The scope containing this variable.
    pub scope: DebugScopeId,
    /// Source location of declaration.
    pub span: Span,
}

/// The kind of debug variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DebugVariableKind {
    /// Local variable in a procedure.
    Local,
    /// Global (module-level) variable.
    Global,
    /// Procedure parameter.
    Parameter { by_val: bool },
    /// Constant (CONST statement).
    Constant,
    /// Static local variable.
    Static,
    /// SHARED variable from module level.
    Shared,
}

/// A debug type representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DebugType {
    // Primitive types
    Integer,
    Long,
    Single,
    Double,
    String,
    FixedString(usize),
    Byte,
    Bit,
    Integer64,
    Float,
    Offset,
    Unsigned(Box<DebugType>),
    /// User-defined type.
    UserDefined(String),
    /// Array type.
    Array {
        element_type: Box<DebugType>,
        dimensions: Vec<DebugArrayDimension>,
    },
    /// Unknown type (for implicit variables before analysis).
    Unknown,
}

impl DebugType {
    /// Converts from AST TypeSpec.
    pub fn from_type_spec(spec: &TypeSpec) -> Self {
        match spec {
            TypeSpec::Integer => DebugType::Integer,
            TypeSpec::Long => DebugType::Long,
            TypeSpec::Single => DebugType::Single,
            TypeSpec::Double => DebugType::Double,
            TypeSpec::String => DebugType::String,
            TypeSpec::FixedString(len) => DebugType::FixedString(*len),
            TypeSpec::Byte => DebugType::Byte,
            TypeSpec::Bit => DebugType::Bit,
            TypeSpec::Integer64 => DebugType::Integer64,
            TypeSpec::Float => DebugType::Float,
            TypeSpec::Offset => DebugType::Offset,
            TypeSpec::Unsigned(inner) => {
                DebugType::Unsigned(Box::new(DebugType::from_type_spec(inner)))
            }
            TypeSpec::UserDefined(name) => DebugType::UserDefined(name.clone()),
        }
    }

    /// Returns a human-readable type name.
    pub fn display_name(&self) -> String {
        match self {
            DebugType::Integer => "INTEGER".to_string(),
            DebugType::Long => "LONG".to_string(),
            DebugType::Single => "SINGLE".to_string(),
            DebugType::Double => "DOUBLE".to_string(),
            DebugType::String => "STRING".to_string(),
            DebugType::FixedString(len) => format!("STRING * {}", len),
            DebugType::Byte => "_BYTE".to_string(),
            DebugType::Bit => "_BIT".to_string(),
            DebugType::Integer64 => "_INTEGER64".to_string(),
            DebugType::Float => "_FLOAT".to_string(),
            DebugType::Offset => "_OFFSET".to_string(),
            DebugType::Unsigned(inner) => format!("_UNSIGNED {}", inner.display_name()),
            DebugType::UserDefined(name) => name.clone(),
            DebugType::Array {
                element_type,
                dimensions,
            } => {
                let dims: Vec<String> = dimensions
                    .iter()
                    .map(|d| {
                        if let (Some(lower), Some(upper)) = (d.lower_bound, d.upper_bound) {
                            format!("{} TO {}", lower, upper)
                        } else {
                            "?".to_string()
                        }
                    })
                    .collect();
                format!("{}({})", element_type.display_name(), dims.join(", "))
            }
            DebugType::Unknown => "?".to_string(),
        }
    }
}

/// Array dimension information for debugging.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DebugArrayDimension {
    /// Lower bound (if known at compile time).
    pub lower_bound: Option<i64>,
    /// Upper bound (if known at compile time).
    pub upper_bound: Option<i64>,
}

/// Unique identifier for a debug scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebugScopeId(pub usize);

impl DebugScopeId {
    /// The global scope ID.
    pub const GLOBAL: DebugScopeId = DebugScopeId(0);
}

/// A debug scope (global or procedure-local).
#[derive(Debug, Clone)]
pub struct DebugScope {
    /// Unique scope identifier.
    pub id: DebugScopeId,
    /// Scope kind.
    pub kind: DebugScopeKind,
    /// Parent scope (None for global).
    pub parent: Option<DebugScopeId>,
    /// Source span of the scope.
    pub span: Span,
}

/// The kind of debug scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DebugScopeKind {
    /// Global (module-level) scope.
    Global,
    /// SUB procedure scope.
    Sub { name: String },
    /// FUNCTION procedure scope.
    Function { name: String },
}

/// A procedure (SUB or FUNCTION) for debugging.
#[derive(Debug, Clone)]
pub struct DebugProcedure {
    /// Procedure name.
    pub name: String,
    /// Whether this is a FUNCTION (true) or SUB (false).
    pub is_function: bool,
    /// Parameter information.
    pub parameters: Vec<DebugParameter>,
    /// Return type (for functions).
    pub return_type: Option<DebugType>,
    /// Scope ID for this procedure's local variables.
    pub scope_id: DebugScopeId,
    /// Source span of the procedure definition.
    pub span: Span,
    /// Source span of just the procedure body (for stepping).
    pub body_span: Option<Span>,
    /// Whether STATIC was specified.
    pub is_static: bool,
}

/// A procedure parameter for debugging.
#[derive(Debug, Clone)]
pub struct DebugParameter {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub param_type: DebugType,
    /// Whether this is BYVAL.
    pub by_val: bool,
    /// Whether this is an array parameter.
    pub is_array: bool,
}

/// A user-defined TYPE for debugging.
#[derive(Debug, Clone)]
pub struct DebugUserType {
    /// Type name.
    pub name: String,
    /// Type members.
    pub members: Vec<DebugTypeMember>,
    /// Source span.
    pub span: Span,
}

/// A member of a user-defined TYPE.
#[derive(Debug, Clone)]
pub struct DebugTypeMember {
    /// Member name.
    pub name: String,
    /// Member type.
    pub member_type: DebugType,
}

/// A label for GOTO/GOSUB.
#[derive(Debug, Clone)]
pub struct DebugLabel {
    /// Label name.
    pub name: String,
    /// Scope containing this label.
    pub scope: DebugScopeId,
    /// Source span.
    pub span: Span,
}

/// Complete debug symbol information for a program.
#[derive(Debug)]
pub struct DebugSymbols {
    /// Source file this symbol table is for.
    pub file: PathBuf,
    /// All variables, indexed by (scope, uppercase_name).
    pub variables: HashMap<(DebugScopeId, String), DebugVariable>,
    /// All procedures, indexed by uppercase name.
    pub procedures: HashMap<String, DebugProcedure>,
    /// All user-defined types, indexed by uppercase name.
    pub user_types: HashMap<String, DebugUserType>,
    /// All labels, indexed by (scope, uppercase_name).
    pub labels: HashMap<(DebugScopeId, String), DebugLabel>,
    /// All scopes.
    pub scopes: HashMap<DebugScopeId, DebugScope>,
    /// Next scope ID to allocate.
    next_scope_id: usize,
}

impl DebugSymbols {
    /// Creates a new empty symbol table.
    pub fn new(file: PathBuf) -> Self {
        let mut symbols = Self {
            file,
            variables: HashMap::new(),
            procedures: HashMap::new(),
            user_types: HashMap::new(),
            labels: HashMap::new(),
            scopes: HashMap::new(),
            next_scope_id: 1, // 0 is reserved for global
        };

        // Create global scope
        symbols.scopes.insert(
            DebugScopeId::GLOBAL,
            DebugScope {
                id: DebugScopeId::GLOBAL,
                kind: DebugScopeKind::Global,
                parent: None,
                span: Span::new(0, 0),
            },
        );

        symbols
    }

    /// Extracts debug symbols from a parsed program.
    pub fn from_program(file: PathBuf, program: &Program) -> Self {
        let mut symbols = Self::new(file);
        symbols.extract_from_statements(&program.statements, DebugScopeId::GLOBAL);
        symbols
    }

    /// Allocates a new scope ID.
    fn alloc_scope(&mut self) -> DebugScopeId {
        let id = DebugScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }

    /// Extracts symbols from a list of statements.
    fn extract_from_statements(&mut self, statements: &[Statement], scope: DebugScopeId) {
        for statement in statements {
            self.extract_from_statement(statement, scope);
        }
    }

    /// Extracts symbols from a single statement.
    fn extract_from_statement(&mut self, statement: &Statement, scope: DebugScopeId) {
        match &statement.kind {
            StatementKind::Dim { variables, shared } => {
                for var in variables {
                    let var_type = if var.dimensions.is_empty() {
                        // Scalar variable
                        var.type_spec
                            .as_ref()
                            .map(DebugType::from_type_spec)
                            .unwrap_or(DebugType::Unknown)
                    } else {
                        // Array variable
                        let element_type = var
                            .type_spec
                            .as_ref()
                            .map(DebugType::from_type_spec)
                            .unwrap_or(DebugType::Unknown);
                        let dimensions = var
                            .dimensions
                            .iter()
                            .map(|d| self.extract_dimension(d))
                            .collect();
                        DebugType::Array {
                            element_type: Box::new(element_type),
                            dimensions,
                        }
                    };

                    let kind = if *shared {
                        DebugVariableKind::Shared
                    } else if scope == DebugScopeId::GLOBAL {
                        DebugVariableKind::Global
                    } else {
                        DebugVariableKind::Local
                    };

                    self.add_variable(DebugVariable {
                        name: var.name.clone(),
                        var_type,
                        kind,
                        scope,
                        span: statement.span.clone(),
                    });
                }
            }

            StatementKind::Const { definitions } => {
                for (name, _value) in definitions {
                    self.add_variable(DebugVariable {
                        name: name.clone(),
                        var_type: DebugType::Unknown, // Would need expression analysis
                        kind: DebugVariableKind::Constant,
                        scope,
                        span: statement.span.clone(),
                    });
                }
            }

            StatementKind::StaticStmt { variables } => {
                for var in variables {
                    let var_type = var
                        .type_spec
                        .as_ref()
                        .map(DebugType::from_type_spec)
                        .unwrap_or(DebugType::Unknown);

                    self.add_variable(DebugVariable {
                        name: var.name.clone(),
                        var_type,
                        kind: DebugVariableKind::Static,
                        scope,
                        span: statement.span.clone(),
                    });
                }
            }

            StatementKind::SubDefinition {
                name,
                params,
                body,
                is_static,
            } => {
                let proc_scope = self.alloc_scope();

                // Add scope
                self.scopes.insert(
                    proc_scope,
                    DebugScope {
                        id: proc_scope,
                        kind: DebugScopeKind::Sub { name: name.clone() },
                        parent: Some(DebugScopeId::GLOBAL),
                        span: statement.span.clone(),
                    },
                );

                // Add parameters
                let parameters = params.iter().map(|p| self.extract_parameter(p)).collect();

                // Add parameters as variables
                for param in params {
                    self.add_variable(DebugVariable {
                        name: param.name.clone(),
                        var_type: param
                            .type_spec
                            .as_ref()
                            .map(DebugType::from_type_spec)
                            .unwrap_or(DebugType::Unknown),
                        kind: DebugVariableKind::Parameter {
                            by_val: param.by_val,
                        },
                        scope: proc_scope,
                        span: statement.span.clone(),
                    });
                }

                // Calculate body span
                let body_span = if body.is_empty() {
                    None
                } else {
                    Some(Span::new(
                        body.first().unwrap().span.start,
                        body.last().unwrap().span.end,
                    ))
                };

                // Add procedure
                self.procedures.insert(
                    name.to_uppercase(),
                    DebugProcedure {
                        name: name.clone(),
                        is_function: false,
                        parameters,
                        return_type: None,
                        scope_id: proc_scope,
                        span: statement.span.clone(),
                        body_span,
                        is_static: *is_static,
                    },
                );

                // Process body
                self.extract_from_statements(body, proc_scope);
            }

            StatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static,
            } => {
                let proc_scope = self.alloc_scope();

                // Add scope
                self.scopes.insert(
                    proc_scope,
                    DebugScope {
                        id: proc_scope,
                        kind: DebugScopeKind::Function { name: name.clone() },
                        parent: Some(DebugScopeId::GLOBAL),
                        span: statement.span.clone(),
                    },
                );

                // Add parameters
                let parameters = params.iter().map(|p| self.extract_parameter(p)).collect();

                // Add parameters as variables
                for param in params {
                    self.add_variable(DebugVariable {
                        name: param.name.clone(),
                        var_type: param
                            .type_spec
                            .as_ref()
                            .map(DebugType::from_type_spec)
                            .unwrap_or(DebugType::Unknown),
                        kind: DebugVariableKind::Parameter {
                            by_val: param.by_val,
                        },
                        scope: proc_scope,
                        span: statement.span.clone(),
                    });
                }

                // Calculate body span
                let body_span = if body.is_empty() {
                    None
                } else {
                    Some(Span::new(
                        body.first().unwrap().span.start,
                        body.last().unwrap().span.end,
                    ))
                };

                // Return type
                let ret_type = return_type.as_ref().map(DebugType::from_type_spec);

                // Add procedure
                self.procedures.insert(
                    name.to_uppercase(),
                    DebugProcedure {
                        name: name.clone(),
                        is_function: true,
                        parameters,
                        return_type: ret_type,
                        scope_id: proc_scope,
                        span: statement.span.clone(),
                        body_span,
                        is_static: *is_static,
                    },
                );

                // Process body
                self.extract_from_statements(body, proc_scope);
            }

            StatementKind::TypeDefinition { name, members, .. } => {
                let debug_members = members
                    .iter()
                    .map(|m| DebugTypeMember {
                        name: m.name.clone(),
                        member_type: DebugType::from_type_spec(&m.type_spec),
                    })
                    .collect();

                self.user_types.insert(
                    name.to_uppercase(),
                    DebugUserType {
                        name: name.clone(),
                        members: debug_members,
                        span: statement.span.clone(),
                    },
                );
            }

            StatementKind::Label { name } => {
                self.labels.insert(
                    (scope, name.to_uppercase()),
                    DebugLabel {
                        name: name.clone(),
                        scope,
                        span: statement.span.clone(),
                    },
                );
            }

            // Handle control structures with nested statements
            StatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                self.extract_from_statements(then_branch, scope);
                for (_, stmts) in elseif_branches {
                    self.extract_from_statements(stmts, scope);
                }
                if let Some(stmts) = else_branch {
                    self.extract_from_statements(stmts, scope);
                }
            }

            StatementKind::For { body, .. } => {
                self.extract_from_statements(body, scope);
            }

            StatementKind::While { body, .. } => {
                self.extract_from_statements(body, scope);
            }

            StatementKind::DoLoop { body, .. } => {
                self.extract_from_statements(body, scope);
            }

            StatementKind::SelectCase {
                cases, case_else, ..
            }
            | StatementKind::SelectEveryCase {
                cases, case_else, ..
            } => {
                for case in cases {
                    self.extract_from_statements(&case.body, scope);
                }
                if let Some(stmts) = case_else {
                    self.extract_from_statements(stmts, scope);
                }
            }

            _ => {}
        }
    }

    /// Adds a variable to the symbol table.
    fn add_variable(&mut self, var: DebugVariable) {
        let key = (var.scope, var.name.to_uppercase());
        self.variables.insert(key, var);
    }

    /// Extracts dimension information from an AST dimension.
    fn extract_dimension(&self, dim: &ArrayDimension) -> DebugArrayDimension {
        // For now, we only capture literal bounds
        // In the future, we could evaluate constant expressions
        DebugArrayDimension {
            lower_bound: dim.lower.as_ref().and_then(|e| self.eval_const_int(e)),
            upper_bound: self.eval_const_int(&dim.upper),
        }
    }

    /// Tries to evaluate a constant integer expression.
    fn eval_const_int(&self, _expr: &qb64fresh::ast::Expr) -> Option<i64> {
        // For now, we don't evaluate expressions
        // This would require a const-eval pass
        // The semantic analyzer does this, but we don't have access to those results
        None
    }

    /// Extracts parameter information from an AST parameter.
    fn extract_parameter(&self, param: &Parameter) -> DebugParameter {
        DebugParameter {
            name: param.name.clone(),
            param_type: param
                .type_spec
                .as_ref()
                .map(DebugType::from_type_spec)
                .unwrap_or(DebugType::Unknown),
            by_val: param.by_val,
            is_array: param.is_array,
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Query Methods
    // ─────────────────────────────────────────────────────────────────────────

    /// Looks up a variable by name in the given scope (checking parent scopes).
    pub fn lookup_variable(&self, name: &str, scope: DebugScopeId) -> Option<&DebugVariable> {
        let upper_name = name.to_uppercase();

        // Check current scope
        if let Some(var) = self.variables.get(&(scope, upper_name.clone())) {
            return Some(var);
        }

        // Check parent scopes
        if let Some(parent_scope) = self.scopes.get(&scope).and_then(|s| s.parent) {
            return self.lookup_variable(name, parent_scope);
        }

        None
    }

    /// Gets all variables in a scope (including inherited from parent scopes).
    pub fn variables_in_scope(&self, scope: DebugScopeId) -> Vec<&DebugVariable> {
        let mut vars = Vec::new();
        let mut current_scope = Some(scope);

        while let Some(s) = current_scope {
            for ((var_scope, _), var) in &self.variables {
                if *var_scope == s {
                    vars.push(var);
                }
            }
            current_scope = self.scopes.get(&s).and_then(|sc| sc.parent);
        }

        vars
    }

    /// Looks up a procedure by name.
    pub fn lookup_procedure(&self, name: &str) -> Option<&DebugProcedure> {
        self.procedures.get(&name.to_uppercase())
    }

    /// Looks up a user-defined type by name.
    pub fn lookup_type(&self, name: &str) -> Option<&DebugUserType> {
        self.user_types.get(&name.to_uppercase())
    }

    /// Looks up a label by name in the given scope.
    pub fn lookup_label(&self, name: &str, scope: DebugScopeId) -> Option<&DebugLabel> {
        self.labels.get(&(scope, name.to_uppercase()))
    }

    /// Finds which scope contains a given source offset.
    pub fn scope_at_offset(&self, offset: usize) -> DebugScopeId {
        // Find the most specific (innermost) scope containing this offset
        let mut best_scope = DebugScopeId::GLOBAL;
        let mut best_span_size = usize::MAX;

        for (id, scope) in &self.scopes {
            if scope.span.start <= offset && offset <= scope.span.end {
                let span_size = scope.span.end - scope.span.start;
                if span_size < best_span_size {
                    best_scope = *id;
                    best_span_size = span_size;
                }
            }
        }

        best_scope
    }

    /// Gets all global variables.
    pub fn global_variables(&self) -> impl Iterator<Item = &DebugVariable> {
        self.variables
            .values()
            .filter(|v| v.scope == DebugScopeId::GLOBAL)
    }

    /// Gets all procedures.
    pub fn all_procedures(&self) -> impl Iterator<Item = &DebugProcedure> {
        self.procedures.values()
    }

    /// Gets all user-defined types.
    pub fn all_types(&self) -> impl Iterator<Item = &DebugUserType> {
        self.user_types.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use qb64fresh::lexer::lex;
    use qb64fresh::parser::Parser;

    fn parse_program(source: &str) -> Program {
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        parser.parse().expect("parse failed")
    }

    #[test]
    fn test_extract_dim() {
        let source = r#"
DIM x AS INTEGER
DIM y AS STRING
DIM arr(10) AS LONG
"#;
        let program = parse_program(source);
        let symbols = DebugSymbols::from_program(PathBuf::from("test.bas"), &program);

        assert!(symbols.lookup_variable("x", DebugScopeId::GLOBAL).is_some());
        assert!(symbols.lookup_variable("y", DebugScopeId::GLOBAL).is_some());
        assert!(symbols
            .lookup_variable("arr", DebugScopeId::GLOBAL)
            .is_some());

        let arr = symbols
            .lookup_variable("arr", DebugScopeId::GLOBAL)
            .unwrap();
        assert!(matches!(arr.var_type, DebugType::Array { .. }));
    }

    #[test]
    fn test_extract_sub() {
        let source = r#"
SUB MySub (x AS INTEGER, y AS STRING)
    DIM local AS LONG
END SUB
"#;
        let program = parse_program(source);
        let symbols = DebugSymbols::from_program(PathBuf::from("test.bas"), &program);

        let proc = symbols.lookup_procedure("MySub").expect("MySub not found");
        assert!(!proc.is_function);
        assert_eq!(proc.parameters.len(), 2);

        // Check that local is in the sub's scope
        let local_vars: Vec<_> = symbols.variables_in_scope(proc.scope_id);
        assert!(local_vars
            .iter()
            .any(|v| v.name.eq_ignore_ascii_case("local")));
    }

    #[test]
    fn test_extract_function() {
        let source = r#"
FUNCTION Add% (a AS INTEGER, b AS INTEGER)
    Add% = a + b
END FUNCTION
"#;
        let program = parse_program(source);
        let symbols = DebugSymbols::from_program(PathBuf::from("test.bas"), &program);

        // Function name includes the type suffix %
        let proc = symbols.lookup_procedure("Add%").expect("Add% not found");
        assert!(proc.is_function);
        assert_eq!(proc.parameters.len(), 2);
    }

    #[test]
    fn test_extract_type() {
        let source = r#"
TYPE Person
    name AS STRING * 20
    age AS INTEGER
END TYPE
"#;
        let program = parse_program(source);
        let symbols = DebugSymbols::from_program(PathBuf::from("test.bas"), &program);

        let udt = symbols.lookup_type("Person").expect("Person not found");
        assert_eq!(udt.members.len(), 2);
        assert_eq!(udt.members[0].name, "name");
        assert_eq!(udt.members[1].name, "age");
    }

    #[test]
    fn test_debug_type_display() {
        assert_eq!(DebugType::Integer.display_name(), "INTEGER");
        assert_eq!(DebugType::String.display_name(), "STRING");
        assert_eq!(DebugType::FixedString(20).display_name(), "STRING * 20");
        assert_eq!(
            DebugType::Unsigned(Box::new(DebugType::Integer)).display_name(),
            "_UNSIGNED INTEGER"
        );
    }
}
