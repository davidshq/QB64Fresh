//! Symbol table for QB64Fresh semantic analysis.
//!
//! The symbol table tracks all named entities (variables, procedures, labels)
//! and their properties during semantic analysis. It supports:
//!
//! - **Nested scopes**: Global scope plus local scopes for SUB/FUNCTION
//! - **SHARED variables**: Access to module-level variables from procedures
//! - **Case-insensitive lookups**: Following BASIC tradition
//! - **DEFtype defaults**: Type defaults by variable first letter
//! - **Type suffix differentiation**: `x%`, `x$`, and `x&` are DIFFERENT variables
//!
//! # Variable Name Semantics
//!
//! In BASIC, variables with different type suffixes are completely different variables:
//! - `A$` is a STRING variable
//! - `A%` is an INTEGER variable
//! - `A&` is a LONG variable
//! - `A` (bare) uses DEFtype or defaults to SINGLE
//!
//! All of these can coexist in the same scope. Case is ignored (`A$` == `a$`).
//!
//! # Scope Rules
//!
//! BASIC has simpler scoping than most modern languages:
//! - Module-level (global) scope contains main program variables
//! - Each SUB/FUNCTION creates a local scope
//! - Variables in local scope shadow global variables (unless SHARED)
//! - Labels are scope-local (can't GOTO into/out of procedures)

use crate::ast::Span;
use crate::semantic::types::{self, BasicType};
use std::collections::{HashMap, HashSet};

/// Strips the type suffix from an identifier name for DEFtype purposes only.
///
/// This is used only to determine which letter-range applies for DEFtype.
/// It does NOT mean the variables are the same - `a$` and `a&` are different.
fn strip_suffix_for_deftype(name: &str) -> &str {
    // Two-character suffixes (must check first)
    let two_char_suffixes = ["%%", "&&", "##", "%&", "~%", "~&", "~`"];
    for suffix in &two_char_suffixes {
        if let Some(stripped) = name.strip_suffix(suffix) {
            return stripped;
        }
    }

    // Single-character suffixes
    let one_char_suffixes = ['$', '%', '&', '!', '#', '`'];
    if let Some(last) = name.chars().last()
        && one_char_suffixes.contains(&last)
    {
        return &name[..name.len() - 1];
    }

    name
}

/// Checks if a type suffix is compatible with a declared type.
///
/// In BASIC, when a variable is declared with an explicit type (e.g., `DIM x AS STRING`),
/// it can be referenced with a type suffix that matches its declared type (e.g., `x$`).
/// This function returns true if the suffix-implied type matches the declared type.
fn suffix_matches_type(suffix_type: &BasicType, declared_type: &BasicType) -> bool {
    // Direct match
    if suffix_type == declared_type {
        return true;
    }

    // String types match (STRING and FixedString both use $ suffix)
    if matches!(suffix_type, BasicType::String)
        && matches!(declared_type, BasicType::String | BasicType::FixedString(_))
    {
        return true;
    }

    false
}

/// Unique identifier for a scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(usize);

impl ScopeId {
    /// The global scope ID (always 0).
    pub const GLOBAL: ScopeId = ScopeId(0);
}

/// A single symbol (variable, constant, or parameter).
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The symbol's name (original casing preserved).
    pub name: String,
    /// What kind of symbol this is.
    pub kind: SymbolKind,
    /// The symbol's type.
    pub basic_type: BasicType,
    /// Where the symbol was defined.
    pub span: Span,
    /// Whether this symbol can be assigned to (false for CONST).
    pub is_mutable: bool,
}

/// The kind of symbol.
#[derive(Debug, Clone)]
pub enum SymbolKind {
    /// A regular variable.
    Variable,
    /// A compile-time constant (CONST statement).
    Constant {
        /// The constant's value (for constant folding).
        value: ConstValue,
    },
    /// A procedure parameter.
    Parameter {
        /// Whether this parameter is passed by value (BYVAL).
        by_val: bool,
    },
    /// An array variable.
    ArrayVariable {
        /// Information about each dimension.
        dimensions: Vec<ArrayDimInfo>,
    },
    /// An external function declared via DECLARE LIBRARY.
    ExternalFunction {
        /// The C function name (from ALIAS or same as BASIC name).
        c_name: String,
        /// Parameter types.
        params: Vec<BasicType>,
        /// Return type.
        return_type: BasicType,
    },
}

/// Information about an array dimension.
#[derive(Debug, Clone)]
pub struct ArrayDimInfo {
    /// Lower bound (inclusive).
    pub lower_bound: i64,
    /// Upper bound (inclusive).
    pub upper_bound: i64,
}

/// A compile-time constant value.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    /// Integer constant.
    Integer(i64),
    /// Floating-point constant.
    Float(f64),
    /// String constant.
    String(String),
}

/// A procedure (SUB or FUNCTION) entry.
#[derive(Debug, Clone)]
pub struct ProcedureEntry {
    /// Procedure name.
    pub name: String,
    /// SUB, FUNCTION, or built-in.
    pub kind: ProcedureKind,
    /// Parameter information.
    pub params: Vec<ParameterInfo>,
    /// Return type (None for SUB).
    pub return_type: Option<BasicType>,
    /// Where the procedure was defined.
    pub span: Span,
    /// Whether STATIC was specified.
    pub is_static: bool,
}

impl ProcedureEntry {
    /// Returns the number of required (non-optional) parameters.
    pub fn required_param_count(&self) -> usize {
        self.params.iter().filter(|p| !p.is_optional).count()
    }
}

/// The kind of procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcedureKind {
    /// SUB - no return value
    Sub,
    /// FUNCTION - returns a value
    Function,
    /// Built-in function (LEN, CHR$, etc.)
    BuiltIn,
    /// External function from DECLARE LIBRARY
    External,
}

/// Information about a procedure parameter.
#[derive(Debug, Clone)]
pub struct ParameterInfo {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub basic_type: BasicType,
    /// Whether this is a BYVAL parameter.
    pub by_val: bool,
    /// Whether this parameter is optional (has a default value).
    /// Used for built-in functions like MID$ (2 or 3 args) and RND (0 or 1 arg).
    pub is_optional: bool,
    /// Whether this parameter is an array (e.g., `arr()` syntax).
    pub is_array: bool,
}

/// A user-defined TYPE definition.
#[derive(Debug, Clone)]
pub struct UserTypeDefinition {
    /// Type name.
    pub name: String,
    /// Type members.
    pub members: Vec<UserTypeMember>,
    /// Where the type was defined.
    pub span: Span,
    /// QB4.5 CUSTOMTYPE modifier - indicates C-compatible memory layout.
    pub custom_type: bool,
}

/// A member of a user-defined TYPE.
#[derive(Debug, Clone)]
pub struct UserTypeMember {
    /// Member name.
    pub name: String,
    /// Member type.
    pub basic_type: BasicType,
}

/// A label for GOTO/GOSUB.
#[derive(Debug, Clone)]
pub struct LabelEntry {
    /// Label name.
    pub name: String,
    /// Where the label was defined.
    pub span: Span,
    /// Which scope the label belongs to.
    pub scope_id: ScopeId,
}

/// The kind of scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// The module-level (global) scope.
    Global,
    /// A SUB's local scope.
    Sub,
    /// A FUNCTION's local scope.
    Function,
}

/// A single scope containing symbols and labels.
///
/// BASIC uses a dual namespace model where scalars and arrays are in separate
/// namespaces. This means `x` (scalar) and `x()` (array) can coexist with
/// different types. The scope maintains separate storage for each namespace.
#[derive(Debug)]
struct Scope {
    /// Unique scope identifier (kept for Debug output).
    #[allow(dead_code)]
    id: ScopeId,
    /// What kind of scope this is.
    kind: ScopeKind,
    /// Scalar symbols (variables, constants, parameters) - uppercase names.
    /// In BASIC, `x` refers to a scalar even if `x()` array exists.
    scalars: HashMap<String, Symbol>,
    /// Array symbols (ArrayVariable kind only) - uppercase names.
    /// In BASIC, `x()` refers to an array even if `x` scalar exists.
    arrays: HashMap<String, Symbol>,
    /// Labels in this scope (uppercase names).
    labels: HashMap<String, LabelEntry>,
    /// Parent scope (None for global).
    parent: Option<ScopeId>,
}

/// The complete symbol table.
///
/// Manages all symbols, procedures, and labels for a compilation unit.
pub struct SymbolTable {
    /// All scopes, indexed by ScopeId.
    scopes: HashMap<ScopeId, Scope>,
    /// Current scope we're analyzing.
    current_scope: ScopeId,
    /// Next scope ID to allocate.
    next_scope_id: usize,

    /// Procedures are stored globally (callable from anywhere).
    procedures: HashMap<String, ProcedureEntry>,

    /// SHARED variables per scope: scope -> list of shared variable names.
    /// These are variables explicitly marked SHARED inside a procedure.
    shared_vars: HashMap<ScopeId, Vec<String>>,

    /// Variables declared with DIM SHARED at module level.
    /// These are automatically visible from all procedures without needing
    /// an explicit SHARED statement inside the procedure.
    module_shared_vars: HashSet<String>,

    /// Default type by first letter (A-Z). Index 0 = 'A', etc.
    /// Default is SINGLE unless changed by DEFtype.
    default_types: [BasicType; 26],

    /// OPTION BASE setting (0 or 1, default 0).
    option_base: i64,

    /// OPTION _EXPLICIT mode - requires all variables to be declared.
    explicit_mode: bool,

    /// OPTION _EXPLICITARRAY mode - requires all arrays to be declared.
    explicit_array_mode: bool,

    /// User-defined TYPE definitions (globally scoped).
    user_types: HashMap<String, UserTypeDefinition>,
}

impl SymbolTable {
    /// Creates a new symbol table with only the global scope.
    pub fn new() -> Self {
        let global_id = ScopeId::GLOBAL;
        let mut scopes = HashMap::new();
        scopes.insert(
            global_id,
            Scope {
                id: global_id,
                kind: ScopeKind::Global,
                scalars: HashMap::new(),
                arrays: HashMap::new(),
                labels: HashMap::new(),
                parent: None,
            },
        );

        Self {
            scopes,
            current_scope: global_id,
            next_scope_id: 1,
            procedures: HashMap::new(),
            shared_vars: HashMap::new(),
            module_shared_vars: HashSet::new(),
            default_types: std::array::from_fn(|_| BasicType::Single),
            user_types: HashMap::new(),
            option_base: 0,
            explicit_mode: false,
            explicit_array_mode: false,
        }
    }

    /// Enters a new scope (for SUB or FUNCTION).
    ///
    /// Returns the new scope's ID.
    pub fn enter_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;

        self.scopes.insert(
            id,
            Scope {
                id,
                kind,
                scalars: HashMap::new(),
                arrays: HashMap::new(),
                labels: HashMap::new(),
                parent: Some(self.current_scope),
            },
        );

        self.current_scope = id;
        id
    }

    /// Exits the current scope, returning to the parent.
    ///
    /// Does nothing if already at global scope.
    pub fn exit_scope(&mut self) {
        if let Some(scope) = self.scopes.get(&self.current_scope)
            && let Some(parent) = scope.parent
        {
            self.current_scope = parent;
        }
    }

    /// Returns the current scope ID.
    pub fn current_scope_id(&self) -> ScopeId {
        self.current_scope
    }

    /// Returns the kind of the current scope.
    pub fn current_scope_kind(&self) -> ScopeKind {
        self.scopes
            .get(&self.current_scope)
            .map(|s| s.kind)
            .unwrap_or(ScopeKind::Global)
    }

    /// Returns true if currently inside a procedure (SUB or FUNCTION).
    pub fn in_procedure(&self) -> bool {
        matches!(
            self.current_scope_kind(),
            ScopeKind::Sub | ScopeKind::Function
        )
    }

    /// Defines a symbol in the current scope.
    ///
    /// Returns `Err((existing, new))` if a symbol with this name already exists
    /// **in the same namespace**. Arrays and scalars are in separate namespaces,
    /// so `DIM x AS INTEGER` and `DIM x(10) AS STRING` can coexist.
    ///
    /// Note: In BASIC, variables with different type suffixes are DIFFERENT variables:
    /// `x%`, `x$`, and `x&` are three separate variables. However, lookups are
    /// case-insensitive, so `X$` and `x$` are the same variable.
    pub fn define_symbol(&mut self, symbol: Symbol) -> Result<(), Box<(Symbol, Symbol)>> {
        let scope = self
            .scopes
            .get_mut(&self.current_scope)
            .expect("current_scope must always exist in scopes map");
        // Use the FULL name (including suffix) for uniqueness, but case-insensitive
        let name_upper = symbol.name.to_uppercase();

        // Determine which namespace based on symbol kind
        let is_array = matches!(symbol.kind, SymbolKind::ArrayVariable { .. });

        if is_array {
            // Array namespace
            if let Some(existing) = scope.arrays.get(&name_upper) {
                return Err(Box::new((existing.clone(), symbol)));
            }
            scope.arrays.insert(name_upper, symbol);
        } else {
            // Scalar namespace (variables, constants, parameters)
            if let Some(existing) = scope.scalars.get(&name_upper) {
                return Err(Box::new((existing.clone(), symbol)));
            }
            scope.scalars.insert(name_upper, symbol);
        }
        Ok(())
    }

    /// Updates an existing symbol or defines a new one in the current scope.
    ///
    /// This is used for REDIM which can resize an existing array (including
    /// array parameters) or create a new dynamic array.
    pub fn update_or_define_symbol(&mut self, symbol: Symbol) {
        let scope = self
            .scopes
            .get_mut(&self.current_scope)
            .expect("current_scope must always exist in scopes map");
        let name_upper = symbol.name.to_uppercase();

        // Determine which namespace based on symbol kind
        let is_array = matches!(symbol.kind, SymbolKind::ArrayVariable { .. });

        if is_array {
            scope.arrays.insert(name_upper, symbol);
        } else {
            scope.scalars.insert(name_upper, symbol);
        }
    }

    /// Looks up a symbol by name, searching in BOTH namespaces.
    ///
    /// This is a general lookup that returns any matching symbol (scalar or array).
    /// For context-specific lookups, use `lookup_scalar()` or `lookup_array()`.
    ///
    /// In BASIC, procedure scopes (SUB/FUNCTION) are isolated from global scope.
    /// Variables from global scope are only visible if explicitly SHARED.
    ///
    /// Variables with different type suffixes are DIFFERENT variables:
    /// `x$`, `x%`, and `x&` are three separate variables. Lookups are case-insensitive.
    ///
    /// However, if a variable is declared with an explicit type (e.g., `DIM x AS STRING`),
    /// it can be referenced with a matching type suffix (e.g., `x$`). This function
    /// handles that case by falling back to base name lookup with type compatibility check.
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        // Use FULL name (including suffix) for lookup, case-insensitive
        let name_upper = name.to_uppercase();
        let scope = self.scopes.get(&self.current_scope)?;

        // Check current scope - scalars first (more common), then arrays
        if let Some(sym) = scope.scalars.get(&name_upper) {
            return Some(sym);
        }
        if let Some(sym) = scope.arrays.get(&name_upper) {
            return Some(sym);
        }

        // Fallback: If name has a type suffix, try looking up the base name
        // and check if the type is compatible with the suffix.
        // This handles cases like: DIM x AS STRING ... x$ = "hello"
        if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
            let base_name = types::strip_suffix(&name_upper).to_uppercase();
            if base_name != name_upper {
                // Try to find base name in current scope
                if let Some(sym) = scope.scalars.get(&base_name)
                    && suffix_matches_type(&suffix_type, &sym.basic_type)
                {
                    return Some(sym);
                }
                if let Some(sym) = scope.arrays.get(&base_name)
                    && suffix_matches_type(&suffix_type, &sym.basic_type)
                {
                    return Some(sym);
                }
            }
        }

        // Reverse fallback: If name has NO suffix, try looking up constants with suffixes
        // This handles cases like: CONST idecpnum& = 27 ... IF y > idecpnum THEN
        // Only apply to constants since suffixed variables are distinct (x$ != x%)
        if types::type_from_suffix(&name_upper).is_none() {
            // Try common type suffixes, but ONLY return if it's a constant
            for suffix in &[
                "&", "%", "!", "#", "$", "&&", "%%", "##", "%&", "~&", "~&&", "~%%", "~%", "~%&",
            ] {
                let suffixed_name = format!("{}{}", name_upper, suffix);
                if let Some(sym) = scope.scalars.get(&suffixed_name)
                    && matches!(sym.kind, SymbolKind::Constant { .. })
                {
                    return Some(sym);
                }
            }
        }

        // If in a procedure scope, SHARED variables are visible from global
        if matches!(scope.kind, ScopeKind::Sub | ScopeKind::Function) {
            // Check if this variable was declared with DIM SHARED at module level
            if self.module_shared_vars.contains(&name_upper)
                && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
            {
                // Check both namespaces in global scope
                if let Some(sym) = global.scalars.get(&name_upper) {
                    return Some(sym);
                }
                if let Some(sym) = global.arrays.get(&name_upper) {
                    return Some(sym);
                }
            }

            // Check if this variable is explicitly SHARED in this scope
            if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                && shared_names.iter().any(|n| n.to_uppercase() == name_upper)
            {
                // Look up in global scope only
                if let Some(global) = self.scopes.get(&ScopeId::GLOBAL) {
                    if let Some(sym) = global.scalars.get(&name_upper) {
                        return Some(sym);
                    }
                    if let Some(sym) = global.arrays.get(&name_upper) {
                        return Some(sym);
                    }
                }
            }

            // Fallback for SHARED variables: try base name lookup
            if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
                let base_name = types::strip_suffix(&name_upper).to_uppercase();
                if base_name != name_upper {
                    // Check module shared vars with base name
                    if self.module_shared_vars.contains(&base_name)
                        && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                    {
                        if let Some(sym) = global.scalars.get(&base_name)
                            && suffix_matches_type(&suffix_type, &sym.basic_type)
                        {
                            return Some(sym);
                        }
                        if let Some(sym) = global.arrays.get(&base_name)
                            && suffix_matches_type(&suffix_type, &sym.basic_type)
                        {
                            return Some(sym);
                        }
                    }

                    // Check explicitly SHARED vars with base name
                    if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                        && shared_names.iter().any(|n| n.to_uppercase() == base_name)
                        && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                    {
                        if let Some(sym) = global.scalars.get(&base_name)
                            && suffix_matches_type(&suffix_type, &sym.basic_type)
                        {
                            return Some(sym);
                        }
                        if let Some(sym) = global.arrays.get(&base_name)
                            && suffix_matches_type(&suffix_type, &sym.basic_type)
                        {
                            return Some(sym);
                        }
                    }
                }
            }

            // Check for built-in constants in global scope (always visible)
            // Constants are immutable, so there's no scoping issue
            if let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                && let Some(sym) = global.scalars.get(&name_upper)
                && matches!(sym.kind, SymbolKind::Constant { .. })
            {
                return Some(sym);
            }

            // Check for constants with suffixes in global scope
            // This handles: CONST idecpnum& = 27 ... IF y > idecpnum THEN
            if types::type_from_suffix(&name_upper).is_none()
                && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
            {
                for suffix in &[
                    "&", "%", "!", "#", "$", "&&", "%%", "##", "%&", "~&", "~&&", "~%%", "~%",
                    "~%&",
                ] {
                    let suffixed_name = format!("{}{}", name_upper, suffix);
                    if let Some(sym) = global.scalars.get(&suffixed_name)
                        && matches!(sym.kind, SymbolKind::Constant { .. })
                    {
                        return Some(sym);
                    }
                }
            }

            // Not SHARED, not found in local scope -> not visible
            return None;
        }

        // In global scope, only check global symbols (already did above)
        None
    }

    /// Looks up an array symbol by name (array namespace only).
    ///
    /// In BASIC, arrays and scalars are in separate namespaces. Use this method
    /// when looking up `name(args)` syntax which should find arrays specifically.
    /// If `x` is a scalar and `x()` is an array, `lookup_array("x")` returns the array.
    ///
    /// This also handles suffix-to-base-name matching: `x$()` will find `x()` if `x` is STRING array.
    pub fn lookup_array(&self, name: &str) -> Option<&Symbol> {
        let name_upper = name.to_uppercase();
        let scope = self.scopes.get(&self.current_scope)?;

        // Check current scope's array namespace
        if let Some(sym) = scope.arrays.get(&name_upper) {
            return Some(sym);
        }

        // Fallback: If name has a type suffix, try looking up the base name
        if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
            let base_name = types::strip_suffix(&name_upper).to_uppercase();
            if base_name != name_upper
                && let Some(sym) = scope.arrays.get(&base_name)
            {
                // For arrays, check the element type
                if let BasicType::Array { element_type, .. } = &sym.basic_type {
                    if suffix_matches_type(&suffix_type, element_type) {
                        return Some(sym);
                    }
                } else if suffix_matches_type(&suffix_type, &sym.basic_type) {
                    return Some(sym);
                }
            }
        }

        // If in a procedure scope, check for SHARED arrays from global
        if matches!(scope.kind, ScopeKind::Sub | ScopeKind::Function) {
            // Check if this array was declared with DIM SHARED at module level
            if self.module_shared_vars.contains(&name_upper)
                && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                && let Some(sym) = global.arrays.get(&name_upper)
            {
                return Some(sym);
            }

            // Check if this variable is explicitly SHARED in this scope
            if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                && shared_names.iter().any(|n| n.to_uppercase() == name_upper)
                && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                && let Some(sym) = global.arrays.get(&name_upper)
            {
                return Some(sym);
            }

            // Fallback for SHARED: try base name lookup
            if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
                let base_name = types::strip_suffix(&name_upper).to_uppercase();
                if base_name != name_upper {
                    // Check module shared with base name
                    if self.module_shared_vars.contains(&base_name)
                        && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                        && let Some(sym) = global.arrays.get(&base_name)
                    {
                        if let BasicType::Array { element_type, .. } = &sym.basic_type {
                            if suffix_matches_type(&suffix_type, element_type) {
                                return Some(sym);
                            }
                        } else if suffix_matches_type(&suffix_type, &sym.basic_type) {
                            return Some(sym);
                        }
                    }

                    // Check explicitly SHARED with base name
                    if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                        && shared_names.iter().any(|n| n.to_uppercase() == base_name)
                        && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                        && let Some(sym) = global.arrays.get(&base_name)
                    {
                        if let BasicType::Array { element_type, .. } = &sym.basic_type {
                            if suffix_matches_type(&suffix_type, element_type) {
                                return Some(sym);
                            }
                        } else if suffix_matches_type(&suffix_type, &sym.basic_type) {
                            return Some(sym);
                        }
                    }
                }
            }
        }

        None
    }

    /// Looks up a scalar (non-array) symbol by name (scalar namespace only).
    ///
    /// In BASIC, arrays and scalars are in separate namespaces. `DIM x AS STRING`
    /// and `DIM x(10) AS INTEGER` can coexist:
    /// - `x` refers to the STRING scalar
    /// - `x(...)` refers to the INTEGER array
    ///
    /// Use this method when looking up simple variable references (not array access).
    /// If `x` is a scalar and `x()` is an array, `lookup_scalar("x")` returns the scalar.
    ///
    /// This also handles suffix-to-base-name matching: `x$` will find `x` if `x` is STRING.
    pub fn lookup_scalar(&self, name: &str) -> Option<&Symbol> {
        let name_upper = name.to_uppercase();
        let scope = self.scopes.get(&self.current_scope)?;

        // Check current scope's scalar namespace
        if let Some(sym) = scope.scalars.get(&name_upper) {
            return Some(sym);
        }

        // Fallback: If name has a type suffix, try looking up the base name
        if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
            let base_name = types::strip_suffix(&name_upper).to_uppercase();
            if base_name != name_upper
                && let Some(sym) = scope.scalars.get(&base_name)
                && suffix_matches_type(&suffix_type, &sym.basic_type)
            {
                return Some(sym);
            }
        }

        // If in a procedure scope, check for SHARED scalars from global
        if matches!(scope.kind, ScopeKind::Sub | ScopeKind::Function) {
            // Check if this variable was declared with DIM SHARED at module level
            if self.module_shared_vars.contains(&name_upper)
                && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                && let Some(sym) = global.scalars.get(&name_upper)
            {
                return Some(sym);
            }

            // Check if this variable is explicitly SHARED in this scope
            if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                && shared_names.iter().any(|n| n.to_uppercase() == name_upper)
                && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                && let Some(sym) = global.scalars.get(&name_upper)
            {
                return Some(sym);
            }

            // Fallback for SHARED: try base name lookup
            if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
                let base_name = types::strip_suffix(&name_upper).to_uppercase();
                if base_name != name_upper {
                    // Check module shared with base name
                    if self.module_shared_vars.contains(&base_name)
                        && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                        && let Some(sym) = global.scalars.get(&base_name)
                        && suffix_matches_type(&suffix_type, &sym.basic_type)
                    {
                        return Some(sym);
                    }

                    // Check explicitly SHARED with base name
                    if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                        && shared_names.iter().any(|n| n.to_uppercase() == base_name)
                        && let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                        && let Some(sym) = global.scalars.get(&base_name)
                        && suffix_matches_type(&suffix_type, &sym.basic_type)
                    {
                        return Some(sym);
                    }
                }
            }

            // Check for built-in constants in global scope (always visible)
            if let Some(global) = self.scopes.get(&ScopeId::GLOBAL)
                && let Some(sym) = global.scalars.get(&name_upper)
                && matches!(sym.kind, SymbolKind::Constant { .. })
            {
                return Some(sym);
            }
        }

        None
    }

    /// Checks if a symbol exists in the current scope only (not parent scopes).
    ///
    /// Checks both scalar and array namespaces.
    /// Variables with different type suffixes are DIFFERENT variables.
    pub fn symbol_in_current_scope(&self, name: &str) -> bool {
        // Use FULL name (including suffix) for lookup, case-insensitive
        let name_upper = name.to_uppercase();
        self.scopes
            .get(&self.current_scope)
            .map(|s| s.scalars.contains_key(&name_upper) || s.arrays.contains_key(&name_upper))
            .unwrap_or(false)
    }

    /// Defines a label in the current scope.
    ///
    /// Returns `Err(existing)` if a label with this name already exists.
    pub fn define_label(&mut self, name: String, span: Span) -> Result<(), LabelEntry> {
        let scope = self
            .scopes
            .get_mut(&self.current_scope)
            .expect("current_scope must always exist in scopes map");
        let name_upper = name.to_uppercase();

        if let Some(existing) = scope.labels.get(&name_upper) {
            return Err(existing.clone());
        }

        scope.labels.insert(
            name_upper,
            LabelEntry {
                name,
                span,
                scope_id: self.current_scope,
            },
        );
        Ok(())
    }

    /// Looks up a label in the current scope only.
    ///
    /// Labels don't cross scope boundaries (can't GOTO into/out of procedures).
    pub fn lookup_label(&self, name: &str) -> Option<&LabelEntry> {
        let name_upper = name.to_uppercase();
        self.scopes
            .get(&self.current_scope)?
            .labels
            .get(&name_upper)
    }

    /// Defines a procedure (SUB or FUNCTION).
    ///
    /// Returns `Err(existing)` if a procedure with this name already exists
    /// and it's a true redefinition (not a DECLARE followed by definition).
    ///
    /// If `allow_redefinition` is true (for actual SUB/FUNCTION definitions),
    /// it will overwrite an existing entry (which was from a DECLARE).
    pub fn define_procedure(&mut self, entry: ProcedureEntry) -> Result<(), ProcedureEntry> {
        self.define_procedure_impl(entry, false)
    }

    /// Defines a procedure, allowing redefinition if specified.
    ///
    /// `allow_redefinition` should be true for actual SUB/FUNCTION definitions
    /// that may follow a DECLARE statement.
    pub fn define_procedure_allow_redef(
        &mut self,
        entry: ProcedureEntry,
    ) -> Result<(), ProcedureEntry> {
        self.define_procedure_impl(entry, true)
    }

    fn define_procedure_impl(
        &mut self,
        entry: ProcedureEntry,
        allow_redefinition: bool,
    ) -> Result<(), ProcedureEntry> {
        let name_upper = entry.name.to_uppercase();

        if let Some(existing) = self.procedures.get(&name_upper)
            && !allow_redefinition
        {
            return Err(existing.clone());
        }

        self.procedures.insert(name_upper, entry);
        Ok(())
    }

    /// Looks up a procedure by name.
    ///
    /// In BASIC, type suffixes (`%`, `$`, `&`, `!`, `#`) are part of the procedure
    /// name but optional when calling. If exact match fails, tries with common
    /// suffixes to support calling `ACCEPT` when declared as `ACCEPT%`.
    pub fn lookup_procedure(&self, name: &str) -> Option<&ProcedureEntry> {
        let name_upper = name.to_uppercase();

        // Try exact match first
        if let Some(proc) = self.procedures.get(&name_upper) {
            return Some(proc);
        }

        // If not found and name doesn't already have a suffix, try with suffixes
        let last_char = name_upper.chars().last();
        let has_suffix = matches!(last_char, Some('%' | '$' | '&' | '!' | '#' | '`'));

        if !has_suffix {
            // Try with common type suffixes
            for suffix in ['%', '&', '!', '#', '$', '`'] {
                let name_with_suffix = format!("{}{}", name_upper, suffix);
                if let Some(proc) = self.procedures.get(&name_with_suffix) {
                    return Some(proc);
                }
            }

            // Also try QB64 extended suffixes
            for suffix in ["%%", "&&", "##"] {
                let name_with_suffix = format!("{}{}", name_upper, suffix);
                if let Some(proc) = self.procedures.get(&name_with_suffix) {
                    return Some(proc);
                }
            }

            // Try unsigned type suffixes (QB64 extension: ~% ~& ~%% ~&& ~`)
            for suffix in ["~%", "~&", "~%%", "~&&", "~`"] {
                let name_with_suffix = format!("{}{}", name_upper, suffix);
                if let Some(proc) = self.procedures.get(&name_with_suffix) {
                    return Some(proc);
                }
            }
        }

        None
    }

    /// Registers a SHARED variable for the current scope.
    ///
    /// When in a procedure, this allows access to the named global variable.
    pub fn add_shared_var(&mut self, name: String) {
        let shared = self.shared_vars.entry(self.current_scope).or_default();
        shared.push(name);
    }

    /// Registers a module-shared variable (from DIM SHARED at module level).
    ///
    /// These variables are automatically visible from all procedures without
    /// needing an explicit SHARED statement inside the procedure.
    pub fn add_module_shared_var(&mut self, name: String) {
        // Use full name including suffix since a$ and a& are different variables
        let name_upper = name.to_uppercase();
        self.module_shared_vars.insert(name_upper);
    }

    /// Defines a symbol as shared at module level.
    ///
    /// This is used for REDIM SHARED statements. The symbol is:
    /// 1. Defined in the global scope (always, regardless of current scope)
    /// 2. Registered as module-shared so it's visible from all procedures
    pub fn define_shared_symbol(&mut self, symbol: Symbol) {
        let name_upper = symbol.name.to_uppercase();
        let is_array = matches!(symbol.kind, SymbolKind::ArrayVariable { .. });

        // Always define in global scope, in the appropriate namespace
        let global = self
            .scopes
            .get_mut(&ScopeId::GLOBAL)
            .expect("global scope must always exist");

        if is_array {
            global.arrays.insert(name_upper.clone(), symbol);
        } else {
            global.scalars.insert(name_upper.clone(), symbol);
        }
        // Mark as module-shared
        self.module_shared_vars.insert(name_upper);
    }

    /// Checks if a variable is a module-level SHARED variable.
    ///
    /// Returns true if the variable was declared with DIM SHARED or REDIM SHARED
    /// at module level, making it visible to all procedures.
    pub fn is_module_shared(&self, name: &str) -> bool {
        self.module_shared_vars.contains(&name.to_uppercase())
    }

    /// Updates a module-level SHARED symbol (array) in the global scope.
    ///
    /// This is used for REDIM _PRESERVE on SHARED arrays from within a SUB/FUNCTION.
    /// The array is updated in the global scope, not the current local scope.
    pub fn update_shared_symbol(&mut self, symbol: Symbol) {
        let name_upper = symbol.name.to_uppercase();
        let is_array = matches!(symbol.kind, SymbolKind::ArrayVariable { .. });

        // Update in global scope, in the appropriate namespace
        let global = self
            .scopes
            .get_mut(&ScopeId::GLOBAL)
            .expect("global scope must always exist");

        if is_array {
            global.arrays.insert(name_upper, symbol);
        } else {
            global.scalars.insert(name_upper, symbol);
        }
    }

    /// Looks up a symbol specifically in the global scope.
    ///
    /// This is used to validate SHARED statements - the variable must exist
    /// at module level to be shared. Checks both namespaces.
    ///
    /// This also handles suffix-to-base-name matching for compatibility.
    pub fn lookup_global_symbol(&self, name: &str) -> Option<&Symbol> {
        // Use full name including suffix since a$ and a& are different variables
        let name_upper = name.to_uppercase();
        let global = self.scopes.get(&ScopeId::GLOBAL)?;

        // Check scalars first, then arrays
        if let Some(sym) = global.scalars.get(&name_upper) {
            return Some(sym);
        }
        if let Some(sym) = global.arrays.get(&name_upper) {
            return Some(sym);
        }

        // Fallback: If name has a type suffix, try looking up the base name
        if let Some(suffix_type) = types::type_from_suffix(&name_upper) {
            let base_name = types::strip_suffix(&name_upper).to_uppercase();
            if base_name != name_upper {
                if let Some(sym) = global.scalars.get(&base_name)
                    && suffix_matches_type(&suffix_type, &sym.basic_type)
                {
                    return Some(sym);
                }
                if let Some(sym) = global.arrays.get(&base_name) {
                    if let BasicType::Array { element_type, .. } = &sym.basic_type {
                        if suffix_matches_type(&suffix_type, element_type) {
                            return Some(sym);
                        }
                    } else if suffix_matches_type(&suffix_type, &sym.basic_type) {
                        return Some(sym);
                    }
                }
            }
        }

        None
    }

    /// Gets the default type for a variable based on its first letter.
    ///
    /// By default, all variables are SINGLE. DEFtype statements change this.
    /// Type suffixes are stripped before determining the first letter,
    /// since DEFtype only affects variables WITHOUT explicit suffixes.
    pub fn default_type_for(&self, name: &str) -> BasicType {
        let base_name = strip_suffix_for_deftype(name);
        let first = base_name.chars().next().unwrap_or('A').to_ascii_uppercase();

        if first.is_ascii_uppercase() {
            self.default_types[(first as usize) - ('A' as usize)].clone()
        } else {
            BasicType::Single
        }
    }

    /// Sets the default type for a range of first letters (DEFtype).
    ///
    /// For example, `set_default_type('I', 'N', BasicType::Integer)` makes
    /// variables starting with I-N default to INTEGER.
    pub fn set_default_type(&mut self, from: char, to: char, typ: BasicType) {
        let from_idx = (from.to_ascii_uppercase() as usize).saturating_sub('A' as usize);
        let to_idx = (to.to_ascii_uppercase() as usize).saturating_sub('A' as usize);

        for i in from_idx..=to_idx.min(25) {
            self.default_types[i] = typ.clone();
        }
    }

    /// Gets the OPTION BASE setting (default array lower bound).
    pub fn option_base(&self) -> i64 {
        self.option_base
    }

    /// Sets the OPTION BASE (0 or 1).
    pub fn set_option_base(&mut self, base: i64) {
        self.option_base = base;
    }

    /// Gets the OPTION _EXPLICIT mode.
    pub fn explicit_mode(&self) -> bool {
        self.explicit_mode
    }

    /// Sets the OPTION _EXPLICIT mode.
    pub fn set_explicit_mode(&mut self, enabled: bool) {
        self.explicit_mode = enabled;
    }

    /// Gets the OPTION _EXPLICITARRAY mode.
    pub fn explicit_array_mode(&self) -> bool {
        self.explicit_array_mode
    }

    /// Sets the OPTION _EXPLICITARRAY mode.
    pub fn set_explicit_array_mode(&mut self, enabled: bool) {
        self.explicit_array_mode = enabled;
    }

    /// Defines a user-defined TYPE.
    ///
    /// Returns `Err(existing)` if a type with this name already exists.
    pub fn define_user_type(
        &mut self,
        definition: UserTypeDefinition,
    ) -> Result<(), UserTypeDefinition> {
        let name_upper = definition.name.to_uppercase();

        if let Some(existing) = self.user_types.get(&name_upper) {
            return Err(existing.clone());
        }

        self.user_types.insert(name_upper, definition);
        Ok(())
    }

    /// Looks up a user-defined TYPE by name.
    pub fn lookup_user_type(&self, name: &str) -> Option<&UserTypeDefinition> {
        self.user_types.get(&name.to_uppercase())
    }

    /// Looks up a field in a user-defined TYPE.
    ///
    /// Returns the member's type if found, None otherwise.
    pub fn lookup_type_member(&self, type_name: &str, field_name: &str) -> Option<BasicType> {
        let type_def = self.user_types.get(&type_name.to_uppercase())?;
        let field_upper = field_name.to_uppercase();

        for member in &type_def.members {
            if member.name.to_uppercase() == field_upper {
                return Some(member.basic_type.clone());
            }
        }
        None
    }

    /// Returns an iterator over all procedures (SUBs and FUNCTIONs).
    ///
    /// Used by the LSP server for document symbols/outline.
    pub fn iter_procedures(&self) -> impl Iterator<Item = &ProcedureEntry> {
        self.procedures.values()
    }

    /// Returns an iterator over all user-defined TYPEs.
    ///
    /// Used by the LSP server for document symbols/outline.
    pub fn iter_user_types(&self) -> impl Iterator<Item = &UserTypeDefinition> {
        self.user_types.values()
    }

    /// Returns an iterator over all global symbols (variables at module level).
    ///
    /// Used by the LSP server for document symbols/outline. Returns symbols from
    /// both scalar and array namespaces.
    pub fn iter_global_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.scopes
            .get(&ScopeId::GLOBAL)
            .map(|s| s.scalars.values().chain(s.arrays.values()))
            .into_iter()
            .flatten()
    }

    /// Collects all variable names (scalars and arrays) from the current scope and parent scopes.
    ///
    /// This is used for error message suggestions when a variable is not found.
    /// Returns names in the order they would be searched (current scope first, then parents).
    pub fn collect_available_variable_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        let mut visited = std::collections::HashSet::new();

        // Walk up the scope chain
        let mut current = Some(self.current_scope);
        while let Some(scope_id) = current {
            if let Some(scope) = self.scopes.get(&scope_id) {
                // Collect scalar names
                for name in scope.scalars.keys() {
                    if visited.insert(name.clone()) {
                        names.push(name.clone());
                    }
                }
                // Collect array names
                for name in scope.arrays.keys() {
                    if visited.insert(name.clone()) {
                        names.push(name.clone());
                    }
                }
            }
            // Move to parent scope
            current = self.scopes.get(&scope_id).and_then(|s| s.parent);
        }

        names
    }

    /// Collects all label names from the current scope.
    ///
    /// Labels are scope-local, so only the current scope is searched.
    pub fn collect_available_label_names(&self) -> Vec<String> {
        self.scopes
            .get(&self.current_scope)
            .map(|scope| scope.labels.keys().cloned().collect())
            .unwrap_or_default()
    }

    /// Collects all procedure names (SUB and FUNCTION).
    ///
    /// Procedures are globally visible, so all procedures are returned.
    pub fn collect_available_procedure_names(&self) -> Vec<String> {
        self.procedures.keys().cloned().collect()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_and_lookup_symbol() {
        let mut table = SymbolTable::new();

        let symbol = Symbol {
            name: "x".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Integer,
            span: Span::new(0, 1, 1),
            is_mutable: true,
        };

        table.define_symbol(symbol).unwrap();

        let found = table.lookup_symbol("x").unwrap();
        assert_eq!(found.basic_type, BasicType::Integer);
    }

    #[test]
    fn test_case_insensitive_lookup() {
        let mut table = SymbolTable::new();

        let symbol = Symbol {
            name: "MyVar".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::String,
            span: Span::new(0, 5, 1),
            is_mutable: true,
        };

        table.define_symbol(symbol).unwrap();

        // All these should find the same symbol
        assert!(table.lookup_symbol("MyVar").is_some());
        assert!(table.lookup_symbol("MYVAR").is_some());
        assert!(table.lookup_symbol("myvar").is_some());
    }

    #[test]
    fn test_duplicate_symbol_error() {
        let mut table = SymbolTable::new();

        let sym1 = Symbol {
            name: "x".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Integer,
            span: Span::new(0, 1, 1),
            is_mutable: true,
        };

        let sym2 = Symbol {
            name: "X".to_string(), // Same name, different case
            kind: SymbolKind::Variable,
            basic_type: BasicType::Long,
            span: Span::new(10, 11, 1),
            is_mutable: true,
        };

        table.define_symbol(sym1).unwrap();
        let result = table.define_symbol(sym2);

        assert!(result.is_err());
    }

    #[test]
    fn test_scope_nesting() {
        let mut table = SymbolTable::new();

        // Define global variable
        let global_var = Symbol {
            name: "g".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Integer,
            span: Span::new(0, 1, 1),
            is_mutable: true,
        };
        table.define_symbol(global_var).unwrap();

        // Enter SUB scope
        table.enter_scope(ScopeKind::Sub);

        // Define local variable (shadows global? No, different name)
        let local_var = Symbol {
            name: "l".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::String,
            span: Span::new(10, 11, 1),
            is_mutable: true,
        };
        table.define_symbol(local_var).unwrap();

        // Local var is visible
        assert!(table.lookup_symbol("l").is_some());

        // Global var is NOT visible (no SHARED)
        assert!(table.lookup_symbol("g").is_none());

        // Exit scope
        table.exit_scope();

        // Now global is visible, local is not
        assert!(table.lookup_symbol("g").is_some());
        assert!(table.lookup_symbol("l").is_none());
    }

    #[test]
    fn test_shared_variable() {
        let mut table = SymbolTable::new();

        // Define global variable
        let global_var = Symbol {
            name: "shared_g".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Integer,
            span: Span::new(0, 8, 1),
            is_mutable: true,
        };
        table.define_symbol(global_var).unwrap();

        // Enter SUB scope
        table.enter_scope(ScopeKind::Sub);

        // Without SHARED, global is not visible
        assert!(table.lookup_symbol("shared_g").is_none());

        // Add SHARED declaration
        table.add_shared_var("shared_g".to_string());

        // Now it's visible
        let found = table.lookup_symbol("shared_g").unwrap();
        assert_eq!(found.basic_type, BasicType::Integer);

        table.exit_scope();
    }

    #[test]
    fn test_define_procedure() {
        let mut table = SymbolTable::new();

        let proc = ProcedureEntry {
            name: "MySub".to_string(),
            kind: ProcedureKind::Sub,
            params: vec![],
            return_type: None,
            span: Span::new(0, 5, 1),
            is_static: false,
        };

        table.define_procedure(proc).unwrap();

        // Case-insensitive lookup
        assert!(table.lookup_procedure("MYSUB").is_some());
        assert!(table.lookup_procedure("mysub").is_some());
    }

    #[test]
    fn test_default_types() {
        let mut table = SymbolTable::new();

        // Default is SINGLE
        assert_eq!(table.default_type_for("x"), BasicType::Single);

        // Set I-N to INTEGER (classic DEFINT I-N)
        table.set_default_type('I', 'N', BasicType::Integer);

        assert_eq!(table.default_type_for("i"), BasicType::Integer);
        assert_eq!(table.default_type_for("Index"), BasicType::Integer);
        assert_eq!(table.default_type_for("n"), BasicType::Integer);
        assert_eq!(table.default_type_for("x"), BasicType::Single); // unchanged
    }

    #[test]
    fn test_labels() {
        let mut table = SymbolTable::new();

        table
            .define_label("start".to_string(), Span::new(0, 5, 1))
            .unwrap();

        assert!(table.lookup_label("START").is_some());
        assert!(table.lookup_label("start").is_some());

        // Duplicate label should fail
        let result = table.define_label("Start".to_string(), Span::new(10, 15, 1));
        assert!(result.is_err());
    }

    #[test]
    fn test_type_suffix_differentiation() {
        let mut table = SymbolTable::new();

        // In BASIC, variables with different suffixes are DIFFERENT variables
        // Define a string variable: name$
        let symbol1 = Symbol {
            name: "name$".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::String,
            span: Span::new(0, 5, 1),
            is_mutable: true,
        };
        table.define_symbol(symbol1).unwrap();

        // name$ should only be found by name$ (case insensitive)
        assert!(table.lookup_symbol("name$").is_some(), "Should find name$");
        assert!(
            table.lookup_symbol("NAME$").is_some(),
            "Should find NAME$ (case insensitive)"
        );
        // name (no suffix) should NOT find name$
        assert!(
            table.lookup_symbol("name").is_none(),
            "Should NOT find via 'name' (different variable)"
        );
        assert!(
            table.lookup_symbol("NAME").is_none(),
            "Should NOT find via 'NAME' (different variable)"
        );

        // Defining with different suffix should SUCCEED (different variable!)
        let symbol2 = Symbol {
            name: "name%".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Integer,
            span: Span::new(10, 15, 1),
            is_mutable: true,
        };
        let result = table.define_symbol(symbol2);
        assert!(
            result.is_ok(),
            "Should succeed: name$ and name% are different variables"
        );

        // Both should be findable by their respective names
        assert!(table.lookup_symbol("name$").is_some(), "Should find name$");
        assert!(table.lookup_symbol("name%").is_some(), "Should find name%");

        // Defining with no suffix should also SUCCEED (another different variable)
        let symbol3 = Symbol {
            name: "name".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Single,
            span: Span::new(20, 24, 1),
            is_mutable: true,
        };
        let result = table.define_symbol(symbol3);
        assert!(
            result.is_ok(),
            "Should succeed: name, name$, name% are all different variables"
        );

        // All three should be findable
        assert!(
            table.lookup_symbol("name").is_some(),
            "Should find name (no suffix)"
        );
        assert!(table.lookup_symbol("name$").is_some(), "Should find name$");
        assert!(table.lookup_symbol("name%").is_some(), "Should find name%");
    }
}
