//! Symbol table for QB64Fresh semantic analysis.
//!
//! The symbol table tracks all named entities (variables, procedures, labels)
//! and their properties during semantic analysis. It supports:
//!
//! - **Nested scopes**: Global scope plus local scopes for SUB/FUNCTION
//! - **SHARED variables**: Access to module-level variables from procedures
//! - **Case-insensitive lookups**: Following BASIC tradition
//! - **DEFtype defaults**: Type defaults by variable first letter
//! - **Type suffix normalization**: `x%`, `x$`, and `x` map to base name "X"
//!
//! # Scope Rules
//!
//! BASIC has simpler scoping than most modern languages:
//! - Module-level (global) scope contains main program variables
//! - Each SUB/FUNCTION creates a local scope
//! - Variables in local scope shadow global variables (unless SHARED)
//! - Labels are scope-local (can't GOTO into/out of procedures)

use crate::ast::Span;
use crate::semantic::types::{BasicType, strip_suffix};
use std::collections::HashMap;

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
#[derive(Debug, Clone)]
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

/// The kind of procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcedureKind {
    /// SUB - no return value
    Sub,
    /// FUNCTION - returns a value
    Function,
    /// Built-in function (LEN, CHR$, etc.)
    BuiltIn,
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
#[derive(Debug)]
struct Scope {
    /// Unique scope identifier (kept for Debug output).
    #[allow(dead_code)]
    id: ScopeId,
    /// What kind of scope this is.
    kind: ScopeKind,
    /// Symbols in this scope (uppercase names for case-insensitive lookup).
    symbols: HashMap<String, Symbol>,
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
    shared_vars: HashMap<ScopeId, Vec<String>>,

    /// Default type by first letter (A-Z). Index 0 = 'A', etc.
    /// Default is SINGLE unless changed by DEFtype.
    default_types: [BasicType; 26],

    /// OPTION BASE setting (0 or 1, default 0).
    option_base: i64,

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
                symbols: HashMap::new(),
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
            default_types: std::array::from_fn(|_| BasicType::Single),
            user_types: HashMap::new(),
            option_base: 0,
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
                symbols: HashMap::new(),
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
    /// Returns `Err((existing, new))` if a symbol with this name already exists.
    ///
    /// Note: Type suffixes are stripped for lookup purposes, so `x%` and `x$`
    /// and `x` all map to the same base name. This follows BASIC convention
    /// where suffixes are type declarations, not part of the variable name.
    pub fn define_symbol(&mut self, symbol: Symbol) -> Result<(), Box<(Symbol, Symbol)>> {
        let scope = self.scopes.get_mut(&self.current_scope).unwrap();
        // Strip suffix for consistent lookups: x%, x$, x -> X
        let name_upper = strip_suffix(&symbol.name).to_uppercase();

        if let Some(existing) = scope.symbols.get(&name_upper) {
            return Err(Box::new((existing.clone(), symbol)));
        }

        scope.symbols.insert(name_upper, symbol);
        Ok(())
    }

    /// Looks up a symbol by name, searching up the scope chain.
    ///
    /// In BASIC, procedure scopes (SUB/FUNCTION) are isolated from global scope.
    /// Variables from global scope are only visible if explicitly SHARED.
    ///
    /// Type suffixes are stripped for lookups, so `x%`, `x$`, and `x` all
    /// resolve to the same symbol.
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        // Strip suffix for consistent lookups: x%, x$, x -> X
        let name_upper = strip_suffix(name).to_uppercase();
        let scope = self.scopes.get(&self.current_scope)?;

        // Check current scope first
        if let Some(sym) = scope.symbols.get(&name_upper) {
            return Some(sym);
        }

        // If in a procedure scope, only SHARED variables are visible from global
        if matches!(scope.kind, ScopeKind::Sub | ScopeKind::Function) {
            // Check if this variable is SHARED (strip suffix for comparison)
            if let Some(shared_names) = self.shared_vars.get(&self.current_scope)
                && shared_names
                    .iter()
                    .any(|n| strip_suffix(n).to_uppercase() == name_upper)
            {
                // Look up in global scope only
                if let Some(global) = self.scopes.get(&ScopeId::GLOBAL) {
                    return global.symbols.get(&name_upper);
                }
            }
            // Not SHARED, not found in local scope -> not visible
            return None;
        }

        // In global scope, only check global symbols (already did above)
        None
    }

    /// Checks if a symbol exists in the current scope only (not parent scopes).
    ///
    /// Type suffixes are stripped for lookups.
    pub fn symbol_in_current_scope(&self, name: &str) -> bool {
        // Strip suffix for consistent lookups: x%, x$, x -> X
        let name_upper = strip_suffix(name).to_uppercase();
        self.scopes
            .get(&self.current_scope)
            .map(|s| s.symbols.contains_key(&name_upper))
            .unwrap_or(false)
    }

    /// Defines a label in the current scope.
    ///
    /// Returns `Err(existing)` if a label with this name already exists.
    pub fn define_label(&mut self, name: String, span: Span) -> Result<(), LabelEntry> {
        let scope = self.scopes.get_mut(&self.current_scope).unwrap();
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
    /// Returns `Err(existing)` if a procedure with this name already exists.
    pub fn define_procedure(&mut self, entry: ProcedureEntry) -> Result<(), ProcedureEntry> {
        let name_upper = entry.name.to_uppercase();

        if let Some(existing) = self.procedures.get(&name_upper) {
            return Err(existing.clone());
        }

        self.procedures.insert(name_upper, entry);
        Ok(())
    }

    /// Looks up a procedure by name.
    pub fn lookup_procedure(&self, name: &str) -> Option<&ProcedureEntry> {
        self.procedures.get(&name.to_uppercase())
    }

    /// Registers a SHARED variable for the current scope.
    ///
    /// When in a procedure, this allows access to the named global variable.
    pub fn add_shared_var(&mut self, name: String) {
        let shared = self.shared_vars.entry(self.current_scope).or_default();
        shared.push(name);
    }

    /// Gets the default type for a variable based on its first letter.
    ///
    /// By default, all variables are SINGLE. DEFtype statements change this.
    /// Type suffixes are stripped before determining the first letter.
    pub fn default_type_for(&self, name: &str) -> BasicType {
        let base_name = strip_suffix(name);
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
            span: Span::new(0, 1),
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
            span: Span::new(0, 5),
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
            span: Span::new(0, 1),
            is_mutable: true,
        };

        let sym2 = Symbol {
            name: "X".to_string(), // Same name, different case
            kind: SymbolKind::Variable,
            basic_type: BasicType::Long,
            span: Span::new(10, 11),
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
            span: Span::new(0, 1),
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
            span: Span::new(10, 11),
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
            span: Span::new(0, 8),
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
            span: Span::new(0, 5),
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
            .define_label("start".to_string(), Span::new(0, 5))
            .unwrap();

        assert!(table.lookup_label("START").is_some());
        assert!(table.lookup_label("start").is_some());

        // Duplicate label should fail
        let result = table.define_label("Start".to_string(), Span::new(10, 15));
        assert!(result.is_err());
    }

    #[test]
    fn test_type_suffix_stripping() {
        let mut table = SymbolTable::new();

        // Define a variable with a type suffix
        let symbol = Symbol {
            name: "name$".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::String,
            span: Span::new(0, 5),
            is_mutable: true,
        };
        table.define_symbol(symbol).unwrap();

        // Should be findable by any variant (suffix stripped for lookup)
        assert!(table.lookup_symbol("name$").is_some(), "Should find name$");
        assert!(
            table.lookup_symbol("name").is_some(),
            "Should find via name (no suffix)"
        );
        assert!(
            table.lookup_symbol("NAME").is_some(),
            "Should find via NAME (case insensitive)"
        );

        // Defining with different suffix should fail (same base name)
        let symbol2 = Symbol {
            name: "name%".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Integer,
            span: Span::new(10, 15),
            is_mutable: true,
        };
        let result = table.define_symbol(symbol2);
        assert!(
            result.is_err(),
            "Should fail: name$ and name% have same base name"
        );

        // Defining with no suffix should also fail
        let symbol3 = Symbol {
            name: "name".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Single,
            span: Span::new(20, 24),
            is_mutable: true,
        };
        let result = table.define_symbol(symbol3);
        assert!(
            result.is_err(),
            "Should fail: name and name$ have same base name"
        );
    }
}
