# C Backend Refactoring Tasks

Technical debt identified in `src/codegen/c_backend/` that should be addressed before continuing with the bootstrap plan.

## Priority: High

### 1. Statement Traversal Duplication (Critical)

**Problem:** The same recursive "walk through If/For/While/DoLoop/SelectCase" pattern is copied 8-10 times across both files.

| File | Functions with identical traversal pattern |
|------|-------------------------------------------|
| `analysis.rs` | `collect_from_stmt`, `collect_redim_shared`, `collect_shared_vars`, `collect_data_from_stmt`, `scan_stmt` |
| `stmt.rs` | `collect_dims`, `collect_implicits`, `collect_byref_vars`, `collect_stmt_byref` |

Each function contains ~30-50 lines of nearly identical `match` arms:

```rust
TypedStatementKind::If { then_branch, elseif_branches, else_branch, .. } => {
    for s in then_branch { recurse(s, ...); }
    for (_, branch) in elseif_branches { for s in branch { recurse(s, ...); } }
    if let Some(else_stmts) = else_branch { for s in else_stmts { recurse(s, ...); } }
}
TypedStatementKind::For { body, .. } => {
    for s in body { recurse(s, ...); }
}
TypedStatementKind::While { body, .. } | TypedStatementKind::DoLoop { body, .. } => {
    for s in body { recurse(s, ...); }
}
TypedStatementKind::SelectCase { cases, case_else, .. } => {
    for case in cases { for s in &case.body { recurse(s, ...); } }
    if let Some(else_stmts) = case_else { for s in else_stmts { recurse(s, ...); } }
}
```

**Solution:** Create a statement visitor trait or fold/walk helper module.

```rust
// Example visitor trait approach
pub trait StmtVisitor {
    fn visit_stmt(&mut self, stmt: &TypedStatement) {
        self.walk_stmt(stmt);  // default: just recurse
    }

    fn walk_stmt(&mut self, stmt: &TypedStatement) {
        match &stmt.kind {
            TypedStatementKind::If { then_branch, elseif_branches, else_branch, .. } => {
                for s in then_branch { self.visit_stmt(s); }
                // ... etc
            }
            // All other compound statements
            _ => {}
        }
    }
}

// Usage: override visit_stmt, call walk_stmt for recursion
impl StmtVisitor for DataCollector {
    fn visit_stmt(&mut self, stmt: &TypedStatement) {
        if let TypedStatementKind::Data { values } = &stmt.kind {
            self.collect_data(values);
        }
        self.walk_stmt(stmt);  // continue recursion
    }
}
```

**Impact:** Would eliminate ~400 lines of duplicated code and make adding new statement types much easier (one place to update instead of 10).

---

### 2. Variable Declaration Pattern Duplicated

**Problem:** This pattern appears 6-7 times:

```rust
if !declared_vars.contains(&c_name) {
    if let BasicType::FixedString(len) = &basic_type {
        decls.push(format!("char {}[{}] = \"\";", c_name, len + 1));
    } else {
        let c_ty = c_type(&basic_type);
        let init = default_init(&basic_type);
        decls.push(format!("{} {} = {};", c_ty, c_name, init));
    }
    declared_vars.insert(c_name);
}
```

**Locations:**
- `analysis.rs`: `add_global` closure (~line 226), `add_var` closure (~line 529)
- `stmt.rs`: `collect_dims` (~line 4032), `collect_implicits` (~line 4146), `collect_byref_vars` (~line 4362, 4415), `declare_input_target` (~line 4616)

**Solution:** Extract to a helper function:

```rust
/// Declares a variable if not already declared, returning true if newly declared.
fn declare_var(
    name: &str,
    basic_type: &BasicType,
    declared_vars: &mut HashSet<String>,
    decls: &mut Vec<String>,
) -> bool {
    let c_name = c_identifier(name);
    if declared_vars.contains(&c_name) {
        return false;
    }

    let decl = match basic_type {
        BasicType::FixedString(len) => format!("char {}[{}] = \"\";", c_name, len + 1),
        BasicType::String => format!("qb_string* {} = NULL;", c_name),
        _ => {
            let c_ty = c_type(basic_type);
            let init = default_init(basic_type);
            format!("{} {} = {};", c_ty, c_name, init)
        }
    };

    decls.push(decl);
    declared_vars.insert(c_name);
    true
}
```

---

### 3. Built-in Constants List Duplicated

**Problem:** The same list of reserved names appears in two places:

- `analysis.rs:203-221` (in `collect_globals`)
- `stmt.rs:3988-4013` (in `collect_implicit_locals`)

```rust
declared_vars.insert("_TRUE".to_string());
declared_vars.insert("_FALSE".to_string());
declared_vars.insert("_EQUAL".to_string());
declared_vars.insert("_GREATER".to_string());
declared_vars.insert("_LESS".to_string());
declared_vars.insert("_STR_EMPTY".to_string());
// ... 15+ more entries
```

**Solution:** Create a const array and helper function:

```rust
/// Built-in constants and runtime variables that should never be redeclared.
const RESERVED_IDENTIFIERS: &[&str] = &[
    // Boolean constants
    "_TRUE", "_FALSE",
    // Comparison constants
    "_EQUAL", "_GREATER", "_LESS",
    // String constant macros
    "_STR_EMPTY", "_STR_CRLF", "_STR_LF", "_STR_CR",
    "_CHR_QUOTE", "_CHR_HT", "_CHR_LF",
    // LEN() dummy variables
    "dummy", "dummy_int_int", "dummy_int", "dummy_lng_lng",
    "dummy_sng", "dummy_dbl", "dummy_dbl_dbl", "dummy_int_lng",
];

fn add_reserved_identifiers(set: &mut HashSet<String>) {
    for &name in RESERVED_IDENTIFIERS {
        set.insert(name.to_string());
    }
}
```

---

## Priority: Medium

### 4. Massive Nested Function Block

**Problem:** `collect_implicit_locals` in `stmt.rs:3977-4640` contains **6 nested functions** spanning 660+ lines:

- `collect_dims` (95 lines)
- `collect_implicits` (180 lines)
- `collect_case_match_byref` (20 lines)
- `collect_byref_vars` (90 lines)
- `collect_stmt_byref` (170 lines)
- `declare_input_target` (25 lines)

**Issues:**
- Hard to test individual functions
- Hard to read and navigate
- Nested functions can't be called from other modules

**Solution:** Move to module-level functions (possibly in a new `implicit_vars.rs` submodule):

```rust
// src/codegen/c_backend/implicit_vars.rs
pub fn collect_implicit_locals(...) -> Vec<String> { ... }

fn collect_dims(...) { ... }
fn collect_implicits(...) { ... }
fn collect_byref_vars(...) { ... }
// etc.
```

---

### 5. `infer_type_from_name` Should Be Shared

**Problem:** Type inference from variable name suffix (`$` -> String, `%` -> Integer, etc.) is defined locally in `analysis.rs:417-434` but is a common BASIC concept that may be needed elsewhere.

**Solution:** Move to `types.rs` as a public utility:

```rust
// In types.rs
/// Infers BASIC type from variable name suffix.
///
/// | Suffix | Type     |
/// |--------|----------|
/// | `$`    | String   |
/// | `%`    | Integer  |
/// | `&`    | Long     |
/// | `!`    | Single   |
/// | `#`    | Double   |
/// | `\``   | Bit      |
/// | (none) | Single   |
pub fn infer_type_from_suffix(name: &str) -> BasicType {
    // ...
}
```

---

## Implementation Order

1. **Create `RESERVED_IDENTIFIERS` constant** - Quick win, 5 minutes
2. **Extract `declare_var` helper** - Medium effort, reduces duplication
3. **Create statement visitor trait** - Larger effort, biggest impact
4. **Move nested functions to module level** - Medium effort, improves testability
5. **Move `infer_type_from_suffix` to types.rs** - Quick win

---

## Notes

- These refactorings are purely internal - no changes to generated C code
- Can be done incrementally without breaking the bootstrap build
- Will make future statement additions much easier (e.g., adding `SELECT CASE` variants)
