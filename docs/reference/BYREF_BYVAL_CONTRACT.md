# BYREF vs BYVAL: Type System and Call-Site Contract

This document clarifies where BYREF/BYVAL is represented and how all call sites (codegen, LSP hover) must use it consistently so that future features (e.g., BYREF UDTs) do not introduce subtle bugs.

## Naming convention

- **`by_val: bool`** — Used for **SUB/FUNCTION** parameters and callback params.  
  `true` = BYVAL, `false` = BYREF.
- **`is_byval: bool`** — Used for **DECLARE LIBRARY** (external) parameters and related IR.  
  Same meaning: `true` = BYVAL, `false` = BYREF.

Use the field that matches the parameter kind; do not mix. When adding new BYREF/BYVAL-aware code, read from the appropriate struct (see below).

## Where the flags live

### SUB/FUNCTION (BASIC procedures)

| Layer        | Type / Symbol              | Field    | Notes                                      |
|-------------|-----------------------------|----------|--------------------------------------------|
| AST         | `Parameter` (stmt.rs)        | `by_val` | Parsed from `BYVAL` / `BYREF` keyword.     |
| Typed IR    | `TypedParameter`            | `by_val` | Carried through to codegen.                |
| Symbols     | `SymbolKind::Parameter`     | `by_val` | Used for LSP hover and symbol resolution.  |
| Symbols     | `ParameterInfo`             | `by_val` | ProcedureEntry params (analyzer).          |
| Symbols     | `CallbackParam` (typed_ir)  | `by_val` | _PROCPTR callback params.                   |

### DECLARE LIBRARY (external procedures)

| Layer        | Type                       | Field     | Notes                                      |
|-------------|----------------------------|-----------|--------------------------------------------|
| AST         | `ExternalParam` (stmt.rs)  | `is_byval`| DECLARE LIBRARY params; parsed from DECLARE. |
| Typed IR    | `TypedExternalParam`       | `is_byval`| Used by codegen for C signature.           |
| Typed IR    | `ExternalParamInfo`        | `is_byval`| Used at call sites for marshalling.        |
| Symbols     | `SymbolKind::ExternalFunction` | `params: Vec<BasicType>` | **No per-param BYVAL**; types only. |

### Codegen state (procedure body)

`ProcedureState` (codegen/c_backend/stmt/state.rs) derives BYREF sets from the current procedure’s `TypedParameter` list:

- `current_func_byref_strings` / `current_func_byref_string_basic_names`
- `current_func_byref_scalar_names`
- `current_func_byref_udt_names`

All are populated from `params` where `!p.by_val` (and type/array filters). Do not introduce separate BYREF logic; keep reading from `p.by_val` on `TypedParameter`.

## Call-site audit: “Where do we read by_val?”

### Codegen (reads `by_val` or `is_byval`)

- **stmt/definitions.rs**  
  SUB/FUNCTION: `p.by_val` (TypedParameter) for param C types, byref copies, and byref sets.  
  DECLARE/external: `p.is_byval` (TypedExternalParam) for C param types.
- **stmt/call.rs**  
  SUB call args: byref when `!p.by_val && !p.is_array` (TypedParameter).
- **expr/calls.rs**  
  FUNCTION call args: same, `!p.by_val && !p.is_array`.
- **mod.rs**  
  Uses `param.by_val` (TypedParameter) where procedure params are handled.
- **implicit_vars.rs**  
  Uses `!p.by_val && !p.is_array` for byref param sets.

All of these consistently treat `by_val == false` as BYREF. No codegen path should infer BYREF from anything other than these fields.

### LSP hover (symbol → display string)

- **semantic/mod.rs** — `format_symbol_hover()`  
  For `SymbolKind::Parameter { by_val }`:
  - **Before:** Showed `"BYVAL "` when `by_val`, else no modifier (BYREF implicit).
  - **After:** Shows `"BYVAL "` when `by_val`, and `"BYREF "` when `!by_val`, so both modes are explicit and consistent with the type system.

Hover for SUB/FUNCTION parameters therefore always reflects the same `by_val` flag that codegen uses.

### External (DECLARE) hover gap

`SymbolKind::ExternalFunction` only stores `params: Vec<BasicType>`. It does **not** store per-param BYVAL/BYREF. Therefore:

- Hover for DECLARE LIBRARY procedures cannot show BYVAL/BYREF per parameter without extending the symbol (e.g. `params: Vec<(BasicType, bool)>` or a small struct).
- Codegen and Typed IR for external calls already use `TypedExternalParam.is_byval` / `ExternalParamInfo.is_byval` correctly; the gap is only in the symbol-based hover text.

Future work: if DECLARE hover should show BYVAL/BYREF per param, add that info to `SymbolKind::ExternalFunction` and to `format_symbol_hover()` (or the procedure-signature path in `get_hover_info`).

## Future features (e.g., BYREF UDTs)

- **Single source of truth:** SUB/FUNCTION param passing is determined by `TypedParameter.by_val` (and codegen state derived from it). Do not add alternate BYREF logic elsewhere.
- **LSP:** Any new UI that displays parameters (hover, signature help, completion) should show BYVAL/BYREF using the same `by_val`/`is_byval` fields so users see what the compiler will do.
- **New param-like constructs:** If we add BYREF UDTs or similar, use the same naming: `by_val: bool` for BASIC procedure params, `is_byval` only where the type is explicitly “external” (e.g. DECLARE), and document new read sites in this contract.

## Quick reference: which field to use

- **SUB/FUNCTION** (TypedParameter, ParameterInfo, SymbolKind::Parameter, CallbackParam): use **`by_val`**.
- **DECLARE / external** (TypedExternalParam, ExternalParamInfo, AST ExternalParam): use **`is_byval`**.
- **Codegen:** Prefer the type you already have (TypedParameter → `by_val`, TypedExternalParam → `is_byval`); do not mix or re-derive from another layer.
