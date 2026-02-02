# Typed IR Contract

This document formalizes the **typed intermediate representation (typed IR)** as the single contract between the **semantic** phase and the **code generation** phase of QB64Fresh. It specifies which IR variants codegen must handle, what invariants semantic guarantees, and what codegen may assume. Use it to avoid regressions when adding language features and to keep semantic and codegen in sync.

**Source of truth for IR types:** `src/semantic/typed_ir.rs`  
**Expression codegen:** `src/codegen/c_backend/expr/mod.rs`  
**Statement codegen:** `src/codegen/c_backend/stmt/mod.rs` (and submodules)

---

## 1. Role of the Typed IR

- **Input to codegen:** A `TypedProgram` (list of `TypedStatement`s). Each statement and expression carries type information (`BasicType`) and source location (`Span`).
- **Produced by:** Semantic analysis (`SemanticAnalyzer::analyze`) only when type checking succeeds (no semantic errors). Codegen is **not** invoked if semantic returns `Err`.
- **Contract:** Semantic guarantees the invariants below; codegen may rely on them and must handle every variant it is dispatched (see sections 2 and 3).

---

## 2. TypedExprKind Variants Codegen Must Handle

The expression emitter (`ExprEmitCtx::emit`) **must** have a match arm for every `TypedExprKind` variant. The following list is the full set; adding a new variant requires both semantic (to produce it) and codegen (to emit it).

| Variant | Description |
|--------|-------------|
| `IntegerLiteral(i64)` | Integer literal |
| `FloatLiteral(f64)` | Float literal |
| `StringLiteral(String)` | String literal |
| `Variable(String)` | Scalar variable reference |
| `Binary { left, op, right }` | Binary operation (arithmetic, comparison, string concat, etc.) |
| `Unary { op, operand }` | Unary operation |
| `Grouped(Box<TypedExpr>)` | Parenthesized expression |
| `FunctionCall { name, args, params }` | Function call (built-in or user-defined); `params` for BYREF |
| `ArrayAccess { name, indices, dimensions }` | Array element access; `dimensions` for linear index |

**Array vs function call:** In the AST, both array access (`arr(i)`) and function calls (`fn(x)`) are represented as `ExprKind::FunctionCall`. The semantic phase disambiguates using the symbol table (array namespace first, then procedure namespace) and produces either `ArrayAccess` or `FunctionCall` in typed IR. See [ARCHITECTURE.md — Array vs function call resolution](../ARCHITECTURE.md#array-vs-function-call-resolution).
| `ArrayRef { name, element_type, dimensions }` | Whole-array reference (e.g. argument to SUB) |
| `Convert { expr, to_type }` | Explicit conversion (inserted by type checker) |
| `FieldAccess { object, field }` | UDT field access |
| `ExternalFunctionCall { name, c_name, args, params }` | DECLARE LIBRARY function call |
| `ProcPtr { name, wrapper_name, params, return_type }` | `_PROCPTR(procedureName)` |
| `CvFunc { target_type, value }` | `_CV` (string bytes → typed value) |
| `MkDollarFunc { source_type, value }` | `_MK$` (typed value → string bytes) |
| `CastFunc { target_type, value }` | `_CAST` |
| `ValWithType { value, target_type }` | `VAL(string$, _INTEGER64)` etc. |
| `MemGetTyped { mem, offset, target_type }` | `_MEMGET(mem, offset, AS type)` |

**Note:** Constant folding may rewrite some `Binary`/`Unary`/`FunctionCall`/`Grouped` expressions before emission; the emitter still must handle the unfolded form.

---

## 3. TypedStatementKind Variants and Codegen Dispatch

Statement emission is dispatched by **kind** to dedicated modules. Every `TypedStatementKind` variant must be covered by one of the match arms in `emit_statement` (or equivalent). Variants are grouped below by **target module**; adding a new statement kind requires adding it to the appropriate arm and implementing emission in that module.

- **assignments** — `Assignment`, `ArrayAssignment`, `ArrayFieldAssignment`, `FieldAssignment`, `MidAssignment`, `AscAssignment`
- **control_flow** — `If`, `SelectCase`, `SelectEveryCase`, `For`, `While`, `DoLoop`, `Goto`, `Gosub`, `Return`, `Exit`, `End`, `Stop`, `System`, `Sleep`, `Wait`, `Delay`, `Limit`, `Erase`, `KeyClear`
- **io** — `Print`, `PrintUsing`, `Input`, `LineInput`
- **definitions** — `SubDefinition`, `FunctionDefinition`, `Dim`, `Const`, `DefType`, `Define`, `OptionBase`, `OptionExplicit`, `OptionExplicitArray`, `CommonStmt`, `SharedStmt`, `StaticStmt`, `Redim`, `TypeDefinition`, `DeclareLibrary`
- **data** — `Read`, `Restore`, `Randomize`
- **call** — `Call`
- **error_jump** — `OnErrorGoto`, `OnErrorResumeNext`, `ResumeStmt`, `ErrorStmt`, `OnGoto`, `OnGosub`
- **def_fn** — `DefFn`, `DefFnMultiLine`
- **file_io** — `OpenFile`, `OpenFileLegacy`, `CloseFile`, `LockFile`, `UnlockFile`, `FilePrint`, `FileWrite`, `FileInput`, `FileLineInput`, `FileGet`, `FilePut`, `FileSeek`
- **graphics** — `Screen`, `Cls`, `Color`, `Locate`, `Pset`, `Preset`, `Line`, `Circle`, `Paint`, `GfxDisplay`, and other graphics-related kinds
- **audio** — `Beep`, `SoundStmt`, `PlayStmt`, `SndClose`, `SndPlay`, etc.
- **system** — `Kill`, `Rename`, `Mkdir`, `Rmdir`, `Chdir`, `Environ`, `ShellCmd`, `ShellHide`, `Bload`, `Bsave`, `Setmem`, `CallAbsolute`, `MouseHide`, `MouseShow`, `MouseMoveStmt`, `ClipboardSet`, `DeclareLibrary`, `DeclareSub`, `DeclareFunction`
- **misc** — `DefSeg`, `Poke`, `MemPutTyped`, `Label`, `Comment`, `Expression`, `IncludeDirective`, `ConditionalBlock`, `ConditionalBlockResolved`, `Data`, `Swap`, `Continue`, `Run`, `Chain`, `Tron`, `Troff`, `Lprint`, `FilesStmt`, `FieldStmt`, `Lset`, `Rset`, event/timer/port/assert stubs, etc.
- **meta** — `MetaCommand`, `MetaLet`, `MetaChecking`, `MetaConsole`, `MetaScreenHide`, `MetaScreenShow`, and other `Meta*` kinds

Some variants (e.g. many graphics/audio/meta) may be emitted as no-ops or comments when the runtime does not support them; the contract is that **every variant is handled** (no fall-through to an unreachable or panic path).

---

## 4. Invariants Semantic Guarantees (at Codegen Entry)

Semantic analysis guarantees the following when it returns `Ok(TypedProgram)`:

1. **No `BasicType::Unknown` in emitted expressions**  
   Every expression that reaches codegen has a resolved type. `Unknown` is used only internally (e.g. during inference or for DECLARE LIBRARY `ANY` parameters); by the time typed IR is produced for normal execution paths, types are concrete. Codegen does not need to special-case `Unknown` for expressions it emits.

2. **Array dimensions resolved**  
   For `ArrayAccess`, `ArrayAssignment`, `ArrayFieldAssignment`, and similar nodes, the `dimensions` (or equivalent) field is populated with `TypedArrayDimension` bounds. Codegen may use them to compute linear indices (row-major) without re-analyzing.

3. **Procedure and symbol resolution**  
   - All called SUBs/FUNCTIONs and referenced variables/labels are resolved (declared and in scope).  
   - `FunctionCall.params` and `Call.params` match the procedure’s parameter list (BYREF/BYVAL and count).  
   - Labels referenced by `Goto`/`Gosub`/`Restore` exist (semantic has collected them).

4. **Type consistency**  
   - Assignments have compatible types; implicit conversions are already represented as `Convert` nodes or as `target_type`/`element_type`/`field_type` on assignment statements.  
   - Loop variable type in `For` matches `start`/`end`/`step` (after conversions).  
   - PRINT/INPUT targets and READ targets have known types.

5. **No duplicate or conflicting definitions in typed IR**  
   Semantic has enforced single definition for symbols in scope; codegen can assume unique names per scope (module/symbol table handles naming).

---

## 5. What Codegen May Assume

- **Types:** Every `TypedExpr.basic_type` and statement-level type field is a concrete type (no `Unknown`) for the code paths that are emitted.
- **Arrays:** `dimensions` and element types are set for array access and array assignments; codegen does not need to infer bounds.
- **Control flow:** Labels and DATA labels referenced in the program exist in the typed IR or in the analysis pass (e.g. DATA/READ/RESTORE).
- **C names:** Procedure and variable names have been mapped to C-safe identifiers where needed (e.g. `variable_renames`, `c_function_name`); codegen uses the names provided in the typed IR and context.
- **Order and structure:** Statements appear in program order; nested blocks (e.g. `If` then/else, `For` body) are already structured. Codegen does not need to re-parse or re-order.

---

## 6. Adding a New Language Feature (Checklist)

When adding a new construct that affects the typed IR:

1. **AST** — Add the construct in `src/ast/` (e.g. new `StatementKind` or `ExprKind`).
2. **Parser** — Parse it and produce the AST node.
3. **Typed IR** — Add the corresponding `TypedStatementKind` or `TypedExprKind` variant in `src/semantic/typed_ir.rs`.
4. **Semantic** — In the type checker, produce the new typed variant and ensure all invariants above (no `Unknown` in emitted paths, dimensions resolved, etc.).
5. **Codegen** — Add a match arm (or extend an existing arm) in:
   - `src/codegen/c_backend/expr/mod.rs` for new `TypedExprKind`, or  
   - `src/codegen/c_backend/stmt/mod.rs` (and the appropriate submodule) for new `TypedStatementKind`.
6. **Contract** — Update this document if you add a new variant or a new guarantee/assumption.

Running the test suite and (where applicable) QB45 compatibility tests helps catch missing arms or invariant violations.

---

*Last updated: 2026-01-31*
