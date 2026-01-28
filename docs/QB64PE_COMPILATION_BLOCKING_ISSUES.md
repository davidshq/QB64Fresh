# QB64pe Compilation with QB64Fresh - Blocking Issues

**Date:** 2026-01-28  
**Status:** 69 C compilation errors remaining (down from 807, 91% reduction)  
**Goal:** Compile and run QB64pe using QB64Fresh

---

## Error Categories

### 1. Type System Issues
- Type compatibility between `qbt_ParseNum*` and `QbString*` (partially fixed in 1183a3a)
- Verify ParseNum UDT maps to `qbt_ParseNum` in generated C
- Additional UDT mappings and conversion helpers may be needed

### 2. Runtime Function Signature Mismatches
**Problem:** Incompatible pointer types (`QbString*` vs `const char*`, struct mismatches)

**Examples:**
- `qb_removestringenclosingpair_str()` - argument type mismatch
- `strcpy()` - pointer type issues
- `qb_net_openclient()` - pointer type issues

**Fix:** Review `runtime/include/qb64fresh_rt.h`, update codegen for proper conversions

### 3. Function Pointer Assignment (1 error)
**Error:** `assignment to 'int32_t' from 'int32_t (*)(void)' makes integer from pointer without a cast`

**Fix:** Add explicit casts in codegen or handle function pointers as distinct type

---

## Missing Features

### Metacommands (Needs Verification)
- ⚠️ `$EXEICON` - Executable icon (parsed, codegen untested)
- ⚠️ `$VERSIONINFO` - Version information (parsed, codegen untested)
- ⚠️ `$DYNAMIC` - Dynamic arrays (syntax exists, commented in source)

### Runtime Functions (May Be Needed)
- String element functions: `qb_elementgetnumericvalue_lng()`, `qb_elementgetstringvalue_lng()`, `qb_elementisnumber_lng()`, `qb_elementisstring_lng()`, `qb_getelement_str()`, `qb_countfunctionelements()`
- Hash table: `qb_hashfind()`, `qb_sub_hashadd()` (const qualifier issues)
- Network: `qb_net_openclient()` (pointer type issues)
- String processing: `qb_removestringenclosingpair_str()` (pointer type issues)

---

## Code Generation Issues

### String-Based Structured Data
QB64pe uses strings to pass structured data (e.g., `args AS STRING` in `EvaluateFunction$`). QB64Fresh treats these as regular strings, but runtime expects structured access.

**Fix:** Recognize special string types, generate appropriate struct access, map `qbt_ParseNum` types

### Complex Type Conversions
Frequent conversions between: string representations of numbers, structured data in strings, function pointers, array access patterns.

**Fix:** Improve type inference, add runtime helpers, better variant type handling

---

## Fix Priority

### Priority 1: High Impact
1. **Runtime function signature mismatches** - Fix const qualifiers, add type conversions
2. **Function pointer handling** - Add explicit casts in codegen
3. **Type system issues** - Verify ParseNum mapping, fix compatibility issues

### Priority 2: Nice to Have
4. **Metacommand verification** - Test `$EXEICON` and `$VERSIONINFO` codegen

---

## Testing Strategy

1. **Incremental:** Start with `EvaluateFunction$` in isolation, test string element functions, expand gradually
2. **Runtime:** Build library, verify signatures match generated code, test memory management
3. **Integration:** Compile `qb64pe.bas`, link runtime, run basic functionality tests

---

## Related Files

**Source Files:**
- `src/codegen/c_backend/expr.rs` - Function calls, arguments, assignments
- `src/codegen/c_backend/types.rs` - Type mapping and conversions
- `src/semantic/types.rs` - Type inference, function pointers
- `runtime/include/qb64fresh_rt.h` - Runtime declarations
- `runtime/src/` - Runtime implementations

**Reference:**
- `QB64pe/source/qb64pe.bas` - Main source
- `QB64pe/source/utilities/const_eval.bas` - Contains `EvaluateFunction$`
- `/tmp/qb64pe_compiled.c` - Generated C (for analysis)

---

## Next Steps

1. Fix type system issues - Verify ParseNum UDT mapping, test struct field access (`.typ`, `.f`, `.i`, `.ui`, `.s`)
2. Implement runtime functions - Add string element functions if needed, fix const qualifiers
3. Update codegen - Handle structured strings, generate proper conversions, add function pointer casts
4. Iterate - Fix remaining 69 errors incrementally, test after each change

---

**Last Updated:** 2026-01-28
