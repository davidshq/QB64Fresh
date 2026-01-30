# Session 088: GDB debug of QB64pe external-runtime crash

**Date:** 2026-01-29

## Goal

Use a C debugger to find why QB64pe (compiled with qb64fresh --runtime external) crashes with "suspicious length" and heap corruption.

## Steps

1. **Built QB64pe C with debug symbols**  
   `gcc -g -O0 ... -o qb64pe_fresh_debug` (link against release runtime; debug runtime link timed out).

2. **Ran under GDB**  
   - Broke on `qb_string_len`, used conditional check: if the "len" field at `(ptr-16)` (QbString header layout) is > 1GB, print backtrace.  
   - Confirmed bad pointers: e.g. `ptr=0x5555563e06a8`, `len_field=296892688389990854`.

3. **Backtrace at first bad call**
   - `#0 qb_string_len`
   - `#1 qb_len_str` (generated helper)
   - `#2 qb_readchunk_str` (generated SUB)
   - `#3 qb_sub_gl_scan_header`
   - `#4 main`

4. **Cause**
   - Bad pointers (`0x5555563e06a8`, `0x5555563e0708`, etc.) are in the executable’s **data segment** (not heap).
   - So `qb_string_len` is being called with the **address of a global variable** (e.g. `&a_str_scalar`, `&l_str_scalar`) instead of the **value** (a valid `QbString*`).
   - That implies somewhere we assign `&some_string_variable` to a string variable, so a string slot holds an address instead of a handle.

5. **Where to fix**
   - **Codegen:** Ensure we never assign the address of a string variable to another string variable. When generating RHS for string assignment, emit the **value** (variable name or expression), not `&variable`.
   - **Inline runtime:** Many uses of `&_qbs_empty` in `runtime/keyboard.rs` and `runtime/strings.rs` are wrong for the same reason: they should use `_qbs_empty` (the pointer value), not `&_qbs_empty` (address of the pointer). Those only affect **inline** runtime; QB64pe uses **external** runtime, so the current crash is from generated code, not from those inline helpers.

## Artifacts

- **GDB script:** `QB64pe/gdb_catch_bad.gdb` – break on `qb_string_len`, check `*(unsigned long*)($rdi - 16) > 1GB`, then `bt 20` and continue.
- **QbString header (64-bit):** `[ref_count: 8][len: 8][capacity: 8]` then data; pointer passed to runtime is the data pointer, so `len` is at `ptr - 16`.

## Next steps

- Search codegen for places that emit `&string_var` as the RHS of a string assignment or as an argument that is then stored in a string variable.
- Fix inline runtime `&_qbs_empty` → `_qbs_empty` for consistency and to avoid the same class of bug when using inline mode.

## Fix applied (same session)

**Root cause:** BYREF string parameters get a local that is already the value (`QbString* name = *name_ref`). When emitting a reference to that variable, we were dereferencing (`*name`), producing invalid pointers (address-of-storage interpreted as string handle).

**Change:** In `src/codegen/c_backend/expr.rs`, Variable emission: for BYREF string (String or FixedString), do not dereference—emit the variable name (value). For BYREF fixed-length strings the local is `char* name`; emit `qb_str_from_c(name)` not `qb_str_from_c(*name)`. Cross-reference added in `stmt/definitions.rs` (emit_byref_copies).

**Verified:** Call/function argument emission adds `&` only when `is_byref`; assignment RHS uses `emit_expr` (value); file I/O and LSET/RSET use value vs address correctly per runtime signatures. No other codegen sites were emitting `&string_var` where a string value is required.
