# Session 065: GDB Debugging QB64PE Crash

**Date:** 2026-01-26
**Focus:** Debugging segmentation faults in QB64Fresh-compiled QB64PE binary

## Summary

Successfully used GDB (sandbox restrictions were not blocking it) to debug crashes in the QB64PE binary compiled with QB64Fresh. Identified and fixed one root cause, found a second issue requiring further investigation.

## Crash 1: Missing `qb_dir()` Declaration (FIXED)

### Symptoms
- Binary crashed immediately with SIGSEGV (exit code 139)
- Crash in `qb_string_concat()` called from `qb_sub_readinitialconfig()`
- `rdi` register showed truncated 32-bit pointer: `0x56256d38` instead of `0x555556256d38`

### Root Cause
The function `qb_dir()` was NOT declared in `runtime/include/qb64fresh_rt.h`. Without a declaration, C assumes `int` return type (32-bit), truncating the returned 64-bit `QbString*` pointer.

```c
// Before: qb_dir() not declared
// C compiler assumes: int qb_dir(...);  // Returns 32-bit int

// Actual signature in Rust:
pub unsafe extern "C" fn qb_dir(spec: *const QbString) -> *mut QbString  // 64-bit pointer
```

### Fix
Added declaration to `runtime/include/qb64fresh_rt.h`:
```c
/* Directory operations (CHDIR, MKDIR, RMDIR, _DIREXISTS, DIR$) */
int32_t qb_chdir(const char* path);
int32_t qb_mkdir(const char* path);
int32_t qb_rmdir(const char* path);
int32_t qb_dir_exists(const QbString* path);
QbString* qb_dir(const QbString* spec);  /* DIR$ - directory listing function */
```

### GDB Session (Key Commands Used)
```bash
# Run binary under GDB and get backtrace on crash
gdb -batch \
  -ex "set debuginfod enabled off" \
  -ex "run" \
  -ex "bt 30" \
  -ex "info registers" \
  /tmp/qb64pe_binary

# Output showed:
# #0  qb_string_concat ()
# #1  qb_sub_readinitialconfig () at /tmp/qb64pe_debug.c:43626
# rdi = 0x56256d38  (truncated!)
# rsi = 0x555556256ca8  (valid)
```

## Crash 2: String Memory Corruption (NEEDS INVESTIGATION)

### Symptoms
After fixing Crash 1, binary progresses further but crashes when finding the 'internal' folder:
- Crash in `qb_rtrim()` called from `qb_readchunk_str()` called from `qb_sub_gl_scan_header()`
- QbString header shows corrupted memory:
  - Refcount: `0x0000000555556515` (looks like truncated pointer)
  - Length: `0x0ac6ccadccf7e9d2` (garbage)
  - Capacity: `0x0000000000000039` = 57 (reasonable)

### Memory Dump
```
0x555556515360: 0x0000000555556515  0x0ac6ccadccf7e9d2  # refcount, length (corrupted)
0x555556515370: 0x0000000000000039  0x50414944474e4957  # capacity, "WINGDAIP..."
```

### Possible Causes
1. Another missing function declaration (pointer truncation)
2. Buffer overflow corrupting adjacent string header
3. Use-after-free (string freed but pointer still in use)
4. Double-free corrupting allocator metadata

### Additional Implicit Function Declarations Found
```bash
gcc -Werror=implicit-function-declaration /tmp/qb64pe_debug.c
# Errors:
# - qb_strig_check_event (returns int, probably OK)
# - qb_strig_event_done (void return, OK)
# - qb_shell_hide (should be qb_shellhide?)
```

## Key Learnings

### Implicit Function Declaration Bug Pattern
On 64-bit systems, when a function returning a pointer is called without declaration:
1. C assumes `int` return type (32-bit)
2. The 64-bit pointer return value gets truncated to 32 bits
3. Upper 32 bits are lost: `0x555556256d38` → `0x56256d38`
4. Accessing the truncated pointer causes SIGSEGV

### GDB Techniques Used
1. **Batch mode** for scripted debugging: `gdb -batch -ex "..." binary`
2. **Examining registers at crash**: `info registers rdi rsi`
3. **Memory examination**: `x/4gx $rdi-0x18` (examine 4 giant words at address)
4. **Source listing**: `list` (after crash with debug symbols)
5. **Full backtrace with locals**: `bt full`

### Detection Strategy
```bash
# Find all implicit function declarations (potential pointer truncation)
gcc -Werror=implicit-function-declaration file.c
```

## Files Modified

- `runtime/include/qb64fresh_rt.h` - Added `qb_dir()` declaration

## Next Steps

1. Search for more missing function declarations that return pointers
2. Investigate the string memory corruption in GL header scanning
3. Consider adding runtime debug assertions for string header validation
4. Review `qb_file_line_input` - it doesn't release old string before assigning new one

## Test Results

| Binary | Without internal/ | With internal/ |
|--------|-------------------|----------------|
| Original `/tmp/qb64pe_binary` | Crash (SIGSEGV) | Crash (SIGSEGV) |
| Fixed `/tmp/qb64pe_fixed` | Shows error message, times out | Crash (SIGSEGV) |

The fix allows the binary to progress further, proving the first bug was real and is now fixed.
