# QB64pe Runtime Compatibility Analysis

**Created:** 2026-02-03  
**Purpose:** Analyze which runtime modules are QB64pe compatibility layers vs. native QB64Fresh functionality

---

## Summary

The runtime includes **compatibility layers** that match QB64pe's libqb API, but these are **our own Rust implementations**, not QB64pe's actual runtime code. However, some of these were added specifically to support running QB64pe IDE and may not be needed for QB64Fresh's own operation.

---

## Compatibility Modules Analysis

### ✅ Needed for QB64Fresh (Keep)

These modules are used by QB64Fresh's own codegen:

1. **`cmem.rs`** - Conventional memory emulation
   - **Used by:** PEEK/POKE operations (inline runtime)
   - **Status:** ✅ Keep - Required for BASIC PEEK/POKE support
   - **Note:** Our codegen emits `static uint8_t qb_cmem[QB_CMEM_SIZE]` in inline mode

2. **`memory.rs`** (runtime module) - _MEM functions
   - **Used by:** _MEMNEW, _MEMFREE, _MEMGET, _MEMPUT, etc.
   - **Status:** ✅ Keep - Required for QB64 _MEM feature support

### ⚠️ QB64pe Compatibility Only (Review)

These modules provide libqb-compatible APIs but are NOT used by QB64Fresh's own codegen:

1. **`qbs_compat.rs`** - qbs struct wrapper
   - **Purpose:** Wraps QbString* to match QB64pe's `struct qbs` layout
   - **Used by:** QB64pe-compiled code expecting qbs API
   - **QB64Fresh usage:** ❌ Not used by our codegen (we use QbString directly)
   - **Status:** ⚠️ Review - Only needed for QB64pe binary compatibility

2. **`mem_lock.rs`** - _MEM lock lifecycle
   - **Purpose:** Provides `new_mem_lock()` / `free_mem_lock()` API matching libqb
   - **Used by:** QB64pe-compiled code using mem_lock API
   - **QB64Fresh usage:** ❌ Not used by our codegen (we use qb_mem struct directly)
   - **Status:** ⚠️ Review - Only needed for QB64pe binary compatibility

3. **`libqb_*` FFI modules** - Various libqb-compatible APIs
   - **`mutex_ffi.rs`** - libqb_mutex_* functions
   - **`condvar_ffi.rs`** - libqb_condvar_* functions
   - **`thread.rs`** - libqb_thread_* functions
   - **`http_ffi.rs`** - libqb_http_* functions
   - **`buffer.rs`** - libqb_buffer_* functions
   - **`logging_ffi.rs`** - libqb_log_* functions
   - **`console_display_ffi.rs`** - libqb console/display stubs
   - **Status:** ⚠️ Review - Some may be used, others only for QB64pe compatibility

---

## Codegen Usage Analysis

### QB64Fresh Codegen Uses:
- ✅ `cmem` array (for PEEK/POKE)
- ✅ `qb_mem` struct (for _MEM functions)
- ✅ `QbString*` (our native string type)
- ✅ `qb_*` functions (our native runtime functions)

### QB64Fresh Codegen Does NOT Use:
- ❌ `qbs` struct (we use QbString)
- ❌ `new_mem_lock()` / `free_mem_lock()` (we use qb_mem directly)
- ❌ Most `libqb_*` functions (we use `qb_*` equivalents)

---

## Recommendation

### Option 1: Keep All (Current State)
- **Pros:** Full QB64pe binary compatibility, can run QB64pe IDE
- **Cons:** Extra code that's not used by QB64Fresh itself

### Option 2: Remove QB64pe-Only Compatibility
- **Remove:**
  - `qbs_compat.rs` - Not used by our codegen
  - `mem_lock.rs` - Not used by our codegen
  - Some `libqb_*` FFI modules if not used
- **Keep:**
  - `cmem.rs` - Used for PEEK/POKE
  - `memory.rs` - Used for _MEM functions
  - Any `libqb_*` functions actually used by our codegen

### Option 3: Feature Flag
- Add `qb64pe-compat` feature flag
- Compatibility modules only compiled when flag is enabled
- Default: disabled (leaner runtime)
- Enable when compiling QB64pe or other QB64pe-compiled code

---

## Files to Review

### Definitely QB64pe Compatibility Only:
- `runtime/src/qbs_compat.rs` - qbs wrapper (not used by our codegen)
- `runtime/src/mem_lock.rs` - mem_lock API (not used by our codegen)

### Possibly QB64pe Compatibility Only:
- `runtime/src/mutex_ffi.rs` - Check if used
- `runtime/src/condvar_ffi.rs` - Check if used
- `runtime/src/thread.rs` - Check if used
- `runtime/src/http_ffi.rs` - Check if used
- `runtime/src/buffer.rs` - Check if used
- `runtime/src/console_display_ffi.rs` - Likely QB64pe-only stubs

### Needed for QB64Fresh:
- `runtime/src/cmem.rs` - Used for PEEK/POKE
- `runtime/src/memory.rs` - Used for _MEM functions

---

## Next Steps

1. **Audit each libqb_* module** - Check if used by QB64Fresh codegen
2. **Decide on removal strategy** - Remove, feature flag, or keep all
3. **Update documentation** - Clarify which modules are compatibility layers
4. **Test impact** - Ensure QB64Fresh's own programs still work after removal
