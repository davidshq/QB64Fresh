//! Array operations for the C backend runtime.
//!
//! This module emits C code for array-related runtime functions including:
//! - Array bounds registry (tracking lower/upper bounds per dimension)
//! - LBOUND and UBOUND functions
//! - Array registration and update functions for DIM/REDIM
//! - ERASE support for clearing array metadata

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits array runtime functions including bounds registry and LBOUND/UBOUND.
///
/// This generates C code for:
/// - Array metadata structure (`_qba_meta`) to track bounds per dimension
/// - Hash table registry for O(1) array lookup
/// - `qb_array_register` / `qb_array_register_md` for DIM statements
/// - `qb_array_update` for REDIM (handles pointer changes from realloc)
/// - `qb_lbound` / `qb_lbound2` for LBOUND function
/// - `qb_ubound` / `qb_ubound2` for UBOUND function
/// - `qb_array_erase` for ERASE statement
pub(super) fn emit_array_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Array Bounds Registry */")?;
    writeln_code!(
        output,
        "/* Tracks lower/upper bounds for each array dimension */"
    )?;
    writeln_code!(output)?;

    // Define the maximum number of dimensions (QB64 supports up to 63)
    writeln_code!(output, "#define QBA_MAX_DIMS 8")?;
    writeln_code!(output, "#define QBA_REGISTRY_SIZE 1024")?;
    writeln_code!(output)?;

    // Array metadata structure
    writeln_code!(output, "typedef struct {{")?;
    writeln_code!(
        output,
        "    void* ptr;                           /* Array pointer (key) */"
    )?;
    writeln_code!(
        output,
        "    int32_t num_dims;                    /* Number of dimensions */"
    )?;
    writeln_code!(
        output,
        "    int32_t lower[QBA_MAX_DIMS];         /* Lower bounds per dimension */"
    )?;
    writeln_code!(
        output,
        "    int32_t upper[QBA_MAX_DIMS];         /* Upper bounds per dimension */"
    )?;
    writeln_code!(output, "}} _qba_meta;")?;
    writeln_code!(output)?;

    // Simple hash table for array metadata
    writeln_code!(output, "static _qba_meta _qba_registry[QBA_REGISTRY_SIZE];")?;
    writeln_code!(output, "static int _qba_count = 0;")?;
    writeln_code!(output)?;

    // Hash function
    writeln_code!(output, "static int _qba_hash(void* ptr) {{")?;
    writeln_code!(
        output,
        "    return (int)(((uintptr_t)ptr >> 3) % QBA_REGISTRY_SIZE);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Find metadata for an array pointer (linear probing)
    writeln_code!(output, "static _qba_meta* _qba_find(void* ptr) {{")?;
    writeln_code!(output, "    if (!ptr) return NULL;")?;
    writeln_code!(output, "    int start = _qba_hash(ptr);")?;
    writeln_code!(output, "    for (int i = 0; i < QBA_REGISTRY_SIZE; i++) {{")?;
    writeln_code!(output, "        int idx = (start + i) % QBA_REGISTRY_SIZE;")?;
    writeln_code!(
        output,
        "        if (_qba_registry[idx].ptr == ptr) return &_qba_registry[idx];"
    )?;
    writeln_code!(
        output,
        "        if (_qba_registry[idx].ptr == NULL) return NULL;"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return NULL;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Register array bounds (1D version - most common)
    writeln_code!(
        output,
        "void qb_array_register(void* ptr, int32_t lower, int32_t upper) {{"
    )?;
    writeln_code!(output, "    if (!ptr) return;")?;
    writeln_code!(output, "    _qba_meta* existing = _qba_find(ptr);")?;
    writeln_code!(output, "    if (existing) {{")?;
    writeln_code!(output, "        existing->num_dims = 1;")?;
    writeln_code!(output, "        existing->lower[0] = lower;")?;
    writeln_code!(output, "        existing->upper[0] = upper;")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    int start = _qba_hash(ptr);")?;
    writeln_code!(output, "    for (int i = 0; i < QBA_REGISTRY_SIZE; i++) {{")?;
    writeln_code!(output, "        int idx = (start + i) % QBA_REGISTRY_SIZE;")?;
    writeln_code!(output, "        if (_qba_registry[idx].ptr == NULL) {{")?;
    writeln_code!(output, "            _qba_registry[idx].ptr = ptr;")?;
    writeln_code!(output, "            _qba_registry[idx].num_dims = 1;")?;
    writeln_code!(output, "            _qba_registry[idx].lower[0] = lower;")?;
    writeln_code!(output, "            _qba_registry[idx].upper[0] = upper;")?;
    writeln_code!(output, "            _qba_count++;")?;
    writeln_code!(output, "            return;")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Register array bounds (multi-dimensional version)
    writeln_code!(
        output,
        "void qb_array_register_md(void* ptr, int32_t num_dims, int32_t* lowers, int32_t* uppers) {{"
    )?;
    writeln_code!(
        output,
        "    if (!ptr || num_dims <= 0 || num_dims > QBA_MAX_DIMS) return;"
    )?;
    writeln_code!(output, "    _qba_meta* existing = _qba_find(ptr);")?;
    writeln_code!(output, "    _qba_meta* meta = existing;")?;
    writeln_code!(output, "    if (!meta) {{")?;
    writeln_code!(output, "        int start = _qba_hash(ptr);")?;
    writeln_code!(
        output,
        "        for (int i = 0; i < QBA_REGISTRY_SIZE; i++) {{"
    )?;
    writeln_code!(
        output,
        "            int idx = (start + i) % QBA_REGISTRY_SIZE;"
    )?;
    writeln_code!(output, "            if (_qba_registry[idx].ptr == NULL) {{")?;
    writeln_code!(output, "                meta = &_qba_registry[idx];")?;
    writeln_code!(output, "                meta->ptr = ptr;")?;
    writeln_code!(output, "                _qba_count++;")?;
    writeln_code!(output, "                break;")?;
    writeln_code!(output, "            }}")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    if (meta) {{")?;
    writeln_code!(output, "        meta->num_dims = num_dims;")?;
    writeln_code!(output, "        for (int d = 0; d < num_dims; d++) {{")?;
    writeln_code!(output, "            meta->lower[d] = lowers[d];")?;
    writeln_code!(output, "            meta->upper[d] = uppers[d];")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Update array bounds after REDIM (pointer may change due to realloc)
    writeln_code!(
        output,
        "void qb_array_update(void* old_ptr, void* new_ptr, int32_t lower, int32_t upper) {{"
    )?;
    writeln_code!(output, "    if (old_ptr == new_ptr) {{")?;
    writeln_code!(output, "        _qba_meta* meta = _qba_find(old_ptr);")?;
    writeln_code!(
        output,
        "        if (meta) {{ meta->lower[0] = lower; meta->upper[0] = upper; }}"
    )?;
    writeln_code!(
        output,
        "        else qb_array_register(new_ptr, lower, upper);"
    )?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        _qba_meta* meta = _qba_find(old_ptr);")?;
    writeln_code!(output, "        if (meta) meta->ptr = NULL;")?;
    writeln_code!(output, "        qb_array_register(new_ptr, lower, upper);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "/* Array Functions */")?;
    writeln_code!(output)?;

    // LBOUND - return lower bound of array (first dimension)
    writeln_code!(output, "int32_t qb_lbound(void* arr) {{")?;
    writeln_code!(output, "    _qba_meta* meta = _qba_find(arr);")?;
    writeln_code!(output, "    return meta ? meta->lower[0] : 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // LBOUND with dimension parameter
    writeln_code!(output, "int32_t qb_lbound2(void* arr, int32_t dim) {{")?;
    writeln_code!(output, "    _qba_meta* meta = _qba_find(arr);")?;
    writeln_code!(
        output,
        "    if (!meta || dim < 1 || dim > meta->num_dims) return 0;"
    )?;
    writeln_code!(output, "    return meta->lower[dim - 1];")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // UBOUND - return upper bound of array (first dimension)
    writeln_code!(output, "int32_t qb_ubound(void* arr) {{")?;
    writeln_code!(output, "    _qba_meta* meta = _qba_find(arr);")?;
    writeln_code!(output, "    return meta ? meta->upper[0] : 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // UBOUND with dimension parameter
    writeln_code!(output, "int32_t qb_ubound2(void* arr, int32_t dim) {{")?;
    writeln_code!(output, "    _qba_meta* meta = _qba_find(arr);")?;
    writeln_code!(
        output,
        "    if (!meta || dim < 1 || dim > meta->num_dims) return 0;"
    )?;
    writeln_code!(output, "    return meta->upper[dim - 1];")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // ERASE - reset array to initial state and clear metadata
    writeln_code!(output, "void qb_array_erase(void* arr) {{")?;
    writeln_code!(output, "    _qba_meta* meta = _qba_find(arr);")?;
    writeln_code!(output, "    if (meta) meta->ptr = NULL;  /* Clear entry */")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
