//! Array operations for the C backend runtime.
//!
//! This module emits C code for array-related runtime functions including:
//! - Array bounds registry (tracking lower/upper bounds per dimension)
//! - LBOUND and UBOUND functions
//! - Array registration and update functions for DIM/REDIM
//! - ERASE support for clearing array metadata

use std::fmt::Write;

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
pub(super) fn emit_array_functions(output: &mut String) {
    writeln!(output, "/* Array Bounds Registry */").unwrap();
    writeln!(
        output,
        "/* Tracks lower/upper bounds for each array dimension */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Define the maximum number of dimensions (QB64 supports up to 63)
    writeln!(output, "#define QBA_MAX_DIMS 8").unwrap();
    writeln!(output, "#define QBA_REGISTRY_SIZE 1024").unwrap();
    writeln!(output).unwrap();

    // Array metadata structure
    writeln!(output, "typedef struct {{").unwrap();
    writeln!(
        output,
        "    void* ptr;                           /* Array pointer (key) */"
    )
    .unwrap();
    writeln!(
        output,
        "    int32_t num_dims;                    /* Number of dimensions */"
    )
    .unwrap();
    writeln!(
        output,
        "    int32_t lower[QBA_MAX_DIMS];         /* Lower bounds per dimension */"
    )
    .unwrap();
    writeln!(
        output,
        "    int32_t upper[QBA_MAX_DIMS];         /* Upper bounds per dimension */"
    )
    .unwrap();
    writeln!(output, "}} _qba_meta;").unwrap();
    writeln!(output).unwrap();

    // Simple hash table for array metadata
    writeln!(output, "static _qba_meta _qba_registry[QBA_REGISTRY_SIZE];").unwrap();
    writeln!(output, "static int _qba_count = 0;").unwrap();
    writeln!(output).unwrap();

    // Hash function
    writeln!(output, "static int _qba_hash(void* ptr) {{").unwrap();
    writeln!(
        output,
        "    return (int)(((uintptr_t)ptr >> 3) % QBA_REGISTRY_SIZE);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Find metadata for an array pointer (linear probing)
    writeln!(output, "static _qba_meta* _qba_find(void* ptr) {{").unwrap();
    writeln!(output, "    if (!ptr) return NULL;").unwrap();
    writeln!(output, "    int start = _qba_hash(ptr);").unwrap();
    writeln!(output, "    for (int i = 0; i < QBA_REGISTRY_SIZE; i++) {{").unwrap();
    writeln!(output, "        int idx = (start + i) % QBA_REGISTRY_SIZE;").unwrap();
    writeln!(
        output,
        "        if (_qba_registry[idx].ptr == ptr) return &_qba_registry[idx];"
    )
    .unwrap();
    writeln!(
        output,
        "        if (_qba_registry[idx].ptr == NULL) return NULL;"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return NULL;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Register array bounds (1D version - most common)
    writeln!(
        output,
        "void qb_array_register(void* ptr, int32_t lower, int32_t upper) {{"
    )
    .unwrap();
    writeln!(output, "    if (!ptr) return;").unwrap();
    writeln!(output, "    _qba_meta* existing = _qba_find(ptr);").unwrap();
    writeln!(output, "    if (existing) {{").unwrap();
    writeln!(output, "        existing->num_dims = 1;").unwrap();
    writeln!(output, "        existing->lower[0] = lower;").unwrap();
    writeln!(output, "        existing->upper[0] = upper;").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    int start = _qba_hash(ptr);").unwrap();
    writeln!(output, "    for (int i = 0; i < QBA_REGISTRY_SIZE; i++) {{").unwrap();
    writeln!(output, "        int idx = (start + i) % QBA_REGISTRY_SIZE;").unwrap();
    writeln!(output, "        if (_qba_registry[idx].ptr == NULL) {{").unwrap();
    writeln!(output, "            _qba_registry[idx].ptr = ptr;").unwrap();
    writeln!(output, "            _qba_registry[idx].num_dims = 1;").unwrap();
    writeln!(output, "            _qba_registry[idx].lower[0] = lower;").unwrap();
    writeln!(output, "            _qba_registry[idx].upper[0] = upper;").unwrap();
    writeln!(output, "            _qba_count++;").unwrap();
    writeln!(output, "            return;").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Register array bounds (multi-dimensional version)
    writeln!(output, "void qb_array_register_md(void* ptr, int32_t num_dims, int32_t* lowers, int32_t* uppers) {{").unwrap();
    writeln!(
        output,
        "    if (!ptr || num_dims <= 0 || num_dims > QBA_MAX_DIMS) return;"
    )
    .unwrap();
    writeln!(output, "    _qba_meta* existing = _qba_find(ptr);").unwrap();
    writeln!(output, "    _qba_meta* meta = existing;").unwrap();
    writeln!(output, "    if (!meta) {{").unwrap();
    writeln!(output, "        int start = _qba_hash(ptr);").unwrap();
    writeln!(
        output,
        "        for (int i = 0; i < QBA_REGISTRY_SIZE; i++) {{"
    )
    .unwrap();
    writeln!(
        output,
        "            int idx = (start + i) % QBA_REGISTRY_SIZE;"
    )
    .unwrap();
    writeln!(output, "            if (_qba_registry[idx].ptr == NULL) {{").unwrap();
    writeln!(output, "                meta = &_qba_registry[idx];").unwrap();
    writeln!(output, "                meta->ptr = ptr;").unwrap();
    writeln!(output, "                _qba_count++;").unwrap();
    writeln!(output, "                break;").unwrap();
    writeln!(output, "            }}").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    if (meta) {{").unwrap();
    writeln!(output, "        meta->num_dims = num_dims;").unwrap();
    writeln!(output, "        for (int d = 0; d < num_dims; d++) {{").unwrap();
    writeln!(output, "            meta->lower[d] = lowers[d];").unwrap();
    writeln!(output, "            meta->upper[d] = uppers[d];").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Update array bounds after REDIM (pointer may change due to realloc)
    writeln!(
        output,
        "void qb_array_update(void* old_ptr, void* new_ptr, int32_t lower, int32_t upper) {{"
    )
    .unwrap();
    writeln!(output, "    if (old_ptr == new_ptr) {{").unwrap();
    writeln!(output, "        _qba_meta* meta = _qba_find(old_ptr);").unwrap();
    writeln!(
        output,
        "        if (meta) {{ meta->lower[0] = lower; meta->upper[0] = upper; }}"
    )
    .unwrap();
    writeln!(
        output,
        "        else qb_array_register(new_ptr, lower, upper);"
    )
    .unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(output, "        _qba_meta* meta = _qba_find(old_ptr);").unwrap();
    writeln!(output, "        if (meta) meta->ptr = NULL;").unwrap();
    writeln!(output, "        qb_array_register(new_ptr, lower, upper);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "/* Array Functions */").unwrap();
    writeln!(output).unwrap();

    // LBOUND - return lower bound of array (first dimension)
    writeln!(output, "int32_t qb_lbound(void* arr) {{").unwrap();
    writeln!(output, "    _qba_meta* meta = _qba_find(arr);").unwrap();
    writeln!(output, "    return meta ? meta->lower[0] : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // LBOUND with dimension parameter
    writeln!(output, "int32_t qb_lbound2(void* arr, int32_t dim) {{").unwrap();
    writeln!(output, "    _qba_meta* meta = _qba_find(arr);").unwrap();
    writeln!(
        output,
        "    if (!meta || dim < 1 || dim > meta->num_dims) return 0;"
    )
    .unwrap();
    writeln!(output, "    return meta->lower[dim - 1];").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // UBOUND - return upper bound of array (first dimension)
    writeln!(output, "int32_t qb_ubound(void* arr) {{").unwrap();
    writeln!(output, "    _qba_meta* meta = _qba_find(arr);").unwrap();
    writeln!(output, "    return meta ? meta->upper[0] : 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // UBOUND with dimension parameter
    writeln!(output, "int32_t qb_ubound2(void* arr, int32_t dim) {{").unwrap();
    writeln!(output, "    _qba_meta* meta = _qba_find(arr);").unwrap();
    writeln!(
        output,
        "    if (!meta || dim < 1 || dim > meta->num_dims) return 0;"
    )
    .unwrap();
    writeln!(output, "    return meta->upper[dim - 1];").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // ERASE - reset array to initial state and clear metadata
    writeln!(output, "void qb_array_erase(void* arr) {{").unwrap();
    writeln!(output, "    _qba_meta* meta = _qba_find(arr);").unwrap();
    writeln!(output, "    if (meta) meta->ptr = NULL;  /* Clear entry */").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
