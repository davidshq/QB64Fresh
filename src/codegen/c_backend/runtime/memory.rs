//! Memory operation functions for the C backend runtime.
//!
//! This module provides code generation for QB64 memory functions including:
//! - PEEK/POKE - Read/write bytes from conventional memory emulation
//! - DEF SEG - Set memory segment for PEEK/POKE operations
//! - VARPTR - Get memory address of variables
//! - _MEM functions - Modern memory block operations (_MEMNEW, _MEMFREE, _MEMGET, _MEMPUT, etc.)
//! - _OFFSET - Get memory address as pointer
//!
//! The conventional memory (cmem) system emulates the DOS-era 1MB memory space
//! that classic BASIC programs expect, while keeping all operations safe within
//! a heap-allocated array.

use std::fmt::Write;

/// Emits C code for memory operation functions.
///
/// This includes:
/// - Sandboxed conventional memory emulation (cmem) for PEEK/POKE
/// - DEF SEG for segment selection
/// - _MEM type and associated functions (_MEMNEW, _MEMFREE, _MEMGET, _MEMPUT, _MEMCOPY, _MEMFILL)
/// - _OFFSET for getting memory addresses
pub(super) fn emit_memory_functions(output: &mut String) {
    writeln!(output, "/* QB64 Memory Operations */").unwrap();
    writeln!(output).unwrap();

    // Sandboxed conventional memory emulation (cmem)
    // This emulates the DOS-era 1MB conventional memory space that PEEK/POKE operate on.
    // Size: 16*65535 + 65535 + 3 = 1,114,099 bytes (enough for highest referenceable dword)
    // Memory layout: [1280 bytes reserved][DBLOCK 64K][remaining space]
    // This is completely safe - it's just a heap array, not real system memory.
    writeln!(output, "#define QB_CMEM_SIZE 1114099").unwrap();
    writeln!(output, "#define QB_DBLOCK_OFFSET 1280").unwrap();
    writeln!(output, "static uint8_t qb_cmem[QB_CMEM_SIZE];").unwrap();
    writeln!(
        output,
        "static uint8_t *qb_defseg = &qb_cmem[QB_DBLOCK_OFFSET];"
    )
    .unwrap();
    writeln!(output, "static int32_t qb_current_segment = -1;").unwrap();
    writeln!(output).unwrap();

    // DEF SEG [= segment] - Set memory segment for PEEK/POKE
    // Segment -1 means default (DBLOCK at offset 1280)
    // Otherwise, segment is multiplied by 16 to get the byte offset (16-bit real mode style)
    writeln!(output, "void qb_def_seg(int32_t segment) {{").unwrap();
    writeln!(output, "    qb_current_segment = segment;").unwrap();
    writeln!(output, "    if (segment == -1) {{").unwrap();
    writeln!(output, "        qb_defseg = &qb_cmem[QB_DBLOCK_OFFSET];").unwrap();
    writeln!(
        output,
        "    }} else if (segment >= -65536 && segment <= 65535) {{"
    )
    .unwrap();
    writeln!(
        output,
        "        qb_defseg = &qb_cmem[0] + ((uint16_t)segment) * 16;"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // qb_get_segment() - Get current segment value (for internal use)
    writeln!(output, "int32_t qb_get_segment(void) {{").unwrap();
    writeln!(output, "    return qb_current_segment;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // PEEK(offset) - Read a byte from memory at current segment + offset
    // Offset is validated and masked to 16-bit (0-65535 range within segment)
    // Returns 0 for out-of-range offsets (matches QB64pe behavior with error suppressed)
    writeln!(output, "int qb_peek(int32_t offset) {{").unwrap();
    writeln!(output, "    if (offset < -65536 || offset > 65535) {{").unwrap();
    writeln!(output, "        return 0;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return qb_defseg[(uint16_t)offset];").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // POKE offset, value - Write a byte to memory at current segment + offset
    // Offset is validated and masked to 16-bit (0-65535 range within segment)
    // Out-of-range offsets are silently ignored (matches QB64pe behavior with error suppressed)
    writeln!(output, "void qb_poke(int32_t offset, uint8_t value) {{").unwrap();
    writeln!(output, "    if (offset < -65536 || offset > 65535) {{").unwrap();
    writeln!(output, "        return;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    qb_defseg[(uint16_t)offset] = value;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEM type definition - QB64 memory block descriptor
    writeln!(output, "typedef struct qb_mem {{").unwrap();
    writeln!(output, "    void* offset;      /* Pointer to data */").unwrap();
    writeln!(output, "    intptr_t size;     /* Size in bytes */").unwrap();
    writeln!(output, "    intptr_t type;     /* Type info (0=generic) */").unwrap();
    writeln!(
        output,
        "    intptr_t elementsize; /* Element size for arrays */"
    )
    .unwrap();
    writeln!(
        output,
        "    int32_t image;     /* Image handle if applicable */"
    )
    .unwrap();
    writeln!(
        output,
        "    int32_t sound;     /* Sound handle if applicable */"
    )
    .unwrap();
    writeln!(output, "}} qb_mem;").unwrap();
    writeln!(output).unwrap();

    // _MEMNEW(size) - Allocate a new memory block
    writeln!(output, "qb_mem qb_memnew(intptr_t size) {{").unwrap();
    writeln!(output, "    qb_mem m;").unwrap();
    writeln!(
        output,
        "    m.offset = (size > 0) ? calloc(1, (size_t)size) : NULL;"
    )
    .unwrap();
    writeln!(output, "    m.size = (m.offset) ? size : 0;").unwrap();
    writeln!(output, "    m.type = 0;").unwrap();
    writeln!(output, "    m.elementsize = 1;").unwrap();
    writeln!(output, "    m.image = 0;").unwrap();
    writeln!(output, "    m.sound = 0;").unwrap();
    writeln!(output, "    return m;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMFREE(mem) - Free a memory block
    writeln!(output, "void qb_memfree(qb_mem* m) {{").unwrap();
    writeln!(output, "    if (m && m->offset) {{").unwrap();
    writeln!(output, "        free(m->offset);").unwrap();
    writeln!(output, "        m->offset = NULL;").unwrap();
    writeln!(output, "        m->size = 0;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMGET - Read value from memory (generic version returns int64)
    // In real QB64, this is type-aware; here we provide basic int64 access
    writeln!(
        output,
        "int64_t qb_memget(qb_mem m, intptr_t byteoffset) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || byteoffset < 0 || byteoffset >= m.size) return 0;"
    )
    .unwrap();
    writeln!(output, "    int64_t result = 0;").unwrap();
    writeln!(
        output,
        "    size_t copysize = (size_t)(m.size - byteoffset);"
    )
    .unwrap();
    writeln!(
        output,
        "    if (copysize > sizeof(int64_t)) copysize = sizeof(int64_t);"
    )
    .unwrap();
    writeln!(
        output,
        "    memcpy(&result, (char*)m.offset + byteoffset, copysize);"
    )
    .unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMPUT - Write value to memory
    writeln!(
        output,
        "void qb_memput(qb_mem m, intptr_t byteoffset, int64_t value) {{"
    )
    .unwrap();
    writeln!(output, "    if (!m.offset || byteoffset < 0) return;").unwrap();
    writeln!(output, "    size_t copysize = sizeof(int64_t);").unwrap();
    writeln!(
        output,
        "    if (byteoffset + (intptr_t)copysize > m.size) {{"
    )
    .unwrap();
    writeln!(output, "        copysize = (size_t)(m.size - byteoffset);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    if (copysize > 0) {{").unwrap();
    writeln!(
        output,
        "        memcpy((char*)m.offset + byteoffset, &value, copysize);"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMCOPY - Copy bytes between memory blocks
    writeln!(output, "void qb_memcopy(qb_mem src, intptr_t src_offset, intptr_t bytes, qb_mem dest, intptr_t dest_offset) {{").unwrap();
    writeln!(output, "    if (!src.offset || !dest.offset) return;").unwrap();
    writeln!(
        output,
        "    if (src_offset < 0 || dest_offset < 0 || bytes <= 0) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    if (src_offset + bytes > src.size) bytes = src.size - src_offset;"
    )
    .unwrap();
    writeln!(
        output,
        "    if (dest_offset + bytes > dest.size) bytes = dest.size - dest_offset;"
    )
    .unwrap();
    writeln!(output, "    if (bytes > 0) {{").unwrap();
    writeln!(output, "        memmove((char*)dest.offset + dest_offset, (char*)src.offset + src_offset, (size_t)bytes);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMFILL - Fill memory with a byte value
    writeln!(
        output,
        "void qb_memfill(qb_mem m, intptr_t byteoffset, intptr_t bytes, int32_t value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || byteoffset < 0 || bytes <= 0) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    if (byteoffset + bytes > m.size) bytes = m.size - byteoffset;"
    )
    .unwrap();
    writeln!(output, "    if (bytes > 0) {{").unwrap();
    writeln!(
        output,
        "        memset((char*)m.offset + byteoffset, (int)(value & 0xFF), (size_t)bytes);"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _OFFSET - Get memory address of variable
    // This is typically used with pointers; here we return an address
    writeln!(output, "intptr_t qb_offset(void* ptr) {{").unwrap();
    writeln!(output, "    return (intptr_t)ptr;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEM - Create a memory block referencing a variable
    // Takes pointer and size, returns qb_mem descriptor
    writeln!(output, "qb_mem qb_mem_of(void* ptr, intptr_t size) {{").unwrap();
    writeln!(output, "    qb_mem m;").unwrap();
    writeln!(output, "    m.offset = ptr;").unwrap();
    writeln!(output, "    m.size = size;").unwrap();
    writeln!(output, "    m.type = 0;").unwrap();
    writeln!(output, "    m.elementsize = 1;").unwrap();
    writeln!(output, "    m.image = 0;").unwrap();
    writeln!(output, "    m.sound = 0;").unwrap();
    writeln!(output, "    return m;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMEXISTS(memBlock) - Check if memory block is still valid
    // Returns -1 (true) if valid, 0 (false) if freed/invalid
    writeln!(output, "int32_t qb_memexists(qb_mem m) {{").unwrap();
    writeln!(
        output,
        "    return (m.offset != NULL && m.size > 0) ? -1 : 0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMELEMENT(memBlock, elementIndex) - Get memory reference to array element
    // Returns _MEM block pointing to the specific element
    writeln!(output, "qb_mem qb_memelement(qb_mem m, intptr_t index) {{").unwrap();
    writeln!(output, "    qb_mem result;").unwrap();
    writeln!(
        output,
        "    if (!m.offset || m.elementsize <= 0 || index < 0) {{"
    )
    .unwrap();
    writeln!(output, "        result.offset = NULL;").unwrap();
    writeln!(output, "        result.size = 0;").unwrap();
    writeln!(output, "        result.type = 0;").unwrap();
    writeln!(output, "        result.elementsize = 0;").unwrap();
    writeln!(output, "        result.image = 0;").unwrap();
    writeln!(output, "        result.sound = 0;").unwrap();
    writeln!(output, "        return result;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    intptr_t byte_offset = index * m.elementsize;").unwrap();
    writeln!(output, "    if (byte_offset >= m.size) {{").unwrap();
    writeln!(output, "        result.offset = NULL;").unwrap();
    writeln!(output, "        result.size = 0;").unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(
        output,
        "        result.offset = (char*)m.offset + byte_offset;"
    )
    .unwrap();
    writeln!(output, "        result.size = m.elementsize;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    result.type = m.type;").unwrap();
    writeln!(output, "    result.elementsize = m.elementsize;").unwrap();
    writeln!(output, "    result.image = m.image;").unwrap();
    writeln!(output, "    result.sound = m.sound;").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMIMAGE(imageHandle) - Get memory reference to image pixel data
    // In inline runtime, images are not supported - return empty block
    writeln!(output, "qb_mem qb_memimage(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(output, "    qb_mem m = {{NULL, 0, 0, 0, 0, 0}};").unwrap();
    writeln!(output, "    return m;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MEMSOUND(soundHandle) - Get memory reference to sound data
    // In inline runtime, sounds are not supported - return empty block
    writeln!(output, "qb_mem qb_memsound(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    qb_mem m = {{NULL, 0, 0, 0, 0, 0}};").unwrap();
    writeln!(output, "    return m;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Type-specific _MEMGET variants for different data sizes
    writeln!(
        output,
        "int8_t qb_memget_byte(qb_mem m, intptr_t offset) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset >= m.size) return 0;"
    )
    .unwrap();
    writeln!(output, "    return *((int8_t*)((char*)m.offset + offset));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "int16_t qb_memget_integer(qb_mem m, intptr_t offset) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 2 > m.size) return 0;"
    )
    .unwrap();
    writeln!(output, "    int16_t result;").unwrap();
    writeln!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(int16_t));"
    )
    .unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "int32_t qb_memget_long(qb_mem m, intptr_t offset) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return 0;"
    )
    .unwrap();
    writeln!(output, "    int32_t result;").unwrap();
    writeln!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(int32_t));"
    )
    .unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "float qb_memget_single(qb_mem m, intptr_t offset) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return 0.0f;"
    )
    .unwrap();
    writeln!(output, "    float result;").unwrap();
    writeln!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(float));"
    )
    .unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "double qb_memget_double(qb_mem m, intptr_t offset) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 8 > m.size) return 0.0;"
    )
    .unwrap();
    writeln!(output, "    double result;").unwrap();
    writeln!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(double));"
    )
    .unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Type-specific _MEMPUT variants for different data sizes
    writeln!(
        output,
        "void qb_memput_byte(qb_mem m, intptr_t offset, int8_t value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset >= m.size) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    *((int8_t*)((char*)m.offset + offset)) = value;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memput_integer(qb_mem m, intptr_t offset, int16_t value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 2 > m.size) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(int16_t));"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memput_long(qb_mem m, intptr_t offset, int32_t value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(int32_t));"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memput_single(qb_mem m, intptr_t offset, float value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(float));"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memput_double(qb_mem m, intptr_t offset, double value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || offset + 8 > m.size) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(double));"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Type-specific _MEMFILL variants for multi-byte patterns
    writeln!(
        output,
        "void qb_memfill_integer(qb_mem m, intptr_t offset, intptr_t count, int16_t value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )
    .unwrap();
    writeln!(output, "    intptr_t bytes = count * sizeof(int16_t);").unwrap();
    writeln!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )
    .unwrap();
    writeln!(
        output,
        "    int16_t* ptr = (int16_t*)((char*)m.offset + offset);"
    )
    .unwrap();
    writeln!(output, "    intptr_t n = bytes / sizeof(int16_t);").unwrap();
    writeln!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memfill_long(qb_mem m, intptr_t offset, intptr_t count, int32_t value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )
    .unwrap();
    writeln!(output, "    intptr_t bytes = count * sizeof(int32_t);").unwrap();
    writeln!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )
    .unwrap();
    writeln!(
        output,
        "    int32_t* ptr = (int32_t*)((char*)m.offset + offset);"
    )
    .unwrap();
    writeln!(output, "    intptr_t n = bytes / sizeof(int32_t);").unwrap();
    writeln!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memfill_single(qb_mem m, intptr_t offset, intptr_t count, float value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )
    .unwrap();
    writeln!(output, "    intptr_t bytes = count * sizeof(float);").unwrap();
    writeln!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )
    .unwrap();
    writeln!(
        output,
        "    float* ptr = (float*)((char*)m.offset + offset);"
    )
    .unwrap();
    writeln!(output, "    intptr_t n = bytes / sizeof(float);").unwrap();
    writeln!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_memfill_double(qb_mem m, intptr_t offset, intptr_t count, double value) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )
    .unwrap();
    writeln!(output, "    intptr_t bytes = count * sizeof(double);").unwrap();
    writeln!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )
    .unwrap();
    writeln!(
        output,
        "    double* ptr = (double*)((char*)m.offset + offset);"
    )
    .unwrap();
    writeln!(output, "    intptr_t n = bytes / sizeof(double);").unwrap();
    writeln!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
