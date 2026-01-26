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

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits C code for memory operation functions.
///
/// This includes:
/// - Sandboxed conventional memory emulation (cmem) for PEEK/POKE
/// - DEF SEG for segment selection
/// - _MEM type and associated functions (_MEMNEW, _MEMFREE, _MEMGET, _MEMPUT, _MEMCOPY, _MEMFILL)
/// - _OFFSET for getting memory addresses
pub(super) fn emit_memory_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* QB64 Memory Operations */")?;
    writeln_code!(output)?;

    // Sandboxed conventional memory emulation (cmem)
    // This emulates the DOS-era 1MB conventional memory space that PEEK/POKE operate on.
    // Size: 16*65535 + 65535 + 3 = 1,114,099 bytes (enough for highest referenceable dword)
    // Memory layout: [1280 bytes reserved][DBLOCK 64K][remaining space]
    // This is completely safe - it's just a heap array, not real system memory.
    writeln_code!(output, "#define QB_CMEM_SIZE 1114099")?;
    writeln_code!(output, "#define QB_DBLOCK_OFFSET 1280")?;
    writeln_code!(output, "static uint8_t qb_cmem[QB_CMEM_SIZE];")?;
    writeln_code!(
        output,
        "static uint8_t *qb_defseg = &qb_cmem[QB_DBLOCK_OFFSET];"
    )?;
    writeln_code!(output, "static int32_t qb_current_segment = -1;")?;
    writeln_code!(output)?;

    // DEF SEG [= segment] - Set memory segment for PEEK/POKE
    // Segment -1 means default (DBLOCK at offset 1280)
    // Otherwise, segment is multiplied by 16 to get the byte offset (16-bit real mode style)
    writeln_code!(output, "void qb_def_seg(int32_t segment) {{")?;
    writeln_code!(output, "    qb_current_segment = segment;")?;
    writeln_code!(output, "    if (segment == -1) {{")?;
    writeln_code!(output, "        qb_defseg = &qb_cmem[QB_DBLOCK_OFFSET];")?;
    writeln_code!(
        output,
        "    }} else if (segment >= -65536 && segment <= 65535) {{"
    )?;
    writeln_code!(
        output,
        "        qb_defseg = &qb_cmem[0] + ((uint16_t)segment) * 16;"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // qb_get_segment() - Get current segment value (for internal use)
    writeln_code!(output, "int32_t qb_get_segment(void) {{")?;
    writeln_code!(output, "    return qb_current_segment;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // PEEK(offset) - Read a byte from memory at current segment + offset
    // Offset is validated and masked to 16-bit (0-65535 range within segment)
    // Returns 0 for out-of-range offsets (matches QB64pe behavior with error suppressed)
    writeln_code!(output, "int qb_peek(int32_t offset) {{")?;
    writeln_code!(output, "    if (offset < -65536 || offset > 65535) {{")?;
    writeln_code!(output, "        return 0;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return qb_defseg[(uint16_t)offset];")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // POKE offset, value - Write a byte to memory at current segment + offset
    // Offset is validated and masked to 16-bit (0-65535 range within segment)
    // Out-of-range offsets are silently ignored (matches QB64pe behavior with error suppressed)
    writeln_code!(output, "void qb_poke(int32_t offset, uint8_t value) {{")?;
    writeln_code!(output, "    if (offset < -65536 || offset > 65535) {{")?;
    writeln_code!(output, "        return;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    qb_defseg[(uint16_t)offset] = value;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEM type definition - QB64 memory block descriptor
    writeln_code!(output, "typedef struct qb_mem {{")?;
    writeln_code!(output, "    void* offset;      /* Pointer to data */")?;
    writeln_code!(output, "    intptr_t size;     /* Size in bytes */")?;
    writeln_code!(output, "    intptr_t type;     /* Type info (0=generic) */")?;
    writeln_code!(
        output,
        "    intptr_t elementsize; /* Element size for arrays */"
    )?;
    writeln_code!(
        output,
        "    int32_t image;     /* Image handle if applicable */"
    )?;
    writeln_code!(
        output,
        "    int32_t sound;     /* Sound handle if applicable */"
    )?;
    writeln_code!(output, "}} qb_mem;")?;
    writeln_code!(output)?;

    // _MEMNEW(size) - Allocate a new memory block
    writeln_code!(output, "qb_mem qb_memnew(intptr_t size) {{")?;
    writeln_code!(output, "    qb_mem m;")?;
    writeln_code!(
        output,
        "    m.offset = (size > 0) ? calloc(1, (size_t)size) : NULL;"
    )?;
    writeln_code!(output, "    m.size = (m.offset) ? size : 0;")?;
    writeln_code!(output, "    m.type = 0;")?;
    writeln_code!(output, "    m.elementsize = 1;")?;
    writeln_code!(output, "    m.image = 0;")?;
    writeln_code!(output, "    m.sound = 0;")?;
    writeln_code!(output, "    return m;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMFREE(mem) - Free a memory block
    writeln_code!(output, "void qb_memfree(qb_mem* m) {{")?;
    writeln_code!(output, "    if (m && m->offset) {{")?;
    writeln_code!(output, "        free(m->offset);")?;
    writeln_code!(output, "        m->offset = NULL;")?;
    writeln_code!(output, "        m->size = 0;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMGET - Read value from memory (generic version returns int64)
    // In real QB64, this is type-aware; here we provide basic int64 access
    writeln_code!(
        output,
        "int64_t qb_memget(qb_mem m, intptr_t byteoffset) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || byteoffset < 0 || byteoffset >= m.size) return 0;"
    )?;
    writeln_code!(output, "    int64_t result = 0;")?;
    writeln_code!(
        output,
        "    size_t copysize = (size_t)(m.size - byteoffset);"
    )?;
    writeln_code!(
        output,
        "    if (copysize > sizeof(int64_t)) copysize = sizeof(int64_t);"
    )?;
    writeln_code!(
        output,
        "    memcpy(&result, (char*)m.offset + byteoffset, copysize);"
    )?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMPUT - Write value to memory
    writeln_code!(
        output,
        "void qb_memput(qb_mem m, intptr_t byteoffset, int64_t value) {{"
    )?;
    writeln_code!(output, "    if (!m.offset || byteoffset < 0) return;")?;
    writeln_code!(output, "    size_t copysize = sizeof(int64_t);")?;
    writeln_code!(
        output,
        "    if (byteoffset + (intptr_t)copysize > m.size) {{"
    )?;
    writeln_code!(output, "        copysize = (size_t)(m.size - byteoffset);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    if (copysize > 0) {{")?;
    writeln_code!(
        output,
        "        memcpy((char*)m.offset + byteoffset, &value, copysize);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMCOPY - Copy bytes between memory blocks
    writeln_code!(
        output,
        "void qb_memcopy(qb_mem src, intptr_t src_offset, intptr_t bytes, qb_mem dest, intptr_t dest_offset) {{"
    )?;
    writeln_code!(output, "    if (!src.offset || !dest.offset) return;")?;
    writeln_code!(
        output,
        "    if (src_offset < 0 || dest_offset < 0 || bytes <= 0) return;"
    )?;
    writeln_code!(
        output,
        "    if (src_offset + bytes > src.size) bytes = src.size - src_offset;"
    )?;
    writeln_code!(
        output,
        "    if (dest_offset + bytes > dest.size) bytes = dest.size - dest_offset;"
    )?;
    writeln_code!(output, "    if (bytes > 0) {{")?;
    writeln_code!(
        output,
        "        memmove((char*)dest.offset + dest_offset, (char*)src.offset + src_offset, (size_t)bytes);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMFILL - Fill memory with a byte value
    writeln_code!(
        output,
        "void qb_memfill(qb_mem m, intptr_t byteoffset, intptr_t bytes, int32_t value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || byteoffset < 0 || bytes <= 0) return;"
    )?;
    writeln_code!(
        output,
        "    if (byteoffset + bytes > m.size) bytes = m.size - byteoffset;"
    )?;
    writeln_code!(output, "    if (bytes > 0) {{")?;
    writeln_code!(
        output,
        "        memset((char*)m.offset + byteoffset, (int)(value & 0xFF), (size_t)bytes);"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _OFFSET - Get memory address of variable
    // This is typically used with pointers; here we return an address
    writeln_code!(output, "intptr_t qb_offset(void* ptr) {{")?;
    writeln_code!(output, "    return (intptr_t)ptr;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEM - Create a memory block referencing a variable
    // Takes pointer and size, returns qb_mem descriptor
    writeln_code!(output, "qb_mem qb_mem_of(void* ptr, intptr_t size) {{")?;
    writeln_code!(output, "    qb_mem m;")?;
    writeln_code!(output, "    m.offset = ptr;")?;
    writeln_code!(output, "    m.size = size;")?;
    writeln_code!(output, "    m.type = 0;")?;
    writeln_code!(output, "    m.elementsize = 1;")?;
    writeln_code!(output, "    m.image = 0;")?;
    writeln_code!(output, "    m.sound = 0;")?;
    writeln_code!(output, "    return m;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMEXISTS(memBlock) - Check if memory block is still valid
    // Returns -1 (true) if valid, 0 (false) if freed/invalid
    writeln_code!(output, "int32_t qb_memexists(qb_mem m) {{")?;
    writeln_code!(
        output,
        "    return (m.offset != NULL && m.size > 0) ? -1 : 0;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMELEMENT(memBlock, elementIndex) - Get memory reference to array element
    // Returns _MEM block pointing to the specific element
    writeln_code!(output, "qb_mem qb_memelement(qb_mem m, intptr_t index) {{")?;
    writeln_code!(output, "    qb_mem result;")?;
    writeln_code!(
        output,
        "    if (!m.offset || m.elementsize <= 0 || index < 0) {{"
    )?;
    writeln_code!(output, "        result.offset = NULL;")?;
    writeln_code!(output, "        result.size = 0;")?;
    writeln_code!(output, "        result.type = 0;")?;
    writeln_code!(output, "        result.elementsize = 0;")?;
    writeln_code!(output, "        result.image = 0;")?;
    writeln_code!(output, "        result.sound = 0;")?;
    writeln_code!(output, "        return result;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    intptr_t byte_offset = index * m.elementsize;")?;
    writeln_code!(output, "    if (byte_offset >= m.size) {{")?;
    writeln_code!(output, "        result.offset = NULL;")?;
    writeln_code!(output, "        result.size = 0;")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(
        output,
        "        result.offset = (char*)m.offset + byte_offset;"
    )?;
    writeln_code!(output, "        result.size = m.elementsize;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    result.type = m.type;")?;
    writeln_code!(output, "    result.elementsize = m.elementsize;")?;
    writeln_code!(output, "    result.image = m.image;")?;
    writeln_code!(output, "    result.sound = m.sound;")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMIMAGE(imageHandle) - Get memory reference to image pixel data
    // In inline runtime, images are not supported - return empty block
    writeln_code!(output, "qb_mem qb_memimage(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    qb_mem m = {{NULL, 0, 0, 0, 0, 0}};")?;
    writeln_code!(output, "    return m;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MEMSOUND(soundHandle) - Get memory reference to sound data
    // In inline runtime, sounds are not supported - return empty block
    writeln_code!(output, "qb_mem qb_memsound(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    qb_mem m = {{NULL, 0, 0, 0, 0, 0}};")?;
    writeln_code!(output, "    return m;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Type-specific _MEMGET variants for different data sizes
    writeln_code!(
        output,
        "int8_t qb_memget_byte(qb_mem m, intptr_t offset) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset >= m.size) return 0;"
    )?;
    writeln_code!(output, "    return *((int8_t*)((char*)m.offset + offset));")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "int16_t qb_memget_integer(qb_mem m, intptr_t offset) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 2 > m.size) return 0;"
    )?;
    writeln_code!(output, "    int16_t result;")?;
    writeln_code!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(int16_t));"
    )?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "int32_t qb_memget_long(qb_mem m, intptr_t offset) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return 0;"
    )?;
    writeln_code!(output, "    int32_t result;")?;
    writeln_code!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(int32_t));"
    )?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "float qb_memget_single(qb_mem m, intptr_t offset) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return 0.0f;"
    )?;
    writeln_code!(output, "    float result;")?;
    writeln_code!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(float));"
    )?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "double qb_memget_double(qb_mem m, intptr_t offset) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 8 > m.size) return 0.0;"
    )?;
    writeln_code!(output, "    double result;")?;
    writeln_code!(
        output,
        "    memcpy(&result, (char*)m.offset + offset, sizeof(double));"
    )?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Type-specific _MEMPUT variants for different data sizes
    writeln_code!(
        output,
        "void qb_memput_byte(qb_mem m, intptr_t offset, int8_t value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset >= m.size) return;"
    )?;
    writeln_code!(
        output,
        "    *((int8_t*)((char*)m.offset + offset)) = value;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memput_integer(qb_mem m, intptr_t offset, int16_t value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 2 > m.size) return;"
    )?;
    writeln_code!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(int16_t));"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memput_long(qb_mem m, intptr_t offset, int32_t value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return;"
    )?;
    writeln_code!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(int32_t));"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memput_single(qb_mem m, intptr_t offset, float value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 4 > m.size) return;"
    )?;
    writeln_code!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(float));"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memput_double(qb_mem m, intptr_t offset, double value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || offset + 8 > m.size) return;"
    )?;
    writeln_code!(
        output,
        "    memcpy((char*)m.offset + offset, &value, sizeof(double));"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Type-specific _MEMFILL variants for multi-byte patterns
    writeln_code!(
        output,
        "void qb_memfill_integer(qb_mem m, intptr_t offset, intptr_t count, int16_t value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )?;
    writeln_code!(output, "    intptr_t bytes = count * sizeof(int16_t);")?;
    writeln_code!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )?;
    writeln_code!(
        output,
        "    int16_t* ptr = (int16_t*)((char*)m.offset + offset);"
    )?;
    writeln_code!(output, "    intptr_t n = bytes / sizeof(int16_t);")?;
    writeln_code!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memfill_long(qb_mem m, intptr_t offset, intptr_t count, int32_t value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )?;
    writeln_code!(output, "    intptr_t bytes = count * sizeof(int32_t);")?;
    writeln_code!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )?;
    writeln_code!(
        output,
        "    int32_t* ptr = (int32_t*)((char*)m.offset + offset);"
    )?;
    writeln_code!(output, "    intptr_t n = bytes / sizeof(int32_t);")?;
    writeln_code!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memfill_single(qb_mem m, intptr_t offset, intptr_t count, float value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )?;
    writeln_code!(output, "    intptr_t bytes = count * sizeof(float);")?;
    writeln_code!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )?;
    writeln_code!(
        output,
        "    float* ptr = (float*)((char*)m.offset + offset);"
    )?;
    writeln_code!(output, "    intptr_t n = bytes / sizeof(float);")?;
    writeln_code!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_memfill_double(qb_mem m, intptr_t offset, intptr_t count, double value) {{"
    )?;
    writeln_code!(
        output,
        "    if (!m.offset || offset < 0 || count <= 0) return;"
    )?;
    writeln_code!(output, "    intptr_t bytes = count * sizeof(double);")?;
    writeln_code!(
        output,
        "    if (offset + bytes > m.size) bytes = m.size - offset;"
    )?;
    writeln_code!(
        output,
        "    double* ptr = (double*)((char*)m.offset + offset);"
    )?;
    writeln_code!(output, "    intptr_t n = bytes / sizeof(double);")?;
    writeln_code!(
        output,
        "    for (intptr_t i = 0; i < n; i++) ptr[i] = value;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
