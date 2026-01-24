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
}
