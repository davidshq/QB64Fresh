//! QB64 _MEM* memory block operations for the external runtime.
//!
//! Provides _MEMNEW, _MEMFREE, _MEMGET, _MEMPUT, _MEMCOPY, _MEMFILL,
//! _MEM (qb_mem_of), _MEMEXISTS, _MEMELEMENT, _MEMIMAGE, _MEMSOUND.
//! Matches the ABI of the inline runtime in `src/codegen/c_backend/runtime/memory.rs`.

use std::alloc::{alloc, dealloc, Layout};
use std::ffi::c_void;
use std::ptr;

/// C ABI compatible memory block descriptor (qb_mem).
/// Layout must match `typedef struct qb_mem { ... } qb_mem` in qb64fresh_rt.h.
#[repr(C)]
pub struct QbMem {
    /// Pointer to data (NULL when freed)
    pub offset: *mut c_void,
    /// Size in bytes
    pub size: isize,
    /// Type info (0 = generic)
    pub type_: isize,
    /// Element size for arrays
    pub elementsize: isize,
    /// Image handle if applicable
    pub image: i32,
    /// Sound handle if applicable
    pub sound: i32,
}

impl Default for QbMem {
    fn default() -> Self {
        QbMem {
            offset: ptr::null_mut(),
            size: 0,
            type_: 0,
            elementsize: 0,
            image: 0,
            sound: 0,
        }
    }
}

/// _MEMNEW - Allocate a new memory block.
#[no_mangle]
pub extern "C" fn qb_memnew(size: isize) -> QbMem {
    if size <= 0 {
        return QbMem::default();
    }
    let size_usize = size as usize;
    let layout = match Layout::from_size_align(size_usize, 1) {
        Ok(l) => l,
        Err(_) => return QbMem::default(),
    };
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        return QbMem::default();
    }
    unsafe { ptr::write_bytes(ptr, 0, size_usize) };
    QbMem {
        offset: ptr as *mut c_void,
        size,
        type_: 0,
        elementsize: 1,
        image: 0,
        sound: 0,
    }
}

/// _MEMFREE - Free a memory block. Zeroes the descriptor.
#[no_mangle]
pub unsafe extern "C" fn qb_memfree(m: *mut QbMem) {
    if m.is_null() {
        return;
    }
    let ref_m = &mut *m;
    if !ref_m.offset.is_null() && ref_m.size > 0 {
        let layout = Layout::from_size_align(ref_m.size as usize, 1).unwrap_or(Layout::new::<u8>());
        dealloc(ref_m.offset as *mut u8, layout);
    }
    ref_m.offset = ptr::null_mut();
    ref_m.size = 0;
}

/// _MEMGET - Read int64 from memory at byte offset.
#[no_mangle]
pub unsafe extern "C" fn qb_memget(m: QbMem, byteoffset: isize) -> i64 {
    if m.offset.is_null() || byteoffset < 0 || byteoffset >= m.size {
        return 0;
    }
    let copysize = (m.size - byteoffset) as usize;
    let copysize = copysize.min(std::mem::size_of::<i64>());
    let mut result: i64 = 0;
    ptr::copy_nonoverlapping(
        (m.offset as *const u8).add(byteoffset as usize),
        &mut result as *mut i64 as *mut u8,
        copysize,
    );
    result
}

/// _MEMPUT - Write int64 to memory at byte offset.
#[no_mangle]
pub unsafe extern "C" fn qb_memput(m: QbMem, byteoffset: isize, value: i64) {
    if m.offset.is_null() || byteoffset < 0 {
        return;
    }
    let mut copysize = std::mem::size_of::<i64>();
    if byteoffset as usize + copysize > m.size as usize {
        copysize = (m.size - byteoffset).max(0) as usize;
    }
    if copysize > 0 {
        ptr::copy_nonoverlapping(
            &value as *const i64 as *const u8,
            (m.offset as *mut u8).add(byteoffset as usize),
            copysize,
        );
    }
}

/// _MEMCOPY - Copy bytes between memory blocks.
#[no_mangle]
pub unsafe extern "C" fn qb_memcopy(
    src: QbMem,
    src_offset: isize,
    bytes: isize,
    dest: QbMem,
    dest_offset: isize,
) {
    if src.offset.is_null()
        || dest.offset.is_null()
        || src_offset < 0
        || dest_offset < 0
        || bytes <= 0
    {
        return;
    }
    let mut bytes = bytes as usize;
    if src_offset as usize + bytes > src.size as usize {
        bytes = (src.size - src_offset).max(0) as usize;
    }
    if dest_offset as usize + bytes > dest.size as usize {
        bytes = (dest.size - dest_offset).max(0) as usize;
    }
    if bytes > 0 {
        ptr::copy(
            (src.offset as *const u8).add(src_offset as usize),
            (dest.offset as *mut u8).add(dest_offset as usize),
            bytes,
        );
    }
}

/// _MEMFILL - Fill memory with byte value.
#[no_mangle]
pub unsafe extern "C" fn qb_memfill(m: QbMem, byteoffset: isize, bytes: isize, value: i32) {
    if m.offset.is_null() || byteoffset < 0 || bytes <= 0 {
        return;
    }
    let mut bytes = bytes as usize;
    if byteoffset as usize + bytes > m.size as usize {
        bytes = (m.size - byteoffset).max(0) as usize;
    }
    if bytes > 0 {
        ptr::write_bytes(
            (m.offset as *mut u8).add(byteoffset as usize),
            (value & 0xFF) as u8,
            bytes,
        );
    }
}

/// _OFFSET - Get integer representation of pointer.
#[no_mangle]
pub extern "C" fn qb_offset(ptr: *mut c_void) -> isize {
    ptr as isize
}

/// _MEM - Create memory block referencing a variable (pointer + size).
#[no_mangle]
pub extern "C" fn qb_mem_of(ptr: *mut c_void, size: isize) -> QbMem {
    QbMem {
        offset: ptr,
        size: size.max(0),
        type_: 0,
        elementsize: 1,
        image: 0,
        sound: 0,
    }
}

/// _MEMEXISTS - Check if memory block is valid. Returns -1 (true) or 0 (false).
#[no_mangle]
pub extern "C" fn qb_memexists(m: QbMem) -> i32 {
    if !m.offset.is_null() && m.size > 0 {
        -1
    } else {
        0
    }
}

/// _MEMELEMENT - Get _MEM block for array element at index.
#[no_mangle]
pub extern "C" fn qb_memelement(m: QbMem, index: isize) -> QbMem {
    if m.offset.is_null() || m.elementsize <= 0 || index < 0 {
        return QbMem::default();
    }
    let byte_offset = index * m.elementsize;
    if byte_offset >= m.size {
        let mut r = QbMem::default();
        r.type_ = m.type_;
        r.elementsize = m.elementsize;
        r.image = m.image;
        r.sound = m.sound;
        return r;
    }
    let ptr = unsafe { (m.offset as *const u8).add(byte_offset as usize) as *mut c_void };
    QbMem {
        offset: ptr,
        size: m.elementsize,
        type_: m.type_,
        elementsize: m.elementsize,
        image: m.image,
        sound: m.sound,
    }
}

/// _MEMIMAGE - Get _MEM block for image pixel data. Stub: returns empty (no image in external yet).
#[no_mangle]
pub extern "C" fn qb_memimage(_handle: i32) -> QbMem {
    QbMem::default()
}

/// _MEMSOUND - Get _MEM block for sound data. Stub: returns empty.
#[no_mangle]
pub extern "C" fn qb_memsound(_handle: i32) -> QbMem {
    QbMem::default()
}
