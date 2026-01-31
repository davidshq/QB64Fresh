//! Generic byte buffer (libqb_buffer) for QB64pe compatibility.
//!
//! Provides a FIFO buffer with `libqb_buffer_init`, `libqb_buffer_clear`,
//! `libqb_buffer_length`, `libqb_buffer_read`, and `libqb_buffer_write`.
//! Structure layout and semantics match QB64pe's `internal/c/libqb/include/buffer.h`
//! so generated or migrated C code can use the same API.

use std::alloc::{alloc, dealloc, Layout};
use std::ffi::c_char;
use std::ptr;

/// Single linked-list entry holding a contiguous byte chunk.
/// Layout must match `struct libqb_buffer_entry` in qb64fresh_rt.h.
#[repr(C)]
pub struct LibqbBufferEntry {
    /// Length of data in this entry.
    pub length: usize,
    /// Allocated data (owned by this entry; freed in clear/read).
    pub data: *mut c_char,
    /// Next entry in the list, or NULL.
    pub next: *mut LibqbBufferEntry,
}

/// FIFO buffer: write appends chunks, read consumes from the head.
/// Layout must match `struct libqb_buffer` in qb64fresh_rt.h.
#[repr(C)]
pub struct LibqbBuffer {
    /// Total number of bytes currently in the buffer.
    pub total_length: usize,
    /// Offset within the first entry already consumed (for partial reads).
    pub cur_entry_offset: usize,
    /// Head of the linked list of entries.
    pub head: *mut LibqbBufferEntry,
    /// Pointer to the "next" slot of the last entry (or &head when empty).
    pub tail: *mut *mut LibqbBufferEntry,
}

/// Initialize a buffer. Call before any other buffer function.
///
/// # Safety
/// `buffer` must point to a valid `LibqbBuffer` (e.g. stack or heap).
#[no_mangle]
pub unsafe extern "C" fn libqb_buffer_init(buffer: *mut LibqbBuffer) {
    if buffer.is_null() {
        return;
    }
    ptr::write_bytes(buffer as *mut u8, 0, std::mem::size_of::<LibqbBuffer>());
    // Reborrow after zeroing: same address, now zeroed; set tail to point at head slot.
    let b = &mut *buffer;
    b.tail = &mut b.head;
}

/// Free one entry: free data and the entry itself.
unsafe fn libqb_buffer_entry_free(ent: *mut LibqbBufferEntry) {
    if ent.is_null() {
        return;
    }
    let e = &*ent;
    if !e.data.is_null() {
        // Only dealloc with the same layout used to alloc; skip if invalid (avoids wrong dealloc).
        if let Ok(layout) = Layout::from_size_align(e.length, 1) {
            dealloc(e.data as *mut u8, layout);
        }
    }
    let layout = Layout::new::<LibqbBufferEntry>();
    dealloc(ent as *mut u8, layout);
}

/// Clear the buffer and free all entries.
///
/// # Safety
/// `buffer` must point to a valid, initialized `LibqbBuffer`.
#[no_mangle]
pub unsafe extern "C" fn libqb_buffer_clear(buffer: *mut LibqbBuffer) {
    if buffer.is_null() {
        return;
    }
    let b = &mut *buffer;
    let mut entry = b.head;
    while !entry.is_null() {
        let nxt = (*entry).next;
        libqb_buffer_entry_free(entry);
        entry = nxt;
    }
    libqb_buffer_init(buffer);
}

/// Return the current number of bytes in the buffer.
///
/// # Safety
/// `buffer` must point to a valid, initialized `LibqbBuffer`.
#[no_mangle]
pub unsafe extern "C" fn libqb_buffer_length(buffer: *const LibqbBuffer) -> usize {
    if buffer.is_null() {
        return 0;
    }
    (*buffer).total_length
}

/// Read up to `length` bytes from the buffer into `out`. Bytes read are consumed.
/// Returns the number of bytes actually read.
///
/// # Safety
/// `buffer` must point to a valid, initialized `LibqbBuffer`. `out` must point to
/// at least `length` bytes of writable memory.
#[no_mangle]
pub unsafe extern "C" fn libqb_buffer_read(
    buffer: *mut LibqbBuffer,
    out: *mut c_char,
    length: usize,
) -> usize {
    if buffer.is_null() || out.is_null() {
        return 0;
    }
    let b = &mut *buffer;
    let mut out = out;
    let mut remaining = length;
    let mut actual_length = 0usize;

    while !b.head.is_null() && remaining > 0 {
        let entry = b.head;
        let offset = b.cur_entry_offset;
        let avail = (*entry).length.saturating_sub(offset);
        let len = avail.min(remaining);

        if len > 0 {
            ptr::copy_nonoverlapping((*entry).data.add(offset), out, len);
            out = out.add(len);
            remaining -= len;
            actual_length += len;
        }

        if len == avail {
            b.head = (*entry).next;
            b.cur_entry_offset = 0;
            libqb_buffer_entry_free(entry);
        } else {
            b.cur_entry_offset = offset + len;
            break;
        }
    }

    if b.head.is_null() {
        b.tail = &mut b.head;
    }
    b.total_length -= actual_length;
    actual_length
}

/// Append `length` bytes from `in_` to the buffer.
/// Does nothing if `length` is 0 (no zero-length chunk is appended).
///
/// # Safety
/// `buffer` must point to a valid, initialized `LibqbBuffer`. `in_` must point
/// to at least `length` bytes of readable memory (or may be null if length is 0).
#[no_mangle]
pub unsafe extern "C" fn libqb_buffer_write(
    buffer: *mut LibqbBuffer,
    in_: *const c_char,
    length: usize,
) {
    if buffer.is_null() || (in_.is_null() && length > 0) {
        return;
    }
    if length == 0 {
        return;
    }
    let b = &mut *buffer;
    let Ok(data_layout) = Layout::from_size_align(length, 1) else {
        return; // length overflow or invalid (e.g. > isize::MAX)
    };
    let layout = Layout::new::<LibqbBufferEntry>();
    let new_ent = alloc(layout) as *mut LibqbBufferEntry;
    if new_ent.is_null() {
        return;
    }
    let data = alloc(data_layout) as *mut c_char;
    if data.is_null() {
        dealloc(new_ent as *mut u8, layout);
        return;
    }
    ptr::copy_nonoverlapping(in_, data, length);
    ptr::write(
        new_ent,
        LibqbBufferEntry {
            length,
            data,
            next: ptr::null_mut(),
        },
    );
    *b.tail = new_ent;
    b.tail = &mut (*new_ent).next;
    b.total_length += length;
}
