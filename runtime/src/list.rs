//! Handle list (qblist.h compatibility).
//!
//! Thread-safe list of fixed-size "slots" used for handle tables (e.g. images,
//! streams). Index 0 is unused; indices are 1-based. Each slot stores
//! [user_data | index] so list_get_index can recover the index from a pointer.
//!
//! Matches QB64pe libqb qblist.h / qblist.cpp semantics for list_new,
//! list_new_threadsafe, list_destroy, list_add, list_remove, list_get,
//! list_get_index.

use std::alloc::{alloc, dealloc, Layout};
use std::ptr;
use std::sync::Mutex;

/// Opaque handle list. C code only sees `list*`.
#[repr(C)]
pub struct List {
    /// User-requested size of each element (bytes).
    user_structure_size: usize,
    /// user_structure_size + size_of::<isize>() for stored index.
    internal_structure_size: usize,
    /// Current number of active (non-freed) slots.
    structures: usize,
    /// Capacity of the current structure block (slots).
    structures_last: usize,
    /// Indexes of freed slots available for reuse (1-based).
    structure_freed: Vec<isize>,
    /// Next index to assign when not reusing (1-based). Max index ever handed out.
    indexes: isize,
    /// index[i] = pointer to slot for index i (0 unused).
    index: Vec<*mut u8>,
    /// For threadsafe lists, guards add/remove.
    lock: Option<Mutex<()>>,
    /// Allocated structure blocks (for destroy).
    structure_bases: Vec<*mut u8>,
    /// Size in bytes of each block in structure_bases (for dealloc).
    structure_base_sizes: Vec<usize>,
}

/// Allocate a new structure block and append one slot; return pointer to the new slot.
/// Caller ensures we need to grow (structures + 1 > structures_last).
fn grow_and_append(list: &mut List) -> *mut u8 {
    let new_last = list.structures_last.saturating_mul(2).saturating_add(1);
    let block_size = list.internal_structure_size * (new_last + 1);
    let layout = Layout::from_size_align(block_size, 1).expect("list block layout");
    let block = unsafe { alloc(layout) };
    if block.is_null() {
        return ptr::null_mut();
    }
    unsafe { ptr::write_bytes(block, 0, block_size) };
    list.structure_bases.push(block);
    list.structure_base_sizes.push(block_size);
    list.structures_last = new_last;
    list.structures = 0;
    // First slot in the new block is at block + internal_structure_size (index 0 unused in block)
    let slot = unsafe { block.add(list.internal_structure_size) };
    list.structures = 1;
    slot
}

/// Add a slot: either reuse a freed index or allocate a new one.
fn list_add_impl(list: &mut List) -> isize {
    let internal_size = list.internal_structure_size;
    let user_size = list.user_structure_size;

    if let Some(i) = list.structure_freed.pop() {
        let slot_ptr = list.index[i as usize];
        if !slot_ptr.is_null() {
            unsafe {
                ptr::write_bytes(slot_ptr, 0, user_size);
                *(slot_ptr.add(user_size) as *mut isize) = i;
            }
            return i;
        }
    }

    // Need new slot.
    if list.structures + 1 > list.structures_last {
        let slot = grow_and_append(list);
        if slot.is_null() {
            return 0;
        }
        list.indexes += 1;
        let i = list.indexes;
        unsafe {
            *(slot.add(user_size) as *mut isize) = i;
        }
        while list.index.len() <= i as usize {
            list.index.push(ptr::null_mut());
        }
        list.index[i as usize] = slot;
        return i;
    }

    // Current block has room. We store slots in the current last block.
    let base = *list.structure_bases.last().unwrap();
    list.structures += 1;
    let slot = unsafe { base.add(internal_size * list.structures) };
    list.indexes += 1;
    let i = list.indexes;
    unsafe {
        ptr::write_bytes(slot, 0, user_size);
        *(slot.add(user_size) as *mut isize) = i;
    }
    while list.index.len() <= i as usize {
        list.index.push(ptr::null_mut());
    }
    list.index[i as usize] = slot;
    i
}

/// Create a new list with the given user structure size (bytes).
/// Returns null on allocation failure.
#[no_mangle]
pub extern "C" fn list_new(structure_size: isize) -> *mut List {
    if structure_size <= 0 {
        return ptr::null_mut();
    }
    let user_size = structure_size as usize;
    let internal_size = user_size + std::mem::size_of::<isize>();
    let list = List {
        user_structure_size: user_size,
        internal_structure_size: internal_size,
        structures: 0,
        structures_last: 0,
        structure_freed: Vec::new(),
        indexes: 0,
        index: vec![ptr::null_mut()],
        lock: None,
        structure_bases: Vec::new(),
    };
    let b = Box::new(list);
    Box::into_raw(b)
}

/// Create a new thread-safe list (mutex guards add/remove).
#[no_mangle]
pub extern "C" fn list_new_threadsafe(structure_size: isize) -> *mut List {
    let l = list_new(structure_size);
    if l.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        (*l).lock = Some(Mutex::new(()));
    }
    l
}

/// Destroy the list and free all blocks.
#[no_mangle]
pub unsafe extern "C" fn list_destroy(l: *mut List) {
    if l.is_null() {
        return;
    }
    let list = Box::from_raw(l);
    for (base, &block_size) in list.structure_bases.iter().zip(list.structure_base_sizes.iter()) {
        if !base.is_null() && block_size > 0 {
            let layout = Layout::from_size_align(block_size, 1).expect("list block layout");
            dealloc(*base, layout);
        }
    }
}

/// Add an entry. Returns the new index (1-based), or 0 on failure.
#[no_mangle]
pub unsafe extern "C" fn list_add(l: *mut List) -> isize {
    if l.is_null() {
        return 0;
    }
    let list = &mut *l;
    if let Some(ref lock) = list.lock {
        let _guard = match lock.lock() {
            Ok(g) => g,
            Err(_) => return 0,
        };
        list_add_impl(list)
    } else {
        list_add_impl(list)
    }
}

/// Remove the entry at index i. Returns -1 on success, 0 on failure.
#[no_mangle]
pub unsafe extern "C" fn list_remove(l: *mut List, i: isize) -> isize {
    if l.is_null() {
        return 0;
    }
    let list = &mut *l;
    if let Some(ref lock) = list.lock {
        let _guard = match lock.lock() {
            Ok(g) => g,
            Err(_) => return 0,
        };
        list_remove_impl(list, i)
    } else {
        list_remove_impl(list, i)
    }
}

fn list_remove_impl(list: &mut List, i: isize) -> isize {
    if i < 1 || i > list.indexes {
        return 0;
    }
    let idx = i as usize;
    if idx >= list.index.len() {
        return 0;
    }
    let slot = list.index[idx];
    if slot.is_null() {
        return 0;
    }
    let stored = unsafe { *(slot.add(list.user_structure_size) as *const isize) };
    if stored == 0 {
        return 0; // already removed
    }
    unsafe {
        *(slot.add(list.user_structure_size) as *mut isize) = 0;
    }
    list.structure_freed.push(i);
    -1
}

/// Get pointer to the user structure at index i. Returns null if invalid or removed.
#[no_mangle]
pub unsafe extern "C" fn list_get(l: *mut List, i: isize) -> *mut std::ffi::c_void {
    if l.is_null() {
        return ptr::null_mut();
    }
    let list = &*l;
    if i < 1 || i > list.indexes {
        return ptr::null_mut();
    }
    let idx = i as usize;
    if idx >= list.index.len() {
        return ptr::null_mut();
    }
    let slot = list.index[idx];
    if slot.is_null() {
        return ptr::null_mut();
    }
    let stored = *(slot.add(list.user_structure_size) as *const isize);
    if stored == 0 {
        return ptr::null_mut();
    }
    slot as *mut std::ffi::c_void
}

/// Get the index of the structure pointed to by `structure` (must be a pointer
/// returned from list_get for this list). Reads the stored index past the user data.
#[no_mangle]
pub unsafe extern "C" fn list_get_index(l: *mut List, structure: *const std::ffi::c_void) -> isize {
    if l.is_null() || structure.is_null() {
        return 0;
    }
    let list = &*l;
    let ptr = structure as *const u8;
    let i = *(ptr.add(list.user_structure_size) as *const isize);
    i
}
