//! Conventional memory (cmem) and DBLOCK for libqb/Play() compatibility.
//!
//! Provides the same symbols as QB64pe's `cmem.h`:
//! - `cmem[1114099]` — conventional memory block (16*65535+65535+3 bytes)
//! - `dblock` — base address of DBLOCK region at offset 1280; required for Play()
//!
//! Used by PEEK/POKE (inline runtime only today) and by Play() when implemented.

/// Size of the conventional memory block (matches libqb cmem.h).
/// 16*65535 + 65535 + 3 = 1,114,099 bytes.
pub const QB_CMEM_SIZE: usize = 1114099;

/// Byte offset of DBLOCK within cmem (matches libqb).
pub const QB_DBLOCK_OFFSET: usize = 1280;

/// Conventional memory block. Exported as `uint8_t cmem[1114099]` for C.
#[no_mangle]
pub static mut cmem: [u8; QB_CMEM_SIZE] = [0; QB_CMEM_SIZE];

/// Base address of DBLOCK; required for Play(). Set in qb_runtime_init().
#[no_mangle]
pub static mut dblock: usize = 0;

/// Initialize dblock to point at DBLOCK. Call from qb_runtime_init().
pub fn init_dblock() {
    unsafe {
        dblock = std::ptr::addr_of!(cmem).add(QB_DBLOCK_OFFSET) as usize;
    }
}
