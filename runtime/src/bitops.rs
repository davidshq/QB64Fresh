//! Bit field get/set operations (libqb bitops.h compatibility).
//!
//! Provides `getubits`, `getbits`, and `setbits` for reading/writing
//! bit fields in a byte buffer. Used by QB64 TYPE handling and
//! DECLARE LIBRARY code.

/// Read an unsigned bit field from a byte buffer.
///
/// # Arguments
///
/// * `bsize` - Number of bits to read (1..64).
/// * `base` - Pointer to the byte buffer.
/// * `i` - Element index (logical element; bits start at `i * bsize`).
///
/// # Returns
///
/// The value of the bit field as an unsigned 64-bit integer.
///
/// # Safety
///
/// Caller must ensure `base` points to at least `((i * bsize + bsize + 7) / 8)` bytes.
#[no_mangle]
pub unsafe extern "C" fn qb_getubits(bsize: u32, base: *mut u8, i: isize) -> u64 {
    if base.is_null() || bsize == 0 || bsize > 64 {
        return 0;
    }
    let bmask = !((-(1i64 << bsize)) as u64);
    let bit_offset = (i as i64 * bsize as i64) as usize;
    let byte_offset = bit_offset >> 3;
    let shift = bit_offset & 7;
    let ptr = base.add(byte_offset) as *const u64;
    let word = ptr.read_unaligned();
    ((word >> shift) & bmask) as u64
}

/// Read a signed bit field from a byte buffer.
///
/// Sign-extends the value if the high bit is set.
///
/// # Arguments
///
/// * `bsize` - Number of bits to read (1..64).
/// * `base` - Pointer to the byte buffer.
/// * `i` - Element index (logical element; bits start at `i * bsize`).
///
/// # Returns
///
/// The value of the bit field, sign-extended to 64 bits.
///
/// # Safety
///
/// Caller must ensure `base` points to at least `((i * bsize + bsize + 7) / 8)` bytes.
#[no_mangle]
pub unsafe extern "C" fn qb_getbits(bsize: u32, base: *mut u8, i: isize) -> i64 {
    if base.is_null() || bsize == 0 || bsize > 64 {
        return 0;
    }
    let bmask = !((-(1i64 << bsize)) as u64);
    let bit_offset = (i as i64 * bsize as i64) as usize;
    let byte_offset = bit_offset >> 3;
    let shift = bit_offset & 7;
    let ptr = base.add(byte_offset) as *const u64;
    let word = ptr.read_unaligned();
    let bval64 = (word >> shift) & bmask;
    let sign_bit = 1u64 << (bsize - 1);
    if (bval64 & sign_bit) != 0 {
        (bval64 | !bmask) as i64
    } else {
        bval64 as i64
    }
}

/// Write a bit field into a byte buffer.
///
/// # Arguments
///
/// * `bsize` - Number of bits to write (1..64).
/// * `base` - Pointer to the byte buffer.
/// * `i` - Element index (logical element; bits start at `i * bsize`).
/// * `val` - Value to write (only low `bsize` bits are stored).
///
/// # Safety
///
/// Caller must ensure `base` points to at least `((i * bsize + bsize + 7) / 8)` bytes.
#[no_mangle]
pub unsafe extern "C" fn qb_setbits(bsize: u32, base: *mut u8, i: isize, val: i64) {
    if base.is_null() || bsize == 0 || bsize > 64 {
        return;
    }
    let bmask = ((1u64 << bsize) - 1) as i64;
    let bit_offset = (i as i64 * bsize as i64) as usize;
    let byte_offset = bit_offset >> 3;
    let shift = bit_offset & 7;
    let bptr64 = base.add(byte_offset) as *mut u64;
    let clear_mask = !((bmask as u64) << shift);
    let new_bits = ((val & bmask) as u64) << shift;
    let old = bptr64.read_unaligned();
    bptr64.write_unaligned((old & clear_mask) | new_bits);
}
