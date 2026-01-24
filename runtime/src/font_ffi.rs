//! C FFI layer for font operations.
//!
//! This module provides C-compatible functions for font loading, freeing,
//! and Unicode text rendering. These functions wrap the FontManager and
//! GraphicsBackend trait methods.
//!
//! # Thread Safety
//!
//! These functions access a global mutex-protected FontManager.
//! They are safe to call from multiple threads, but graphics operations
//! should still be single-threaded for the SDL2 backend.

use std::ffi::CStr;
use std::os::raw::c_char;

// Import QbString type and accessor functions from our string module
use crate::string::{qb_string_data, qb_string_len, QbString};

// ============================================================================
// Font Loading Functions
// ============================================================================

/// Load a TrueType/OpenType font from a file.
///
/// # Arguments
/// - `path`: Path to the font file (C string)
/// - `size`: Pixel height of the font
/// - `options`: Font loading options:
///   - 8: DONTBLEND (no anti-aliasing)
///   - 16: MONOSPACE (force fixed width)
///   - 32: UNICODE (UTF-8 input mode)
///   - 64: AUTOMONO (auto-detect monospace)
///
/// # Returns
/// Font handle on success (positive), or 0 on failure
#[no_mangle]
#[cfg(feature = "freetype")]
pub extern "C" fn qb_loadfont(path: *const c_char, size: i32, options: i32) -> i64 {
    if path.is_null() {
        return 0;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    use crate::font_manager::FONT_MANAGER;
    let mut fm = FONT_MANAGER.lock().unwrap();
    fm.load_font(path_str, size as u16, options as u32)
}

#[no_mangle]
#[cfg(not(feature = "freetype"))]
pub extern "C" fn qb_loadfont(_path: *const c_char, _size: i32, _options: i32) -> i64 {
    // FreeType not enabled, try falling back to graphics backend
    0
}

/// Load a font using QbString path.
///
/// This is the variant called by generated code with QB strings.
#[no_mangle]
#[cfg(feature = "freetype")]
pub extern "C" fn qb_loadfont_qb(path: *const QbString, size: i64, options: i32) -> i64 {
    if path.is_null() {
        return 0;
    }

    let path_str = unsafe {
        let len = qb_string_len(path);
        if len == 0 {
            return 0;
        }
        let data = qb_string_data(path);
        if data.is_null() {
            return 0;
        }
        match std::str::from_utf8(std::slice::from_raw_parts(data as *const u8, len)) {
            Ok(s) => s,
            Err(_) => return 0,
        }
    };

    use crate::font_manager::FONT_MANAGER;
    let mut fm = FONT_MANAGER.lock().unwrap();
    fm.load_font(path_str, size as u16, options as u32)
}

#[no_mangle]
#[cfg(not(feature = "freetype"))]
pub extern "C" fn qb_loadfont_qb(_path: *const QbString, _size: i64, _options: i32) -> i64 {
    0
}

/// Free a loaded font.
///
/// # Arguments
/// - `handle`: Font handle returned by qb_loadfont
#[no_mangle]
#[cfg(feature = "freetype")]
pub extern "C" fn qb_freefont(handle: i64) {
    if handle <= 0 {
        return; // Built-in font cannot be freed
    }

    use crate::font_manager::FONT_MANAGER;
    let mut fm = FONT_MANAGER.lock().unwrap();
    fm.free_font(handle);
}

#[no_mangle]
#[cfg(not(feature = "freetype"))]
pub extern "C" fn qb_freefont(_handle: i64) {
    // No-op when FreeType is not enabled
}

// ============================================================================
// Unicode Text Rendering Functions
// ============================================================================

/// Print Unicode text at pixel coordinates.
///
/// Uses the current font to render UTF-8 text at the specified position.
///
/// # Arguments
/// - `x`, `y`: Pixel coordinates for text origin
/// - `text`: UTF-8 encoded text (QbString pointer)
#[no_mangle]
pub extern "C" fn qb_uprintstring(x: i64, y: i64, text: *const QbString) {
    if text.is_null() {
        return;
    }

    let text_str = unsafe {
        let len = qb_string_len(text);
        if len == 0 {
            return;
        }
        let data = qb_string_data(text);
        if data.is_null() {
            return;
        }
        match std::str::from_utf8(std::slice::from_raw_parts(data as *const u8, len)) {
            Ok(s) => s,
            Err(_) => return,
        }
    };

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            let _ = backend.print_string_unicode(x as i32, y as i32, text_str);
        }
    }
}

/// Get the pixel width of Unicode text.
///
/// Returns the total advance width for rendering the given UTF-8 text
/// with the current font.
///
/// # Arguments
/// - `text`: UTF-8 encoded text (QbString pointer)
///
/// # Returns
/// Width in pixels
#[no_mangle]
pub extern "C" fn qb_uprintwidth(text: *const QbString) -> i64 {
    if text.is_null() {
        return 0;
    }

    let text_str = unsafe {
        let len = qb_string_len(text);
        if len == 0 {
            return 0;
        }
        let data = qb_string_data(text);
        if data.is_null() {
            return 0;
        }
        match std::str::from_utf8(std::slice::from_raw_parts(data as *const u8, len)) {
            Ok(s) => s,
            Err(_) => return 0,
        }
    };

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_print_width_unicode(text_str)
        } else {
            0
        }
    }
}

/// Get the height of a font in pixels.
///
/// # Arguments
/// - `handle`: Font handle (0 = current font)
///
/// # Returns
/// Font height in pixels
#[no_mangle]
pub extern "C" fn qb_ufontheight(handle: i64) -> i64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_unicode_font_height(handle)
        } else {
            16 // Default height
        }
    }
}

/// Get the line spacing for the current font.
///
/// This is typically the font height plus any extra leading.
///
/// # Returns
/// Line spacing in pixels
#[no_mangle]
pub extern "C" fn qb_ulinespacing() -> i64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_unicode_line_spacing()
        } else {
            16 // Default line spacing
        }
    }
}

/// Get the x-position of a character in a text string.
///
/// Returns the x-offset where the character at the given index would be rendered.
///
/// # Arguments
/// - `text`: UTF-8 encoded text (QbString pointer)
/// - `pos`: Character index (1-based, QB64 convention)
///
/// # Returns
/// X position in pixels, or -1 if position is invalid
#[no_mangle]
pub extern "C" fn qb_ucharpos(text: *const QbString, pos: i64) -> i64 {
    if text.is_null() || pos < 1 {
        return -1;
    }

    let text_str = unsafe {
        let len = qb_string_len(text);
        if len == 0 {
            return -1;
        }
        let data = qb_string_data(text);
        if data.is_null() {
            return -1;
        }
        match std::str::from_utf8(std::slice::from_raw_parts(data as *const u8, len)) {
            Ok(s) => s,
            Err(_) => return -1,
        }
    };

    let index = (pos - 1) as usize; // Convert to 0-based

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            let positions = backend.get_unicode_char_positions(text_str);
            if index < positions.len() {
                positions[index]
            } else if index == positions.len() {
                // Position after last character = total width
                backend.get_print_width_unicode(text_str)
            } else {
                -1
            }
        } else {
            // Fallback: assume 8 pixels per character
            (index as i64) * 8
        }
    }
}

// ============================================================================
// Font Query Functions
// ============================================================================

/// Get the current font handle.
///
/// # Returns
/// Current font handle (0 = built-in font)
#[no_mangle]
pub extern "C" fn qb_font_get() -> i64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_current_font()
        } else {
            0
        }
    }
}

/// Get the height of the current font.
///
/// # Returns
/// Font height in pixels
#[no_mangle]
pub extern "C" fn qb_fontheight() -> i64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_font_height() as i64
        } else {
            16
        }
    }
}

/// Get the width of the current font (for monospace).
///
/// # Returns
/// Font width in pixels
#[no_mangle]
pub extern "C" fn qb_fontwidth() -> i64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_font_width() as i64
        } else {
            8
        }
    }
}

/// Get the pixel width of a string with the current font.
///
/// # Arguments
/// - `text`: Text string (QbString pointer)
///
/// # Returns
/// Width in pixels
#[no_mangle]
pub extern "C" fn qb_printwidth(text: *const QbString) -> i64 {
    if text.is_null() {
        return 0;
    }

    unsafe {
        let len = qb_string_len(text);
        if len == 0 {
            return 0;
        }
        let data = qb_string_data(text);
        if data.is_null() {
            return 0;
        }

        let text_str = match std::str::from_utf8(std::slice::from_raw_parts(data as *const u8, len))
        {
            Ok(s) => s,
            Err(_) => {
                // If not valid UTF-8, fall back to byte count
                return (len as i64) * 8;
            }
        };

        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_print_width(text_str)
        } else {
            (text_str.len() as i64) * 8
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    // Font FFI tests would require a graphics context, so we just
    // verify the module compiles correctly.

    #[test]
    fn test_module_compiles() {
        // This test just ensures the module compiles without errors
        assert!(true);
    }
}
