//! C FFI layer for graphics operations.
//!
//! This module provides C-compatible functions that can be called from
//! generated C code. Each function wraps the corresponding GraphicsBackend
//! trait method.
//!
//! # Error Handling
//!
//! All functions return an `i32`:
//! - `0`: Success
//! - Non-zero: Error (specific codes TBD)
//!
//! # Thread Safety
//!
//! These functions are NOT thread-safe. They access a global mutable state.
//! Generated programs should be single-threaded for graphics operations.

use std::ffi::CStr;
use std::os::raw::{c_char, c_int};

/// Initialize the graphics system.
///
/// # Arguments
/// - `width`: Screen width in pixels
/// - `height`: Screen height in pixels
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_init(width: u32, height: u32) -> c_int {
    match crate::graphics::init_graphics(width, height) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// Shut down the graphics system.
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_shutdown() -> c_int {
    match crate::graphics::shutdown_graphics() {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// Clear the screen with the current background color.
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_cls() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.cls() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1 // Not initialized
        }
    }
}

/// Set the foreground and background colors.
///
/// # Arguments
/// - `foreground`: Foreground color (ARGB format)
/// - `background`: Background color (ARGB format)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_color(foreground: u32, background: u32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_color(foreground, background) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get the current foreground color.
///
/// # Returns
/// - The foreground color in ARGB format
#[no_mangle]
pub extern "C" fn qb_gfx_get_foreground() -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_foreground_color()
        } else {
            0xFFFFFFFF // Default white
        }
    }
}

/// Get the current background color.
///
/// # Returns
/// - The background color in ARGB format
#[no_mangle]
pub extern "C" fn qb_gfx_get_background() -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_background_color()
        } else {
            0xFF000000 // Default black
        }
    }
}

/// Move the text cursor to the specified position.
///
/// # Arguments
/// - `row`: Row number (1-based)
/// - `col`: Column number (1-based)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_locate(row: u32, col: u32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.locate(row, col) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get the current cursor row.
///
/// # Returns
/// - The current row (1-based)
#[no_mangle]
pub extern "C" fn qb_gfx_csrlin() -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_cursor_position().0
        } else {
            1
        }
    }
}

/// Get the current cursor column.
///
/// # Returns
/// - The current column (1-based)
#[no_mangle]
pub extern "C" fn qb_gfx_pos() -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_cursor_position().1
        } else {
            1
        }
    }
}

/// Print text at the current cursor position.
///
/// # Arguments
/// - `text`: Null-terminated C string to print
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
///
/// # Safety
/// The `text` pointer must be valid and null-terminated.
#[no_mangle]
pub unsafe extern "C" fn qb_gfx_print(text: *const c_char) -> c_int {
    if text.is_null() {
        return 1;
    }

    let c_str = match CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        match backend.print(c_str) {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        1
    }
}

/// Plot a pixel at the specified coordinates.
///
/// # Arguments
/// - `x`: X coordinate
/// - `y`: Y coordinate
/// - `color`: Pixel color (ARGB format)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_pset(x: i32, y: i32, color: u32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.pset(x, y, color) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get the color of a pixel at the specified coordinates.
///
/// # Arguments
/// - `x`: X coordinate
/// - `y`: Y coordinate
///
/// # Returns
/// - The pixel color (ARGB format), or 0 on error
#[no_mangle]
pub extern "C" fn qb_gfx_point(x: i32, y: i32) -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.point(x, y).unwrap_or(0)
        } else {
            0
        }
    }
}

/// Draw a line from (x1, y1) to (x2, y2).
///
/// # Arguments
/// - `x1`, `y1`: Start coordinates
/// - `x2`, `y2`: End coordinates
/// - `color`: Line color (ARGB format)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_line(x1: i32, y1: i32, x2: i32, y2: i32, color: u32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.line(x1, y1, x2, y2, color, false) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Draw a box (rectangle outline or filled).
///
/// # Arguments
/// - `x1`, `y1`: First corner coordinates
/// - `x2`, `y2`: Opposite corner coordinates
/// - `color`: Box color (ARGB format)
/// - `filled`: 0 for outline, non-zero for filled
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_box(
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    color: u32,
    filled: c_int,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.line(x1, y1, x2, y2, color, filled != 0) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Draw a circle.
///
/// # Arguments
/// - `x`, `y`: Center coordinates
/// - `radius`: Circle radius
/// - `color`: Circle color (ARGB format)
/// - `filled`: 0 for outline, non-zero for filled
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_circle(x: i32, y: i32, radius: i32, color: u32, filled: c_int) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.circle(x, y, radius, color, filled != 0) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Flood fill starting from the given point.
///
/// # Arguments
/// - `x`, `y`: Starting point
/// - `color`: Fill color (ARGB format)
/// - `boundary_color`: Boundary color (-1 for match mode, else boundary mode)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_paint(x: i32, y: i32, color: u32, boundary_color: i32) -> c_int {
    let boundary = if boundary_color < 0 {
        None
    } else {
        Some(boundary_color as u32)
    };

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.paint(x, y, color, boundary) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Update the display with the current backbuffer.
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_display() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.display() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Poll for window events.
///
/// # Returns
/// - `1` if window should remain open
/// - `0` if window should close (user clicked X or pressed Escape)
/// - `-1` on error
#[no_mangle]
pub extern "C" fn qb_gfx_poll_events() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.poll_events() {
                Ok(true) => 1,
                Ok(false) => 0,
                Err(_) => -1,
            }
        } else {
            -1
        }
    }
}

/// Get the screen width.
///
/// # Returns
/// - The screen width in pixels
#[no_mangle]
pub extern "C" fn qb_gfx_width() -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_screen_size().0
        } else {
            0
        }
    }
}

/// Get the screen height.
///
/// # Returns
/// - The screen height in pixels
#[no_mangle]
pub extern "C" fn qb_gfx_height() -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_screen_size().1
        } else {
            0
        }
    }
}

/// Convert RGB values to an ARGB color value.
///
/// # Arguments
/// - `r`: Red component (0-255)
/// - `g`: Green component (0-255)
/// - `b`: Blue component (0-255)
///
/// # Returns
/// - ARGB color value with alpha = 255
#[no_mangle]
pub extern "C" fn qb_rgb(r: u32, g: u32, b: u32) -> u32 {
    0xFF000000 | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)
}

/// Convert RGBA values to an ARGB color value.
///
/// # Arguments
/// - `r`: Red component (0-255)
/// - `g`: Green component (0-255)
/// - `b`: Blue component (0-255)
/// - `a`: Alpha component (0-255)
///
/// # Returns
/// - ARGB color value
#[no_mangle]
pub extern "C" fn qb_rgba(r: u32, g: u32, b: u32, a: u32) -> u32 {
    ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)
}

/// Alias for qb_rgb (32-bit color).
#[no_mangle]
pub extern "C" fn qb_rgb32(r: u32, g: u32, b: u32) -> u32 {
    qb_rgb(r, g, b)
}

/// Alias for qb_rgba (32-bit color with alpha).
#[no_mangle]
pub extern "C" fn qb_rgba32(r: u32, g: u32, b: u32, a: u32) -> u32 {
    qb_rgba(r, g, b, a)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rgb_functions() {
        assert_eq!(qb_rgb(255, 0, 0), 0xFFFF0000); // Red
        assert_eq!(qb_rgb(0, 255, 0), 0xFF00FF00); // Green
        assert_eq!(qb_rgb(0, 0, 255), 0xFF0000FF); // Blue
        assert_eq!(qb_rgb(255, 255, 255), 0xFFFFFFFF); // White
        assert_eq!(qb_rgb(0, 0, 0), 0xFF000000); // Black
    }

    #[test]
    fn test_rgba_functions() {
        assert_eq!(qb_rgba(255, 0, 0, 128), 0x80FF0000); // Semi-transparent red
        assert_eq!(qb_rgba(0, 0, 0, 0), 0x00000000); // Fully transparent
    }

    #[test]
    fn test_init_without_display() {
        // In a headless environment, init should fail gracefully
        let result = qb_gfx_init(320, 200);
        // Result depends on whether display is available
        // Just ensure it doesn't panic
        if result == 0 {
            let _ = qb_gfx_shutdown();
        }
    }
}
