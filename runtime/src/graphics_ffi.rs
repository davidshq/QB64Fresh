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

// ============================================================================
// Extended Graphics FFI (WIDTH, VIEW, WINDOW, DRAW)
// ============================================================================

/// Set text mode width.
#[no_mangle]
pub extern "C" fn qb_gfx_set_width(columns: u32, rows: u32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_width(columns, rows) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Set viewport for graphics.
#[no_mangle]
pub extern "C" fn qb_gfx_view(
    screen: c_int,
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    fill_color: i32,
    border_color: i32,
) -> c_int {
    let fill = if fill_color < 0 {
        None
    } else {
        Some(fill_color as u32)
    };
    let border = if border_color < 0 {
        None
    } else {
        Some(border_color as u32)
    };

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_view(screen != 0, x1, y1, x2, y2, fill, border) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Reset viewport to full screen.
#[no_mangle]
pub extern "C" fn qb_gfx_view_reset() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.reset_view() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Set world coordinate system.
#[no_mangle]
pub extern "C" fn qb_gfx_window(screen: c_int, x1: f64, y1: f64, x2: f64, y2: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_window(screen != 0, x1, y1, x2, y2) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Reset window to pixel coordinates.
#[no_mangle]
pub extern "C" fn qb_gfx_window_reset() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.reset_window() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Execute DRAW turtle graphics commands.
///
/// # Safety
/// - `commands` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_gfx_draw(commands: *const c_char) -> c_int {
    if commands.is_null() {
        return 1;
    }

    let cmd_str = match CStr::from_ptr(commands).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        match backend.draw(cmd_str) {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        1
    }
}

/// Set a palette entry.
///
/// # Arguments
/// - `index`: Palette index (0-255)
/// - `color`: ARGB color value
#[no_mangle]
pub extern "C" fn qb_gfx_palette(index: i32, color: u32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_palette(index, color) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Reset palette to default.
#[no_mangle]
pub extern "C" fn qb_gfx_palette_reset() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.reset_palette() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get a palette entry.
#[no_mangle]
pub extern "C" fn qb_gfx_palette_get(index: i32) -> u32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_palette(index)
        } else {
            0
        }
    }
}

// ============================================================================
// QB64 Image Buffer FFI
// ============================================================================

/// Create a new image buffer.
#[no_mangle]
pub extern "C" fn qb_gfx_newimage(width: i32, height: i32, mode: i32) -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.new_image(width, height, mode)
        } else {
            -1
        }
    }
}

/// Load an image from file.
///
/// # Safety
/// - `filename` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_gfx_loadimage(filename: *const c_char, mode: i32) -> i32 {
    if filename.is_null() {
        return -1;
    }

    let fname = match CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        backend.load_image(fname, mode)
    } else {
        -1
    }
}

/// Free an image buffer.
#[no_mangle]
pub extern "C" fn qb_gfx_freeimage(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.free_image(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Simple put_image without source coordinates.
#[no_mangle]
pub extern "C" fn qb_gfx_putimage_simple(src_handle: i32, dest_handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.put_image(0, 0, -1, -1, src_handle, dest_handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Put_image with destination coordinates.
#[no_mangle]
pub extern "C" fn qb_gfx_putimage(
    dx1: i32,
    dy1: i32,
    dx2: i32,
    dy2: i32,
    src_handle: i32,
    dest_handle: i32,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.put_image(dx1, dy1, dx2, dy2, src_handle, dest_handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Full put_image with source and destination coordinates.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub extern "C" fn qb_gfx_putimage_full(
    dx1: i32,
    dy1: i32,
    dx2: i32,
    dy2: i32,
    src_handle: i32,
    dest_handle: i32,
    sx1: i32,
    sy1: i32,
    sx2: i32,
    sy2: i32,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.put_image_full(
                dx1,
                dy1,
                dx2,
                dy2,
                src_handle,
                dest_handle,
                sx1,
                sy1,
                sx2,
                sy2,
            ) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Set source image.
#[no_mangle]
pub extern "C" fn qb_gfx_source(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_source(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Set destination image.
#[no_mangle]
pub extern "C" fn qb_gfx_dest(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_dest(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Print string at pixel coordinates.
///
/// # Safety
/// - `text` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_gfx_printstring(x: i32, y: i32, text: *const c_char) -> c_int {
    if text.is_null() {
        return 1;
    }

    let txt = match CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        match backend.print_string(x, y, txt) {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        1
    }
}

/// Set auto-display mode.
#[no_mangle]
pub extern "C" fn qb_gfx_autodisplay(enabled: c_int) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.set_autodisplay(enabled != 0) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get image width.
#[no_mangle]
pub extern "C" fn qb_gfx_image_width(handle: i32) -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_image_width(handle)
        } else {
            0
        }
    }
}

/// Get image height.
#[no_mangle]
pub extern "C" fn qb_gfx_image_height(handle: i32) -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_image_height(handle)
        } else {
            0
        }
    }
}

// ============================================================================
// Mouse Input Functions (Phase 5)
// ============================================================================

/// _MOUSEX - Get the current mouse X position.
#[no_mangle]
pub extern "C" fn qb_mouse_x() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_mouse_x()
        } else {
            0
        }
    }
}

/// _MOUSEY - Get the current mouse Y position.
#[no_mangle]
pub extern "C" fn qb_mouse_y() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_mouse_y()
        } else {
            0
        }
    }
}

/// _MOUSEBUTTON - Get the state of a mouse button.
///
/// Button 1 = left, 2 = right, 3 = middle.
/// Returns -1 (true) if pressed, 0 (false) otherwise.
#[no_mangle]
pub extern "C" fn qb_mouse_button(button: i32) -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            if backend.get_mouse_button(button as u32) {
                -1 // True in BASIC
            } else {
                0 // False
            }
        } else {
            0
        }
    }
}

/// _MOUSEINPUT - Check for and consume mouse input events.
///
/// Returns -1 (true) if there was input, 0 (false) otherwise.
/// Must be called in a loop to get mouse updates.
#[no_mangle]
pub extern "C" fn qb_mouse_input() -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if backend.poll_mouse_input() {
                -1 // True
            } else {
                0 // False
            }
        } else {
            0
        }
    }
}

/// _MOUSEMOVEMENTX - Get mouse X movement since last call.
#[no_mangle]
pub extern "C" fn qb_mouse_movement_x() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_mouse_movement_x()
        } else {
            0
        }
    }
}

/// _MOUSEMOVEMENTY - Get mouse Y movement since last call.
#[no_mangle]
pub extern "C" fn qb_mouse_movement_y() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_mouse_movement_y()
        } else {
            0
        }
    }
}

/// _MOUSEWHEEL - Get mouse wheel delta.
///
/// Returns number of scroll notches (positive = up, negative = down).
#[no_mangle]
pub extern "C" fn qb_mouse_wheel() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_mouse_wheel()
        } else {
            0
        }
    }
}

/// _MOUSEHIDE - Hide the mouse cursor.
#[no_mangle]
pub extern "C" fn qb_mouse_hide() {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.hide_mouse();
        }
    }
}

/// _MOUSESHOW - Show the mouse cursor.
#[no_mangle]
pub extern "C" fn qb_mouse_show() {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.show_mouse();
        }
    }
}

/// _MOUSEMOVE - Move the mouse cursor to a position.
#[no_mangle]
pub extern "C" fn qb_mouse_move(x: i32, y: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.move_mouse(x, y);
        }
    }
}

// ============================================================================
// Clipboard Functions (Phase 5)
// ============================================================================

/// _CLIPBOARD$ (get) - Get text from the system clipboard.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_clipboard_get() -> *mut crate::string::QbString {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            if let Some(text) = backend.get_clipboard() {
                return crate::string::qb_string_from_bytes(text.as_ptr(), text.len());
            }
        }
        crate::string::qb_string_empty()
    }
}

/// _CLIPBOARD$ = text$ - Set text to the system clipboard.
///
/// # Safety
/// - `text` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_clipboard_set(text: *const std::os::raw::c_char) {
    if text.is_null() {
        return;
    }

    let text_str = match std::ffi::CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        backend.set_clipboard(text_str);
    }
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
