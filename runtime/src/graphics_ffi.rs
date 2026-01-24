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

/// Set graphics mode using classic SCREEN mode numbers.
///
/// Maps traditional QBasic/QB64 SCREEN modes to appropriate dimensions
/// and initializes the graphics system.
///
/// # Arguments
/// - `mode`: SCREEN mode number (0-13 for classic, higher for QB64 extensions)
/// - `color_switch`: Color switch parameter (ignored in 32-bit modes)
/// - `active_page`: Active page number (default 0)
/// - `visual_page`: Visual page number (default 0)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
///
/// # Classic SCREEN Modes
/// - 0: Text mode (80x25 or 40x25 characters)
/// - 1: 320x200, 4 colors
/// - 2: 640x200, 2 colors
/// - 7: 320x200, 16 colors
/// - 8: 640x200, 16 colors
/// - 9: 640x350, 16 colors
/// - 10: 640x350, 2 colors (monochrome)
/// - 11: 640x480, 2 colors
/// - 12: 640x480, 16 colors
/// - 13: 320x200, 256 colors
#[no_mangle]
pub extern "C" fn qb_gfx_screen(
    mode: i32,
    _color_switch: i32,
    _active_page: i32,
    _visual_page: i32,
) -> c_int {
    // Map SCREEN mode to dimensions
    let (width, height) = match mode {
        0 => {
            // Text mode - use 640x400 for 80x25 character display
            (640, 400)
        }
        1 => (320, 200),  // CGA 4-color
        2 => (640, 200),  // CGA 2-color
        7 => (320, 200),  // EGA 16-color
        8 => (640, 200),  // EGA 16-color
        9 => (640, 350),  // EGA 16-color
        10 => (640, 350), // EGA 2-color mono
        11 => (640, 480), // VGA 2-color
        12 => (640, 480), // VGA 16-color
        13 => (320, 200), // VGA 256-color (popular for retro games)
        // QB64 extended modes (custom resolutions)
        // Negative modes in QB64 represent custom dimensions, but we don't support that here
        // For now, default to 640x480 for unknown modes
        _ => {
            if mode > 13 {
                // Treat large positive numbers as width hints
                // QB64 uses SCREEN _NEWIMAGE(w, h, 32) for custom sizes
                (640, 480)
            } else {
                // Invalid mode
                return 1;
            }
        }
    };

    match crate::graphics::init_graphics(width, height) {
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

/// Plot a pixel at the specified coordinates with optional STEP mode.
///
/// # Arguments
/// - `x`: X coordinate (or offset if step is non-zero)
/// - `y`: Y coordinate (or offset if step is non-zero)
/// - `color`: Pixel color (ARGB format)
/// - `step`: If non-zero, coordinates are relative to last graphics point
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_pset_step(x: i32, y: i32, color: u32, step: c_int) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.pset_step(x, y, color, step != 0) {
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

/// Draw a line with optional STEP mode for endpoints.
///
/// # Arguments
/// - `x1`, `y1`: Start coordinates (or offset if step1 is non-zero)
/// - `x2`, `y2`: End coordinates (or offset if step2 is non-zero)
/// - `color`: Line color (ARGB format)
/// - `step1`: If non-zero, start coordinates are relative to last graphics point
/// - `step2`: If non-zero, end coordinates are relative to resolved start
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_line_step(
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    color: u32,
    step1: c_int,
    step2: c_int,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.line_step(x1, y1, x2, y2, color, false, step1 != 0, step2 != 0) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Draw a box with optional STEP mode for corners.
///
/// # Arguments
/// - `x1`, `y1`: First corner coordinates (or offset if step1 is non-zero)
/// - `x2`, `y2`: Opposite corner coordinates (or offset if step2 is non-zero)
/// - `color`: Box color (ARGB format)
/// - `filled`: 0 for outline, non-zero for filled
/// - `step1`: If non-zero, first corner is relative to last graphics point
/// - `step2`: If non-zero, second corner is relative to resolved first corner
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_box_step(
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    color: u32,
    filled: c_int,
    step1: c_int,
    step2: c_int,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.line_step(x1, y1, x2, y2, color, filled != 0, step1 != 0, step2 != 0) {
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

/// Draw a circle with optional STEP mode.
///
/// # Arguments
/// - `x`: X coordinate of center (or offset if step is non-zero)
/// - `y`: Y coordinate of center (or offset if step is non-zero)
/// - `radius`: Circle radius
/// - `color`: Circle color (ARGB format)
/// - `filled`: Non-zero to fill the circle
/// - `step`: If non-zero, coordinates are relative to last graphics point
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_circle_step(
    x: i32,
    y: i32,
    radius: i32,
    color: u32,
    filled: c_int,
    step: c_int,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.circle_step(x, y, radius, color, filled != 0, step != 0) {
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

/// Flood fill with optional STEP mode.
///
/// # Arguments
/// - `x`, `y`: Starting point (or offset if step is non-zero)
/// - `color`: Fill color (ARGB format)
/// - `boundary_color`: Boundary color (-1 for match mode, else boundary mode)
/// - `step`: If non-zero, coordinates are relative to last graphics point
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
#[no_mangle]
pub extern "C" fn qb_gfx_paint_step(
    x: i32,
    y: i32,
    color: u32,
    boundary_color: i32,
    step: c_int,
) -> c_int {
    let boundary = if boundary_color < 0 {
        None
    } else {
        Some(boundary_color as u32)
    };

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.paint_step(x, y, color, boundary, step != 0) {
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

/// Copy one video page to another.
///
/// Used for double-buffering and animation. Copies all pixels from
/// the source page to the destination page.
///
/// # Arguments
/// - `src`: Source page number
/// - `dst`: Destination page number
///
/// # Returns
/// - `0` on success, non-zero on error
#[no_mangle]
pub extern "C" fn qb_gfx_pcopy(src: i32, dst: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.pcopy(src, dst) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Map coordinates between world and screen coordinate systems.
///
/// PMAP converts coordinates based on the WINDOW statement settings.
///
/// # Arguments
/// - `coord`: The coordinate value to convert
/// - `func_code`: Conversion function:
///   - 0: World X to Screen X
///   - 1: World Y to Screen Y
///   - 2: Screen X to World X
///   - 3: Screen Y to World Y
///
/// # Returns
/// - The converted coordinate value
#[no_mangle]
pub extern "C" fn qb_gfx_pmap(coord: f64, func_code: i32) -> f64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.pmap(coord, func_code)
        } else {
            // No backend initialized - return coordinate unchanged
            coord
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

// ============================================================================
// Font Functions (TrueType Support)
// ============================================================================

/// _LOADFONT - Load a TrueType font from a file.
///
/// Returns a font handle on success, or 0 on failure.
///
/// # Safety
/// - `path` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_loadfont(path: *const c_char, size: i64) -> i64 {
    if path.is_null() {
        return 0;
    }

    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    #[cfg(feature = "graphics-sdl2")]
    {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            return backend.load_font(path_str, size.max(1).min(65535) as u16);
        }
    }

    0
}

/// _FONT - Set the current font for text rendering.
///
/// Returns the previous font handle.
#[no_mangle]
pub extern "C" fn qb_font(handle: i64) -> i64 {
    #[cfg(feature = "graphics-sdl2")]
    {
        unsafe {
            if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
                return backend.set_font(handle);
            }
        }
    }

    0
}

/// _FREEFONT - Free a loaded font.
#[no_mangle]
pub extern "C" fn qb_freefont(handle: i64) -> i64 {
    #[cfg(feature = "graphics-sdl2")]
    {
        unsafe {
            if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
                backend.free_font(handle);
                return 0;
            }
        }
    }

    0
}

/// _FONTHEIGHT - Get the height of the current font.
#[no_mangle]
pub extern "C" fn qb_fontheight() -> i64 {
    #[cfg(feature = "graphics-sdl2")]
    {
        unsafe {
            if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
                return backend.get_font_height() as i64;
            }
        }
    }

    16 // Default height
}

/// _FONTWIDTH - Get the width of the current font.
#[no_mangle]
pub extern "C" fn qb_fontwidth() -> i64 {
    #[cfg(feature = "graphics-sdl2")]
    {
        unsafe {
            if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
                return backend.get_font_width() as i64;
            }
        }
    }

    8 // Default width
}

/// _PRINTWIDTH - Get the pixel width of a string with the current font.
///
/// # Safety
/// - `text` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_printwidth(text: *const c_char) -> i64 {
    if text.is_null() {
        return 0;
    }

    let text_str = match CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    #[cfg(feature = "graphics-sdl2")]
    {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            return backend.get_print_width(text_str);
        }
    }

    // Fallback: 8 pixels per character
    (text_str.len() as i64) * 8
}

// ============================================================================
// GET/PUT Graphics Array Operations
// ============================================================================

/// PUT action constants matching the C defines in runtime.rs
const QB_PUT_XOR: c_int = 0;
const QB_PUT_PSET: c_int = 1;
const QB_PUT_PRESET: c_int = 2;
const QB_PUT_AND: c_int = 3;
const QB_PUT_OR: c_int = 4;

/// GET - Capture a screen region to an array.
///
/// Array format:
/// - Bytes 0-1: Width (16-bit little-endian)
/// - Bytes 2-3: Height (16-bit little-endian)
/// - Bytes 4+: Pixel data (32-bit ARGB per pixel, row-major order)
///
/// # Arguments
/// - `x1`, `y1`: Top-left corner of region
/// - `x2`, `y2`: Bottom-right corner of region
/// - `arr`: Pointer to destination array (must be large enough)
///
/// # Safety
/// - `arr` must point to a valid, writable memory region large enough
///   to hold 4 bytes header + (width * height * 4) bytes of pixel data
#[no_mangle]
pub extern "C" fn qb_gfx_get(x1: i32, y1: i32, x2: i32, y2: i32, arr: *mut u8) -> c_int {
    if arr.is_null() {
        return 1;
    }

    // Normalize coordinates (ensure x1 <= x2, y1 <= y2)
    let (left, right) = if x1 <= x2 { (x1, x2) } else { (x2, x1) };
    let (top, bottom) = if y1 <= y2 { (y1, y2) } else { (y2, y1) };

    let width = (right - left + 1) as u16;
    let height = (bottom - top + 1) as u16;

    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            // Write header: width and height as 16-bit values
            let header = arr as *mut u16;
            *header = width;
            *header.add(1) = height;

            // Write pixel data
            let pixels = arr.add(4) as *mut u32;
            let mut idx = 0usize;

            for py in top..=bottom {
                for px in left..=right {
                    let color = backend.point(px, py).unwrap_or(0);
                    *pixels.add(idx) = color;
                    idx += 1;
                }
            }

            0 // Success
        } else {
            1 // Not initialized
        }
    }
}

/// GET with STEP variant - coordinates may be relative.
///
/// For GET, the STEP applies to the second coordinate pair,
/// making (w, h) relative offsets from (x1, y1).
#[no_mangle]
pub extern "C" fn qb_gfx_get_step(x1: i32, y1: i32, w: i32, h: i32, arr: *mut u8) -> c_int {
    // STEP variant: w and h are offsets from x1, y1
    qb_gfx_get(x1, y1, x1 + w, y1 + h, arr)
}

/// PUT - Draw array contents to screen.
///
/// Reads image data from array and draws it to screen at (x, y),
/// applying the specified action mode.
///
/// # Arguments
/// - `x`, `y`: Top-left corner of destination
/// - `arr`: Pointer to source array (format from GET)
/// - `action`: Drawing mode (XOR=0, PSET=1, PRESET=2, AND=3, OR=4)
/// - `clip`: If non-zero, clip to screen boundaries
/// - `trans_color`: Transparent color (-1 for none, otherwise skip pixels of this color)
///
/// # Safety
/// - `arr` must point to a valid array with proper header and pixel data
#[no_mangle]
pub extern "C" fn qb_gfx_put(
    x: i32,
    y: i32,
    arr: *const u8,
    action: c_int,
    clip: c_int,
    trans_color: i32,
) -> c_int {
    if arr.is_null() {
        return 1;
    }

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            // Read header
            let header = arr as *const u16;
            let width = *header as i32;
            let height = *header.add(1) as i32;

            if width <= 0 || height <= 0 {
                return 1; // Invalid dimensions
            }

            // Get screen size for clipping
            let (screen_w, screen_h) = backend.get_screen_size();
            let screen_w = screen_w as i32;
            let screen_h = screen_h as i32;

            // Read pixel data
            let pixels = arr.add(4) as *const u32;

            for py in 0..height {
                let dest_y = y + py;

                // Clip vertically
                if clip != 0 && (dest_y < 0 || dest_y >= screen_h) {
                    continue;
                }

                for px in 0..width {
                    let dest_x = x + px;

                    // Clip horizontally
                    if clip != 0 && (dest_x < 0 || dest_x >= screen_w) {
                        continue;
                    }

                    let src_idx = (py * width + px) as usize;
                    let src_color = *pixels.add(src_idx);

                    // Check transparent color
                    if trans_color >= 0 && src_color == trans_color as u32 {
                        continue;
                    }

                    // Apply action mode
                    let final_color = match action {
                        QB_PUT_PSET => src_color,
                        QB_PUT_PRESET => !src_color | 0xFF000000, // Invert RGB, keep alpha opaque
                        QB_PUT_XOR => {
                            let dest_color = backend.point(dest_x, dest_y).unwrap_or(0);
                            (src_color ^ dest_color) | 0xFF000000 // XOR RGB, keep alpha opaque
                        }
                        QB_PUT_AND => {
                            let dest_color = backend.point(dest_x, dest_y).unwrap_or(0);
                            (src_color & dest_color) | 0xFF000000 // AND RGB, keep alpha opaque
                        }
                        QB_PUT_OR => {
                            let dest_color = backend.point(dest_x, dest_y).unwrap_or(0);
                            (src_color | dest_color) | 0xFF000000 // OR RGB, keep alpha opaque
                        }
                        _ => src_color, // Default to PSET for unknown actions
                    };

                    let _ = backend.pset(dest_x, dest_y, final_color);
                }
            }

            0 // Success
        } else {
            1 // Not initialized
        }
    }
}

/// PUT with STEP variant - position is relative to last graphics point.
///
/// Note: In a full implementation, this would track the last graphics
/// position. For now, it behaves the same as the non-STEP variant.
#[no_mangle]
pub extern "C" fn qb_gfx_put_step(
    x: i32,
    y: i32,
    arr: *const u8,
    action: c_int,
    clip: c_int,
    trans_color: i32,
) -> c_int {
    // TODO: Track last graphics position for proper STEP behavior
    // For now, just pass through to the non-STEP variant
    qb_gfx_put(x, y, arr, action, clip, trans_color)
}

// ============================================================================
// Window Control Functions
// ============================================================================

/// _FULLSCREEN - Set fullscreen mode.
///
/// # Arguments
/// - `mode`: 0 = windowed, 1 = fullscreen, 2 = fullscreen desktop
///
/// # Returns
/// Previous fullscreen mode
#[no_mangle]
pub extern "C" fn qb_fullscreen(mode: i32) -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.set_fullscreen(mode)
        } else {
            0
        }
    }
}

/// _FULLSCREEN (function) - Get current fullscreen mode.
///
/// # Returns
/// - 0: Windowed
/// - 1: Fullscreen
/// - 2: Fullscreen desktop
#[no_mangle]
pub extern "C" fn qb_fullscreen_get() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_fullscreen()
        } else {
            0
        }
    }
}

/// _SCREENMOVE - Move the window to the specified position.
///
/// # Arguments
/// - `x`: X position in screen coordinates
/// - `y`: Y position in screen coordinates
#[no_mangle]
pub extern "C" fn qb_screenmove(x: i32, y: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.screen_move(x, y);
        }
    }
}

/// _SCREENSHOW - Show the window (make visible).
#[no_mangle]
pub extern "C" fn qb_screenshow() {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.screen_show();
        }
    }
}

/// _SCREENHIDE - Hide the window (make invisible).
#[no_mangle]
pub extern "C" fn qb_screenhide() {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.screen_hide();
        }
    }
}

// ============================================================================
// Windows-Only Desktop Functions
// ============================================================================

/// _WINDOWHANDLE - Get the native window handle.
///
/// # Returns
/// - On Windows: HWND as i64
/// - On other platforms: 0
#[no_mangle]
pub extern "C" fn qb_windowhandle() -> i64 {
    #[cfg(target_os = "windows")]
    {
        use winapi::um::winuser::GetActiveWindow;
        unsafe { GetActiveWindow() as i64 }
    }
    #[cfg(not(target_os = "windows"))]
    {
        0
    }
}

/// _SCREENCLICK - Simulate a mouse click on the desktop.
///
/// # Arguments
/// - `x`: X coordinate
/// - `y`: Y coordinate
/// - `button`: Mouse button (1 = left, 2 = right)
///
/// Windows only - no-op on other platforms.
#[no_mangle]
pub extern "C" fn qb_screenclick(x: i32, y: i32, button: i32) {
    #[cfg(target_os = "windows")]
    {
        use std::mem::zeroed;
        use winapi::shared::windef::RECT;
        use winapi::um::winuser::{
            GetDesktopWindow, GetWindowRect, SendInput, INPUT, INPUT_MOUSE, MOUSEEVENTF_ABSOLUTE,
            MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_LEFTUP, MOUSEEVENTF_MOVE, MOUSEEVENTF_RIGHTDOWN,
            MOUSEEVENTF_RIGHTUP,
        };

        unsafe {
            let hwnd = GetDesktopWindow();
            let mut rect: RECT = zeroed();
            GetWindowRect(hwnd, &mut rect);

            let fx = 65535.0 / (rect.right - rect.left) as f64;
            let fy = 65535.0 / (rect.bottom - rect.top) as f64;

            let mut input: INPUT = zeroed();
            input.type_ = INPUT_MOUSE;
            let mi = input.u.mi_mut();
            mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE;
            mi.dx = (x as f64 * fx) as i32;
            mi.dy = (y as f64 * fy) as i32;
            SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);

            // Button down
            mi.dwFlags = if button == 2 {
                MOUSEEVENTF_RIGHTDOWN
            } else {
                MOUSEEVENTF_LEFTDOWN
            };
            SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);

            // Button up
            mi.dwFlags = if button == 2 {
                MOUSEEVENTF_RIGHTUP
            } else {
                MOUSEEVENTF_LEFTUP
            };
            SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);
        }
    }
    #[cfg(not(target_os = "windows"))]
    {
        let _ = (x, y, button);
    }
}

/// _SCREENPRINT - Simulate keyboard input to the focused window.
///
/// # Arguments
/// - `text`: Pointer to C string to type
///
/// Windows only - no-op on other platforms.
#[no_mangle]
pub unsafe extern "C" fn qb_screenprint(text: *const c_char) {
    if text.is_null() {
        return;
    }

    #[cfg(target_os = "windows")]
    {
        use std::mem::zeroed;
        use winapi::um::winuser::{
            MapVirtualKeyA, SendInput, VkKeyScanA, INPUT, INPUT_KEYBOARD, KEYEVENTF_KEYUP,
            MAPVK_VK_TO_VSC, VK_SHIFT,
        };

        let c_str = CStr::from_ptr(text);
        if let Ok(s) = c_str.to_str() {
            for c in s.chars() {
                if !c.is_ascii() {
                    continue;
                }
                let vk = VkKeyScanA(c as i8);
                if vk == -1 {
                    continue;
                }
                let scancode = MapVirtualKeyA((vk & 0xFF) as u32, MAPVK_VK_TO_VSC) as u16;
                let shift = ((vk >> 8) & 1) != 0;

                // Shift down if needed
                if shift {
                    let mut input: INPUT = zeroed();
                    input.type_ = INPUT_KEYBOARD;
                    let ki = input.u.ki_mut();
                    ki.wVk = VK_SHIFT as u16;
                    SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);
                }

                // Key down
                let mut input: INPUT = zeroed();
                input.type_ = INPUT_KEYBOARD;
                let ki = input.u.ki_mut();
                ki.wVk = (vk & 0xFF) as u16;
                ki.wScan = scancode;
                SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);

                // Key up
                let ki = input.u.ki_mut();
                ki.dwFlags = KEYEVENTF_KEYUP;
                SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);

                // Shift up if needed
                if shift {
                    let mut input: INPUT = zeroed();
                    input.type_ = INPUT_KEYBOARD;
                    let ki = input.u.ki_mut();
                    ki.wVk = VK_SHIFT as u16;
                    ki.dwFlags = KEYEVENTF_KEYUP;
                    SendInput(1, &mut input, std::mem::size_of::<INPUT>() as i32);
                }
            }
        }
    }
    #[cfg(not(target_os = "windows"))]
    {
        let _ = text;
    }
}

/// _SCREENIMAGE - Capture a screenshot of the desktop.
///
/// # Arguments
/// - `x1, y1, x2, y2`: Rectangle to capture (all 0 = full screen)
///
/// # Returns
/// - Image handle on success
/// - -1 on failure or non-Windows platforms
///
/// Windows only - returns -1 on other platforms.
#[no_mangle]
pub extern "C" fn qb_screenimage(x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
    #[cfg(target_os = "windows")]
    {
        use std::mem::zeroed;
        use winapi::shared::windef::RECT;
        use winapi::um::wingdi::{
            BitBlt, CreateCompatibleBitmap, CreateCompatibleDC, DeleteDC, DeleteObject,
            SelectObject, SRCCOPY,
        };
        use winapi::um::winuser::{GetDC, GetDesktopWindow, GetWindowRect, ReleaseDC};

        unsafe {
            let hwnd = GetDesktopWindow();
            let mut rect: RECT = zeroed();
            GetWindowRect(hwnd, &mut rect);

            // If all coords are 0, capture full screen
            let (x1, y1, w, h) = if x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0 {
                (0, 0, rect.right, rect.bottom)
            } else {
                let x1 = x1.max(0);
                let y1 = y1.max(0);
                let x2 = x2.min(rect.right - 1);
                let y2 = y2.min(rect.bottom - 1);
                (x1, y1, x2 - x1 + 1, y2 - y1 + 1)
            };

            if w <= 0 || h <= 0 {
                return -1;
            }

            let hdc = GetDC(std::ptr::null_mut());
            let hdc2 = CreateCompatibleDC(hdc);
            let bitmap = CreateCompatibleBitmap(hdc, w, h);
            SelectObject(hdc2, bitmap as *mut _);
            BitBlt(hdc2, 0, 0, w, h, hdc, x1, y1, SRCCOPY);

            // Create image - for now just create an empty image
            // Full implementation would copy bitmap pixels to image buffer
            let img = qb_gfx_newimage(w, h, 32);

            DeleteObject(bitmap as *mut _);
            DeleteDC(hdc2);
            ReleaseDC(std::ptr::null_mut(), hdc);

            img
        }
    }
    #[cfg(not(target_os = "windows"))]
    {
        let _ = (x1, y1, x2, y2);
        -1
    }
}

// ============================================================================
// Alpha Blending Functions
// ============================================================================

/// _BLEND - Enable alpha blending for an image.
///
/// # Arguments
/// - `handle`: Image handle (0 = screen)
#[no_mangle]
pub extern "C" fn qb_blend(handle: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.set_blend(handle);
        }
    }
}

/// _DONTBLEND - Disable alpha blending for an image.
///
/// # Arguments
/// - `handle`: Image handle (0 = screen)
#[no_mangle]
pub extern "C" fn qb_dontblend(handle: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.set_dontblend(handle);
        }
    }
}

/// _CLEARCOLOR - Set a transparent color for an image.
///
/// # Arguments
/// - `color`: Color to make transparent (ARGB format)
/// - `handle`: Image handle (0 = screen)
#[no_mangle]
pub extern "C" fn qb_clearcolor(color: u32, handle: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.set_clearcolor(color, handle);
        }
    }
}

/// _CLEARCOLOR (no args) - Disable transparent color for an image.
///
/// # Arguments
/// - `handle`: Image handle (0 = screen)
#[no_mangle]
pub extern "C" fn qb_clearcolor_none(handle: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.clear_clearcolor(handle);
        }
    }
}

/// _CLEARCOLOR (function) - Get the current clear color for an image.
///
/// # Arguments
/// - `handle`: Image handle (0 = screen)
///
/// # Returns
/// The clear color, or -1 if no clear color is set
#[no_mangle]
pub extern "C" fn qb_clearcolor_get(handle: i32) -> i64 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_clearcolor(handle)
        } else {
            -1
        }
    }
}

// ============================================================================
// Palette Operations
// ============================================================================

/// _COPYPALETTE - Copy palette from one image to another.
///
/// # Arguments
/// - `src_handle`: Source image handle (0 = screen)
/// - `dest_handle`: Destination image handle (0 = screen)
#[no_mangle]
pub extern "C" fn qb_copypalette(src_handle: i32, dest_handle: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.copy_palette(src_handle, dest_handle);
        }
    }
}

// ============================================================================
// Display Layer Ordering
// ============================================================================

/// _DISPLAYORDER - Set the display layer order.
///
/// Controls which rendering layers appear on top of others.
/// Layer constants: _SOFTWARE=1, _HARDWARE=2, _HARDWARE1=3, _GLRENDER=4
///
/// # Arguments
/// - `layer1` to `layer4`: Layer constants in back-to-front order
#[no_mangle]
pub extern "C" fn qb_displayorder(layer1: i32, layer2: i32, layer3: i32, layer4: i32) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.set_display_order(layer1, layer2, layer3, layer4);
        }
    }
}

// ============================================================================
// System Interrupt Emulation (INT 0x33 mouse)
// ============================================================================

/// Internal function to emulate specific DOS interrupts.
///
/// Supports INT 0x33 (mouse) with the following subfunctions:
/// - AX=0: Check mouse installed (returns AX=0xFFFF, BX=2)
/// - AX=1: Show mouse cursor
/// - AX=2: Hide mouse cursor
/// - AX=3: Get position and buttons (BX=buttons, CX=X, DX=Y)
fn call_int(int_num: i32, regs: &mut [i16; 10]) {
    if int_num == 0x33 {
        // Mouse interrupt emulation
        let ax = regs[0];

        if ax == 0 {
            // Check if mouse installed
            regs[0] = -1_i16; // 0xFFFF = mouse installed
            regs[1] = 2; // 2 buttons
            return;
        }

        if ax == 1 {
            // Show mouse cursor
            qb_mouse_show();
            return;
        }

        if ax == 2 {
            // Hide mouse cursor
            qb_mouse_hide();
            return;
        }

        if ax == 3 {
            // Get mouse position and button status
            let mut buttons: i16 = 0;
            if qb_mouse_button(1) != 0 {
                buttons |= 1;
            }
            if qb_mouse_button(2) != 0 {
                buttons |= 2;
            }
            if qb_mouse_button(3) != 0 {
                buttons |= 4;
            }
            regs[1] = buttons; // BX = buttons
            regs[2] = qb_mouse_x() as i16; // CX = X
            regs[3] = qb_mouse_y() as i16; // DX = Y
            return;
        }

        // AX=7,8 (min/max range) and others - no-op for compatibility
    }
    // Other interrupts are no-ops
}

/// INTERRUPT statement - call system interrupt with RegType structure.
///
/// RegType is 16 bytes: AX, BX, CX, DX, BP, SI, DI, FLAGS (8 x int16)
///
/// # Safety
/// - `in_regs` and `out_regs` must point to valid 16-byte buffers
#[no_mangle]
pub unsafe extern "C" fn qb_interrupt(int_num: i32, in_regs: *const i16, out_regs: *mut i16) {
    if in_regs.is_null() || out_regs.is_null() {
        return;
    }

    // Copy input registers to working buffer (10 elements for compatibility with INTERRUPTX)
    let mut regs: [i16; 10] = [0; 10];
    for i in 0..8 {
        regs[i] = *in_regs.add(i);
    }

    // Call interrupt emulation
    call_int(int_num, &mut regs);

    // Copy result to output registers
    for i in 0..8 {
        *out_regs.add(i) = regs[i];
    }
}

/// INTERRUPTX statement - call system interrupt with RegTypeX structure.
///
/// RegTypeX is 20 bytes: AX, BX, CX, DX, BP, SI, DI, FLAGS, DS, ES (10 x int16)
///
/// # Safety
/// - `in_regs` and `out_regs` must point to valid 20-byte buffers
#[no_mangle]
pub unsafe extern "C" fn qb_interruptx(int_num: i32, in_regs: *const i16, out_regs: *mut i16) {
    if in_regs.is_null() || out_regs.is_null() {
        return;
    }

    // Copy input registers to working buffer
    let mut regs: [i16; 10] = [0; 10];
    for i in 0..10 {
        regs[i] = *in_regs.add(i);
    }

    // Call interrupt emulation
    call_int(int_num, &mut regs);

    // Copy result to output registers
    for i in 0..10 {
        *out_regs.add(i) = regs[i];
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
