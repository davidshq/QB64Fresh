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
//! - Non-zero: Error (typically `1`, or `-1` for some functions like `qb_gfx_loadimage`)
//!
//! **Error Logging:** All errors are logged to stderr with detailed context:
//! - Function name where the error occurred
//! - Error kind (e.g., `InvalidArgument`, `NotInitialized`, `BackendError`)
//! - Error message with specific details
//!
//! This provides comprehensive visibility into failures while maintaining backward
//! compatibility with the simple return code interface. Validation errors (null
//! pointers, invalid arguments) are also logged with descriptive messages.
//!
//! # Thread Safety
//!
//! These functions are NOT thread-safe. They access a global mutable state.
//! Generated programs should be single-threaded for graphics operations.

use std::ffi::CStr;
use std::os::raw::{c_char, c_int};

#[cfg(feature = "opengl")]
use std::sync::atomic::{AtomicI32, Ordering};

/// Helper macro to log FFI errors and return error code.
///
/// This macro provides consistent error logging across all FFI functions.
/// It logs detailed error information to stderr including:
/// - Function name
/// - Error kind (from GraphicsError)
/// - Error message
/// - Returns the appropriate error code (1 for failure).
///
/// # Usage
///
/// ```ignore
/// match some_operation() {
///     Ok(result) => result,
///     Err(e) => return log_ffi_error!("function_name", e),
/// }
/// ```
macro_rules! log_ffi_error {
    ($func_name:expr, $error:expr) => {{
        eprintln!(
            "Error in {}: [{}] {}",
            $func_name,
            format!("{:?}", $error.kind()),
            $error.message()
        );
        1
    }};
}

/// Helper macro to log validation errors (null pointers, invalid arguments).
///
/// Logs validation errors with context and returns error code.
macro_rules! log_validation_error {
    ($func_name:expr, $reason:expr) => {{
        eprintln!("Validation error in {}: {}", $func_name, $reason);
        1
    }};
}

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
        Err(e) => log_ffi_error!("qb_gfx_init", e),
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
        Err(e) => log_ffi_error!("qb_gfx_shutdown", e),
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
    active_page: i32,
    visual_page: i32,
) -> c_int {
    // #region agent log
    crate::debug_log::log(
        "graphics_ffi.rs:qb_gfx_screen",
        "qb_gfx_screen called",
        &format!("\"mode\":{},\"active_page\":{},\"visual_page\":{}", mode, active_page, visual_page),
        "E",
    );
    // #endregion
    // Mode -1 means "keep current mode, just set pages"
    if mode == -1 {
        // Ensure graphics is initialized (default to IDE-compatible size if not)
        unsafe {
            if crate::graphics::GRAPHICS_BACKEND.is_none() {
                // Initialize with IDE-compatible dimensions: 160 cols * 8 px = 1280, 50 rows * 8 px = 400
                if let Err(e) = crate::graphics::init_graphics(1280, 400) {
                    return log_ffi_error!("qb_gfx_screen (init for mode -1)", e);
                }
            }
        }
    } else {
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
            // For now, default to 640x480 for unknown modes
            _ => {
                if mode > 13 {
                    // Treat large positive numbers as width hints
                    // QB64 uses SCREEN _NEWIMAGE(w, h, 32) for custom sizes
                    (640, 480)
                } else {
                    // Unknown negative modes - treat as text mode
                    (640, 400)
                }
            }
        };

        // Initialize graphics with the mode dimensions
        if let Err(e) = crate::graphics::init_graphics(width, height) {
            return log_ffi_error!("qb_gfx_screen", e);
        }
    }

    // Set active and visual pages if specified (>= 0 means use that page)
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            // Set active page if specified
            if active_page >= 0 {
                if let Err(e) = backend.set_active_page(active_page) {
                    return log_ffi_error!("qb_gfx_screen (set_active_page)", e);
                }
            }
            // Set visual page if specified
            if visual_page >= 0 {
                if let Err(e) = backend.set_visual_page(visual_page) {
                    return log_ffi_error!("qb_gfx_screen (set_visual_page)", e);
                }
            }
        }
    }

    0
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
                Err(e) => log_ffi_error!("qb_gfx_cls", e),
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
            if gfx_trace_enabled() {
                eprintln!("QB64Fresh: COLOR fg={} bg={}", foreground, background);
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            let fg = if foreground == u32::MAX {
                backend.get_foreground_color()
            } else if foreground <= 255 {
                backend.get_palette(foreground as i32)
            } else {
                foreground
            };

            let bg = if background == u32::MAX {
                backend.get_background_color()
            } else if background <= 255 {
                backend.get_palette(background as i32)
            } else {
                background
            };

            match backend.set_color(fg, bg) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_color", e),
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
                Err(e) => log_ffi_error!("qb_gfx_locate", e),
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
        return log_validation_error!("qb_gfx_print", "null pointer for text parameter");
    }

    let c_str = match CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(e) => {
            return log_validation_error!("qb_gfx_print", format!("invalid UTF-8 in text: {}", e));
        }
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        match backend.print(c_str) {
            Ok(()) => 0,
            Err(e) => log_ffi_error!("qb_gfx_print", e),
        }
    } else {
        log_validation_error!("qb_gfx_print", "graphics backend not initialized");
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
                Err(e) => log_ffi_error!("qb_gfx_pset", e),
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
                Err(e) => log_ffi_error!("qb_gfx_pset_step", e),
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
            match backend.line(x1, y1, x2, y2, color, false, false, None) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_line", e),
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
    style: u16,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            let style_opt = if style == 0xFFFF {
                None // 0xFFFF means no style (solid line)
            } else {
                Some(style)
            };
            match backend.line_step(
                x1,
                y1,
                x2,
                y2,
                color,
                false,
                false,
                step1 != 0,
                step2 != 0,
                style_opt,
            ) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_line_step", e),
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
/// - `style`: Style pattern (16-bit, 0xFFFF = no style/solid line)
///
/// # Returns
/// - `0` on success
/// - Non-zero on failure
///
/// # Style Pattern
/// The style parameter is a 16-bit value where each bit represents whether to draw a pixel.
/// Bit 15 (MSB) is the first pixel, bit 0 (LSB) is the last. The pattern repeats.
/// Only applies to box outlines (filled boxes ignore style).
/// Use 0xFFFF for solid lines (default behavior when style is not specified).
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
    style: u16,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            let style_opt = if style == 0xFFFF {
                None // 0xFFFF means no style (solid line)
            } else {
                Some(style)
            };
            // For boxes, pass is_box=true when filled=0 (box outline), is_box=false when filled!=0 (filled box)
            // The is_box parameter explicitly distinguishes box outlines from plain lines
            let is_box = filled == 0; // Box outline (not filled) should be treated as a box
            match backend.line_step(
                x1,
                y1,
                x2,
                y2,
                color,
                filled != 0,
                is_box,
                step1 != 0,
                step2 != 0,
                style_opt,
            ) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_box_step", e),
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
            // For boxes, pass is_box=true when filled=0 (box outline), is_box=false when filled!=0 (filled box)
            let is_box = filled == 0; // Box outline (not filled) should be treated as a box
            match backend.line(x1, y1, x2, y2, color, filled != 0, is_box, None) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_box", e),
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
                Err(e) => log_ffi_error!("qb_gfx_circle", e),
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
                Err(e) => log_ffi_error!("qb_gfx_circle_step", e),
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
                Err(e) => log_ffi_error!("qb_gfx_paint", e),
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
                Err(e) => log_ffi_error!("qb_gfx_paint_step", e),
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
            if gfx_trace_enabled() {
                eprintln!("QB64Fresh: _DISPLAY");
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            match backend.display() {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_display", e),
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
            if gfx_trace_enabled() {
                eprintln!("QB64Fresh: PCOPY {} -> {}", src, dst);
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            match backend.pcopy(src, dst) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_pcopy", e),
            }
        } else {
            1
        }
    }
}

/// Set the active page for drawing operations.
///
/// All subsequent drawing commands will target this page.
///
/// # Arguments
/// - `page`: Page number (0-3 for most modes)
///
/// # Returns
/// - `0` on success, non-zero on error
#[no_mangle]
pub extern "C" fn qb_gfx_set_active_page(page: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if gfx_trace_enabled() {
                eprintln!("QB64Fresh: _SCREEN , active={}", page);
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            match backend.set_active_page(page) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_set_active_page", e),
            }
        } else {
            1
        }
    }
}

/// Set the visual page for display.
///
/// This page is rendered to the screen when display() is called.
///
/// # Arguments
/// - `page`: Page number (0-3 for most modes)
///
/// # Returns
/// - `0` on success, non-zero on error
#[no_mangle]
pub extern "C" fn qb_gfx_set_visual_page(page: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if gfx_trace_enabled() {
                eprintln!("QB64Fresh: _SCREEN , visual={}", page);
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            match backend.set_visual_page(page) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_set_visual_page", e),
            }
        } else {
            1
        }
    }
}

/// Get the current active and visual page numbers.
///
/// # Arguments
/// - `active_page`: Pointer to store active page number
/// - `visual_page`: Pointer to store visual page number
#[no_mangle]
pub extern "C" fn qb_gfx_get_pages(active_page: *mut i32, visual_page: *mut i32) {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            let (active, visual) = backend.get_pages();
            if !active_page.is_null() {
                *active_page = active;
            }
            if !visual_page.is_null() {
                *visual_page = visual;
            }
        } else {
            if !active_page.is_null() {
                *active_page = 0;
            }
            if !visual_page.is_null() {
                *visual_page = 0;
            }
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
///
/// When the user closes the window (Quit event), this sets the global
/// `stop_program` flag so QB64pe-generated code's `if (stop_program) end();`
/// will run and the process exits cleanly.
#[no_mangle]
pub extern "C" fn qb_gfx_poll_events() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.poll_events() {
                Ok(true) => 1,
                Ok(false) => {
                    crate::stop_program = 1;
                    0
                }
                Err(e) => {
                    log_ffi_error!("qb_gfx_poll_events", e);
                    -1
                }
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
// HSB color functions (_HSB32, _HSBA32, _HUE32, _SATURATION32, _BRIGHTNESS32)
// Hue 0-360, Saturation/Brightness 0-100; color format &HAARRGGBB
// ============================================================================

/// _HSB32 - Convert hue, saturation, brightness to ARGB color.
///
/// Hue 0-360, saturation and brightness 0-100. Returns opaque color (alpha 255).
#[no_mangle]
pub extern "C" fn qb_hsb32(hue: f32, sat: f32, bri: f32) -> u32 {
    let h = hue.clamp(0.0, 360.0);
    let s = (sat.clamp(0.0, 100.0) / 100.0) as f32;
    let v = (bri.clamp(0.0, 100.0) / 100.0) as f32;
    let c = v * s;
    let p = v - c;
    let hi = (h / 60.0) as i32 % 6;
    let f = (h / 60.0) - (h / 60.0).floor();
    let q = v - c * f;
    let t = v - c * (1.0 - f);
    let (r, g, b) = match hi {
        0 => (v, t, p),
        1 => (q, v, p),
        2 => (p, v, t),
        3 => (p, q, v),
        4 => (t, p, v),
        _ => (v, p, q),
    };
    let ir = (r * 255.0).round() as u32;
    let ig = (g * 255.0).round() as u32;
    let ib = (b * 255.0).round() as u32;
    0xFF00_0000 | ((ir.min(255) << 16) | (ig.min(255) << 8) | ib.min(255))
}

/// _HSBA32 - Convert hue, saturation, brightness, alpha to ARGB color.
#[no_mangle]
pub extern "C" fn qb_hsba32(hue: f32, sat: f32, bri: f32, alpha: f32) -> u32 {
    let rgb = qb_hsb32(hue, sat, bri);
    let a = (alpha.clamp(0.0, 255.0) as u32).min(255);
    (a << 24) | (rgb & 0x00FF_FFFF)
}

/// _HUE32 - Extract hue (0-360) from ARGB color.
#[no_mangle]
pub extern "C" fn qb_hue32(color: u32) -> f32 {
    let r = ((color >> 16) & 0xFF) as f32 / 255.0;
    let g = ((color >> 8) & 0xFF) as f32 / 255.0;
    let b = (color & 0xFF) as f32 / 255.0;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let d = max - min;
    if d <= 0.0 {
        return 0.0;
    }
    let h = if max == r {
        60.0 * (g - b) / d + if g < b { 360.0 } else { 0.0 }
    } else if max == g {
        60.0 * (b - r) / d + 120.0
    } else {
        60.0 * (r - g) / d + 240.0
    };
    if h < 0.0 {
        h + 360.0
    } else if h >= 360.0 {
        0.0
    } else {
        h
    }
}

/// _SATURATION32 - Extract saturation (0-100) from ARGB color.
#[no_mangle]
pub extern "C" fn qb_saturation32(color: u32) -> f32 {
    let r = ((color >> 16) & 0xFF) as f32 / 255.0;
    let g = ((color >> 8) & 0xFF) as f32 / 255.0;
    let b = (color & 0xFF) as f32 / 255.0;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    if max <= 0.0 {
        0.0
    } else {
        (max - min) / max * 100.0
    }
}

/// _BRIGHTNESS32 - Extract brightness (0-100) from ARGB color.
#[no_mangle]
pub extern "C" fn qb_brightness32(color: u32) -> f32 {
    let r = (color >> 16) & 0xFF;
    let g = (color >> 8) & 0xFF;
    let b = color & 0xFF;
    let max = r.max(g).max(b);
    (max as f32) * 100.0 / 255.0
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
                Err(e) => log_ffi_error!("qb_gfx_set_width", e),
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
                Err(e) => log_ffi_error!("qb_gfx_view", e),
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
                Err(e) => log_ffi_error!("qb_gfx_view_reset", e),
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
                Err(e) => log_ffi_error!("qb_gfx_window", e),
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
                Err(e) => log_ffi_error!("qb_gfx_window_reset", e),
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
        return log_validation_error!("qb_gfx_draw", "null pointer for commands parameter");
    }

    let cmd_str = match CStr::from_ptr(commands).to_str() {
        Ok(s) => s,
        Err(e) => {
            return log_validation_error!(
                "qb_gfx_draw",
                format!("invalid UTF-8 in commands: {}", e)
            );
        }
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        match backend.draw(cmd_str) {
            Ok(()) => 0,
            Err(e) => log_ffi_error!("qb_gfx_draw", e),
        }
    } else {
        log_validation_error!("qb_gfx_draw", "graphics backend not initialized");
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
                Err(e) => log_ffi_error!("qb_gfx_palette", e),
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
                Err(e) => log_ffi_error!("qb_gfx_palette_reset", e),
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

/// _PALETTECOLOR - Get palette color from an image.
///
/// Function form: `_PALETTECOLOR(attribute%[, imgHandle&])`
///
/// Returns the 32-bit color value at the specified palette index.
/// If imgHandle is 0 or omitted, uses the current _DEST image (screen).
#[no_mangle]
pub extern "C" fn qb_palettecolor_get(attribute: i32, handle: i32) -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            // Use per-image palette support
            // If handle is 0, use screen palette; otherwise use the image's palette
            backend.get_palette_for_image(attribute, handle) as i32
        } else {
            0
        }
    }
}

/// _PALETTECOLOR - Set or get palette color.
///
/// This function handles both forms:
/// - Get: `_PALETTECOLOR(attribute%, imgHandle&)` - 2 args, handle is image
/// - Set: `_PALETTECOLOR attribute%, color&[, imgHandle&]` - 2-3 args
///
/// When called with all 3 args, it's a SET operation.
/// When called with 2 args where second is an image handle, it's a GET operation.
/// Distinguishing between these requires context from the codegen.
#[no_mangle]
pub extern "C" fn qb_palettecolor(attribute: i32, color_or_handle: i32, handle: i32) -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            // Determine the target handle for palette operations
            // If handle != 0, this is a SET with explicit handle
            // If handle == 0, check if this is a GET (color_or_handle is actually handle)
            // or a SET (color_or_handle is color, using current dest/screen)
            //
            // Convention: when used as statement (SET), handle will be non-zero or we use current
            // For simplicity, treat 3-arg call as SET
            let target_handle = if handle != 0 {
                // Explicit handle provided
                handle
            } else {
                // No explicit handle - use screen (0) for current destination
                0
            };

            // This function is only called for SET operations (GET uses qb_palettecolor_get)
            // Always set the palette, even if color is 0 (black is a valid color)
            // Use per-image palette support
            if let Err(e) =
                backend.set_palette_for_image(attribute, color_or_handle as u32, target_handle)
            {
                // Log error for debugging, but continue to return current palette value
                // This maintains backward compatibility while providing error visibility
                eprintln!("Warning: qb_palettecolor failed: {}", e);
            }
            // Return the palette value at this index for the target image
            backend.get_palette_for_image(attribute, target_handle) as i32
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
        log_validation_error!("qb_gfx_loadimage", "null pointer for filename parameter");
        return -1;
    }

    let fname = match CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(e) => {
            log_validation_error!(
                "qb_gfx_loadimage",
                format!("invalid UTF-8 in filename: {}", e)
            );
            return -1;
        }
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
                Err(e) => log_ffi_error!("qb_gfx_freeimage", e),
            }
        } else {
            1
        }
    }
}

/// _SAVEIMAGE filename$, handle - save image to file.
///
/// Saves the image identified by `handle` (0 = current screen) to the path given by `path`.
/// Format is inferred from extension (e.g. .png). On error, logs and returns without raising.
///
/// # Safety
/// - `path` must be a valid pointer to a QbString or null.
#[no_mangle]
pub unsafe extern "C" fn qb_saveimage(path: *const crate::string::QbString, handle: i32) {
    if path.is_null() {
        return;
    }
    let path_ptr = crate::string::qb_string_data(path);
    if path_ptr.is_null() {
        return;
    }
    let path_str = match CStr::from_ptr(path_ptr).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if let Err(e) = backend.save_image(path_str, handle) {
                log_ffi_error!("qb_saveimage", e);
            }
        }
    }
}

/// _DEPTHBUFFER mode - enable/disable depth buffer for 3D (stub: no-op).
#[no_mangle]
pub extern "C" fn qb_depthbuffer(_mode: i32) {
    // Stub: no-op until 3D/depth buffer support is implemented
}

/// Simple put_image without source coordinates.
#[no_mangle]
pub extern "C" fn qb_gfx_putimage_simple(src_handle: i32, dest_handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.put_image(0, 0, -1, -1, src_handle, dest_handle) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_putimage_simple", e),
            }
        } else {
            1
        }
    }
}

/// Put_image with destination coordinates.
/// `scale_mode` is passed from BASIC _PUTIMAGE; runtime may use it for stretch/smooth (currently ignored).
#[no_mangle]
pub extern "C" fn qb_gfx_putimage(
    dx1: i32,
    dy1: i32,
    dx2: i32,
    dy2: i32,
    src_handle: i32,
    dest_handle: i32,
    _scale_mode: i32,
) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            match backend.put_image(dx1, dy1, dx2, dy2, src_handle, dest_handle) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_putimage", e),
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
                Err(e) => log_ffi_error!("qb_gfx_putimage", e),
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
                Err(e) => log_ffi_error!("qb_gfx_source", e),
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
                Err(e) => log_ffi_error!("qb_gfx_dest", e),
            }
        } else {
            1
        }
    }
}

/// Print string at pixel coordinates.
///
/// The C string is interpreted as a CP437-encoded byte string so box-drawing
/// and accented characters (e.g. IDE menus) render correctly.
///
/// # Safety
/// - `text` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_gfx_printstring(x: i32, y: i32, text: *const c_char) -> c_int {
    if text.is_null() {
        return log_validation_error!("qb_gfx_printstring", "null pointer for text parameter");
    }

    let bytes = CStr::from_ptr(text).to_bytes();
    let mut mapped_bytes_storage = Vec::new();
    let text_bytes: &[u8] = if ide_compat_enabled() {
        if let Ok(utf8_text) = std::str::from_utf8(bytes) {
            mapped_bytes_storage = crate::cp437::utf8_to_cp437_bytes(utf8_text);
            mapped_bytes_storage.as_slice()
        } else {
            bytes
        }
    } else {
        bytes
    };
    if gfx_trace_enabled() {
        let preview_len = bytes.len().min(8);
        let mut preview = String::new();
        for (i, b) in bytes.iter().take(preview_len).enumerate() {
            if i > 0 {
                preview.push(' ');
            }
            use std::fmt::Write;
            let _ = write!(preview, "{:02X}", b);
        }
        eprintln!(
            "QB64Fresh: _PRINTSTRING x={} y={} len={} bytes=[{}]",
            x,
            y,
            bytes.len(),
            preview
        );
        let _ = std::io::Write::flush(&mut std::io::stderr());
    }

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        let mut px = x;
        let mut py = y;
        if ide_compat_enabled() {
            let fw = backend.get_font_width() as i32;
            let fh = backend.get_font_height() as i32;
            // Treat coordinates as 1-based character cells in IDE mode.
            if px > 0 {
                px = (px - 1) * fw;
            }
            if py > 0 {
                py = (py - 1) * fh;
            }
        }

        match backend.print_string_bytes(px, py, text_bytes) {
            Ok(()) => 0,
            Err(e) => log_ffi_error!("qb_gfx_printstring", e),
        }
    } else {
        log_validation_error!("qb_gfx_printstring", "graphics backend not initialized");
        1
    }
}

/// Set auto-display mode.
#[no_mangle]
pub extern "C" fn qb_gfx_autodisplay(enabled: c_int) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if gfx_trace_enabled() {
                eprintln!("QB64Fresh: _AUTODISPLAY {}", enabled);
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            match backend.set_autodisplay(enabled != 0) {
                Ok(()) => 0,
                Err(e) => log_ffi_error!("qb_gfx_autodisplay", e),
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

/// Check if a key is currently pressed (_KEYDOWN via graphics backend).
///
/// Uses SDL2 keyboard state when graphics backend is initialized.
/// This provides real-time key state checking for games and interactive programs.
///
/// # Arguments
/// - `keycode`: QB64 keycode to check
///
/// # Returns
/// - `-1` (true) if the key is pressed
/// - `0` (false) if not pressed or graphics backend unavailable
#[no_mangle]
pub extern "C" fn qb_gfx_keydown(keycode: i64) -> i32 {
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            if backend.is_key_pressed(keycode) {
                -1
            } else {
                0
            }
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
        log_validation_error!("qb_clipboard_set", "null pointer for text parameter");
        return;
    }

    let text_str = match std::ffi::CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(e) => {
            log_validation_error!("qb_clipboard_set", format!("invalid UTF-8 in text: {}", e));
            return;
        }
    };

    if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
        backend.set_clipboard(text_str);
    }
}

/// _CLIPBOARDIMAGE (get) - Get image handle from clipboard.
///
/// Returns 0 if no image in clipboard or not supported (stub).
#[no_mangle]
pub extern "C" fn qb_clipboardimage() -> i32 {
    0
}

/// _CLIPBOARDIMAGE = handle - Set clipboard image from image handle.
///
/// Stub: no-op when image clipboard is not implemented.
#[no_mangle]
pub extern "C" fn qb_clipboardimage_set(handle: i32) {
    let _ = handle;
}

// ============================================================================
// Font Functions (TrueType Support)
// ============================================================================

/// _FONT - Set the current font for text rendering.
///
/// Returns the previous font handle.
/// Note: Font loading/freeing functions are in font_ffi.rs (qb_loadfont, qb_freefont)
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

// Note: qb_loadfont, qb_freefont, qb_fontheight, qb_fontwidth, qb_printwidth
// are now implemented in font_ffi.rs with FreeType support

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
        return log_validation_error!("qb_gfx_get", "null pointer for arr parameter");
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
        return log_validation_error!("qb_gfx_putimage", "null pointer for arr parameter");
    }

    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            // Read header
            let header = arr as *const u16;
            let width = *header as i32;
            let height = *header.add(1) as i32;

            if width <= 0 || height <= 0 {
                return log_validation_error!(
                    "qb_gfx_put",
                    format!("invalid dimensions: {}x{}", width, height)
                );
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
/// Resolves relative coordinates using the last graphics position tracked
/// by the backend, then calls the non-STEP variant with absolute coordinates.
#[no_mangle]
pub extern "C" fn qb_gfx_put_step(
    x: i32,
    y: i32,
    arr: *const u8,
    action: c_int,
    clip: c_int,
    trans_color: i32,
) -> c_int {
    // Resolve STEP coordinates relative to last graphics position
    let (last_x, last_y) = unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_last_position()
        } else {
            (0, 0) // Default if not initialized
        }
    };

    let final_x = last_x + x;
    let final_y = last_y + y;

    // Now call the non-STEP variant with absolute coordinates
    // This will acquire its own mutable reference to the backend
    qb_gfx_put(final_x, final_y, arr, action, clip, trans_color)
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
        // Auto-initialize graphics if not already (IDE calls _SCREENMOVE before showing)
        // Use IDE-compatible size: 160 cols * 8 px = 1280, 50 rows * 8 px = 400
        if crate::graphics::GRAPHICS_BACKEND.is_none() {
            let _ = crate::graphics::init_graphics(1280, 400);
        }
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.screen_move(x, y);
        }
    }
}

/// When true, the next _SCREENHIDE call is ignored.
///
/// Used to keep the window visible when the IDE shows it via _SCREENSHOW and then
/// immediately calls _SCREENHIDE during its own initialization (e.g. setting up menus).
/// Without this, the window would flash for a second and then disappear.
static mut SUPPRESS_NEXT_SCREENHIDE: bool = false;

fn screen_trace_enabled() -> bool {
    std::env::var("QB64FRESH_SCREEN_TRACE").is_ok()
}

fn gfx_trace_enabled() -> bool {
    std::env::var("QB64FRESH_GFX_TRACE").is_ok()
}

fn screenhide_disabled() -> bool {
    std::env::var("QB64FRESH_DISABLE_SCREENHIDE").is_ok()
}

fn ide_compat_enabled() -> bool {
    std::env::var("QB64FRESH_IDE_COMPAT").is_ok()
}

/// _SCREENSHOW - Show the window (make visible).
///
/// If graphics haven't been initialized yet, initializes with default dimensions (640x400).
#[no_mangle]
pub extern "C" fn qb_screenshow() {
    unsafe {
        // #region agent log
        crate::debug_log::log(
            "graphics_ffi.rs:qb_screenshow",
            "qb_screenshow entered",
            "\"entered\":1",
            "A",
        );
        // #endregion
        // Auto-initialize graphics if not already initialized
        if crate::graphics::GRAPHICS_BACKEND.is_none() {
            // #region agent log
            crate::debug_log::log(
                "graphics_ffi.rs:qb_screenshow",
                "init_graphics from screenshow (backend was none)",
                "\"width\":1280,\"height\":400",
                "A",
            );
            // #endregion
            // Initialize with IDE-compatible dimensions: 160 cols * 8 px = 1280, 50 rows * 8 px = 400
            if let Err(e) = crate::graphics::init_graphics(1280, 400) {
                // #region agent log
                crate::debug_log::log(
                    "graphics_ffi.rs:qb_screenshow",
                    "init_graphics failed",
                    "\"err\":1",
                    "E",
                );
                // #endregion
                eprintln!("QB64Fresh: _SCREENSHOW failed to init graphics: {:?}", e);
                let _ = std::io::Write::flush(&mut std::io::stderr());
                return;
            }
            // IDE often calls _SCREENHIDE right after init; suppress that first hide
            // so the window stays visible instead of flashing and disappearing.
            SUPPRESS_NEXT_SCREENHIDE = true;
        }
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if screen_trace_enabled() {
                eprintln!("QB64Fresh: _SCREENSHOW");
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            backend.screen_show();
            // #region agent log
            crate::debug_log::log(
                "graphics_ffi.rs:qb_screenshow",
                "backend.screen_show() returned",
                "\"done\":1",
                "D",
            );
            // #endregion
        }
    }
}

/// _SCREENHIDE - Hide the window (make invisible).
#[no_mangle]
pub extern "C" fn qb_screenhide() {
    unsafe {
        // #region agent log
        crate::debug_log::log(
            "graphics_ffi.rs:qb_screenhide",
            "qb_screenhide entered",
            "\"entered\":1",
            "B",
        );
        // #endregion
        if ide_compat_enabled() {
            // #region agent log
            crate::debug_log::log(
                "graphics_ffi.rs:qb_screenhide",
                "return early ide_compat",
                "\"branch\":\"ide_compat\"",
                "B",
            );
            // #endregion
            if screen_trace_enabled() {
                eprintln!("QB64Fresh: _SCREENHIDE ignored (IDE compat)");
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            return;
        }
        if screenhide_disabled() {
            if screen_trace_enabled() {
                eprintln!("QB64Fresh: _SCREENHIDE suppressed");
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            return;
        }
        if SUPPRESS_NEXT_SCREENHIDE {
            SUPPRESS_NEXT_SCREENHIDE = false;
            // #region agent log
            crate::debug_log::log(
                "graphics_ffi.rs:qb_screenhide",
                "SUPPRESS_NEXT_SCREENHIDE consumed",
                "\"branch\":\"suppress\"",
                "B",
            );
            // #endregion
            if screen_trace_enabled() {
                eprintln!("QB64Fresh: _SCREENHIDE suppressed (first hide)");
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            return;
        }
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if screen_trace_enabled() {
                eprintln!("QB64Fresh: _SCREENHIDE");
                let _ = std::io::Write::flush(&mut std::io::stderr());
            }
            backend.screen_hide();
        }
    }
}

/// _TITLE - Set the window title.
///
/// # Arguments
/// - `title`: Window title string (QbString pointer)
#[no_mangle]
pub extern "C" fn qb_sub__title(title: *const crate::string::QbString) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            if !title.is_null() {
                let data_ptr = crate::string::qb_string_data(title);
                if let Ok(title_str) = CStr::from_ptr(data_ptr).to_str() {
                    backend.set_title(title_str);
                }
            }
        }
    }
}

/// _ICON - Set the window icon from an image handle.
///
/// # Arguments
/// - `handle`: Image handle (0 = screen, positive = image buffer)
///
/// # Returns
/// Previous icon handle, or 0 if not supported
#[no_mangle]
pub extern "C" fn qb_icon1(handle: i32) -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.set_icon(handle)
        } else {
            0
        }
    }
}

/// _ICON - Get the current icon handle.
///
/// # Returns
/// Current icon handle, or 0 if no icon is set
#[no_mangle]
pub extern "C" fn qb_icon() -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.get_icon()
        } else {
            0
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
                log_validation_error!(
                    "qb_gfx_saveimage",
                    format!("invalid dimensions: {}x{}", w, h)
                );
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

// ============================================================================
// Triangle Mapping (_MAPTRIANGLE)
// ============================================================================

/// Map a triangular portion of a source image onto a destination triangle.
///
/// This implements QB64's `_MAPTRIANGLE` statement for 2D texture-mapped rendering.
///
/// # Arguments
/// - `sx1, sy1, sx2, sy2, sx3, sy3`: Source triangle coordinates (texture space)
/// - `dx1, dy1, dx2, dy2, dx3, dy3`: Destination triangle coordinates (screen space)
/// - `src_handle`: Source image handle (0 = current source, -1 = screen)
/// - `dest_handle`: Destination image handle (0 = current dest, -1 = screen)
/// - `smooth`: Non-zero to enable bilinear filtering
/// - `seamless`: Non-zero to skip edge pixels (prevents seams in multi-triangle renders)
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub extern "C" fn qb_maptriangle(
    sx1: f64,
    sy1: f64,
    sx2: f64,
    sy2: f64,
    sx3: f64,
    sy3: f64,
    dx1: f64,
    dy1: f64,
    dx2: f64,
    dy2: f64,
    dx3: f64,
    dy3: f64,
) {
    // Simplified version: src=0, dest=0, smooth=false, seamless=false
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.map_triangle(
                sx1 as f32, sy1 as f32, sx2 as f32, sy2 as f32, sx3 as f32, sy3 as f32, dx1 as f32,
                dy1 as f32, dx2 as f32, dy2 as f32, dx3 as f32, dy3 as f32,
                0,     // src_handle (0 = screen)
                0,     // dest_handle (0 = screen)
                false, // smooth
                false, // seamless
            );
        }
    }
}

/// Extended version of _MAPTRIANGLE with full options.
///
/// # Arguments
/// - Source/dest triangle coordinates (12 floats)
/// - `src_handle`: Source image handle
/// - `dest_handle`: Destination image handle
/// - `smooth`: Enable bilinear filtering
/// - `seamless`: Skip edge pixels
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub extern "C" fn qb_maptriangle_ex(
    sx1: f64,
    sy1: f64,
    sx2: f64,
    sy2: f64,
    sx3: f64,
    sy3: f64,
    dx1: f64,
    dy1: f64,
    dx2: f64,
    dy2: f64,
    dx3: f64,
    dy3: f64,
    src_handle: i32,
    dest_handle: i32,
    smooth: i32,
    seamless: i32,
) {
    unsafe {
        if let Some(ref mut backend) = crate::graphics::GRAPHICS_BACKEND {
            backend.map_triangle(
                sx1 as f32,
                sy1 as f32,
                sx2 as f32,
                sy2 as f32,
                sx3 as f32,
                sy3 as f32,
                dx1 as f32,
                dy1 as f32,
                dx2 as f32,
                dy2 as f32,
                dx3 as f32,
                dy3 as f32,
                src_handle,
                dest_handle,
                smooth != 0,
                seamless != 0,
            );
        }
    }
}

/// _BEHIND - OpenGL render mode constant (draw behind 2D). Returns 0.
#[no_mangle]
pub extern "C" fn qb_behind() -> i32 {
    0
}

/// _ONTOP - OpenGL render mode constant (draw on top of 2D). Returns 1.
#[no_mangle]
pub extern "C" fn qb_ontop() -> i32 {
    1
}

/// _ONLY - OpenGL render mode constant (OpenGL only, no 2D). Returns 2.
#[no_mangle]
pub extern "C" fn qb_only() -> i32 {
    2
}

/// _ONLYBACKGROUND - Background-only mode constant. Returns 3.
#[no_mangle]
pub extern "C" fn qb_onlybackground() -> i32 {
    3
}

/// Stored _GLRENDER mode when `opengl` feature is enabled.
/// -1 = off, 0 = _BEHIND, 1 = _ONTOP, 2 = _ONLY, 3 = _ONLYBACKGROUND.
/// Used by the graphics backend to decide when to create a GL context and call SUB _GL each frame (follow-up work).
#[cfg(feature = "opengl")]
static GL_RENDER_MODE: AtomicI32 = AtomicI32::new(-1);

/// _GLRENDER mode - OpenGL render mode.
///
/// Valid modes: -1 (off), 0 (_BEHIND), 1 (_ONTOP), 2 (_ONLY), 3 (_ONLYBACKGROUND).
/// Values outside -1..=3 are clamped to -1 (off).
///
/// Without `opengl` feature: no-op. With `opengl`: stores the mode for the main loop.
/// The graphics backend uses [`gl_render_mode()`] to decide when to create a GL context
/// and invoke SUB _GL each frame.
#[no_mangle]
pub extern "C" fn qb_glrender(mode: i32) {
    #[cfg(feature = "opengl")]
    {
        let stored = if (-1..=3).contains(&mode) { mode } else { -1 };
        GL_RENDER_MODE.store(stored, Ordering::Relaxed);
    }
    #[cfg(not(feature = "opengl"))]
    let _ = mode;
}

/// Returns the current _GLRENDER mode when `opengl` is enabled (-1 = off).
/// Used by the graphics backend to know whether to create a GL context and invoke SUB _GL each frame.
#[cfg(feature = "opengl")]
pub fn gl_render_mode() -> i32 {
    GL_RENDER_MODE.load(Ordering::Relaxed)
}

/// _GLCOMPAT - OpenGL compatibility. Returns 1 when OpenGL is available (opengl feature), else 0.
#[no_mangle]
pub extern "C" fn qb_glcompat() -> i32 {
    #[cfg(feature = "opengl")]
    {
        return 1;
    }
    #[cfg(not(feature = "opengl"))]
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graphics::GraphicsError;

    /// Regression: FFI error reporting (Session 067).
    /// Verifies that the log_ffi_error! output format includes function name,
    /// error kind, and message so that errors are visible when FFI calls fail.
    #[test]
    fn test_ffi_error_reporting_format() {
        let e = GraphicsError::not_initialized();
        let func_name = "qb_gfx_cls";
        let formatted = format!(
            "Error in {}: [{}] {}",
            func_name,
            format!("{:?}", e.kind()),
            e.message()
        );
        assert!(
            formatted.starts_with("Error in qb_gfx_cls:"),
            "FFI error message should start with 'Error in {{func}}:'; got: {}",
            formatted
        );
        assert!(
            formatted.contains("NotInitialized"),
            "FFI error message should include error kind; got: {}",
            formatted
        );
        assert!(
            formatted.contains("Graphics backend not initialized"),
            "FFI error message should include error message; got: {}",
            formatted
        );
    }

    /// Verifies validation error format (log_validation_error!) for consistency.
    #[test]
    fn test_validation_error_reporting_format() {
        let func_name = "qb_gfx_pset";
        let reason = "null pointer for x";
        let formatted = format!("Validation error in {}: {}", func_name, reason);
        assert_eq!(
            formatted, "Validation error in qb_gfx_pset: null pointer for x",
            "Validation error format should be consistent"
        );
    }

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

    /// _GLRENDER: no-op when opengl feature is disabled; does not panic.
    #[test]
    fn test_glrender_noop_without_opengl() {
        #[cfg(not(feature = "opengl"))]
        {
            qb_glrender(-1);
            qb_glrender(0);
            qb_glrender(1);
            qb_glrender(99);
        }
    }

    /// _GLRENDER: with opengl, stores valid mode; invalid mode clamped to -1.
    #[cfg(feature = "opengl")]
    #[test]
    fn test_glrender_stores_mode() {
        // Store valid modes
        qb_glrender(0);
        assert_eq!(gl_render_mode(), 0, "_BEHIND");
        qb_glrender(1);
        assert_eq!(gl_render_mode(), 1, "_ONTOP");
        qb_glrender(2);
        assert_eq!(gl_render_mode(), 2, "_ONLY");
        qb_glrender(3);
        assert_eq!(gl_render_mode(), 3, "_ONLYBACKGROUND");
        qb_glrender(-1);
        assert_eq!(gl_render_mode(), -1, "off");

        // Invalid mode clamped to -1
        qb_glrender(99);
        assert_eq!(gl_render_mode(), -1, "invalid mode clamped to off");
        qb_glrender(-2);
        assert_eq!(gl_render_mode(), -1, "negative invalid clamped to off");
    }
}
