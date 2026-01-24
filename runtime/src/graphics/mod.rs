//! Graphics abstraction layer for QB64Fresh.
//!
//! This module provides a trait-based graphics backend system that allows
//! multiple graphics implementations (SDL2, native APIs, WebAssembly, etc.)
//! to be plugged in without changing the compiler or generated code.
//!
//! # Architecture
//!
//! ```text
//! Compiled Program
//!      ↓
//! C FFI API (qb64_gfx_*)
//!      ↓
//! GraphicsBackend Trait
//!      ↓
//! Concrete Implementation (SDL2Backend, NativeBackend, etc.)
//! ```
//!
//! # Design Principles
//!
//! 1. **Single Responsibility**: Only handles graphics rendering, not BASIC semantics
//! 2. **Backend Agnostic**: Compiler generates C code that calls stable FFI
//! 3. **Pluggable**: New backends added without changing existing code
//! 4. **Testable**: Mock backend enables headless testing
//!
//! # Example
//!
//! ```ignore
//! use qb64fresh_rt::graphics::GraphicsBackend;
//!
//! let mut backend = SDL2Backend::new();
//! backend.initialize(320, 200)?;
//! backend.pset(100, 100, 15)?;
//! backend.display()?;
//! backend.shutdown()?;
//! ```

mod error;
pub mod font;
pub mod mock;

#[cfg(feature = "graphics-sdl2")]
pub mod sdl2;

pub use error::{GraphicsError, GraphicsErrorKind};

/// Trait for graphics rendering backends.
///
/// Each backend is responsible for implementing the same interface but using
/// different underlying graphics libraries (SDL2, Win32 GDI, Xlib, WebGL, etc.).
///
/// # Thread Safety
///
/// Implementations may or may not be thread-safe. The current design is single-threaded.
/// If future backends require thread safety, this trait can be extended.
///
/// # Error Handling
///
/// All operations return `Result<T, GraphicsError>`. Backends should be defensive
/// and fail gracefully rather than panicking.
pub trait GraphicsBackend {
    // ============================================================================
    // Initialization & Cleanup
    // ============================================================================

    /// Initialize the graphics system with the given screen dimensions.
    ///
    /// This must be called before any drawing operations.
    ///
    /// # Arguments
    /// - `width`: Screen width in pixels
    /// - `height`: Screen height in pixels
    ///
    /// # Errors
    /// Returns an error if the backend cannot initialize (e.g., no display available).
    fn initialize(&mut self, width: u32, height: u32) -> Result<(), GraphicsError>;

    /// Shut down the graphics system and release all resources.
    ///
    /// After shutdown, the backend must be reinitialized before further use.
    fn shutdown(&mut self) -> Result<(), GraphicsError>;

    /// Check if the graphics system is currently initialized.
    fn is_initialized(&self) -> bool;

    // ============================================================================
    // Screen & Color State
    // ============================================================================

    /// Clear the screen with the current background color.
    fn cls(&mut self) -> Result<(), GraphicsError>;

    /// Set the foreground and background colors for text operations.
    ///
    /// Colors are expected in RGB or RGBA format (platform-dependent).
    fn set_color(&mut self, foreground: u32, background: u32) -> Result<(), GraphicsError>;

    /// Get the current foreground color.
    fn get_foreground_color(&self) -> u32;

    /// Get the current background color.
    fn get_background_color(&self) -> u32;

    // ============================================================================
    // Text Operations
    // ============================================================================

    /// Locate the cursor to the given row and column (text-mode coordinates).
    ///
    /// # Arguments
    /// - `row`: Row number (1-based, typically 1-25)
    /// - `col`: Column number (1-based, typically 1-80)
    fn locate(&mut self, row: u32, col: u32) -> Result<(), GraphicsError>;

    /// Get the current cursor position.
    fn get_cursor_position(&self) -> (u32, u32);

    /// Print text at the current cursor position.
    ///
    /// Handles line wrapping and scrolling as needed.
    fn print(&mut self, text: &str) -> Result<(), GraphicsError>;

    // ============================================================================
    // Drawing Primitives
    // ============================================================================

    /// Plot a single pixel at the given coordinates.
    ///
    /// # Arguments
    /// - `x`, `y`: Pixel coordinates
    /// - `color`: Pixel color
    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError>;

    /// Get the color of a pixel at the given coordinates.
    fn point(&self, x: i32, y: i32) -> Result<u32, GraphicsError>;

    /// Draw a line from (x1, y1) to (x2, y2).
    ///
    /// # Arguments
    /// - `x1`, `y1`: Start coordinates
    /// - `x2`, `y2`: End coordinates
    /// - `color`: Line color
    /// - `filled`: Whether to fill (for box drawing)
    fn line(
        &mut self,
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
        color: u32,
        filled: bool,
    ) -> Result<(), GraphicsError>;

    /// Draw a circle or filled circle.
    ///
    /// # Arguments
    /// - `x`, `y`: Center coordinates
    /// - `radius`: Circle radius in pixels
    /// - `color`: Circle color
    /// - `filled`: Whether to fill the circle
    fn circle(
        &mut self,
        x: i32,
        y: i32,
        radius: i32,
        color: u32,
        filled: bool,
    ) -> Result<(), GraphicsError>;

    /// Flood fill starting from the given point.
    ///
    /// # Arguments
    /// - `x`, `y`: Starting point
    /// - `color`: Fill color
    /// - `boundary_color`: Color of the boundary to stop at (None = match source color)
    fn paint(
        &mut self,
        x: i32,
        y: i32,
        color: u32,
        boundary_color: Option<u32>,
    ) -> Result<(), GraphicsError>;

    // ============================================================================
    // STEP Variants (relative coordinate support)
    // ============================================================================

    /// Plot a pixel with optional STEP mode (relative coordinates).
    ///
    /// When `step` is true, coordinates are relative to the last graphics point.
    /// The default implementation simply calls `pset` (ignoring STEP).
    /// Backends that track the last graphics point should override this.
    ///
    /// # Arguments
    /// - `x`, `y`: Coordinates (absolute or relative if step=true)
    /// - `color`: Pixel color
    /// - `step`: If true, coordinates are relative to last graphics point
    fn pset_step(&mut self, x: i32, y: i32, color: u32, step: bool) -> Result<(), GraphicsError> {
        // Default: ignore step flag, just use absolute coordinates
        let _ = step;
        self.pset(x, y, color)
    }

    /// Draw a line with optional STEP mode for both endpoints.
    ///
    /// When `step1` is true, (x1, y1) is relative to the last graphics point.
    /// When `step2` is true, (x2, y2) is relative to (x1, y1) after STEP1 resolution.
    ///
    /// # Arguments
    /// - `x1`, `y1`: Start coordinates
    /// - `x2`, `y2`: End coordinates
    /// - `color`: Line color
    /// - `filled`: Whether to fill (for box drawing)
    /// - `step1`: If true, start coordinates are relative
    /// - `step2`: If true, end coordinates are relative to resolved start
    fn line_step(
        &mut self,
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
        color: u32,
        filled: bool,
        step1: bool,
        step2: bool,
    ) -> Result<(), GraphicsError> {
        // Default: ignore step flags, just use absolute coordinates
        let _ = (step1, step2);
        self.line(x1, y1, x2, y2, color, filled)
    }

    /// Draw a circle with optional STEP mode.
    ///
    /// When `step` is true, center coordinates are relative to the last graphics point.
    ///
    /// # Arguments
    /// - `x`, `y`: Center coordinates (absolute or relative if step=true)
    /// - `radius`: Circle radius in pixels
    /// - `color`: Circle color
    /// - `filled`: Whether to fill the circle
    /// - `step`: If true, center is relative to last graphics point
    fn circle_step(
        &mut self,
        x: i32,
        y: i32,
        radius: i32,
        color: u32,
        filled: bool,
        step: bool,
    ) -> Result<(), GraphicsError> {
        // Default: ignore step flag, just use absolute coordinates
        let _ = step;
        self.circle(x, y, radius, color, filled)
    }

    /// Flood fill with optional STEP mode.
    ///
    /// When `step` is true, starting point is relative to the last graphics point.
    ///
    /// # Arguments
    /// - `x`, `y`: Starting point (absolute or relative if step=true)
    /// - `color`: Fill color
    /// - `boundary_color`: Color of the boundary to stop at
    /// - `step`: If true, starting point is relative
    fn paint_step(
        &mut self,
        x: i32,
        y: i32,
        color: u32,
        boundary_color: Option<u32>,
        step: bool,
    ) -> Result<(), GraphicsError> {
        // Default: ignore step flag, just use absolute coordinates
        let _ = step;
        self.paint(x, y, color, boundary_color)
    }

    // ============================================================================
    // Display Management
    // ============================================================================

    /// Update the display with the current backbuffer contents.
    ///
    /// This should be called after drawing operations to make them visible.
    /// Some backends use double-buffering and require explicit flipping.
    fn display(&mut self) -> Result<(), GraphicsError>;

    /// Poll for window events (resize, close, etc.).
    ///
    /// # Returns
    /// - `Ok(true)`: The window is still open, continue running
    /// - `Ok(false)`: The window should close (user clicked X)
    /// - `Err(_)`: A graphics error occurred
    fn poll_events(&mut self) -> Result<bool, GraphicsError>;

    /// Get the current screen dimensions.
    fn get_screen_size(&self) -> (u32, u32);

    // ============================================================================
    // Page Copy and Coordinate Mapping Operations
    // ============================================================================

    /// Copy one video page to another.
    ///
    /// Used for double-buffering and animation.
    fn pcopy(&mut self, _src: i32, _dst: i32) -> Result<(), GraphicsError> {
        Ok(()) // Default: no-op (single page mode)
    }

    /// Map coordinates between world and screen coordinate systems.
    ///
    /// # Arguments
    /// - `coord`: The coordinate value to convert
    /// - `func_code`: Conversion function (0-3)
    ///   - 0: World X → Screen X
    ///   - 1: World Y → Screen Y
    ///   - 2: Screen X → World X
    ///   - 3: Screen Y → World Y
    ///
    /// # Returns
    /// The converted coordinate value
    fn pmap(&self, coord: f64, _func_code: i32) -> f64 {
        coord // Default: identity mapping (no WINDOW set)
    }

    // ============================================================================
    // Batch Operations (optimization)
    // ============================================================================

    /// Begin a batch of drawing operations for optimization.
    ///
    /// Some backends can batch draw calls for better performance.
    /// This is optional to implement; default can be a no-op.
    fn begin_batch(&mut self) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// End a batch of drawing operations and apply them.
    fn end_batch(&mut self) -> Result<(), GraphicsError> {
        Ok(())
    }

    // ============================================================================
    // Extended Graphics Operations (WIDTH, VIEW, WINDOW, DRAW)
    // ============================================================================

    /// Set the text mode width (columns and optionally rows).
    fn set_width(&mut self, _columns: u32, _rows: u32) -> Result<(), GraphicsError> {
        Ok(()) // Default: no-op for text-mode resize
    }

    /// Set the viewport for graphics operations.
    ///
    /// # Arguments
    /// - `screen`: If true, use absolute screen coordinates
    /// - `x1`, `y1`: Top-left corner of viewport
    /// - `x2`, `y2`: Bottom-right corner of viewport
    /// - `fill_color`: Optional fill color for the viewport
    /// - `border_color`: Optional border color
    fn set_view(
        &mut self,
        _screen: bool,
        _x1: i32,
        _y1: i32,
        _x2: i32,
        _y2: i32,
        _fill_color: Option<u32>,
        _border_color: Option<u32>,
    ) -> Result<(), GraphicsError> {
        Ok(()) // Default: viewport not supported
    }

    /// Reset the viewport to full screen.
    fn reset_view(&mut self) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Set the world coordinate system.
    ///
    /// # Arguments
    /// - `screen`: If true, Y coordinates increase downward
    /// - `x1`, `y1`: World coordinate for top-left
    /// - `x2`, `y2`: World coordinate for bottom-right
    fn set_window(
        &mut self,
        _screen: bool,
        _x1: f64,
        _y1: f64,
        _x2: f64,
        _y2: f64,
    ) -> Result<(), GraphicsError> {
        Ok(()) // Default: world coordinates not supported
    }

    /// Reset window coordinates to pixel coordinates.
    fn reset_window(&mut self) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Execute DRAW turtle graphics commands.
    ///
    /// # Arguments
    /// - `commands`: DRAW command string (e.g., "U10 R20 D10 L20")
    fn draw(&mut self, _commands: &str) -> Result<(), GraphicsError> {
        Ok(()) // Default: DRAW not supported
    }

    /// Set a palette entry.
    ///
    /// # Arguments
    /// - `index`: Palette index (0-255)
    /// - `color`: ARGB color value
    fn set_palette(&mut self, _index: i32, _color: u32) -> Result<(), GraphicsError> {
        Ok(()) // Default: palette not supported
    }

    /// Reset palette to default values.
    fn reset_palette(&mut self) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Get a palette entry.
    fn get_palette(&self, _index: i32) -> u32 {
        0 // Default: return black
    }

    // ============================================================================
    // QB64 Image Buffer Operations
    // ============================================================================

    /// Create a new image buffer.
    ///
    /// # Returns
    /// Image handle (negative numbers for errors, 0 for display, positive for images)
    fn new_image(&mut self, _width: i32, _height: i32, _mode: i32) -> i32 {
        -1 // Default: not supported
    }

    /// Load an image from a file.
    ///
    /// # Returns
    /// Image handle
    fn load_image(&mut self, _filename: &str, _mode: i32) -> i32 {
        -1 // Default: not supported
    }

    /// Free an image buffer.
    fn free_image(&mut self, _handle: i32) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Copy pixels from one image to another.
    fn put_image(
        &mut self,
        _dest_x1: i32,
        _dest_y1: i32,
        _dest_x2: i32,
        _dest_y2: i32,
        _src_handle: i32,
        _dest_handle: i32,
    ) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Full put_image with source coordinates.
    fn put_image_full(
        &mut self,
        _dest_x1: i32,
        _dest_y1: i32,
        _dest_x2: i32,
        _dest_y2: i32,
        _src_handle: i32,
        _dest_handle: i32,
        _src_x1: i32,
        _src_y1: i32,
        _src_x2: i32,
        _src_y2: i32,
    ) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Set the source image for reading operations.
    fn set_source(&mut self, _handle: i32) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Set the destination image for drawing operations.
    fn set_dest(&mut self, _handle: i32) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Copy an image buffer.
    fn copy_image(&mut self, _handle: i32, _mode: i32) -> i32 {
        -1 // Default: not supported
    }

    /// Capture screen to an image.
    fn screen_image(&mut self, _x1: i32, _y1: i32, _x2: i32, _y2: i32) -> i32 {
        -1 // Default: not supported
    }

    /// Print text at pixel coordinates.
    fn print_string(&mut self, _x: i32, _y: i32, _text: &str) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Set auto-display mode.
    fn set_autodisplay(&mut self, _enabled: bool) -> Result<(), GraphicsError> {
        Ok(())
    }

    /// Get the width of an image (or screen if handle is 0).
    fn get_image_width(&self, _handle: i32) -> i32 {
        0
    }

    /// Get the height of an image (or screen if handle is 0).
    fn get_image_height(&self, _handle: i32) -> i32 {
        0
    }

    // ============================================================================
    // Mouse Input (Phase 5)
    // ============================================================================

    /// Get the current mouse X position.
    fn get_mouse_x(&self) -> i32 {
        0
    }

    /// Get the current mouse Y position.
    fn get_mouse_y(&self) -> i32 {
        0
    }

    /// Get the state of a mouse button.
    ///
    /// # Arguments
    /// - `button`: 1 = left, 2 = right, 3 = middle
    fn get_mouse_button(&self, _button: u32) -> bool {
        false
    }

    /// Poll for mouse input events.
    ///
    /// Returns true if there was new mouse input.
    fn poll_mouse_input(&mut self) -> bool {
        false
    }

    /// Get mouse X movement since last call.
    fn get_mouse_movement_x(&self) -> i32 {
        0
    }

    /// Get mouse Y movement since last call.
    fn get_mouse_movement_y(&self) -> i32 {
        0
    }

    /// Get mouse wheel delta.
    fn get_mouse_wheel(&self) -> i32 {
        0
    }

    /// Hide the mouse cursor.
    fn hide_mouse(&mut self) {}

    /// Show the mouse cursor.
    fn show_mouse(&mut self) {}

    /// Move the mouse cursor to a position.
    fn move_mouse(&mut self, _x: i32, _y: i32) {}

    // ============================================================================
    // Clipboard (Phase 5)
    // ============================================================================

    /// Get text from the system clipboard.
    fn get_clipboard(&self) -> Option<String> {
        None
    }

    /// Set text to the system clipboard.
    fn set_clipboard(&mut self, _text: &str) {}

    // ============================================================================
    // Font Support (TrueType)
    // ============================================================================

    /// Load a TrueType font from a file.
    ///
    /// Returns a font handle on success, or 0 on failure.
    fn load_font(&mut self, _path: &str, _size: u16) -> i64 {
        0 // Default: fonts not supported
    }

    /// Set the current font for text rendering.
    ///
    /// Returns the previous font handle.
    fn set_font(&mut self, _handle: i64) -> i64 {
        0 // Default: only built-in font
    }

    /// Free a loaded font.
    fn free_font(&mut self, _handle: i64) {}

    /// Get the height of the current font in pixels.
    fn get_font_height(&self) -> u32 {
        16 // Default: 16 pixels for built-in font (8x8 doubled for readability)
    }

    /// Get the width of the current font in pixels.
    fn get_font_width(&self) -> u32 {
        8 // Default: 8 pixels for built-in font
    }

    /// Get the pixel width of a string with the current font.
    fn get_print_width(&self, text: &str) -> i64 {
        (text.len() as i64) * 8 // Default: 8 pixels per character
    }

    // ============================================================================
    // Window Control Functions
    // ============================================================================

    /// Set fullscreen mode.
    ///
    /// # Arguments
    /// - `mode`: 0 = windowed, 1 = fullscreen, 2 = fullscreen desktop (stretched)
    ///
    /// # Returns
    /// Previous fullscreen mode
    fn set_fullscreen(&mut self, _mode: i32) -> i32 {
        0 // Default: windowed mode, return 0
    }

    /// Get current fullscreen mode.
    ///
    /// # Returns
    /// - 0: Windowed
    /// - 1: Fullscreen
    /// - 2: Fullscreen desktop (stretched)
    fn get_fullscreen(&self) -> i32 {
        0 // Default: windowed
    }

    /// Move the window to the specified position.
    ///
    /// # Arguments
    /// - `x`: X position in screen coordinates
    /// - `y`: Y position in screen coordinates
    fn screen_move(&mut self, _x: i32, _y: i32) {
        // Default: no-op
    }

    /// Show the window (make visible).
    fn screen_show(&mut self) {
        // Default: no-op
    }

    /// Hide the window (make invisible).
    fn screen_hide(&mut self) {
        // Default: no-op
    }

    /// Check if the window is visible.
    fn is_screen_visible(&self) -> bool {
        true // Default: visible
    }

    // ============================================================================
    // Alpha Blending Functions
    // ============================================================================

    /// Enable alpha blending for an image.
    ///
    /// When enabled, pixels are blended using their alpha channel.
    ///
    /// # Arguments
    /// - `handle`: Image handle (0 = screen)
    fn set_blend(&mut self, _handle: i32) {
        // Default: no-op
    }

    /// Disable alpha blending for an image.
    ///
    /// When disabled, pixels are copied directly without alpha blending.
    ///
    /// # Arguments
    /// - `handle`: Image handle (0 = screen)
    fn set_dontblend(&mut self, _handle: i32) {
        // Default: no-op
    }

    /// Check if alpha blending is enabled for an image.
    ///
    /// # Arguments
    /// - `handle`: Image handle (0 = screen)
    ///
    /// # Returns
    /// true if blending is enabled
    fn get_blend(&self, _handle: i32) -> bool {
        true // Default: blending enabled
    }

    /// Set a transparent (clear) color for an image.
    ///
    /// Pixels matching this color will be treated as transparent during _PUTIMAGE.
    ///
    /// # Arguments
    /// - `color`: Color to make transparent (ARGB format)
    /// - `handle`: Image handle (0 = screen)
    fn set_clearcolor(&mut self, _color: u32, _handle: i32) {
        // Default: no-op
    }

    /// Disable the transparent color for an image.
    ///
    /// # Arguments
    /// - `handle`: Image handle (0 = screen)
    fn clear_clearcolor(&mut self, _handle: i32) {
        // Default: no-op
    }

    /// Get the current clear color for an image.
    ///
    /// # Arguments
    /// - `handle`: Image handle (0 = screen)
    ///
    /// # Returns
    /// The clear color, or -1 if no clear color is set
    fn get_clearcolor(&self, _handle: i32) -> i64 {
        -1 // Default: no clear color
    }
}

/// Global graphics backend instance.
///
/// This is used to bridge from C FFI code to the Rust implementation.
/// In a multi-threaded environment, this would need to be protected by a Mutex.
///
/// # Safety
///
/// This static is intentionally unsafe. The assumption is that:
/// 1. Graphics operations are single-threaded
/// 2. The runtime ensures proper initialization before use
/// 3. No concurrent access occurs
pub static mut GRAPHICS_BACKEND: Option<Box<dyn GraphicsBackend>> = None;

/// Initialize the global graphics backend with the default implementation.
///
/// This is called from the FFI layer when a program starts.
///
/// # Arguments
/// - `width`: Screen width
/// - `height`: Screen height
///
/// # Errors
/// Returns an error if initialization fails.
pub fn init_graphics(width: u32, height: u32) -> Result<(), GraphicsError> {
    #[cfg(feature = "graphics-sdl2")]
    {
        let mut backend = Box::new(sdl2::SDL2Backend::new());
        backend.initialize(width, height)?;
        unsafe {
            GRAPHICS_BACKEND = Some(backend);
        }
        Ok(())
    }

    #[cfg(not(any(feature = "graphics-sdl2")))]
    {
        let mut backend = Box::new(mock::MockBackend::new());
        backend.initialize(width, height)?;
        unsafe {
            GRAPHICS_BACKEND = Some(backend);
        }
        Ok(())
    }
}

/// Shut down the global graphics backend.
pub fn shutdown_graphics() -> Result<(), GraphicsError> {
    unsafe {
        if let Some(ref mut backend) = GRAPHICS_BACKEND {
            backend.shutdown()?;
            GRAPHICS_BACKEND = None;
        }
    }
    Ok(())
}

/// Helper macro to call methods on the global backend.
#[macro_export]
macro_rules! with_graphics {
    ($method:ident($($arg:expr),*)) => {{
        unsafe {
            if let Some(ref mut backend) = $crate::graphics::GRAPHICS_BACKEND {
                backend.$method($($arg),*)
            } else {
                Err($crate::graphics::GraphicsError::not_initialized())
            }
        }
    }};
}
