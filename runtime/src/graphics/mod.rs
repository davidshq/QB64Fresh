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
