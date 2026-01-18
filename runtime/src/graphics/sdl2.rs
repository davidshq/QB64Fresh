//! SDL2 graphics backend implementation.
//!
//! This is the primary graphics backend for QB64Fresh.
//! It uses the `sdl2` crate to provide cross-platform graphics support.

use super::{GraphicsBackend, GraphicsError, GraphicsErrorKind};
use sdl2::event::Event;
use sdl2::pixels::Color;
use sdl2::rect::{Point, Rect};
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::EventPump;
use sdl2::Sdl;

/// SDL2-based graphics backend.
///
/// Provides graphics rendering using the Simple DirectMedia Layer (SDL2) library.
/// Supports:
/// - Cross-platform rendering (Windows, Linux, macOS)
/// - Hardware acceleration via SDL2's renderer
/// - Window management and event handling
/// - 2D drawing primitives
pub struct SDL2Backend {
    /// SDL2 context - must be kept alive for the duration of graphics operations
    sdl_context: Option<Sdl>,
    /// Canvas for rendering
    canvas: Option<Canvas<Window>>,
    /// Event pump for handling window events
    event_pump: Option<EventPump>,
    /// Whether the backend is initialized
    initialized: bool,
    /// Screen width
    width: u32,
    /// Screen height
    height: u32,
    /// Foreground color (ARGB format)
    fg_color: u32,
    /// Background color (ARGB format)
    bg_color: u32,
    /// Cursor row (1-based, text mode)
    cursor_row: u32,
    /// Cursor column (1-based, text mode)
    cursor_col: u32,
    /// Pixel buffer for POINT() function - stores pixel colors
    /// SDL2's read_pixels is slow, so we maintain our own buffer
    pixel_buffer: Vec<u32>,
}

impl std::fmt::Debug for SDL2Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SDL2Backend")
            .field("initialized", &self.initialized)
            .field("width", &self.width)
            .field("height", &self.height)
            .field("fg_color", &self.fg_color)
            .field("bg_color", &self.bg_color)
            .finish()
    }
}

impl SDL2Backend {
    /// Create a new SDL2 backend.
    pub fn new() -> Self {
        Self {
            sdl_context: None,
            canvas: None,
            event_pump: None,
            initialized: false,
            width: 0,
            height: 0,
            fg_color: 0xFFFFFFFF, // White (ARGB)
            bg_color: 0xFF000000, // Black (ARGB)
            cursor_row: 1,
            cursor_col: 1,
            pixel_buffer: Vec::new(),
        }
    }

    /// Convert ARGB color to SDL2 Color.
    fn argb_to_sdl_color(argb: u32) -> Color {
        let a = ((argb >> 24) & 0xFF) as u8;
        let r = ((argb >> 16) & 0xFF) as u8;
        let g = ((argb >> 8) & 0xFF) as u8;
        let b = (argb & 0xFF) as u8;
        Color::RGBA(r, g, b, a)
    }

    /// Get pixel buffer index for coordinates.
    fn pixel_index(&self, x: i32, y: i32) -> Option<usize> {
        if x >= 0 && y >= 0 && (x as u32) < self.width && (y as u32) < self.height {
            Some((y as u32 * self.width + x as u32) as usize)
        } else {
            None
        }
    }

    /// Set a pixel in the internal buffer.
    fn set_pixel_buffer(&mut self, x: i32, y: i32, color: u32) {
        if let Some(idx) = self.pixel_index(x, y) {
            if idx < self.pixel_buffer.len() {
                self.pixel_buffer[idx] = color;
            }
        }
    }

    /// Get a pixel from the internal buffer.
    fn get_pixel_buffer(&self, x: i32, y: i32) -> Option<u32> {
        self.pixel_index(x, y)
            .and_then(|idx| self.pixel_buffer.get(idx).copied())
    }

    /// Draw a circle using the midpoint circle algorithm.
    fn draw_circle_outline(&mut self, cx: i32, cy: i32, radius: i32, color: u32) {
        if radius <= 0 {
            return;
        }

        let canvas = match self.canvas.as_mut() {
            Some(c) => c,
            None => return,
        };
        canvas.set_draw_color(Self::argb_to_sdl_color(color));

        let mut x = radius;
        let mut y = 0;
        let mut p = 1 - radius;

        // Draw initial points
        let points = [
            Point::new(cx + x, cy),
            Point::new(cx - x, cy),
            Point::new(cx, cy + x),
            Point::new(cx, cy - x),
        ];
        let _ = canvas.draw_points(&points[..]);

        while x > y {
            y += 1;
            if p <= 0 {
                p = p + 2 * y + 1;
            } else {
                x -= 1;
                p = p + 2 * y - 2 * x + 1;
            }

            if x < y {
                break;
            }

            // Draw 8 symmetric points
            let points = [
                Point::new(cx + x, cy + y),
                Point::new(cx - x, cy + y),
                Point::new(cx + x, cy - y),
                Point::new(cx - x, cy - y),
                Point::new(cx + y, cy + x),
                Point::new(cx - y, cy + x),
                Point::new(cx + y, cy - x),
                Point::new(cx - y, cy - x),
            ];
            let _ = canvas.draw_points(&points[..]);
        }
    }

    /// Draw a filled circle using horizontal lines.
    fn draw_circle_filled(&mut self, cx: i32, cy: i32, radius: i32, color: u32) {
        if radius <= 0 {
            return;
        }

        let canvas = match self.canvas.as_mut() {
            Some(c) => c,
            None => return,
        };
        canvas.set_draw_color(Self::argb_to_sdl_color(color));

        let mut x = radius;
        let mut y = 0;
        let mut p = 1 - radius;

        // Draw horizontal lines for filled circle
        let _ = canvas.draw_line(Point::new(cx - x, cy), Point::new(cx + x, cy));

        while x > y {
            y += 1;
            if p <= 0 {
                p = p + 2 * y + 1;
            } else {
                x -= 1;
                p = p + 2 * y - 2 * x + 1;
            }

            if x < y {
                break;
            }

            // Draw horizontal lines at each y level
            let _ = canvas.draw_line(Point::new(cx - x, cy + y), Point::new(cx + x, cy + y));
            let _ = canvas.draw_line(Point::new(cx - x, cy - y), Point::new(cx + x, cy - y));
            if x != y {
                let _ = canvas.draw_line(Point::new(cx - y, cy + x), Point::new(cx + y, cy + x));
                let _ = canvas.draw_line(Point::new(cx - y, cy - x), Point::new(cx + y, cy - x));
            }
        }
    }

    /// Flood fill using scanline algorithm (more efficient than recursive).
    fn flood_fill(&mut self, start_x: i32, start_y: i32, fill_color: u32, boundary: Option<u32>) {
        // Get the target color (color at start point)
        let target_color = match self.get_pixel_buffer(start_x, start_y) {
            Some(c) => c,
            None => return,
        };

        // Don't fill if already the fill color
        if target_color == fill_color {
            return;
        }

        // For boundary mode, check if we're on a boundary
        if let Some(bc) = boundary {
            if target_color == bc {
                return;
            }
        }

        let width = self.width as i32;
        let height = self.height as i32;

        // Stack-based flood fill
        let mut stack = vec![(start_x, start_y)];

        while let Some((x, y)) = stack.pop() {
            // Skip if out of bounds
            if x < 0 || x >= width || y < 0 || y >= height {
                continue;
            }

            // Get current pixel color
            let current = match self.get_pixel_buffer(x, y) {
                Some(c) => c,
                None => continue,
            };

            // Check if we should fill this pixel
            let should_fill = if let Some(bc) = boundary {
                // Boundary mode: stop at boundary color
                current != bc && current != fill_color
            } else {
                // Match mode: only fill pixels matching target color
                current == target_color
            };

            if !should_fill {
                continue;
            }

            // Fill this pixel
            self.set_pixel_buffer(x, y, fill_color);
            if let Some(canvas) = self.canvas.as_mut() {
                canvas.set_draw_color(Self::argb_to_sdl_color(fill_color));
                let _ = canvas.draw_point(Point::new(x, y));
            }

            // Add neighbors to stack
            stack.push((x + 1, y));
            stack.push((x - 1, y));
            stack.push((x, y + 1));
            stack.push((x, y - 1));
        }
    }
}

impl Default for SDL2Backend {
    fn default() -> Self {
        Self::new()
    }
}

impl GraphicsBackend for SDL2Backend {
    fn initialize(&mut self, width: u32, height: u32) -> Result<(), GraphicsError> {
        if self.initialized {
            return Err(GraphicsError::already_initialized());
        }

        // Initialize SDL2
        let sdl_context = sdl2::init().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 init failed: {}", e),
            )
        })?;

        // Initialize video subsystem
        let video_subsystem = sdl_context.video().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 video init failed: {}", e),
            )
        })?;

        // Create window
        let window = video_subsystem
            .window("QB64Fresh", width, height)
            .position_centered()
            .build()
            .map_err(|e| {
                GraphicsError::new(
                    GraphicsErrorKind::BackendError,
                    format!("SDL2 window creation failed: {}", e),
                )
            })?;

        // Create canvas (renderer)
        let canvas = window.into_canvas().build().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 canvas creation failed: {}", e),
            )
        })?;

        // Create event pump
        let event_pump = sdl_context.event_pump().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 event pump creation failed: {}", e),
            )
        })?;

        // Initialize pixel buffer
        let pixel_buffer = vec![self.bg_color; (width * height) as usize];

        self.sdl_context = Some(sdl_context);
        self.canvas = Some(canvas);
        self.event_pump = Some(event_pump);
        self.initialized = true;
        self.width = width;
        self.height = height;
        self.pixel_buffer = pixel_buffer;

        // Clear screen to background color
        self.cls()?;
        self.display()?;

        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Ok(());
        }

        // Drop SDL2 resources in reverse order
        self.event_pump = None;
        self.canvas = None;
        self.sdl_context = None;
        self.pixel_buffer.clear();
        self.initialized = false;

        Ok(())
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn cls(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        if let Some(canvas) = self.canvas.as_mut() {
            canvas.set_draw_color(Self::argb_to_sdl_color(self.bg_color));
            canvas.clear();
        }

        // Clear pixel buffer
        self.pixel_buffer.fill(self.bg_color);

        // Reset cursor
        self.cursor_row = 1;
        self.cursor_col = 1;

        Ok(())
    }

    fn set_color(&mut self, foreground: u32, background: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        self.fg_color = foreground;
        self.bg_color = background;

        Ok(())
    }

    fn get_foreground_color(&self) -> u32 {
        self.fg_color
    }

    fn get_background_color(&self) -> u32 {
        self.bg_color
    }

    fn locate(&mut self, row: u32, col: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        self.cursor_row = row.max(1);
        self.cursor_col = col.max(1);

        Ok(())
    }

    fn get_cursor_position(&self) -> (u32, u32) {
        (self.cursor_row, self.cursor_col)
    }

    fn print(&mut self, text: &str) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        // Text rendering requires SDL2_ttf which adds complexity.
        // For now, we'll render each character as a simple 8x8 block pattern.
        // This is a placeholder - proper font rendering would use SDL2_ttf.
        let char_width = 8;
        let char_height = 8;

        let start_x = ((self.cursor_col - 1) * char_width) as i32;
        let start_y = ((self.cursor_row - 1) * char_height) as i32;

        if let Some(canvas) = self.canvas.as_mut() {
            canvas.set_draw_color(Self::argb_to_sdl_color(self.fg_color));

            for (i, _ch) in text.chars().enumerate() {
                // Draw a simple filled rectangle for each character
                // In a full implementation, this would use font glyphs
                let x = start_x + (i as i32 * char_width as i32);
                let rect = Rect::new(x, start_y, char_width - 1, char_height - 1);
                let _ = canvas.draw_rect(rect);
            }
        }

        // Update cursor position
        self.cursor_col += text.len() as u32;

        Ok(())
    }

    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        // Update pixel buffer
        self.set_pixel_buffer(x, y, color);

        // Draw to canvas
        if let Some(canvas) = self.canvas.as_mut() {
            canvas.set_draw_color(Self::argb_to_sdl_color(color));
            let _ = canvas.draw_point(Point::new(x, y));
        }

        Ok(())
    }

    fn point(&self, x: i32, y: i32) -> Result<u32, GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        // Read from pixel buffer
        self.get_pixel_buffer(x, y).ok_or_else(|| {
            GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                "Coordinates out of bounds",
            )
        })
    }

    fn line(
        &mut self,
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
        color: u32,
        filled: bool,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        if let Some(canvas) = self.canvas.as_mut() {
            canvas.set_draw_color(Self::argb_to_sdl_color(color));

            if filled {
                // Draw filled rectangle (box)
                let x = x1.min(x2);
                let y = y1.min(y2);
                let w = (x1 - x2).unsigned_abs();
                let h = (y1 - y2).unsigned_abs();
                let rect = Rect::new(x, y, w.max(1), h.max(1));
                let _ = canvas.fill_rect(rect);

                // Update pixel buffer for the filled area
                for py in y..(y + h as i32) {
                    for px in x..(x + w as i32) {
                        self.set_pixel_buffer(px, py, color);
                    }
                }
            } else {
                // Draw line
                let _ = canvas.draw_line(Point::new(x1, y1), Point::new(x2, y2));

                // Update pixel buffer along the line (Bresenham's)
                let dx = (x2 - x1).abs();
                let dy = (y2 - y1).abs();
                let sx = if x1 < x2 { 1 } else { -1 };
                let sy = if y1 < y2 { 1 } else { -1 };
                let mut err = dx - dy;
                let mut x = x1;
                let mut y = y1;

                loop {
                    self.set_pixel_buffer(x, y, color);
                    if x == x2 && y == y2 {
                        break;
                    }
                    let e2 = 2 * err;
                    if e2 > -dy {
                        err -= dy;
                        x += sx;
                    }
                    if e2 < dx {
                        err += dx;
                        y += sy;
                    }
                }
            }
        }

        Ok(())
    }

    fn circle(
        &mut self,
        x: i32,
        y: i32,
        radius: i32,
        color: u32,
        filled: bool,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        if filled {
            self.draw_circle_filled(x, y, radius, color);
        } else {
            self.draw_circle_outline(x, y, radius, color);
        }

        // Update pixel buffer (simplified - proper implementation would track all pixels)
        // For filled circles, we update the buffer
        if filled {
            for py in (y - radius)..=(y + radius) {
                for px in (x - radius)..=(x + radius) {
                    let dx = px - x;
                    let dy = py - y;
                    if dx * dx + dy * dy <= radius * radius {
                        self.set_pixel_buffer(px, py, color);
                    }
                }
            }
        }

        Ok(())
    }

    fn paint(
        &mut self,
        x: i32,
        y: i32,
        color: u32,
        boundary_color: Option<u32>,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        self.flood_fill(x, y, color, boundary_color);

        Ok(())
    }

    fn display(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        if let Some(canvas) = self.canvas.as_mut() {
            canvas.present();
        }

        Ok(())
    }

    fn poll_events(&mut self) -> Result<bool, GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        if let Some(event_pump) = self.event_pump.as_mut() {
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. } => return Ok(false),
                    Event::KeyDown {
                        keycode: Some(sdl2::keyboard::Keycode::Escape),
                        ..
                    } => return Ok(false),
                    _ => {}
                }
            }
        }

        Ok(true)
    }

    fn get_screen_size(&self) -> (u32, u32) {
        (self.width, self.height)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_backend() {
        let backend = SDL2Backend::new();
        assert!(!backend.is_initialized());
        assert_eq!(backend.get_screen_size(), (0, 0));
    }

    #[test]
    fn test_color_conversion() {
        // Test ARGB to SDL color conversion
        let color = 0xFF112233; // A=255, R=17, G=34, B=51
        let sdl_color = SDL2Backend::argb_to_sdl_color(color);
        assert_eq!(sdl_color, Color::RGBA(0x11, 0x22, 0x33, 0xFF));
    }

    #[test]
    fn test_initialization_in_headless() {
        // This test may fail in headless environments, which is expected
        // For CI, use the mock backend instead
        let mut backend = SDL2Backend::new();

        // Try to initialize - may fail without display
        match backend.initialize(320, 200) {
            Ok(()) => {
                assert!(backend.is_initialized());
                assert_eq!(backend.get_screen_size(), (320, 200));
                let _ = backend.shutdown();
                assert!(!backend.is_initialized());
            }
            Err(_) => {
                // Expected in headless environment
                assert!(!backend.is_initialized());
            }
        }
    }

    #[test]
    fn test_double_init_error() {
        let mut backend = SDL2Backend::new();

        // First init may fail in headless
        if backend.initialize(320, 200).is_ok() {
            // Second init should fail with AlreadyInitialized
            let result = backend.initialize(640, 480);
            assert!(result.is_err());
            let _ = backend.shutdown();
        }
    }
}
