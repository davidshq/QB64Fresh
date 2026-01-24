//! SDL2 graphics backend implementation.
//!
//! This is the primary graphics backend for QB64Fresh.
//! It uses the `sdl2` crate to provide cross-platform graphics support.

use super::font::{get_char_bitmap, is_pixel_set, FONT_HEIGHT, FONT_WIDTH};
use super::{GraphicsBackend, GraphicsError, GraphicsErrorKind};
use sdl2::event::Event;
use sdl2::mouse::MouseButton;
use sdl2::pixels::Color;
use sdl2::rect::{Point, Rect};
use sdl2::render::Canvas;
use sdl2::video::{FullscreenType, Window};
use sdl2::EventPump;
use sdl2::Sdl;
use std::collections::HashMap;

#[cfg(feature = "graphics-sdl2-ttf")]
use sdl2::ttf::Sdl2TtfContext;
#[cfg(feature = "graphics-sdl2-ttf")]
use std::path::Path;

/// Image buffer for _NEWIMAGE/_LOADIMAGE
#[derive(Debug)]
struct ImageBuffer {
    width: u32,
    height: u32,
    pixels: Vec<u32>,
    mode: i32,                // 0 = text, 32 = 32-bit color, etc.
    blend_enabled: bool,      // _BLEND/_DONTBLEND state (true = alpha blending on)
    clear_color: Option<u32>, // _CLEARCOLOR transparency key (None = disabled)
}

impl ImageBuffer {
    fn new(width: u32, height: u32, mode: i32, fill_color: u32) -> Self {
        let pixels = vec![fill_color; (width * height) as usize];
        Self {
            width,
            height,
            pixels,
            mode,
            blend_enabled: true, // Alpha blending enabled by default
            clear_color: None,   // No transparency key by default
        }
    }

    fn get_pixel(&self, x: i32, y: i32) -> Option<u32> {
        if x >= 0 && y >= 0 && (x as u32) < self.width && (y as u32) < self.height {
            Some(self.pixels[(y as u32 * self.width + x as u32) as usize])
        } else {
            None
        }
    }

    fn set_pixel(&mut self, x: i32, y: i32, color: u32) {
        if x >= 0 && y >= 0 && (x as u32) < self.width && (y as u32) < self.height {
            self.pixels[(y as u32 * self.width + x as u32) as usize] = color;
        }
    }
}

/// Viewport for VIEW statement
#[derive(Debug, Clone, Copy)]
struct Viewport {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    screen: bool, // If true, coordinates are absolute screen coordinates
}

impl Default for Viewport {
    fn default() -> Self {
        Self {
            x1: 0,
            y1: 0,
            x2: 0,
            y2: 0,
            screen: false,
        }
    }
}

/// World coordinate system for WINDOW statement
#[derive(Debug, Clone, Copy)]
struct WorldCoords {
    x1: f64,
    y1: f64,
    x2: f64,
    y2: f64,
    screen: bool, // If true, Y increases downward
    enabled: bool,
}

impl Default for WorldCoords {
    fn default() -> Self {
        Self {
            x1: 0.0,
            y1: 0.0,
            x2: 0.0,
            y2: 0.0,
            screen: true,
            enabled: false,
        }
    }
}

/// Color palette for PALETTE statement (256 entries)
#[derive(Debug, Clone)]
struct ColorPalette {
    colors: [u32; 256],
}

impl Default for ColorPalette {
    fn default() -> Self {
        // Default EGA/VGA palette
        let mut colors = [0xFF000000u32; 256];
        // First 16 colors (EGA palette)
        colors[0] = 0xFF000000; // Black
        colors[1] = 0xFF0000AA; // Blue
        colors[2] = 0xFF00AA00; // Green
        colors[3] = 0xFF00AAAA; // Cyan
        colors[4] = 0xFFAA0000; // Red
        colors[5] = 0xFFAA00AA; // Magenta
        colors[6] = 0xFFAA5500; // Brown
        colors[7] = 0xFFAAAAAA; // Light gray
        colors[8] = 0xFF555555; // Dark gray
        colors[9] = 0xFF5555FF; // Light blue
        colors[10] = 0xFF55FF55; // Light green
        colors[11] = 0xFF55FFFF; // Light cyan
        colors[12] = 0xFFFF5555; // Light red
        colors[13] = 0xFFFF55FF; // Light magenta
        colors[14] = 0xFFFFFF55; // Yellow
        colors[15] = 0xFFFFFFFF; // White
        Self { colors }
    }
}

/// DRAW turtle graphics state
#[derive(Debug, Clone)]
struct TurtleState {
    x: f64,
    y: f64,
    angle: f64, // In degrees, 0 = up, clockwise positive
    scale: f64, // Scale factor (default 4)
    pen_down: bool,
    color: u32,
}

impl Default for TurtleState {
    fn default() -> Self {
        Self {
            x: 0.0,
            y: 0.0,
            angle: 0.0,
            scale: 4.0,
            pen_down: true,
            color: 0xFFFFFFFF,
        }
    }
}

/// Loaded TrueType font information
#[cfg(feature = "graphics-sdl2-ttf")]
struct LoadedFont {
    /// File path of the font
    #[allow(dead_code)]
    path: String,
    /// Point size
    #[allow(dead_code)]
    size: u16,
    /// Character width (fixed for monospace, average for proportional)
    char_width: u32,
    /// Character height
    char_height: u32,
    /// Pre-rendered ASCII characters (32-126) as ARGB pixel buffers
    /// Each entry is (width, height, pixels)
    glyph_cache: HashMap<char, (u32, u32, Vec<u32>)>,
}

/// SDL2-based graphics backend.
pub struct SDL2Backend {
    sdl_context: Option<Sdl>,
    canvas: Option<Canvas<Window>>,
    event_pump: Option<EventPump>,
    initialized: bool,
    width: u32,
    height: u32,
    fg_color: u32,
    bg_color: u32,
    cursor_row: u32,
    cursor_col: u32,
    /// Main screen pixel buffer (handle 0)
    pixel_buffer: Vec<u32>,
    /// Image buffers (handle -> buffer)
    images: HashMap<i32, ImageBuffer>,
    /// Next available image handle
    next_handle: i32,
    /// Current source image handle (0 = screen)
    source_handle: i32,
    /// Current destination image handle (0 = screen)
    dest_handle: i32,
    /// Viewport for VIEW
    viewport: Option<Viewport>,
    /// World coordinates for WINDOW
    world_coords: WorldCoords,
    /// Color palette
    palette: ColorPalette,
    /// DRAW turtle state
    turtle: TurtleState,
    /// Last referenced graphics point (for STEP coordinates)
    last_gfx_x: i32,
    last_gfx_y: i32,
    /// Auto-display mode
    autodisplay: bool,
    // Mouse state
    mouse_x: i32,
    mouse_y: i32,
    mouse_buttons: [bool; 3],
    mouse_move_x: i32,
    mouse_move_y: i32,
    mouse_wheel: i32,
    mouse_input_available: bool,
    // Font state (TTF support)
    #[cfg(feature = "graphics-sdl2-ttf")]
    /// TTF context (must outlive all fonts)
    ttf_context: Option<Sdl2TtfContext>,
    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Loaded fonts (handle -> font info)
    fonts: HashMap<i64, LoadedFont>,
    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Next available font handle
    next_font_handle: i64,
    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Current font handle (0 = built-in 8x8)
    current_font: i64,
    // Window control state
    /// Current fullscreen mode (0 = windowed, 1 = fullscreen, 2 = fullscreen desktop)
    fullscreen_mode: i32,
    /// Whether the window is visible
    screen_visible: bool,
    // Screen blending state (for screen buffer, handle 0)
    /// Whether alpha blending is enabled for the screen
    screen_blend_enabled: bool,
    /// Transparency key for the screen (None = disabled)
    screen_clear_color: Option<u32>,
}

impl std::fmt::Debug for SDL2Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SDL2Backend")
            .field("initialized", &self.initialized)
            .field("width", &self.width)
            .field("height", &self.height)
            .field("fg_color", &self.fg_color)
            .field("bg_color", &self.bg_color)
            .field("images_count", &self.images.len())
            .finish()
    }
}

impl SDL2Backend {
    pub fn new() -> Self {
        Self {
            sdl_context: None,
            canvas: None,
            event_pump: None,
            initialized: false,
            width: 0,
            height: 0,
            fg_color: 0xFFFFFFFF,
            bg_color: 0xFF000000,
            cursor_row: 1,
            cursor_col: 1,
            pixel_buffer: Vec::new(),
            images: HashMap::new(),
            next_handle: 1, // 0 is reserved for screen
            source_handle: 0,
            dest_handle: 0,
            viewport: None,
            world_coords: WorldCoords::default(),
            palette: ColorPalette::default(),
            turtle: TurtleState::default(),
            last_gfx_x: 0,
            last_gfx_y: 0,
            autodisplay: true,
            mouse_x: 0,
            mouse_y: 0,
            mouse_buttons: [false; 3],
            mouse_move_x: 0,
            mouse_move_y: 0,
            mouse_wheel: 0,
            mouse_input_available: false,
            #[cfg(feature = "graphics-sdl2-ttf")]
            ttf_context: None,
            #[cfg(feature = "graphics-sdl2-ttf")]
            fonts: HashMap::new(),
            #[cfg(feature = "graphics-sdl2-ttf")]
            next_font_handle: 1, // 0 = built-in bitmap font
            #[cfg(feature = "graphics-sdl2-ttf")]
            current_font: 0,
            // Window control
            fullscreen_mode: 0,   // Start windowed
            screen_visible: true, // Start visible
            // Screen blending
            screen_blend_enabled: true,
            screen_clear_color: None,
        }
    }

    fn argb_to_sdl_color(argb: u32) -> Color {
        let a = ((argb >> 24) & 0xFF) as u8;
        let r = ((argb >> 16) & 0xFF) as u8;
        let g = ((argb >> 8) & 0xFF) as u8;
        let b = (argb & 0xFF) as u8;
        Color::RGBA(r, g, b, a)
    }

    /// Transform world coordinates to screen coordinates
    fn world_to_screen(&self, wx: f64, wy: f64) -> (i32, i32) {
        if !self.world_coords.enabled {
            return (wx as i32, wy as i32);
        }

        let (vx1, vy1, vx2, vy2) = if let Some(vp) = self.viewport {
            (vp.x1, vp.y1, vp.x2, vp.y2)
        } else {
            (0, 0, self.width as i32 - 1, self.height as i32 - 1)
        };

        let wc = &self.world_coords;
        let sx = ((wx - wc.x1) / (wc.x2 - wc.x1)) * (vx2 - vx1) as f64 + vx1 as f64;
        let sy = if wc.screen {
            // Y increases downward
            ((wy - wc.y1) / (wc.y2 - wc.y1)) * (vy2 - vy1) as f64 + vy1 as f64
        } else {
            // Y increases upward (Cartesian)
            ((wc.y2 - wy) / (wc.y2 - wc.y1)) * (vy2 - vy1) as f64 + vy1 as f64
        };

        (sx.round() as i32, sy.round() as i32)
    }

    /// Clip coordinates to viewport
    fn clip_to_viewport(&self, x: i32, y: i32) -> Option<(i32, i32)> {
        if let Some(vp) = self.viewport {
            if x >= vp.x1 && x <= vp.x2 && y >= vp.y1 && y <= vp.y2 {
                Some((x, y))
            } else {
                None
            }
        } else {
            // No viewport, clip to screen
            if x >= 0 && x < self.width as i32 && y >= 0 && y < self.height as i32 {
                Some((x, y))
            } else {
                None
            }
        }
    }

    fn pixel_index(&self, x: i32, y: i32) -> Option<usize> {
        if x >= 0 && y >= 0 && (x as u32) < self.width && (y as u32) < self.height {
            Some((y as u32 * self.width + x as u32) as usize)
        } else {
            None
        }
    }

    fn set_pixel_buffer(&mut self, x: i32, y: i32, color: u32) {
        if self.dest_handle == 0 {
            // Drawing to screen
            if let Some(idx) = self.pixel_index(x, y) {
                if idx < self.pixel_buffer.len() {
                    self.pixel_buffer[idx] = color;
                }
            }
        } else {
            // Drawing to image buffer
            if let Some(img) = self.images.get_mut(&self.dest_handle) {
                img.set_pixel(x, y, color);
            }
        }
    }

    fn get_pixel_buffer(&self, x: i32, y: i32) -> Option<u32> {
        if self.source_handle == 0 {
            // Reading from screen
            self.pixel_index(x, y)
                .and_then(|idx| self.pixel_buffer.get(idx).copied())
        } else {
            // Reading from image buffer
            self.images
                .get(&self.source_handle)
                .and_then(|img| img.get_pixel(x, y))
        }
    }

    /// Draw a single character at pixel coordinates using the embedded font
    fn draw_char(&mut self, ch: u8, px: i32, py: i32, fg_color: u32, bg_color: u32) {
        let bitmap = get_char_bitmap(ch);
        let width = self.width;
        let height = self.height;

        // First pass: update pixel buffer
        for row in 0..FONT_HEIGHT {
            for col in 0..FONT_WIDTH {
                let x = px + col as i32;
                let y = py + row as i32;

                if x < 0 || y < 0 || x >= width as i32 || y >= height as i32 {
                    continue;
                }

                let color = if is_pixel_set(bitmap, row as usize, col as usize) {
                    fg_color
                } else {
                    bg_color
                };

                // Update pixel buffer
                if let Some(idx) = self.pixel_index(x, y) {
                    if idx < self.pixel_buffer.len() {
                        self.pixel_buffer[idx] = color;
                    }
                }
            }
        }

        // Second pass: draw to canvas
        if let Some(canvas) = self.canvas.as_mut() {
            for row in 0..FONT_HEIGHT {
                for col in 0..FONT_WIDTH {
                    let x = px + col as i32;
                    let y = py + row as i32;

                    if x < 0 || y < 0 || x >= width as i32 || y >= height as i32 {
                        continue;
                    }

                    let color = if is_pixel_set(bitmap, row as usize, col as usize) {
                        fg_color
                    } else {
                        bg_color
                    };

                    canvas.set_draw_color(Self::argb_to_sdl_color(color));
                    let _ = canvas.draw_point(Point::new(x, y));
                }
            }
        }
    }

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

            let _ = canvas.draw_line(Point::new(cx - x, cy + y), Point::new(cx + x, cy + y));
            let _ = canvas.draw_line(Point::new(cx - x, cy - y), Point::new(cx + x, cy - y));
            if x != y {
                let _ = canvas.draw_line(Point::new(cx - y, cy + x), Point::new(cx + y, cy + x));
                let _ = canvas.draw_line(Point::new(cx - y, cy - x), Point::new(cx + y, cy - x));
            }
        }
    }

    fn flood_fill(&mut self, start_x: i32, start_y: i32, fill_color: u32, boundary: Option<u32>) {
        let target_color = match self.get_pixel_buffer(start_x, start_y) {
            Some(c) => c,
            None => return,
        };

        if target_color == fill_color {
            return;
        }

        if let Some(bc) = boundary {
            if target_color == bc {
                return;
            }
        }

        let width = self.width as i32;
        let height = self.height as i32;

        let mut stack = vec![(start_x, start_y)];

        while let Some((x, y)) = stack.pop() {
            if x < 0 || x >= width || y < 0 || y >= height {
                continue;
            }

            let current = match self.get_pixel_buffer(x, y) {
                Some(c) => c,
                None => continue,
            };

            let should_fill = if let Some(bc) = boundary {
                current != bc && current != fill_color
            } else {
                current == target_color
            };

            if !should_fill {
                continue;
            }

            self.set_pixel_buffer(x, y, fill_color);
            if let Some(canvas) = self.canvas.as_mut() {
                canvas.set_draw_color(Self::argb_to_sdl_color(fill_color));
                let _ = canvas.draw_point(Point::new(x, y));
            }

            stack.push((x + 1, y));
            stack.push((x - 1, y));
            stack.push((x, y + 1));
            stack.push((x, y - 1));
        }
    }

    /// Parse and execute DRAW commands
    fn execute_draw_commands(&mut self, commands: &str) -> Result<(), GraphicsError> {
        let mut chars = commands.chars().peekable();

        // Helper to parse a number
        fn parse_number(chars: &mut std::iter::Peekable<std::str::Chars>) -> Option<i32> {
            let mut num_str = String::new();
            let negative = if chars.peek() == Some(&'-') {
                chars.next();
                true
            } else {
                false
            };
            while let Some(&c) = chars.peek() {
                if c.is_ascii_digit() {
                    num_str.push(c);
                    chars.next();
                } else {
                    break;
                }
            }
            if num_str.is_empty() {
                None
            } else {
                let n: i32 = num_str.parse().ok()?;
                Some(if negative { -n } else { n })
            }
        }

        while let Some(c) = chars.next() {
            match c.to_ascii_uppercase() {
                // Movement commands
                'U' => {
                    // Up
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_y = self.turtle.y - n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            self.turtle.x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.y = new_y;
                }
                'D' => {
                    // Down
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_y = self.turtle.y + n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            self.turtle.x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.y = new_y;
                }
                'L' => {
                    // Left
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_x = self.turtle.x - n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            self.turtle.y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                }
                'R' => {
                    // Right
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_x = self.turtle.x + n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            self.turtle.y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                }
                'E' => {
                    // Up-Right diagonal
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_x = self.turtle.x + n;
                    let new_y = self.turtle.y - n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                    self.turtle.y = new_y;
                }
                'F' => {
                    // Down-Right diagonal
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_x = self.turtle.x + n;
                    let new_y = self.turtle.y + n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                    self.turtle.y = new_y;
                }
                'G' => {
                    // Down-Left diagonal
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_x = self.turtle.x - n;
                    let new_y = self.turtle.y + n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                    self.turtle.y = new_y;
                }
                'H' => {
                    // Up-Left diagonal
                    let n = parse_number(&mut chars).unwrap_or(1) as f64 * self.turtle.scale;
                    let new_x = self.turtle.x - n;
                    let new_y = self.turtle.y - n;
                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                    self.turtle.y = new_y;
                }
                'M' => {
                    // Move to x,y (absolute or relative with +/-)
                    let relative = chars.peek() == Some(&'+') || chars.peek() == Some(&'-');
                    let x = parse_number(&mut chars).unwrap_or(0) as f64;
                    // Skip comma or whitespace
                    while let Some(&c) = chars.peek() {
                        if c == ',' || c.is_whitespace() {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    let y = parse_number(&mut chars).unwrap_or(0) as f64;

                    let (new_x, new_y) = if relative {
                        (self.turtle.x + x, self.turtle.y + y)
                    } else {
                        (x, y)
                    };

                    if self.turtle.pen_down {
                        self.line(
                            self.turtle.x as i32,
                            self.turtle.y as i32,
                            new_x as i32,
                            new_y as i32,
                            self.turtle.color,
                            false,
                        )?;
                    }
                    self.turtle.x = new_x;
                    self.turtle.y = new_y;
                }
                'B' => {
                    // Move without drawing (blank)
                    self.turtle.pen_down = false;
                }
                'N' => {
                    // Move and return to original position
                    // This is a prefix for the next command
                    // For simplicity, we'll just set a flag (not fully implemented)
                }
                'A' => {
                    // Set angle (0-3, each 90 degrees)
                    let n = parse_number(&mut chars).unwrap_or(0);
                    self.turtle.angle = (n * 90) as f64;
                }
                'T' => {
                    // Turn by n degrees (T for Turn Angle in QB64)
                    if chars.peek() == Some(&'A') {
                        chars.next(); // skip 'A' in 'TA'
                    }
                    let n = parse_number(&mut chars).unwrap_or(0);
                    self.turtle.angle = n as f64;
                }
                'C' => {
                    // Set color
                    let n = parse_number(&mut chars).unwrap_or(15) as usize;
                    self.turtle.color = self.palette.colors[n.min(255)];
                }
                'S' => {
                    // Set scale
                    let n = parse_number(&mut chars).unwrap_or(4);
                    self.turtle.scale = n as f64;
                }
                'P' => {
                    // Paint (fill)
                    let fill = parse_number(&mut chars).unwrap_or(15) as usize;
                    // Skip comma
                    while let Some(&c) = chars.peek() {
                        if c == ',' || c.is_whitespace() {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    let border = parse_number(&mut chars).unwrap_or(fill as i32) as usize;

                    let fill_color = self.palette.colors[fill.min(255)];
                    let border_color = self.palette.colors[border.min(255)];
                    self.flood_fill(
                        self.turtle.x as i32,
                        self.turtle.y as i32,
                        fill_color,
                        Some(border_color),
                    );
                }
                ' ' | ';' => {
                    // Whitespace and semicolons are separators
                }
                _ => {
                    // Unknown command, skip
                }
            }

            // After any non-B command, pen is down again
            if c.to_ascii_uppercase() != 'B' {
                self.turtle.pen_down = true;
            }
        }

        Ok(())
    }

    /// Copy pixels from source image to destination
    fn copy_pixels(
        &mut self,
        src_handle: i32,
        src_x1: i32,
        src_y1: i32,
        src_x2: i32,
        src_y2: i32,
        dest_handle: i32,
        dest_x1: i32,
        dest_y1: i32,
        dest_x2: i32,
        dest_y2: i32,
    ) -> Result<(), GraphicsError> {
        // Get source dimensions, pixels, and blending settings
        let (src_w, src_h, src_pixels, src_blend, src_clear_color) = if src_handle == 0 {
            (
                self.width,
                self.height,
                self.pixel_buffer.clone(),
                self.screen_blend_enabled,
                self.screen_clear_color,
            )
        } else if let Some(img) = self.images.get(&src_handle) {
            (
                img.width,
                img.height,
                img.pixels.clone(),
                img.blend_enabled,
                img.clear_color,
            )
        } else {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                "Invalid source image handle",
            ));
        };

        // Calculate source rectangle
        let sx1 = src_x1.max(0) as u32;
        let sy1 = src_y1.max(0) as u32;
        let sx2 = if src_x2 < 0 {
            src_w - 1
        } else {
            (src_x2 as u32).min(src_w - 1)
        };
        let sy2 = if src_y2 < 0 {
            src_h - 1
        } else {
            (src_y2 as u32).min(src_h - 1)
        };

        // Calculate dest rectangle
        let dx1 = dest_x1;
        let dy1 = dest_y1;
        let dx2 = if dest_x2 < 0 {
            dx1 + (sx2 - sx1) as i32
        } else {
            dest_x2
        };
        let dy2 = if dest_y2 < 0 {
            dy1 + (sy2 - sy1) as i32
        } else {
            dest_y2
        };

        let src_width = (sx2 - sx1 + 1) as f64;
        let src_height = (sy2 - sy1 + 1) as f64;
        let dest_width = (dx2 - dx1 + 1) as f64;
        let dest_height = (dy2 - dy1 + 1) as f64;

        // Copy pixels with optional scaling, blending, and transparency
        for dy in dy1..=dy2 {
            for dx in dx1..=dx2 {
                // Map dest coord to source coord
                let sx = sx1 as f64 + ((dx - dx1) as f64 / dest_width) * src_width;
                let sy = sy1 as f64 + ((dy - dy1) as f64 / dest_height) * src_height;

                let sxi = sx as u32;
                let syi = sy as u32;

                if sxi < src_w && syi < src_h {
                    let src_idx = (syi * src_w + sxi) as usize;
                    if src_idx < src_pixels.len() {
                        let src_color = src_pixels[src_idx];

                        // Check _CLEARCOLOR transparency
                        if let Some(clear) = src_clear_color {
                            // Compare RGB only (ignore alpha in comparison)
                            if (src_color & 0x00FFFFFF) == (clear & 0x00FFFFFF) {
                                continue; // Skip transparent pixels
                            }
                        }

                        // Determine final color based on blend setting
                        let final_color = if src_blend {
                            // Alpha blending enabled
                            let src_alpha = (src_color >> 24) & 0xFF;
                            if src_alpha == 0 {
                                continue; // Fully transparent, skip
                            } else if src_alpha == 255 {
                                src_color // Fully opaque, just copy
                            } else {
                                // Blend with destination
                                let dest_color = if dest_handle == 0 {
                                    self.get_pixel_buffer(dx, dy).unwrap_or(0)
                                } else {
                                    self.images
                                        .get(&dest_handle)
                                        .and_then(|img| img.get_pixel(dx, dy))
                                        .unwrap_or(0)
                                };

                                Self::blend_colors(src_color, dest_color)
                            }
                        } else {
                            // No blending - direct copy (but make fully opaque)
                            src_color | 0xFF000000
                        };

                        // Write to destination
                        if dest_handle == 0 {
                            self.set_pixel_buffer(dx, dy, final_color);
                            if let Some(canvas) = self.canvas.as_mut() {
                                canvas.set_draw_color(Self::argb_to_sdl_color(final_color));
                                let _ = canvas.draw_point(Point::new(dx, dy));
                            }
                        } else if let Some(img) = self.images.get_mut(&dest_handle) {
                            img.set_pixel(dx, dy, final_color);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Blend two ARGB colors using source alpha.
    fn blend_colors(src: u32, dst: u32) -> u32 {
        let src_a = ((src >> 24) & 0xFF) as u32;
        let src_r = ((src >> 16) & 0xFF) as u32;
        let src_g = ((src >> 8) & 0xFF) as u32;
        let src_b = (src & 0xFF) as u32;

        let dst_a = ((dst >> 24) & 0xFF) as u32;
        let dst_r = ((dst >> 16) & 0xFF) as u32;
        let dst_g = ((dst >> 8) & 0xFF) as u32;
        let dst_b = (dst & 0xFF) as u32;

        // Standard alpha blending: out = src * alpha + dst * (1 - alpha)
        let inv_alpha = 255 - src_a;
        let out_r = (src_r * src_a + dst_r * inv_alpha) / 255;
        let out_g = (src_g * src_a + dst_g * inv_alpha) / 255;
        let out_b = (src_b * src_a + dst_b * inv_alpha) / 255;
        // Output alpha: combine using standard formula
        let out_a = src_a + (dst_a * inv_alpha) / 255;

        ((out_a.min(255)) << 24)
            | ((out_r.min(255)) << 16)
            | ((out_g.min(255)) << 8)
            | (out_b.min(255))
    }

    // ========== TrueType Font Support (requires graphics-sdl2-ttf feature) ==========

    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Load a TrueType font from a file.
    ///
    /// Returns a font handle on success, or 0 on failure.
    pub fn load_font(&mut self, path: &str, size: u16) -> i64 {
        let ttf_ctx = match self.ttf_context.as_ref() {
            Some(ctx) => ctx,
            None => return 0, // TTF not initialized
        };

        // Try to load the font
        let font = match ttf_ctx.load_font(Path::new(path), size) {
            Ok(f) => f,
            Err(_) => return 0, // Font load failed
        };

        // Calculate character dimensions
        // Use 'M' as reference for monospace width estimation
        let (char_width, char_height) = {
            let metrics = font.find_glyph_metrics('M');
            let advance = metrics.map(|m| m.advance).unwrap_or(size as i32);
            (advance.max(1) as u32, font.height().max(1) as u32)
        };

        // Pre-render and cache common ASCII characters (32-126)
        let mut glyph_cache = HashMap::new();
        for c in 32u8..=126 {
            let ch = c as char;
            if let Ok(surface) = font
                .render_char(ch)
                .blended(Color::RGBA(255, 255, 255, 255))
            {
                let width = surface.width();
                let height = surface.height();
                let pixels = surface
                    .without_lock()
                    .map(|data| {
                        // Convert SDL2 surface pixels to ARGB
                        let pitch = surface.pitch() as usize;
                        let bpp = surface.pixel_format_enum().byte_size_per_pixel();
                        let mut argb_pixels = Vec::with_capacity((width * height) as usize);

                        for y in 0..height {
                            for x in 0..width {
                                let offset = y as usize * pitch + x as usize * bpp;
                                if offset + 3 < data.len() {
                                    // Assuming RGBA format
                                    let r = data[offset];
                                    let g = data[offset + 1];
                                    let b = data[offset + 2];
                                    let a = if bpp > 3 { data[offset + 3] } else { 255 };
                                    argb_pixels.push(
                                        ((a as u32) << 24)
                                            | ((r as u32) << 16)
                                            | ((g as u32) << 8)
                                            | (b as u32),
                                    );
                                } else {
                                    argb_pixels.push(0);
                                }
                            }
                        }
                        argb_pixels
                    })
                    .unwrap_or_default();

                if !pixels.is_empty() {
                    glyph_cache.insert(ch, (width, height, pixels));
                }
            }
        }

        let handle = self.next_font_handle;
        self.next_font_handle += 1;

        self.fonts.insert(
            handle,
            LoadedFont {
                path: path.to_string(),
                size,
                char_width,
                char_height,
                glyph_cache,
            },
        );

        handle
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    /// Load a TrueType font (stub - TTF support not enabled).
    pub fn load_font(&mut self, _path: &str, _size: u16) -> i64 {
        0 // TTF not supported
    }

    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Set the current font for text rendering.
    ///
    /// Returns the previous font handle.
    pub fn set_font(&mut self, handle: i64) -> i64 {
        let prev = self.current_font;
        if handle == 0 || self.fonts.contains_key(&handle) {
            self.current_font = handle;
        }
        prev
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    /// Set the current font (stub - TTF support not enabled).
    pub fn set_font(&mut self, _handle: i64) -> i64 {
        0
    }

    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Free a loaded font.
    pub fn free_font(&mut self, handle: i64) {
        if handle > 0 {
            self.fonts.remove(&handle);
            if self.current_font == handle {
                self.current_font = 0;
            }
        }
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    /// Free a loaded font (stub - TTF support not enabled).
    pub fn free_font(&mut self, _handle: i64) {}

    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Get the height of the current font.
    pub fn get_font_height(&self) -> u32 {
        if self.current_font == 0 {
            FONT_HEIGHT
        } else if let Some(font) = self.fonts.get(&self.current_font) {
            font.char_height
        } else {
            FONT_HEIGHT
        }
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    /// Get the height of the current font.
    pub fn get_font_height(&self) -> u32 {
        FONT_HEIGHT
    }

    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Get the width of the current font (for monospace) or average width.
    pub fn get_font_width(&self) -> u32 {
        if self.current_font == 0 {
            FONT_WIDTH
        } else if let Some(font) = self.fonts.get(&self.current_font) {
            font.char_width
        } else {
            FONT_WIDTH
        }
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    /// Get the width of the current font.
    pub fn get_font_width(&self) -> u32 {
        FONT_WIDTH
    }

    #[cfg(feature = "graphics-sdl2-ttf")]
    /// Get the pixel width of a string with the current font.
    pub fn get_print_width(&self, text: &str) -> i64 {
        if self.current_font == 0 {
            // Built-in 8x8 font: simple calculation
            (text.len() as i64) * (FONT_WIDTH as i64)
        } else if let Some(font) = self.fonts.get(&self.current_font) {
            // TTF font: sum individual character widths from cache
            let mut width = 0i64;
            for ch in text.chars() {
                if let Some((w, _, _)) = font.glyph_cache.get(&ch) {
                    width += *w as i64;
                } else {
                    // Use average width for uncached characters
                    width += font.char_width as i64;
                }
            }
            width
        } else {
            (text.len() as i64) * (FONT_WIDTH as i64)
        }
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    /// Get the pixel width of a string with the current font.
    pub fn get_print_width(&self, text: &str) -> i64 {
        (text.len() as i64) * (FONT_WIDTH as i64)
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

        let sdl_context = sdl2::init().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 init failed: {}", e),
            )
        })?;

        let video_subsystem = sdl_context.video().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 video init failed: {}", e),
            )
        })?;

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

        let canvas = window.into_canvas().build().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 canvas creation failed: {}", e),
            )
        })?;

        let event_pump = sdl_context.event_pump().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 event pump creation failed: {}", e),
            )
        })?;

        let pixel_buffer = vec![self.bg_color; (width * height) as usize];

        // Initialize TTF subsystem for TrueType font support (if enabled)
        #[cfg(feature = "graphics-sdl2-ttf")]
        {
            let ttf_context = sdl2::ttf::init().map_err(|e| {
                GraphicsError::new(
                    GraphicsErrorKind::BackendError,
                    format!("SDL2_ttf init failed: {}", e),
                )
            })?;
            self.ttf_context = Some(ttf_context);
        }

        self.sdl_context = Some(sdl_context);
        self.canvas = Some(canvas);
        self.event_pump = Some(event_pump);
        self.initialized = true;
        self.width = width;
        self.height = height;
        self.pixel_buffer = pixel_buffer;

        // Initialize turtle to center of screen
        self.turtle.x = width as f64 / 2.0;
        self.turtle.y = height as f64 / 2.0;

        self.cls()?;
        self.display()?;

        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Ok(());
        }

        // Clear fonts before TTF context (if TTF enabled)
        #[cfg(feature = "graphics-sdl2-ttf")]
        {
            self.fonts.clear();
            self.current_font = 0;
            self.ttf_context = None;
        }

        self.event_pump = None;
        self.canvas = None;
        self.sdl_context = None;
        self.pixel_buffer.clear();
        self.images.clear();
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

        self.pixel_buffer.fill(self.bg_color);
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

        let char_width = FONT_WIDTH;
        let char_height = FONT_HEIGHT;
        let cols = self.width / char_width;
        let rows = self.height / char_height;

        for ch in text.bytes() {
            if ch == b'\n' {
                // Newline
                self.cursor_col = 1;
                self.cursor_row += 1;
                // TODO: scroll if needed
                continue;
            }

            if ch == b'\r' {
                // Carriage return
                self.cursor_col = 1;
                continue;
            }

            // Calculate pixel position
            let px = ((self.cursor_col - 1) * char_width) as i32;
            let py = ((self.cursor_row - 1) * char_height) as i32;

            // Draw the character
            self.draw_char(ch, px, py, self.fg_color, self.bg_color);

            // Advance cursor
            self.cursor_col += 1;

            // Wrap if needed
            if self.cursor_col > cols {
                self.cursor_col = 1;
                self.cursor_row += 1;
            }

            // Scroll if needed (simplified - just wrap)
            if self.cursor_row > rows {
                self.cursor_row = rows;
                // TODO: implement actual scrolling
            }
        }

        Ok(())
    }

    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        // Update last referenced graphics point
        self.last_gfx_x = x;
        self.last_gfx_y = y;

        // Apply coordinate transformation if WINDOW is set
        let (sx, sy) = self.world_to_screen(x as f64, y as f64);

        // Clip to viewport if set
        if self.clip_to_viewport(sx, sy).is_none() {
            return Ok(());
        }

        self.set_pixel_buffer(sx, sy, color);

        if let Some(canvas) = self.canvas.as_mut() {
            canvas.set_draw_color(Self::argb_to_sdl_color(color));
            let _ = canvas.draw_point(Point::new(sx, sy));
        }

        Ok(())
    }

    fn pset_step(&mut self, x: i32, y: i32, color: u32, step: bool) -> Result<(), GraphicsError> {
        let (final_x, final_y) = if step {
            (self.last_gfx_x + x, self.last_gfx_y + y)
        } else {
            (x, y)
        };
        self.pset(final_x, final_y, color)
    }

    fn point(&self, x: i32, y: i32) -> Result<u32, GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        let (sx, sy) = self.world_to_screen(x as f64, y as f64);

        self.get_pixel_buffer(sx, sy).ok_or_else(|| {
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

        // Transform coordinates
        let (sx1, sy1) = self.world_to_screen(x1 as f64, y1 as f64);
        let (sx2, sy2) = self.world_to_screen(x2 as f64, y2 as f64);

        if let Some(canvas) = self.canvas.as_mut() {
            canvas.set_draw_color(Self::argb_to_sdl_color(color));

            if filled {
                // Draw filled rectangle (box)
                let x = sx1.min(sx2);
                let y = sy1.min(sy2);
                let w = (sx1 - sx2).unsigned_abs();
                let h = (sy1 - sy2).unsigned_abs();
                let rect = Rect::new(x, y, w.max(1), h.max(1));
                let _ = canvas.fill_rect(rect);

                for py in y..(y + h as i32) {
                    for px in x..(x + w as i32) {
                        self.set_pixel_buffer(px, py, color);
                    }
                }
            } else {
                let _ = canvas.draw_line(Point::new(sx1, sy1), Point::new(sx2, sy2));

                // Update pixel buffer (Bresenham's)
                let dx = (sx2 - sx1).abs();
                let dy = (sy2 - sy1).abs();
                let sxd = if sx1 < sx2 { 1 } else { -1 };
                let syd = if sy1 < sy2 { 1 } else { -1 };
                let mut err = dx - dy;
                let mut x = sx1;
                let mut y = sy1;

                loop {
                    self.set_pixel_buffer(x, y, color);
                    if x == sx2 && y == sy2 {
                        break;
                    }
                    let e2 = 2 * err;
                    if e2 > -dy {
                        err -= dy;
                        x += sxd;
                    }
                    if e2 < dx {
                        err += dx;
                        y += syd;
                    }
                }
            }
        }

        Ok(())
    }

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
        // Resolve first coordinate
        let (final_x1, final_y1) = if step1 {
            (self.last_gfx_x + x1, self.last_gfx_y + y1)
        } else {
            (x1, y1)
        };

        // Resolve second coordinate (relative to resolved first if step2)
        let (final_x2, final_y2) = if step2 {
            (final_x1 + x2, final_y1 + y2)
        } else {
            (x2, y2)
        };

        // Update last graphics point to the end of the line
        self.last_gfx_x = final_x2;
        self.last_gfx_y = final_y2;

        self.line(final_x1, final_y1, final_x2, final_y2, color, filled)
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

        // Update last referenced graphics point
        self.last_gfx_x = x;
        self.last_gfx_y = y;

        let (cx, cy) = self.world_to_screen(x as f64, y as f64);

        if filled {
            self.draw_circle_filled(cx, cy, radius, color);

            for py in (cy - radius)..=(cy + radius) {
                for px in (cx - radius)..=(cx + radius) {
                    let dx = px - cx;
                    let dy = py - cy;
                    if dx * dx + dy * dy <= radius * radius {
                        self.set_pixel_buffer(px, py, color);
                    }
                }
            }
        } else {
            self.draw_circle_outline(cx, cy, radius, color);
        }

        Ok(())
    }

    fn circle_step(
        &mut self,
        x: i32,
        y: i32,
        radius: i32,
        color: u32,
        filled: bool,
        step: bool,
    ) -> Result<(), GraphicsError> {
        let (final_x, final_y) = if step {
            (self.last_gfx_x + x, self.last_gfx_y + y)
        } else {
            (x, y)
        };
        self.circle(final_x, final_y, radius, color, filled)
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

        // Update last referenced graphics point
        self.last_gfx_x = x;
        self.last_gfx_y = y;

        let (sx, sy) = self.world_to_screen(x as f64, y as f64);
        self.flood_fill(sx, sy, color, boundary_color);

        Ok(())
    }

    fn paint_step(
        &mut self,
        x: i32,
        y: i32,
        color: u32,
        boundary_color: Option<u32>,
        step: bool,
    ) -> Result<(), GraphicsError> {
        let (final_x, final_y) = if step {
            (self.last_gfx_x + x, self.last_gfx_y + y)
        } else {
            (x, y)
        };
        self.paint(final_x, final_y, color, boundary_color)
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
                    Event::MouseMotion {
                        x, y, xrel, yrel, ..
                    } => {
                        self.mouse_x = x;
                        self.mouse_y = y;
                        self.mouse_move_x += xrel;
                        self.mouse_move_y += yrel;
                        self.mouse_input_available = true;
                    }
                    Event::MouseButtonDown {
                        mouse_btn, x, y, ..
                    } => {
                        self.mouse_x = x;
                        self.mouse_y = y;
                        match mouse_btn {
                            MouseButton::Left => self.mouse_buttons[0] = true,
                            MouseButton::Right => self.mouse_buttons[1] = true,
                            MouseButton::Middle => self.mouse_buttons[2] = true,
                            _ => {}
                        }
                        self.mouse_input_available = true;
                    }
                    Event::MouseButtonUp {
                        mouse_btn, x, y, ..
                    } => {
                        self.mouse_x = x;
                        self.mouse_y = y;
                        match mouse_btn {
                            MouseButton::Left => self.mouse_buttons[0] = false,
                            MouseButton::Right => self.mouse_buttons[1] = false,
                            MouseButton::Middle => self.mouse_buttons[2] = false,
                            _ => {}
                        }
                        self.mouse_input_available = true;
                    }
                    Event::MouseWheel { y, .. } => {
                        self.mouse_wheel += y;
                        self.mouse_input_available = true;
                    }
                    _ => {}
                }
            }
        }

        Ok(true)
    }

    fn get_screen_size(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    // ========================================================================
    // VIEW/WINDOW Implementation
    // ========================================================================

    fn set_view(
        &mut self,
        screen: bool,
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
        fill_color: Option<u32>,
        border_color: Option<u32>,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        self.viewport = Some(Viewport {
            x1,
            y1,
            x2,
            y2,
            screen,
        });

        // Draw border if specified
        if let Some(bc) = border_color {
            self.line(x1, y1, x2, y1, bc, false)?; // Top
            self.line(x1, y2, x2, y2, bc, false)?; // Bottom
            self.line(x1, y1, x1, y2, bc, false)?; // Left
            self.line(x2, y1, x2, y2, bc, false)?; // Right
        }

        // Fill viewport if specified
        if let Some(fc) = fill_color {
            self.line(x1 + 1, y1 + 1, x2 - 1, y2 - 1, fc, true)?;
        }

        Ok(())
    }

    fn reset_view(&mut self) -> Result<(), GraphicsError> {
        self.viewport = None;
        Ok(())
    }

    fn set_window(
        &mut self,
        screen: bool,
        x1: f64,
        y1: f64,
        x2: f64,
        y2: f64,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        self.world_coords = WorldCoords {
            x1,
            y1,
            x2,
            y2,
            screen,
            enabled: true,
        };

        Ok(())
    }

    fn reset_window(&mut self) -> Result<(), GraphicsError> {
        self.world_coords.enabled = false;
        Ok(())
    }

    // ========================================================================
    // DRAW Turtle Graphics
    // ========================================================================

    fn draw(&mut self, commands: &str) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        self.execute_draw_commands(commands)
    }

    fn set_palette(&mut self, index: i32, color: u32) -> Result<(), GraphicsError> {
        if index >= 0 && index < 256 {
            self.palette.colors[index as usize] = color;
        }
        Ok(())
    }

    fn reset_palette(&mut self) -> Result<(), GraphicsError> {
        self.palette = ColorPalette::default();
        Ok(())
    }

    fn get_palette(&self, index: i32) -> u32 {
        if index >= 0 && index < 256 {
            self.palette.colors[index as usize]
        } else {
            0
        }
    }

    // ========================================================================
    // Image Buffer Operations
    // ========================================================================

    fn new_image(&mut self, width: i32, height: i32, mode: i32) -> i32 {
        if !self.initialized || width <= 0 || height <= 0 {
            return -1;
        }

        let handle = self.next_handle;
        self.next_handle += 1;

        let img = ImageBuffer::new(width as u32, height as u32, mode, self.bg_color);
        self.images.insert(handle, img);

        handle
    }

    fn load_image(&mut self, filename: &str, mode: i32) -> i32 {
        if !self.initialized {
            return -1;
        }

        #[cfg(feature = "graphics-sdl2")]
        {
            use image::GenericImageView;

            let img = match image::open(filename) {
                Ok(img) => img,
                Err(_) => return -1,
            };

            let (width, height) = img.dimensions();
            let rgba = img.to_rgba8();

            let handle = self.next_handle;
            self.next_handle += 1;

            let mut pixels = Vec::with_capacity((width * height) as usize);
            for pixel in rgba.pixels() {
                let r = pixel[0] as u32;
                let g = pixel[1] as u32;
                let b = pixel[2] as u32;
                let a = pixel[3] as u32;
                pixels.push((a << 24) | (r << 16) | (g << 8) | b);
            }

            let img_buf = ImageBuffer {
                width,
                height,
                pixels,
                mode,
                blend_enabled: true,
                clear_color: None,
            };
            self.images.insert(handle, img_buf);

            handle
        }

        #[cfg(not(feature = "graphics-sdl2"))]
        {
            let _ = (filename, mode);
            -1
        }
    }

    fn free_image(&mut self, handle: i32) -> Result<(), GraphicsError> {
        if handle <= 0 {
            return Ok(()); // Can't free screen (0) or invalid handles
        }
        self.images.remove(&handle);
        Ok(())
    }

    fn put_image(
        &mut self,
        dest_x1: i32,
        dest_y1: i32,
        dest_x2: i32,
        dest_y2: i32,
        src_handle: i32,
        dest_handle: i32,
    ) -> Result<(), GraphicsError> {
        // Get source dimensions
        let (src_w, src_h) = if src_handle == 0 {
            (self.width as i32 - 1, self.height as i32 - 1)
        } else if let Some(img) = self.images.get(&src_handle) {
            (img.width as i32 - 1, img.height as i32 - 1)
        } else {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                "Invalid source handle",
            ));
        };

        self.copy_pixels(
            src_handle,
            0,
            0,
            src_w,
            src_h,
            dest_handle,
            dest_x1,
            dest_y1,
            dest_x2,
            dest_y2,
        )
    }

    fn put_image_full(
        &mut self,
        dest_x1: i32,
        dest_y1: i32,
        dest_x2: i32,
        dest_y2: i32,
        src_handle: i32,
        dest_handle: i32,
        src_x1: i32,
        src_y1: i32,
        src_x2: i32,
        src_y2: i32,
    ) -> Result<(), GraphicsError> {
        self.copy_pixels(
            src_handle,
            src_x1,
            src_y1,
            src_x2,
            src_y2,
            dest_handle,
            dest_x1,
            dest_y1,
            dest_x2,
            dest_y2,
        )
    }

    fn set_source(&mut self, handle: i32) -> Result<(), GraphicsError> {
        if handle != 0 && !self.images.contains_key(&handle) {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                "Invalid source handle",
            ));
        }
        self.source_handle = handle;
        Ok(())
    }

    fn set_dest(&mut self, handle: i32) -> Result<(), GraphicsError> {
        if handle != 0 && !self.images.contains_key(&handle) {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                "Invalid destination handle",
            ));
        }
        self.dest_handle = handle;
        Ok(())
    }

    fn copy_image(&mut self, handle: i32, mode: i32) -> i32 {
        let (width, height, pixels, blend_enabled, clear_color) = if handle == 0 {
            (
                self.width,
                self.height,
                self.pixel_buffer.clone(),
                self.screen_blend_enabled,
                self.screen_clear_color,
            )
        } else if let Some(img) = self.images.get(&handle) {
            (
                img.width,
                img.height,
                img.pixels.clone(),
                img.blend_enabled,
                img.clear_color,
            )
        } else {
            return -1;
        };

        let new_handle = self.next_handle;
        self.next_handle += 1;

        let img = ImageBuffer {
            width,
            height,
            pixels,
            mode,
            blend_enabled,
            clear_color,
        };
        self.images.insert(new_handle, img);

        new_handle
    }

    fn screen_image(&mut self, x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
        let w = (x2 - x1 + 1).max(1) as u32;
        let h = (y2 - y1 + 1).max(1) as u32;

        let handle = self.next_handle;
        self.next_handle += 1;

        let mut pixels = Vec::with_capacity((w * h) as usize);
        for y in y1..=y2 {
            for x in x1..=x2 {
                let color = self.get_pixel_buffer(x, y).unwrap_or(self.bg_color);
                pixels.push(color);
            }
        }

        let img = ImageBuffer {
            width: w,
            height: h,
            pixels,
            mode: 32,
            blend_enabled: true,
            clear_color: None,
        };
        self.images.insert(handle, img);

        handle
    }

    fn print_string(&mut self, x: i32, y: i32, text: &str) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        let mut px = x;
        for ch in text.bytes() {
            self.draw_char(ch, px, y, self.fg_color, self.bg_color);
            px += FONT_WIDTH as i32;
        }

        Ok(())
    }

    fn set_autodisplay(&mut self, enabled: bool) -> Result<(), GraphicsError> {
        self.autodisplay = enabled;
        Ok(())
    }

    fn get_image_width(&self, handle: i32) -> i32 {
        if handle == 0 {
            self.width as i32
        } else {
            self.images
                .get(&handle)
                .map(|i| i.width as i32)
                .unwrap_or(0)
        }
    }

    fn get_image_height(&self, handle: i32) -> i32 {
        if handle == 0 {
            self.height as i32
        } else {
            self.images
                .get(&handle)
                .map(|i| i.height as i32)
                .unwrap_or(0)
        }
    }

    // ========================================================================
    // Mouse Input
    // ========================================================================

    fn get_mouse_x(&self) -> i32 {
        self.mouse_x
    }

    fn get_mouse_y(&self) -> i32 {
        self.mouse_y
    }

    fn get_mouse_button(&self, button: u32) -> bool {
        match button {
            1 => self.mouse_buttons[0],
            2 => self.mouse_buttons[1],
            3 => self.mouse_buttons[2],
            _ => false,
        }
    }

    fn poll_mouse_input(&mut self) -> bool {
        let had_input = self.mouse_input_available;
        self.mouse_input_available = false;
        self.mouse_move_x = 0;
        self.mouse_move_y = 0;
        self.mouse_wheel = 0;
        had_input
    }

    fn get_mouse_movement_x(&self) -> i32 {
        self.mouse_move_x
    }

    fn get_mouse_movement_y(&self) -> i32 {
        self.mouse_move_y
    }

    fn get_mouse_wheel(&self) -> i32 {
        self.mouse_wheel
    }

    fn hide_mouse(&mut self) {
        if self.initialized {
            self.sdl_context
                .as_ref()
                .map(|ctx| ctx.mouse().show_cursor(false));
        }
    }

    fn show_mouse(&mut self) {
        if self.initialized {
            self.sdl_context
                .as_ref()
                .map(|ctx| ctx.mouse().show_cursor(true));
        }
    }

    fn move_mouse(&mut self, x: i32, y: i32) {
        if self.initialized {
            if let Some(ctx) = self.sdl_context.as_ref() {
                ctx.mouse().warp_mouse_in_window(
                    self.canvas.as_ref().map(|c| c.window()).unwrap(),
                    x,
                    y,
                );
            }
            self.mouse_x = x;
            self.mouse_y = y;
        }
    }

    // ========================================================================
    // Window Control Implementation
    // ========================================================================

    fn set_fullscreen(&mut self, mode: i32) -> i32 {
        let previous = self.fullscreen_mode;

        if let Some(canvas) = self.canvas.as_mut() {
            let fullscreen_type = match mode {
                0 => FullscreenType::Off,
                1 => FullscreenType::True,
                _ => FullscreenType::Desktop, // mode 2 or _SQUAREPIXELS
            };

            if canvas.window_mut().set_fullscreen(fullscreen_type).is_ok() {
                self.fullscreen_mode = mode.clamp(0, 2);
            }
        }

        previous
    }

    fn get_fullscreen(&self) -> i32 {
        self.fullscreen_mode
    }

    fn screen_move(&mut self, x: i32, y: i32) {
        if let Some(canvas) = self.canvas.as_mut() {
            canvas.window_mut().set_position(
                sdl2::video::WindowPos::Positioned(x),
                sdl2::video::WindowPos::Positioned(y),
            );
        }
    }

    fn screen_show(&mut self) {
        if let Some(canvas) = self.canvas.as_mut() {
            canvas.window_mut().show();
            self.screen_visible = true;
        }
    }

    fn screen_hide(&mut self) {
        if let Some(canvas) = self.canvas.as_mut() {
            canvas.window_mut().hide();
            self.screen_visible = false;
        }
    }

    fn is_screen_visible(&self) -> bool {
        self.screen_visible
    }

    // ========================================================================
    // Alpha Blending Implementation
    // ========================================================================

    fn set_blend(&mut self, handle: i32) {
        if handle == 0 {
            self.screen_blend_enabled = true;
        } else if let Some(img) = self.images.get_mut(&handle) {
            img.blend_enabled = true;
        }
    }

    fn set_dontblend(&mut self, handle: i32) {
        if handle == 0 {
            self.screen_blend_enabled = false;
        } else if let Some(img) = self.images.get_mut(&handle) {
            img.blend_enabled = false;
        }
    }

    fn get_blend(&self, handle: i32) -> bool {
        if handle == 0 {
            self.screen_blend_enabled
        } else {
            self.images
                .get(&handle)
                .map(|img| img.blend_enabled)
                .unwrap_or(true)
        }
    }

    fn set_clearcolor(&mut self, color: u32, handle: i32) {
        if handle == 0 {
            self.screen_clear_color = Some(color);
        } else if let Some(img) = self.images.get_mut(&handle) {
            img.clear_color = Some(color);
        }
    }

    fn clear_clearcolor(&mut self, handle: i32) {
        if handle == 0 {
            self.screen_clear_color = None;
        } else if let Some(img) = self.images.get_mut(&handle) {
            img.clear_color = None;
        }
    }

    fn get_clearcolor(&self, handle: i32) -> i64 {
        let color = if handle == 0 {
            self.screen_clear_color
        } else {
            self.images.get(&handle).and_then(|img| img.clear_color)
        };

        match color {
            Some(c) => c as i64,
            None => -1, // No clear color set
        }
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
        let color = 0xFF112233;
        let sdl_color = SDL2Backend::argb_to_sdl_color(color);
        assert_eq!(sdl_color, Color::RGBA(0x11, 0x22, 0x33, 0xFF));
    }

    #[test]
    fn test_image_buffer() {
        let mut img = ImageBuffer::new(10, 10, 32, 0xFF000000);
        assert_eq!(img.get_pixel(0, 0), Some(0xFF000000));
        img.set_pixel(5, 5, 0xFFFF0000);
        assert_eq!(img.get_pixel(5, 5), Some(0xFFFF0000));
        assert_eq!(img.get_pixel(100, 100), None);
    }

    #[test]
    fn test_default_palette() {
        let palette = ColorPalette::default();
        assert_eq!(palette.colors[0], 0xFF000000); // Black
        assert_eq!(palette.colors[15], 0xFFFFFFFF); // White
    }

    #[test]
    fn test_world_coords_disabled() {
        let backend = SDL2Backend::new();
        let (x, y) = backend.world_to_screen(100.0, 200.0);
        assert_eq!((x, y), (100, 200));
    }

    #[test]
    fn test_mouse_state_initial() {
        let backend = SDL2Backend::new();
        assert_eq!(backend.get_mouse_x(), 0);
        assert_eq!(backend.get_mouse_y(), 0);
        assert!(!backend.get_mouse_button(1));
        assert!(!backend.get_mouse_button(2));
        assert!(!backend.get_mouse_button(3));
    }
}
