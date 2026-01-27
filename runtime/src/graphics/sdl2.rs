//! SDL2 graphics backend implementation.
//!
//! This is the primary graphics backend for QB64Fresh.
//! It uses the `sdl2` crate to provide cross-platform graphics support.

use super::font::{get_char_bitmap, is_pixel_set, FONT_HEIGHT, FONT_WIDTH};
use super::{GraphicsBackend, GraphicsError, GraphicsErrorKind};
use sdl2::event::Event;
use sdl2::keyboard::Scancode;
use sdl2::mouse::MouseButton;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::{Point, Rect};
use sdl2::render::{Canvas, Texture, TextureCreator};
use sdl2::surface::Surface;
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
    palette: [u32; 256],      // Per-image palette (for 256-color modes)
}

impl ImageBuffer {
    fn new(width: u32, height: u32, mode: i32, fill_color: u32) -> Self {
        let pixels = vec![fill_color; (width * height) as usize];
        // Initialize with default EGA/VGA palette
        let palette = ColorPalette::default().colors;
        Self {
            width,
            height,
            pixels,
            mode,
            blend_enabled: true, // Alpha blending enabled by default
            clear_color: None,   // No transparency key by default
            palette,
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
    /// Screen page buffers (handle 0) - multiple pages for page flipping
    /// Classic modes support 2-4 pages depending on resolution/memory
    page_buffers: Vec<Vec<u32>>,
    /// Active page - drawing operations target this page
    active_page: usize,
    /// Visual page - this page is displayed on screen
    visual_page: usize,
    /// Maximum number of pages for current mode
    max_pages: usize,
    /// Persistent GPU textures for each page (hardware acceleration)
    /// When dirty, the CPU buffer is uploaded to the texture in display()
    page_textures: Vec<Option<Texture>>,
    /// Dirty flags - true if page needs texture upload before display
    page_dirty: Vec<bool>,
    /// Texture creator for creating persistent textures
    /// Uses unsafe_textures feature for 'static lifetime simplification
    texture_creator: Option<TextureCreator<sdl2::video::WindowContext>>,
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
    // Display layer ordering
    /// Display order for layers: [layer1, layer2, layer3, layer4]
    /// Constants: _SOFTWARE=1, _HARDWARE=2, _HARDWARE1=3, _GLRENDER=4
    display_order: [i32; 4],
    // Screen palette (for handle 0)
    /// Palette for the main screen buffer
    screen_palette: [u32; 256],
    // Keyboard state
    /// Current keyboard state (scancode -> pressed)
    /// Updated in poll_events using SDL_GetKeyboardState
    keyboard_state: HashMap<Scancode, bool>,
    // Icon state
    /// Current window icon handle (0 = no icon set)
    current_icon_handle: i32,
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
            page_buffers: Vec::new(),
            active_page: 0,
            visual_page: 0,
            max_pages: 4, // Default to 4 pages
            page_textures: Vec::new(),
            page_dirty: Vec::new(),
            texture_creator: None,
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
            // Display order: default is SOFTWARE at back, then HARDWARE layers
            display_order: [1, 2, 3, 4], // _SOFTWARE, _HARDWARE, _HARDWARE1, _GLRENDER
            // Screen palette
            screen_palette: ColorPalette::default().colors,
            // Keyboard state
            keyboard_state: HashMap::new(),
            // Icon state
            current_icon_handle: 0, // No icon set initially
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

    /// Scroll the text console up by one line.
    ///
    /// This shifts all pixel rows up by `FONT_HEIGHT` pixels and clears the bottom
    /// line with the background color. Used when text output would exceed the
    /// bottom of the screen.
    ///
    /// # Implementation
    ///
    /// The page buffer is stored as a flat array: `pixels[y * width + x]`.
    /// To scroll:
    /// 1. Copy rows `char_height..height` to rows `0..(height - char_height)`
    /// 2. Clear the bottom `char_height` rows with background color
    fn scroll_text_up(&mut self) {
        let char_height = FONT_HEIGHT as usize;
        let width = self.width as usize;
        let height = self.height as usize;

        if char_height >= height {
            // Screen is too small to scroll, just clear it
            if let Some(page) = self.page_buffers.get_mut(self.active_page) {
                page.fill(self.bg_color);
            }
            return;
        }

        if let Some(page) = self.page_buffers.get_mut(self.active_page) {
            // Shift all rows up by char_height pixels
            // Copy from row char_height to row 0, row char_height+1 to row 1, etc.
            // IMPORTANT: Copy in reverse order (bottom to top) to avoid overwriting
            // data that hasn't been copied yet when ranges overlap.
            for src_y in (char_height..height).rev() {
                let dst_y = src_y - char_height;
                let src_start = src_y * width;
                let dst_start = dst_y * width;
                if src_start + width <= page.len() && dst_start + width <= page.len() {
                    page.copy_within(src_start..(src_start + width), dst_start);
                }
            }

            // Clear the bottom char_height rows with background color
            let clear_start = (height - char_height) * width;
            let clear_end = height * width;
            if clear_start < page.len() && clear_end <= page.len() {
                page[clear_start..clear_end].fill(self.bg_color);
            }
        }

        // Mark page as dirty for deferred texture upload in display()
        if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
            *dirty = true;
        }
    }

    fn set_pixel_buffer(&mut self, x: i32, y: i32, color: u32) {
        if self.dest_handle == 0 {
            // Drawing to screen (active page)
            if let Some(idx) = self.pixel_index(x, y) {
                if let Some(page) = self.page_buffers.get_mut(self.active_page) {
                    if idx < page.len() {
                        page[idx] = color;
                    }
                }
            }
        } else {
            // Drawing to image buffer
            if let Some(img) = self.images.get_mut(&self.dest_handle) {
                img.set_pixel(x, y, color);
            }
        }
    }

    /// Set a pixel with alpha blending support.
    ///
    /// When blending is enabled for the destination:
    /// - Fully transparent pixels (alpha=0) are skipped
    /// - Semi-transparent pixels (0 < alpha < 255) are blended with destination
    /// - Fully opaque pixels (alpha=255) are written directly
    ///
    /// When blending is disabled, pixels are written directly regardless of alpha.
    fn set_pixel_blended(&mut self, x: i32, y: i32, color: u32) {
        let src_alpha = (color >> 24) & 0xFF;

        // Check if blending is enabled for the destination
        let blend_enabled = if self.dest_handle == 0 {
            self.screen_blend_enabled
        } else {
            self.images
                .get(&self.dest_handle)
                .map(|img| img.blend_enabled)
                .unwrap_or(true)
        };

        if blend_enabled {
            if src_alpha == 0 {
                // Fully transparent - skip this pixel
                return;
            } else if src_alpha < 255 {
                // Semi-transparent - blend with destination
                if let Some(dest_color) = self.get_pixel_buffer_dest(x, y) {
                    let blended = Self::blend_colors(color, dest_color);
                    self.set_pixel_buffer(x, y, blended);
                    return;
                }
            }
        }

        // Fully opaque or blending disabled - direct write
        self.set_pixel_buffer(x, y, color);
    }

    /// Get a pixel from the destination buffer (for blending).
    ///
    /// Unlike `get_pixel_buffer()` which reads from the source, this reads
    /// from the current destination (for alpha compositing).
    fn get_pixel_buffer_dest(&self, x: i32, y: i32) -> Option<u32> {
        if self.dest_handle == 0 {
            // Reading from screen (active page)
            self.pixel_index(x, y).and_then(|idx| {
                self.page_buffers
                    .get(self.active_page)
                    .and_then(|page| page.get(idx).copied())
            })
        } else {
            // Reading from image buffer
            self.images
                .get(&self.dest_handle)
                .and_then(|img| img.get_pixel(x, y))
        }
    }

    /// Compute the final color after alpha blending (if enabled).
    ///
    /// Returns:
    /// - `Some(color)` - The final color to draw (either blended or original)
    /// - `None` - Skip this pixel (fully transparent with blending enabled)
    fn compute_blended_color(&self, x: i32, y: i32, color: u32) -> Option<u32> {
        let src_alpha = (color >> 24) & 0xFF;

        // Check if blending is enabled for the destination
        let blend_enabled = if self.dest_handle == 0 {
            self.screen_blend_enabled
        } else {
            self.images
                .get(&self.dest_handle)
                .map(|img| img.blend_enabled)
                .unwrap_or(true)
        };

        if blend_enabled {
            if src_alpha == 0 {
                // Fully transparent - skip this pixel
                return None;
            } else if src_alpha < 255 {
                // Semi-transparent - blend with destination
                if let Some(dest_color) = self.get_pixel_buffer_dest(x, y) {
                    return Some(Self::blend_colors(color, dest_color));
                }
            }
        }

        // Fully opaque or blending disabled - use original color
        Some(color)
    }

    fn get_pixel_buffer(&self, x: i32, y: i32) -> Option<u32> {
        if self.source_handle == 0 {
            // Reading from screen (active page)
            self.pixel_index(x, y).and_then(|idx| {
                self.page_buffers
                    .get(self.active_page)
                    .and_then(|page| page.get(idx).copied())
            })
        } else {
            // Reading from image buffer
            self.images
                .get(&self.source_handle)
                .and_then(|img| img.get_pixel(x, y))
        }
    }

    /// Sample a pixel using nearest neighbor (point) sampling.
    ///
    /// Used by `_MAPTRIANGLE` for fast texture sampling without filtering.
    fn sample_nearest(pixels: &[u32], width: i32, height: i32, x: f32, y: f32) -> u32 {
        let ix = x.round() as i32;
        let iy = y.round() as i32;

        if ix >= 0 && iy >= 0 && ix < width && iy < height {
            let idx = (iy * width + ix) as usize;
            if idx < pixels.len() {
                return pixels[idx];
            }
        }
        0 // Transparent black for out-of-bounds
    }

    /// Sample a pixel using bilinear interpolation.
    ///
    /// Used by `_MAPTRIANGLE` with the `_SMOOTH` option for higher quality
    /// texture mapping that reduces aliasing artifacts.
    fn sample_bilinear(pixels: &[u32], width: i32, height: i32, x: f32, y: f32) -> u32 {
        let x0 = x.floor() as i32;
        let y0 = y.floor() as i32;
        let x1 = x0 + 1;
        let y1 = y0 + 1;

        // Fractional parts for interpolation weights
        let fx = x - x0 as f32;
        let fy = y - y0 as f32;

        // Sample 4 neighboring pixels
        let c00 = Self::get_pixel_safe(pixels, width, height, x0, y0);
        let c10 = Self::get_pixel_safe(pixels, width, height, x1, y0);
        let c01 = Self::get_pixel_safe(pixels, width, height, x0, y1);
        let c11 = Self::get_pixel_safe(pixels, width, height, x1, y1);

        // Bilinear interpolation for each channel
        let lerp_channel = |c00: u32, c10: u32, c01: u32, c11: u32, shift: u32| -> u8 {
            let v00 = ((c00 >> shift) & 0xFF) as f32;
            let v10 = ((c10 >> shift) & 0xFF) as f32;
            let v01 = ((c01 >> shift) & 0xFF) as f32;
            let v11 = ((c11 >> shift) & 0xFF) as f32;

            let top = v00 * (1.0 - fx) + v10 * fx;
            let bottom = v01 * (1.0 - fx) + v11 * fx;
            let result = top * (1.0 - fy) + bottom * fy;
            result.clamp(0.0, 255.0) as u8
        };

        let a = lerp_channel(c00, c10, c01, c11, 24);
        let r = lerp_channel(c00, c10, c01, c11, 16);
        let g = lerp_channel(c00, c10, c01, c11, 8);
        let b = lerp_channel(c00, c10, c01, c11, 0);

        ((a as u32) << 24) | ((r as u32) << 16) | ((g as u32) << 8) | (b as u32)
    }

    /// Get a pixel safely, returning transparent black for out-of-bounds coordinates.
    fn get_pixel_safe(pixels: &[u32], width: i32, height: i32, x: i32, y: i32) -> u32 {
        if x >= 0 && y >= 0 && x < width && y < height {
            let idx = (y * width + x) as usize;
            if idx < pixels.len() {
                return pixels[idx];
            }
        }
        0 // Transparent black
    }

    /// Draw a single character at pixel coordinates using the embedded font
    fn draw_char(&mut self, ch: u8, px: i32, py: i32, fg_color: u32, bg_color: u32) {
        let bitmap = get_char_bitmap(ch);
        let width = self.width;
        let height = self.height;

        // Update pixel buffer only - canvas update deferred to display()
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

                // Update pixel buffer (active page)
                if let Some(idx) = self.pixel_index(x, y) {
                    if let Some(page) = self.page_buffers.get_mut(self.active_page) {
                        if idx < page.len() {
                            page[idx] = color;
                        }
                    }
                }
            }
        }

        // Mark page as dirty for deferred texture upload in display()
        if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
            *dirty = true;
        }
    }

    /// Draw a FreeType glyph with alpha blending.
    ///
    /// This method blends the glyph's alpha values with the foreground color
    /// onto the pixel buffer, providing anti-aliased text rendering.
    #[cfg(feature = "freetype")]
    fn draw_glyph_alpha(
        &mut self,
        x: i32,
        y: i32,
        glyph: &crate::font_manager::GlyphBitmap,
        fg_color: u32,
    ) {
        let width = self.width;
        let height = self.height;

        // Empty glyphs (like space) have no bitmap data
        if glyph.width == 0 || glyph.height == 0 {
            return;
        }

        // Extract foreground RGB components
        let fg_r = ((fg_color >> 16) & 0xFF) as u8;
        let fg_g = ((fg_color >> 8) & 0xFF) as u8;
        let fg_b = (fg_color & 0xFF) as u8;

        // Update pixel buffer with alpha blending (canvas update deferred to display())
        for gy in 0..glyph.height {
            for gx in 0..glyph.width {
                let px = x + gx as i32;
                let py = y + gy as i32;

                if px < 0 || py < 0 || px >= width as i32 || py >= height as i32 {
                    continue;
                }

                let alpha_idx = (gy * glyph.width + gx) as usize;
                if alpha_idx >= glyph.data.len() {
                    continue;
                }

                let alpha = glyph.data[alpha_idx];
                if alpha == 0 {
                    continue; // Fully transparent, skip
                }

                if let Some(idx) = self.pixel_index(px, py) {
                    if let Some(page) = self.page_buffers.get_mut(self.active_page) {
                        if idx < page.len() {
                            let bg = page[idx];
                            let blended = Self::blend_alpha(fg_r, fg_g, fg_b, alpha, bg);
                            page[idx] = blended;
                        }
                    }
                }
            }
        }

        // Mark page as dirty for deferred texture upload in display()
        if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
            *dirty = true;
        }
    }

    /// Blend a foreground color with an alpha value onto a background color.
    #[cfg(feature = "freetype")]
    fn blend_alpha(fg_r: u8, fg_g: u8, fg_b: u8, alpha: u8, bg: u32) -> u32 {
        let bg_r = ((bg >> 16) & 0xFF) as u8;
        let bg_g = ((bg >> 8) & 0xFF) as u8;
        let bg_b = (bg & 0xFF) as u8;
        let bg_a = ((bg >> 24) & 0xFF) as u8;

        let alpha_f = alpha as f32 / 255.0;
        let inv_alpha = 1.0 - alpha_f;

        let r = (fg_r as f32 * alpha_f + bg_r as f32 * inv_alpha) as u8;
        let g = (fg_g as f32 * alpha_f + bg_g as f32 * inv_alpha) as u8;
        let b = (fg_b as f32 * alpha_f + bg_b as f32 * inv_alpha) as u8;

        // Keep background alpha or set to fully opaque
        let a = if bg_a == 0 { 255 } else { bg_a };

        ((a as u32) << 24) | ((r as u32) << 16) | ((g as u32) << 8) | (b as u32)
    }

    fn draw_circle_outline(&mut self, cx: i32, cy: i32, radius: i32, color: u32) {
        if radius <= 0 {
            return;
        }

        // Midpoint circle algorithm - write to pixel buffer only
        let mut x = radius;
        let mut y = 0;
        let mut p = 1 - radius;

        // Initial 4 points
        self.set_pixel_buffer(cx + x, cy, color);
        self.set_pixel_buffer(cx - x, cy, color);
        self.set_pixel_buffer(cx, cy + x, color);
        self.set_pixel_buffer(cx, cy - x, color);

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

            // Draw 8 octant points
            self.set_pixel_buffer(cx + x, cy + y, color);
            self.set_pixel_buffer(cx - x, cy + y, color);
            self.set_pixel_buffer(cx + x, cy - y, color);
            self.set_pixel_buffer(cx - x, cy - y, color);
            self.set_pixel_buffer(cx + y, cy + x, color);
            self.set_pixel_buffer(cx - y, cy + x, color);
            self.set_pixel_buffer(cx + y, cy - x, color);
            self.set_pixel_buffer(cx - y, cy - x, color);
        }

        // Mark page as dirty for deferred texture upload in display()
        if self.dest_handle == 0 {
            if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
                *dirty = true;
            }
        }
    }

    fn draw_circle_filled(&mut self, cx: i32, cy: i32, radius: i32, color: u32) {
        if radius <= 0 {
            return;
        }

        // Midpoint circle algorithm with horizontal line fills
        // Uses set_pixel_blended() to support alpha blending when enabled
        let mut x = radius;
        let mut y = 0;
        let mut p = 1 - radius;

        // Helper to draw a horizontal line with blending support
        let draw_hline = |slf: &mut Self, x1: i32, x2: i32, y: i32| {
            for px in x1..=x2 {
                slf.set_pixel_blended(px, y, color);
            }
        };

        // Initial horizontal line through center
        draw_hline(self, cx - x, cx + x, cy);

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

            // Fill horizontal spans for each octant pair
            draw_hline(self, cx - x, cx + x, cy + y);
            draw_hline(self, cx - x, cx + x, cy - y);
            if x != y {
                draw_hline(self, cx - y, cx + y, cy + x);
                draw_hline(self, cx - y, cx + y, cy - x);
            }
        }

        // Mark page as dirty for deferred texture upload in display()
        if self.dest_handle == 0 {
            if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
                *dirty = true;
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
        let mut filled = false;

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
            filled = true;

            stack.push((x + 1, y));
            stack.push((x - 1, y));
            stack.push((x, y + 1));
            stack.push((x, y - 1));
        }

        // Mark page as dirty for deferred texture upload in display()
        if filled && self.dest_handle == 0 {
            if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
                *dirty = true;
            }
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
                            false,
                            None,
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
        // When source is screen (handle 0), read from visual page
        let (src_w, src_h, src_pixels, src_blend, src_clear_color) = if src_handle == 0 {
            let page_pixels = self
                .page_buffers
                .get(self.visual_page)
                .cloned()
                .unwrap_or_default();
            (
                self.width,
                self.height,
                page_pixels,
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

                        // Write to destination (pixel buffer only, canvas updated in display())
                        if dest_handle == 0 {
                            self.set_pixel_buffer(dx, dy, final_color);
                        } else if let Some(img) = self.images.get_mut(&dest_handle) {
                            img.set_pixel(dx, dy, final_color);
                        }
                    }
                }
            }
        }

        // Mark page as dirty for deferred texture upload in display()
        if dest_handle == 0 {
            if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
                *dirty = true;
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

    /// Draw a line segment with optional style pattern.
    ///
    /// # Arguments
    /// - `sx1`, `sy1`: Start screen coordinates
    /// - `sx2`, `sy2`: End screen coordinates
    /// - `color`: Line color
    /// - `style`: Optional 16-bit style pattern (bit 15 = first pixel, bit 0 = last)
    pub(crate) fn draw_line_with_style(
        &mut self,
        sx1: i32,
        sy1: i32,
        sx2: i32,
        sy2: i32,
        color: u32,
        style: Option<u16>,
    ) {
        let dx = (sx2 - sx1).abs();
        let dy = (sy2 - sy1).abs();
        let sxd = if sx1 < sx2 { 1 } else { -1 };
        let syd = if sy1 < sy2 { 1 } else { -1 };
        let mut err = dx - dy;
        let mut x = sx1;
        let mut y = sy1;
        let mut pixel_index = 0u32;

        loop {
            // Apply style pattern if provided
            let should_draw = if let Some(pattern) = style {
                // Pattern repeats every 16 pixels
                // Bit 15 (MSB) is first pixel, bit 0 (LSB) is last
                let bit_pos = 15 - ((pixel_index % 16) as usize);
                (pattern >> bit_pos) & 1 != 0
            } else {
                true // No style pattern - draw all pixels
            };

            if should_draw {
                self.set_pixel_blended(x, y, color);
            }

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
            pixel_index += 1;
        }
    }
}

impl Default for SDL2Backend {
    fn default() -> Self {
        Self::new()
    }
}

impl SDL2Backend {
    /// Map QB64 keycode to SDL2 scancode (helper method).
    ///
    /// QB64 keycode system:
    /// - ASCII values (0-127): map directly to ASCII scancodes
    /// - Extended keys: negative values `-(ext + 256)` where ext is the extended key code
    /// - Special modifier keys: specific codes (100305, 100306, etc.)
    ///
    /// # Arguments
    /// - `qb64_keycode`: QB64 keycode
    ///
    /// # Returns
    /// - `Some(Scancode)` if mapping found
    /// - `None` if keycode cannot be mapped
    fn map_qb64_to_scancode(qb64_keycode: i64) -> Option<Scancode> {
        // Handle ASCII keys (0-127)
        if qb64_keycode >= 0 && qb64_keycode <= 127 {
            // Map ASCII to scancode
            // For printable ASCII, we can use the keycode directly
            // SDL2 scancodes for ASCII are typically the same as the ASCII value
            // but we need to use the proper scancode enum
            match qb64_keycode {
                8 => Some(Scancode::Backspace),
                9 => Some(Scancode::Tab),
                13 => Some(Scancode::Return),
                27 => Some(Scancode::Escape),
                32 => Some(Scancode::Space),
                // Letters A-Z (case-insensitive in BASIC)
                65..=90 => {
                    // Map to lowercase scancode (SDL2 letter scancodes are lowercase)
                    let offset = (qb64_keycode - 65) as u8;
                    match offset {
                        0 => Some(Scancode::A),
                        1 => Some(Scancode::B),
                        2 => Some(Scancode::C),
                        3 => Some(Scancode::D),
                        4 => Some(Scancode::E),
                        5 => Some(Scancode::F),
                        6 => Some(Scancode::G),
                        7 => Some(Scancode::H),
                        8 => Some(Scancode::I),
                        9 => Some(Scancode::J),
                        10 => Some(Scancode::K),
                        11 => Some(Scancode::L),
                        12 => Some(Scancode::M),
                        13 => Some(Scancode::N),
                        14 => Some(Scancode::O),
                        15 => Some(Scancode::P),
                        16 => Some(Scancode::Q),
                        17 => Some(Scancode::R),
                        18 => Some(Scancode::S),
                        19 => Some(Scancode::T),
                        20 => Some(Scancode::U),
                        21 => Some(Scancode::V),
                        22 => Some(Scancode::W),
                        23 => Some(Scancode::X),
                        24 => Some(Scancode::Y),
                        25 => Some(Scancode::Z),
                        _ => None,
                    }
                }
                // Numbers 0-9
                48..=57 => match qb64_keycode {
                    48 => Some(Scancode::Num0),
                    49 => Some(Scancode::Num1),
                    50 => Some(Scancode::Num2),
                    51 => Some(Scancode::Num3),
                    52 => Some(Scancode::Num4),
                    53 => Some(Scancode::Num5),
                    54 => Some(Scancode::Num6),
                    55 => Some(Scancode::Num7),
                    56 => Some(Scancode::Num8),
                    57 => Some(Scancode::Num9),
                    _ => None,
                },
                _ => {
                    // For other ASCII, try to find by character
                    // This is a simplified mapping - full implementation would need
                    // a complete ASCII to scancode table
                    None
                }
            }
        }
        // Handle extended keys (negative values: -(ext + 256))
        else if qb64_keycode < 0 {
            let ext_code = (-qb64_keycode) - 256;
            match ext_code {
                // Arrow keys
                72 => Some(Scancode::Up),    // Up arrow
                80 => Some(Scancode::Down),  // Down arrow
                75 => Some(Scancode::Left),  // Left arrow
                77 => Some(Scancode::Right), // Right arrow
                // Function keys F1-F12
                59 => Some(Scancode::F1),
                60 => Some(Scancode::F2),
                61 => Some(Scancode::F3),
                62 => Some(Scancode::F4),
                63 => Some(Scancode::F5),
                64 => Some(Scancode::F6),
                65 => Some(Scancode::F7),
                66 => Some(Scancode::F8),
                67 => Some(Scancode::F9),
                68 => Some(Scancode::F10),
                133 => Some(Scancode::F11), // F11
                134 => Some(Scancode::F12), // F12
                // Other extended keys
                82 => Some(Scancode::Insert),
                83 => Some(Scancode::Delete),
                71 => Some(Scancode::Home),
                79 => Some(Scancode::End),
                73 => Some(Scancode::PageUp),
                81 => Some(Scancode::PageDown),
                _ => None,
            }
        }
        // Handle special modifier keys (100xxx range)
        else if qb64_keycode >= 100000 {
            match qb64_keycode {
                100303 => Some(Scancode::RShift), // _KEY_RSHIFT
                100304 => Some(Scancode::LShift), // _KEY_LSHIFT
                100305 => Some(Scancode::RCtrl),  // _KEY_RCTRL
                100306 => Some(Scancode::LCtrl),  // _KEY_LCTRL
                100307 => Some(Scancode::RAlt),   // _KEY_RALT
                100308 => Some(Scancode::LAlt),   // _KEY_LALT
                100309 => Some(Scancode::RGui),   // _KEY_RAPPLE
                100310 => Some(Scancode::LGui),   // _KEY_LAPPLE
                _ => None,
            }
        }
        // Handle arrow keys (direct codes like 18432, 20480)
        else {
            match qb64_keycode {
                18432 => Some(Scancode::Up),       // _KEY_UP
                20480 => Some(Scancode::Down),     // _KEY_DOWN
                19200 => Some(Scancode::Left),     // _KEY_LEFT
                19712 => Some(Scancode::Right),    // _KEY_RIGHT
                20992 => Some(Scancode::Insert),   // _KEY_INSERT
                21248 => Some(Scancode::Delete),   // _KEY_DELETE
                18176 => Some(Scancode::Home),     // _KEY_HOME
                20224 => Some(Scancode::End),      // _KEY_END
                18688 => Some(Scancode::PageUp),   // _KEY_PAGEUP
                20736 => Some(Scancode::PageDown), // _KEY_PAGEDOWN
                // Function keys
                15104 => Some(Scancode::F1),
                15360 => Some(Scancode::F2),
                15616 => Some(Scancode::F3),
                15872 => Some(Scancode::F4),
                16128 => Some(Scancode::F5),
                16384 => Some(Scancode::F6),
                16640 => Some(Scancode::F7),
                16896 => Some(Scancode::F8),
                17152 => Some(Scancode::F9),
                17408 => Some(Scancode::F10),
                34048 => Some(Scancode::F11),
                34304 => Some(Scancode::F12),
                _ => None,
            }
        }
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

        // Create texture creator for persistent GPU textures (hardware acceleration)
        // Uses unsafe_textures feature for 'static lifetime simplification
        let texture_creator = canvas.texture_creator();

        // Create persistent streaming textures for each page
        // These textures remain allocated and are only updated when dirty
        let page_textures: Vec<Option<Texture>> = (0..self.max_pages)
            .map(|_| {
                texture_creator
                    .create_texture_streaming(PixelFormatEnum::ARGB8888, width, height)
                    .ok()
            })
            .collect();

        // Initialize dirty flags - all pages dirty initially to force first upload
        let page_dirty = vec![true; self.max_pages];

        let event_pump = sdl_context.event_pump().map_err(|e| {
            GraphicsError::new(
                GraphicsErrorKind::BackendError,
                format!("SDL2 event pump creation failed: {}", e),
            )
        })?;

        // Initialize page buffers (4 pages for classic modes)
        // Each page is a full-screen pixel buffer
        let page_size = (width * height) as usize;
        let page_buffers: Vec<Vec<u32>> = (0..self.max_pages)
            .map(|_| vec![self.bg_color; page_size])
            .collect();

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
        self.texture_creator = Some(texture_creator);
        self.page_textures = page_textures;
        self.page_dirty = page_dirty;
        self.initialized = true;
        self.width = width;
        self.height = height;
        self.page_buffers = page_buffers;
        self.active_page = 0;
        self.visual_page = 0;

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
        // Clear textures before texture_creator (textures depend on it)
        self.page_textures.clear();
        self.page_dirty.clear();
        self.texture_creator = None;
        self.canvas = None;
        self.sdl_context = None;
        self.page_buffers.clear();
        self.active_page = 0;
        self.visual_page = 0;
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

        // Clear the active page buffer
        if let Some(page) = self.page_buffers.get_mut(self.active_page) {
            page.fill(self.bg_color);
        }

        // Mark page as dirty for deferred texture upload in display()
        if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
            *dirty = true;
        }

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
                // Scroll if cursor exceeds bottom row
                if self.cursor_row > rows {
                    self.scroll_text_up();
                    self.cursor_row = rows;
                }
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
                // Scroll if cursor exceeds bottom row after wrapping
                if self.cursor_row > rows {
                    self.scroll_text_up();
                    self.cursor_row = rows;
                }
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

        // Apply alpha blending if enabled
        let final_color = self.compute_blended_color(sx, sy, color);

        // Skip fully transparent pixels when blending is enabled
        let final_color = match final_color {
            Some(c) => c,
            None => return Ok(()),
        };

        self.set_pixel_buffer(sx, sy, final_color);

        // Mark page as dirty for deferred texture upload in display()
        if self.dest_handle == 0 {
            if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
                *dirty = true;
            }
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

    fn get_last_position(&self) -> (i32, i32) {
        (self.last_gfx_x, self.last_gfx_y)
    }

    fn line(
        &mut self,
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
        color: u32,
        filled: bool,
        is_box: bool,
        style: Option<u16>,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        // Transform coordinates
        let (sx1, sy1) = self.world_to_screen(x1 as f64, y1 as f64);
        let (sx2, sy2) = self.world_to_screen(x2 as f64, y2 as f64);

        if filled {
            // Filled boxes ignore style pattern
            // Draw filled rectangle (box) to pixel buffer only
            let x = sx1.min(sx2);
            let y = sy1.min(sy2);
            // Use saturating cast to prevent overflow when converting from u32 to i32
            let w = (sx1 - sx2).unsigned_abs().min(i32::MAX as u32) as i32;
            let h = (sy1 - sy2).unsigned_abs().min(i32::MAX as u32) as i32;

            // Update pixel buffer with blending support
            for py in y..(y + h.max(1)) {
                for px in x..(x + w.max(1)) {
                    self.set_pixel_blended(px, py, color);
                }
            }
        } else if is_box {
            // Box outline - draw 4 edges
            let x_min = sx1.min(sx2);
            let x_max = sx1.max(sx2);
            let y_min = sy1.min(sy2);
            let y_max = sy1.max(sy2);

            // Top edge: (x_min, y_min) to (x_max, y_min)
            self.draw_line_with_style(x_min, y_min, x_max, y_min, color, style);
            // Right edge: (x_max, y_min) to (x_max, y_max)
            self.draw_line_with_style(x_max, y_min, x_max, y_max, color, style);
            // Bottom edge: (x_max, y_max) to (x_min, y_max)
            self.draw_line_with_style(x_max, y_max, x_min, y_max, color, style);
            // Left edge: (x_min, y_max) to (x_min, y_min)
            self.draw_line_with_style(x_min, y_max, x_min, y_min, color, style);
        } else {
            // Plain line - single line from (x1,y1) to (x2,y2)
            self.draw_line_with_style(sx1, sy1, sx2, sy2, color, style);
        }

        // Mark page as dirty for deferred texture upload in display()
        if self.dest_handle == 0 {
            if let Some(dirty) = self.page_dirty.get_mut(self.active_page) {
                *dirty = true;
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
        is_box: bool,
        step1: bool,
        step2: bool,
        style: Option<u16>,
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

        self.line(
            final_x1, final_y1, final_x2, final_y2, color, filled, is_box, style,
        )
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
            // draw_circle_filled() now uses set_pixel_blended() internally,
            // so no redundant drawing is needed
            self.draw_circle_filled(cx, cy, radius, color);
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

        let vp = self.visual_page;
        let width = self.width as usize;
        let height = self.height as usize;

        // Upload dirty visual page to its persistent texture
        // This replaces per-pixel canvas.draw_point() calls with a single bulk upload
        if self.page_dirty.get(vp).copied().unwrap_or(false) {
            if let Some(page) = self.page_buffers.get(vp) {
                if let Some(Some(texture)) = self.page_textures.get_mut(vp) {
                    // Bulk copy pixel buffer to GPU texture
                    let _ = texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
                        for y in 0..height {
                            for x in 0..width {
                                let pixel = page[y * width + x];
                                let offset = y * pitch + x * 4;
                                // ARGB8888 format (SDL expects BGRA byte order)
                                buffer[offset] = (pixel & 0xFF) as u8; // B
                                buffer[offset + 1] = ((pixel >> 8) & 0xFF) as u8; // G
                                buffer[offset + 2] = ((pixel >> 16) & 0xFF) as u8; // R
                                buffer[offset + 3] = ((pixel >> 24) & 0xFF) as u8;
                                // A
                            }
                        }
                    });
                    // Mark page as clean after upload
                    if let Some(dirty) = self.page_dirty.get_mut(vp) {
                        *dirty = false;
                    }
                }
            }
        }

        // Blit persistent texture to screen (single GPU operation)
        if let Some(canvas) = self.canvas.as_mut() {
            if let Some(Some(texture)) = self.page_textures.get(vp) {
                let _ = canvas.copy(texture, None, None);
            }
            canvas.present();
        }

        Ok(())
    }

    fn pcopy(&mut self, src: i32, dst: i32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        let src_page = src as usize;
        let dst_page = dst as usize;

        if src_page >= self.max_pages || dst_page >= self.max_pages {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                format!(
                    "Invalid page number: src={}, dst={}, max={}",
                    src, dst, self.max_pages
                ),
            ));
        }

        // Copy page buffer contents
        if src_page != dst_page {
            // Clone source page, then assign to destination
            // Use .get() for defensive bounds checking even though we validated above
            let src_data = match self.page_buffers.get(src_page) {
                Some(data) => data.clone(),
                None => {
                    return Err(GraphicsError::new(
                        GraphicsErrorKind::InvalidArgument,
                        format!("Page buffer access failed: src={}", src_page),
                    ));
                }
            };

            match self.page_buffers.get_mut(dst_page) {
                Some(dst_buf) => *dst_buf = src_data,
                None => {
                    return Err(GraphicsError::new(
                        GraphicsErrorKind::InvalidArgument,
                        format!("Page buffer access failed: dst={}", dst_page),
                    ));
                }
            }

            // Mark destination page as dirty - texture upload deferred to display()
            if let Some(dirty) = self.page_dirty.get_mut(dst_page) {
                *dirty = true;
            }
        }

        Ok(())
    }

    fn set_active_page(&mut self, page: i32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        let page_num = page as usize;
        if page_num >= self.max_pages {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                format!("Invalid active page: {}, max={}", page, self.max_pages),
            ));
        }

        self.active_page = page_num;
        Ok(())
    }

    fn set_visual_page(&mut self, page: i32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        let page_num = page as usize;
        if page_num >= self.max_pages {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                format!("Invalid visual page: {}, max={}", page, self.max_pages),
            ));
        }

        // If switching to a different visual page, mark it as dirty
        // Texture upload deferred to display() - O(1) page switch!
        if page_num != self.visual_page {
            self.visual_page = page_num;

            // Mark new visual page as dirty so display() will upload it
            if let Some(dirty) = self.page_dirty.get_mut(page_num) {
                *dirty = true;
            }
        }

        Ok(())
    }

    fn get_pages(&self) -> (i32, i32) {
        (self.active_page as i32, self.visual_page as i32)
    }

    fn poll_events(&mut self) -> Result<bool, GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        if let Some(event_pump) = self.event_pump.as_mut() {
            // Update keyboard state from SDL2
            let keyboard_state = event_pump.keyboard_state();
            // Update state for all scancodes we care about
            // We'll update them on-demand, but for now, clear and rebuild
            self.keyboard_state.clear();
            // Check common keys (we'll expand this as needed)
            let common_scancodes = [
                Scancode::A,
                Scancode::B,
                Scancode::C,
                Scancode::D,
                Scancode::E,
                Scancode::F,
                Scancode::G,
                Scancode::H,
                Scancode::I,
                Scancode::J,
                Scancode::K,
                Scancode::L,
                Scancode::M,
                Scancode::N,
                Scancode::O,
                Scancode::P,
                Scancode::Q,
                Scancode::R,
                Scancode::S,
                Scancode::T,
                Scancode::U,
                Scancode::V,
                Scancode::W,
                Scancode::X,
                Scancode::Y,
                Scancode::Z,
                Scancode::Num0,
                Scancode::Num1,
                Scancode::Num2,
                Scancode::Num3,
                Scancode::Num4,
                Scancode::Num5,
                Scancode::Num6,
                Scancode::Num7,
                Scancode::Num8,
                Scancode::Num9,
                Scancode::Space,
                Scancode::Return,
                Scancode::Escape,
                Scancode::Backspace,
                Scancode::Tab,
                Scancode::Up,
                Scancode::Down,
                Scancode::Left,
                Scancode::Right,
                Scancode::F1,
                Scancode::F2,
                Scancode::F3,
                Scancode::F4,
                Scancode::F5,
                Scancode::F6,
                Scancode::F7,
                Scancode::F8,
                Scancode::F9,
                Scancode::F10,
                Scancode::F11,
                Scancode::F12,
                Scancode::Insert,
                Scancode::Delete,
                Scancode::Home,
                Scancode::End,
                Scancode::PageUp,
                Scancode::PageDown,
                Scancode::LShift,
                Scancode::RShift,
                Scancode::LCtrl,
                Scancode::RCtrl,
                Scancode::LAlt,
                Scancode::RAlt,
                Scancode::LGui,
                Scancode::RGui,
            ];
            for &scancode in &common_scancodes {
                self.keyboard_state
                    .insert(scancode, keyboard_state.is_scancode_pressed(scancode));
            }

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

    fn is_key_pressed(&self, keycode: i64) -> bool {
        if !self.initialized {
            return false;
        }

        // Map QB64 keycode to SDL2 scancode
        let scancode = match SDL2Backend::map_qb64_to_scancode(keycode) {
            Some(sc) => sc,
            None => return false,
        };

        // Check keyboard state
        self.keyboard_state.get(&scancode).copied().unwrap_or(false)
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
            self.line(x1, y1, x2, y1, bc, false, false, None)?; // Top
            self.line(x1, y2, x2, y2, bc, false, false, None)?; // Bottom
            self.line(x1, y1, x1, y2, bc, false, false, None)?; // Left
            self.line(x2, y1, x2, y2, bc, false, false, None)?; // Right
        }

        // Fill viewport if specified
        if let Some(fc) = fill_color {
            self.line(x1 + 1, y1 + 1, x2 - 1, y2 - 1, fc, true, false, None)?;
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

    fn get_palette_for_image(&self, index: i32, handle: i32) -> u32 {
        if index < 0 || index >= 256 {
            return 0;
        }

        if handle == 0 {
            // Screen/current destination - use screen_palette
            self.screen_palette[index as usize]
        } else if let Some(img) = self.images.get(&handle) {
            // Per-image palette
            img.palette[index as usize]
        } else {
            // Invalid handle - fall back to global palette
            self.palette.colors[index as usize]
        }
    }

    fn set_palette_for_image(
        &mut self,
        index: i32,
        color: u32,
        handle: i32,
    ) -> Result<(), GraphicsError> {
        if index < 0 || index >= 256 {
            return Err(GraphicsError::new(
                GraphicsErrorKind::InvalidArgument,
                format!("Palette index out of range: {} (must be 0-255)", index),
            ));
        }

        if handle == 0 {
            // Screen/current destination - update screen_palette
            self.screen_palette[index as usize] = color;
            // Also update global palette for compatibility
            self.palette.colors[index as usize] = color;
        } else if let Some(img) = self.images.get_mut(&handle) {
            // Per-image palette
            img.palette[index as usize] = color;
        } else {
            // Invalid handle - fall back to global palette
            self.palette.colors[index as usize] = color;
        }

        Ok(())
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
                palette: ColorPalette::default().colors,
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
        // When copying from screen (handle 0), copy from visual page
        let (width, height, pixels, blend_enabled, clear_color, src_palette) = if handle == 0 {
            let page_pixels = self
                .page_buffers
                .get(self.visual_page)
                .cloned()
                .unwrap_or_default();
            (
                self.width,
                self.height,
                page_pixels,
                self.screen_blend_enabled,
                self.screen_clear_color,
                self.screen_palette,
            )
        } else if let Some(img) = self.images.get(&handle) {
            (
                img.width,
                img.height,
                img.pixels.clone(),
                img.blend_enabled,
                img.clear_color,
                img.palette,
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
            palette: src_palette,
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
            palette: self.screen_palette,
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
                if let Some(window) = self.canvas.as_ref().map(|c| c.window()) {
                    ctx.mouse().warp_mouse_in_window(window, x, y);
                }
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

    fn set_title(&mut self, title: &str) {
        if let Some(canvas) = self.canvas.as_mut() {
            let _ = canvas.window_mut().set_title(title);
        }
    }

    fn set_icon(&mut self, handle: i32) -> i32 {
        let previous_handle = self.current_icon_handle;

        // If handle is 0, clear the icon
        if handle == 0 {
            if let Some(canvas) = self.canvas.as_mut() {
                // Clear icon by setting None
                // set_icon requires a Surface, not None - skip if no icon
            }
            self.current_icon_handle = 0;
            return previous_handle;
        }

        // Get the image buffer
        let image = match self.images.get(&handle) {
            Some(img) => img,
            None => {
                // Invalid handle, return previous handle
                return previous_handle;
            }
        };

        // Convert ARGB pixels to RGBA format for SDL2
        // ARGB format: AAAAAAAA RRRRRRRR GGGGGGGG BBBBBBBB
        // RGBA format: RRRRRRRR GGGGGGGG BBBBBBBB AAAAAAAA
        let mut rgba_pixels = Vec::with_capacity((image.pixels.len() * 4) as usize);
        for &argb in &image.pixels {
            let a = ((argb >> 24) & 0xFF) as u8;
            let r = ((argb >> 16) & 0xFF) as u8;
            let g = ((argb >> 8) & 0xFF) as u8;
            let b = (argb & 0xFF) as u8;
            rgba_pixels.push(r);
            rgba_pixels.push(g);
            rgba_pixels.push(b);
            rgba_pixels.push(a);
        }

        // Create SDL2 Surface from pixel data
        // Note: from_data borrows the pixel buffer, but set_icon takes ownership
        // We need to ensure the surface is created and used before the buffer is dropped
        if let Some(canvas) = self.canvas.as_mut() {
            // Create surface from RGBA pixel data
            // The surface will reference the pixel buffer, so we need to keep it alive
            match Surface::from_data(
                &mut rgba_pixels,
                image.width,
                image.height,
                (image.width * 4) as u32, // Pitch = width * 4 bytes per pixel
                PixelFormatEnum::RGBA32,
            ) {
                Ok(surface) => {
                    // Set the window icon (takes ownership of the surface)
                    canvas.window_mut().set_icon(&surface);
                    self.current_icon_handle = handle;
                    // Surface is dropped here, but SDL2 should have copied the data
                }
                Err(e) => {
                    // Surface creation failed, log error but don't crash
                    eprintln!("Failed to create icon surface: {}", e);
                }
            }
        }

        previous_handle
    }

    fn get_icon(&self) -> i32 {
        self.current_icon_handle
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

    fn copy_palette(&mut self, src_handle: i32, dest_handle: i32) {
        // Get source palette
        let src_palette = if src_handle == 0 {
            self.screen_palette
        } else if let Some(src_img) = self.images.get(&src_handle) {
            src_img.palette
        } else {
            return; // Invalid source handle
        };

        // Copy to destination
        if dest_handle == 0 {
            self.screen_palette = src_palette;
            // Also update the global palette for compatibility
            self.palette.colors = src_palette;
        } else if let Some(dest_img) = self.images.get_mut(&dest_handle) {
            dest_img.palette = src_palette;
        }
        // Invalid dest handle is silently ignored
    }

    fn set_display_order(&mut self, layer1: i32, layer2: i32, layer3: i32, layer4: i32) {
        self.display_order = [layer1, layer2, layer3, layer4];
        // Note: In a full implementation, this would affect compositing order
        // For now, we store the order but SDL2 rendering is immediate mode
    }

    #[allow(clippy::too_many_arguments)]
    fn map_triangle(
        &mut self,
        sx1: f32,
        sy1: f32,
        sx2: f32,
        sy2: f32,
        sx3: f32,
        sy3: f32,
        dx1: f32,
        dy1: f32,
        dx2: f32,
        dy2: f32,
        dx3: f32,
        dy3: f32,
        src_handle: i32,
        dest_handle: i32,
        smooth: bool,
        _seamless: bool,
    ) {
        // Get source image dimensions and pixels
        // When source is screen (handle 0), read from visual page
        let (src_width, src_height, src_pixels) = if src_handle == 0 {
            // Source is the screen (visual page)
            let (w, h) = self.get_screen_size();
            let pixels = self
                .page_buffers
                .get(self.visual_page)
                .cloned()
                .unwrap_or_default();
            (w as i32, h as i32, pixels)
        } else if let Some(img) = self.images.get(&src_handle) {
            (img.width as i32, img.height as i32, img.pixels.clone())
        } else {
            return; // Invalid source handle
        };

        // Calculate bounding box of destination triangle
        let min_x = dx1.min(dx2).min(dx3).floor() as i32;
        let max_x = dx1.max(dx2).max(dx3).ceil() as i32;
        let min_y = dy1.min(dy2).min(dy3).floor() as i32;
        let max_y = dy1.max(dy2).max(dy3).ceil() as i32;

        // Pre-calculate edge function denominators for barycentric coordinates
        let denom = (dy2 - dy3) * (dx1 - dx3) + (dx3 - dx2) * (dy1 - dy3);
        if denom.abs() < 0.0001 {
            return; // Degenerate triangle (zero area)
        }
        let inv_denom = 1.0 / denom;

        // Rasterize the triangle
        for py in min_y..=max_y {
            for px in min_x..=max_x {
                let px_f = px as f32 + 0.5;
                let py_f = py as f32 + 0.5;

                // Calculate barycentric coordinates
                let w1 = ((dy2 - dy3) * (px_f - dx3) + (dx3 - dx2) * (py_f - dy3)) * inv_denom;
                let w2 = ((dy3 - dy1) * (px_f - dx3) + (dx1 - dx3) * (py_f - dy3)) * inv_denom;
                let w3 = 1.0 - w1 - w2;

                // Check if point is inside triangle
                if w1 >= 0.0 && w2 >= 0.0 && w3 >= 0.0 {
                    // Interpolate source coordinates
                    let src_x = w1 * sx1 + w2 * sx2 + w3 * sx3;
                    let src_y = w1 * sy1 + w2 * sy2 + w3 * sy3;

                    // Sample source pixel
                    let color = if smooth {
                        // Bilinear filtering
                        Self::sample_bilinear(&src_pixels, src_width, src_height, src_x, src_y)
                    } else {
                        // Nearest neighbor
                        Self::sample_nearest(&src_pixels, src_width, src_height, src_x, src_y)
                    };

                    // Write to destination
                    if dest_handle == 0 {
                        // Write to screen buffer (active page)
                        let (screen_w, screen_h) = self.get_screen_size();
                        if px >= 0 && py >= 0 && px < screen_w as i32 && py < screen_h as i32 {
                            let idx = (py as u32 * screen_w + px as u32) as usize;
                            if let Some(page) = self.page_buffers.get_mut(self.active_page) {
                                if idx < page.len() {
                                    page[idx] = Self::blend_colors(color, page[idx]);
                                }
                            }
                        }
                    } else if let Some(img) = self.images.get_mut(&dest_handle) {
                        if px >= 0 && py >= 0 && px < img.width as i32 && py < img.height as i32 {
                            let idx = (py as u32 * img.width + px as u32) as usize;
                            if idx < img.pixels.len() {
                                img.pixels[idx] = Self::blend_colors(color, img.pixels[idx]);
                            }
                        }
                    }
                }
            }
        }
    }

    // ========================================================================
    // Unicode Font Support (FreeType)
    // ========================================================================

    #[cfg(feature = "freetype")]
    fn load_font_with_options(&mut self, path: &str, size: u16, options: u32) -> i64 {
        use crate::font_manager::FONT_MANAGER;
        let mut fm = FONT_MANAGER
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        fm.load_font(path, size, options)
    }

    #[cfg(not(feature = "freetype"))]
    fn load_font_with_options(&mut self, path: &str, size: u16, _options: u32) -> i64 {
        // Fall back to SDL2_TTF if available, otherwise return 0
        self.load_font(path, size)
    }

    #[cfg(feature = "freetype")]
    fn print_string_unicode(&mut self, x: i32, y: i32, text: &str) -> Result<(), GraphicsError> {
        use crate::font_manager::FONT_MANAGER;

        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }

        #[cfg(feature = "graphics-sdl2-ttf")]
        let current_font = self.current_font;
        #[cfg(not(feature = "graphics-sdl2-ttf"))]
        let current_font: i64 = 0;

        // If using built-in font (handle 0), fall back to ASCII rendering
        if current_font == 0 {
            return self.print_string(x, y, text);
        }

        // Use FreeType font rendering
        let mut fm = FONT_MANAGER
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(font) = fm.get_font_mut(current_font) {
            let mut px = x;
            let baseline = font.baseline;

            for ch in text.chars() {
                if let Some(glyph) = font.get_glyph(ch) {
                    // Calculate glyph position
                    let gx = px + glyph.bearing_x;
                    let gy = y + baseline - glyph.bearing_y;

                    // Render glyph with alpha blending
                    self.draw_glyph_alpha(gx, gy, glyph, self.fg_color);

                    px += glyph.advance_x;
                }
            }
        }

        Ok(())
    }

    #[cfg(not(feature = "freetype"))]
    fn print_string_unicode(&mut self, x: i32, y: i32, text: &str) -> Result<(), GraphicsError> {
        // Fall back to ASCII print_string
        self.print_string(x, y, text)
    }

    #[cfg(feature = "freetype")]
    fn get_print_width_unicode(&mut self, text: &str) -> i64 {
        use crate::font_manager::FONT_MANAGER;

        #[cfg(feature = "graphics-sdl2-ttf")]
        let current_font = self.current_font;
        #[cfg(not(feature = "graphics-sdl2-ttf"))]
        let current_font: i64 = 0;

        if current_font == 0 {
            return self.get_print_width(text);
        }

        let mut fm = FONT_MANAGER
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        fm.get_text_width(current_font, text)
    }

    #[cfg(not(feature = "freetype"))]
    fn get_print_width_unicode(&mut self, text: &str) -> i64 {
        self.get_print_width(text)
    }

    #[cfg(feature = "freetype")]
    fn get_unicode_font_height(&self, handle: i64) -> i64 {
        use crate::font_manager::FONT_MANAGER;

        if handle == 0 {
            return self.get_font_height() as i64;
        }

        let fm = FONT_MANAGER
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        fm.get_font_height(handle).unwrap_or(16) as i64
    }

    #[cfg(not(feature = "freetype"))]
    fn get_unicode_font_height(&self, _handle: i64) -> i64 {
        self.get_font_height() as i64
    }

    #[cfg(feature = "freetype")]
    fn get_unicode_line_spacing(&self) -> i64 {
        use crate::font_manager::FONT_MANAGER;

        #[cfg(feature = "graphics-sdl2-ttf")]
        let current_font = self.current_font;
        #[cfg(not(feature = "graphics-sdl2-ttf"))]
        let current_font: i64 = 0;

        if current_font == 0 {
            return self.get_font_height() as i64;
        }

        let fm = FONT_MANAGER
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        fm.get_font_height(current_font).unwrap_or(16) as i64
    }

    #[cfg(not(feature = "freetype"))]
    fn get_unicode_line_spacing(&self) -> i64 {
        self.get_font_height() as i64
    }

    #[cfg(feature = "freetype")]
    fn get_unicode_char_positions(&mut self, text: &str) -> Vec<i64> {
        use crate::font_manager::FONT_MANAGER;

        #[cfg(feature = "graphics-sdl2-ttf")]
        let current_font = self.current_font;
        #[cfg(not(feature = "graphics-sdl2-ttf"))]
        let current_font: i64 = 0;

        if current_font == 0 {
            return text
                .chars()
                .enumerate()
                .map(|(i, _)| (i as i64) * (FONT_WIDTH as i64))
                .collect();
        }

        let mut fm = FONT_MANAGER
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        fm.get_char_positions(current_font, text)
    }

    #[cfg(not(feature = "freetype"))]
    fn get_unicode_char_positions(&mut self, text: &str) -> Vec<i64> {
        text.chars()
            .enumerate()
            .map(|(i, _)| (i as i64) * (FONT_WIDTH as i64))
            .collect()
    }

    #[cfg(feature = "graphics-sdl2-ttf")]
    fn get_current_font(&self) -> i64 {
        self.current_font
    }

    #[cfg(not(feature = "graphics-sdl2-ttf"))]
    fn get_current_font(&self) -> i64 {
        0
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

    // --- Additional SDL2 backend unit tests (no initialize; exercise real backend code) ---

    #[test]
    fn test_argb_to_sdl_color_black_white() {
        let black = SDL2Backend::argb_to_sdl_color(0xFF00_0000);
        assert_eq!(black, Color::RGBA(0, 0, 0, 0xFF));
        let white = SDL2Backend::argb_to_sdl_color(0xFFFF_FFFF);
        assert_eq!(white, Color::RGBA(0xFF, 0xFF, 0xFF, 0xFF));
    }

    #[test]
    fn test_image_buffer_multiple_pixels() {
        let mut img = ImageBuffer::new(8, 8, 32, 0xFF00_8000); // ARGB dark green fill
        assert_eq!(img.get_pixel(0, 0), Some(0xFF00_8000));
        img.set_pixel(3, 4, 0xFFFF_00FF);
        img.set_pixel(7, 7, 0xFF00_00FF);
        assert_eq!(img.get_pixel(3, 4), Some(0xFFFF_00FF));
        assert_eq!(img.get_pixel(7, 7), Some(0xFF00_00FF));
        assert_eq!(img.get_pixel(1, 1), Some(0xFF00_8000)); // unchanged
    }

    #[test]
    fn test_palette_entries() {
        let palette = ColorPalette::default();
        assert_eq!(palette.colors[0], 0xFF00_0000);
        assert_eq!(palette.colors[7], 0xFFAA_AAAA); // EGA light gray
        assert_eq!(palette.colors[255], 0xFF00_0000); // last entry (only 0–15 set)
    }

    // ========================================================================
    // Alpha Blending Tests
    // ========================================================================

    #[test]
    fn test_blend_colors_fully_opaque() {
        // Fully opaque source should completely replace destination
        let src = 0xFF_FF0000; // Opaque red
        let dst = 0xFF_00FF00; // Opaque green
        let result = SDL2Backend::blend_colors(src, dst);
        // With alpha=255, source dominates
        assert_eq!((result >> 16) & 0xFF, 0xFF); // Red channel
        assert_eq!((result >> 8) & 0xFF, 0x00); // Green channel (from src)
    }

    #[test]
    fn test_blend_colors_fully_transparent() {
        // Fully transparent source should leave destination unchanged
        let src = 0x00_FF0000; // Transparent red
        let dst = 0xFF_00FF00; // Opaque green
        let result = SDL2Backend::blend_colors(src, dst);
        // With alpha=0, destination dominates
        assert_eq!((result >> 16) & 0xFF, 0x00); // Red channel (from dst)
        assert_eq!((result >> 8) & 0xFF, 0xFF); // Green channel
    }

    #[test]
    fn test_blend_colors_semi_transparent() {
        // 50% transparent red over opaque green
        let src = 0x80_FF0000; // 50% red (alpha = 128)
        let dst = 0xFF_00FF00; // Opaque green
        let result = SDL2Backend::blend_colors(src, dst);

        let out_r = (result >> 16) & 0xFF;
        let out_g = (result >> 8) & 0xFF;

        // Red should be roughly 128 (50% of 255)
        // Green should be roughly 127 (50% of 255)
        // Allow some tolerance for integer rounding
        assert!(out_r >= 125 && out_r <= 130, "Red: {}", out_r);
        assert!(out_g >= 125 && out_g <= 130, "Green: {}", out_g);
    }

    #[test]
    fn test_blend_enabled_default() {
        // Alpha blending should be enabled by default for screen
        let backend = SDL2Backend::new();
        assert!(backend.screen_blend_enabled);
    }

    #[test]
    fn test_image_buffer_blend_enabled_default() {
        // Alpha blending should be enabled by default for new images
        let img = ImageBuffer::new(10, 10, 32, 0xFF000000);
        assert!(img.blend_enabled);
    }
}
