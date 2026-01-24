//! FreeType-based font management for QB64Fresh.
//!
//! This module provides a thread-safe font manager that handles TrueType/OpenType
//! font loading, glyph rendering, and caching. It mirrors QB64PE's FontManager
//! design for compatibility.
//!
//! # Architecture
//!
//! ```text
//! FONT_MANAGER (global, mutex-protected)
//!     ↓
//! FontManager
//!     ├── FreeType Library
//!     └── HashMap<i64, LoadedFont>
//!                    ↓
//!               LoadedFont
//!                   ├── FreeType Face
//!                   ├── Font metrics (height, baseline, etc.)
//!                   └── HashMap<char, GlyphBitmap> (cache)
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use qb64fresh_rt::font_manager::FONT_MANAGER;
//!
//! let handle = {
//!     let mut fm = FONT_MANAGER.lock().unwrap();
//!     fm.load_font("DejaVuSans.ttf", 24, 0)?
//! };
//!
//! // Render text using handle...
//! ```

#[cfg(feature = "freetype")]
use freetype::{Face, Library};
#[cfg(feature = "freetype")]
use std::collections::HashMap;
#[cfg(feature = "freetype")]
use std::sync::Mutex;

/// Font loading option flags (QB64PE compatibility)
pub const FONT_LOAD_DONTBLEND: u32 = 8; // No anti-aliasing (1-bit rendering)
pub const FONT_LOAD_MONOSPACE: u32 = 16; // Force monospace width
pub const FONT_LOAD_UNICODE: u32 = 32; // UTF-8 input mode
pub const FONT_LOAD_AUTOMONO: u32 = 64; // Auto-detect monospace

/// Cached glyph bitmap with metrics.
///
/// Contains the rasterized glyph data and positioning information
/// needed to correctly place the glyph when rendering text.
#[cfg(feature = "freetype")]
#[derive(Debug, Clone)]
pub struct GlyphBitmap {
    /// Alpha values (0-255) for each pixel, row-major order
    pub data: Vec<u8>,
    /// Width of the bitmap in pixels
    pub width: u32,
    /// Height of the bitmap in pixels
    pub height: u32,
    /// Horizontal advance in pixels (distance to next glyph)
    pub advance_x: i32,
    /// X offset from origin to left edge of glyph
    pub bearing_x: i32,
    /// Y offset from baseline to top of glyph
    pub bearing_y: i32,
}

#[cfg(feature = "freetype")]
impl GlyphBitmap {
    /// Create an empty glyph (for spaces, etc.)
    pub fn empty(advance_x: i32) -> Self {
        Self {
            data: Vec::new(),
            width: 0,
            height: 0,
            advance_x,
            bearing_x: 0,
            bearing_y: 0,
        }
    }
}

/// Loaded font with glyph cache.
///
/// Contains the FreeType face, font metrics, and a cache of
/// previously rendered glyphs to avoid redundant rasterization.
#[cfg(feature = "freetype")]
pub struct LoadedFont {
    /// The FreeType font face
    face: Face,
    /// Keep font data alive for face lifetime
    _font_data: Vec<u8>,
    /// Pixel height of the font
    pub pixel_height: u32,
    /// Baseline offset from top of cell
    pub baseline: i32,
    /// Width for monospace fonts (None for proportional)
    pub monospace_width: Option<u32>,
    /// Whether this font is monospace
    pub is_monospace: bool,
    /// Whether to use 1-bit rendering (no anti-aliasing)
    pub no_blend: bool,
    /// Cache of rendered glyphs
    glyph_cache: HashMap<char, GlyphBitmap>,
    /// Font loading options (for reference)
    pub options: u32,
}

#[cfg(feature = "freetype")]
impl LoadedFont {
    /// Render a glyph and cache it, or return from cache if available.
    pub fn get_glyph(&mut self, codepoint: char) -> Option<&GlyphBitmap> {
        if self.glyph_cache.contains_key(&codepoint) {
            return self.glyph_cache.get(&codepoint);
        }

        // Render the glyph
        if let Some(glyph) = self.render_glyph(codepoint) {
            self.glyph_cache.insert(codepoint, glyph);
            self.glyph_cache.get(&codepoint)
        } else {
            None
        }
    }

    /// Get a glyph from cache (without rendering)
    pub fn get_cached_glyph(&self, codepoint: char) -> Option<&GlyphBitmap> {
        self.glyph_cache.get(&codepoint)
    }

    /// Render a single glyph using FreeType.
    fn render_glyph(&self, codepoint: char) -> Option<GlyphBitmap> {
        // Load the glyph for this character
        let glyph_index = match self.face.get_char_index(codepoint as usize) {
            Ok(idx) => idx.get(),  // NonZero<u32> -> u32
            Err(_) => return None, // Character not found in font
        };

        // Load with or without anti-aliasing based on options
        let load_flags = if self.no_blend {
            freetype::face::LoadFlag::MONOCHROME | freetype::face::LoadFlag::TARGET_MONO
        } else {
            freetype::face::LoadFlag::TARGET_NORMAL
        };

        self.face.load_glyph(glyph_index, load_flags).ok()?;

        let glyph = self.face.glyph();

        // Render to bitmap
        let render_mode = if self.no_blend {
            freetype::RenderMode::Mono
        } else {
            freetype::RenderMode::Normal
        };

        glyph.render_glyph(render_mode).ok()?;

        let bitmap = glyph.bitmap();
        let width = bitmap.width() as u32;
        let height = bitmap.rows() as u32;

        // Calculate advance (with optional monospace override)
        let advance_x = if let Some(mono_width) = self.monospace_width {
            mono_width as i32
        } else {
            (glyph.advance().x >> 6) as i32
        };

        // Handle empty glyphs (like space)
        if width == 0 || height == 0 {
            return Some(GlyphBitmap::empty(advance_x));
        }

        // Convert bitmap to alpha values
        let data = if self.no_blend {
            // 1-bit monochrome: expand to 8-bit alpha
            let pitch = bitmap.pitch().unsigned_abs() as usize;
            let buffer = bitmap.buffer();
            let mut alpha_data = Vec::with_capacity((width * height) as usize);

            for row in 0..height as usize {
                for col in 0..width as usize {
                    let byte_idx = row * pitch + (col >> 3);
                    let bit_idx = 7 - (col & 7);
                    if byte_idx < buffer.len() {
                        let bit = (buffer[byte_idx] >> bit_idx) & 1;
                        alpha_data.push(if bit != 0 { 255 } else { 0 });
                    } else {
                        alpha_data.push(0);
                    }
                }
            }
            alpha_data
        } else {
            // 8-bit grayscale: copy directly
            bitmap.buffer().to_vec()
        };

        Some(GlyphBitmap {
            data,
            width,
            height,
            advance_x,
            bearing_x: glyph.bitmap_left(),
            bearing_y: glyph.bitmap_top(),
        })
    }
}

/// Rendered text result containing pixel data and dimensions.
#[cfg(feature = "freetype")]
#[derive(Debug)]
pub struct RenderedText {
    /// ARGB pixel data, row-major order
    pub pixels: Vec<u32>,
    /// Width of the rendered text in pixels
    pub width: u32,
    /// Height of the rendered text in pixels
    pub height: u32,
    /// Baseline offset from top
    pub baseline: i32,
}

/// Global font manager (thread-safe).
///
/// Manages all loaded fonts and provides the central interface
/// for font loading, rendering, and text measurement.
#[cfg(feature = "freetype")]
pub struct FontManager {
    /// FreeType library instance
    library: Library,
    /// Loaded fonts indexed by handle
    fonts: HashMap<i64, LoadedFont>,
    /// Next available font handle (starts at 1, 0 = built-in font)
    next_handle: i64,
}

// SAFETY: FontManager is protected by a Mutex, so only one thread can access
// it at a time. FreeType's Library and Face types contain raw pointers that
// don't implement Send/Sync, but since we serialize all access through the
// mutex, this is safe.
#[cfg(feature = "freetype")]
unsafe impl Send for FontManager {}
#[cfg(feature = "freetype")]
unsafe impl Sync for FontManager {}

#[cfg(feature = "freetype")]
impl FontManager {
    /// Create a new font manager.
    pub fn new() -> Result<Self, String> {
        let library =
            Library::init().map_err(|e| format!("Failed to initialize FreeType: {:?}", e))?;

        Ok(Self {
            library,
            fonts: HashMap::new(),
            next_handle: 1,
        })
    }

    /// Load a font from a file.
    ///
    /// # Arguments
    /// - `path`: Path to the TrueType/OpenType font file
    /// - `size`: Pixel height of the font
    /// - `options`: Font loading options (FONT_LOAD_* constants)
    ///
    /// # Returns
    /// Font handle on success (positive number), or 0 on failure
    pub fn load_font(&mut self, path: &str, size: u16, options: u32) -> i64 {
        // Read font file into memory
        let font_data = match std::fs::read(path) {
            Ok(data) => data,
            Err(_) => return 0,
        };

        // Create font face from memory
        let face = match self.library.new_memory_face(font_data.clone(), 0) {
            Ok(f) => f,
            Err(_) => return 0,
        };

        // Set the pixel size
        if face.set_pixel_sizes(0, size as u32).is_err() {
            return 0;
        }

        // Determine if font is monospace
        let is_monospace = (options & FONT_LOAD_MONOSPACE != 0) || face.is_fixed_width();

        // Calculate monospace width if needed
        let monospace_width = if is_monospace || (options & FONT_LOAD_AUTOMONO != 0) {
            // Get width of 'M' character as reference
            if face
                .load_char('M' as usize, freetype::face::LoadFlag::DEFAULT)
                .is_ok()
            {
                Some((face.glyph().advance().x >> 6) as u32)
            } else {
                Some(size as u32 / 2) // Fallback
            }
        } else {
            None
        };

        // Calculate baseline from ascender
        let ascender = face.ascender() as i32;
        let units_per_em = face.em_size() as i32;
        let baseline = if units_per_em > 0 {
            (ascender * size as i32) / units_per_em
        } else {
            (size as i32 * 3) / 4 // Fallback: 75% of height
        };

        let loaded_font = LoadedFont {
            face,
            _font_data: font_data,
            pixel_height: size as u32,
            baseline,
            monospace_width: if is_monospace { monospace_width } else { None },
            is_monospace,
            no_blend: options & FONT_LOAD_DONTBLEND != 0,
            glyph_cache: HashMap::new(),
            options,
        };

        let handle = self.next_handle;
        self.next_handle += 1;
        self.fonts.insert(handle, loaded_font);

        handle
    }

    /// Free a loaded font.
    pub fn free_font(&mut self, handle: i64) {
        self.fonts.remove(&handle);
    }

    /// Get a mutable reference to a loaded font.
    pub fn get_font_mut(&mut self, handle: i64) -> Option<&mut LoadedFont> {
        self.fonts.get_mut(&handle)
    }

    /// Get a reference to a loaded font.
    pub fn get_font(&self, handle: i64) -> Option<&LoadedFont> {
        self.fonts.get(&handle)
    }

    /// Get font height in pixels.
    pub fn get_font_height(&self, handle: i64) -> Option<u32> {
        self.fonts.get(&handle).map(|f| f.pixel_height)
    }

    /// Get font width (for monospace fonts).
    pub fn get_font_width(&self, handle: i64) -> Option<u32> {
        self.fonts
            .get(&handle)
            .and_then(|f| f.monospace_width.or(Some(f.pixel_height / 2)))
    }

    /// Calculate the width of a text string in pixels.
    pub fn get_text_width(&mut self, handle: i64, text: &str) -> i64 {
        let font = match self.fonts.get_mut(&handle) {
            Some(f) => f,
            None => return 0,
        };

        let mut width = 0i64;
        for ch in text.chars() {
            if let Some(glyph) = font.get_glyph(ch) {
                width += glyph.advance_x as i64;
            }
        }

        width
    }

    /// Get character positions for text (x position of each character).
    ///
    /// Returns a vector of x positions where each character starts.
    pub fn get_char_positions(&mut self, handle: i64, text: &str) -> Vec<i64> {
        let font = match self.fonts.get_mut(&handle) {
            Some(f) => f,
            None => {
                return text
                    .chars()
                    .enumerate()
                    .map(|(i, _)| i as i64 * 8)
                    .collect()
            }
        };

        let mut positions = Vec::with_capacity(text.chars().count());
        let mut x = 0i64;

        for ch in text.chars() {
            positions.push(x);
            if let Some(glyph) = font.get_glyph(ch) {
                x += glyph.advance_x as i64;
            }
        }

        positions
    }
}

#[cfg(feature = "freetype")]
impl Default for FontManager {
    fn default() -> Self {
        Self::new().expect("Failed to initialize FontManager")
    }
}

// Global font manager singleton
#[cfg(feature = "freetype")]
lazy_static::lazy_static! {
    /// Global thread-safe font manager instance.
    ///
    /// Access via `FONT_MANAGER.lock().unwrap()`.
    pub static ref FONT_MANAGER: Mutex<FontManager> = Mutex::new(
        FontManager::new().expect("Failed to initialize global FontManager")
    );
}

// ============================================================================
// UTF-8 Conversion Utilities
// ============================================================================

/// CP437 to Unicode mapping table.
///
/// Maps bytes 0x00-0xFF from Code Page 437 to their Unicode equivalents.
/// This is used for rendering classic DOS text with Unicode fonts.
pub static CP437_TO_UNICODE: [char; 256] = [
    // 0x00-0x0F: Special symbols
    '\u{0000}', '\u{263A}', '\u{263B}', '\u{2665}', '\u{2666}', '\u{2663}', '\u{2660}', '\u{2022}',
    '\u{25D8}', '\u{25CB}', '\u{25D9}', '\u{2642}', '\u{2640}', '\u{266A}', '\u{266B}', '\u{263C}',
    // 0x10-0x1F: Arrows and symbols
    '\u{25BA}', '\u{25C4}', '\u{2195}', '\u{203C}', '\u{00B6}', '\u{00A7}', '\u{25AC}', '\u{21A8}',
    '\u{2191}', '\u{2193}', '\u{2192}', '\u{2190}', '\u{221F}', '\u{2194}', '\u{25B2}', '\u{25BC}',
    // 0x20-0x7E: Standard ASCII
    ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2',
    '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
    'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~',
    '\u{2302}', // 0x80-0x9F: Accented characters
    '\u{00C7}', '\u{00FC}', '\u{00E9}', '\u{00E2}', '\u{00E4}', '\u{00E0}', '\u{00E5}', '\u{00E7}',
    '\u{00EA}', '\u{00EB}', '\u{00E8}', '\u{00EF}', '\u{00EE}', '\u{00EC}', '\u{00C4}', '\u{00C5}',
    '\u{00C9}', '\u{00E6}', '\u{00C6}', '\u{00F4}', '\u{00F6}', '\u{00F2}', '\u{00FB}', '\u{00F9}',
    '\u{00FF}', '\u{00D6}', '\u{00DC}', '\u{00A2}', '\u{00A3}', '\u{00A5}', '\u{20A7}', '\u{0192}',
    // 0xA0-0xBF: More accented + box drawing
    '\u{00E1}', '\u{00ED}', '\u{00F3}', '\u{00FA}', '\u{00F1}', '\u{00D1}', '\u{00AA}', '\u{00BA}',
    '\u{00BF}', '\u{2310}', '\u{00AC}', '\u{00BD}', '\u{00BC}', '\u{00A1}', '\u{00AB}', '\u{00BB}',
    '\u{2591}', '\u{2592}', '\u{2593}', '\u{2502}', '\u{2524}', '\u{2561}', '\u{2562}', '\u{2556}',
    '\u{2555}', '\u{2563}', '\u{2551}', '\u{2557}', '\u{255D}', '\u{255C}', '\u{255B}', '\u{2510}',
    // 0xC0-0xDF: More box drawing
    '\u{2514}', '\u{2534}', '\u{252C}', '\u{251C}', '\u{2500}', '\u{253C}', '\u{255E}', '\u{255F}',
    '\u{255A}', '\u{2554}', '\u{2569}', '\u{2566}', '\u{2560}', '\u{2550}', '\u{256C}', '\u{2567}',
    '\u{2568}', '\u{2564}', '\u{2565}', '\u{2559}', '\u{2558}', '\u{2552}', '\u{2553}', '\u{256B}',
    '\u{256A}', '\u{2518}', '\u{250C}', '\u{2588}', '\u{2584}', '\u{258C}', '\u{2590}', '\u{2580}',
    // 0xE0-0xFF: Greek and math symbols
    '\u{03B1}', '\u{00DF}', '\u{0393}', '\u{03C0}', '\u{03A3}', '\u{03C3}', '\u{00B5}', '\u{03C4}',
    '\u{03A6}', '\u{0398}', '\u{03A9}', '\u{03B4}', '\u{221E}', '\u{03C6}', '\u{03B5}', '\u{2229}',
    '\u{2261}', '\u{00B1}', '\u{2265}', '\u{2264}', '\u{2320}', '\u{2321}', '\u{00F7}', '\u{2248}',
    '\u{00B0}', '\u{2219}', '\u{00B7}', '\u{221A}', '\u{207F}', '\u{00B2}', '\u{25A0}', '\u{00A0}',
];

/// Convert a CP437 byte to Unicode character.
pub fn cp437_to_unicode(byte: u8) -> char {
    CP437_TO_UNICODE[byte as usize]
}

/// Convert a CP437 byte string to Unicode.
pub fn cp437_string_to_unicode(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| cp437_to_unicode(b)).collect()
}

/// Convert UTF-8 string to codepoints.
pub fn utf8_to_codepoints(text: &str) -> Vec<char> {
    text.chars().collect()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cp437_ascii_range() {
        // ASCII printable range should map to themselves
        for byte in 0x20..=0x7E {
            let ch = cp437_to_unicode(byte);
            assert_eq!(
                ch as u8, byte,
                "ASCII byte {:02X} should map to itself",
                byte
            );
        }
    }

    #[test]
    fn test_cp437_special_chars() {
        // Check a few known mappings
        assert_eq!(cp437_to_unicode(0x01), '\u{263A}'); // Smiley
        assert_eq!(cp437_to_unicode(0x03), '\u{2665}'); // Heart
        assert_eq!(cp437_to_unicode(0xB0), '\u{2591}'); // Light shade
        assert_eq!(cp437_to_unicode(0xDB), '\u{2588}'); // Full block
    }

    #[test]
    fn test_utf8_to_codepoints() {
        let text = "Hello, 世界!";
        let codepoints = utf8_to_codepoints(text);
        assert_eq!(codepoints.len(), 10);
        assert_eq!(codepoints[7], '世');
        assert_eq!(codepoints[8], '界');
    }

    #[test]
    fn test_cp437_string_conversion() {
        let bytes = [0x48, 0x65, 0x6C, 0x6C, 0x6F]; // "Hello"
        let result = cp437_string_to_unicode(&bytes);
        assert_eq!(result, "Hello");
    }
}
