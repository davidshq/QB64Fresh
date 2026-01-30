//! CP437 (Code Page 437) to Unicode mapping.
//!
//! Used for interpreting byte-oriented text (e.g. from `qb_gfx_printstring`) as
//! IBM PC / DOS box-drawing and accented characters. The embedded 8×8 font and
//! this mapping ensure IDE menus and classic BASIC text render correctly.

/// CP437 to Unicode mapping table (bytes 0x00–0xFF).
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

/// Converts a single CP437 byte to its Unicode character.
#[inline]
pub fn cp437_to_unicode(byte: u8) -> char {
    CP437_TO_UNICODE[byte as usize]
}

/// Converts a byte string interpreted as CP437 to a Unicode `String`.
///
/// Used when rendering byte-oriented text (e.g. IDE menus) so box-drawing
/// and accented characters display correctly.
#[inline]
pub fn cp437_string_to_unicode(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| cp437_to_unicode(b)).collect()
}

/// Convert a Unicode character to the closest CP437 byte.
///
/// Returns None if the character is not representable in CP437.
#[inline]
pub fn unicode_to_cp437(ch: char) -> Option<u8> {
    CP437_TO_UNICODE
        .iter()
        .position(|&c| c == ch)
        .map(|idx| idx as u8)
}

/// Convert a UTF-8 string to CP437 bytes, replacing unknown chars with '?'.
#[inline]
pub fn utf8_to_cp437_bytes(s: &str) -> Vec<u8> {
    s.chars()
        .map(|ch| unicode_to_cp437(ch).unwrap_or(b'?'))
        .collect()
}
