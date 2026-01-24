//! Position and span utilities for LSP.
//!
//! This module provides conversion utilities between byte offsets (used internally)
//! and LSP positions (line/character pairs using UTF-16 code units).

use tower_lsp::lsp_types::{Position, Range};

use crate::semantic::BasicType;

/// Checks if a variable name has an explicit type suffix.
///
/// Type suffixes in BASIC indicate the variable's type:
/// - `$` = STRING
/// - `%` = INTEGER
/// - `&` = LONG
/// - `!` = SINGLE
/// - `#` = DOUBLE
/// - `%%` = _INTEGER64
/// - etc.
pub(crate) fn has_type_suffix(name: &str) -> bool {
    let suffixes = ['$', '%', '&', '!', '#', '`'];
    name.chars().last().is_some_and(|c| suffixes.contains(&c))
}

/// Formats a BasicType for display in inlay hints.
pub(crate) fn format_basic_type(ty: &BasicType) -> String {
    match ty {
        BasicType::Integer => "INTEGER".to_string(),
        BasicType::Long => "LONG".to_string(),
        BasicType::Integer64 => "_INTEGER64".to_string(),
        BasicType::Single => "SINGLE".to_string(),
        BasicType::Double => "DOUBLE".to_string(),
        BasicType::String => "STRING".to_string(),
        BasicType::Byte => "_BYTE".to_string(),
        BasicType::UnsignedByte => "_UNSIGNED _BYTE".to_string(),
        BasicType::UnsignedInteger => "_UNSIGNED INTEGER".to_string(),
        BasicType::UnsignedLong => "_UNSIGNED LONG".to_string(),
        BasicType::UnsignedInteger64 => "_UNSIGNED _INTEGER64".to_string(),
        BasicType::Offset => "_OFFSET".to_string(),
        BasicType::Float => "_FLOAT".to_string(),
        BasicType::Bit => "_BIT".to_string(),
        BasicType::UnsignedBit => "_UNSIGNED _BIT".to_string(),
        BasicType::UserDefined(name) => name.clone(),
        BasicType::FixedString(len) => format!("STRING * {}", len),
        BasicType::Array {
            element_type,
            dimensions,
        } => {
            format!(
                "{}({})",
                format_basic_type(element_type),
                "...".repeat(*dimensions)
            )
        }
        BasicType::Mem => "_MEM".to_string(),
        BasicType::Void => "VOID".to_string(),
        BasicType::Unknown => "?".to_string(),
    }
}

/// Converts a byte range to an LSP Range.
pub(crate) fn span_to_range(source: &str, start: usize, end: usize) -> Range {
    let start_pos = offset_to_position(source, start);
    let end_pos = offset_to_position(source, end);
    Range {
        start: start_pos,
        end: end_pos,
    }
}

/// Converts a byte offset to an LSP Position (line, character).
///
/// LSP uses UTF-16 code units for character positions, so we must count
/// UTF-16 code units rather than Unicode code points. Characters outside
/// the Basic Multilingual Plane (like emojis) take 2 UTF-16 code units.
pub(crate) fn offset_to_position(source: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;

    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            // Count UTF-16 code units (1 for BMP chars, 2 for supplementary)
            character += c.len_utf16() as u32;
        }
    }

    Position { line, character }
}

/// Converts an LSP Position to a byte offset.
///
/// LSP uses UTF-16 code units for character positions, so we must count
/// UTF-16 code units rather than Unicode code points.
///
/// # Edge Cases
///
/// - If `position.character` exceeds the line length, returns the offset of
///   the newline character (end of line). This clamping behavior is intentional
///   for robustness when handling positions from potentially buggy clients.
/// - If `position.line` exceeds the number of lines, returns `None`.
pub(crate) fn position_to_offset(source: &str, position: Position) -> Option<usize> {
    let mut current_line = 0u32;
    let mut current_char = 0u32;

    for (i, c) in source.char_indices() {
        if current_line == position.line && current_char == position.character {
            return Some(i);
        }
        if c == '\n' {
            if current_line == position.line {
                // Position is past end of line
                return Some(i);
            }
            current_line += 1;
            current_char = 0;
        } else {
            // Count UTF-16 code units (1 for BMP chars, 2 for supplementary)
            current_char += c.len_utf16() as u32;
        }
    }

    // Position might be at the very end
    if current_line == position.line {
        Some(source.len())
    } else {
        None
    }
}
