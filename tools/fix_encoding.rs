//! Encoding Fix Utility for Legacy BASIC Files
//!
//! This tool handles various encoding issues found in DOS-era BASIC source files:
//!
//! 1. **DOS Control-Z (0x1A) EOF markers**: DOS used Ctrl-Z as an EOF marker in text files.
//!    These should be stripped from modern file systems.
//!
//! 2. **CP437 (IBM PC Code Page)**: DOS used Code Page 437 which includes line-drawing
//!    characters (like ═, ║, ╔, ╗, etc.) in the 0x80-0xFF range. These need to be
//!    converted to UTF-8 equivalents.
//!
//! 3. **ISO-8859-1 (Latin1)**: Some files use Latin1 encoding for Western European
//!    characters. This is a simple byte-to-Unicode mapping.
//!
//! 4. **CR/LF Line Endings**: DOS used \r\n, Unix uses \n. We normalize to \n.
//!
//! 5. **Embedded Binary DATA**: DOS-era BASIC programs often stored sprite/image data
//!    as binary strings in DATA statements. These are detected and preserved exactly
//!    as-is to avoid corruption.
//!
//! Usage:
//!   fix_encoding [OPTIONS] <FILE>...
//!
//!   --dry-run       Show what would be changed without modifying files
//!   --backup        Create .bak backup files before modifying
//!   --encoding <E>  Force encoding: cp437, latin1, auto (default: auto)
//!   --recursive     Process directories recursively
//!   --verbose       Show detailed information about changes

use std::collections::HashMap;
use std::env;
use std::fs;
// std::io is available if needed for future enhancements
use std::path::{Path, PathBuf};

/// CP437 to Unicode mapping for bytes 0x80-0xFF
/// This is the IBM PC character set used in DOS
fn cp437_to_unicode() -> HashMap<u8, char> {
    let mut map = HashMap::new();

    // 0x80-0x9F: Accented characters and special symbols
    let chars_80_9f = [
        'Ç', 'ü', 'é', 'â', 'ä', 'à', 'å', 'ç', 'ê', 'ë', 'è', 'ï', 'î', 'ì', 'Ä', 'Å', 'É', 'æ',
        'Æ', 'ô', 'ö', 'ò', 'û', 'ù', 'ÿ', 'Ö', 'Ü', '¢', '£', '¥', '₧', 'ƒ',
    ];

    // 0xA0-0xBF: More accented characters and symbols
    let chars_a0_bf = [
        'á', 'í', 'ó', 'ú', 'ñ', 'Ñ', 'ª', 'º', '¿', '⌐', '¬', '½', '¼', '¡', '«', '»', '░', '▒',
        '▓', '│', '┤', '╡', '╢', '╖', '╕', '╣', '║', '╗', '╝', '╜', '╛', '┐',
    ];

    // 0xC0-0xDF: Box-drawing characters
    let chars_c0_df = [
        '└', '┴', '┬', '├', '─', '┼', '╞', '╟', '╚', '╔', '╩', '╦', '╠', '═', '╬', '╧', '╨', '╤',
        '╥', '╙', '╘', '╒', '╓', '╫', '╪', '┘', '┌', '█', '▄', '▌', '▐', '▀',
    ];

    // 0xE0-0xFF: Greek letters and math symbols
    let chars_e0_ff = [
        'α', 'ß', 'Γ', 'π', 'Σ', 'σ', 'µ', 'τ', 'Φ', 'Θ', 'Ω', 'δ', '∞', 'φ', 'ε', '∩', '≡', '±',
        '≥', '≤', '⌠', '⌡', '÷', '≈', '°', '∙', '·', '√', 'ⁿ', '²', '■', ' ',
    ];

    for (i, &c) in chars_80_9f.iter().enumerate() {
        map.insert(0x80 + i as u8, c);
    }
    for (i, &c) in chars_a0_bf.iter().enumerate() {
        map.insert(0xA0 + i as u8, c);
    }
    for (i, &c) in chars_c0_df.iter().enumerate() {
        map.insert(0xC0 + i as u8, c);
    }
    for (i, &c) in chars_e0_ff.iter().enumerate() {
        map.insert(0xE0 + i as u8, c);
    }

    map
}

/// Encoding detection result
#[derive(Debug, Clone, Copy, PartialEq)]
enum DetectedEncoding {
    Ascii,
    Utf8,
    Cp437,
    Latin1,
    Binary,
    /// BASIC file with embedded binary DATA statements - needs special handling
    BasicWithBinaryData,
}

/// Statistics about a file's encoding
#[derive(Debug, Default)]
struct EncodingStats {
    total_bytes: usize,
    ascii_bytes: usize,
    high_bytes: usize,
    control_z_count: usize,
    cr_lf_count: usize,
    box_drawing_likely: usize,
    latin1_likely: usize,
    invalid_utf8_sequences: usize,
    /// Number of lines that appear to be DATA statements with binary content
    binary_data_lines: usize,
    /// High bytes found outside of DATA statements
    high_bytes_in_code: usize,
    /// High bytes found inside DATA statement strings
    high_bytes_in_data: usize,
}

impl EncodingStats {
    fn analyze(data: &[u8]) -> Self {
        let mut stats = EncodingStats {
            total_bytes: data.len(),
            ..Default::default()
        };

        // Split into lines for smarter analysis
        let lines = Self::split_lines(data);

        for line in &lines {
            let is_data_line = Self::is_data_statement(line);
            let line_high_bytes = Self::count_high_bytes_in_line(line, is_data_line);

            if is_data_line && line_high_bytes > 10 {
                // DATA statement with lots of high bytes = likely binary sprite data
                stats.binary_data_lines += 1;
                stats.high_bytes_in_data += line_high_bytes;
            } else {
                stats.high_bytes_in_code += line_high_bytes;
            }
        }

        // Now do byte-level analysis
        let mut i = 0;
        while i < data.len() {
            let b = data[i];

            if b == 0x1A {
                stats.control_z_count += 1;
            } else if b == 0x0D && i + 1 < data.len() && data[i + 1] == 0x0A {
                stats.cr_lf_count += 1;
            }

            if b < 0x80 {
                stats.ascii_bytes += 1;
            } else {
                stats.high_bytes += 1;

                // Check for box-drawing characters (common in CP437)
                if (0xB0..=0xDF).contains(&b) || b == 0xFE || b == 0xFF {
                    stats.box_drawing_likely += 1;
                }

                // Check for Latin1-likely characters (accented letters)
                if (0xC0..=0xFF).contains(&b) {
                    // In Latin1, 0xC0-0xFF are accented letters
                    // In CP437, some of these are box-drawing
                    stats.latin1_likely += 1;
                }

                // Check if this could be valid UTF-8
                if b >= 0xC0 {
                    let expected_continuation = if b < 0xE0 {
                        1
                    } else if b < 0xF0 {
                        2
                    } else {
                        3
                    };

                    let mut valid = true;
                    for j in 1..=expected_continuation {
                        if i + j >= data.len() || (data[i + j] & 0xC0) != 0x80 {
                            valid = false;
                            break;
                        }
                    }

                    if !valid {
                        stats.invalid_utf8_sequences += 1;
                    }
                }
            }

            i += 1;
        }

        stats
    }

    /// Split data into lines (handles both CRLF and LF)
    fn split_lines(data: &[u8]) -> Vec<&[u8]> {
        let mut lines = Vec::new();
        let mut start = 0;

        for i in 0..data.len() {
            if data[i] == b'\n' {
                // Handle CRLF
                let end = if i > 0 && data[i - 1] == b'\r' {
                    i - 1
                } else {
                    i
                };
                lines.push(&data[start..end]);
                start = i + 1;
            }
        }

        // Don't forget the last line if no trailing newline
        if start < data.len() {
            lines.push(&data[start..]);
        }

        lines
    }

    /// Check if a line is a DATA statement
    fn is_data_statement(line: &[u8]) -> bool {
        // Skip leading whitespace and optional line number
        let trimmed = Self::skip_line_number_and_whitespace(line);

        // Check for DATA keyword (case-insensitive)
        if trimmed.len() >= 4 {
            let prefix = &trimmed[..4];
            if prefix.eq_ignore_ascii_case(b"DATA") {
                // Make sure it's followed by whitespace or end of line
                return trimmed.len() == 4 || !trimmed[4].is_ascii_alphanumeric();
            }
        }

        false
    }

    /// Skip line number and leading whitespace
    fn skip_line_number_and_whitespace(line: &[u8]) -> &[u8] {
        let mut i = 0;

        // Skip leading whitespace
        while i < line.len() && (line[i] == b' ' || line[i] == b'\t') {
            i += 1;
        }

        // Skip line number (digits)
        while i < line.len() && line[i].is_ascii_digit() {
            i += 1;
        }

        // Skip whitespace after line number
        while i < line.len() && (line[i] == b' ' || line[i] == b'\t') {
            i += 1;
        }

        &line[i..]
    }

    /// Count high bytes in a line, with awareness of whether it's a DATA line
    fn count_high_bytes_in_line(line: &[u8], _is_data: bool) -> usize {
        line.iter().filter(|&&b| b >= 0x80).count()
    }

    fn detect_encoding(&self) -> DetectedEncoding {
        // Pure ASCII
        if self.high_bytes == 0 {
            return DetectedEncoding::Ascii;
        }

        // Check for BASIC file with embedded binary DATA statements
        // This is detected when:
        // 1. There are DATA lines with lots of high bytes
        // 2. Most high bytes are in DATA statements, not in code
        if self.binary_data_lines > 0 && self.high_bytes_in_data > self.high_bytes_in_code * 2 {
            return DetectedEncoding::BasicWithBinaryData;
        }

        // Likely binary (too many high bytes overall, not in DATA)
        if self.high_bytes > self.total_bytes / 4 {
            return DetectedEncoding::Binary;
        }

        // Valid UTF-8 (no invalid sequences)
        if self.invalid_utf8_sequences == 0 && self.high_bytes > 0 {
            return DetectedEncoding::Utf8;
        }

        // More box-drawing than Latin1 letters suggests CP437
        if self.box_drawing_likely > self.latin1_likely / 2 {
            return DetectedEncoding::Cp437;
        }

        // Default to Latin1 for other high-byte content
        DetectedEncoding::Latin1
    }
}

/// Convert bytes to UTF-8 string using CP437 encoding
fn cp437_to_utf8(data: &[u8]) -> String {
    let map = cp437_to_unicode();
    let mut result = String::with_capacity(data.len());

    for &b in data {
        if b < 0x80 {
            result.push(b as char);
        } else if let Some(&c) = map.get(&b) {
            result.push(c);
        } else {
            // Fallback: use Unicode replacement character
            result.push('\u{FFFD}');
        }
    }

    result
}

/// Convert bytes to UTF-8 string using Latin1 encoding
fn latin1_to_utf8(data: &[u8]) -> String {
    // Latin1 is a direct byte-to-Unicode mapping for 0x00-0xFF
    data.iter().map(|&b| b as char).collect()
}

/// Convert a BASIC file line-by-line, preserving binary DATA statements
/// Returns (converted_content, lines_converted, lines_preserved)
fn convert_basic_preserving_data(
    data: &[u8],
    encoding: DetectedEncoding,
) -> (Vec<u8>, usize, usize) {
    let map = cp437_to_unicode();
    let mut result = Vec::with_capacity(data.len());
    let mut lines_converted = 0;
    let mut lines_preserved = 0;

    let lines = EncodingStats::split_lines(data);

    for (i, line) in lines.iter().enumerate() {
        let is_data_line = EncodingStats::is_data_statement(line);
        let has_high_bytes = line.iter().any(|&b| b >= 0x80);

        if is_data_line && has_high_bytes {
            // Preserve DATA lines with binary content exactly as-is
            result.extend_from_slice(line);
            lines_preserved += 1;
        } else if has_high_bytes {
            // Convert this line
            for &b in *line {
                if b < 0x80 {
                    result.push(b);
                } else {
                    // Convert based on encoding
                    let ch = match encoding {
                        DetectedEncoding::Cp437 | DetectedEncoding::BasicWithBinaryData => {
                            map.get(&b).copied().unwrap_or('\u{FFFD}')
                        }
                        DetectedEncoding::Latin1 => b as char,
                        _ => b as char,
                    };
                    // Write UTF-8 bytes
                    let mut buf = [0u8; 4];
                    let s = ch.encode_utf8(&mut buf);
                    result.extend_from_slice(s.as_bytes());
                }
            }
            lines_converted += 1;
        } else {
            // Pure ASCII line, copy as-is
            result.extend_from_slice(line);
        }

        // Add newline (except for last line if original didn't have one)
        if i < lines.len() - 1 || data.last() == Some(&b'\n') {
            result.push(b'\n');
        }
    }

    (result, lines_converted, lines_preserved)
}

/// Result of processing a file
#[derive(Debug)]
struct ProcessResult {
    #[allow(dead_code)]
    path: PathBuf,
    original_encoding: DetectedEncoding,
    had_control_z: bool,
    had_crlf: bool,
    high_byte_count: usize,
    changes_made: Vec<String>,
    error: Option<String>,
}

/// Process a single file
fn process_file(
    path: &Path,
    encoding: Option<DetectedEncoding>,
    dry_run: bool,
    backup: bool,
) -> ProcessResult {
    let mut result = ProcessResult {
        path: path.to_path_buf(),
        original_encoding: DetectedEncoding::Ascii,
        had_control_z: false,
        had_crlf: false,
        high_byte_count: 0,
        changes_made: Vec::new(),
        error: None,
    };

    // Read file as bytes
    let data = match fs::read(path) {
        Ok(d) => d,
        Err(e) => {
            result.error = Some(format!("Failed to read file: {}", e));
            return result;
        }
    };

    // Analyze encoding
    let stats = EncodingStats::analyze(&data);
    result.original_encoding = encoding.unwrap_or_else(|| stats.detect_encoding());
    result.had_control_z = stats.control_z_count > 0;
    result.had_crlf = stats.cr_lf_count > 0;
    result.high_byte_count = stats.high_bytes;

    // Skip pure binary files (no BASIC structure detected)
    if result.original_encoding == DetectedEncoding::Binary {
        result
            .changes_made
            .push("Skipped: appears to be binary".to_string());
        return result;
    }

    // Skip already valid UTF-8 with no issues
    if result.original_encoding == DetectedEncoding::Utf8
        && !result.had_control_z
        && !result.had_crlf
    {
        result
            .changes_made
            .push("No changes needed: already valid UTF-8".to_string());
        return result;
    }

    // Strip Control-Z (DOS EOF marker)
    let data: Vec<u8> = data.into_iter().take_while(|&b| b != 0x1A).collect();
    if result.had_control_z {
        result
            .changes_made
            .push("Stripped DOS Control-Z EOF marker".to_string());
    }

    // Convert to UTF-8 based on detected/forced encoding
    let content_bytes: Vec<u8>;
    match result.original_encoding {
        DetectedEncoding::Ascii | DetectedEncoding::Utf8 => {
            content_bytes = data;
        }
        DetectedEncoding::BasicWithBinaryData => {
            // Special handling: convert code lines but preserve binary DATA statements
            let (converted, lines_converted, lines_preserved) =
                convert_basic_preserving_data(&data, result.original_encoding);
            if lines_converted > 0 {
                result.changes_made.push(format!(
                    "Converted {} code lines from CP437 to UTF-8 (preserved {} binary DATA lines)",
                    lines_converted, lines_preserved
                ));
            }
            if lines_preserved > 0 && lines_converted == 0 {
                result.changes_made.push(format!(
                    "Preserved {} binary DATA lines (no code lines needed conversion)",
                    lines_preserved
                ));
            }
            // For BasicWithBinaryData, line endings are already normalized by convert_basic_preserving_data
            // so we skip the string conversion and write directly
            if !dry_run {
                if backup {
                    let backup_path = path.with_extension(format!(
                        "{}.bak",
                        path.extension()
                            .map(|s| s.to_string_lossy())
                            .unwrap_or_default()
                    ));
                    if let Err(e) = fs::copy(path, &backup_path) {
                        result.error = Some(format!("Failed to create backup: {}", e));
                        return result;
                    }
                    result
                        .changes_made
                        .push(format!("Created backup: {}", backup_path.display()));
                }

                if let Err(e) = fs::write(path, &converted) {
                    result.error = Some(format!("Failed to write file: {}", e));
                    return result;
                }
            } else {
                result
                    .changes_made
                    .push("(dry run - no files modified)".to_string());
            }
            return result;
        }
        DetectedEncoding::Cp437 => {
            result.changes_made.push(format!(
                "Converted {} high bytes from CP437 to UTF-8",
                stats.high_bytes
            ));
            content_bytes = cp437_to_utf8(&data).into_bytes();
        }
        DetectedEncoding::Latin1 => {
            result.changes_made.push(format!(
                "Converted {} high bytes from Latin1 to UTF-8",
                stats.high_bytes
            ));
            content_bytes = latin1_to_utf8(&data).into_bytes();
        }
        DetectedEncoding::Binary => unreachable!(),
    };

    // Convert to string for line ending normalization
    let mut content = String::from_utf8_lossy(&content_bytes).into_owned();

    // Normalize line endings (CRLF -> LF)
    if content.contains("\r\n") {
        content = content.replace("\r\n", "\n");
        result
            .changes_made
            .push("Normalized CRLF to LF line endings".to_string());
    }

    // Remove trailing CR (old Mac style)
    if content.contains('\r') {
        content = content.replace('\r', "\n");
        result
            .changes_made
            .push("Converted CR to LF line endings".to_string());
    }

    // Don't write if no changes
    if result.changes_made.is_empty()
        || (result.changes_made.len() == 1 && result.changes_made[0].starts_with("No changes"))
    {
        return result;
    }

    if dry_run {
        result
            .changes_made
            .push("(dry run - no files modified)".to_string());
        return result;
    }

    // Create backup if requested
    if backup {
        let backup_path = path.with_extension(format!(
            "{}.bak",
            path.extension()
                .map(|s| s.to_string_lossy())
                .unwrap_or_default()
        ));
        if let Err(e) = fs::copy(path, &backup_path) {
            result.error = Some(format!("Failed to create backup: {}", e));
            return result;
        }
        result
            .changes_made
            .push(format!("Created backup: {}", backup_path.display()));
    }

    // Write converted content
    if let Err(e) = fs::write(path, content.as_bytes()) {
        result.error = Some(format!("Failed to write file: {}", e));
        return result;
    }

    result
}

/// Find all .bas files in a directory
fn find_bas_files(dir: &Path, recursive: bool) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() && recursive {
                files.extend(find_bas_files(&path, true));
            } else if let Some(ext) = path.extension() {
                if ext.eq_ignore_ascii_case("bas") {
                    files.push(path);
                }
            }
        }
    }

    files
}

fn print_usage() {
    eprintln!("Usage: fix_encoding [OPTIONS] <FILE>...");
    eprintln!();
    eprintln!("Fix encoding issues in legacy BASIC source files.");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --dry-run       Show what would be changed without modifying files");
    eprintln!("  --backup        Create .bak backup files before modifying");
    eprintln!("  --encoding <E>  Force encoding: cp437, latin1, auto (default: auto)");
    eprintln!("  --recursive     Process directories recursively");
    eprintln!("  --verbose       Show detailed information about changes");
    eprintln!("  --analyze       Only analyze files, don't make any changes");
    eprintln!("  --help          Show this help message");
    eprintln!();
    eprintln!("Supported encodings:");
    eprintln!("  cp437   - IBM PC Code Page 437 (DOS box-drawing characters)");
    eprintln!("  latin1  - ISO-8859-1 (Western European)");
    eprintln!("  auto    - Auto-detect encoding (default)");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  fix_encoding --dry-run myfile.bas");
    eprintln!("  fix_encoding --backup --recursive ./legacy_code/");
    eprintln!("  fix_encoding --encoding cp437 oldgame.bas");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut dry_run = false;
    let mut backup = false;
    let mut recursive = false;
    let mut verbose = false;
    let mut analyze_only = false;
    let mut forced_encoding: Option<DetectedEncoding> = None;
    let mut files: Vec<PathBuf> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--dry-run" => dry_run = true,
            "--backup" => backup = true,
            "--recursive" => recursive = true,
            "--verbose" => verbose = true,
            "--analyze" => analyze_only = true,
            "--help" | "-h" => {
                print_usage();
                return;
            }
            "--encoding" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --encoding requires an argument");
                    std::process::exit(1);
                }
                forced_encoding = match args[i].to_lowercase().as_str() {
                    "cp437" => Some(DetectedEncoding::Cp437),
                    "latin1" => Some(DetectedEncoding::Latin1),
                    "auto" => None,
                    other => {
                        eprintln!(
                            "Error: unknown encoding '{}'. Use cp437, latin1, or auto.",
                            other
                        );
                        std::process::exit(1);
                    }
                };
            }
            arg if arg.starts_with('-') => {
                eprintln!("Error: unknown option '{}'", arg);
                std::process::exit(1);
            }
            path => {
                let p = PathBuf::from(path);
                if p.is_dir() {
                    files.extend(find_bas_files(&p, recursive));
                } else {
                    files.push(p);
                }
            }
        }
        i += 1;
    }

    if files.is_empty() {
        eprintln!("Error: no files specified");
        print_usage();
        std::process::exit(1);
    }

    if analyze_only {
        dry_run = true;
    }

    let mut total_files = 0;
    let mut files_with_issues = 0;
    let mut files_fixed = 0;
    let mut errors = 0;

    for path in &files {
        total_files += 1;

        if analyze_only {
            // Just analyze and report
            let data = match fs::read(path) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("Error reading {}: {}", path.display(), e);
                    errors += 1;
                    continue;
                }
            };

            let stats = EncodingStats::analyze(&data);
            let encoding = stats.detect_encoding();

            if encoding != DetectedEncoding::Ascii && encoding != DetectedEncoding::Utf8
                || stats.control_z_count > 0
                || stats.cr_lf_count > 0
            {
                files_with_issues += 1;
                println!("{}", path.display());
                println!("  Encoding: {:?}", encoding);
                println!("  Total bytes: {}", stats.total_bytes);
                println!("  High bytes (0x80-0xFF): {}", stats.high_bytes);
                if stats.control_z_count > 0 {
                    println!("  DOS Control-Z markers: {}", stats.control_z_count);
                }
                if stats.cr_lf_count > 0 {
                    println!("  CR/LF line endings: {}", stats.cr_lf_count);
                }
                if stats.binary_data_lines > 0 {
                    println!(
                        "  Binary DATA lines: {} ({} high bytes preserved)",
                        stats.binary_data_lines, stats.high_bytes_in_data
                    );
                    println!("  High bytes in code: {}", stats.high_bytes_in_code);
                }
                if verbose {
                    println!("  Box-drawing chars likely: {}", stats.box_drawing_likely);
                    println!("  Latin1 chars likely: {}", stats.latin1_likely);
                    println!(
                        "  Invalid UTF-8 sequences: {}",
                        stats.invalid_utf8_sequences
                    );
                }
                println!();
            } else if verbose {
                println!("{}: OK (ASCII/UTF-8)", path.display());
            }
        } else {
            // Process file
            let result = process_file(path, forced_encoding, dry_run, backup);

            if let Some(ref err) = result.error {
                eprintln!("Error processing {}: {}", path.display(), err);
                errors += 1;
                continue;
            }

            if result
                .changes_made
                .iter()
                .any(|s| !s.starts_with("No changes") && !s.starts_with("Skipped"))
            {
                files_with_issues += 1;
                if !dry_run {
                    files_fixed += 1;
                }

                println!("{}", path.display());
                for change in &result.changes_made {
                    println!("  {}", change);
                }
                if verbose {
                    println!("  Original encoding: {:?}", result.original_encoding);
                    println!("  High bytes: {}", result.high_byte_count);
                }
                println!();
            } else if verbose {
                println!("{}: No changes needed", path.display());
            }
        }
    }

    // Summary
    println!("---");
    println!("Summary:");
    println!("  Total files scanned: {}", total_files);
    println!("  Files with encoding issues: {}", files_with_issues);
    if !analyze_only && !dry_run {
        println!("  Files fixed: {}", files_fixed);
    }
    if errors > 0 {
        println!("  Errors: {}", errors);
    }

    if dry_run && !analyze_only && files_with_issues > 0 {
        println!();
        println!("Run without --dry-run to apply changes.");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cp437_mapping() {
        let map = cp437_to_unicode();

        // Test some box-drawing characters
        assert_eq!(map.get(&0xC4), Some(&'─')); // Horizontal line
        assert_eq!(map.get(&0xB3), Some(&'│')); // Vertical line
        assert_eq!(map.get(&0xC9), Some(&'╔')); // Top-left corner
        assert_eq!(map.get(&0xBB), Some(&'╗')); // Top-right corner
        assert_eq!(map.get(&0xDB), Some(&'█')); // Full block

        // Test some accented characters
        assert_eq!(map.get(&0x80), Some(&'Ç'));
        assert_eq!(map.get(&0x82), Some(&'é'));
    }

    #[test]
    fn test_encoding_detection_ascii() {
        let data = b"PRINT \"Hello, World!\"";
        let stats = EncodingStats::analyze(data);
        assert_eq!(stats.detect_encoding(), DetectedEncoding::Ascii);
    }

    #[test]
    fn test_encoding_detection_cp437() {
        // Data with box-drawing characters mixed with mostly ASCII content
        // Simulates a BASIC file with a box-drawn menu
        let data = b"REM MENU PROGRAM\nPRINT \"\xC9\xCD\xCD\xCD\xCD\xBB\"\nPRINT \"\xBA    \xBA\"\nPRINT \"\xC8\xCD\xCD\xCD\xCD\xBC\"\n";
        let stats = EncodingStats::analyze(data);
        assert_eq!(stats.detect_encoding(), DetectedEncoding::Cp437);
    }

    #[test]
    fn test_control_z_stripping() {
        let data = b"PRINT \"Hello\"\x1AGARBAGE";
        let stats = EncodingStats::analyze(data);
        assert_eq!(stats.control_z_count, 1);
    }

    #[test]
    fn test_cp437_conversion() {
        let data = [0xC9, 0xCD, 0xBB]; // ╔═╗
        let result = cp437_to_utf8(&data);
        assert_eq!(result, "╔═╗");
    }

    #[test]
    fn test_latin1_conversion() {
        let data = [0xE9, 0xE8, 0xEA]; // éèê
        let result = latin1_to_utf8(&data);
        assert_eq!(result, "éèê");
    }

    #[test]
    fn test_is_data_statement() {
        assert!(EncodingStats::is_data_statement(b"DATA 1, 2, 3"));
        assert!(EncodingStats::is_data_statement(b"  DATA 1, 2, 3"));
        assert!(EncodingStats::is_data_statement(b"100 DATA 1, 2, 3"));
        assert!(EncodingStats::is_data_statement(b"  100  DATA 1, 2, 3"));
        assert!(!EncodingStats::is_data_statement(b"PRINT \"DATA\""));
        assert!(!EncodingStats::is_data_statement(b"REM DATA statement"));
        assert!(!EncodingStats::is_data_statement(b"DATABASE = 1"));
    }

    #[test]
    fn test_binary_data_detection() {
        // Simulates a BASIC file with embedded sprite data in DATA statements
        // The DATA lines have lots of high bytes (binary sprite data)
        // The code lines are pure ASCII
        let data = b"REM SPRITE PROGRAM\n\
                     SCREEN 13\n\
                     DATA \"\xC9\xCD\xCD\xCD\xCD\xBB\xC9\xCD\xCD\xCD\xCD\xBB\xC9\xCD\xCD\xCD\"\n\
                     DATA \"\xBA\xFF\xFF\xFF\xFF\xBA\xBA\xFF\xFF\xFF\xFF\xBA\xBA\xFF\xFF\xFF\"\n\
                     DATA \"\xC8\xCD\xCD\xCD\xCD\xBC\xC8\xCD\xCD\xCD\xCD\xBC\xC8\xCD\xCD\xCD\"\n\
                     PRINT \"Loading sprites...\"\n";

        let stats = EncodingStats::analyze(data);

        // Should detect as BasicWithBinaryData because most high bytes are in DATA lines
        assert_eq!(
            stats.detect_encoding(),
            DetectedEncoding::BasicWithBinaryData
        );
        assert_eq!(stats.binary_data_lines, 3);
        assert!(stats.high_bytes_in_data > stats.high_bytes_in_code);
    }

    #[test]
    fn test_convert_preserving_data() {
        // A file with both box-drawing in comments AND binary DATA
        let data = b"REM \xC9\xCD\xCD\xBB Menu\n\
                     DATA \"\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\"\n\
                     PRINT \"Hello\"\n";

        let (converted, lines_converted, lines_preserved) =
            convert_basic_preserving_data(data, DetectedEncoding::BasicWithBinaryData);

        // Should have converted the REM line but preserved the DATA line
        assert_eq!(lines_converted, 1);
        assert_eq!(lines_preserved, 1);

        // The converted output should have UTF-8 box drawing in the REM line
        let converted_str = String::from_utf8_lossy(&converted);
        assert!(converted_str.contains("╔══╗"));

        // But the DATA line should still have raw bytes (not converted)
        // Check that we preserved the original bytes in DATA line
        assert!(converted.windows(4).any(|w| w == b"\xFF\xFF\xFF\xFF"));
    }
}
