//! QB64Fresh Code Formatter CLI
//!
//! A command-line tool for formatting QB64/QBasic BASIC source files.
//!
//! ## Usage
//!
//! ```bash
//! # Format a file in place
//! qb64fresh-fmt myprogram.bas
//!
//! # Check if files are formatted (exit 1 if not)
//! qb64fresh-fmt --check *.bas
//!
//! # Format and write to stdout
//! qb64fresh-fmt --stdout myprogram.bas
//!
//! # Use specific style
//! qb64fresh-fmt --style qb64 myprogram.bas
//! ```

use clap::{Parser, ValueEnum};
use qb64fresh_fmt::{FormatError, Formatter, FormatterConfig, IndentStyle, KeywordCase};
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// QB64Fresh code formatter - format BASIC source files
#[derive(Parser, Debug)]
#[command(name = "qb64fresh-fmt")]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Files to format (use - for stdin)
    #[arg(required = true)]
    files: Vec<PathBuf>,

    /// Check if files are formatted without making changes
    #[arg(short, long)]
    check: bool,

    /// Write output to stdout instead of modifying files
    #[arg(long)]
    stdout: bool,

    /// Show diff of changes
    #[arg(long)]
    diff: bool,

    /// Create backup files before modifying (.bak extension)
    #[arg(short, long)]
    backup: bool,

    /// Process directories recursively
    #[arg(short, long)]
    recursive: bool,

    /// Formatting style preset
    #[arg(long, value_enum, default_value = "default")]
    style: StylePreset,

    /// Keyword capitalization style
    #[arg(long, value_enum)]
    keyword_case: Option<KeywordCaseArg>,

    /// Indentation style
    #[arg(long, value_enum)]
    indent_style: Option<IndentStyleArg>,

    /// Number of spaces per indent level
    #[arg(long)]
    indent_width: Option<usize>,

    /// Add spaces around operators
    #[arg(long)]
    space_around_operators: Option<bool>,

    /// Add space after commas
    #[arg(long)]
    space_after_comma: Option<bool>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Quiet mode - only show errors
    #[arg(short, long)]
    quiet: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum StylePreset {
    /// Default formatting style (uppercase keywords, 4-space indent)
    Default,
    /// Minimal changes (preserve original style)
    Minimal,
    /// QB64 IDE style
    Qb64,
    /// Pretty formatting with all enhancements
    Pretty,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum KeywordCaseArg {
    /// UPPERCASE keywords
    Upper,
    /// lowercase keywords
    Lower,
    /// Title Case keywords
    Title,
    /// Preserve original case
    Preserve,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum IndentStyleArg {
    /// Use spaces for indentation
    Spaces,
    /// Use tabs for indentation
    Tabs,
}

fn main() -> ExitCode {
    let args = Args::parse();

    // Build configuration
    let mut config = match args.style {
        StylePreset::Default => FormatterConfig::default(),
        StylePreset::Minimal => FormatterConfig::minimal(),
        StylePreset::Qb64 => FormatterConfig::qb64_style(),
        StylePreset::Pretty => FormatterConfig::pretty(),
    };

    // Apply command-line overrides
    if let Some(kc) = args.keyword_case {
        config.keyword_case = match kc {
            KeywordCaseArg::Upper => KeywordCase::Upper,
            KeywordCaseArg::Lower => KeywordCase::Lower,
            KeywordCaseArg::Title => KeywordCase::Title,
            KeywordCaseArg::Preserve => KeywordCase::Preserve,
        };
    }

    if let Some(is) = args.indent_style {
        config.indent_style = match is {
            IndentStyleArg::Spaces => IndentStyle::Spaces,
            IndentStyleArg::Tabs => IndentStyle::Tabs,
        };
    }

    if let Some(iw) = args.indent_width {
        config.indent_width = iw;
    }

    if let Some(sao) = args.space_around_operators {
        config.space_around_operators = sao;
    }

    if let Some(sac) = args.space_after_comma {
        config.space_after_comma = sac;
    }

    let formatter = Formatter::new(config);

    // Collect files to process
    let files = collect_files(&args.files, args.recursive);

    if files.is_empty() {
        if !args.quiet {
            eprintln!("No .bas files found to format");
        }
        return ExitCode::FAILURE;
    }

    let mut total_files = 0;
    let mut formatted_files = 0;
    let mut unchanged_files = 0;
    let mut error_count = 0;

    for file in &files {
        total_files += 1;

        match process_file(file, &formatter, &args) {
            Ok(ProcessResult::Formatted) => {
                formatted_files += 1;
                if args.verbose {
                    println!("Formatted: {}", file.display());
                }
            }
            Ok(ProcessResult::Unchanged) => {
                unchanged_files += 1;
                if args.verbose {
                    println!("Unchanged: {}", file.display());
                }
            }
            Ok(ProcessResult::WouldChange) => {
                formatted_files += 1;
                if !args.quiet {
                    println!("Would format: {}", file.display());
                }
            }
            Err(e) => {
                error_count += 1;
                eprintln!("Error processing {}: {}", file.display(), e);
            }
        }
    }

    // Print summary
    if !args.quiet {
        println!("---");
        println!("Summary:");
        println!("  Total files: {}", total_files);
        if args.check {
            println!(
                "  Would be formatted: {} ({} already formatted)",
                formatted_files, unchanged_files
            );
        } else {
            println!("  Formatted: {}", formatted_files);
            println!("  Unchanged: {}", unchanged_files);
        }
        if error_count > 0 {
            println!("  Errors: {}", error_count);
        }
    }

    // Exit code
    if error_count > 0 {
        ExitCode::FAILURE
    } else if args.check && formatted_files > 0 {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

#[derive(Debug)]
enum ProcessResult {
    Formatted,
    Unchanged,
    WouldChange,
}

fn process_file(
    path: &Path,
    formatter: &Formatter,
    args: &Args,
) -> Result<ProcessResult, FormatError> {
    // Handle stdin
    if path.as_os_str() == "-" {
        let mut source = String::new();
        io::stdin()
            .read_to_string(&mut source)
            .map_err(|e| FormatError::ReadError {
                path: path.to_path_buf(),
                source: e,
            })?;

        let formatted = formatter.format(&source)?;

        if args.check {
            if source == formatted {
                return Ok(ProcessResult::Unchanged);
            } else {
                return Ok(ProcessResult::WouldChange);
            }
        }

        io::stdout()
            .write_all(formatted.as_bytes())
            .map_err(|e| FormatError::WriteError {
                path: path.to_path_buf(),
                source: e,
            })?;

        return Ok(if source == formatted {
            ProcessResult::Unchanged
        } else {
            ProcessResult::Formatted
        });
    }

    // Read source file
    let source = fs::read_to_string(path).map_err(|e| FormatError::ReadError {
        path: path.to_path_buf(),
        source: e,
    })?;

    // Format
    let formatted = formatter.format(&source)?;

    // Check mode
    if args.check {
        return if source == formatted {
            Ok(ProcessResult::Unchanged)
        } else {
            if args.diff {
                print_diff(path, &source, &formatted);
            }
            Ok(ProcessResult::WouldChange)
        };
    }

    // Stdout mode
    if args.stdout {
        io::stdout()
            .write_all(formatted.as_bytes())
            .map_err(|e| FormatError::WriteError {
                path: path.to_path_buf(),
                source: e,
            })?;

        return Ok(if source == formatted {
            ProcessResult::Unchanged
        } else {
            ProcessResult::Formatted
        });
    }

    // No changes needed
    if source == formatted {
        return Ok(ProcessResult::Unchanged);
    }

    // Show diff if requested
    if args.diff {
        print_diff(path, &source, &formatted);
    }

    // Create backup if requested
    if args.backup {
        let backup_path = path.with_extension(format!(
            "{}.bak",
            path.extension()
                .map(|s| s.to_string_lossy())
                .unwrap_or_default()
        ));
        fs::copy(path, &backup_path).map_err(|e| FormatError::WriteError {
            path: backup_path,
            source: e,
        })?;
    }

    // Write formatted output
    fs::write(path, formatted.as_bytes()).map_err(|e| FormatError::WriteError {
        path: path.to_path_buf(),
        source: e,
    })?;

    Ok(ProcessResult::Formatted)
}

fn collect_files(paths: &[PathBuf], recursive: bool) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for path in paths {
        if path.as_os_str() == "-" {
            // Stdin
            files.push(path.clone());
        } else if path.is_dir() {
            files.extend(find_bas_files(path, recursive));
        } else if path.is_file() {
            files.push(path.clone());
        }
    }

    files
}

fn find_bas_files(dir: &Path, recursive: bool) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() && recursive {
                files.extend(find_bas_files(&path, true));
            } else if let Some(ext) = path.extension() {
                if ext.eq_ignore_ascii_case("bas")
                    || ext.eq_ignore_ascii_case("bi")
                    || ext.eq_ignore_ascii_case("bm")
                {
                    files.push(path);
                }
            }
        }
    }

    files
}

fn print_diff(path: &Path, original: &str, formatted: &str) {
    println!("--- {}", path.display());
    println!("+++ {} (formatted)", path.display());

    let original_lines: Vec<&str> = original.lines().collect();
    let formatted_lines: Vec<&str> = formatted.lines().collect();

    let max_lines = original_lines.len().max(formatted_lines.len());

    for i in 0..max_lines {
        let orig = original_lines.get(i).unwrap_or(&"");
        let fmt = formatted_lines.get(i).unwrap_or(&"");

        if orig != fmt {
            if i < original_lines.len() {
                println!("-{}", orig);
            }
            if i < formatted_lines.len() {
                println!("+{}", fmt);
            }
        }
    }
    println!();
}
