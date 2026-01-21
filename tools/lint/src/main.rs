//! QB64Fresh Code Linter CLI
//!
//! A command-line tool for linting QB64/QBasic BASIC source files.
//!
//! ## Usage
//!
//! ```bash
//! # Lint a file
//! qb64fresh-lint myprogram.bas
//!
//! # Lint multiple files
//! qb64fresh-lint *.bas
//!
//! # Lint with strict mode (warnings as errors)
//! qb64fresh-lint --strict myprogram.bas
//!
//! # Lint with specific rules enabled/disabled
//! qb64fresh-lint --enable unused_variable --disable goto_usage myprogram.bas
//!
//! # Show all available rules
//! qb64fresh-lint --list-rules
//! ```

use clap::{Parser, ValueEnum};
use qb64fresh_lint::{DiagnosticCounts, LintConfig, Linter, Severity};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// QB64Fresh code linter - check BASIC source files for issues
#[derive(Parser, Debug)]
#[command(name = "qb64fresh-lint")]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Files to lint (use - for stdin)
    #[arg(required_unless_present = "list_rules")]
    files: Vec<PathBuf>,

    /// Preset configuration
    #[arg(long, value_enum, default_value = "default")]
    preset: Preset,

    /// Treat warnings as errors
    #[arg(long)]
    strict: bool,

    /// Enable specific rules (can be repeated)
    #[arg(long = "enable", short = 'e', value_name = "RULE")]
    enable_rules: Vec<String>,

    /// Disable specific rules (can be repeated)
    #[arg(long = "disable", short = 'd', value_name = "RULE")]
    disable_rules: Vec<String>,

    /// Process directories recursively
    #[arg(short, long)]
    recursive: bool,

    /// Output format
    #[arg(long, value_enum, default_value = "text")]
    format: OutputFormat,

    /// Maximum number of diagnostics to show (0 = unlimited)
    #[arg(long, default_value = "0")]
    max_diagnostics: usize,

    /// Show only errors (suppress warnings and hints)
    #[arg(long)]
    errors_only: bool,

    /// List all available lint rules
    #[arg(long)]
    list_rules: bool,

    /// Configuration file path
    #[arg(long, value_name = "FILE")]
    config: Option<PathBuf>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Quiet mode - only show errors and summary
    #[arg(short, long)]
    quiet: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Preset {
    /// Default configuration (correctness rules only)
    Default,
    /// Permissive configuration (minimal warnings)
    Permissive,
    /// Strict configuration (warnings as errors)
    Strict,
    /// Pedantic configuration (all rules enabled)
    Pedantic,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum OutputFormat {
    /// Human-readable text output
    Text,
    /// JSON output for tooling integration
    Json,
    /// Compact one-line-per-diagnostic output
    Compact,
}

fn main() -> ExitCode {
    let args = Args::parse();

    // Handle --list-rules
    if args.list_rules {
        return list_rules();
    }

    // Build configuration
    let mut config = match args.preset {
        Preset::Default => LintConfig::default(),
        Preset::Permissive => LintConfig::permissive(),
        Preset::Strict => LintConfig::strict(),
        Preset::Pedantic => LintConfig::pedantic(),
    };

    // Load config file if specified, or discover one
    if let Some(config_path) = &args.config {
        match LintConfig::from_file(config_path) {
            Ok(file_config) => config = file_config,
            Err(e) => {
                eprintln!("Error loading config: {}", e);
                return ExitCode::FAILURE;
            }
        }
    } else if let Some(discovered) = LintConfig::discover() {
        config = discovered;
    }

    // Apply command-line overrides
    if args.strict {
        config.default_severity = Severity::Error;
    }

    for rule in &args.enable_rules {
        config.set_rule_severity(rule, config.default_severity);
    }

    for rule in &args.disable_rules {
        config.set_rule_severity(rule, Severity::Off);
    }

    config.max_diagnostics = args.max_diagnostics;

    // Create linter
    let linter = Linter::new(config);

    // Collect files to process
    let files = collect_files(&args.files, args.recursive);

    if files.is_empty() {
        if !args.quiet {
            eprintln!("No .bas files found to lint");
        }
        return ExitCode::FAILURE;
    }

    // Process files
    let mut total_counts = DiagnosticCounts::default();
    let mut file_count = 0;
    let mut error_count = 0;

    for file in &files {
        file_count += 1;

        match lint_file(file, &linter, &args) {
            Ok(counts) => {
                total_counts.errors += counts.errors;
                total_counts.warnings += counts.warnings;
                total_counts.hints += counts.hints;
            }
            Err(e) => {
                error_count += 1;
                eprintln!("Error linting {}: {}", file.display(), e);
            }
        }
    }

    // Print summary
    if !args.quiet {
        println!();
        println!("────────────────────────────────────────");
        println!("Summary:");
        println!("  Files checked: {}", file_count);
        if error_count > 0 {
            println!("  Files with errors: {}", error_count);
        }
        println!(
            "  Diagnostics: {} errors, {} warnings, {} hints",
            total_counts.errors, total_counts.warnings, total_counts.hints
        );
    }

    // Determine exit code
    if error_count > 0 || total_counts.errors > 0 {
        ExitCode::from(1)
    } else if args.strict && total_counts.warnings > 0 {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn lint_file(path: &Path, linter: &Linter, args: &Args) -> Result<DiagnosticCounts, String> {
    // Handle stdin
    let source = if path.as_os_str() == "-" {
        use std::io::Read;
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .map_err(|e| e.to_string())?;
        buf
    } else {
        fs::read_to_string(path).map_err(|e| e.to_string())?
    };

    let diagnostics = linter.check_source(&source).map_err(|e| e.to_string())?;

    // Filter if errors_only
    let diagnostics: Vec<_> = if args.errors_only {
        diagnostics
            .into_iter()
            .filter(|d| d.severity == Severity::Error)
            .collect()
    } else {
        diagnostics
    };

    let counts = DiagnosticCounts::from_diagnostics(&diagnostics);

    // Output diagnostics
    if !diagnostics.is_empty() || args.verbose {
        match args.format {
            OutputFormat::Text => {
                output_text(path, &source, &diagnostics, args.verbose);
            }
            OutputFormat::Json => {
                output_json(path, &diagnostics);
            }
            OutputFormat::Compact => {
                output_compact(path, &source, &diagnostics);
            }
        }
    }

    Ok(counts)
}

fn output_text(
    path: &Path,
    source: &str,
    diagnostics: &[qb64fresh_lint::LintDiagnostic],
    verbose: bool,
) {
    if diagnostics.is_empty() {
        if verbose {
            println!("{}: OK", path.display());
        }
        return;
    }

    println!("{}:", path.display());

    for diag in diagnostics {
        let (line, col) = offset_to_line_col(source, diag.span.start);

        let severity_str = match diag.severity {
            Severity::Error => "\x1b[31merror\x1b[0m",
            Severity::Warning => "\x1b[33mwarning\x1b[0m",
            Severity::Hint => "\x1b[34mhint\x1b[0m",
            Severity::Off => "off",
        };

        println!(
            "  {}:{}:{}: {}: {} [{}]",
            path.display(),
            line,
            col,
            severity_str,
            diag.message,
            diag.rule_name
        );

        // Show the source line
        if let Some(source_line) = source.lines().nth(line.saturating_sub(1)) {
            println!("    {}", source_line);
            // Show caret pointing to the issue
            let caret_offset = col.saturating_sub(1);
            println!("    {}^", " ".repeat(caret_offset));
        }

        if let Some(suggestion) = &diag.suggestion {
            println!("    \x1b[32msuggestion\x1b[0m: {}", suggestion);
        }

        for note in &diag.notes {
            println!("    \x1b[34mnote\x1b[0m: {}", note);
        }

        println!();
    }
}

fn output_json(path: &Path, diagnostics: &[qb64fresh_lint::LintDiagnostic]) {
    #[derive(serde::Serialize)]
    struct JsonDiagnostic {
        file: String,
        rule: &'static str,
        severity: String,
        message: String,
        start: usize,
        end: usize,
        suggestion: Option<String>,
    }

    let json_diags: Vec<JsonDiagnostic> = diagnostics
        .iter()
        .map(|d| JsonDiagnostic {
            file: path.display().to_string(),
            rule: d.rule_name,
            severity: d.severity.name().to_string(),
            message: d.message.clone(),
            start: d.span.start,
            end: d.span.end,
            suggestion: d.suggestion.clone(),
        })
        .collect();

    if let Ok(json) = serde_json::to_string_pretty(&json_diags) {
        println!("{}", json);
    }
}

fn output_compact(path: &Path, source: &str, diagnostics: &[qb64fresh_lint::LintDiagnostic]) {
    for diag in diagnostics {
        let (line, col) = offset_to_line_col(source, diag.span.start);
        println!(
            "{}:{}:{}: {}: {} [{}]",
            path.display(),
            line,
            col,
            diag.severity.name(),
            diag.message,
            diag.rule_name
        );
    }
}

fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

fn collect_files(paths: &[PathBuf], recursive: bool) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for path in paths {
        if path.as_os_str() == "-" {
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

fn list_rules() -> ExitCode {
    println!("Available lint rules:");
    println!();

    // Create a temporary registry to get rule info
    let registry = qb64fresh_lint::rules::RuleRegistry::new();

    for rule in registry.iter() {
        let info = rule.info();
        println!("  \x1b[1m{}\x1b[0m [{}]", info.name, info.category.name());
        println!("    {}", info.description);
        println!("    Default severity: {}", info.default_severity.name());
        println!();
    }

    println!("Use --enable <rule> or --disable <rule> to control individual rules.");
    println!("Use --preset <preset> to select a configuration preset.");

    ExitCode::SUCCESS
}
