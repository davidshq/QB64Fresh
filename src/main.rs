//! QB64Fresh - A modern BASIC compiler
//!
//! This is the command-line interface for the QB64Fresh compiler.

use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode, program_uses_opengl};
use qb64fresh::error_formatting::{format_parse_errors, format_semantic_errors};
use qb64fresh::lexer::{TokenKind, lex, lex_with_progress};
use qb64fresh::parser::Parser;
use qb64fresh::preprocessor::preprocess;
use qb64fresh::semantic::SemanticAnalyzer;

/// Default maximum size for raw input file (100MB).
const DEFAULT_MAX_INPUT_BYTES: usize = 100_000_000;

/// Gets the maximum allowed size for raw input from environment variable.
///
/// Reads `QB64FRESH_MAX_INPUT_BYTES` environment variable, or returns the default
/// (100MB) if not set or if parsing fails.
fn get_max_input_bytes() -> usize {
    std::env::var("QB64FRESH_MAX_INPUT_BYTES")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(DEFAULT_MAX_INPUT_BYTES)
}

/// Gets current RSS (Resident Set Size) in bytes, or 0 if unavailable.
///
/// On Linux, reads `/proc/self/status` and parses VmRSS. On other platforms,
/// returns 0 (no-op for diagnostics).
fn get_rss_bytes() -> usize {
    #[cfg(target_os = "linux")]
    {
        if let Ok(status) = std::fs::read_to_string("/proc/self/status") {
            for line in status.lines() {
                if line.starts_with("VmRSS:")
                    && let Some(value) = line.split_whitespace().nth(1)
                    && let Ok(kb) = value.parse::<usize>()
                {
                    return kb * 1024; // Convert KB to bytes
                }
            }
        }
    }
    0
}

/// Reports RSS if verbose mode or QB64FRESH_REPORT_RSS is set.
fn report_rss(phase: &str, verbose: bool) {
    let should_report = verbose
        || std::env::var("QB64FRESH_REPORT_RSS")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false);

    if should_report {
        let rss_bytes = get_rss_bytes();
        if rss_bytes > 0 {
            let rss_mib = rss_bytes as f64 / (1024.0 * 1024.0);
            eprintln!("[{}] RSS: {:.1} MiB", phase, rss_mib);
        }
    }
}

/// QB64Fresh - A modern BASIC compiler
#[derive(ClapParser, Debug)]
#[command(name = "qb64fresh")]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input BASIC source file (.bas)
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Output file (default: input name with .c extension)
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Only run lexer and print tokens (for debugging)
    #[arg(long)]
    tokens: bool,

    /// Parse and print AST (for debugging)
    #[arg(long)]
    ast: bool,

    /// Run semantic analysis and print typed IR (for debugging)
    #[arg(long)]
    typed_ir: bool,

    /// Generate C code and write to output file
    #[arg(long = "emit-c")]
    emit_c: bool,

    /// Runtime mode: 'external' (default, full graphics) or 'inline' (headless stubs)
    #[arg(long, default_value = "external")]
    runtime: String,

    /// Use headless mode with stub graphics (shorthand for --runtime inline)
    #[arg(long)]
    headless: bool,

    /// Enable debug mode (include debug hooks for breakpoints/stepping)
    #[arg(long)]
    debug: bool,

    /// Skip $INCLUDE preprocessing
    #[arg(long)]
    no_preprocess: bool,

    /// Disable SHELL and _SHELLHIDE when using --emit-c (compile error if used). No effect with --ast, --tokens, --typed-ir.
    #[arg(long)]
    no_shell: bool,

    /// Enable OpenGL code emission (#define QB64FRESH_OPENGL; link OpenGL when program uses SUB _GL or _GL*).
    #[arg(long)]
    opengl: bool,

    /// Disable OpenGL even if program uses SUB _GL or _GL* (no QB64FRESH_OPENGL).
    #[arg(long)]
    no_opengl: bool,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Write preprocessed source to file (for debugging)
    #[arg(long)]
    write_preprocessed: Option<PathBuf>,
}

fn main() {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let args = Args::parse();

    // Read source file - try UTF-8 first, fall back to latin1 (preserves any bytes)
    let raw_source = match fs::read_to_string(&args.input) {
        Ok(s) => s,
        Err(_) => {
            // UTF-8 failed, try reading as bytes and converting from latin1/cp1252
            // This handles legacy DOS/Windows files encoded in CP437/CP1252
            match fs::read(&args.input) {
                Ok(bytes) => {
                    // Convert bytes to string assuming latin1 (each byte maps to its Unicode value)
                    // Strip trailing Control-Z (0x1A) which DOS uses as EOF marker
                    let bytes: Vec<u8> = bytes.iter().copied().take_while(|&b| b != 0x1A).collect();
                    bytes.iter().map(|&b| b as char).collect::<String>()
                }
                Err(e) => {
                    eprintln!("Error reading '{}': {}", args.input.display(), e);
                    std::process::exit(1);
                }
            }
        }
    };

    if args.verbose {
        println!("Compiling: {}", args.input.display());
        println!("Source length: {} bytes", raw_source.len());
    }

    // Check raw input size
    let max_input_bytes = get_max_input_bytes();
    if raw_source.len() > max_input_bytes {
        eprintln!(
            "Error: Input file too large: {} bytes (maximum allowed: {} bytes)",
            raw_source.len(),
            max_input_bytes
        );
        eprintln!("To override this limit, set QB64FRESH_MAX_INPUT_BYTES environment variable.");
        eprintln!(
            "Alternatively, use 'ulimit -v' to limit virtual memory, or split your program into smaller modules."
        );
        std::process::exit(1);
    }

    report_rss("After read", args.verbose);

    // Preprocessor phase (expand $INCLUDE directives)
    let (source, embedded_files) = if args.no_preprocess {
        (raw_source, Vec::new())
    } else {
        let base_path = args.input.parent().unwrap_or(std::path::Path::new("."));
        match preprocess(&raw_source, base_path, Some(args.input.as_path())) {
            Ok(result) => {
                if args.verbose && result.source.len() != raw_source.len() {
                    println!(
                        "Preprocessor: expanded {} bytes -> {} bytes",
                        raw_source.len(),
                        result.source.len()
                    );
                }
                if args.verbose && !result.embedded_files.is_empty() {
                    println!(
                        "Preprocessor: collected {} embedded file(s)",
                        result.embedded_files.len()
                    );
                }
                (result.source, result.embedded_files)
            }
            Err(e) => {
                eprintln!("Preprocessor error: {}", e);
                std::process::exit(1);
            }
        }
    };

    report_rss("After preprocess", args.verbose);

    // Write preprocessed source if requested
    if let Some(preproc_path) = &args.write_preprocessed {
        if let Err(e) = fs::write(preproc_path, &source) {
            eprintln!(
                "Error writing preprocessed source to '{}': {}",
                preproc_path.display(),
                e
            );
        } else if args.verbose {
            println!("Wrote preprocessed source to: {}", preproc_path.display());
        }
    }

    // Lexer phase
    if args.verbose {
        eprintln!("[1/4] Lexing... (source: {} bytes)", source.len());
        // Flush stderr to ensure message appears immediately
        use std::io::Write;
        let _ = std::io::stderr().flush();
    }
    let tokens = if args.verbose && source.len() > 100_000 {
        // Use progress reporting for large files
        lex_with_progress(&source, true)
    } else {
        lex(&source)
    };
    if args.verbose {
        eprintln!("\r[1/4] Lexing complete: {} tokens", tokens.len());
        use std::io::Write;
        let _ = std::io::stderr().flush();
    }

    report_rss("After lex", args.verbose);

    if args.tokens {
        // Print tokens for debugging
        println!("Tokens for {}:", args.input.display());
        println!("{:-<60}", "");

        for token in &tokens {
            // Skip newlines in output for readability unless verbose
            if token.kind == TokenKind::Newline && !args.verbose {
                continue;
            }

            println!(
                "{:4}..{:<4} {:20} {:?}",
                token.span.start,
                token.span.end,
                format!("{:?}", token.kind),
                token.text
            );
        }
        return;
    }

    // Parser phase
    if args.verbose {
        eprintln!("[2/4] Parsing...");
    }
    let mut parser = Parser::new(&tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(errors) => {
            let file_path = args.input.to_string_lossy();
            let formatted = format_parse_errors(&source, &errors, &file_path);
            eprint!("{}", formatted);
            std::process::exit(1);
        }
    };
    if args.verbose {
        eprintln!(
            "[2/4] Parsing complete: {} statements",
            program.statements.len()
        );
    }

    report_rss("After parse", args.verbose);

    if args.ast {
        println!("AST for {}:", args.input.display());
        println!("{:-<60}", "");
        println!("Parsed {} statements:\n", program.statements.len());
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("{}. {:?}", i + 1, stmt);
            println!();
        }
        return;
    }

    // Semantic analysis phase
    if args.verbose {
        eprintln!("[3/4] Semantic analysis...");
    }
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = match analyzer.analyze(&program) {
        Ok(tp) => tp,
        Err(errors) => {
            let file_path = args.input.to_string_lossy();
            let formatted = format_semantic_errors(&source, &errors, &file_path);
            eprint!("{}", formatted);
            std::process::exit(1);
        }
    };
    if args.verbose {
        eprintln!(
            "[3/4] Semantic analysis complete: {} typed statements",
            typed_program.statements.len()
        );
    }

    report_rss("After semantic", args.verbose);

    if args.typed_ir {
        println!("Typed IR for {}:", args.input.display());
        println!("{:-<60}", "");
        println!("Analyzed {} statements:\n", typed_program.statements.len());
        for (i, stmt) in typed_program.statements.iter().enumerate() {
            println!("{}. {:?}", i + 1, stmt);
            println!();
        }
        return;
    }

    // Code generation phase
    if args.emit_c {
        // Parse runtime mode - headless flag overrides to inline (stub graphics)
        let runtime_mode = if args.headless {
            RuntimeMode::inline()
        } else {
            match args.runtime.to_lowercase().as_str() {
                "inline" => RuntimeMode::inline(),
                "external" => RuntimeMode::external(),
                other => {
                    eprintln!(
                        "Unknown runtime mode: '{}'. Use 'inline' or 'external'.",
                        other
                    );
                    std::process::exit(1);
                }
            }
        };

        if args.verbose {
            eprintln!("[4/4] Code generation...");
        }
        let runtime_mode_for_backend = runtime_mode.clone();
        let mut backend = CBackend::with_runtime_mode(runtime_mode_for_backend)
            .with_embedded_files(embedded_files);
        if args.debug {
            let source_file = args.input.to_string_lossy().to_string();
            backend = backend.with_debug(true).with_source_file(&source_file);
        }
        if args.no_shell {
            backend = backend.with_no_shell(true);
        }
        let uses_opengl = if args.no_opengl {
            false
        } else if args.opengl {
            true
        } else {
            program_uses_opengl(&typed_program)
        };
        backend = backend.with_opengl(uses_opengl);
        let output = match backend.generate(&typed_program) {
            Ok(o) => o,
            Err(errors) => {
                eprintln!("Code generation errors:");
                for err in &errors {
                    eprintln!("  {}", err);
                }
                std::process::exit(1);
            }
        };

        // Determine output file path
        let output_path = args.output.unwrap_or_else(|| {
            let mut p = args.input.clone();
            p.set_extension("c");
            p
        });

        report_rss("After codegen", args.verbose);

        match fs::write(&output_path, &output.code) {
            Ok(()) => {
                println!("Generated: {}", output_path.display());
                if args.verbose {
                    println!("Output size: {} bytes", output.code.len());
                    println!("Runtime mode: {:?}", runtime_mode);
                }
            }
            Err(e) => {
                eprintln!("Error writing '{}': {}", output_path.display(), e);
                std::process::exit(1);
            }
        }

        // Write resource files (icon.rc, manifest.h, *.manifest) if any
        if !output.resource_files.is_empty() {
            let output_dir = output_path
                .parent()
                .unwrap_or_else(|| std::path::Path::new("."));
            for (filename, content) in &output.resource_files {
                let resource_path = output_dir.join(filename);
                match fs::write(&resource_path, content) {
                    Ok(()) => {
                        println!("Generated: {}", resource_path.display());
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning: Error writing resource file '{}': {}",
                            resource_path.display(),
                            e
                        );
                    }
                }
            }
        }

        return;
    }

    // Default: show pipeline status
    println!("QB64Fresh v{}", env!("CARGO_PKG_VERSION"));
    println!();
    if !args.no_preprocess {
        println!("Preprocessor: OK ($INCLUDE expansion)");
    }
    println!("Lexer: OK ({} tokens)", tokens.len());
    println!("Parser: OK ({} statements)", program.statements.len());
    println!(
        "Semantic analysis: OK ({} typed statements)",
        typed_program.statements.len()
    );
    println!("Code generation: Ready (use --emit-c to generate C code)");
    println!();
    println!("Options:");
    println!("  --tokens        Show lexer output");
    println!("  --ast           Show parsed AST");
    println!("  --typed-ir      Show typed IR after semantic analysis");
    println!("  --emit-c        Generate C code to .c file");
    println!("  --no-preprocess Skip $INCLUDE preprocessing");
    println!(
        "  --no-shell      Disable SHELL/_SHELLHIDE when using --emit-c (compile error if used)"
    );
    println!(
        "  --opengl        Enable OpenGL (#define QB64FRESH_OPENGL; link OpenGL when program uses SUB _GL or _GL*)"
    );
    println!("  --no-opengl     Disable OpenGL even if program uses SUB _GL or _GL*");
}
