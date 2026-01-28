//! QB64Fresh - A modern BASIC compiler
//!
//! This is the command-line interface for the QB64Fresh compiler.

use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::{TokenKind, lex, lex_with_progress};
use qb64fresh::parser::Parser;
use qb64fresh::preprocessor::preprocess;
use qb64fresh::semantic::SemanticAnalyzer;

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

    // Preprocessor phase (expand $INCLUDE directives)
    let source = if args.no_preprocess {
        raw_source
    } else {
        let base_path = args.input.parent().unwrap_or(std::path::Path::new("."));
        match preprocess(&raw_source, base_path, Some(args.input.as_path())) {
            Ok(s) => {
                if args.verbose && s.len() != raw_source.len() {
                    println!(
                        "Preprocessor: expanded {} bytes -> {} bytes",
                        raw_source.len(),
                        s.len()
                    );
                }
                s
            }
            Err(e) => {
                eprintln!("Preprocessor error: {}", e);
                std::process::exit(1);
            }
        }
    };

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
            eprintln!("Parse errors:");
            for err in &errors {
                // Compute line number from span if available
                if let Some(span) = err.span() {
                    let line = if span.start <= source.len() {
                        source[..span.start].chars().filter(|&c| c == '\n').count() + 1
                    } else {
                        1 // Fallback if span is out of bounds
                    };
                    eprintln!("  line {}: {}", line, err);
                } else {
                    eprintln!("  {}", err);
                }
            }
            std::process::exit(1);
        }
    };
    if args.verbose {
        eprintln!(
            "[2/4] Parsing complete: {} statements",
            program.statements.len()
        );
    }

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
            eprintln!("Semantic errors:");
            for err in &errors {
                // Compute line number from span
                let span = err.span();
                let line = if span.start <= source.len() {
                    source[..span.start].chars().filter(|&c| c == '\n').count() + 1
                } else {
                    1 // Fallback if span is out of bounds
                };
                eprintln!("  line {}: {}", line, err);
            }
            std::process::exit(1);
        }
    };
    if args.verbose {
        eprintln!(
            "[3/4] Semantic analysis complete: {} typed statements",
            typed_program.statements.len()
        );
    }

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
        let mut backend = CBackend::with_runtime_mode(runtime_mode_for_backend);
        if args.debug {
            let source_file = args.input.to_string_lossy().to_string();
            backend = backend.with_debug(true).with_source_file(&source_file);
        }
        if args.no_shell {
            backend = backend.with_no_shell(true);
        }
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
}
