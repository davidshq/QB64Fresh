//! QB64Fresh - A modern BASIC compiler
//!
//! This is the command-line interface for the QB64Fresh compiler.

use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::{TokenKind, lex};
use qb64fresh::parser::Parser;
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

    /// Runtime mode: 'inline' (default) or 'external'
    #[arg(long, default_value = "inline")]
    runtime: String,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

fn main() {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let args = Args::parse();

    // Read source file
    let source = match fs::read_to_string(&args.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading '{}': {}", args.input.display(), e);
            std::process::exit(1);
        }
    };

    if args.verbose {
        println!("Compiling: {}", args.input.display());
        println!("Source length: {} bytes", source.len());
    }

    // Lexer phase
    let tokens = lex(&source);

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
    let mut parser = Parser::new(&tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(errors) => {
            eprintln!("Parse errors:");
            for err in errors {
                eprintln!("  {}", err);
            }
            std::process::exit(1);
        }
    };

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
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = match analyzer.analyze(&program) {
        Ok(tp) => tp,
        Err(errors) => {
            eprintln!("Semantic errors:");
            for err in &errors {
                eprintln!("  {}", err);
            }
            std::process::exit(1);
        }
    };

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
        // Parse runtime mode
        let runtime_mode = match args.runtime.to_lowercase().as_str() {
            "inline" => RuntimeMode::Inline,
            "external" => RuntimeMode::External,
            other => {
                eprintln!(
                    "Unknown runtime mode: '{}'. Use 'inline' or 'external'.",
                    other
                );
                std::process::exit(1);
            }
        };

        let backend = CBackend::with_runtime_mode(runtime_mode);
        let output = match backend.generate(&typed_program) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("Code generation error: {}", e);
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
    println!("Lexer: OK ({} tokens)", tokens.len());
    println!("Parser: OK ({} statements)", program.statements.len());
    println!(
        "Semantic analysis: OK ({} typed statements)",
        typed_program.statements.len()
    );
    println!("Code generation: Ready (use --emit-c to generate C code)");
    println!();
    println!("Options:");
    println!("  --tokens     Show lexer output");
    println!("  --ast        Show parsed AST");
    println!("  --typed-ir   Show typed IR after semantic analysis");
    println!("  --emit-c     Generate C code to .c file");
}
