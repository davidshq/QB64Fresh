//! QB64Fresh - A modern BASIC compiler
//!
//! This is the command-line interface for the QB64Fresh compiler.

use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;

use qb64fresh::lexer::{TokenKind, lex};
use qb64fresh::parser::Parser;

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
    if args.ast {
        println!("AST for {}:", args.input.display());
        println!("{:-<60}", "");

        let mut parser = Parser::new(&tokens);
        match parser.parse() {
            Ok(program) => {
                println!("Parsed {} statements:\n", program.statements.len());
                for (i, stmt) in program.statements.iter().enumerate() {
                    println!("{}. {:?}", i + 1, stmt);
                    println!();
                }
            }
            Err(errors) => {
                eprintln!("Parse errors:");
                for err in errors {
                    eprintln!("  {}", err);
                }
                std::process::exit(1);
            }
        }
        return;
    }

    // Default: show pipeline status
    let mut parser = Parser::new(&tokens);
    let parse_result = parser.parse();

    println!("QB64Fresh v{}", env!("CARGO_PKG_VERSION"));
    println!();
    println!("Lexer: OK ({} tokens)", tokens.len());

    match parse_result {
        Ok(program) => {
            println!("Parser: OK ({} statements)", program.statements.len());
        }
        Err(errors) => {
            println!("Parser: FAILED ({} errors)", errors.len());
            for err in &errors {
                eprintln!("  {}", err);
            }
        }
    }

    println!("Semantic analysis: Not yet implemented");
    println!("Code generation: Not yet implemented");
    println!();
    println!("Use --tokens to see lexer output.");
    println!("Use --ast to see parsed AST.");
}
