//! Bootstrap tests for QB64pe compilation
//!
//! These tests verify that QB64Fresh can successfully compile the QB64pe
//! compiler source code (~59,000 lines of BASIC across 39 files).
//!
//! Test categories:
//! - Compilation tests: Verify QB64pe compiles without errors
//! - Golden tests: Detect changes in generated C code
//! - (Future) Execution tests: Verify compiled QB64pe can compile BASIC programs

use std::path::Path;
use std::time::Instant;

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::preprocessor::preprocess_file;
use qb64fresh::semantic::SemanticAnalyzer;

/// Path to QB64pe source (relative to workspace root)
const QB64PE_SOURCE: &str = "../QB64pe/source/qb64pe.bas";

/// Get the path to QB64pe source file
fn qb64pe_path() -> std::path::PathBuf {
    // Get the workspace root (parent of tests directory)
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest_dir).join(QB64PE_SOURCE)
}

/// Helper to compile QB64pe through all stages
fn compile_qb64pe() -> Result<CompilationResult, String> {
    let path = qb64pe_path();

    if !path.exists() {
        return Err(format!(
            "QB64pe source not found at: {}\n\
             Make sure QB64pe is checked out alongside QB64Fresh",
            path.display()
        ));
    }

    let start = Instant::now();

    // Preprocess (handles $INCLUDE directives)
    let source = preprocess_file(&path).map_err(|e| format!("Preprocessor error: {}", e))?;
    let preprocess_time = start.elapsed();

    // Lexer
    let lex_start = Instant::now();
    let tokens = lex(&source);
    let lex_time = lex_start.elapsed();

    // Parser
    let parse_start = Instant::now();
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|errors| format!("Parse errors ({} total): {:?}", errors.len(), errors))?;
    let parse_time = parse_start.elapsed();

    // Semantic analysis
    let semantic_start = Instant::now();
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|errors| format!("Semantic errors ({} total): {:?}", errors.len(), errors))?;
    let semantic_time = semantic_start.elapsed();

    // Code generation
    let codegen_start = Instant::now();
    let backend = CBackend::with_runtime_mode(RuntimeMode::External);
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {}", e))?;
    let codegen_time = codegen_start.elapsed();

    Ok(CompilationResult {
        source_bytes: source.len(),
        token_count: tokens.len(),
        statement_count: program.statements.len(),
        c_code_bytes: output.code.len(),
        c_code_lines: output.code.lines().count(),
        preprocess_time,
        lex_time,
        parse_time,
        semantic_time,
        codegen_time,
        total_time: start.elapsed(),
        c_code: output.code,
    })
}

/// Results from compiling QB64pe
#[derive(Debug)]
struct CompilationResult {
    source_bytes: usize,
    token_count: usize,
    statement_count: usize,
    c_code_bytes: usize,
    c_code_lines: usize,
    preprocess_time: std::time::Duration,
    lex_time: std::time::Duration,
    parse_time: std::time::Duration,
    semantic_time: std::time::Duration,
    codegen_time: std::time::Duration,
    total_time: std::time::Duration,
    c_code: String,
}

// ============================================================================
// Compilation Tests
// ============================================================================

/// Test that QB64pe source compiles without any errors.
/// This is the primary regression test for the bootstrap project.
///
/// Note: Uses a spawned thread with larger stack to handle QB64pe's deep AST.
#[test]
fn qb64pe_compiles_successfully() {
    // QB64pe has very deep nested structures requiring larger stack
    let builder = std::thread::Builder::new()
        .name("qb64pe_compile".into())
        .stack_size(16 * 1024 * 1024); // 16MB stack

    let handle = builder
        .spawn(|| compile_qb64pe())
        .expect("Failed to spawn thread");

    let result = handle.join().expect("Thread panicked");

    match result {
        Ok(stats) => {
            println!("\n=== QB64pe Compilation Successful ===");
            println!(
                "Source size:     {} bytes ({:.2} MB)",
                stats.source_bytes,
                stats.source_bytes as f64 / 1_000_000.0
            );
            println!("Tokens:          {}", stats.token_count);
            println!("Statements:      {}", stats.statement_count);
            println!(
                "C output:        {} lines ({:.2} MB)",
                stats.c_code_lines,
                stats.c_code_bytes as f64 / 1_000_000.0
            );
            println!("\nTiming:");
            println!("  Preprocess:    {:?}", stats.preprocess_time);
            println!("  Lexer:         {:?}", stats.lex_time);
            println!("  Parser:        {:?}", stats.parse_time);
            println!("  Semantic:      {:?}", stats.semantic_time);
            println!("  CodeGen:       {:?}", stats.codegen_time);
            println!("  Total:         {:?}", stats.total_time);
        }
        Err(e) => {
            panic!("QB64pe compilation failed:\n{}", e);
        }
    }
}

/// Verify the generated C code has expected characteristics.
/// This catches regressions in code generation size/structure.
#[test]
fn qb64pe_c_output_sanity_checks() {
    // Use larger stack for deep AST processing
    let builder = std::thread::Builder::new()
        .name("qb64pe_sanity".into())
        .stack_size(16 * 1024 * 1024);

    let handle = builder
        .spawn(|| compile_qb64pe())
        .expect("Failed to spawn thread");

    let result = handle
        .join()
        .expect("Thread panicked")
        .expect("QB64pe should compile");

    // The generated C code should be substantial
    // (QB64pe is ~59K lines of BASIC, generates ~86K lines of C)
    assert!(
        result.c_code_lines > 50_000,
        "Expected >50K lines of C, got {}",
        result.c_code_lines
    );

    // Should include standard runtime functions
    assert!(
        result.c_code.contains("qb_print"),
        "Generated C should include qb_print"
    );

    // Should have main function
    assert!(
        result.c_code.contains("int main("),
        "Generated C should have main function"
    );

    // Should have TYPE definitions from QB64pe
    // (QB64pe defines many types like usedVarList, etc.)
    assert!(
        result.c_code.contains("typedef struct"),
        "Generated C should have struct definitions"
    );
}

/// Verify that QB64pe can be parsed (lexer + parser only).
/// Faster test for catching parse regressions.
#[test]
fn qb64pe_parses_successfully() {
    let path = qb64pe_path();

    if !path.exists() {
        println!("Skipping: QB64pe source not found at {}", path.display());
        return;
    }

    let source = preprocess_file(&path).expect("Preprocessor should succeed");

    let tokens = lex(&source);
    let mut parser = Parser::new(&tokens);

    match parser.parse() {
        Ok(program) => {
            println!("Parse successful: {} statements", program.statements.len());
            assert!(
                program.statements.len() > 2000,
                "Expected >2000 statements, got {}",
                program.statements.len()
            );
        }
        Err(errors) => {
            // Debug: show problematic lines
            let lines: Vec<&str> = source.lines().collect();
            println!("\n=== DEBUG: Source context around errors ===");
            for err in errors.iter().take(5) {
                // Extract line number from error span
                let span_start = match err {
                    qb64fresh::parser::ParseError::InvalidStatement { span, .. } => span.start,
                    qb64fresh::parser::ParseError::InvalidExpression { span, .. } => span.start,
                    qb64fresh::parser::ParseError::UnexpectedToken { span, .. } => span.start,
                    _ => continue,
                };
                // Find line number from byte offset
                let mut line_num = 0;
                let mut byte_count = 0;
                for (i, line) in lines.iter().enumerate() {
                    if byte_count + line.len() + 1 > span_start {
                        line_num = i;
                        break;
                    }
                    byte_count += line.len() + 1; // +1 for newline
                }
                println!("\nError near line {} (byte {}):", line_num + 1, span_start);
                for i in line_num.saturating_sub(3)..=(line_num + 3).min(lines.len() - 1) {
                    let marker = if i == line_num { ">>>" } else { "   " };
                    println!("{} {:5}: {}", marker, i + 1, lines[i]);
                }
            }
            panic!("Parse failed with {} errors:\n{:?}", errors.len(), errors);
        }
    }
}

// ============================================================================
// Golden File Tests (C output stability)
// ============================================================================

/// Test that a small QB64pe subset produces stable C output.
/// Uses a representative subset to avoid storing 4MB golden files.
#[test]
#[ignore = "Golden file not yet created - run with UPDATE_GOLDEN=1 to create"]
fn qb64pe_codegen_golden() {
    // This test would compare against a golden file
    // For now, it's marked ignore until we decide on golden file strategy
    todo!("Implement golden file comparison for QB64pe subset");
}

// ============================================================================
// Execution Tests (require runtime - placeholder)
// ============================================================================

/// Placeholder for future execution test.
/// Will verify that QB64pe compiled by QB64Fresh can compile a simple program.
#[test]
#[ignore = "Requires real runtime implementation"]
fn qb64pe_can_compile_hello_world() {
    // Future test outline:
    // 1. Compile QB64pe with QB64Fresh
    // 2. Build the executable (gcc)
    // 3. Run QB64pe on a simple "PRINT Hello" program
    // 4. Verify it produces valid output
    todo!("Implement once runtime stubs are replaced with real implementations");
}

// ============================================================================
// Regression Tests
// ============================================================================

/// Test specific features that caused issues during bootstrap development.
/// Each test documents a bug that was fixed.
mod regression_tests {
    use super::*;

    /// Regression: Function calls must use canonical name with type suffix.
    /// Bug: `qb_getelement$` was called as `qb_getelement` causing linker errors.
    #[test]
    fn function_call_uses_canonical_name() {
        // This is tested implicitly by qb64pe_compiles_successfully
        // but we document it here for future reference
        let source = r#"
            FUNCTION GetValue$
                GetValue$ = "test"
            END FUNCTION
            PRINT GetValue$
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
        let output = backend.generate(&typed).expect("Should generate");

        // The call should use the canonical name with _str suffix
        assert!(
            output.code.contains("getvalue_str"),
            "Function call should use canonical name: {}",
            output.code
        );
    }

    /// Regression: Dual namespace model - arrays and scalars can share names.
    /// Bug: `sf` scalar and `SF()` array couldn't coexist.
    #[test]
    fn dual_namespace_arrays_and_scalars() {
        let source = r#"
            DIM sf AS INTEGER
            DIM SF(10) AS INTEGER
            sf = 5
            SF(1) = 10
            PRINT sf; SF(1)
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        analyzer
            .analyze(&program)
            .expect("Should analyze - dual namespace");
    }
}
