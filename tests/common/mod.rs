//! Common test utilities for QB64Fresh integration tests
//!
//! This module provides shared helpers used across multiple test files.

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;

/// Compile BASIC source code to C, returning Ok(code) or Err(message)
pub fn compile_to_c(source: &str) -> Result<String, String> {
    compile_with_mode(source, RuntimeMode::inline())
}

/// Compile BASIC source code with a specific runtime mode
pub fn compile_with_mode(source: &str, mode: RuntimeMode) -> Result<String, String> {
    // Lexer phase
    let tokens = lex(source);

    // Parser phase
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|errors| format!("Parse errors: {:?}", errors))?;

    // Semantic analysis phase
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|errors| format!("Semantic errors: {:?}", errors))?;

    // Code generation phase
    let backend = CBackend::with_runtime_mode(mode);
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {}", e))?;

    Ok(output.code)
}

/// Assert that source compiles successfully
pub fn assert_compiles(source: &str) {
    match compile_to_c(source) {
        Ok(_) => {}
        Err(e) => panic!("Expected compilation to succeed, but got: {}", e),
    }
}

/// Assert that source fails to compile with error containing substring
pub fn assert_compile_error(source: &str, expected_substring: &str) {
    match compile_to_c(source) {
        Ok(_) => panic!("Expected compilation to fail, but it succeeded"),
        Err(e) => {
            assert!(
                e.to_lowercase().contains(&expected_substring.to_lowercase()),
                "Expected error containing '{}', but got: {}",
                expected_substring,
                e
            );
        }
    }
}

/// Count the number of tokens in source code
pub fn token_count(source: &str) -> usize {
    lex(source).len()
}

/// Parse source and return the number of statements
pub fn statement_count(source: &str) -> Result<usize, String> {
    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|e| format!("Parse error: {:?}", e))?;
    Ok(program.statements.len())
}
