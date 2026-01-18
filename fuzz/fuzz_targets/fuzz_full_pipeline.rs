//! Fuzz target for the full compilation pipeline.
//!
//! This fuzzer tests that the entire compilation pipeline (lexer -> parser ->
//! semantic analyzer -> code generator) never panics on arbitrary input.
//! Each stage should gracefully handle errors from previous stages.

#![no_main]

use libfuzzer_sys::fuzz_target;
use qb64fresh::codegen::c_backend::CBackend;
use qb64fresh::codegen::{CodeGenerator, RuntimeMode};
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string (may be invalid UTF-8)
    if let Ok(input) = std::str::from_utf8(data) {
        // Lex
        let tokens = qb64fresh::lexer::lex(input);

        // Parse
        if let Ok(ast) = Parser::new(&tokens).parse() {
            // Semantic analysis
            let mut analyzer = SemanticAnalyzer::new();
            if let Ok(typed_program) = analyzer.analyze(&ast) {
                // Code generation
                let backend = CBackend::new(RuntimeMode::Inline);
                let _ = backend.generate(&typed_program);
            }
        }
    }
});
