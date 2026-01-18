//! Fuzz target for the parser.
//!
//! This fuzzer tests that the parser never panics on arbitrary token sequences.
//! The parser should gracefully handle any input, returning parse errors
//! without crashing.

#![no_main]

use libfuzzer_sys::fuzz_target;
use qb64fresh::parser::Parser;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string (may be invalid UTF-8)
    if let Ok(input) = std::str::from_utf8(data) {
        // First lex the input
        let tokens = qb64fresh::lexer::lex(input);

        // Then try to parse - should never panic
        let _ = Parser::new(&tokens).parse();
    }
});
