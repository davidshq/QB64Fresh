//! Fuzz target for the lexer.
//!
//! This fuzzer tests that the lexer never panics on arbitrary input.
//! The lexer should gracefully handle any input, returning an error
//! or producing tokens for invalid input without crashing.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string (may be invalid UTF-8)
    if let Ok(input) = std::str::from_utf8(data) {
        // The lexer should never panic, even on arbitrary input
        let _ = qb64fresh::lexer::lex(input);
    }
});
