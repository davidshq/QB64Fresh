//! Property-based tests using proptest
//!
//! These tests verify that the compiler never panics on arbitrary input,
//! even when the input is syntactically invalid. This is important for
//! security and robustness - a compiler should gracefully handle any input.

use proptest::prelude::*;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;

/// Generate arbitrary ASCII strings (basic characters that might appear in source)
fn arbitrary_source() -> impl Strategy<Value = String> {
    proptest::collection::vec(any::<u8>(), 0..1000).prop_map(|bytes| {
        // Convert to string, replacing invalid UTF-8
        String::from_utf8_lossy(&bytes).into_owned()
    })
}

/// Generate BASIC-like source code (more realistic inputs)
fn basic_like_source() -> impl Strategy<Value = String> {
    // Keywords and fragments that might appear in BASIC code
    let keywords = prop_oneof![
        Just("PRINT".to_string()),
        Just("DIM".to_string()),
        Just("IF".to_string()),
        Just("THEN".to_string()),
        Just("ELSE".to_string()),
        Just("END".to_string()),
        Just("FOR".to_string()),
        Just("NEXT".to_string()),
        Just("WHILE".to_string()),
        Just("WEND".to_string()),
        Just("DO".to_string()),
        Just("LOOP".to_string()),
        Just("SUB".to_string()),
        Just("FUNCTION".to_string()),
        Just("AS".to_string()),
        Just("INTEGER".to_string()),
        Just("LONG".to_string()),
        Just("SINGLE".to_string()),
        Just("DOUBLE".to_string()),
        Just("STRING".to_string()),
        Just("SELECT".to_string()),
        Just("CASE".to_string()),
        Just("DATA".to_string()),
        Just("READ".to_string()),
        Just("GOTO".to_string()),
        Just("GOSUB".to_string()),
        Just("RETURN".to_string()),
    ];

    let literals = prop_oneof![
        any::<i32>().prop_map(|n| n.to_string()),
        any::<f64>()
            .prop_filter("finite", |f| f.is_finite())
            .prop_map(|f| f.to_string()),
        "\"[a-zA-Z0-9 ]{0,20}\"",
    ];

    let operators = prop_oneof![
        Just("+".to_string()),
        Just("-".to_string()),
        Just("*".to_string()),
        Just("/".to_string()),
        Just("\\".to_string()),
        Just("=".to_string()),
        Just("<>".to_string()),
        Just("<".to_string()),
        Just(">".to_string()),
        Just("<=".to_string()),
        Just(">=".to_string()),
        Just("AND".to_string()),
        Just("OR".to_string()),
        Just("NOT".to_string()),
        Just("MOD".to_string()),
    ];

    let identifiers = "[a-zA-Z][a-zA-Z0-9_]{0,15}[%&!#$]?";

    let whitespace = prop_oneof![Just(" ".to_string()), Just("\n".to_string()),];

    let token = prop_oneof![
        keywords,
        literals,
        operators,
        identifiers.prop_map(String::from),
        whitespace,
    ];

    proptest::collection::vec(token, 1..50).prop_map(|tokens| tokens.join(" "))
}

proptest! {
    /// The lexer should never panic, even on arbitrary binary input
    #[test]
    fn lexer_never_panics(input in arbitrary_source()) {
        // This should not panic - it may return errors, but shouldn't crash
        let _ = qb64fresh::lexer::lex(&input);
    }

    /// The lexer should handle BASIC-like input without panicking
    #[test]
    fn lexer_handles_basic_like_input(input in basic_like_source()) {
        let _ = qb64fresh::lexer::lex(&input);
    }

    /// The lexer should produce valid output for all printable ASCII
    #[test]
    fn lexer_handles_printable_ascii(input in "[[:print:]]{0,500}") {
        let result = qb64fresh::lexer::lex(&input);
        // Should always produce a token list (even if it's just errors/unknown tokens)
        assert!(!result.is_empty() || input.is_empty() || input.trim().is_empty());
    }

    /// The parser should never panic on lexer output
    #[test]
    fn parser_never_panics_on_basic_input(input in basic_like_source()) {
        let tokens = qb64fresh::lexer::lex(&input);
        // Parser should return Result, not panic
        let _ = Parser::new(&tokens).parse();
    }

    /// The parser should handle arbitrary token sequences
    #[test]
    fn parser_handles_arbitrary_input(input in arbitrary_source()) {
        let tokens = qb64fresh::lexer::lex(&input);
        let _ = Parser::new(&tokens).parse();
    }

    /// Full compilation pipeline should never panic
    #[test]
    fn full_pipeline_never_panics(input in basic_like_source()) {
        let tokens = qb64fresh::lexer::lex(&input);
        if let Ok(ast) = Parser::new(&tokens).parse() {
            // Even if parsing succeeds, semantic analysis might fail
            // but it should never panic
            let _ = SemanticAnalyzer::new().analyze(&ast);
        }
    }

    /// Long programs should not cause stack overflow
    #[test]
    fn handles_long_programs(n in 1..100usize) {
        // Generate a long but syntactically valid program
        let mut program = String::new();
        for i in 0..n {
            program.push_str(&format!("x{} = {}\n", i, i));
        }
        let tokens = qb64fresh::lexer::lex(&program);
        let _ = Parser::new(&tokens).parse();
    }

    /// Deeply nested expressions should be handled
    #[test]
    fn handles_nested_expressions(depth in 1..20usize) {
        // Generate nested parentheses: (((...)))
        let mut expr = "x".to_string();
        for _ in 0..depth {
            expr = format!("({})", expr);
        }
        let program = format!("PRINT {}", expr);
        let tokens = qb64fresh::lexer::lex(&program);
        let _ = Parser::new(&tokens).parse();
    }

    /// Strings with escape-like sequences shouldn't cause issues
    #[test]
    fn handles_string_edge_cases(content in "[^\x00]{0,50}") {
        let program = format!("PRINT \"{}\"", content.replace('"', "\"\""));
        let tokens = qb64fresh::lexer::lex(&program);
        let _ = Parser::new(&tokens).parse();
    }

    /// Very long identifiers should be handled
    #[test]
    fn handles_long_identifiers(length in 1..256usize) {
        let ident: String = "a".repeat(length);
        let program = format!("DIM {} AS INTEGER", ident);
        let tokens = qb64fresh::lexer::lex(&program);
        let _ = Parser::new(&tokens).parse();
    }

    /// Numbers at boundary values should work
    #[test]
    fn handles_boundary_numbers(n in i64::MIN..i64::MAX) {
        let program = format!("x = {}", n);
        let tokens = qb64fresh::lexer::lex(&program);
        let _ = Parser::new(&tokens).parse();
    }
}

#[cfg(test)]
mod deterministic_tests {
    //! Non-property tests that verify specific edge cases

    use qb64fresh::parser::Parser;

    #[test]
    fn empty_input_doesnt_panic() {
        let tokens = qb64fresh::lexer::lex("");
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn null_byte_in_string_doesnt_panic() {
        let tokens = qb64fresh::lexer::lex("PRINT \"hello\x00world\"");
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn unicode_input_doesnt_panic() {
        let tokens = qb64fresh::lexer::lex("PRINT \"こんにちは\"");
        let _ = Parser::new(&tokens).parse();
        let tokens = qb64fresh::lexer::lex("変数 = 42");
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn very_long_line_doesnt_panic() {
        let long_line: String = "x + ".repeat(10000);
        let program = format!("PRINT {}", long_line);
        let tokens = qb64fresh::lexer::lex(&program);
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn many_newlines_doesnt_panic() {
        let many_newlines: String = "\n".repeat(10000);
        let tokens = qb64fresh::lexer::lex(&many_newlines);
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn unbalanced_parens_doesnt_panic() {
        let tokens = qb64fresh::lexer::lex("PRINT ((((x");
        let _ = Parser::new(&tokens).parse();
        let tokens = qb64fresh::lexer::lex("PRINT x))))");
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn unterminated_string_doesnt_panic() {
        let tokens = qb64fresh::lexer::lex("PRINT \"unterminated");
        let _ = Parser::new(&tokens).parse();
    }

    #[test]
    fn repeated_keywords_doesnt_panic() {
        let tokens = qb64fresh::lexer::lex("IF IF IF IF IF");
        let _ = Parser::new(&tokens).parse();
        let tokens = qb64fresh::lexer::lex("END END END END");
        let _ = Parser::new(&tokens).parse();
    }
}
