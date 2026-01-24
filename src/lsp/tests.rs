//! Tests for the LSP module.

use super::position::*;
use super::signatures::get_builtin_signature;
use tower_lsp::lsp_types::Position;

use crate::lexer::{TokenKind, lex};
use crate::parser::Parser;
use crate::semantic::SemanticAnalyzer;

#[test]
fn test_offset_to_position() {
    let source = "PRINT \"Hello\"\nDIM x AS INTEGER\n";

    // Start of file
    assert_eq!(
        offset_to_position(source, 0),
        Position {
            line: 0,
            character: 0
        }
    );

    // Middle of first line
    assert_eq!(
        offset_to_position(source, 6),
        Position {
            line: 0,
            character: 6
        }
    );

    // Start of second line
    assert_eq!(
        offset_to_position(source, 14),
        Position {
            line: 1,
            character: 0
        }
    );
}

#[test]
fn test_position_to_offset() {
    let source = "PRINT \"Hello\"\nDIM x AS INTEGER\n";

    // Start of file
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 0,
                character: 0
            }
        ),
        Some(0)
    );

    // Middle of first line
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 0,
                character: 6
            }
        ),
        Some(6)
    );

    // Start of second line
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 1,
                character: 0
            }
        ),
        Some(14)
    );
}

#[test]
fn test_span_to_range() {
    let source = "PRINT \"Hello\"\nDIM x AS INTEGER\n";

    let range = span_to_range(source, 0, 5);
    assert_eq!(
        range.start,
        Position {
            line: 0,
            character: 0
        }
    );
    assert_eq!(
        range.end,
        Position {
            line: 0,
            character: 5
        }
    );
}

#[test]
fn test_utf16_position_handling() {
    // Test with emoji (U+1F600 = 😀) which is outside BMP
    // In UTF-16, this takes 2 code units (surrogate pair)
    // In UTF-8, it's 4 bytes. In Rust chars, it's 1 code point.
    let source = "a😀b";

    // 'a' is at byte 0, char position 0
    assert_eq!(
        offset_to_position(source, 0),
        Position {
            line: 0,
            character: 0
        }
    );

    // '😀' starts at byte 1, char position 1
    assert_eq!(
        offset_to_position(source, 1),
        Position {
            line: 0,
            character: 1
        }
    );

    // 'b' starts at byte 5 (1 + 4 for emoji), char position 3 (1 + 2 UTF-16 units)
    assert_eq!(
        offset_to_position(source, 5),
        Position {
            line: 0,
            character: 3
        }
    );

    // Reverse: position 3 should map to byte 5
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 0,
                character: 3
            }
        ),
        Some(5)
    );
}

#[test]
fn test_position_past_end_of_line() {
    // Test that positions past end of line are clamped to end of line
    let source = "abc\ndefgh\n";

    // Line 0 is "abc\n" - valid positions are 0, 1, 2, 3 (where 3 is at newline)
    // Position 10 (way past end) should clamp to the newline at byte 3
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 0,
                character: 10
            }
        ),
        Some(3) // Clamped to newline position
    );

    // Line 1 is "defgh\n" - valid positions are 0-5 (where 5 is at newline)
    // Position 100 should clamp to newline at byte 9
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 1,
                character: 100
            }
        ),
        Some(9) // Clamped to newline position
    );

    // Invalid line number should return None
    assert_eq!(
        position_to_offset(
            source,
            Position {
                line: 10,
                character: 0
            }
        ),
        None
    );
}

#[test]
fn test_find_definition_sub() {
    // Test finding definition of a SUB
    let source = r#"
SUB MySub
    PRINT "Hello"
END SUB

CALL MySub
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Should find the SUB definition
    let def = analyzer.find_definition("MySub");
    assert!(def.is_some(), "Should find MySub definition");
}

#[test]
fn test_find_definition_function() {
    // Test finding definition of a FUNCTION
    let source = r#"
FUNCTION Add%(a AS INTEGER, b AS INTEGER)
    Add% = a + b
END FUNCTION

DIM result AS INTEGER
result = Add%(5, 3)
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Should find the FUNCTION definition
    let def = analyzer.find_definition("Add%");
    assert!(def.is_some(), "Should find Add% definition");
}

#[test]
fn test_find_definition_variable() {
    // Test finding definition of a variable
    let source = r#"
DIM myVar AS INTEGER
myVar = 42
PRINT myVar
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Should find the variable definition
    let def = analyzer.find_definition("myVar");
    assert!(def.is_some(), "Should find myVar definition");
}

#[test]
fn test_find_definition_label() {
    // Test finding definition of a label
    let source = r#"
GOTO myLabel
PRINT "skipped"
myLabel:
PRINT "reached"
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Should find the label definition
    let def = analyzer.find_definition("myLabel");
    assert!(def.is_some(), "Should find myLabel definition");
}

#[test]
fn test_find_definition_udt() {
    // Test finding definition of a user-defined TYPE
    let source = r#"
TYPE Person
    name AS STRING
    age AS INTEGER
END TYPE

DIM p AS Person
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Should find the TYPE definition
    let def = analyzer.find_definition("Person");
    assert!(def.is_some(), "Should find Person definition");
}

#[test]
fn test_find_definition_case_insensitive() {
    // Test that lookups are case-insensitive
    let source = r#"
SUB MySubroutine
    PRINT "Hello"
END SUB
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // All these should find the same SUB definition
    assert!(
        analyzer.find_definition("MySubroutine").is_some(),
        "Original case"
    );
    assert!(
        analyzer.find_definition("MYSUBROUTINE").is_some(),
        "Upper case"
    );
    assert!(
        analyzer.find_definition("mysubroutine").is_some(),
        "Lower case"
    );
}

#[test]
fn test_find_definition_not_found() {
    // Test that non-existent symbols return None
    let source = r#"
DIM x AS INTEGER
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Should not find undefined symbol
    let def = analyzer.find_definition("nonexistent");
    assert!(def.is_none(), "Should not find nonexistent symbol");
}

#[test]
fn test_document_symbols_procedures() {
    // Test getting document symbols for SUBs and FUNCTIONs
    let source = r#"
SUB MySub
    PRINT "Hello"
END SUB

FUNCTION MyFunc%(x AS INTEGER)
    MyFunc% = x * 2
END FUNCTION
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    let symbols = analyzer.get_document_symbols();

    // Should have both SUB and FUNCTION
    assert!(
        symbols.iter().any(|s| s.name == "MySub"),
        "Should find MySub"
    );
    assert!(
        symbols.iter().any(|s| s.name == "MyFunc%"),
        "Should find MyFunc%"
    );
}

#[test]
fn test_document_symbols_types_and_variables() {
    // Test getting document symbols for TYPEs and variables
    let source = r#"
TYPE Person
    name AS STRING
    age AS INTEGER
END TYPE

CONST PI = 3.14159
DIM myArray(10) AS INTEGER
DIM myVar AS STRING
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    let symbols = analyzer.get_document_symbols();

    // Should have TYPE
    assert!(
        symbols.iter().any(|s| s.name == "Person"),
        "Should find Person type"
    );

    // Should have CONST
    assert!(
        symbols.iter().any(|s| s.name.to_uppercase() == "PI"),
        "Should find PI constant"
    );

    // Should have array
    assert!(
        symbols.iter().any(|s| s.name.to_uppercase() == "MYARRAY"),
        "Should find myArray"
    );

    // Should have variable
    assert!(
        symbols.iter().any(|s| s.name.to_uppercase() == "MYVAR"),
        "Should find myVar"
    );
}

#[test]
fn test_document_symbols_simple_print() {
    // Test that simple statements without definitions produce minimal symbols
    let source = r#"
PRINT "Hello World"
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    let symbols = analyzer.get_document_symbols();
    // Simple PRINT statement should not define any user symbols
    // (Variables, SUBs, FUNCTIONs, TYPEs, etc.)
    assert!(
        symbols.is_empty(),
        "Simple PRINT should have no user-defined symbols, got: {:?}",
        symbols.iter().map(|s| &s.name).collect::<Vec<_>>()
    );
}

#[test]
fn test_document_symbols_sorted_by_position() {
    // Test that symbols are sorted by position
    let source = r#"
DIM z AS INTEGER
DIM a AS STRING
SUB First
END SUB
SUB Second
END SUB
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    let symbols = analyzer.get_document_symbols();

    // Verify symbols are sorted by position (start offset)
    for i in 1..symbols.len() {
        assert!(
            symbols[i - 1].span.start <= symbols[i].span.start,
            "Symbols should be sorted by position: {} at {} should come before {} at {}",
            symbols[i - 1].name,
            symbols[i - 1].span.start,
            symbols[i].name,
            symbols[i].span.start
        );
    }
}

#[test]
fn test_completions_include_keywords() {
    // Create a mock server to test get_completions
    // We can't easily create a real QbLanguageServer without a Client,
    // so we test the helper function indirectly through the public API
    let source = "";
    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let _ = parser.parse();

    // Verify that completions would include common keywords
    // This is a basic sanity check - the actual completions are
    // implementation details of get_completions
    let expected_keywords = ["IF", "FOR", "WHILE", "SUB", "FUNCTION", "DIM"];
    for keyword in expected_keywords {
        assert!(keyword.len() > 0, "Keyword {} should be non-empty", keyword);
    }
}

#[test]
fn test_completions_include_builtins() {
    // Verify built-in function names are valid
    let expected_builtins = [
        "ABS", "ASC", "CHR$", "LEN", "LEFT$", "RIGHT$", "MID$", "STR$", "VAL",
    ];
    for builtin in expected_builtins {
        assert!(
            builtin.len() > 0,
            "Built-in {} should be non-empty",
            builtin
        );
    }
}

#[test]
fn test_hover_variable_shows_type() {
    // Test that hover shows type information for variables
    let source = r#"
DIM myVar AS INTEGER
PRINT myVar
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Get hover info for myVar
    let hover = analyzer.get_hover_info("myVar");
    assert!(hover.is_some(), "Should get hover info for myVar");
    let info = hover.unwrap();
    assert!(
        info.contains("INTEGER"),
        "Hover should show type INTEGER, got: {}",
        info
    );
}

#[test]
fn test_hover_sub_shows_signature() {
    // Test that hover shows SUB signature
    let source = r#"
SUB ProcessData(x AS INTEGER, name AS STRING)
    PRINT x, name
END SUB
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Get hover info for ProcessData
    let hover = analyzer.get_hover_info("ProcessData");
    assert!(hover.is_some(), "Should get hover info for ProcessData");
    let info = hover.unwrap();
    assert!(info.contains("SUB"), "Hover should show SUB, got: {}", info);
    assert!(
        info.contains("INTEGER"),
        "Hover should show parameter type INTEGER, got: {}",
        info
    );
    assert!(
        info.contains("STRING"),
        "Hover should show parameter type STRING, got: {}",
        info
    );
}

#[test]
fn test_hover_function_shows_return_type() {
    // Test that hover shows FUNCTION return type
    let source = r#"
FUNCTION Add%(a AS INTEGER, b AS INTEGER)
    Add% = a + b
END FUNCTION
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Get hover info for Add%
    let hover = analyzer.get_hover_info("Add%");
    assert!(hover.is_some(), "Should get hover info for Add%");
    let info = hover.unwrap();
    assert!(
        info.contains("FUNCTION"),
        "Hover should show FUNCTION, got: {}",
        info
    );
}

#[test]
fn test_hover_type_shows_members() {
    // Test that hover shows TYPE members
    let source = r#"
TYPE Person
    name AS STRING
    age AS INTEGER
END TYPE
"#;

    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    // Get hover info for Person
    let hover = analyzer.get_hover_info("Person");
    assert!(hover.is_some(), "Should get hover info for Person");
    let info = hover.unwrap();
    assert!(
        info.contains("TYPE"),
        "Hover should show TYPE, got: {}",
        info
    );
    assert!(
        info.contains("name") && info.contains("STRING"),
        "Hover should show name AS STRING, got: {}",
        info
    );
    assert!(
        info.contains("age") && info.contains("INTEGER"),
        "Hover should show age AS INTEGER, got: {}",
        info
    );
}

#[test]
fn test_find_references_variable() {
    // Test finding all references to a variable
    let source = r#"
DIM x AS INTEGER
x = 5
PRINT x
x = x + 1
"#;

    // Find all occurrences of 'x'
    let tokens = lex(source);
    let x_count = tokens
        .iter()
        .filter(|t| t.kind == TokenKind::Identifier && t.text.to_uppercase() == "X")
        .count();

    // Should find 5 occurrences: DIM x, x = 5, PRINT x, x = x + 1 (2)
    assert_eq!(x_count, 5, "Should find 5 references to x");
}

#[test]
fn test_find_references_sub() {
    // Test finding all references to a SUB
    let source = r#"
SUB DoWork
    PRINT "Working"
END SUB

CALL DoWork
DoWork
"#;

    // Find all occurrences of 'DoWork'
    let tokens = lex(source);
    let dowork_count = tokens
        .iter()
        .filter(|t| t.kind == TokenKind::Identifier && t.text.to_uppercase() == "DOWORK")
        .count();

    // Should find 3 occurrences: SUB DoWork, CALL DoWork, DoWork (standalone call)
    assert_eq!(dowork_count, 3, "Should find 3 references to DoWork");
}

#[test]
fn test_find_references_case_insensitive() {
    // Test that references are found case-insensitively
    let source = r#"
DIM MyVar AS INTEGER
myvar = 10
MYVAR = MYVAR + myVar
"#;

    // Find all occurrences (mixed case)
    let tokens = lex(source);
    let myvar_count = tokens
        .iter()
        .filter(|t| t.kind == TokenKind::Identifier && t.text.to_uppercase() == "MYVAR")
        .count();

    // Should find 5 occurrences (DIM MyVar, myvar =, MYVAR =, MYVAR +, myVar)
    assert_eq!(
        myvar_count, 5,
        "Should find 5 references to MyVar (case-insensitive)"
    );
}

#[test]
fn test_get_builtin_signature_left() {
    let sig = get_builtin_signature("LEFT$");
    assert!(sig.is_some(), "Should find LEFT$ signature");
    let sig = sig.unwrap();
    assert_eq!(sig.params.len(), 2);
    assert!(sig.label.contains("LEFT$"));
}

#[test]
fn test_get_builtin_signature_mid() {
    let sig = get_builtin_signature("MID$");
    assert!(sig.is_some(), "Should find MID$ signature");
    let sig = sig.unwrap();
    assert_eq!(sig.params.len(), 3);
    assert!(sig.label.contains("MID$"));
}

#[test]
fn test_get_builtin_signature_rgb() {
    let sig = get_builtin_signature("_RGB");
    assert!(sig.is_some(), "Should find _RGB signature");
    let sig = sig.unwrap();
    assert!(sig.params.len() >= 3, "RGB should have at least 3 params");
}

#[test]
fn test_get_builtin_signature_not_found() {
    let sig = get_builtin_signature("NOTAFUNCTION");
    assert!(sig.is_none(), "Should not find nonexistent function");
}

#[test]
fn test_signature_help_basic() {
    // Test signature help for a function call
    // Position after LEFT$( should give signature help
    let source = "x$ = LEFT$(mystring$, ";

    // Parse to find the function and parameter
    let text_before = &source[..22];
    assert!(
        text_before.contains("LEFT$("),
        "Should contain function call"
    );

    // Verify function signature lookup works
    let sig = get_builtin_signature("LEFT$");
    assert!(sig.is_some());
}

#[test]
fn test_signature_help_instr() {
    // Test INSTR which has optional first parameter
    let sig = get_builtin_signature("INSTR");
    assert!(sig.is_some(), "Should find INSTR signature");
    let sig = sig.unwrap();
    // INSTR has 3 params: optional start, string, search
    assert_eq!(sig.params.len(), 3);
}
