//! Parser for C function declarations from header files.
//!
//! Extracts function declarations from tokenized C header content.

use super::lexer::Token;
use super::{CFunction, CParam, c_type_to_basic};

/// Parse function declarations from tokens.
pub fn parse_functions(tokens: &[Token]) -> Vec<CFunction> {
    let mut functions = Vec::new();
    let mut pos = 0;

    while pos < tokens.len() {
        // Skip typedef, struct, enum definitions
        if matches!(
            tokens.get(pos),
            Some(Token::Typedef | Token::Struct | Token::Enum)
        ) {
            pos = skip_definition(tokens, pos);
            continue;
        }

        // Try to parse a function declaration
        if let Some((func, new_pos)) = try_parse_function(tokens, pos) {
            functions.push(func);
            pos = new_pos;
        } else {
            pos += 1;
        }
    }

    functions
}

/// Skip a typedef, struct, or enum definition.
fn skip_definition(tokens: &[Token], mut pos: usize) -> usize {
    // Skip until we find a semicolon outside of braces
    let mut brace_depth: usize = 0;

    while pos < tokens.len() {
        match &tokens[pos] {
            Token::LBrace => brace_depth += 1,
            Token::RBrace => brace_depth = brace_depth.saturating_sub(1),
            Token::Semicolon if brace_depth == 0 => {
                return pos + 1;
            }
            Token::Eof => return pos,
            _ => {}
        }
        pos += 1;
    }
    pos
}

/// Try to parse a function declaration starting at the given position.
///
/// Returns the parsed function and the position after the declaration,
/// or None if this doesn't look like a function declaration.
fn try_parse_function(tokens: &[Token], start: usize) -> Option<(CFunction, usize)> {
    let mut pos = start;

    // Skip any leading modifiers (extern, static, inline, const)
    while matches!(
        tokens.get(pos),
        Some(Token::Extern | Token::Static | Token::Inline | Token::Const)
    ) {
        pos += 1;
    }

    // Parse return type
    let (return_type_parts, is_return_ptr, new_pos) = parse_type(tokens, pos)?;
    pos = new_pos;

    // Next should be the function name (identifier)
    let func_name = match tokens.get(pos) {
        Some(Token::Ident(name)) => name.clone(),
        _ => return None,
    };
    pos += 1;

    // Next should be opening paren
    if !matches!(tokens.get(pos), Some(Token::LParen)) {
        return None;
    }
    pos += 1;

    // Parse parameters
    let mut params = Vec::new();

    // Handle void parameter (no params)
    if matches!(
        (tokens.get(pos), tokens.get(pos + 1)),
        (Some(Token::Ident(name)), Some(Token::RParen)) if name == "void"
    ) {
        pos += 2; // Skip "void" and ")"
    } else {
        // Parse parameter list
        loop {
            match tokens.get(pos) {
                Some(Token::RParen) => {
                    pos += 1;
                    break;
                }
                Some(Token::Eof) => return None,
                Some(Token::Comma) => {
                    pos += 1;
                    continue;
                }
                _ => {
                    // Try to parse a parameter
                    if let Some((param, new_pos)) = parse_parameter(tokens, pos) {
                        params.push(param);
                        pos = new_pos;
                    } else {
                        // Couldn't parse parameter, skip to next comma or rparen
                        while !matches!(
                            tokens.get(pos),
                            Some(Token::Comma | Token::RParen | Token::Eof)
                        ) {
                            pos += 1;
                        }
                    }
                }
            }
        }
    }

    // Should end with semicolon (declaration) or lbrace (definition)
    // Skip lbrace and body for definitions
    match tokens.get(pos) {
        Some(Token::Semicolon) => pos += 1,
        Some(Token::LBrace) => {
            // Skip function body
            pos = skip_definition(tokens, pos);
        }
        _ => return None,
    }

    // Convert return type to BASIC
    let return_type = c_type_to_basic(&return_type_parts.join(" "), is_return_ptr);

    Some((
        CFunction {
            name: func_name,
            return_type,
            params,
        },
        pos,
    ))
}

/// Parse a C type (possibly with const and pointer modifiers).
///
/// Returns (type parts, is_pointer, new position).
fn parse_type(tokens: &[Token], mut pos: usize) -> Option<(Vec<String>, bool, usize)> {
    let mut type_parts = Vec::new();
    let mut is_pointer = false;

    // Collect type keywords
    loop {
        match tokens.get(pos) {
            Some(Token::Const) => {
                type_parts.push("const".to_string());
                pos += 1;
            }
            Some(Token::Ident(name)) => {
                // Check if this is a type name or could be the next identifier
                let is_type = matches!(
                    name.as_str(),
                    "void"
                        | "char"
                        | "short"
                        | "int"
                        | "long"
                        | "float"
                        | "double"
                        | "signed"
                        | "unsigned"
                        | "int8_t"
                        | "int16_t"
                        | "int32_t"
                        | "int64_t"
                        | "uint8_t"
                        | "uint16_t"
                        | "uint32_t"
                        | "uint64_t"
                        | "size_t"
                        | "ssize_t"
                        | "intptr_t"
                        | "uintptr_t"
                        | "ptrdiff_t"
                );

                if is_type || type_parts.is_empty() {
                    type_parts.push(name.clone());
                    pos += 1;
                } else {
                    break;
                }
            }
            Some(Token::Star) => {
                is_pointer = true;
                pos += 1;
            }
            _ => break,
        }
    }

    if type_parts.is_empty() {
        return None;
    }

    Some((type_parts, is_pointer, pos))
}

/// Parse a function parameter.
fn parse_parameter(tokens: &[Token], pos: usize) -> Option<(CParam, usize)> {
    let (type_parts, is_pointer, mut pos) = parse_type(tokens, pos)?;

    // Optional parameter name
    let name = if let Some(Token::Ident(name)) = tokens.get(pos) {
        // Check it's not a comma or rparen (unnamed param)
        if !matches!(
            tokens.get(pos + 1),
            Some(Token::Comma | Token::RParen | Token::LParen)
        ) || matches!(tokens.get(pos + 1), None)
        {
            pos += 1;
            name.clone()
        } else {
            pos += 1;
            name.clone()
        }
    } else {
        String::new()
    };

    // Handle array syntax (e.g., `int arr[]`)
    if matches!(tokens.get(pos), Some(Token::Ident(s)) if s == "[") {
        // Skip array brackets
        while !matches!(
            tokens.get(pos),
            Some(Token::Comma | Token::RParen | Token::Eof)
        ) {
            pos += 1;
        }
    }

    let typ = c_type_to_basic(&type_parts.join(" "), is_pointer);

    Some((CParam { name, typ }, pos))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::header_parser::lexer::tokenize;
    use crate::semantic::types::BasicType;

    #[test]
    fn test_parse_simple_function() {
        let input = "int foo(int x);";
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "foo");
        assert_eq!(funcs[0].return_type, BasicType::Long);
        assert_eq!(funcs[0].params.len(), 1);
        assert_eq!(funcs[0].params[0].name, "x");
        assert_eq!(funcs[0].params[0].typ, BasicType::Long);
    }

    #[test]
    fn test_parse_void_function() {
        let input = "void bar(void);";
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "bar");
        assert_eq!(funcs[0].return_type, BasicType::Void);
        assert_eq!(funcs[0].params.len(), 0);
    }

    #[test]
    fn test_parse_string_function() {
        let input = "char* strdup(const char* s);";
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "strdup");
        assert_eq!(funcs[0].return_type, BasicType::String);
        assert_eq!(funcs[0].params.len(), 1);
        assert_eq!(funcs[0].params[0].typ, BasicType::String);
    }

    #[test]
    fn test_parse_multiple_functions() {
        let input = r#"
            int foo(int x);
            double bar(float a, float b);
            void baz(void);
        "#;
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 3);
        assert_eq!(funcs[0].name, "foo");
        assert_eq!(funcs[1].name, "bar");
        assert_eq!(funcs[2].name, "baz");
    }

    #[test]
    fn test_skip_typedef() {
        let input = r#"
            typedef struct { int x; } MyStruct;
            int foo(void);
        "#;
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "foo");
    }
}
