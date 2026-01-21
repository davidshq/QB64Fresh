//! Simple lexer for C header files.
//!
//! Tokenizes C source into basic tokens for function declaration parsing.

/// A token from C source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// An identifier (type name, function name, parameter name)
    Ident(String),
    /// Opening parenthesis `(`
    LParen,
    /// Closing parenthesis `)`
    RParen,
    /// Opening brace `{`
    LBrace,
    /// Closing brace `}`
    RBrace,
    /// Comma `,`
    Comma,
    /// Semicolon `;`
    Semicolon,
    /// Asterisk `*` (for pointers)
    Star,
    /// `const` keyword
    Const,
    /// `extern` keyword
    Extern,
    /// `static` keyword
    Static,
    /// `inline` keyword
    Inline,
    /// `struct` keyword
    Struct,
    /// `enum` keyword
    Enum,
    /// `typedef` keyword
    Typedef,
    /// End of input
    Eof,
}

/// Tokenize C header content.
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            // Whitespace - skip
            c if c.is_whitespace() => {
                chars.next();
            }

            // Single-line comment
            '/' if chars.clone().nth(1) == Some('/') => {
                // Skip to end of line
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c == '\n' {
                        break;
                    }
                }
            }

            // Multi-line comment
            '/' if chars.clone().nth(1) == Some('*') => {
                chars.next(); // '/'
                chars.next(); // '*'
                // Skip until */
                loop {
                    match chars.next() {
                        Some('*') if chars.peek() == Some(&'/') => {
                            chars.next();
                            break;
                        }
                        None => break,
                        _ => {}
                    }
                }
            }

            // Preprocessor directive - skip entire line
            '#' => {
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c == '\n' {
                        break;
                    }
                    // Handle line continuation
                    if c == '\\' {
                        if chars.peek() == Some(&'\n') {
                            chars.next();
                        }
                    }
                }
            }

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let token = match ident.as_str() {
                    "const" => Token::Const,
                    "extern" => Token::Extern,
                    "static" => Token::Static,
                    "inline" => Token::Inline,
                    "__inline" | "__inline__" => Token::Inline,
                    "struct" => Token::Struct,
                    "enum" => Token::Enum,
                    "typedef" => Token::Typedef,
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }

            // Punctuation
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '{' => {
                tokens.push(Token::LBrace);
                chars.next();
            }
            '}' => {
                tokens.push(Token::RBrace);
                chars.next();
            }
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            }
            ';' => {
                tokens.push(Token::Semicolon);
                chars.next();
            }
            '*' => {
                tokens.push(Token::Star);
                chars.next();
            }

            // Skip other characters (numbers, strings, etc.)
            _ => {
                chars.next();
            }
        }
    }

    tokens.push(Token::Eof);
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_function() {
        let input = "int foo(int x);";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Ident("int".to_string()),
                Token::Ident("foo".to_string()),
                Token::LParen,
                Token::Ident("int".to_string()),
                Token::Ident("x".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_with_comments() {
        let input = "/* comment */ int foo(); // line comment\nint bar();";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Ident("int".to_string()),
                Token::Ident("foo".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Semicolon,
                Token::Ident("int".to_string()),
                Token::Ident("bar".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_with_preprocessor() {
        let input = "#include <stdio.h>\nint foo();";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Ident("int".to_string()),
                Token::Ident("foo".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_pointer() {
        let input = "char* foo(const char* s);";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Ident("char".to_string()),
                Token::Star,
                Token::Ident("foo".to_string()),
                Token::LParen,
                Token::Const,
                Token::Ident("char".to_string()),
                Token::Star,
                Token::Ident("s".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }
}
