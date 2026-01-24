//! Simple lexer for C header files.
//!
//! Tokenizes C source into basic tokens for function declaration parsing.
//! Now supports preprocessor directives, literals, and struct member syntax.

/// A preprocessor directive parsed from a C header.
#[derive(Debug, Clone, PartialEq)]
pub enum PreprocessorDirective {
    /// `#define NAME value` - Simple constant definition
    Define { name: String, value: Option<String> },
    /// `#define NAME(args) ...` - Function-like macro (we skip these)
    FunctionMacro { name: String },
    /// `#ifdef NAME`
    Ifdef { name: String },
    /// `#ifndef NAME`
    Ifndef { name: String },
    /// `#if expression`
    If { condition: String },
    /// `#elif expression`
    Elif { condition: String },
    /// `#else`
    Else,
    /// `#endif`
    Endif,
    /// `#include`, `#pragma`, etc. (we skip these)
    Other,
}

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
    /// Opening bracket `[`
    LBracket,
    /// Closing bracket `]`
    RBracket,
    /// Comma `,`
    Comma,
    /// Semicolon `;`
    Semicolon,
    /// Asterisk `*` (for pointers)
    Star,
    /// Integer literal (decimal, hex, or octal)
    IntegerLiteral(i64),
    /// Floating-point literal
    FloatLiteral(f64),
    /// String literal
    StringLiteral(String),
    /// A preprocessor directive
    Preprocessor(PreprocessorDirective),
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
    /// `union` keyword
    Union,
    /// End of input
    Eof,
}

/// Tokenize C header content.
pub fn tokenize(input: &str) -> Vec<Token> {
    Lexer::new(input).tokenize()
}

/// Internal lexer state for tokenizing C headers.
struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            tokens: Vec::new(),
        }
    }

    fn tokenize(mut self) -> Vec<Token> {
        while let Some(&c) = self.chars.peek() {
            match c {
                // Whitespace - skip
                c if c.is_whitespace() => {
                    self.chars.next();
                }

                // Single-line comment
                '/' if self.peek_ahead(1) == Some('/') => {
                    self.skip_line_comment();
                }

                // Multi-line comment
                '/' if self.peek_ahead(1) == Some('*') => {
                    self.skip_block_comment();
                }

                // Preprocessor directive - now parse instead of skip
                '#' => {
                    if let Some(directive) = self.parse_preprocessor() {
                        self.tokens.push(Token::Preprocessor(directive));
                    }
                }

                // String literal
                '"' => {
                    let s = self.parse_string_literal();
                    self.tokens.push(Token::StringLiteral(s));
                }

                // Character literal (treat as integer)
                '\'' => {
                    let value = self.parse_char_literal();
                    self.tokens.push(Token::IntegerLiteral(value));
                }

                // Numbers
                c if c.is_ascii_digit() => {
                    self.parse_number();
                }

                // Identifiers and keywords
                c if c.is_alphabetic() || c == '_' => {
                    self.parse_identifier();
                }

                // Punctuation
                '(' => {
                    self.tokens.push(Token::LParen);
                    self.chars.next();
                }
                ')' => {
                    self.tokens.push(Token::RParen);
                    self.chars.next();
                }
                '{' => {
                    self.tokens.push(Token::LBrace);
                    self.chars.next();
                }
                '}' => {
                    self.tokens.push(Token::RBrace);
                    self.chars.next();
                }
                '[' => {
                    self.tokens.push(Token::LBracket);
                    self.chars.next();
                }
                ']' => {
                    self.tokens.push(Token::RBracket);
                    self.chars.next();
                }
                ',' => {
                    self.tokens.push(Token::Comma);
                    self.chars.next();
                }
                ';' => {
                    self.tokens.push(Token::Semicolon);
                    self.chars.next();
                }
                '*' => {
                    self.tokens.push(Token::Star);
                    self.chars.next();
                }

                // Skip other characters (operators, etc.)
                _ => {
                    self.chars.next();
                }
            }
        }

        self.tokens.push(Token::Eof);
        self.tokens
    }

    /// Peek ahead n characters without consuming.
    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    /// Skip a single-line comment (// ...).
    fn skip_line_comment(&mut self) {
        while let Some(&c) = self.chars.peek() {
            self.chars.next();
            if c == '\n' {
                break;
            }
        }
    }

    /// Skip a block comment (/* ... */).
    fn skip_block_comment(&mut self) {
        self.chars.next(); // '/'
        self.chars.next(); // '*'
        loop {
            match self.chars.next() {
                Some('*') if self.chars.peek() == Some(&'/') => {
                    self.chars.next();
                    break;
                }
                None => break,
                _ => {}
            }
        }
    }

    /// Parse a preprocessor directive.
    fn parse_preprocessor(&mut self) -> Option<PreprocessorDirective> {
        self.chars.next(); // consume '#'
        self.skip_whitespace_except_newline();

        // Collect directive name
        let directive_name = self.collect_identifier();

        match directive_name.as_str() {
            "define" => self.parse_define(),
            "ifdef" => {
                self.skip_whitespace_except_newline();
                let name = self.collect_identifier();
                self.skip_to_end_of_directive();
                Some(PreprocessorDirective::Ifdef { name })
            }
            "ifndef" => {
                self.skip_whitespace_except_newline();
                let name = self.collect_identifier();
                self.skip_to_end_of_directive();
                Some(PreprocessorDirective::Ifndef { name })
            }
            "if" => {
                self.skip_whitespace_except_newline();
                let condition = self.collect_to_end_of_directive();
                Some(PreprocessorDirective::If { condition })
            }
            "elif" => {
                self.skip_whitespace_except_newline();
                let condition = self.collect_to_end_of_directive();
                Some(PreprocessorDirective::Elif { condition })
            }
            "else" => {
                self.skip_to_end_of_directive();
                Some(PreprocessorDirective::Else)
            }
            "endif" => {
                self.skip_to_end_of_directive();
                Some(PreprocessorDirective::Endif)
            }
            _ => {
                // #include, #pragma, #error, etc. - skip
                self.skip_to_end_of_directive();
                Some(PreprocessorDirective::Other)
            }
        }
    }

    /// Parse a #define directive.
    fn parse_define(&mut self) -> Option<PreprocessorDirective> {
        self.skip_whitespace_except_newline();
        let name = self.collect_identifier();

        if name.is_empty() {
            self.skip_to_end_of_directive();
            return Some(PreprocessorDirective::Other);
        }

        // Check if it's a function-like macro
        if self.chars.peek() == Some(&'(') {
            self.skip_to_end_of_directive();
            return Some(PreprocessorDirective::FunctionMacro { name });
        }

        // Collect the value
        self.skip_whitespace_except_newline();
        let value = self.collect_to_end_of_directive();

        Some(PreprocessorDirective::Define {
            name,
            value: if value.is_empty() { None } else { Some(value) },
        })
    }

    /// Skip whitespace but not newlines.
    fn skip_whitespace_except_newline(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if c == ' ' || c == '\t' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    /// Collect an identifier.
    fn collect_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        ident
    }

    /// Collect everything until end of directive (handling line continuation).
    fn collect_to_end_of_directive(&mut self) -> String {
        let mut content = String::new();
        while let Some(&c) = self.chars.peek() {
            if c == '\n' {
                self.chars.next();
                break;
            }
            if c == '\\' {
                self.chars.next();
                // Line continuation
                if self.chars.peek() == Some(&'\n') {
                    self.chars.next();
                    content.push(' '); // Replace continuation with space
                    continue;
                }
            }
            content.push(c);
            self.chars.next();
        }
        content.trim().to_string()
    }

    /// Skip to end of preprocessor directive.
    fn skip_to_end_of_directive(&mut self) {
        while let Some(&c) = self.chars.peek() {
            self.chars.next();
            if c == '\n' {
                break;
            }
            // Handle line continuation
            if c == '\\' {
                if self.chars.peek() == Some(&'\n') {
                    self.chars.next();
                }
            }
        }
    }

    /// Parse a string literal.
    fn parse_string_literal(&mut self) -> String {
        self.chars.next(); // consume opening '"'
        let mut s = String::new();
        while let Some(&c) = self.chars.peek() {
            self.chars.next();
            match c {
                '"' => break,
                '\\' => {
                    // Handle escape sequences
                    if let Some(&escaped) = self.chars.peek() {
                        self.chars.next();
                        match escaped {
                            'n' => s.push('\n'),
                            'r' => s.push('\r'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            '0' => s.push('\0'),
                            _ => {
                                s.push('\\');
                                s.push(escaped);
                            }
                        }
                    }
                }
                _ => s.push(c),
            }
        }
        s
    }

    /// Parse a character literal as an integer value.
    fn parse_char_literal(&mut self) -> i64 {
        self.chars.next(); // consume opening '\''
        let value = if let Some(&c) = self.chars.peek() {
            self.chars.next();
            if c == '\\' {
                // Escape sequence
                if let Some(&escaped) = self.chars.peek() {
                    self.chars.next();
                    match escaped {
                        'n' => '\n' as i64,
                        'r' => '\r' as i64,
                        't' => '\t' as i64,
                        '\\' => '\\' as i64,
                        '\'' => '\'' as i64,
                        '0' => 0,
                        _ => escaped as i64,
                    }
                } else {
                    0
                }
            } else {
                c as i64
            }
        } else {
            0
        };
        // Skip closing quote
        if self.chars.peek() == Some(&'\'') {
            self.chars.next();
        }
        value
    }

    /// Parse a number (integer or float, decimal/hex/octal).
    fn parse_number(&mut self) {
        let mut num_str = String::new();

        // Check for hex (0x), binary (0b), or octal (0)
        if self.chars.peek() == Some(&'0') {
            num_str.push('0');
            self.chars.next();

            match self.chars.peek() {
                Some(&'x') | Some(&'X') => {
                    // Hexadecimal
                    self.chars.next();
                    let mut hex = String::new();
                    while let Some(&c) = self.chars.peek() {
                        if c.is_ascii_hexdigit() {
                            hex.push(c);
                            self.chars.next();
                        } else if c == '_' {
                            self.chars.next(); // Skip digit separators
                        } else {
                            break;
                        }
                    }
                    self.skip_integer_suffix();
                    let value = i64::from_str_radix(&hex, 16).unwrap_or(0);
                    self.tokens.push(Token::IntegerLiteral(value));
                    return;
                }
                Some(&'b') | Some(&'B') => {
                    // Binary
                    self.chars.next();
                    let mut bin = String::new();
                    while let Some(&c) = self.chars.peek() {
                        if c == '0' || c == '1' {
                            bin.push(c);
                            self.chars.next();
                        } else if c == '_' {
                            self.chars.next();
                        } else {
                            break;
                        }
                    }
                    self.skip_integer_suffix();
                    let value = i64::from_str_radix(&bin, 2).unwrap_or(0);
                    self.tokens.push(Token::IntegerLiteral(value));
                    return;
                }
                Some(c) if c.is_ascii_digit() => {
                    // Octal (or could be float starting with 0)
                    // Continue collecting digits
                }
                _ => {
                    // Just "0"
                    self.skip_integer_suffix();
                    self.tokens.push(Token::IntegerLiteral(0));
                    return;
                }
            }
        }

        // Collect digits
        let mut has_dot = false;
        let mut has_exp = false;
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.chars.next();
            } else if c == '.' && !has_dot && !has_exp {
                // Could be a float
                if let Some(next) = self.peek_ahead(1) {
                    if next.is_ascii_digit() {
                        has_dot = true;
                        num_str.push(c);
                        self.chars.next();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else if (c == 'e' || c == 'E') && !has_exp {
                has_exp = true;
                num_str.push(c);
                self.chars.next();
                // Optional sign
                if let Some(&sign) = self.chars.peek() {
                    if sign == '+' || sign == '-' {
                        num_str.push(sign);
                        self.chars.next();
                    }
                }
            } else if c == '_' {
                self.chars.next(); // Skip digit separators
            } else {
                break;
            }
        }

        if has_dot || has_exp {
            // Float
            self.skip_float_suffix();
            let value = num_str.parse::<f64>().unwrap_or(0.0);
            self.tokens.push(Token::FloatLiteral(value));
        } else {
            // Integer
            self.skip_integer_suffix();
            let value = num_str.parse::<i64>().unwrap_or(0);
            self.tokens.push(Token::IntegerLiteral(value));
        }
    }

    /// Skip integer suffixes like U, L, LL, UL, etc.
    fn skip_integer_suffix(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if c == 'u' || c == 'U' || c == 'l' || c == 'L' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    /// Skip float suffixes like f, F, l, L.
    fn skip_float_suffix(&mut self) {
        if let Some(&c) = self.chars.peek() {
            if c == 'f' || c == 'F' || c == 'l' || c == 'L' {
                self.chars.next();
            }
        }
    }

    /// Parse an identifier or keyword.
    fn parse_identifier(&mut self) {
        let ident = self.collect_identifier();
        let token = match ident.as_str() {
            "const" => Token::Const,
            "extern" => Token::Extern,
            "static" => Token::Static,
            "inline" => Token::Inline,
            "__inline" | "__inline__" => Token::Inline,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "union" => Token::Union,
            "typedef" => Token::Typedef,
            _ => Token::Ident(ident),
        };
        self.tokens.push(token);
    }
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
        // Now we capture preprocessor directives as tokens
        let input = "#include <stdio.h>\nint foo();";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::Other), // #include
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

    #[test]
    fn test_tokenize_define_constant() {
        let input = "#define FOO 123";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::Define {
                    name: "FOO".to_string(),
                    value: Some("123".to_string()),
                }),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_define_hex() {
        let input = "#define HEX_VALUE 0xFF";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::Define {
                    name: "HEX_VALUE".to_string(),
                    value: Some("0xFF".to_string()),
                }),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_function_macro() {
        let input = "#define MAX(a, b) ((a) > (b) ? (a) : (b))";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::FunctionMacro {
                    name: "MAX".to_string(),
                }),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_ifdef() {
        let input = "#ifdef WIN32\nint win_func();\n#endif";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::Ifdef {
                    name: "WIN32".to_string(),
                }),
                Token::Ident("int".to_string()),
                Token::Ident("win_func".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Semicolon,
                Token::Preprocessor(PreprocessorDirective::Endif),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_ifndef_else() {
        let input = "#ifndef LINUX\n#else\n#endif";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::Ifndef {
                    name: "LINUX".to_string(),
                }),
                Token::Preprocessor(PreprocessorDirective::Else),
                Token::Preprocessor(PreprocessorDirective::Endif),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_integer_literal() {
        let input = "int arr[256];";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Ident("int".to_string()),
                Token::Ident("arr".to_string()),
                Token::LBracket,
                Token::IntegerLiteral(256),
                Token::RBracket,
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_hex_literal() {
        let input = "int x = 0xFF;";
        let tokens = tokenize(input);
        // We skip '=' but capture the hex literal
        assert!(tokens.contains(&Token::IntegerLiteral(255)));
    }

    #[test]
    fn test_tokenize_float_literal() {
        let input = "float f = 3.14;";
        let tokens = tokenize(input);
        assert!(tokens.contains(&Token::FloatLiteral(3.14)));
    }

    #[test]
    fn test_tokenize_string_literal() {
        let input = r#"char* s = "hello";"#;
        let tokens = tokenize(input);
        assert!(tokens.contains(&Token::StringLiteral("hello".to_string())));
    }

    #[test]
    fn test_tokenize_struct() {
        let input = "struct Point { int x; int y; };";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Struct,
                Token::Ident("Point".to_string()),
                Token::LBrace,
                Token::Ident("int".to_string()),
                Token::Ident("x".to_string()),
                Token::Semicolon,
                Token::Ident("int".to_string()),
                Token::Ident("y".to_string()),
                Token::Semicolon,
                Token::RBrace,
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_line_continuation() {
        let input = "#define LONG_VALUE \\\n    123";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::Define {
                    name: "LONG_VALUE".to_string(),
                    value: Some("123".to_string()),
                }),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_tokenize_if_directive() {
        let input = "#if defined(WIN32) || defined(_WIN32)";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Preprocessor(PreprocessorDirective::If {
                    condition: "defined(WIN32) || defined(_WIN32)".to_string(),
                }),
                Token::Eof,
            ]
        );
    }
}
