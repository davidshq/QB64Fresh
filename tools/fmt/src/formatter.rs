//! Core formatter implementation.
//!
//! The formatter works by:
//! 1. Lexing the source code into tokens
//! 2. Iterating through tokens, applying formatting rules
//! 3. Reconstructing the source with proper spacing and indentation

use crate::config::FormatterConfig;
use crate::error::{FormatError, FormatResult};
use crate::rules::{
    ends_block, is_binary_operator, is_continuation, is_keyword, keyword_text, no_space_after,
    no_space_before, starts_block,
};
use qb64fresh::lexer::{Lexer, Token, TokenKind};

/// The main formatter struct.
pub struct Formatter {
    config: FormatterConfig,
}

impl Formatter {
    /// Create a new formatter with the given configuration.
    pub fn new(config: FormatterConfig) -> Self {
        Self { config }
    }

    /// Create a formatter with default configuration.
    pub fn default_formatter() -> Self {
        Self::new(FormatterConfig::default())
    }

    /// Format the given source code and return the formatted result.
    pub fn format(&self, source: &str) -> FormatResult<String> {
        let tokens = self.tokenize(source)?;
        self.format_tokens(&tokens, source)
    }

    /// Tokenize the source code.
    fn tokenize(&self, source: &str) -> FormatResult<Vec<Token>> {
        let lexer = Lexer::new(source);
        let tokens: Vec<Token> = lexer.collect();

        // Check for lexer errors
        for token in &tokens {
            if token.kind == TokenKind::Error {
                return Err(FormatError::LexerError {
                    offset: token.span.start,
                    message: format!("Unrecognized character: '{}'", &token.text),
                });
            }
        }

        Ok(tokens)
    }

    /// Format a sequence of tokens into a string.
    fn format_tokens(&self, tokens: &[Token], source: &str) -> FormatResult<String> {
        let mut output = String::with_capacity(source.len());
        let mut indent_level: usize = 0;
        let mut at_line_start = true;
        let mut pending_space = false;
        let mut last_kind: Option<TokenKind> = None;
        let mut consecutive_blank_lines = 0;
        // Track if we just emitted an operator without trailing space
        let mut after_operator_no_space = false;

        // Track if we're in a single-line IF context
        let mut single_line_if = false;

        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];

            match &token.kind {
                TokenKind::Newline => {
                    // Handle newline
                    if at_line_start {
                        // This is a blank line
                        consecutive_blank_lines += 1;
                        if self.config.preserve_blank_lines
                            && (self.config.max_blank_lines == 0
                                || consecutive_blank_lines <= self.config.max_blank_lines)
                        {
                            output.push('\n');
                        }
                    } else {
                        // End of a content line
                        if self.config.trim_trailing_whitespace {
                            // Remove any trailing spaces we may have added
                            while output.ends_with(' ') {
                                output.pop();
                            }
                        }
                        output.push('\n');
                        consecutive_blank_lines = 0;
                    }
                    at_line_start = true;
                    pending_space = false;
                    single_line_if = false;
                }

                TokenKind::Comment | TokenKind::RemComment => {
                    // Handle comments
                    if at_line_start {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if self.config.align_comments > 0 {
                        // Align trailing comment to column
                        let current_col = self.current_column(&output);
                        if current_col < self.config.align_comments {
                            output.push_str(&" ".repeat(self.config.align_comments - current_col));
                        } else {
                            output.push(' ');
                        }
                    } else {
                        output.push(' ');
                    }

                    // Format comment text
                    let comment_text = self.format_comment(&token.text);
                    output.push_str(&comment_text);
                    at_line_start = false;
                    pending_space = false;
                }

                // Block structure tokens
                kind if ends_block(kind) || is_continuation(kind) => {
                    // Check if this is END followed by IF/SUB/FUNCTION/etc.
                    let is_block_end = if *kind == TokenKind::End {
                        // Look ahead to see what follows END
                        if let Some(next) = tokens.get(i + 1) {
                            matches!(
                                next.kind,
                                TokenKind::If
                                    | TokenKind::Sub
                                    | TokenKind::Function
                                    | TokenKind::Select
                                    | TokenKind::Type
                            )
                        } else {
                            false
                        }
                    } else {
                        ends_block(kind)
                    };

                    // Decrease indent for this line if it's a block end or continuation
                    if at_line_start && (is_block_end || is_continuation(kind)) && indent_level > 0
                    {
                        indent_level = indent_level.saturating_sub(1);
                    }

                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space {
                        output.push(' ');
                    }

                    output.push_str(&self.format_token(token));
                    at_line_start = false;
                    pending_space = self.config.space_after_keyword;

                    // If this is a continuation (ELSE, ELSEIF, CASE), re-increase for body
                    if is_continuation(kind) {
                        indent_level += 1;
                    }
                }

                kind if starts_block(kind) => {
                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space {
                        output.push(' ');
                    }

                    // Check for single-line IF
                    if *kind == TokenKind::If {
                        single_line_if = self.is_single_line_if(tokens, i);
                    }

                    output.push_str(&self.format_token(token));
                    at_line_start = false;
                    pending_space = self.config.space_after_keyword;

                    // Only increase indent for multi-line blocks
                    if !single_line_if
                        && self.config.indent_blocks
                        && (*kind == TokenKind::If
                            || *kind == TokenKind::For
                            || *kind == TokenKind::While
                            || *kind == TokenKind::Do
                            || *kind == TokenKind::Select)
                    {
                        // Increase indent after THEN for IF blocks
                        // For other blocks, increase now
                        if *kind != TokenKind::If {
                            indent_level += 1;
                        }
                    }

                    if (*kind == TokenKind::Sub || *kind == TokenKind::Function)
                        && self.config.indent_procedures
                    {
                        indent_level += 1;
                    }

                    if *kind == TokenKind::Type {
                        indent_level += 1;
                    }
                }

                TokenKind::Then => {
                    // THEN keyword
                    if pending_space {
                        output.push(' ');
                    }
                    output.push_str(&self.format_token(token));
                    at_line_start = false;
                    pending_space = self.config.space_after_keyword;

                    // If not single-line IF, increase indent for next line
                    if !single_line_if && self.config.indent_blocks {
                        indent_level += 1;
                    }
                }

                // Binary operators
                kind if is_binary_operator(kind) => {
                    if self.config.space_around_operators {
                        if !output.ends_with(' ') && !at_line_start {
                            output.push(' ');
                        }
                        output.push_str(&self.format_token(token));
                        pending_space = true;
                        after_operator_no_space = false;
                    } else {
                        // No spaces around operators - don't add space even if pending
                        output.push_str(&self.format_token(token));
                        pending_space = false;
                        after_operator_no_space = true;
                    }
                    at_line_start = false;
                }

                // Keywords
                kind if is_keyword(kind) => {
                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space && !no_space_before(kind) {
                        output.push(' ');
                    }

                    output.push_str(&self.format_token(token));
                    at_line_start = false;
                    pending_space = self.config.space_after_keyword && !no_space_after(kind);
                }

                // Punctuation
                TokenKind::LeftParen => {
                    // No space before open paren in function calls
                    // This includes both identifiers AND keyword-functions like LEN, CHR$, etc.
                    // But DO add space after:
                    // - Control flow keywords like IF, WHILE
                    // - Binary operators when space_around_operators is enabled
                    let after_control_flow = last_kind.as_ref().is_some_and(|k| {
                        matches!(
                            k,
                            TokenKind::If
                                | TokenKind::While
                                | TokenKind::Until
                                | TokenKind::ElseIf
                                | TokenKind::Return
                        )
                    });
                    let after_operator = last_kind.as_ref().is_some_and(|k| is_binary_operator(k));

                    if pending_space && (after_control_flow || after_operator) {
                        output.push(' ');
                    }
                    output.push('(');
                    at_line_start = false;
                    pending_space = false;
                }

                TokenKind::RightParen => {
                    output.push(')');
                    at_line_start = false;
                    pending_space = true;
                }

                TokenKind::Comma => {
                    output.push(',');
                    at_line_start = false;
                    pending_space = self.config.space_after_comma;
                }

                TokenKind::Semicolon => {
                    output.push(';');
                    at_line_start = false;
                    // Keep space after semicolon in PRINT statements for readability
                    pending_space = true;
                }

                TokenKind::Colon => {
                    // Colon can be statement separator or label terminator
                    // Check if previous token was identifier (label)
                    if last_kind == Some(TokenKind::Identifier) {
                        output.push(':');
                        pending_space = false;
                    } else {
                        output.push(':');
                        pending_space = true;
                    }
                    at_line_start = false;
                }

                TokenKind::Hash => {
                    if pending_space {
                        output.push(' ');
                    }
                    output.push('#');
                    at_line_start = false;
                    pending_space = false;
                }

                TokenKind::Dot => {
                    output.push('.');
                    at_line_start = false;
                    pending_space = false;
                }

                // Literals
                TokenKind::IntegerLiteral
                | TokenKind::FloatLiteral
                | TokenKind::HexLiteral
                | TokenKind::OctalLiteral
                | TokenKind::BinaryLiteral => {
                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space && !after_operator_no_space {
                        output.push(' ');
                    }
                    output.push_str(&token.text);
                    at_line_start = false;
                    pending_space = true;
                    after_operator_no_space = false;
                }

                TokenKind::StringLiteral | TokenKind::UnterminatedString => {
                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space && !after_operator_no_space {
                        output.push(' ');
                    }
                    output.push_str(&token.text);
                    at_line_start = false;
                    pending_space = true;
                    after_operator_no_space = false;
                }

                // Identifiers
                TokenKind::Identifier => {
                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space
                        && !no_space_before(&TokenKind::Identifier)
                        && !after_operator_no_space
                    {
                        // Don't add space after dot
                        if last_kind != Some(TokenKind::Dot) {
                            output.push(' ');
                        }
                    }
                    output.push_str(&token.text);
                    at_line_start = false;
                    pending_space = true;
                    after_operator_no_space = false;
                }

                // Metacommands/preprocessor directives
                TokenKind::IncludeDirective
                | TokenKind::MetaIf
                | TokenKind::MetaElse
                | TokenKind::MetaElseIf
                | TokenKind::MetaEndIf
                | TokenKind::MetaLet
                | TokenKind::MetaChecking
                | TokenKind::MetaConsoleOnly
                | TokenKind::MetaConsole
                | TokenKind::MetaScreenHide
                | TokenKind::MetaScreenShow
                | TokenKind::MetaResizeOn
                | TokenKind::MetaResizeOff
                | TokenKind::MetaResizeStretch
                | TokenKind::MetaResizeSmooth
                | TokenKind::MetaStatic
                | TokenKind::MetaDynamic
                | TokenKind::MetaDebug
                | TokenKind::MetaIncludeOnce
                | TokenKind::MetaExeIcon
                | TokenKind::MetaVersionInfo
                | TokenKind::MetaError
                | TokenKind::MetaEmbed
                | TokenKind::MetaMidiSoundFont
                | TokenKind::MetaUnstable
                | TokenKind::MetaFormat
                | TokenKind::MetaUseLibrary
                | TokenKind::MetaCommand => {
                    // Metacommands are preserved as-is, typically at column 0
                    if !at_line_start && self.config.trim_trailing_whitespace {
                        while output.ends_with(' ') {
                            output.pop();
                        }
                    }
                    if !at_line_start {
                        output.push('\n');
                    }
                    output.push_str(&token.text);
                    at_line_start = false;
                    pending_space = false;
                }

                // Type suffixes (when standalone)
                TokenKind::DollarSign
                | TokenKind::Percent
                | TokenKind::Ampersand
                | TokenKind::Exclamation => {
                    // These should attach to previous identifier
                    output.push_str(&token.text);
                    at_line_start = false;
                    pending_space = true;
                }

                // Everything else
                _ => {
                    if at_line_start && self.config.indent_blocks {
                        output.push_str(&self.config.indent_string(indent_level));
                    } else if pending_space {
                        output.push(' ');
                    }
                    output.push_str(&token.text);
                    at_line_start = false;
                    pending_space = true;
                }
            }

            last_kind = Some(token.kind.clone());
            i += 1;
        }

        // Ensure file ends with newline
        if self.config.insert_final_newline && !output.ends_with('\n') {
            output.push('\n');
        }

        // Trim trailing whitespace from last line
        if self.config.trim_trailing_whitespace {
            while output.ends_with(" \n") || output.ends_with("\t\n") {
                let newline_pos = output.len() - 1;
                output.remove(newline_pos - 1);
            }
        }

        Ok(output)
    }

    /// Format a single token according to configuration.
    fn format_token(&self, token: &Token) -> String {
        // Check if this is a keyword and apply case transformation
        if is_keyword(&token.kind) {
            if let Some(canonical) = keyword_text(&token.kind) {
                return self.config.keyword_case.apply(canonical);
            }
        }
        token.text.clone()
    }

    /// Format a comment token.
    fn format_comment(&self, text: &str) -> String {
        // Preserve comment content but ensure proper formatting
        if text.starts_with('\'') {
            // Single-quote comment
            let content = text.strip_prefix('\'').unwrap_or(text);
            if content.starts_with(' ') || content.is_empty() {
                text.to_string()
            } else {
                format!("' {}", content)
            }
        } else if text.to_uppercase().starts_with("REM") {
            // REM comment - apply keyword case
            let prefix = &text[..3];
            let rest = &text[3..];
            let formatted_prefix = self.config.keyword_case.apply(prefix);
            format!("{}{}", formatted_prefix, rest)
        } else {
            text.to_string()
        }
    }

    /// Check if an IF statement at the given position is a single-line IF.
    fn is_single_line_if(&self, tokens: &[Token], if_pos: usize) -> bool {
        // Look for THEN followed by a statement on the same line
        let mut found_then = false;
        for token in tokens.iter().skip(if_pos + 1) {
            match token.kind {
                TokenKind::Newline => return found_then,
                TokenKind::Then => found_then = true,
                TokenKind::Comment | TokenKind::RemComment => {
                    // Comment after THEN is still single-line
                    if found_then {
                        continue;
                    }
                }
                _ => {
                    if found_then {
                        // There's content after THEN on the same line
                        return true;
                    }
                }
            }
        }
        found_then
    }

    /// Calculate the current column position in the output.
    fn current_column(&self, output: &str) -> usize {
        output
            .rfind('\n')
            .map(|pos| output.len() - pos - 1)
            .unwrap_or(output.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::KeywordCase;

    fn format(source: &str) -> String {
        Formatter::default_formatter().format(source).unwrap()
    }

    fn format_with(source: &str, config: FormatterConfig) -> String {
        Formatter::new(config).format(source).unwrap()
    }

    #[test]
    fn test_keyword_uppercase() {
        assert_eq!(format("print \"hello\""), "PRINT \"hello\"\n");
    }

    #[test]
    fn test_keyword_lowercase() {
        let config = FormatterConfig {
            keyword_case: KeywordCase::Lower,
            ..Default::default()
        };
        assert_eq!(format_with("PRINT \"hello\"", config), "print \"hello\"\n");
    }

    #[test]
    fn test_space_around_operators() {
        assert_eq!(format("x=1+2"), "x = 1 + 2\n");
        assert_eq!(format("IF x>1 THEN"), "IF x > 1 THEN\n");
    }

    #[test]
    fn test_no_space_around_operators() {
        let config = FormatterConfig {
            space_around_operators: false,
            ..Default::default()
        };
        assert_eq!(format_with("x = 1 + 2", config), "x=1+2\n");
    }

    #[test]
    fn test_space_after_comma() {
        assert_eq!(format("PRINT a,b,c"), "PRINT a, b, c\n");
    }

    #[test]
    fn test_comment_formatting() {
        assert_eq!(format("x = 1 'comment"), "x = 1 ' comment\n");
        assert_eq!(format("' already spaced"), "' already spaced\n");
    }

    #[test]
    fn test_rem_comment_case() {
        assert_eq!(format("rem this is a comment"), "REM this is a comment\n");
    }

    #[test]
    fn test_final_newline() {
        assert!(format("PRINT 1").ends_with('\n'));
    }

    #[test]
    fn test_preserve_string_literals() {
        assert_eq!(
            format("PRINT \"Hello, World!\""),
            "PRINT \"Hello, World!\"\n"
        );
    }

    #[test]
    fn test_function_call_no_space() {
        // No space between function name and opening paren
        assert_eq!(format("x = LEN(s$)"), "x = LEN(s$)\n");
    }
}
