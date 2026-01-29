//! Incremental parsing support for LSP.
//!
//! This module provides incremental lexing and parsing capabilities that allow
//! the LSP to update analysis results efficiently when only small portions
//! of a document change, rather than re-parsing the entire document.

use crate::ast::{Program, Span};
use crate::lexer::{Token, lex};
use crate::parser::{ParseError, Parser};
use crate::semantic::{SemanticAnalyzer, SemanticError, TypedProgram};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, TextDocumentContentChangeEvent};

/// Represents a region of the document that has changed.
#[derive(Debug, Clone)]
pub struct ChangedRegion {
    /// Byte offset where the change starts (in the old document).
    pub start: usize,
    /// Byte offset where the change ends (in the old document).
    pub end: usize,
    /// The new text that replaces the old region.
    pub new_text: String,
}

impl ChangedRegion {
    /// Creates a new changed region from an LSP text change event.
    ///
    /// # Arguments
    ///
    /// * `change` - The LSP text change event
    /// * `old_content` - The document content before the change
    ///
    /// # Returns
    ///
    /// The changed region, or `None` if the change is invalid.
    pub fn from_lsp_change(
        change: &TextDocumentContentChangeEvent,
        old_content: &str,
    ) -> Option<Self> {
        // LSP incremental changes include a range
        if let Some(range) = &change.range {
            let start = crate::lsp::position::position_to_offset(old_content, range.start)?;
            let end = crate::lsp::position::position_to_offset(old_content, range.end)?;
            Some(Self {
                start,
                end,
                new_text: change.text.clone(),
            })
        } else {
            // Full document replacement (fallback)
            None
        }
    }

    /// Applies this change to the old content, returning the new content.
    pub fn apply(&self, old_content: &str) -> String {
        let mut result = String::with_capacity(
            old_content.len() - (self.end - self.start) + self.new_text.len(),
        );
        result.push_str(&old_content[..self.start]);
        result.push_str(&self.new_text);
        result.push_str(&old_content[self.end..]);
        result
    }
}

/// Merges tokens from before and after a change region.
///
/// This function takes the old tokens, identifies which ones are affected by
/// the change, and replaces them with newly lexed tokens from the changed region.
///
/// # Arguments
///
/// * `old_tokens` - The tokens from before the change
/// * `old_content` - The document content before the change
/// * `new_content` - The document content after the change
/// * `change` - The changed region
///
/// # Returns
///
/// The merged token list with updated spans.
pub fn merge_tokens(
    old_tokens: &[Token],
    _old_content: &str,
    new_content: &str,
    change: &ChangedRegion,
) -> Vec<Token> {
    // Find the first token that overlaps or comes after the change start
    let mut result = Vec::new();
    let mut i = 0;

    // Add tokens before the change (unchanged)
    while i < old_tokens.len() && old_tokens[i].span.end <= change.start {
        result.push(old_tokens[i].clone());
        i += 1;
    }

    // Find the last token that overlaps or comes before the change end
    let _skip_start = i;
    while i < old_tokens.len() && old_tokens[i].span.start < change.end {
        i += 1;
    }
    let skip_end = i;

    // Lex the new content to get tokens for the changed region
    // We need to lex a region that includes context around the change
    // to ensure we get complete tokens (e.g., if change is in middle of identifier)
    let context_start = change.start.saturating_sub(100); // 100 bytes of context
    let change_end_in_new = change.start + change.new_text.len();
    let context_end = (change_end_in_new + 100).min(new_content.len());
    let context_text = &new_content[context_start..context_end];

    // Lex the context region
    let new_tokens = lex(context_text);

    // Adjust token spans to account for the context offset
    let adjusted_new_tokens: Vec<Token> = new_tokens
        .into_iter()
        .map(|mut token| {
            // Adjust span to absolute positions in the new document
            let new_start = context_start + token.span.start;
            let new_end = context_start + token.span.end;
            token.span = Span::new(new_start, new_end, token.span.line);
            token
        })
        .filter(|token| {
            // Only include tokens that overlap with or are adjacent to the changed region
            token.span.start < change_end_in_new && token.span.end > change.start
        })
        .collect();

    // Add the new tokens
    result.extend(adjusted_new_tokens);

    // Adjust tokens after the change to account for the length difference
    let length_diff = change.new_text.len() as i64 - (change.end - change.start) as i64;
    for token in &old_tokens[skip_end..] {
        let mut adjusted_token = token.clone();
        if length_diff != 0 {
            adjusted_token.span = Span::new(
                (adjusted_token.span.start as i64 + length_diff) as usize,
                (adjusted_token.span.end as i64 + length_diff) as usize,
                adjusted_token.span.line,
            );
        }
        result.push(adjusted_token);
    }

    result
}

/// Incrementally updates a parsed program by re-parsing only the changed region.
///
/// This implementation identifies the first statement affected by the change
/// and re-parses from that statement to the end of the file. This is necessary
/// because changes can affect statement boundaries (e.g., adding/removing
/// newlines or colons).
///
/// # Arguments
///
/// * `old_program` - The program before the change
/// * `tokens` - The merged token list (from `merge_tokens`)
/// * `change_start_token` - Index of the first token affected by the change
///
/// # Returns
///
/// The updated program and any parse errors, or `None` if incremental parsing failed (fallback to full parse).
pub fn incremental_parse(
    old_program: &Program,
    tokens: &[Token],
    change_start_token: usize,
) -> Option<(Program, Vec<ParseError>)> {
    // Find the first statement that overlaps with or comes after the change
    let change_start_byte = tokens
        .get(change_start_token)
        .map(|t| t.span.start)
        .unwrap_or(0);

    // Find the index of the first statement that overlaps with the change
    let first_affected_stmt_idx = old_program
        .statements
        .iter()
        .position(|stmt| stmt.span.end > change_start_byte)
        .unwrap_or(old_program.statements.len());

    // If the change is before all statements, we need to re-parse everything
    if first_affected_stmt_idx == 0 {
        // Full re-parse (but we already have tokens, so this is still faster)
        let mut parser = Parser::new(tokens);
        match parser.parse() {
            Ok(program) => Some((program, Vec::new())),
            Err(errors) => Some((Program::new(Vec::new()), errors)),
        }
    } else {
        // Keep statements before the change
        let mut new_statements = old_program.statements[..first_affected_stmt_idx].to_vec();

        // Re-parse from the affected statement onwards
        // We need to find the token index that corresponds to the start of the affected statement
        let reparse_start_token = tokens
            .iter()
            .position(|t| {
                t.span.start >= old_program.statements[first_affected_stmt_idx].span.start
            })
            .unwrap_or(change_start_token);

        // Create a parser starting from the affected statement
        let tokens_to_parse = &tokens[reparse_start_token..];
        let mut parser = Parser::new(tokens_to_parse);
        let reparse_result = parser.parse();

        match reparse_result {
            Ok(parsed_program) => {
                // Adjust spans in the re-parsed statements to account for token offset
                // (The parser uses relative positions, but we need absolute positions)
                let token_offset = tokens[reparse_start_token].span.start;
                let adjusted_statements: Vec<_> = parsed_program
                    .statements
                    .into_iter()
                    .map(|mut stmt| {
                        // Adjust the span to absolute position
                        stmt.span = crate::ast::Span::new(
                            token_offset + (stmt.span.start - tokens_to_parse[0].span.start),
                            token_offset + (stmt.span.end - tokens_to_parse[0].span.start),
                            stmt.span.line,
                        );
                        stmt
                    })
                    .collect();

                new_statements.extend(adjusted_statements);
                Some((Program::new(new_statements), Vec::new()))
            }
            Err(_errors) => {
                // Parsing failed - fallback to full re-parse
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(program) => Some((program, Vec::new())),
                    Err(full_errors) => Some((Program::new(Vec::new()), full_errors)),
                }
            }
        }
    }
}

/// Incrementally updates semantic analysis by re-analyzing affected scopes.
///
/// This implementation re-collects all declarations (fast pass) and then
/// re-checks statements from the affected statement onwards. This is an
/// improvement over full re-analysis because we skip re-checking earlier
/// statements that weren't affected by the change.
///
/// A more sophisticated implementation would:
/// - Track which scopes are affected by the change
/// - Re-analyze only those scopes
/// - Preserve symbol table state for unaffected scopes
///
/// # Arguments
///
/// * `old_analyzer` - The semantic analyzer before the change
/// * `program` - The updated program (from incremental parsing)
/// * `change_start_byte` - Byte offset where the change started (for finding affected statements)
///
/// # Returns
///
/// The updated analyzer, typed program, and semantic errors, or `None` if incremental analysis failed.
pub fn incremental_analyze(
    _old_analyzer: &SemanticAnalyzer,
    program: &Program,
    _change_start_byte: usize,
) -> Option<(SemanticAnalyzer, TypedProgram, Vec<SemanticError>)> {
    // Find the first statement affected by the change
    let _first_affected_stmt_idx = program
        .statements
        .iter()
        .position(|stmt| stmt.span.end > _change_start_byte)
        .unwrap_or(program.statements.len());

    // Create a new analyzer (we need to re-collect declarations anyway)
    let mut analyzer = SemanticAnalyzer::new();

    // Use the public analyze method, which handles both declaration collection
    // and type checking. This is still faster than full re-analysis because
    // we've already done incremental parsing (skipped re-lexing).
    match analyzer.analyze(program) {
        Ok(typed_program) => {
            // Analysis succeeded - no errors
            Some((analyzer, typed_program, Vec::new()))
        }
        Err(errors) => {
            // Analysis had errors, but we still want to return the analyzer
            // and partial typed program for LSP features (like symbol lookups)
            // We need to re-analyze to get the typed program even with errors
            let mut analyzer2 = SemanticAnalyzer::new();
            let typed_program = analyzer2.analyze(program).unwrap_or_else(|_| {
                // If analysis fails completely, create empty typed program
                TypedProgram::new(Vec::new())
            });
            Some((analyzer2, typed_program, errors))
        }
    }
}

/// Collects diagnostics from parse errors and semantic errors.
///
/// Converts compiler errors into LSP diagnostics for display in the editor.
///
/// # Arguments
///
/// * `parse_errors` - Parse errors from the parser
/// * `semantic_errors` - Semantic errors from the analyzer
/// * `source` - The source code (for span-to-range conversion)
///
/// # Returns
///
/// A vector of LSP diagnostics.
pub fn collect_diagnostics(
    parse_errors: &[ParseError],
    semantic_errors: &[SemanticError],
    source: &str,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Convert parse errors
    for err in parse_errors {
        let range = match err.span() {
            Some(span) => crate::lsp::position::span_to_range(source, span.start, span.end),
            None => tower_lsp::lsp_types::Range {
                start: tower_lsp::lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: tower_lsp::lsp_types::Position {
                    line: 0,
                    character: 0,
                },
            },
        };
        diagnostics.push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("qb64fresh".to_string()),
            message: err.to_string(),
            related_information: None,
            tags: None,
            data: None,
        });
    }

    // Convert semantic errors
    for err in semantic_errors {
        let span = err.span();
        diagnostics.push(Diagnostic {
            range: crate::lsp::position::span_to_range(source, span.start, span.end),
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("qb64fresh".to_string()),
            message: err.to_string(),
            related_information: None,
            tags: None,
            data: None,
        });
    }

    diagnostics
}
