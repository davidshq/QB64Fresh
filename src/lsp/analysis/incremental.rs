//! Incremental parsing support for LSP.
//!
//! This module provides incremental lexing and parsing capabilities that allow
//! the LSP to update analysis results efficiently when only small portions
//! of a document change, rather than re-parsing the entire document.

use crate::ast::{Program, Span};
use crate::lexer::{Token, lex};
use crate::parser::Parser;
use crate::semantic::{SemanticAnalyzer, TypedProgram};
use tower_lsp::lsp_types::TextDocumentContentChangeEvent;

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
/// This is a simplified version that re-parses from the start of the change
/// to the end of the file. A more sophisticated implementation would:
/// - Identify the affected statement/block boundaries
/// - Re-parse only those sections
/// - Merge the results
///
/// # Arguments
///
/// * `old_program` - The program before the change
/// * `tokens` - The merged token list (from `merge_tokens`)
/// * `change_start_token` - Index of the first token affected by the change
///
/// # Returns
///
/// The updated program, or `None` if incremental parsing failed (fallback to full parse).
pub fn incremental_parse(
    _old_program: &Program,
    tokens: &[Token],
    _change_start_token: usize,
) -> Option<Program> {
    // For now, we do a full re-parse. A more sophisticated implementation
    // would identify statement boundaries and re-parse only affected sections.
    // This is still faster than re-lexing + re-parsing because we skip lexing.
    let mut parser = Parser::new(tokens);
    parser.parse().ok()
}

/// Incrementally updates semantic analysis by re-analyzing affected scopes.
///
/// This is a simplified version that re-analyzes the entire program.
/// A more sophisticated implementation would:
/// - Track which scopes are affected by the change
/// - Re-analyze only those scopes
/// - Preserve symbol table state for unaffected scopes
///
/// # Arguments
///
/// * `old_analyzer` - The semantic analyzer before the change
/// * `program` - The updated program (from incremental parsing)
///
/// # Returns
///
/// The updated analyzer and typed program, or `None` if incremental analysis failed.
pub fn incremental_analyze(
    _old_analyzer: &SemanticAnalyzer,
    program: &Program,
) -> Option<(SemanticAnalyzer, TypedProgram)> {
    // For now, we do a full re-analysis. A more sophisticated implementation
    // would track scope dependencies and re-analyze only affected scopes.
    let mut analyzer = SemanticAnalyzer::new();
    analyzer
        .analyze(program)
        .ok()
        .map(|typed_program| (analyzer, typed_program))
}
