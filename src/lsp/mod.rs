//! Language Server Protocol implementation for QB64Fresh.
//!
//! This module provides an LSP server that enables IDE features like:
//! - Real-time error diagnostics
//! - Hover information (types, documentation)
//! - Go to definition
//! - Document symbols (outline)
//!
//! # Architecture
//!
//! The LSP server uses `tower-lsp` and communicates via JSON-RPC over stdio.
//! It maintains a cache of open documents and their analysis results.
//!
//! ```text
//! Editor (VSCode, etc.)
//!     ↓ JSON-RPC over stdio
//! QbLanguageServer
//!     ↓ Uses
//! QB64Fresh Compiler (lexer, parser, semantic)
//! ```

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::lexer::lex;
use crate::parser::Parser;
use crate::semantic::SemanticAnalyzer;

/// State for a single open document.
#[derive(Debug)]
pub struct DocumentState {
    /// The document's content.
    pub content: String,
    /// The document's version (for incremental updates).
    pub version: i32,
}

/// Shared state for the language server.
#[derive(Debug, Default)]
pub struct ServerState {
    /// Open documents indexed by URI.
    pub documents: HashMap<Url, DocumentState>,
}

/// The QB64Fresh Language Server.
pub struct QbLanguageServer {
    /// Client handle for sending notifications.
    client: Client,
    /// Shared server state.
    state: Arc<RwLock<ServerState>>,
}

impl QbLanguageServer {
    /// Creates a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(RwLock::new(ServerState::default())),
        }
    }

    /// Analyzes a document and publishes diagnostics.
    async fn analyze_document(&self, uri: &Url, content: &str) {
        let diagnostics = self.get_diagnostics(content);
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    /// Runs the compiler pipeline and collects diagnostics.
    fn get_diagnostics(&self, source: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Lexer phase
        let tokens = lex(source);

        // Parser phase
        let mut parser = Parser::new(&tokens);
        let program = match parser.parse() {
            Ok(p) => p,
            Err(errors) => {
                // Convert parse errors to diagnostics
                for err in errors {
                    // Get span if available, or use start of file for errors without position
                    let range = match err.span() {
                        Some(span) => span_to_range(source, span.start, span.end),
                        None => Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
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
                return diagnostics;
            }
        };

        // Semantic analysis phase
        let mut analyzer = SemanticAnalyzer::new();
        if let Err(errors) = analyzer.analyze(&program) {
            for err in errors {
                let span = err.span();
                diagnostics.push(Diagnostic {
                    range: span_to_range(source, span.start, span.end),
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
        }

        diagnostics
    }

    /// Gets hover information at a position.
    fn get_hover_info(&self, source: &str, position: Position) -> Option<String> {
        // Convert position to byte offset
        let offset = position_to_offset(source, position)?;

        // Lex the source
        let tokens = lex(source);

        // Find the token at this position
        for token in &tokens {
            if token.span.start <= offset && offset < token.span.end {
                // Return info about this token
                return Some(format!(
                    "**Token:** `{:?}`\n\n**Text:** `{}`",
                    token.kind, token.text
                ));
            }
        }

        None
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for QbLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Document sync - we want full content on each change
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // Hover support
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                // Diagnostics are published proactively (no explicit capability needed)
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "qb64fresh-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "QB64Fresh LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let version = params.text_document.version;

        // Store document state
        {
            let mut state = self.state.write().await;
            state.documents.insert(
                uri.clone(),
                DocumentState {
                    content: content.clone(),
                    version,
                },
            );
        }

        // Analyze and publish diagnostics
        self.analyze_document(&uri, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        // Get the new content (we're using FULL sync, so there's one change with full content)
        if let Some(change) = params.content_changes.into_iter().next() {
            let content = change.text;

            // Update document state
            {
                let mut state = self.state.write().await;
                if let Some(doc) = state.documents.get_mut(&uri) {
                    doc.content = content.clone();
                    doc.version = params.text_document.version;
                }
            }

            // Re-analyze and publish diagnostics
            self.analyze_document(&uri, &content).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        // Remove document from state
        {
            let mut state = self.state.write().await;
            state.documents.remove(&uri);
        }

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content
            && let Some(info) = self.get_hover_info(&content, position)
        {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: info,
                }),
                range: None,
            }));
        }

        Ok(None)
    }
}

/// Converts a byte range to an LSP Range.
fn span_to_range(source: &str, start: usize, end: usize) -> Range {
    let start_pos = offset_to_position(source, start);
    let end_pos = offset_to_position(source, end);
    Range {
        start: start_pos,
        end: end_pos,
    }
}

/// Converts a byte offset to an LSP Position (line, character).
fn offset_to_position(source: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;

    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }

    Position { line, character }
}

/// Converts an LSP Position to a byte offset.
fn position_to_offset(source: &str, position: Position) -> Option<usize> {
    let mut current_line = 0u32;
    let mut current_char = 0u32;

    for (i, c) in source.char_indices() {
        if current_line == position.line && current_char == position.character {
            return Some(i);
        }
        if c == '\n' {
            if current_line == position.line {
                // Position is past end of line
                return Some(i);
            }
            current_line += 1;
            current_char = 0;
        } else {
            current_char += 1;
        }
    }

    // Position might be at the very end
    if current_line == position.line {
        Some(source.len())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
