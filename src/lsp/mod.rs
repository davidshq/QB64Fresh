//! Language Server Protocol implementation for QB64Fresh.
//!
//! This module provides an LSP server that enables IDE features like:
//! - Real-time error diagnostics
//! - Hover information (types, documentation)
//! - Go to definition
//! - Document symbols (outline view, Ctrl+Shift+O)
//! - Workspace symbols (Ctrl+T search)
//! - Inlay hints (type annotations)
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

mod analysis;
mod position;
mod signatures;
#[cfg(test)]
mod tests;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::ast::Span;
use crate::lexer::{TokenKind, lex};
use crate::semantic::DocumentSymbolKind;

use analysis::AnalysisCache;
use position::{
    format_basic_type, has_type_suffix, offset_to_position, position_to_offset, span_to_range,
};
use signatures::get_builtin_signature;

/// State for a single open document.
pub struct DocumentState {
    /// The document's content.
    pub content: String,
    /// The document's version (for incremental updates).
    pub version: i32,
    /// Cached analysis results (AST, typed IR, diagnostics).
    /// Wrapped in Arc to allow sharing without cloning SemanticAnalyzer.
    pub analysis: Option<Arc<AnalysisCache>>,
}

/// Shared state for the language server.
#[derive(Default)]
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

    /// Gets cached analysis for a document, or creates a new one if missing/invalid.
    ///
    /// This is an async helper that checks the document state for cached analysis.
    /// If the cache exists and is valid for the current version, returns a reference to it.
    /// Otherwise, a fresh analysis is performed and cached.
    async fn get_or_analyze(&self, uri: &Url, content: &str, version: i32) -> Arc<AnalysisCache> {
        // Check if we have valid cached analysis
        {
            let state = self.state.read().await;
            if let Some(doc) = state.documents.get(uri)
                && let Some(ref cache) = doc.analysis
                && cache.is_valid_for(version)
            {
                return Arc::clone(cache);
            }
        }

        // No valid cache - perform fresh analysis
        let cache = Arc::new(AnalysisCache::analyze_document(content, version));

        // Store the cache
        {
            let mut state = self.state.write().await;
            if let Some(doc) = state.documents.get_mut(uri) {
                doc.analysis = Some(Arc::clone(&cache));
            }
        }

        cache
    }

    /// Gets hover information at a position using cached analysis.
    fn get_hover_info(
        &self,
        source: &str,
        position: Position,
        cache: &Arc<AnalysisCache>,
    ) -> Option<String> {
        // Convert position to byte offset
        let offset = position_to_offset(source, position)?;

        // Lex the source (we still need tokens for position lookup)
        let tokens = lex(source);

        // Find the token at this position
        let token = tokens
            .iter()
            .find(|t| t.span.start <= offset && offset < t.span.end)?;

        // For identifiers, try to get detailed symbol information from cached analyzer
        if token.kind == TokenKind::Identifier
            && let Some(ref analyzer) = cache.analyzer
            && let Some(info) = analyzer.get_hover_info(&token.text)
        {
            return Some(info);
        }

        // Fallback: show token information for non-identifiers or unresolved symbols
        Some(format!(
            "**{}**\n\n`{}`",
            format!("{:?}", token.kind).replace("Kind", ""),
            token.text
        ))
    }

    /// Finds the definition location of a symbol at the given position using cached analysis.
    ///
    /// Returns the definition span if the position is on an identifier that
    /// references a defined symbol (variable, procedure, label, or type).
    fn find_definition(
        &self,
        source: &str,
        position: Position,
        cache: &Arc<AnalysisCache>,
    ) -> Option<Span> {
        // Convert position to byte offset
        let offset = position_to_offset(source, position)?;

        // Lex the source (we still need tokens for position lookup)
        let tokens = lex(source);

        // Find the identifier token at this position
        let identifier_name = tokens.iter().find_map(|token| {
            if token.span.start <= offset && offset < token.span.end {
                // Only handle identifiers
                if token.kind == TokenKind::Identifier {
                    return Some(token.text.clone());
                }
            }
            None
        })?;

        // Use cached analyzer to find definition
        if let Some(ref analyzer) = cache.analyzer {
            return analyzer.find_definition(&identifier_name);
        }

        None
    }

    /// Gets all document symbols for the outline view using cached analysis.
    fn get_document_symbols(
        &self,
        source: &str,
        cache: &Arc<AnalysisCache>,
    ) -> Vec<SymbolInformation> {
        // Use cached analyzer if available
        if let Some(ref analyzer) = cache.analyzer {
            return analyzer
                .get_document_symbols()
                .into_iter()
                .map(|sym| SymbolInformation {
                    name: sym.name,
                    kind: match sym.kind {
                        DocumentSymbolKind::Sub => SymbolKind::FUNCTION,
                        DocumentSymbolKind::Function => SymbolKind::FUNCTION,
                        DocumentSymbolKind::Type => SymbolKind::STRUCT,
                        DocumentSymbolKind::Constant => SymbolKind::CONSTANT,
                        DocumentSymbolKind::Array => SymbolKind::ARRAY,
                        DocumentSymbolKind::Variable => SymbolKind::VARIABLE,
                    },
                    location: Location {
                        uri: Url::parse("file:///").expect("file:/// should be a valid URL"), // Will be replaced
                        range: span_to_range(source, sym.span.start, sym.span.end),
                    },
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    container_name: None,
                })
                .collect();
        }

        Vec::new()
    }

    /// Gets completion items for code completion using cached analysis.
    fn get_completions(
        &self,
        _source: &str,
        _position: Position,
        cache: &Arc<AnalysisCache>,
    ) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Add keywords
        let keywords = [
            ("IF", "IF condition THEN\n\nEND IF"),
            ("FOR", "FOR var = start TO end\n\nNEXT var"),
            ("WHILE", "WHILE condition\n\nWEND"),
            ("DO", "DO WHILE condition\n\nLOOP"),
            (
                "SELECT CASE",
                "SELECT CASE expression\n    CASE value\n\nEND SELECT",
            ),
            ("SUB", "SUB name()\n\nEND SUB"),
            ("FUNCTION", "FUNCTION name()\n\nEND FUNCTION"),
            ("DIM", "DIM variable AS type"),
            ("CONST", "CONST name = value"),
            ("TYPE", "TYPE name\n    member AS type\nEND TYPE"),
            ("PRINT", "PRINT expression"),
            ("INPUT", "INPUT \"prompt\"; variable"),
            ("OPEN", "OPEN filename FOR mode AS #filenum"),
            ("CLOSE", "CLOSE #filenum"),
            ("GOTO", "GOTO label"),
            ("GOSUB", "GOSUB label"),
            ("RETURN", "RETURN"),
            ("EXIT", "EXIT FOR/SUB/FUNCTION/DO/WHILE"),
            ("END", "END"),
        ];

        for (keyword, snippet) in keywords {
            completions.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("Keyword".to_string()),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            });
        }

        // Add user-defined procedures from cached analyzer
        if let Some(ref analyzer) = cache.analyzer {
            for sym in analyzer.get_document_symbols() {
                let (kind, detail) = match sym.kind {
                    DocumentSymbolKind::Sub => (CompletionItemKind::FUNCTION, "SUB"),
                    DocumentSymbolKind::Function => (CompletionItemKind::FUNCTION, "FUNCTION"),
                    DocumentSymbolKind::Variable => (CompletionItemKind::VARIABLE, "Variable"),
                    DocumentSymbolKind::Array => (CompletionItemKind::VARIABLE, "Array"),
                    DocumentSymbolKind::Constant => (CompletionItemKind::CONSTANT, "CONST"),
                    DocumentSymbolKind::Type => (CompletionItemKind::STRUCT, "TYPE"),
                };
                completions.push(CompletionItem {
                    label: sym.name,
                    kind: Some(kind),
                    detail: Some(detail.to_string()),
                    ..Default::default()
                });
            }
        }

        // Add common built-in functions
        let builtins = [
            ("ABS", "ABS(number) - Absolute value"),
            ("ASC", "ASC(string$) - ASCII code of first character"),
            ("ATN", "ATN(number) - Arctangent"),
            ("CHR$", "CHR$(code) - Character from ASCII code"),
            ("COS", "COS(angle) - Cosine"),
            ("EXP", "EXP(power) - e raised to power"),
            ("FIX", "FIX(number) - Truncate toward zero"),
            ("INKEY$", "INKEY$ - Read keyboard without waiting"),
            ("INSTR", "INSTR([start,] string$, search$) - Find substring"),
            ("INT", "INT(number) - Truncate toward negative infinity"),
            ("LCASE$", "LCASE$(string$) - Convert to lowercase"),
            ("LEFT$", "LEFT$(string$, n) - Left n characters"),
            ("LEN", "LEN(string$) - Length of string"),
            ("LOG", "LOG(number) - Natural logarithm"),
            ("LTRIM$", "LTRIM$(string$) - Remove leading spaces"),
            ("MID$", "MID$(string$, start[, length]) - Substring"),
            ("RIGHT$", "RIGHT$(string$, n) - Right n characters"),
            ("RND", "RND[(n)] - Random number 0 to 1"),
            ("RTRIM$", "RTRIM$(string$) - Remove trailing spaces"),
            ("SGN", "SGN(number) - Sign (-1, 0, or 1)"),
            ("SIN", "SIN(angle) - Sine"),
            ("SPACE$", "SPACE$(n) - String of n spaces"),
            ("SQR", "SQR(number) - Square root"),
            ("STR$", "STR$(number) - Convert number to string"),
            ("STRING$", "STRING$(n, char) - String of n copies of char"),
            ("TAN", "TAN(angle) - Tangent"),
            ("TIMER", "TIMER - Seconds since midnight"),
            ("UCASE$", "UCASE$(string$) - Convert to uppercase"),
            ("VAL", "VAL(string$) - Convert string to number"),
        ];

        for (name, detail) in builtins {
            completions.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail.to_string()),
                ..Default::default()
            });
        }

        completions
    }

    /// Gets signature help for a function call at the given position.
    ///
    /// This finds the function being called and which parameter the cursor is on,
    /// then returns the function signature with the active parameter highlighted.
    fn get_signature_help(&self, source: &str, position: Position) -> Option<SignatureHelp> {
        let offset = position_to_offset(source, position)?;

        // Get the text up to the cursor to analyze the context
        let text_before = &source[..offset];

        // Find the last unmatched open parenthesis
        let mut paren_depth = 0i32;
        let mut func_call_start = None;
        let mut comma_count = 0u32;

        for (i, c) in text_before.char_indices().rev() {
            match c {
                ')' => paren_depth += 1,
                '(' => {
                    paren_depth -= 1;
                    if paren_depth < 0 {
                        // Found our unmatched open paren
                        func_call_start = Some(i);
                        break;
                    }
                }
                ',' if paren_depth == 0 => comma_count += 1,
                '\n' => {
                    // Don't look past line boundaries for safety
                    break;
                }
                _ => {}
            }
        }

        let func_start = func_call_start?;

        // Find the function name before the parenthesis
        let text_before_paren = &text_before[..func_start];
        let func_name = text_before_paren
            .chars()
            .rev()
            .take_while(|c| {
                c.is_alphanumeric()
                    || *c == '_'
                    || *c == '$'
                    || *c == '%'
                    || *c == '&'
                    || *c == '!'
                    || *c == '#'
            })
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>();

        if func_name.is_empty() {
            return None;
        }

        // Look up the function signature
        let sig = get_builtin_signature(&func_name.to_uppercase())?;

        Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label: sig.label.to_string(),
                documentation: Some(Documentation::String(sig.doc.to_string())),
                parameters: Some(
                    sig.params
                        .iter()
                        .map(|p| ParameterInformation {
                            label: ParameterLabel::Simple(p.to_string()),
                            documentation: None,
                        })
                        .collect(),
                ),
                active_parameter: Some(comma_count),
            }],
            active_signature: Some(0),
            active_parameter: Some(comma_count),
        })
    }

    /// Finds all references to an identifier at the given position using cached analysis.
    ///
    /// Returns a list of locations where the identifier appears in the document.
    /// This is a simple text-based search that finds all matching identifier tokens.
    fn find_references(
        &self,
        source: &str,
        position: Position,
        _cache: &Arc<AnalysisCache>,
    ) -> Vec<Range> {
        // Convert position to byte offset
        let offset = match position_to_offset(source, position) {
            Some(o) => o,
            None => return Vec::new(),
        };

        // Lex the source
        let tokens = lex(source);

        // Find the identifier at this position
        let target_name = tokens.iter().find_map(|token| {
            if token.span.start <= offset
                && offset < token.span.end
                && token.kind == TokenKind::Identifier
            {
                return Some(token.text.to_uppercase());
            }
            None
        });

        let target_name = match target_name {
            Some(name) => name,
            None => return Vec::new(),
        };

        // Find all occurrences of this identifier (case-insensitive)
        tokens
            .iter()
            .filter_map(|token| {
                if token.kind == TokenKind::Identifier && token.text.to_uppercase() == target_name {
                    Some(span_to_range(source, token.span.start, token.span.end))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Gets the identifier at the given position, returning its name and range.
    ///
    /// Used by prepare_rename to validate that the position is on a renameable identifier.
    fn get_identifier_at_position(
        &self,
        source: &str,
        position: Position,
    ) -> Option<(String, Range)> {
        let offset = position_to_offset(source, position)?;
        let tokens = lex(source);

        for token in &tokens {
            if token.span.start <= offset
                && offset < token.span.end
                && token.kind == TokenKind::Identifier
            {
                let range = span_to_range(source, token.span.start, token.span.end);
                return Some((token.text.clone(), range));
            }
        }
        None
    }

    /// Computes inlay hints for a document range using cached analysis.
    ///
    /// Currently provides:
    /// - Type hints after variable names in DIM statements (showing inferred or explicit types)
    /// - Return type hints for FUNCTION declarations
    ///
    /// The hints appear as subtle inline annotations in the editor.
    fn get_inlay_hints(
        &self,
        source: &str,
        range: Range,
        cache: &Arc<AnalysisCache>,
    ) -> Vec<InlayHint> {
        let mut hints = Vec::new();

        // Use cached analyzer if available
        let analyzer = match &cache.analyzer {
            Some(a) => a,
            None => return hints,
        };

        // Convert range to byte offsets for filtering
        let range_start = position_to_offset(source, range.start).unwrap_or(0);
        let range_end = position_to_offset(source, range.end).unwrap_or(source.len());

        // Get all global symbols (variables, constants, arrays)
        for sym in analyzer.get_document_symbols() {
            // Skip symbols outside the requested range
            if sym.span.end < range_start || sym.span.start > range_end {
                continue;
            }

            // Get the actual symbol to access its type
            let type_str = if let Some(symbol) = analyzer.find_symbol_info(&sym.name) {
                format_basic_type(&symbol.basic_type)
            } else {
                continue;
            };

            // Don't show hints for symbols that already have explicit type suffixes
            if has_type_suffix(&sym.name) {
                continue;
            }

            // Create hint at the end of the variable name
            let hint_position = offset_to_position(source, sym.span.end);
            hints.push(InlayHint {
                position: hint_position,
                label: InlayHintLabel::String(format!(": {}", type_str)),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: Some(InlayHintTooltip::String(format!("Type of '{}'", sym.name))),
                padding_left: Some(false),
                padding_right: Some(true),
                data: None,
            });
        }

        hints
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for QbLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Document sync - use incremental updates for better performance
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        will_save: None,
                        will_save_wait_until: None,
                        save: None,
                    },
                )),
                // Hover support
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                // Go to definition support
                definition_provider: Some(OneOf::Left(true)),
                // Document symbol support (outline)
                document_symbol_provider: Some(OneOf::Left(true)),
                // Workspace symbol support (Ctrl+T)
                workspace_symbol_provider: Some(OneOf::Left(true)),
                // Code completion support
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                // Find references support
                references_provider: Some(OneOf::Left(true)),
                // Signature help support
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                // Inlay hints support (type annotations)
                inlay_hint_provider: Some(OneOf::Left(true)),
                // Rename symbol support
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
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
                    analysis: None, // Will be set by analyze_document
                },
            );
        }

        // Analyze and publish diagnostics (this will cache the results)
        let cache = Arc::new(AnalysisCache::analyze_document(&content, version));
        let diagnostics = cache.diagnostics.clone();

        // Store the cache
        {
            let mut state = self.state.write().await;
            if let Some(doc) = state.documents.get_mut(&uri) {
                doc.analysis = Some(Arc::clone(&cache));
            }
        }

        // Publish diagnostics
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Get the old document state
        let (old_content, old_cache) = {
            let state = self.state.read().await;
            if let Some(doc) = state.documents.get(&uri) {
                (doc.content.clone(), doc.analysis.as_ref().map(Arc::clone))
            } else {
                // Document doesn't exist - this shouldn't happen per LSP spec,
                // but handle gracefully by treating as new document
                (String::new(), None)
            }
        };

        // Process incremental changes
        let mut new_content = old_content.clone();
        let cache = if let Some(old_cache) = old_cache {
            // Try incremental update for each change
            let mut current_cache_opt: Option<AnalysisCache> = None;

            for change_event in &params.content_changes {
                if let Some(change) =
                    crate::lsp::analysis::ChangedRegion::from_lsp_change(change_event, &new_content)
                {
                    new_content = change.apply(&new_content);
                    // Update cache incrementally
                    let cache_to_update = current_cache_opt.as_ref().unwrap_or(&*old_cache);
                    let updated_cache =
                        cache_to_update.update_incremental(&change, &new_content, version);
                    current_cache_opt = Some(updated_cache);
                } else {
                    // Full document replacement - fallback to full analysis
                    new_content = change_event.text.clone();
                    current_cache_opt =
                        Some(AnalysisCache::analyze_document(&new_content, version));
                    break;
                }
            }

            if let Some(current_cache) = current_cache_opt {
                Arc::new(current_cache)
            } else {
                // No changes processed - keep old cache but update version
                old_cache
            }
        } else {
            // No old cache - do full analysis
            if let Some(change) = params.content_changes.into_iter().next() {
                new_content = change.text;
            }
            Arc::new(AnalysisCache::analyze_document(&new_content, version))
        };

        let diagnostics = cache.diagnostics.clone();

        // Store the cache and update document
        {
            let mut state = self.state.write().await;
            if let Some(doc) = state.documents.get_mut(&uri) {
                doc.content = new_content;
                doc.version = version;
                doc.analysis = Some(Arc::clone(&cache));
            } else {
                // Document doesn't exist - create it
                state.documents.insert(
                    uri.clone(),
                    DocumentState {
                        content: new_content,
                        version,
                        analysis: Some(Arc::clone(&cache)),
                    },
                );
            }
        }

        // Publish diagnostics
        self.client
            .publish_diagnostics(uri, diagnostics, Some(version))
            .await;
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

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            if let Some(info) = self.get_hover_info(&content, position, &cache) {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: info,
                    }),
                    range: None,
                }));
            }
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            if let Some(def_span) = self.find_definition(&content, position, &cache) {
                let range = span_to_range(&content, def_span.start, def_span.end);
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range,
                })));
            }
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            let mut symbols = self.get_document_symbols(&content, &cache);
            if symbols.is_empty() {
                return Ok(None);
            }
            // Set the correct URI on each symbol
            for sym in &mut symbols {
                sym.location.uri = uri.clone();
            }
            return Ok(Some(DocumentSymbolResponse::Flat(symbols)));
        }

        Ok(None)
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query.to_lowercase();
        let mut all_symbols = Vec::new();

        // Search across all open documents
        let documents = {
            let state = self.state.read().await;
            state
                .documents
                .iter()
                .map(|(uri, doc)| (uri.clone(), doc.content.clone(), doc.version))
                .collect::<Vec<_>>()
        };

        for (uri, content, version) in documents {
            // Get or create cached analysis for each document
            let cache = self.get_or_analyze(&uri, &content, version).await;
            let mut symbols = self.get_document_symbols(&content, &cache);

            // Filter by query if provided
            if !query.is_empty() {
                symbols.retain(|s| s.name.to_lowercase().contains(&query));
            }

            // Set the correct URI on each symbol
            for sym in &mut symbols {
                sym.location.uri = uri.clone();
            }

            all_symbols.extend(symbols);
        }

        if all_symbols.is_empty() {
            return Ok(None);
        }

        Ok(Some(all_symbols))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            let completions = self.get_completions(&content, position, &cache);
            if completions.is_empty() {
                return Ok(None);
            }
            return Ok(Some(CompletionResponse::Array(completions)));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            let ranges = self.find_references(&content, position, &cache);
            if ranges.is_empty() {
                return Ok(None);
            }
            let locations: Vec<Location> = ranges
                .into_iter()
                .map(|range| Location {
                    uri: uri.clone(),
                    range,
                })
                .collect();
            return Ok(Some(locations));
        }

        Ok(None)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            return Ok(self.get_signature_help(&content, position));
        }

        Ok(None)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let range = params.range;

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            let hints = self.get_inlay_hints(&content, range, &cache);
            if hints.is_empty() {
                return Ok(None);
            }
            return Ok(Some(hints));
        }

        Ok(None)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let position = params.position;

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            // Check if there's a valid identifier at this position
            if let Some((name, range)) = self.get_identifier_at_position(&content, position) {
                // Return the range and placeholder text for the rename dialog
                return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                    range,
                    placeholder: name,
                }));
            }
        }

        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = &params.new_name;

        // Get document content and version
        if let Some((content, version)) = {
            let state = self.state.read().await;
            state
                .documents
                .get(uri)
                .map(|d| (d.content.clone(), d.version))
        } {
            // Get or create cached analysis
            let cache = self.get_or_analyze(uri, &content, version).await;

            // Find all references to rename
            let ranges = self.find_references(&content, position, &cache);
            if ranges.is_empty() {
                return Ok(None);
            }

            // Create text edits for all occurrences
            let edits: Vec<TextEdit> = ranges
                .into_iter()
                .map(|range| TextEdit {
                    range,
                    new_text: new_name.clone(),
                })
                .collect();

            // Build workspace edit
            let mut changes = HashMap::new();
            changes.insert(uri.clone(), edits);

            return Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }));
        }

        Ok(None)
    }
}
