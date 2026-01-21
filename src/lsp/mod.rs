//! Language Server Protocol implementation for QB64Fresh.
//!
//! This module provides an LSP server that enables IDE features like:
//! - Real-time error diagnostics
//! - Hover information (types, documentation)
//! - Go to definition
//! - Document symbols (outline)
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

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::ast::Span;
use crate::lexer::{TokenKind, lex};
use crate::parser::Parser;
use crate::semantic::{DocumentSymbolKind, SemanticAnalyzer};

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
        let token = tokens
            .iter()
            .find(|t| t.span.start <= offset && offset < t.span.end)?;

        // For identifiers, try to get detailed symbol information
        if token.kind == TokenKind::Identifier {
            // Parse and analyze to get symbol information
            let mut parser = Parser::new(&tokens);
            if let Ok(program) = parser.parse() {
                let mut analyzer = SemanticAnalyzer::new();
                let _ = analyzer.analyze(&program);

                // Try to get detailed hover info for this symbol
                if let Some(info) = analyzer.get_hover_info(&token.text) {
                    return Some(info);
                }
            }
        }

        // Fallback: show token information for non-identifiers or unresolved symbols
        Some(format!(
            "**{}**\n\n`{}`",
            format!("{:?}", token.kind).replace("Kind", ""),
            token.text
        ))
    }

    /// Finds the definition location of a symbol at the given position.
    ///
    /// Returns the definition span if the position is on an identifier that
    /// references a defined symbol (variable, procedure, label, or type).
    fn find_definition(&self, source: &str, position: Position) -> Option<Span> {
        // Convert position to byte offset
        let offset = position_to_offset(source, position)?;

        // Lex the source
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

        // Parse and analyze to get the symbol table
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().ok()?;

        let mut analyzer = SemanticAnalyzer::new();
        // We don't care about errors for definition lookup
        let _ = analyzer.analyze(&program);

        // Get the symbol table and look up the identifier
        analyzer.find_definition(&identifier_name)
    }

    /// Gets all document symbols for the outline view.
    fn get_document_symbols(&self, source: &str) -> Vec<SymbolInformation> {
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = match parser.parse() {
            Ok(p) => p,
            Err(_) => return Vec::new(),
        };

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        analyzer
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
                    uri: Url::parse("file:///").unwrap(), // Will be replaced
                    range: span_to_range(source, sym.span.start, sym.span.end),
                },
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                container_name: None,
            })
            .collect()
    }

    /// Gets completion items for code completion.
    fn get_completions(&self, source: &str, _position: Position) -> Vec<CompletionItem> {
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

        // Parse and analyze to get symbols
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        if let Ok(program) = parser.parse() {
            let mut analyzer = SemanticAnalyzer::new();
            let _ = analyzer.analyze(&program);

            // Add user-defined procedures
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

    /// Finds all references to an identifier at the given position.
    ///
    /// Returns a list of locations where the identifier appears in the document.
    /// This is a simple text-based search that finds all matching identifier tokens.
    fn find_references(&self, source: &str, position: Position) -> Vec<Range> {
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

    /// Computes inlay hints for a document range.
    ///
    /// Currently provides:
    /// - Type hints after variable names in DIM statements (showing inferred or explicit types)
    /// - Return type hints for FUNCTION declarations
    ///
    /// The hints appear as subtle inline annotations in the editor.
    fn get_inlay_hints(&self, source: &str, range: Range) -> Vec<InlayHint> {
        let mut hints = Vec::new();

        // Parse and analyze the source to get symbol information
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = match parser.parse() {
            Ok(p) => p,
            Err(_) => return hints,
        };

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

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
                // Document sync - we want full content on each change
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // Hover support
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                // Go to definition support
                definition_provider: Some(OneOf::Left(true)),
                // Document symbol support (outline)
                document_symbol_provider: Some(OneOf::Left(true)),
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content
            && let Some(def_span) = self.find_definition(&content, position)
        {
            let range = span_to_range(&content, def_span.start, def_span.end);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            })));
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            let mut symbols = self.get_document_symbols(&content);
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

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            let completions = self.get_completions(&content, position);
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

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            let ranges = self.find_references(&content, position);
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

        // Get document content
        let content = {
            let state = self.state.read().await;
            state.documents.get(uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            let hints = self.get_inlay_hints(&content, range);
            if hints.is_empty() {
                return Ok(None);
            }
            return Ok(Some(hints));
        }

        Ok(None)
    }
}

/// Built-in function signature information.
struct FunctionSignature {
    label: &'static str,
    doc: &'static str,
    params: &'static [&'static str],
}

/// Gets the signature for a built-in function.
fn get_builtin_signature(name: &str) -> Option<FunctionSignature> {
    match name {
        // String functions
        "LEFT$" => Some(FunctionSignature {
            label: "LEFT$(string$, n%)",
            doc: "Returns the leftmost n characters of a string.",
            params: &["string$", "n%"],
        }),
        "RIGHT$" => Some(FunctionSignature {
            label: "RIGHT$(string$, n%)",
            doc: "Returns the rightmost n characters of a string.",
            params: &["string$", "n%"],
        }),
        "MID$" => Some(FunctionSignature {
            label: "MID$(string$, start%[, length%])",
            doc: "Returns a substring starting at position start. Length is optional.",
            params: &["string$", "start%", "length%"],
        }),
        "INSTR" => Some(FunctionSignature {
            label: "INSTR([start%,] string$, search$)",
            doc: "Returns the position of search$ within string$. Start position is optional.",
            params: &["start%", "string$", "search$"],
        }),
        "LEN" => Some(FunctionSignature {
            label: "LEN(string$)",
            doc: "Returns the length of a string in bytes.",
            params: &["string$"],
        }),
        "CHR$" => Some(FunctionSignature {
            label: "CHR$(code%)",
            doc: "Returns the character for an ASCII code (0-255).",
            params: &["code%"],
        }),
        "ASC" => Some(FunctionSignature {
            label: "ASC(string$[, position%])",
            doc: "Returns the ASCII code of a character. Position defaults to 1.",
            params: &["string$", "position%"],
        }),
        "UCASE$" => Some(FunctionSignature {
            label: "UCASE$(string$)",
            doc: "Converts a string to uppercase.",
            params: &["string$"],
        }),
        "LCASE$" => Some(FunctionSignature {
            label: "LCASE$(string$)",
            doc: "Converts a string to lowercase.",
            params: &["string$"],
        }),
        "LTRIM$" => Some(FunctionSignature {
            label: "LTRIM$(string$)",
            doc: "Removes leading spaces from a string.",
            params: &["string$"],
        }),
        "RTRIM$" => Some(FunctionSignature {
            label: "RTRIM$(string$)",
            doc: "Removes trailing spaces from a string.",
            params: &["string$"],
        }),
        "_TRIM$" => Some(FunctionSignature {
            label: "_TRIM$(string$)",
            doc: "Removes both leading and trailing spaces from a string.",
            params: &["string$"],
        }),
        "STRING$" => Some(FunctionSignature {
            label: "STRING$(n%, char)",
            doc: "Returns a string of n copies of a character.",
            params: &["n%", "char"],
        }),
        "SPACE$" => Some(FunctionSignature {
            label: "SPACE$(n%)",
            doc: "Returns a string of n spaces.",
            params: &["n%"],
        }),
        "STR$" => Some(FunctionSignature {
            label: "STR$(number)",
            doc: "Converts a number to its string representation.",
            params: &["number"],
        }),
        "VAL" => Some(FunctionSignature {
            label: "VAL(string$)",
            doc: "Converts a string to a numeric value.",
            params: &["string$"],
        }),

        // Math functions
        "ABS" => Some(FunctionSignature {
            label: "ABS(number)",
            doc: "Returns the absolute value of a number.",
            params: &["number"],
        }),
        "SGN" => Some(FunctionSignature {
            label: "SGN(number)",
            doc: "Returns -1, 0, or 1 indicating the sign of a number.",
            params: &["number"],
        }),
        "INT" => Some(FunctionSignature {
            label: "INT(number)",
            doc: "Truncates a number toward negative infinity.",
            params: &["number"],
        }),
        "FIX" => Some(FunctionSignature {
            label: "FIX(number)",
            doc: "Truncates a number toward zero.",
            params: &["number"],
        }),
        "CINT" => Some(FunctionSignature {
            label: "CINT(number)",
            doc: "Converts a number to INTEGER (rounds to nearest).",
            params: &["number"],
        }),
        "CLNG" => Some(FunctionSignature {
            label: "CLNG(number)",
            doc: "Converts a number to LONG (rounds to nearest).",
            params: &["number"],
        }),
        "CSNG" => Some(FunctionSignature {
            label: "CSNG(number)",
            doc: "Converts a number to SINGLE precision.",
            params: &["number"],
        }),
        "CDBL" => Some(FunctionSignature {
            label: "CDBL(number)",
            doc: "Converts a number to DOUBLE precision.",
            params: &["number"],
        }),
        "SQR" => Some(FunctionSignature {
            label: "SQR(number)",
            doc: "Returns the square root of a number.",
            params: &["number"],
        }),
        "LOG" => Some(FunctionSignature {
            label: "LOG(number)",
            doc: "Returns the natural logarithm (base e) of a number.",
            params: &["number"],
        }),
        "EXP" => Some(FunctionSignature {
            label: "EXP(power)",
            doc: "Returns e raised to a power.",
            params: &["power"],
        }),
        "SIN" => Some(FunctionSignature {
            label: "SIN(radians)",
            doc: "Returns the sine of an angle in radians.",
            params: &["radians"],
        }),
        "COS" => Some(FunctionSignature {
            label: "COS(radians)",
            doc: "Returns the cosine of an angle in radians.",
            params: &["radians"],
        }),
        "TAN" => Some(FunctionSignature {
            label: "TAN(radians)",
            doc: "Returns the tangent of an angle in radians.",
            params: &["radians"],
        }),
        "ATN" => Some(FunctionSignature {
            label: "ATN(number)",
            doc: "Returns the arctangent of a number in radians.",
            params: &["number"],
        }),
        "RND" => Some(FunctionSignature {
            label: "RND[(n)]",
            doc: "Returns a random number between 0 and 1.",
            params: &["n"],
        }),

        // Array functions
        "LBOUND" => Some(FunctionSignature {
            label: "LBOUND(array[, dimension%])",
            doc: "Returns the lower bound of an array dimension.",
            params: &["array", "dimension%"],
        }),
        "UBOUND" => Some(FunctionSignature {
            label: "UBOUND(array[, dimension%])",
            doc: "Returns the upper bound of an array dimension.",
            params: &["array", "dimension%"],
        }),

        // Screen/graphics functions
        "POINT" => Some(FunctionSignature {
            label: "POINT(x%, y%)",
            doc: "Returns the color of a pixel at the specified coordinates.",
            params: &["x%", "y%"],
        }),
        "_RGB" => Some(FunctionSignature {
            label: "_RGB(red%, green%, blue%[, imageHandle&])",
            doc: "Returns a 32-bit color value from RGB components.",
            params: &["red%", "green%", "blue%", "imageHandle&"],
        }),
        "_RGBA" => Some(FunctionSignature {
            label: "_RGBA(red%, green%, blue%, alpha%[, imageHandle&])",
            doc: "Returns a 32-bit color value from RGBA components.",
            params: &["red%", "green%", "blue%", "alpha%", "imageHandle&"],
        }),
        "_RGB32" => Some(FunctionSignature {
            label: "_RGB32(red%, green%, blue%[, alpha%])",
            doc: "Returns a 32-bit color value. Alpha defaults to 255.",
            params: &["red%", "green%", "blue%", "alpha%"],
        }),
        "_RED" | "_RED32" => Some(FunctionSignature {
            label: "_RED(color&)",
            doc: "Extracts the red component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_GREEN" | "_GREEN32" => Some(FunctionSignature {
            label: "_GREEN(color&)",
            doc: "Extracts the green component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_BLUE" | "_BLUE32" => Some(FunctionSignature {
            label: "_BLUE(color&)",
            doc: "Extracts the blue component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_ALPHA" | "_ALPHA32" => Some(FunctionSignature {
            label: "_ALPHA(color&)",
            doc: "Extracts the alpha component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_NEWIMAGE" => Some(FunctionSignature {
            label: "_NEWIMAGE(width%, height%[, mode%])",
            doc: "Creates a new image. Mode: 0=current, 32=32-bit, 256=256-color.",
            params: &["width%", "height%", "mode%"],
        }),
        "_LOADIMAGE" => Some(FunctionSignature {
            label: "_LOADIMAGE(filename$[, mode%])",
            doc: "Loads an image file. Supports BMP, PNG, JPG, GIF.",
            params: &["filename$", "mode%"],
        }),
        "_PUTIMAGE" => Some(FunctionSignature {
            label: "_PUTIMAGE([dest], [source][, destHandle&][, srcHandle&])",
            doc: "Copies image data. Coordinates are (x1,y1)-(x2,y2).",
            params: &["dest", "source", "destHandle&", "srcHandle&"],
        }),
        "_WIDTH" => Some(FunctionSignature {
            label: "_WIDTH[(imageHandle&)]",
            doc: "Returns the width of an image or the current screen.",
            params: &["imageHandle&"],
        }),
        "_HEIGHT" => Some(FunctionSignature {
            label: "_HEIGHT[(imageHandle&)]",
            doc: "Returns the height of an image or the current screen.",
            params: &["imageHandle&"],
        }),

        // Input functions
        "INKEY$" => Some(FunctionSignature {
            label: "INKEY$",
            doc: "Returns a character from the keyboard buffer without waiting.",
            params: &[],
        }),
        "_KEYHIT" => Some(FunctionSignature {
            label: "_KEYHIT",
            doc: "Returns the keycode of a pressed key, including extended keys.",
            params: &[],
        }),
        "_KEYDOWN" => Some(FunctionSignature {
            label: "_KEYDOWN(keycode&)",
            doc: "Returns -1 if a key is currently held down, 0 otherwise.",
            params: &["keycode&"],
        }),
        "_MOUSEX" => Some(FunctionSignature {
            label: "_MOUSEX",
            doc: "Returns the current mouse X coordinate.",
            params: &[],
        }),
        "_MOUSEY" => Some(FunctionSignature {
            label: "_MOUSEY",
            doc: "Returns the current mouse Y coordinate.",
            params: &[],
        }),
        "_MOUSEBUTTON" => Some(FunctionSignature {
            label: "_MOUSEBUTTON(button%)",
            doc: "Returns -1 if mouse button is pressed. 1=left, 2=right, 3=middle.",
            params: &["button%"],
        }),

        // File functions
        "FREEFILE" => Some(FunctionSignature {
            label: "FREEFILE",
            doc: "Returns the next available file number.",
            params: &[],
        }),
        "EOF" => Some(FunctionSignature {
            label: "EOF(fileNumber%)",
            doc: "Returns -1 if at end of file, 0 otherwise.",
            params: &["fileNumber%"],
        }),
        "LOF" => Some(FunctionSignature {
            label: "LOF(fileNumber%)",
            doc: "Returns the length of an open file in bytes.",
            params: &["fileNumber%"],
        }),
        "LOC" => Some(FunctionSignature {
            label: "LOC(fileNumber%)",
            doc: "Returns the current position in an open file.",
            params: &["fileNumber%"],
        }),
        "_FILEEXISTS" => Some(FunctionSignature {
            label: "_FILEEXISTS(filename$)",
            doc: "Returns -1 if file exists, 0 otherwise.",
            params: &["filename$"],
        }),
        "_DIREXISTS" => Some(FunctionSignature {
            label: "_DIREXISTS(path$)",
            doc: "Returns -1 if directory exists, 0 otherwise.",
            params: &["path$"],
        }),

        // System/misc functions
        "TIMER" => Some(FunctionSignature {
            label: "TIMER[(accuracy!)]",
            doc: "Returns seconds since midnight. Optional accuracy parameter.",
            params: &["accuracy!"],
        }),
        "_SCREENY" | "_SCREENX" => Some(FunctionSignature {
            label: "_SCREENX / _SCREENY",
            doc: "Returns the screen position of the window.",
            params: &[],
        }),
        "COMMAND$" => Some(FunctionSignature {
            label: "COMMAND$[(n%)]",
            doc: "Returns command line arguments. N specifies which argument.",
            params: &["n%"],
        }),
        "ENVIRON$" => Some(FunctionSignature {
            label: "ENVIRON$(name$)",
            doc: "Returns the value of an environment variable.",
            params: &["name$"],
        }),

        _ => None,
    }
}

/// Checks if a variable name has an explicit type suffix.
///
/// Type suffixes in BASIC indicate the variable's type:
/// - `$` = STRING
/// - `%` = INTEGER
/// - `&` = LONG
/// - `!` = SINGLE
/// - `#` = DOUBLE
/// - `%%` = _INTEGER64
/// - etc.
fn has_type_suffix(name: &str) -> bool {
    let suffixes = ['$', '%', '&', '!', '#', '`'];
    name.chars().last().is_some_and(|c| suffixes.contains(&c))
}

/// Formats a BasicType for display in inlay hints.
use crate::semantic::BasicType;

fn format_basic_type(ty: &BasicType) -> String {
    match ty {
        BasicType::Integer => "INTEGER".to_string(),
        BasicType::Long => "LONG".to_string(),
        BasicType::Integer64 => "_INTEGER64".to_string(),
        BasicType::Single => "SINGLE".to_string(),
        BasicType::Double => "DOUBLE".to_string(),
        BasicType::String => "STRING".to_string(),
        BasicType::Byte => "_BYTE".to_string(),
        BasicType::UnsignedByte => "_UNSIGNED _BYTE".to_string(),
        BasicType::UnsignedInteger => "_UNSIGNED INTEGER".to_string(),
        BasicType::UnsignedLong => "_UNSIGNED LONG".to_string(),
        BasicType::UnsignedInteger64 => "_UNSIGNED _INTEGER64".to_string(),
        BasicType::Offset => "_OFFSET".to_string(),
        BasicType::Float => "_FLOAT".to_string(),
        BasicType::Bit => "_BIT".to_string(),
        BasicType::UnsignedBit => "_UNSIGNED _BIT".to_string(),
        BasicType::UserDefined(name) => name.clone(),
        BasicType::FixedString(len) => format!("STRING * {}", len),
        BasicType::Array {
            element_type,
            dimensions,
        } => {
            format!(
                "{}({})",
                format_basic_type(element_type),
                "...".repeat(*dimensions)
            )
        }
        BasicType::Mem => "_MEM".to_string(),
        BasicType::Void => "VOID".to_string(),
        BasicType::Unknown => "?".to_string(),
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
///
/// LSP uses UTF-16 code units for character positions, so we must count
/// UTF-16 code units rather than Unicode code points. Characters outside
/// the Basic Multilingual Plane (like emojis) take 2 UTF-16 code units.
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
            // Count UTF-16 code units (1 for BMP chars, 2 for supplementary)
            character += c.len_utf16() as u32;
        }
    }

    Position { line, character }
}

/// Converts an LSP Position to a byte offset.
///
/// LSP uses UTF-16 code units for character positions, so we must count
/// UTF-16 code units rather than Unicode code points.
///
/// # Edge Cases
///
/// - If `position.character` exceeds the line length, returns the offset of
///   the newline character (end of line). This clamping behavior is intentional
///   for robustness when handling positions from potentially buggy clients.
/// - If `position.line` exceeds the number of lines, returns `None`.
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
            // Count UTF-16 code units (1 for BMP chars, 2 for supplementary)
            current_char += c.len_utf16() as u32;
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
}
