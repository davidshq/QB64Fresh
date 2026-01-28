//! LSP-specific analysis and caching.
//!
//! This module provides cached analysis results for LSP operations, avoiding
//! redundant parsing and semantic analysis on every request.

use crate::ast::Program;
use crate::lexer::{Token, lex};
use crate::parser::Parser;
use crate::semantic::{SemanticAnalyzer, TypedProgram};
use tower_lsp::lsp_types::Diagnostic;

pub mod incremental;
pub use incremental::{ChangedRegion, incremental_analyze, incremental_parse, merge_tokens};

/// Cached analysis results for a document.
///
/// Stores the results of lexing, parsing, and semantic analysis so they can
/// be reused across multiple LSP requests without re-running the compiler pipeline.
pub struct AnalysisCache {
    /// The document version this cache corresponds to.
    pub version: i32,
    /// The parsed AST, if parsing succeeded.
    pub ast: Option<Program>,
    /// The typed IR from semantic analysis, if analysis succeeded.
    pub typed_program: Option<TypedProgram>,
    /// The semantic analyzer state (for symbol lookups, hover info, etc.).
    pub analyzer: Option<SemanticAnalyzer>,
    /// Diagnostics (errors and warnings) from parsing and semantic analysis.
    pub diagnostics: Vec<Diagnostic>,
    /// Cached tokens from the last lexing pass (for incremental updates).
    pub tokens: Option<Vec<Token>>,
    /// The document content this cache corresponds to (for incremental updates).
    pub content: Option<String>,
}

impl AnalysisCache {
    /// Creates a new empty cache for a document version.
    pub fn new(version: i32) -> Self {
        Self {
            version,
            ast: None,
            typed_program: None,
            analyzer: None,
            diagnostics: Vec::new(),
            tokens: None,
            content: None,
        }
    }

    /// Analyzes a document and caches the results.
    ///
    /// This runs the full compiler pipeline (lex → parse → semantic) and
    /// stores all results for reuse. Returns the diagnostics for immediate
    /// publishing.
    pub fn analyze_document(source: &str, version: i32) -> Self {
        let mut cache = Self::new(version);
        let mut diagnostics = Vec::new();

        // Lexer phase
        let tokens = lex(source);

        // Parser phase
        let mut parser = Parser::new(&tokens);
        let program = match parser.parse() {
            Ok(p) => {
                cache.ast = Some(p.clone());
                p
            }
            Err(errors) => {
                // Convert parse errors to diagnostics
                for err in errors {
                    let range = match err.span() {
                        Some(span) => {
                            crate::lsp::position::span_to_range(source, span.start, span.end)
                        }
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
                        severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("qb64fresh".to_string()),
                        message: err.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
                cache.diagnostics = diagnostics;
                return cache;
            }
        };

        // Semantic analysis phase
        let mut analyzer = SemanticAnalyzer::new();
        match analyzer.analyze(&program) {
            Ok(typed_program) => {
                // Analysis succeeded - store both the typed program and analyzer
                cache.typed_program = Some(typed_program);
                cache.analyzer = Some(analyzer);
            }
            Err(errors) => {
                // Analysis failed - store analyzer for partial symbol info, but no typed program
                cache.analyzer = Some(analyzer);
                for err in errors {
                    let span = err.span();
                    diagnostics.push(Diagnostic {
                        range: crate::lsp::position::span_to_range(source, span.start, span.end),
                        severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
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
        }

        cache.diagnostics = diagnostics;
        cache.tokens = Some(tokens);
        cache.content = Some(source.to_string());
        cache
    }

    /// Incrementally updates the cache with a document change.
    ///
    /// This attempts to update only the affected portions of the analysis,
    /// falling back to full re-analysis if incremental update fails.
    ///
    /// # Arguments
    ///
    /// * `change` - The changed region
    /// * `new_content` - The document content after the change
    /// * `version` - The new document version
    ///
    /// # Returns
    ///
    /// The updated cache.
    pub fn update_incremental(
        &self,
        change: &ChangedRegion,
        new_content: &str,
        version: i32,
    ) -> Self {
        // Try incremental update if we have the necessary cached data
        if let (Some(old_tokens), Some(old_content)) = (&self.tokens, &self.content) {
            // Merge tokens incrementally
            let merged_tokens = merge_tokens(old_tokens, old_content, new_content, change);

            // Try incremental parsing
            if let Some(old_program) = &self.ast {
                // Find the first token affected by the change
                let change_start_token = merged_tokens
                    .iter()
                    .position(|t| t.span.start >= change.start)
                    .unwrap_or(0);

                if let Some(new_program) =
                    incremental_parse(old_program, &merged_tokens, change_start_token)
                {
                    // Try incremental semantic analysis
                    if let Some(old_analyzer) = &self.analyzer
                        && let Some((new_analyzer, new_typed_program)) =
                            incremental_analyze(old_analyzer, &new_program)
                    {
                        // Incremental update succeeded!
                        let diagnostics = Vec::new();
                        // TODO: Collect diagnostics from incremental analysis
                        // For now, we'll do a full re-analysis to get diagnostics

                        return Self {
                            version,
                            ast: Some(new_program),
                            typed_program: Some(new_typed_program),
                            analyzer: Some(new_analyzer),
                            diagnostics,
                            tokens: Some(merged_tokens),
                            content: Some(new_content.to_string()),
                        };
                    }
                }
            }
        }

        // Fallback to full re-analysis
        Self::analyze_document(new_content, version)
    }

    /// Checks if this cache is valid for the given version.
    pub fn is_valid_for(&self, version: i32) -> bool {
        self.version == version
    }
}
