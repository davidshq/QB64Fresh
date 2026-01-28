//! LSP-specific analysis and caching.
//!
//! This module provides cached analysis results for LSP operations, avoiding
//! redundant parsing and semantic analysis on every request.

use crate::ast::Program;
use crate::lexer::lex;
use crate::parser::Parser;
use crate::semantic::{SemanticAnalyzer, TypedProgram};
use tower_lsp::lsp_types::Diagnostic;

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
        cache
    }

    /// Checks if this cache is valid for the given version.
    pub fn is_valid_for(&self, version: i32) -> bool {
        self.version == version
    }
}
