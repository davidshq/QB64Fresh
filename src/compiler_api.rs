//! Compiler API facade for QB64Fresh.
//!
//! This module exposes a small, stable surface for tools (formatter, linter,
//! debugger, LSP) to run compiler phases without reaching into internal modules.
//! Use these entry points instead of calling lexer, parser, preprocessor, and
//! semantic directly so that dependency boundaries stay clear and the compiler
//! can evolve internally without breaking consumers.
//!
//! ## Pipeline stages
//!
//! ```text
//! Source → [preprocess] → Lex → Parse → AST → Analyze → Typed IR → CodeGen → C
//! ```
//!
//! ## API surface
//!
//! | Entry point              | Input        | Output        | Use case                    |
//! |--------------------------|-------------|---------------|-----------------------------|
//! | [`parse`]                | source + opts | Program      | Parse only (with optional $INCLUDE) |
//! | [`parse_tokens`]         | tokens      | Program       | Parse from already-lexed tokens      |
//! | [`analyze`]              | Program     | TypedProgram  | Analyze from AST only               |
//! | [`parse_and_analyze`]    | source + opts | TypedProgram | Parse + analyze in one call         |
//! | [`compile`]               | source + opts | GeneratedOutput | Full pipeline (parse → analyze → codegen) |
//!
//! ## Example
//!
//! ```ignore
//! use qb64fresh::compiler_api::{parse, parse_and_analyze, ParseOptions};
//! use std::path::Path;
//!
//! // Parse only (e.g. for formatter or syntax highlighting)
//! let opts = ParseOptions::no_preprocess();
//! let program = parse("PRINT 1 + 2", &opts).expect("parse failed");
//!
//! // Parse + analyze (e.g. for LSP hover, go-to-def, diagnostics)
//! let typed = parse_and_analyze("x = 3 : PRINT x", &opts).expect("analysis failed");
//! ```

use std::path::{Path, PathBuf};

use crate::ast::Program;
use crate::codegen::{CBackend, CodeGenerator, GeneratedOutput, RuntimeMode};
use crate::lexer::{lex, Token};
use crate::parser::{ParseError, Parser};
use crate::preprocessor::{PreprocessorError, preprocess};
use crate::semantic::{SemanticAnalyzer, SemanticError, TypedProgram};

/// Options for the parse stage when parsing from source.
///
/// Controls whether `$INCLUDE` preprocessing is run and where to resolve paths.
#[derive(Clone, Debug, Default)]
pub struct ParseOptions {
    /// If true, run the preprocessor (expand $INCLUDE, join continued lines) before lexing.
    pub preprocess: bool,
    /// Base path for resolving relative paths in $INCLUDE (e.g. directory of the main file).
    pub base_path: Option<PathBuf>,
    /// Path of the source file (for error messages and include context). If set, used as the "from" file for includes.
    pub source_path: Option<PathBuf>,
}

impl ParseOptions {
    /// Options for parsing without preprocessing (e.g. snippets, single buffer).
    ///
    /// Use this when you have a string that does not use $INCLUDE or when
    /// you have already preprocessed the source elsewhere.
    pub fn no_preprocess() -> Self {
        Self {
            preprocess: false,
            base_path: None,
            source_path: None,
        }
    }

    /// Options for parsing with preprocessing (e.g. file-based compilation).
    ///
    /// `base_path` is typically the directory containing the main file;
    /// `source_path` is the path of the file being compiled (for diagnostics and include context).
    pub fn with_preprocess(base_path: impl AsRef<Path>, source_path: Option<impl AsRef<Path>>) -> Self {
        Self {
            preprocess: true,
            base_path: Some(base_path.as_ref().to_path_buf()),
            source_path: source_path.map(|p| p.as_ref().to_path_buf()),
        }
    }
}

/// Error returned when parsing from source fails (preprocessor or parser).
#[derive(Debug)]
pub enum ParseApiError {
    /// Preprocessor error (e.g. include file not found).
    Preprocessor(PreprocessorError),
    /// One or more parse errors.
    Parse(Vec<ParseError>),
}

impl std::fmt::Display for ParseApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseApiError::Preprocessor(e) => e.fmt(f),
            ParseApiError::Parse(errors) => {
                for (i, e) in errors.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    e.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for ParseApiError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseApiError::Preprocessor(e) => Some(e),
            ParseApiError::Parse(_) => None,
        }
    }
}

/// Error returned when parse-and-analyze or compile fails (parse or semantic).
#[derive(Debug)]
pub enum CompileApiError {
    /// Parse stage failed.
    Parse(ParseApiError),
    /// Semantic analysis failed.
    Semantic(Vec<SemanticError>),
    /// Code generation failed.
    Codegen(Vec<crate::codegen::CodeGenError>),
}

impl std::fmt::Display for CompileApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileApiError::Parse(e) => write!(f, "parse error: {}", e),
            CompileApiError::Semantic(errors) => {
                for (i, e) in errors.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    e.fmt(f)?;
                }
                Ok(())
            }
            CompileApiError::Codegen(errors) => {
                for (i, e) in errors.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    e.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CompileApiError {}

impl From<ParseApiError> for CompileApiError {
    fn from(e: ParseApiError) -> Self {
        CompileApiError::Parse(e)
    }
}

/// Parses source into an AST (parse only).
///
/// If `options.preprocess` is true, runs the preprocessor first; `base_path`
/// defaults to the current directory (`.`) if not set. Otherwise lexes and parses
/// the given string as-is.
///
/// # Errors
///
/// Returns `ParseApiError::Preprocessor` if preprocessing is enabled and fails,
/// or `ParseApiError::Parse` if parsing fails.
pub fn parse(source: &str, options: &ParseOptions) -> Result<Program, ParseApiError> {
    let (source_to_parse, _) = if options.preprocess {
        let base = options.base_path.as_deref().unwrap_or(Path::new("."));
        let src_path = options.source_path.as_deref();
        let result = preprocess(source, base, src_path).map_err(ParseApiError::Preprocessor)?;
        (result.source, result.embedded_files)
    } else {
        (source.to_string(), vec![])
    };

    let tokens = lex(&source_to_parse);
    parse_tokens(&tokens).map_err(ParseApiError::Parse)
}

/// Parses a token stream into an AST (parse only, no preprocessing or lexing).
///
/// Use this when you already have tokens (e.g. from a cached lex run or
/// incremental lexer).
///
/// # Errors
///
/// Returns a non-empty list of parse errors if parsing fails.
pub fn parse_tokens(tokens: &[Token]) -> Result<Program, Vec<ParseError>> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

/// Runs semantic analysis on an AST (analyze from AST only).
///
/// Use this when you already have a `Program` (e.g. from [`parse`] or
/// [`parse_tokens`]) and want typed IR for codegen or IDE features.
///
/// # Errors
///
/// Returns a non-empty list of semantic errors if analysis fails.
pub fn analyze(program: &Program) -> Result<TypedProgram, Vec<SemanticError>> {
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(program)
}

/// Parses source and runs semantic analysis (parse + analyze in one call).
///
/// Equivalent to `parse(source, options).and_then(|p| analyze(&p))`, but
/// returns a single error type so tools can handle parse vs semantic failures
/// uniformly.
///
/// # Errors
///
/// Returns `CompileApiError::Parse` or `CompileApiError::Semantic` as appropriate.
pub fn parse_and_analyze(source: &str, options: &ParseOptions) -> Result<TypedProgram, CompileApiError> {
    let program = parse(source, options).map_err(CompileApiError::Parse)?;
    analyze(&program).map_err(CompileApiError::Semantic)
}

/// Options for full compilation (parse → analyze → codegen).
#[derive(Clone, Debug)]
pub struct CompileOptions {
    /// Parse options (preprocessing, paths).
    pub parse: ParseOptions,
    /// Runtime mode for generated C (inline stubs vs external library).
    pub runtime_mode: RuntimeMode,
    /// Embedded files from $EMBED (usually from preprocess when parsing a file).
    pub embedded_files: Vec<crate::preprocessor::EmbeddedFile>,
    /// Source file path for debug info / diagnostics (optional).
    pub source_path: Option<PathBuf>,
    /// Enable OpenGL code emission when the program uses _GL*.
    /// For auto-detection, call [`crate::codegen::program_uses_opengl`] on the typed program and set this accordingly.
    pub opengl: bool,
    /// Disable SHELL/_SHELLHIDE (compile error if used).
    pub no_shell: bool,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            parse: ParseOptions::no_preprocess(),
            runtime_mode: RuntimeMode::external(),
            embedded_files: Vec::new(),
            source_path: None,
            opengl: false,
            no_shell: false,
        }
    }
}

/// Runs the full pipeline: parse → analyze → codegen.
///
/// Use this when you need generated C (and optional resource files) in one call.
/// For tools that only need AST or typed IR, prefer [`parse`], [`analyze`], or
/// [`parse_and_analyze`].
///
/// # Errors
///
/// Returns `CompileApiError::Parse`, `CompileApiError::Semantic`, or
/// `CompileApiError::Codegen` as appropriate.
pub fn compile(source: &str, options: &CompileOptions) -> Result<GeneratedOutput, CompileApiError> {
    let (source_to_parse, embedded_files) = if options.parse.preprocess {
        let base = options.parse.base_path.as_deref().unwrap_or(Path::new("."));
        let src_path = options.parse.source_path.as_deref();
        let result = preprocess(source, base, src_path).map_err(ParseApiError::Preprocessor)?;
        (result.source, result.embedded_files)
    } else {
        (
            source.to_string(),
            options.embedded_files.clone(),
        )
    };

    let tokens = lex(&source_to_parse);
    let program = parse_tokens(&tokens).map_err(ParseApiError::Parse).map_err(CompileApiError::Parse)?;
    let typed = analyze(&program).map_err(CompileApiError::Semantic)?;

    let mut backend = CBackend::with_runtime_mode(options.runtime_mode.clone())
        .with_embedded_files(embedded_files)
        .with_opengl(options.opengl)
        .with_no_shell(options.no_shell);
    // Prefer top-level source_path; fall back to parse.source_path (e.g. when building from a file).
    let source_file = options
        .source_path
        .as_ref()
        .or(options.parse.source_path.as_ref());
    if let Some(p) = source_file {
        backend = backend.with_source_file(p.to_string_lossy().as_ref());
    }

    backend.generate(&typed).map_err(CompileApiError::Codegen)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_only_no_preprocess() {
        let opts = ParseOptions::no_preprocess();
        let program = parse("PRINT 1 + 2", &opts).expect("parse failed");
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_tokens() {
        let tokens = lex("x = 3");
        let program = parse_tokens(&tokens).expect("parse failed");
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_analyze_from_ast() {
        let opts = ParseOptions::no_preprocess();
        let program = parse("DIM n AS INTEGER : n = 5", &opts).expect("parse failed");
        let typed = analyze(&program).expect("analyze failed");
        assert_eq!(typed.statements.len(), 2);
    }

    #[test]
    fn test_parse_and_analyze() {
        let opts = ParseOptions::no_preprocess();
        let typed = parse_and_analyze("PRINT 42", &opts).expect("parse_and_analyze failed");
        assert_eq!(typed.statements.len(), 1);
    }

    #[test]
    fn test_compile_simple() {
        let mut opts = CompileOptions::default();
        opts.parse = ParseOptions::no_preprocess();
        opts.runtime_mode = RuntimeMode::inline();
        let output = compile("PRINT 1", &opts).expect("compile failed");
        assert!(!output.code.is_empty());
        assert!(output.code.contains("main") || output.code.contains("MAIN"));
    }
}
