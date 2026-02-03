//! Debug symbol extraction from the AST.
//!
//! Walks the parsed program to collect procedure names and variable names
//! for the debugger. Line numbers require source mapping (byte offset → line).

use qb64fresh::prelude::*;
use std::path::Path;

/// Debug symbols for a single source file (procedures, globals).
#[derive(Debug, Default)]
pub struct DebugSymbols {
    /// Procedure names and byte offset of definition start (for future line mapping).
    pub procedures: Vec<(String, usize)>,

    /// Global variable names (from DIM at module level).
    pub globals: Vec<String>,
}

/// Extract debug symbols from a parsed program.
///
/// Uses the AST only; line numbers would require mapping span start/end
/// byte offsets to line numbers from the source.
pub fn extract_symbols(source: &str, _path: Option<&Path>) -> crate::DebugResult<DebugSymbols> {
    let tokens = qb64fresh::lexer::lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|_| crate::DebugError::CompileError {
            message: "Parse failed".to_string(),
        })?;

    let mut symbols = DebugSymbols::default();

    for stmt in &program.statements {
        match &stmt.kind {
            StatementKind::SubDefinition { name, .. } => {
                symbols.procedures.push((name.clone(), stmt.span.start));
            }
            StatementKind::FunctionDefinition { name, .. } => {
                symbols.procedures.push((name.clone(), stmt.span.start));
            }
            StatementKind::Dim { variables, .. } => {
                for v in variables {
                    symbols.globals.push(v.name.clone());
                }
            }
            _ => {}
        }
    }

    Ok(symbols)
}
