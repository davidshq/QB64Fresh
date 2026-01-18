//! C code generation backend for QB64Fresh.
//!
//! This module generates portable C99 code from the typed intermediate representation.
//! The generated code uses `<stdint.h>` for predictable type sizes and a minimal
//! runtime library for I/O operations.
//!
//! # Module Structure
//!
//! The C backend is organized into focused sub-modules:
//!
//! - [`types`] - BASIC-to-C type mapping utilities
//! - [`runtime`] - Inline runtime library code generation
//! - [`expr`] - Expression emission
//! - [`stmt`] - Statement emission
//! - [`analysis`] - Program analysis (globals, DATA collection)
//!
//! # Generated Code Structure
//!
//! ```c
//! // Includes and runtime declarations
//! #include <stdio.h>
//! #include <stdint.h>
//! #include "qb_runtime.h"
//!
//! // Global variables
//! int32_t my_global;
//!
//! // Forward declarations for SUBs/FUNCTIONs
//! void sub_mysub(void);
//!
//! // SUB/FUNCTION definitions
//! void sub_mysub(void) { ... }
//!
//! // Main program
//! int main(int argc, char** argv) {
//!     // Main program statements
//!     return 0;
//! }
//! ```
//!
//! # Type Mapping
//!
//! | BASIC Type    | C Type        |
//! |---------------|---------------|
//! | INTEGER       | int16_t       |
//! | LONG          | int32_t       |
//! | _INTEGER64    | int64_t       |
//! | SINGLE        | float         |
//! | DOUBLE        | double        |
//! | STRING        | qb_string*    |

mod analysis;
mod expr;
mod runtime;
mod stmt;
mod types;

use std::fmt::Write;

use crate::codegen::error::CodeGenError;
use crate::codegen::{CodeGenerator, GeneratedOutput};
use crate::semantic::typed_ir::{TypedProgram, TypedStatementKind};

use self::analysis::collect_data_values;
use self::runtime::emit_header;
use self::stmt::{StmtEmitter, emit_params};

/// Runtime mode for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RuntimeMode {
    /// Inline runtime code in generated output (no external dependencies).
    #[default]
    Inline,
    /// Use external runtime library (requires libqb64fresh_rt.a).
    External,
}

/// C code generation backend.
///
/// Produces portable C99 code that compiles with gcc, clang, or MSVC.
///
/// # Example
///
/// ```ignore
/// use qb64fresh::codegen::{CodeGenerator, CBackend};
///
/// let backend = CBackend::new();
/// let output = backend.generate(&typed_program)?;
/// println!("{}", output.code);
/// ```
pub struct CBackend {
    /// Runtime mode.
    runtime_mode: RuntimeMode,
}

impl Default for CBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl CBackend {
    /// Creates a new C backend with inline runtime.
    pub fn new() -> Self {
        Self {
            runtime_mode: RuntimeMode::Inline,
        }
    }

    /// Creates a new C backend with the specified runtime mode.
    pub fn with_runtime_mode(runtime_mode: RuntimeMode) -> Self {
        Self { runtime_mode }
    }
}

impl CodeGenerator for CBackend {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError> {
        let mut emitter = StmtEmitter::new();
        let mut output = String::new();

        // Header
        emit_header(&mut output, self.runtime_mode);

        // Collect globals and forward declarations
        let (globals, forward_decls) = analysis::collect_globals(program, emit_params);

        // Global variables
        if !globals.is_empty() {
            writeln!(output, "/* Global Variables */").unwrap();
            for decl in globals {
                writeln!(output, "{}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Forward declarations
        if !forward_decls.is_empty() {
            writeln!(output, "/* Forward Declarations */").unwrap();
            for decl in forward_decls {
                writeln!(output, "{}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Collect and emit DATA pool
        let data_pool = collect_data_values(program);
        // Store label indices for RESTORE statement emission
        emitter.data_label_indices = data_pool.label_indices;

        if !data_pool.values.is_empty() {
            writeln!(output, "/* DATA Pool */").unwrap();
            writeln!(output, "typedef struct {{ char type; union {{ double n; const char* s; }} v; }} _qb_data_item;").unwrap();
            write!(output, "static _qb_data_item _qb_data[] = {{").unwrap();
            for (i, (val, type_tag)) in data_pool.values.iter().enumerate() {
                if i > 0 {
                    write!(output, ",").unwrap();
                }
                if *type_tag == "d" {
                    write!(output, " {{'d', {{.n = {}}}}}", val).unwrap();
                } else {
                    write!(output, " {{'s', {{.s = {}}}}}", val).unwrap();
                }
            }
            writeln!(output, " }};").unwrap();
            writeln!(output, "static int _qb_data_ptr = 0;").unwrap();
            writeln!(
                output,
                "static const int _qb_data_count = {};",
                data_pool.values.len()
            )
            .unwrap();
            writeln!(output).unwrap();
        }

        // SUB/FUNCTION definitions (emit before main)
        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. } => {
                    emitter.emit_stmt(stmt, &mut output)?;
                }
                _ => {}
            }
        }

        // Main function
        writeln!(output, "int main(int argc, char** argv) {{").unwrap();
        writeln!(output, "    (void)argc; (void)argv;").unwrap();
        writeln!(output).unwrap();

        emitter.indent = 1;

        // Emit main program statements (excluding SUB/FUNCTION definitions)
        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. } => {
                    // Already emitted above
                }
                _ => {
                    emitter.emit_stmt(stmt, &mut output)?;
                }
            }
        }

        writeln!(output).unwrap();
        writeln!(output, "    return 0;").unwrap();
        writeln!(output, "}}").unwrap();

        Ok(GeneratedOutput::c_code(output))
    }

    fn backend_name(&self) -> &str {
        "C"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::typed_ir::{
        TypedExpr, TypedPrintItem, TypedStatement, TypedStatementKind,
    };
    use crate::semantic::types::BasicType;

    #[test]
    fn test_generate_empty_program() {
        let program = TypedProgram::new(vec![]);
        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("int main("));
        assert!(result.code.contains("return 0;"));
    }

    #[test]
    fn test_generate_print_statement() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Print {
                items: vec![TypedPrintItem {
                    expr: TypedExpr::string("Hello".to_string(), Span::new(6, 13)),
                    separator: None,
                }],
                newline: true,
            },
            Span::new(0, 13),
        )]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("qb_print_string"));
        assert!(result.code.contains("qb_print_newline"));
    }

    #[test]
    fn test_generate_assignment() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "x".to_string(),
                value: TypedExpr::integer(42, Span::new(4, 6)),
                target_type: BasicType::Long,
            },
            Span::new(0, 6),
        )]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("x = 42LL"));
    }

    #[test]
    fn test_restore_with_label_codegen() {
        let program = TypedProgram::new(vec![
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "testLabel".to_string(),
                },
                Span::new(0, 10),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![
                        crate::semantic::typed_ir::TypedDataValue::Integer(1),
                        crate::semantic::typed_ir::TypedDataValue::Integer(2),
                    ],
                },
                Span::new(11, 20),
            ),
            TypedStatement::new(
                TypedStatementKind::Restore {
                    label: Some("testLabel".to_string()),
                },
                Span::new(21, 36),
            ),
        ]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(
            result
                .code
                .contains("_qb_data_ptr = 0; /* RESTORE testLabel */")
        );
    }
}
