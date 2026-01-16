//! Code generation module for QB64Fresh.
//!
//! This module transforms the typed intermediate representation (from semantic analysis)
//! into executable code. The architecture uses a trait-based backend system to allow
//! multiple code generation targets.
//!
//! # Architecture
//!
//! ```text
//! TypedProgram → CodeGenerator → GeneratedOutput
//!                     ↑
//!              CBackend (or future: LLVMBackend, CraneliftBackend)
//! ```
//!
//! # Current Backends
//!
//! - **C Backend** (`CBackend`): Generates portable C99 code that compiles with gcc/clang
//!
//! # Example
//!
//! ```ignore
//! use qb64fresh::codegen::{CBackend, CodeGenerator};
//! use qb64fresh::semantic::TypedProgram;
//!
//! let typed_program: TypedProgram = /* from semantic analysis */;
//! let backend = CBackend::new();
//! let output = backend.generate(&typed_program)?;
//!
//! // Write the generated C code to a file
//! std::fs::write("output.c", &output.code)?;
//! ```

mod c_backend;
mod error;

pub use c_backend::{CBackend, RuntimeMode};
pub use error::{CodeGenError, CodeGenErrorKind};

use crate::semantic::TypedProgram;

/// Output from code generation.
///
/// Contains the generated code and any associated metadata needed for
/// subsequent compilation steps.
#[derive(Debug)]
pub struct GeneratedOutput {
    /// The generated source code (e.g., C code for CBackend).
    pub code: String,

    /// Human-readable description of the output format.
    pub format: String,

    /// Suggested file extension for the output.
    pub extension: String,
}

impl GeneratedOutput {
    /// Creates a new generated output for C code.
    pub fn c_code(code: String) -> Self {
        Self {
            code,
            format: "C99".to_string(),
            extension: "c".to_string(),
        }
    }
}

/// Trait for code generation backends.
///
/// This trait defines the interface that all code generation backends must implement.
/// The design follows the Open/Closed principle: adding new backends (LLVM, Cranelift)
/// means adding new implementations, not modifying existing code.
///
/// # Backend Responsibilities
///
/// Each backend is responsible for:
/// - Translating typed IR to target format
/// - Managing target-specific type mappings
/// - Generating any required runtime library calls
/// - Producing well-formatted, readable output (where applicable)
///
/// # Example Implementation
///
/// ```ignore
/// struct MyBackend;
///
/// impl CodeGenerator for MyBackend {
///     fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError> {
///         // Transform program to target format
///         let code = self.emit(program)?;
///         Ok(GeneratedOutput {
///             code,
///             format: "MyFormat".to_string(),
///             extension: "my".to_string(),
///         })
///     }
/// }
/// ```
pub trait CodeGenerator {
    /// Generates code from a typed program.
    ///
    /// # Arguments
    ///
    /// * `program` - The semantically analyzed program with type information
    ///
    /// # Returns
    ///
    /// * `Ok(GeneratedOutput)` - Successfully generated code
    /// * `Err(CodeGenError)` - Code generation failed
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError>;

    /// Returns the name of this backend for diagnostics.
    fn backend_name(&self) -> &str;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::TypedProgram;

    #[test]
    fn test_generated_output_c() {
        let output = GeneratedOutput::c_code("int main() { return 0; }".to_string());
        assert_eq!(output.format, "C99");
        assert_eq!(output.extension, "c");
    }

    #[test]
    fn test_c_backend_name() {
        let backend = CBackend::new();
        assert_eq!(backend.backend_name(), "C");
    }

    #[test]
    fn test_empty_program() {
        let program = TypedProgram::new(vec![]);
        let backend = CBackend::new();
        let result = backend.generate(&program);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.code.contains("int main("));
    }
}
