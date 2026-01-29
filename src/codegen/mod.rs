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

    /// Windows resource files (generated from $EXEICON and $VERSIONINFO).
    ///
    /// Map of filename -> content for resource files that should be written
    /// alongside the generated C code. Keys are filenames like "icon.rc",
    /// "manifest.h", "{basename}.manifest".
    pub resource_files: std::collections::HashMap<String, String>,
}

impl GeneratedOutput {
    /// Creates a new generated output for C code.
    pub fn c_code(code: String) -> Self {
        Self {
            code,
            format: "C99".to_string(),
            extension: "c".to_string(),
            resource_files: std::collections::HashMap::new(),
        }
    }

    /// Adds a resource file to the output.
    pub fn add_resource_file(&mut self, filename: String, content: String) {
        self.resource_files.insert(filename, content);
    }
}

/// Context for collecting code generation errors.
///
/// This context allows code generation to collect multiple errors instead of
/// stopping at the first error, providing better user experience.
pub struct CodeGenContext {
    /// Collected errors during code generation.
    errors: Vec<CodeGenError>,
}

impl CodeGenContext {
    /// Creates a new empty code generation context.
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Pushes an error to the context.
    pub fn push_error(&mut self, error: CodeGenError) {
        self.errors.push(error);
    }

    /// Checks if any errors have been collected.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Gets a reference to the collected errors.
    pub fn errors(&self) -> &[CodeGenError] {
        &self.errors
    }

    /// Consumes the context and returns either the value or the collected errors.
    pub fn into_result<T>(self, value: T) -> Result<T, Vec<CodeGenError>> {
        if self.errors.is_empty() {
            Ok(value)
        } else {
            Err(self.errors)
        }
    }

    /// Consumes the context and returns the errors, or None if there are no errors.
    pub fn into_errors(self) -> Option<Vec<CodeGenError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(self.errors)
        }
    }
}

impl Default for CodeGenContext {
    fn default() -> Self {
        Self::new()
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
/// # Error Handling
///
/// Backends should collect multiple errors during code generation rather than
/// stopping at the first error. This provides better user experience by showing
/// all issues at once.
///
/// # Example Implementation
///
/// ```ignore
/// struct MyBackend;
///
/// impl CodeGenerator for MyBackend {
///     fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, Vec<CodeGenError>> {
///         let mut ctx = CodeGenContext::new();
///         // Transform program to target format, collecting errors
///         let code = self.emit(program, &mut ctx)?;
///         ctx.into_result(GeneratedOutput {
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
    /// * `Err(Vec<CodeGenError>)` - Code generation failed (may contain multiple errors)
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, Vec<CodeGenError>>;

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
        assert!(output.resource_files.is_empty());
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
        let output = result.expect("generating empty program should succeed");
        assert!(output.code.contains("int main("));
    }
}
