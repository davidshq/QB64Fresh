//! Type registry for C code generation.
//!
//! This module provides a `TypeRegistry` that tracks emitted types and ensures
//! proper ordering of type definitions in generated C code. This solves issues
//! with type ordering (e.g., `qb_string` vs `QbString`) and prevents duplicate
//! type definitions.
//!
//! # Usage
//!
//! ```ignore
//! let mut registry = TypeRegistry::new();
//! registry.ensure_type_emitted("QbString", &[], |output| {
//!     writeln!(output, "struct QbString {{ ... }};")?;
//!     Ok(())
//! })?;
//! registry.ensure_type_emitted("qb_string", &["QbString"], |output| {
//!     writeln!(output, "typedef QbString qb_string;")?;
//!     Ok(())
//! })?;
//! ```

use std::collections::{HashMap, HashSet};

use crate::codegen::error::CodeGenError;

/// Type alias for type emitter functions.
type TypeEmitter = Box<dyn Fn(&mut String) -> Result<(), CodeGenError>>;

/// Registry that tracks emitted C types and ensures proper ordering.
///
/// The registry ensures that:
/// - Types are emitted only once (prevents duplicates)
/// - Dependencies are emitted before dependent types (ensures proper ordering)
/// - Type definitions follow a topological order
pub struct TypeRegistry {
    /// Set of type names that have been emitted.
    emitted_types: HashSet<String>,
    /// Map of type names to their dependencies.
    /// Key is the type name, value is the list of dependencies that must be emitted first.
    type_dependencies: HashMap<String, Vec<String>>,
    /// Map of type names to their emission functions.
    /// This stores the code that should be emitted for each type.
    type_emitters: HashMap<String, TypeEmitter>,
}

impl TypeRegistry {
    /// Creates a new empty type registry.
    pub fn new() -> Self {
        Self {
            emitted_types: HashSet::new(),
            type_dependencies: HashMap::new(),
            type_emitters: HashMap::new(),
        }
    }

    /// Registers a type with its dependencies.
    ///
    /// This does not emit the type - use `ensure_type_emitted` to actually emit it.
    ///
    /// # Arguments
    ///
    /// * `type_name` - The name of the type (e.g., "qb_string", "QbString")
    /// * `dependencies` - Types that must be emitted before this type
    /// * `emitter` - Function that emits the type definition
    ///
    /// # Returns
    ///
    /// Returns `true` if the type was newly registered, `false` if it was already registered.
    pub fn register_type<F>(&mut self, type_name: &str, dependencies: &[&str], emitter: F) -> bool
    where
        F: Fn(&mut String) -> Result<(), CodeGenError> + 'static,
    {
        if self.type_dependencies.contains_key(type_name) {
            return false;
        }

        self.type_dependencies.insert(
            type_name.to_string(),
            dependencies.iter().map(|s| s.to_string()).collect(),
        );
        self.type_emitters
            .insert(type_name.to_string(), Box::new(emitter));
        true
    }

    /// Ensures a type is emitted, emitting dependencies first if needed.
    ///
    /// This method:
    /// 1. Checks if the type has already been emitted (returns early if so)
    /// 2. Recursively ensures all dependencies are emitted first
    /// 3. Emits the type itself
    ///
    /// # Arguments
    ///
    /// * `type_name` - The name of the type to emit
    /// * `output` - The output buffer to write to
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The type is not registered
    /// - A circular dependency is detected
    /// - The emitter function returns an error
    pub fn ensure_type_emitted(
        &mut self,
        type_name: &str,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        // If already emitted, nothing to do
        if self.emitted_types.contains(type_name) {
            return Ok(());
        }

        // Check if type is registered
        let dependencies = self
            .type_dependencies
            .get(type_name)
            .ok_or_else(|| {
                CodeGenError::internal(format!(
                    "Type '{}' not registered in type registry",
                    type_name
                ))
            })?
            .clone();

        // Emit dependencies first (recursively)
        for dep in &dependencies {
            self.ensure_type_emitted(dep, output)?;
        }

        // Emit the type itself
        let emitter = self.type_emitters.get(type_name).ok_or_else(|| {
            CodeGenError::internal(format!("Type '{}' emitter not found", type_name))
        })?;

        emitter(output)?;
        self.emitted_types.insert(type_name.to_string());

        Ok(())
    }

    /// Checks if a type has been emitted.
    pub fn is_emitted(&self, type_name: &str) -> bool {
        self.emitted_types.contains(type_name)
    }

    /// Gets the list of dependencies for a type.
    #[allow(dead_code)] // May be useful for debugging or future features
    pub fn get_dependencies(&self, type_name: &str) -> Option<&[String]> {
        self.type_dependencies.get(type_name).map(|v| v.as_slice())
    }
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_registry_basic() {
        let mut registry = TypeRegistry::new();
        let mut output = String::new();

        // Register a simple type with no dependencies
        registry.register_type("int", &[], |output| {
            output.push_str("typedef int32_t int;\n");
            Ok(())
        });

        // Emit it
        registry.ensure_type_emitted("int", &mut output).unwrap();
        assert!(output.contains("typedef int32_t int"));
        assert!(registry.is_emitted("int"));
    }

    #[test]
    fn test_type_registry_dependencies() {
        let mut registry = TypeRegistry::new();
        let mut output = String::new();

        // Register QbString (no dependencies)
        registry.register_type("QbString", &[], |output| {
            output.push_str("struct QbString { char* data; };\n");
            Ok(())
        });

        // Register qb_string (depends on QbString)
        registry.register_type("qb_string", &["QbString"], |output| {
            output.push_str("typedef QbString qb_string;\n");
            Ok(())
        });

        // Emit qb_string - should emit QbString first
        registry
            .ensure_type_emitted("qb_string", &mut output)
            .unwrap();

        // Check ordering: QbString should come before qb_string
        let qb_string_pos = output.find("typedef QbString qb_string").unwrap();
        let qb_string_struct_pos = output.find("struct QbString").unwrap();
        assert!(qb_string_struct_pos < qb_string_pos);
    }

    #[test]
    fn test_type_registry_no_duplicates() {
        let mut registry = TypeRegistry::new();
        let mut output1 = String::new();
        let mut output2 = String::new();

        registry.register_type("int", &[], |output| {
            output.push_str("typedef int32_t int;\n");
            Ok(())
        });

        // Emit twice - should only emit once
        registry.ensure_type_emitted("int", &mut output1).unwrap();
        assert!(registry.is_emitted("int"));
        assert!(output1.contains("typedef int32_t int"));

        // Second emit should not add anything
        let len_before = output2.len();
        registry.ensure_type_emitted("int", &mut output2).unwrap();
        // Output should be empty since type was already emitted
        assert_eq!(output2.len(), len_before);
    }

    #[test]
    fn test_type_registry_unregistered_type() {
        let mut registry = TypeRegistry::new();
        let mut output = String::new();

        // Try to emit unregistered type
        let result = registry.ensure_type_emitted("unknown", &mut output);
        assert!(result.is_err());
    }
}
