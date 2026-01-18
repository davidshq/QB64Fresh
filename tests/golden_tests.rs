//! Golden tests for QB64Fresh compiler
//!
//! These tests compare the compiler's output against pre-approved "golden" files.
//! This approach catches unintended changes to generated code while allowing
//! intentional updates through the UPDATE_GOLDEN environment variable.
//!
//! ## How it works
//!
//! 1. Each test has a `.bas` source file and a corresponding `.golden` expected output
//! 2. The test compiles the source and compares against the golden file
//! 3. If they differ, the test fails showing the diff
//!
//! ## Updating golden files
//!
//! When making intentional changes to code generation:
//! ```bash
//! UPDATE_GOLDEN=1 cargo test golden
//! ```
//! This will update the `.golden` files to match current output.

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

/// Get the path to the golden test fixtures directory
fn golden_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden")
}

/// Compile source code to C
fn compile_to_c(source: &str) -> Result<String, String> {
    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|e| format!("Semantic error: {:?}", e))?;

    let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {}", e))?;

    Ok(output.code)
}

/// Run a golden test comparing generated output against expected
///
/// # Arguments
/// * `test_name` - Name of the test (without extension)
///
/// # Files expected
/// * `tests/golden/{test_name}.bas` - Source code
/// * `tests/golden/{test_name}.golden` - Expected output
fn run_golden_test(test_name: &str) {
    let base_path = golden_dir();
    let source_path = base_path.join(format!("{}.bas", test_name));
    let golden_path = base_path.join(format!("{}.golden", test_name));

    // Read source file
    let source = match fs::read_to_string(&source_path) {
        Ok(s) => s,
        Err(e) => {
            panic!(
                "Could not read source file '{}': {}\nHint: Create the file at {}",
                test_name,
                e,
                source_path.display()
            );
        }
    };

    // Compile to C
    let actual = match compile_to_c(&source) {
        Ok(code) => code,
        Err(e) => {
            panic!("Compilation failed for '{}': {}", test_name, e);
        }
    };

    // Check if we should update golden files
    let should_update = env::var("UPDATE_GOLDEN").is_ok();

    if should_update {
        // Update mode: write actual output as new golden file
        fs::write(&golden_path, &actual).unwrap_or_else(|e| {
            panic!(
                "Failed to write golden file '{}': {}",
                golden_path.display(),
                e
            )
        });
        println!("Updated golden file: {}", golden_path.display());
        return;
    }

    // Normal mode: compare against golden file
    let expected = match fs::read_to_string(&golden_path) {
        Ok(s) => s,
        Err(e) => {
            panic!(
                "Could not read golden file '{}': {}\nHint: Run with UPDATE_GOLDEN=1 to create it:\n  UPDATE_GOLDEN=1 cargo test golden::{}",
                golden_path.display(),
                e,
                test_name
            );
        }
    };

    if actual != expected {
        // Generate a helpful diff
        let diff = generate_diff(&expected, &actual);
        panic!(
            "Golden test '{}' failed!\n\n\
             To update the golden file, run:\n  \
             UPDATE_GOLDEN=1 cargo test golden::{}\n\n\
             Diff (expected vs actual):\n{}",
            test_name, test_name, diff
        );
    }
}

/// Generate a simple line-by-line diff for debugging
fn generate_diff(expected: &str, actual: &str) -> String {
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();

    let mut diff = String::new();
    let max_lines = expected_lines.len().max(actual_lines.len());

    for i in 0..max_lines {
        let exp = expected_lines.get(i).copied().unwrap_or("");
        let act = actual_lines.get(i).copied().unwrap_or("");

        if exp != act {
            diff.push_str(&format!("Line {}:\n", i + 1));
            diff.push_str(&format!("  - {}\n", exp));
            diff.push_str(&format!("  + {}\n", act));
        }
    }

    if diff.is_empty() {
        "No visible differences (possible whitespace/encoding issue)".to_string()
    } else {
        diff
    }
}

/// Run a golden test that expects compilation to produce specific errors
fn run_golden_error_test(test_name: &str) {
    let base_path = golden_dir();
    let source_path = base_path.join(format!("{}.bas", test_name));
    let golden_path = base_path.join(format!("{}.err.golden", test_name));

    // Read source file
    let source = match fs::read_to_string(&source_path) {
        Ok(s) => s,
        Err(e) => {
            panic!(
                "Could not read source file '{}': {}",
                source_path.display(),
                e
            );
        }
    };

    // Compile - expecting error
    let actual_error = match compile_to_c(&source) {
        Ok(_) => {
            panic!(
                "Expected compilation to fail for '{}', but it succeeded",
                test_name
            );
        }
        Err(e) => e,
    };

    // Check if we should update golden files
    let should_update = env::var("UPDATE_GOLDEN").is_ok();

    if should_update {
        fs::write(&golden_path, &actual_error).unwrap_or_else(|e| {
            panic!(
                "Failed to write error golden file '{}': {}",
                golden_path.display(),
                e
            )
        });
        println!("Updated error golden file: {}", golden_path.display());
        return;
    }

    // Normal mode: compare against golden file
    let expected = match fs::read_to_string(&golden_path) {
        Ok(s) => s,
        Err(e) => {
            panic!(
                "Could not read error golden file '{}': {}\nHint: Run with UPDATE_GOLDEN=1 to create it",
                golden_path.display(),
                e
            );
        }
    };

    if actual_error != expected {
        panic!(
            "Error golden test '{}' failed!\n\nExpected error:\n{}\n\nActual error:\n{}\n\n\
             To update, run: UPDATE_GOLDEN=1 cargo test golden::{}",
            test_name, expected, actual_error, test_name
        );
    }
}

// =============================================================================
// Golden Tests
// =============================================================================

// Note: These tests require corresponding .bas and .golden files in tests/golden/
// Create the files and run with UPDATE_GOLDEN=1 to initialize golden files.

#[test]
fn golden_hello_world() {
    run_golden_test("hello_world");
}

#[test]
fn golden_variables() {
    run_golden_test("variables");
}

#[test]
fn golden_arithmetic() {
    run_golden_test("arithmetic");
}

#[test]
fn golden_control_flow() {
    run_golden_test("control_flow");
}

#[test]
fn golden_for_loop() {
    run_golden_test("for_loop");
}

#[test]
fn golden_function() {
    run_golden_test("function");
}

#[test]
fn golden_array() {
    run_golden_test("array");
}

#[test]
fn golden_data_read() {
    run_golden_test("data_read");
}

// =============================================================================
// Error Golden Tests
// =============================================================================

#[test]
fn golden_error_undefined_variable() {
    run_golden_error_test("error_undefined_variable");
}

#[test]
fn golden_error_type_mismatch() {
    run_golden_error_test("error_type_mismatch");
}

// =============================================================================
// Utility Functions for External Use
// =============================================================================

/// List all available golden tests
pub fn list_golden_tests() -> Vec<String> {
    let dir = golden_dir();
    if !dir.exists() {
        return vec![];
    }

    fs::read_dir(&dir)
        .unwrap()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()? == "bas" {
                Some(path.file_stem()?.to_string_lossy().into_owned())
            } else {
                None
            }
        })
        .collect()
}

/// Check if golden files exist for a test
pub fn golden_files_exist(test_name: &str) -> bool {
    let base_path = golden_dir();
    let source_path = base_path.join(format!("{}.bas", test_name));
    let golden_path = base_path.join(format!("{}.golden", test_name));
    source_path.exists() && golden_path.exists()
}
