//! QB64pe Compatibility Test Framework
//!
//! This module provides infrastructure for running tests ported from QB64pe
//! to verify compatibility with the original QB64 compiler behavior.
//!
//! ## Test Format (matching QB64pe)
//!
//! Each test consists of files in `tests/fixtures/`:
//!
//! ### Success Tests (expected to compile and run)
//! - `{category}/{name}.bas` - BASIC source code
//! - `{category}/{name}.output` - Expected program output (optional)
//!
//! ### Error Tests (expected to fail compilation)
//! - `{category}/{name}.bas` - BASIC source code
//! - `{category}/{name}.err` - Expected error message substring
//!
//! ## Adding New Tests
//!
//! 1. Create a `.bas` file in the appropriate category directory
//! 2. Add either a `.output` file (for success tests) or `.err` file (for error tests)
//! 3. The test will be automatically discovered and run

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;
use std::fs;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

/// Get the path to the test fixtures directory
fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

/// Monotonic counter for temp file names so parallel test threads don't collide.
static COMPAT_TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Normalize line endings to `\n` so expected (often LF) and actual (e.g. CRLF on Windows) match.
fn normalize_line_endings(s: &str) -> String {
    s.replace("\r\n", "\n").replace('\r', "\n")
}

/// Default timeout for running a compatibility test executable (prevents hung tests).
const RUN_TIMEOUT: Duration = Duration::from_secs(30);

/// Compile generated C code with gcc and run the binary, returning stdout.
///
/// Uses inline runtime (no external libs), so only `-lm` is needed.
/// Requires `gcc` to be available on the system.
/// Run is limited to [`RUN_TIMEOUT`]; a hanging program is killed and reported as an error.
///
/// # Errors
///
/// Returns an error if writing the temp file, running gcc, or running the
/// executable fails, the program exits with a non-zero status, or the run times out.
fn compile_and_run_c(c_code: &str) -> Result<String, String> {
    let temp_dir = std::env::temp_dir();
    let unique_id = std::process::id();
    let counter = COMPAT_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let c_file = temp_dir.join(format!("qb64_compat_{}_{}.c", unique_id, counter));
    let exe_file = temp_dir.join(format!(
        "qb64_compat_{}_{}{}",
        unique_id,
        counter,
        if cfg!(windows) { ".exe" } else { "" }
    ));

    let mut file =
        fs::File::create(&c_file).map_err(|e| format!("Failed to create C file: {}", e))?;
    file.write_all(c_code.as_bytes())
        .map_err(|e| format!("Failed to write C file: {}", e))?;
    drop(file);

    // Use lossy path strings so non-UTF-8 temp paths don't panic
    let exe_str = exe_file.to_string_lossy();
    let c_str = c_file.to_string_lossy();

    let compile_result = Command::new("gcc")
        .args(["-o", exe_str.as_ref(), c_str.as_ref(), "-lm"])
        .output()
        .map_err(|e| format!("Failed to run gcc: {}", e))?;

    // Always remove C file (we wrote it); on compile failure exe won't exist
    let _ = fs::remove_file(&c_file);

    if !compile_result.status.success() {
        let stderr = String::from_utf8_lossy(&compile_result.stderr);
        return Err(format!("C compilation failed: {}", stderr));
    }

    // Run with timeout: spawn, poll try_wait(), kill if over RUN_TIMEOUT
    let mut child = Command::new(&exe_file)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to run executable: {}", e))?;

    let start = Instant::now();
    loop {
        match child
            .try_wait()
            .map_err(|e| format!("Failed to wait for process: {}", e))?
        {
            Some(status) => {
                let mut stdout = String::new();
                if let Some(mut s) = child.stdout.take() {
                    let _ = s.read_to_string(&mut stdout);
                }
                if !status.success() {
                    let mut stderr = String::new();
                    if let Some(mut s) = child.stderr.take() {
                        let _ = s.read_to_string(&mut stderr);
                    }
                    let _ = fs::remove_file(&exe_file);
                    return Err(format!(
                        "Program execution failed (exit code {:?}): {}",
                        status.code(),
                        stderr
                    ));
                }
                let _ = fs::remove_file(&exe_file);
                return Ok(stdout);
            }
            None => {
                if start.elapsed() >= RUN_TIMEOUT {
                    let _ = child.kill();
                    let _ = fs::remove_file(&exe_file);
                    return Err(format!(
                        "Program run timed out after {:?} (killed)",
                        RUN_TIMEOUT
                    ));
                }
                std::thread::sleep(Duration::from_millis(50));
            }
        }
    }
}

/// Result of compiling a BASIC source file
#[derive(Debug)]
pub enum CompileResult {
    /// Compilation succeeded, contains generated C code
    Success(String),
    /// Compilation failed, contains error message
    Error(String),
}

/// Compile BASIC source code
pub fn compile(source: &str) -> CompileResult {
    // Lexer phase
    let tokens = lex(source);

    // Parser phase
    let mut parser = Parser::new(&tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(errors) => {
            let msg = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            return CompileResult::Error(format!("Parse error:\n{}", msg));
        }
    };

    // Semantic analysis phase
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = match analyzer.analyze(&program) {
        Ok(tp) => tp,
        Err(errors) => {
            let msg = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            return CompileResult::Error(format!("Semantic error:\n{}", msg));
        }
    };

    // Code generation phase
    let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
    match backend.generate(&typed_program) {
        Ok(output) => CompileResult::Success(output.code),
        Err(e) => CompileResult::Error(format!("CodeGen error: {:?}", e)),
    }
}

/// A single compatibility test case
#[derive(Debug, Clone)]
pub struct TestCase {
    /// Test category (subdirectory name)
    pub category: String,
    /// Test name (file stem)
    pub name: String,
    /// Full path to the .bas file
    pub source_path: PathBuf,
    /// Type of test
    pub test_type: TestType,
}

/// Type of compatibility test
#[derive(Debug, Clone)]
pub enum TestType {
    /// Test expects successful compilation
    /// If output_path is Some, also compare program output
    Success { output_path: Option<PathBuf> },
    /// Test expects compilation to fail with error containing the string in err_path
    Error { err_path: PathBuf },
}

impl TestCase {
    /// Run this test case
    pub fn run(&self) -> Result<(), String> {
        // Read source file
        let source = fs::read_to_string(&self.source_path)
            .map_err(|e| format!("Failed to read source: {}", e))?;

        // Compile
        let result = compile(&source);

        match &self.test_type {
            TestType::Success { output_path } => {
                // Test expects successful compilation
                match result {
                    CompileResult::Success(code) => {
                        // Verify we got non-empty code
                        if code.is_empty() {
                            return Err("Generated empty C code".to_string());
                        }
                        // If there's an expected output file, compile and run the C code
                        // and compare stdout to the expected output.
                        if let Some(expected_path) = output_path {
                            let expected = fs::read_to_string(expected_path)
                                .map_err(|e| format!("Failed to read expected output: {}", e))?;
                            let actual = compile_and_run_c(&code)
                                .map_err(|e| format!("Run failed: {}", e))?;
                            // Normalize line endings so CRLF vs LF doesn't cause false failures
                            let expected_trim = normalize_line_endings(expected.trim());
                            let actual_trim = normalize_line_endings(actual.trim());
                            if actual_trim != expected_trim {
                                return Err(format!(
                                    "Output mismatch.\nExpected:\n{}\n\nActual:\n{}",
                                    expected_trim, actual_trim
                                ));
                            }
                        }
                        Ok(())
                    }
                    CompileResult::Error(e) => {
                        Err(format!("Expected compilation to succeed, but got: {}", e))
                    }
                }
            }
            TestType::Error { err_path } => {
                // Test expects compilation to fail
                match result {
                    CompileResult::Success(_) => {
                        Err("Expected compilation to fail, but it succeeded".to_string())
                    }
                    CompileResult::Error(actual_error) => {
                        // Read expected error substring
                        let expected = fs::read_to_string(err_path)
                            .map_err(|e| format!("Failed to read expected error: {}", e))?;
                        let expected = expected.trim();

                        // Check if actual error contains expected substring
                        if actual_error.contains(expected) {
                            Ok(())
                        } else {
                            Err(format!(
                                "Error message mismatch.\nExpected to contain: {}\nActual: {}",
                                expected, actual_error
                            ))
                        }
                    }
                }
            }
        }
    }

    /// Get a unique test identifier
    pub fn id(&self) -> String {
        format!("{}_{}", self.category, self.name)
    }
}

/// Discover all test cases in the fixtures directory
pub fn discover_tests() -> Vec<TestCase> {
    let fixtures = fixtures_dir();
    if !fixtures.exists() {
        return vec![];
    }

    let mut tests = Vec::new();

    // Scan success tests
    let success_dir = fixtures.join("success");
    if success_dir.exists() {
        tests.extend(discover_tests_in_dir(&success_dir, "success", true));
    }

    // Scan error tests
    let error_dir = fixtures.join("error");
    if error_dir.exists() {
        tests.extend(discover_tests_in_dir(&error_dir, "error", false));
    }

    // Scan any category directories
    if let Ok(entries) = fs::read_dir(&fixtures) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().unwrap().to_string_lossy().to_string();
                if name != "success" && name != "error" {
                    // This is a category directory, scan for tests
                    tests.extend(discover_tests_in_category(&path, &name));
                }
            }
        }
    }

    tests
}

/// Discover tests in a directory (success or error)
fn discover_tests_in_dir(dir: &Path, category: &str, expect_success: bool) -> Vec<TestCase> {
    let mut tests = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "bas") {
                let name = path.file_stem().unwrap().to_string_lossy().to_string();

                let test_type = if expect_success {
                    let output_path = dir.join(format!("{}.output", name));
                    TestType::Success {
                        output_path: if output_path.exists() {
                            Some(output_path)
                        } else {
                            None
                        },
                    }
                } else {
                    let err_path = dir.join(format!("{}.err", name));
                    if !err_path.exists() {
                        // Skip test without .err file
                        continue;
                    }
                    TestType::Error { err_path }
                };

                tests.push(TestCase {
                    category: category.to_string(),
                    name,
                    source_path: path,
                    test_type,
                });
            }
        }
    }

    tests
}

/// Discover tests in a category directory (auto-detect success/error by file presence)
fn discover_tests_in_category(dir: &Path, category: &str) -> Vec<TestCase> {
    let mut tests = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "bas") {
                let name = path.file_stem().unwrap().to_string_lossy().to_string();

                // Check what kind of test this is
                let err_path = dir.join(format!("{}.err", name));
                let output_path = dir.join(format!("{}.output", name));

                let test_type = if err_path.exists() {
                    // Error test
                    TestType::Error { err_path }
                } else {
                    // Success test
                    TestType::Success {
                        output_path: if output_path.exists() {
                            Some(output_path)
                        } else {
                            None
                        },
                    }
                };

                tests.push(TestCase {
                    category: category.to_string(),
                    name,
                    source_path: path,
                    test_type,
                });
            }
        }
    }

    tests
}

// =============================================================================
// Auto-generated Compatibility Tests
// =============================================================================

/// Run all discovered compatibility tests
#[test]
fn run_compatibility_tests() {
    let tests = discover_tests();

    if tests.is_empty() {
        println!("No compatibility tests found in tests/fixtures/");
        println!("To add tests:");
        println!("  1. Create tests/fixtures/success/*.bas for success tests");
        println!("  2. Create tests/fixtures/error/*.bas with matching *.err for error tests");
        return;
    }

    let mut failures = Vec::new();

    for test in &tests {
        print!("Running {}... ", test.id());
        match test.run() {
            Ok(()) => println!("OK"),
            Err(e) => {
                println!("FAILED");
                failures.push((test.id(), e));
            }
        }
    }

    if !failures.is_empty() {
        let mut msg = format!("\n{} of {} tests failed:\n", failures.len(), tests.len());
        for (id, error) in &failures {
            msg.push_str(&format!("\n=== {} ===\n{}\n", id, error));
        }
        panic!("{}", msg);
    }

    println!("\nAll {} compatibility tests passed!", tests.len());
}

// =============================================================================
// Specific Category Tests (for better cargo test filtering)
// =============================================================================

mod success_tests {
    use super::*;

    #[test]
    fn all_success_tests() {
        let fixtures = fixtures_dir();
        let success_dir = fixtures.join("success");
        if !success_dir.exists() {
            println!("No success test directory at {:?}", success_dir);
            return;
        }

        let tests = discover_tests_in_dir(&success_dir, "success", true);
        run_test_batch(&tests, "success");
    }
}

mod error_tests {
    use super::*;

    #[test]
    fn all_error_tests() {
        let fixtures = fixtures_dir();
        let error_dir = fixtures.join("error");
        if !error_dir.exists() {
            println!("No error test directory at {:?}", error_dir);
            return;
        }

        let tests = discover_tests_in_dir(&error_dir, "error", false);
        run_test_batch(&tests, "error");
    }
}

/// Helper to run a batch of tests with nice output
fn run_test_batch(tests: &[TestCase], category: &str) {
    if tests.is_empty() {
        println!("No {} tests found", category);
        return;
    }

    let mut passed = 0;
    let mut failed = Vec::new();

    for test in tests {
        match test.run() {
            Ok(()) => passed += 1,
            Err(e) => failed.push((test.name.clone(), e)),
        }
    }

    if !failed.is_empty() {
        let mut msg = format!(
            "\n{} {} tests failed (of {}):\n",
            failed.len(),
            category,
            tests.len()
        );
        for (name, error) in &failed {
            msg.push_str(&format!("\n=== {} ===\n{}\n", name, error));
        }
        panic!("{}", msg);
    }

    println!(
        "All {} {} tests passed!",
        passed,
        if passed == 1 { "test" } else { "tests" }
    );
}

// =============================================================================
// Test Utilities for External Use
// =============================================================================

/// Get statistics about discovered tests
pub fn test_stats() -> (usize, usize, usize) {
    let tests = discover_tests();
    let success_count = tests
        .iter()
        .filter(|t| matches!(t.test_type, TestType::Success { .. }))
        .count();
    let error_count = tests
        .iter()
        .filter(|t| matches!(t.test_type, TestType::Error { .. }))
        .count();
    (tests.len(), success_count, error_count)
}

/// Print a summary of discovered tests
pub fn print_test_summary() {
    let tests = discover_tests();
    println!("\nCompatibility Test Summary");
    println!("==========================");

    if tests.is_empty() {
        println!("No tests found in tests/fixtures/");
        return;
    }

    // Group by category
    let mut by_category: std::collections::HashMap<String, Vec<&TestCase>> =
        std::collections::HashMap::new();
    for test in &tests {
        by_category
            .entry(test.category.clone())
            .or_default()
            .push(test);
    }

    for (category, tests) in by_category {
        let success = tests
            .iter()
            .filter(|t| matches!(t.test_type, TestType::Success { .. }))
            .count();
        let error = tests.len() - success;
        println!(
            "\n{}: {} tests ({} success, {} error)",
            category,
            tests.len(),
            success,
            error
        );
        for test in tests {
            let type_str = match &test.test_type {
                TestType::Success { output_path } => {
                    if output_path.is_some() {
                        "success (with output)"
                    } else {
                        "success (compile only)"
                    }
                }
                TestType::Error { .. } => "error",
            };
            println!("  - {} [{}]", test.name, type_str);
        }
    }

    println!("\nTotal: {} tests", tests.len());
}
