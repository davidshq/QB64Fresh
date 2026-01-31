//! QB4.5 Compatibility Test Runner
//!
//! This test module attempts to compile QB4.5 test cases from the QB64PE repository
//! and reports which ones pass/fail at each compilation stage.
//!
//! Run with: cargo test --test qb45_compat -- --nocapture
//!
//! Or run a specific test: cargo test --test qb45_compat qb45com -- --nocapture

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use qb64fresh::codegen::{CBackend, CodeGenerator};
use qb64fresh::lexer::{TokenKind, lex};
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;

/// Result of attempting to compile a BASIC file
#[derive(Debug, Clone)]
#[allow(dead_code)] // String fields used for Debug output
enum CompileResult {
    /// Successfully generated C code
    Success,
    /// Failed during lexing
    LexError(String),
    /// Failed during parsing
    ParseError(String),
    /// Failed during semantic analysis
    SemanticError(String),
    /// Failed during code generation
    CodegenError(String),
}

impl CompileResult {
    fn is_success(&self) -> bool {
        matches!(self, CompileResult::Success)
    }

    fn stage(&self) -> &'static str {
        match self {
            CompileResult::Success => "success",
            CompileResult::LexError(_) => "lexer",
            CompileResult::ParseError(_) => "parser",
            CompileResult::SemanticError(_) => "semantic",
            CompileResult::CodegenError(_) => "codegen",
        }
    }

    fn error_msg(&self) -> Option<&str> {
        match self {
            CompileResult::Success => None,
            CompileResult::LexError(s) => Some(s),
            CompileResult::ParseError(s) => Some(s),
            CompileResult::SemanticError(s) => Some(s),
            CompileResult::CodegenError(s) => Some(s),
        }
    }
}

/// Get line and column from byte offset in source
fn get_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Attempt to compile a BASIC source file through all stages
fn try_compile(source: &str) -> CompileResult {
    // Stage 1: Lexing
    // The lexer doesn't fail - it produces Error tokens for invalid input
    // Note: We don't reject Error tokens at this stage because the parser
    // may successfully consume them (e.g., extended ASCII in DATA statements)
    let tokens = lex(source);

    // Stage 2: Parsing
    let mut parser = Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(a) => a,
        Err(errors) => {
            // Format first error with line info
            if let Some(first_err) = errors.first()
                && let Some(span) = first_err.span()
            {
                let (line, col) = get_line_col(source, span.start);
                return CompileResult::ParseError(format!("line {}:{}: {}", line, col, first_err));
            }
            return CompileResult::ParseError(format!("{:?}", errors));
        }
    };

    // Stage 3: Semantic Analysis
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = match analyzer.analyze(&ast) {
        Ok(tp) => tp,
        Err(e) => return CompileResult::SemanticError(format!("{:?}", e)),
    };

    // Stage 4: Code Generation
    let backend = CBackend::new();
    match backend.generate(&typed_program) {
        Ok(_) => CompileResult::Success,
        Err(e) => CompileResult::CodegenError(format!("{:?}", e)),
    }
}

/// Find all .bas files in a directory recursively
fn find_bas_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(find_bas_files(&path));
            } else if let Some(ext) = path.extension()
                && ext.eq_ignore_ascii_case("bas")
            {
                files.push(path);
            }
        }
    }
    files
}

/// Result of running compatibility tests
#[allow(dead_code)] // passing_files used in verbose mode
struct TestResults {
    passed_count: usize,
    failed_count: usize,
    passing_files: Vec<String>,
    failures_by_stage: HashMap<String, Vec<String>>,
}

/// Run compatibility tests on a directory of BASIC files
fn run_compat_tests(test_dir: &Path, category: &str) -> TestResults {
    let files = find_bas_files(test_dir);
    let mut passed = 0;
    let mut failed = 0;
    let mut passing_files = Vec::new();
    let mut failures_by_stage: HashMap<String, Vec<String>> = HashMap::new();

    for file in &files {
        let relative_path = file
            .strip_prefix(test_dir)
            .unwrap_or(file)
            .display()
            .to_string();

        // Try UTF-8 first, fall back to latin1 for legacy DOS files
        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => {
                // UTF-8 failed, try reading as bytes and converting from latin1
                match fs::read(file) {
                    Ok(bytes) => {
                        // Strip trailing Control-Z (DOS EOF marker) and convert
                        bytes
                            .iter()
                            .take_while(|&&b| b != 0x1A)
                            .map(|&b| b as char)
                            .collect()
                    }
                    Err(e) => {
                        failed += 1;
                        failures_by_stage
                            .entry("io_error".to_string())
                            .or_default()
                            .push(format!("{}: {}", relative_path, e));
                        continue;
                    }
                }
            }
        };

        let result = try_compile(&source);
        if result.is_success() {
            passed += 1;
            passing_files.push(relative_path);
        } else {
            failed += 1;
            let stage = result.stage().to_string();
            // Include error message for verbose output
            let entry = if let Some(msg) = result.error_msg() {
                format!("{}: {}", relative_path, msg)
            } else {
                relative_path
            };
            failures_by_stage.entry(stage).or_default().push(entry);
        }
    }

    println!("\n=== {} ===", category);
    println!("Total: {} files", files.len());
    println!(
        "Passed: {} ({:.1}%)",
        passed,
        100.0 * passed as f64 / files.len().max(1) as f64
    );
    println!(
        "Failed: {} ({:.1}%)",
        failed,
        100.0 * failed as f64 / files.len().max(1) as f64
    );

    if !failures_by_stage.is_empty() {
        println!("\nFailures by stage:");
        for (stage, files) in &failures_by_stage {
            println!("  {}: {} files", stage, files.len());
        }

        // Also print failing file names in verbose mode
        if std::env::var("VERBOSE").is_ok() {
            println!("\nFailing files:");
            for (stage, files) in &failures_by_stage {
                for f in files {
                    println!("  ✗ [{}] {}", stage, f);
                }
            }
        }
    }

    // Print passing files in verbose mode
    if std::env::var("VERBOSE").is_ok() && !passing_files.is_empty() {
        println!("\nPassing files:");
        for f in &passing_files {
            println!("  ✓ {}", f);
        }
    }

    TestResults {
        passed_count: passed,
        failed_count: failed,
        passing_files,
        failures_by_stage,
    }
}

/// Get the QB64PE test cases directory
fn get_qb64pe_testcases_dir() -> Option<PathBuf> {
    // Try relative to workspace root
    let candidates = [
        "../QB64pe/tests/qbasic_testcases",
        "QB64pe/tests/qbasic_testcases",
        "/home/dave/repos/qb64contain/QB64pe/tests/qbasic_testcases",
    ];

    for candidate in candidates {
        let path = PathBuf::from(candidate);
        if path.exists() {
            return Some(path);
        }
    }
    None
}

#[test]
fn qb45com_compatibility() {
    let Some(base_dir) = get_qb64pe_testcases_dir() else {
        println!("QB64PE test cases directory not found, skipping");
        return;
    };

    let qb45_dir = base_dir.join("qb45com");
    if !qb45_dir.exists() {
        println!("qb45com directory not found at {:?}, skipping", qb45_dir);
        return;
    }

    let results = run_compat_tests(&qb45_dir, "QB4.5 Compatibility (qb45com)");

    // Print detailed failure info for debugging
    if std::env::var("VERBOSE").is_ok() {
        for (stage, files) in &results.failures_by_stage {
            println!("\n--- {} failures ---", stage);
            for f in files.iter().take(10) {
                println!("  {}", f);
            }
            if files.len() > 10 {
                println!("  ... and {} more", files.len() - 10);
            }
        }
    }

    // This test tracks progress - we expect some failures initially
    println!("\nNote: Run with VERBOSE=1 to see detailed failure info");
    let total = results.passed_count + results.failed_count;
    println!(
        "Current compatibility: {:.1}%",
        100.0 * results.passed_count as f64 / total.max(1) as f64
    );
}

#[test]
fn misc_compatibility() {
    let Some(base_dir) = get_qb64pe_testcases_dir() else {
        println!("QB64PE test cases directory not found, skipping");
        return;
    };

    let misc_dir = base_dir.join("misc");
    if !misc_dir.exists() {
        println!("misc directory not found, skipping");
        return;
    }

    run_compat_tests(&misc_dir, "Miscellaneous Tests (misc)");
}

#[test]
fn all_testcases_summary() {
    let Some(base_dir) = get_qb64pe_testcases_dir() else {
        println!("QB64PE test cases directory not found, skipping");
        return;
    };

    println!("\n========================================");
    println!("QB64Fresh Compatibility Test Summary");
    println!("========================================\n");

    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut all_failures: HashMap<String, usize> = HashMap::new();

    // Test each subdirectory
    let subdirs = ["qb45com", "misc", "n54", "pete", "thebob"];
    for subdir in subdirs {
        let dir = base_dir.join(subdir);
        if dir.exists() {
            let results = run_compat_tests(&dir, subdir);
            total_passed += results.passed_count;
            total_failed += results.failed_count;
            for (stage, files) in results.failures_by_stage {
                *all_failures.entry(stage).or_default() += files.len();
            }
        }
    }

    println!("\n========================================");
    println!("OVERALL SUMMARY");
    println!("========================================");
    println!("Total files tested: {}", total_passed + total_failed);
    println!(
        "Successfully compiled: {} ({:.1}%)",
        total_passed,
        100.0 * total_passed as f64 / (total_passed + total_failed).max(1) as f64
    );
    println!("Failed to compile: {}", total_failed);

    if !all_failures.is_empty() {
        println!("\nFailure breakdown:");
        let mut stages: Vec<_> = all_failures.iter().collect();
        stages.sort_by(|a, b| b.1.cmp(a.1));
        for (stage, count) in stages {
            println!("  {}: {}", stage, count);
        }
    }
}

/// Test a single file and show detailed error
#[test]
#[ignore] // Run explicitly with --ignored
fn test_single_file() {
    // Change this path to test a specific file
    let file_path =
        "/home/dave/repos/qb64contain/QB64pe/tests/qbasic_testcases/qb45com/action/4pong.bas";

    let source = fs::read_to_string(file_path).expect("Failed to read file");
    let result = try_compile(&source);

    println!("File: {}", file_path);
    println!("Result: {:?}", result);
}

/// Detailed compile attempt that returns full error information
fn try_compile_detailed(source: &str) -> (CompileResult, Vec<String>) {
    let mut details = Vec::new();

    // Stage 1: Lexing
    let tokens = lex(source);
    let lex_errors: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.kind, TokenKind::Error))
        .collect();

    if !lex_errors.is_empty() {
        for err in &lex_errors {
            details.push(format!(
                "Lexer error at position {}: {:?}",
                err.span.start, err.text
            ));
        }
        return (
            CompileResult::LexError(format!("{} error token(s)", lex_errors.len())),
            details,
        );
    }

    // Stage 2: Parsing
    let mut parser = Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(a) => a,
        Err(errors) => {
            for err in &errors {
                details.push(format!("{:?}", err));
            }
            return (
                CompileResult::ParseError(format!("{} error(s)", errors.len())),
                details,
            );
        }
    };

    // Stage 3: Semantic Analysis
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = match analyzer.analyze(&ast) {
        Ok(tp) => tp,
        Err(errors) => {
            for err in &errors {
                details.push(format!("{:?}", err));
            }
            return (
                CompileResult::SemanticError(format!("{} error(s)", errors.len())),
                details,
            );
        }
    };

    // Stage 4: Code Generation
    let backend = CBackend::new();
    match backend.generate(&typed_program) {
        Ok(_) => (CompileResult::Success, details),
        Err(e) => {
            details.push(format!("{:?}", e));
            (CompileResult::CodegenError(format!("{:?}", e)), details)
        }
    }
}

/// Show detailed diagnostics for first N failures in each category
#[test]
fn diagnose_failures() {
    let Some(base_dir) = get_qb64pe_testcases_dir() else {
        println!("QB64PE test cases directory not found, skipping");
        return;
    };

    println!("\n========================================");
    println!("QB64Fresh Failure Diagnostics");
    println!("========================================\n");

    let mut parser_failures: Vec<(String, Vec<String>)> = Vec::new();
    let mut semantic_failures: Vec<(String, Vec<String>)> = Vec::new();

    // Collect failures from qb45com directory
    let qb45_dir = base_dir.join("qb45com");
    if qb45_dir.exists() {
        for file in find_bas_files(&qb45_dir) {
            if let Ok(source) = fs::read_to_string(&file) {
                let (result, details) = try_compile_detailed(&source);
                let relative = file
                    .strip_prefix(&qb45_dir)
                    .unwrap_or(&file)
                    .display()
                    .to_string();

                match result {
                    CompileResult::ParseError(_) if parser_failures.len() < 3 => {
                        parser_failures.push((relative, details));
                    }
                    CompileResult::SemanticError(_) if semantic_failures.len() < 3 => {
                        semantic_failures.push((relative, details));
                    }
                    _ => {}
                }

                // Stop early if we have enough samples
                if parser_failures.len() >= 3 && semantic_failures.len() >= 3 {
                    break;
                }
            }
        }
    }

    // Print parser failures
    if !parser_failures.is_empty() {
        println!(
            "=== PARSER FAILURES (first {}) ===\n",
            parser_failures.len()
        );
        for (file, details) in &parser_failures {
            println!("--- {} ---", file);
            for (i, detail) in details.iter().take(5).enumerate() {
                println!("  {}. {}", i + 1, detail);
            }
            if details.len() > 5 {
                println!("  ... and {} more errors", details.len() - 5);
            }
            println!();
        }
    }

    // Print semantic failures
    if !semantic_failures.is_empty() {
        println!(
            "=== SEMANTIC FAILURES (first {}) ===\n",
            semantic_failures.len()
        );
        for (file, details) in &semantic_failures {
            println!("--- {} ---", file);
            for (i, detail) in details.iter().take(5).enumerate() {
                println!("  {}. {}", i + 1, detail);
            }
            if details.len() > 5 {
                println!("  ... and {} more errors", details.len() - 5);
            }
            println!();
        }
    }
}

/// Categorize semantic errors to identify common patterns
#[test]
fn categorize_semantic_errors() {
    let Some(base_dir) = get_qb64pe_testcases_dir() else {
        println!("QB64PE test cases directory not found, skipping");
        return;
    };

    println!("\n========================================");
    println!("Semantic Error Categories");
    println!("========================================\n");

    let mut error_categories: HashMap<String, usize> = HashMap::new();

    let qb45_dir = base_dir.join("qb45com");
    if qb45_dir.exists() {
        for file in find_bas_files(&qb45_dir) {
            if let Ok(source) = fs::read_to_string(&file) {
                let (result, details) = try_compile_detailed(&source);
                if matches!(result, CompileResult::SemanticError(_)) {
                    for detail in details {
                        // Extract error type from the detail string
                        let category = if detail.contains("type mismatch") {
                            "type mismatch"
                        } else if detail.contains("undefined label") {
                            "undefined label"
                        } else if detail.contains("undefined") {
                            "undefined identifier"
                        } else if detail.contains("SHARED") {
                            "SHARED scope issue"
                        } else if detail.contains("duplicate") {
                            "duplicate definition"
                        } else {
                            "other"
                        };
                        *error_categories.entry(category.to_string()).or_default() += 1;
                    }
                }
            }
        }
    }

    // Sort by count descending
    let mut categories: Vec<_> = error_categories.into_iter().collect();
    categories.sort_by(|a, b| b.1.cmp(&a.1));

    println!("Error category breakdown:");
    for (category, count) in categories {
        println!("  {}: {}", category, count);
    }
}
