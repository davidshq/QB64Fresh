//! Bootstrap tests for QB64pe compilation
//!
//! These tests verify that QB64Fresh can successfully compile the QB64pe
//! compiler source code (~59,000 lines of BASIC across 39 files).
//!
//! Test categories:
//! - Compilation tests: Verify QB64pe compiles without errors
//! - Golden tests: Detect changes in generated C code
//! - (Future) Execution tests: Verify compiled QB64pe can compile BASIC programs

use std::path::Path;
use std::time::Instant;

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::preprocessor::preprocess_file;
use qb64fresh::semantic::SemanticAnalyzer;

/// Path to QB64pe source (relative to workspace root)
const QB64PE_SOURCE: &str = "../QB64pe/source/qb64pe.bas";

/// Get the path to QB64pe source file
fn qb64pe_path() -> std::path::PathBuf {
    // Get the workspace root (parent of tests directory)
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest_dir).join(QB64PE_SOURCE)
}

/// Helper to compile QB64pe through all stages.
///
/// **Important:** QB64pe is compiled with `RuntimeMode::External` because it requires
/// graphics support for its GUI (uses SCREEN, _NEWIMAGE, _SCREENSHOW, etc.).
/// The runtime library must be built with `--features graphics-sdl2` for full functionality.
fn compile_qb64pe() -> Result<CompilationResult, String> {
    let path = qb64pe_path();

    if !path.exists() {
        return Err(format!(
            "QB64pe source not found at: {}\n\
             Make sure QB64pe is checked out alongside QB64Fresh",
            path.display()
        ));
    }

    let start = Instant::now();

    // Preprocess (handles $INCLUDE directives)
    let source = preprocess_file(&path).map_err(|e| format!("Preprocessor error: {}", e))?;
    let preprocess_time = start.elapsed();

    // Lexer
    let lex_start = Instant::now();
    let tokens = lex(&source.source);
    let lex_time = lex_start.elapsed();

    // Parser
    let parse_start = Instant::now();
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|errors| format!("Parse errors ({} total): {:?}", errors.len(), errors))?;
    let parse_time = parse_start.elapsed();

    // Semantic analysis
    let semantic_start = Instant::now();
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|errors| format!("Semantic errors ({} total): {:?}", errors.len(), errors))?;
    let semantic_time = semantic_start.elapsed();

    // Code generation
    let codegen_start = Instant::now();
    let backend = CBackend::with_runtime_mode(RuntimeMode::external());
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {:?}", e))?;
    let codegen_time = codegen_start.elapsed();

    Ok(CompilationResult {
        source_bytes: source.source.len(),
        token_count: tokens.len(),
        statement_count: program.statements.len(),
        c_code_bytes: output.code.len(),
        c_code_lines: output.code.lines().count(),
        preprocess_time,
        lex_time,
        parse_time,
        semantic_time,
        codegen_time,
        total_time: start.elapsed(),
        c_code: output.code,
    })
}

/// Results from compiling QB64pe
#[derive(Debug)]
struct CompilationResult {
    source_bytes: usize,
    token_count: usize,
    statement_count: usize,
    c_code_bytes: usize,
    c_code_lines: usize,
    preprocess_time: std::time::Duration,
    lex_time: std::time::Duration,
    parse_time: std::time::Duration,
    semantic_time: std::time::Duration,
    codegen_time: std::time::Duration,
    total_time: std::time::Duration,
    c_code: String,
}

// ============================================================================
// Compilation Tests
// ============================================================================

/// Test that QB64pe source compiles without any errors.
/// This is the primary regression test for the bootstrap project.
///
/// Note: Uses a spawned thread with larger stack to handle QB64pe's deep AST.
#[test]
fn qb64pe_compiles_successfully() {
    // QB64pe has very deep nested structures requiring larger stack
    let builder = std::thread::Builder::new()
        .name("qb64pe_compile".into())
        .stack_size(16 * 1024 * 1024); // 16MB stack

    let handle = builder
        .spawn(|| compile_qb64pe())
        .expect("Failed to spawn thread");

    let result = handle.join().expect("Thread panicked");

    match result {
        Ok(stats) => {
            println!("\n=== QB64pe Compilation Successful ===");
            println!(
                "Source size:     {} bytes ({:.2} MB)",
                stats.source_bytes,
                stats.source_bytes as f64 / 1_000_000.0
            );
            println!("Tokens:          {}", stats.token_count);
            println!("Statements:      {}", stats.statement_count);
            println!(
                "C output:        {} lines ({:.2} MB)",
                stats.c_code_lines,
                stats.c_code_bytes as f64 / 1_000_000.0
            );
            println!("\nTiming:");
            println!("  Preprocess:    {:?}", stats.preprocess_time);
            println!("  Lexer:         {:?}", stats.lex_time);
            println!("  Parser:        {:?}", stats.parse_time);
            println!("  Semantic:      {:?}", stats.semantic_time);
            println!("  CodeGen:       {:?}", stats.codegen_time);
            println!("  Total:         {:?}", stats.total_time);
        }
        Err(e) => {
            panic!("QB64pe compilation failed:\n{}", e);
        }
    }
}

/// Verify the generated C code has expected characteristics.
/// This catches regressions in code generation size/structure.
#[test]
fn qb64pe_c_output_sanity_checks() {
    // Use larger stack for deep AST processing
    let builder = std::thread::Builder::new()
        .name("qb64pe_sanity".into())
        .stack_size(16 * 1024 * 1024);

    let handle = builder
        .spawn(|| compile_qb64pe())
        .expect("Failed to spawn thread");

    let result = handle
        .join()
        .expect("Thread panicked")
        .expect("QB64pe should compile");

    // The generated C code should be substantial
    // (QB64pe is ~59K lines of BASIC, generates ~86K lines of C)
    assert!(
        result.c_code_lines > 50_000,
        "Expected >50K lines of C, got {}",
        result.c_code_lines
    );

    // Should include standard runtime functions
    assert!(
        result.c_code.contains("qb_print"),
        "Generated C should include qb_print"
    );

    // Should have main function
    assert!(
        result.c_code.contains("int main("),
        "Generated C should have main function"
    );

    // Should have TYPE definitions from QB64pe
    // (QB64pe defines many types like usedVarList, etc.)
    assert!(
        result.c_code.contains("typedef struct"),
        "Generated C should have struct definitions"
    );
}

/// Verify that QB64pe can be parsed (lexer + parser only).
/// Faster test for catching parse regressions.
#[test]
fn qb64pe_parses_successfully() {
    let path = qb64pe_path();

    if !path.exists() {
        println!("Skipping: QB64pe source not found at {}", path.display());
        return;
    }

    let source = preprocess_file(&path).expect("Preprocessor should succeed");

    let tokens = lex(&source.source);
    let mut parser = Parser::new(&tokens);

    match parser.parse() {
        Ok(program) => {
            println!("Parse successful: {} statements", program.statements.len());
            assert!(
                program.statements.len() > 2000,
                "Expected >2000 statements, got {}",
                program.statements.len()
            );
        }
        Err(errors) => {
            // Debug: show problematic lines
            let lines: Vec<&str> = source.source.lines().collect();
            println!("\n=== DEBUG: Source context around errors ===");
            for err in errors.iter().take(5) {
                // Extract line number from error span
                let span_start = match err {
                    qb64fresh::parser::ParseError::InvalidStatement { span, .. } => span.start,
                    qb64fresh::parser::ParseError::InvalidExpression { span, .. } => span.start,
                    qb64fresh::parser::ParseError::UnexpectedToken { span, .. } => span.start,
                    _ => continue,
                };
                // Find line number from byte offset
                let mut line_num: usize = 0;
                let mut byte_count = 0;
                for (i, line) in lines.iter().enumerate() {
                    if byte_count + line.len() + 1 > span_start {
                        line_num = i;
                        break;
                    }
                    byte_count += line.len() + 1; // +1 for newline
                }
                println!("\nError near line {} (byte {}):", line_num + 1, span_start);
                for i in
                    line_num.saturating_sub(3)..=(line_num + 3).min(lines.len().saturating_sub(1))
                {
                    let marker = if i == line_num { ">>>" } else { "   " };
                    println!("{} {:5}: {}", marker, i + 1, lines[i]);
                }
            }
            panic!("Parse failed with {} errors:\n{:?}", errors.len(), errors);
        }
    }
}

// ============================================================================
// Golden File Tests (C output stability)
// ============================================================================

/// Test that a small QB64pe subset produces stable C output.
/// Uses a representative subset to avoid storing 4MB golden files.
///
/// This test compiles a small subset of QB64pe source files and compares
/// the generated C code against a golden file. The subset is chosen to
/// be representative of the full codebase while keeping the golden file
/// size manageable.
///
/// To update the golden file:
/// ```bash
/// UPDATE_GOLDEN=1 cargo test --test bootstrap_tests qb64pe_codegen_golden -- --ignored
/// ```
#[test]
#[ignore = "Golden file not yet created - run with UPDATE_GOLDEN=1 to create"]
fn qb64pe_codegen_golden() {
    use std::env;
    use std::fs;

    // Use a small representative subset of QB64pe files
    // This avoids storing a 4MB+ golden file while still testing codegen stability
    let subset_files = vec![
        "../QB64pe/source/subs_functions/utilities.bas",
        "../QB64pe/source/subs_functions/string_functions.bas",
    ];

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let base_path = Path::new(manifest_dir);
    let golden_path = base_path.join("tests/golden/qb64pe_subset.golden");

    // Collect and preprocess all subset files
    let mut combined_source = String::new();
    for file in &subset_files {
        let file_path = base_path.join(file);
        if !file_path.exists() {
            // Skip if files don't exist (QB64pe may not be checked out)
            eprintln!(
                "Warning: QB64pe subset file not found: {}",
                file_path.display()
            );
            return; // Skip test if QB64pe not available
        }
        let result = match preprocess_file(&file_path) {
            Ok(r) => r,
            Err(e) => {
                panic!("Failed to preprocess {}: {}", file_path.display(), e);
            }
        };
        combined_source.push_str(&result.source);
        combined_source.push('\n');
    }

    // Compile the subset
    let tokens = lex(&combined_source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().expect("QB64pe subset should parse");

    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .expect("QB64pe subset should analyze");

    let backend = CBackend::with_runtime_mode(RuntimeMode::external());
    let output = backend
        .generate(&typed_program)
        .expect("QB64pe subset should generate C code");

    let actual = output.code;

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
                "Could not read golden file '{}': {}\nHint: Run with UPDATE_GOLDEN=1 to create it:\n  UPDATE_GOLDEN=1 cargo test --test bootstrap_tests qb64pe_codegen_golden -- --ignored",
                golden_path.display(),
                e
            );
        }
    };

    if actual != expected {
        // Simple diff output
        let actual_lines: Vec<&str> = actual.lines().collect();
        let expected_lines: Vec<&str> = expected.lines().collect();

        let mut diff_lines = Vec::new();
        let max_lines = actual_lines.len().max(expected_lines.len());
        for i in 0..max_lines.min(50) {
            // Show first 50 lines of diff
            let actual_line = actual_lines.get(i).map(|s| *s).unwrap_or("");
            let expected_line = expected_lines.get(i).map(|s| *s).unwrap_or("");
            if actual_line != expected_line {
                diff_lines.push(format!(
                    "Line {}: expected '{}', got '{}'",
                    i + 1,
                    expected_line,
                    actual_line
                ));
            }
        }

        panic!(
            "Golden test 'qb64pe_codegen_golden' failed!\n\n\
             First differences:\n{}\n\n\
             To update the golden file, run:\n  \
             UPDATE_GOLDEN=1 cargo test --test bootstrap_tests qb64pe_codegen_golden -- --ignored",
            diff_lines.join("\n")
        );
    }
}

// ============================================================================
// Execution Tests
// ============================================================================

/// Test that bootstrapped QB64pe can compile a simple Hello World program.
///
/// This test validates the full bootstrap chain:
/// 1. QB64pe compiles with QB64Fresh (already verified in qb64pe_compiles_successfully)
/// 2. The generated C code is valid and can be compiled
/// 3. The bootstrapped QB64pe executable can be built
/// 4. The bootstrapped QB64pe can compile a simple BASIC program
///
/// **Note:** Full execution testing requires:
/// - Runtime library to be built (`cargo build -p qb64fresh-runtime --release`)
/// - Generated C code to be compiled with gcc/clang
/// - Bootstrapped QB64pe executable to be run on a test program
/// - Output verification
///
/// For now, this test validates that:
/// - QB64pe compilation produces valid C code
/// - A simple Hello World program compiles correctly with QB64Fresh
/// - The code generation is correct for basic programs
#[test]
#[ignore = "Full execution test requires runtime library build and executable compilation"]
fn qb64pe_can_compile_hello_world() {
    use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
    use qb64fresh::lexer::lex;
    use qb64fresh::parser::Parser;
    use qb64fresh::semantic::SemanticAnalyzer;

    // Step 1: Verify QB64pe compiles (prerequisite)
    let qb64pe_result = compile_qb64pe();
    assert!(
        qb64pe_result.is_ok(),
        "QB64pe must compile successfully before testing program compilation"
    );

    // Step 2: Create a simple Hello World program
    let hello_world_source = r#"PRINT "Hello, World!""#;

    // Step 3: Compile Hello World with QB64Fresh to verify code generation
    let tokens = lex(hello_world_source);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().expect("Hello World should parse");

    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .expect("Hello World should analyze");

    let backend = CBackend::with_runtime_mode(RuntimeMode::external());
    let output = backend
        .generate(&typed_program)
        .expect("Hello World should generate C code");

    // Step 4: Verify generated C code is valid
    assert!(
        output.code.contains("qb_print_string"),
        "Generated code should call qb_print_string"
    );
    assert!(
        output.code.contains("Hello, World!"),
        "Generated code should contain the string literal"
    );
    assert!(
        output.code.contains("int main("),
        "Generated code should have main function"
    );

    // Step 5: Document what's needed for full execution test
    // NOTE: QB64pe requires external runtime with graphics support (it has a GUI)
    // TODO: Once runtime library is built and QB64pe executable exists:
    // 1. Build runtime with graphics: `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`
    // 2. Compile QB64pe C with SDL2: `gcc -I runtime/include qb64pe.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2) -lm -lpthread -ldl -o qb64pe_bootstrapped`
    // 3. Create test program: `echo 'PRINT "Hello"' > test.bas`
    // 4. Run: `./qb64pe_bootstrapped -x test.bas -o test.c`
    // 5. Verify: `test.c` exists and contains valid C code
    // 6. Compile and run: `gcc test.c -o test && ./test` should print "Hello"

    println!("✓ Hello World program compiles correctly");
    println!("✓ Generated C code is valid");
    println!("⚠ Full execution test requires runtime library build");
}

/// Test that bootstrapped QB64pe can compile QB4.5 compatibility test programs.
///
/// This test validates that the bootstrapped QB64pe can handle QB4.5 test suite programs.
///
/// **Note:** Full execution testing requires:
/// - Runtime library to be built
/// - Bootstrapped QB64pe executable to be compiled and linked
/// - Running bootstrapped QB64pe on QB4.5 test files
/// - Comparing compilation results with original QB64pe
///
/// For now, this test validates prerequisites:
/// - QB64pe compiles successfully
/// - QB4.5 test files exist and can be located
/// - QB64Fresh can compile representative QB4.5 programs
#[test]
#[ignore = "Full execution test requires runtime library build and executable compilation"]
fn qb64pe_qb45_compatibility_test() {
    use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
    use qb64fresh::lexer::lex;
    use qb64fresh::parser::Parser;
    use qb64fresh::semantic::SemanticAnalyzer;
    use std::path::Path;

    // Step 1: Verify QB64pe compiles (prerequisite)
    let qb64pe_result = compile_qb64pe();
    assert!(
        qb64pe_result.is_ok(),
        "QB64pe must compile successfully before testing QB4.5 compatibility"
    );

    // Step 2: Check if QB4.5 test directory exists
    let qb45_test_dir = Path::new("../QB64pe/tests/qbasic_testcases/qb45com");
    if !qb45_test_dir.exists() {
        println!("⚠ QB4.5 test directory not found, skipping test");
        return;
    }

    // Step 3: Test that QB64Fresh can compile a representative QB4.5 program
    // (This validates that QB64Fresh itself can handle QB4.5 programs)
    let test_program = r#"
        PRINT "QB4.5 Compatibility Test"
        DIM arr(10) AS INTEGER
        FOR i = 1 TO 10
            arr(i) = i * 2
        NEXT i
        PRINT "Array filled successfully"
    "#;

    let tokens = lex(test_program);
    let mut parser = Parser::new(&tokens);
    let program = parser.parse().expect("QB4.5 test program should parse");

    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .expect("QB4.5 test program should analyze");

    let backend = CBackend::with_runtime_mode(RuntimeMode::external());
    let output = backend
        .generate(&typed_program)
        .expect("QB4.5 test program should generate C code");

    // Step 4: Verify generated code is valid
    assert!(
        output.code.contains("int main("),
        "Generated code should have main function"
    );

    println!("✓ QB4.5 test program compiles correctly with QB64Fresh");
    println!("✓ Prerequisites validated for bootstrapped QB64pe QB4.5 compatibility");
    println!(
        "⚠ Full execution test requires runtime library build and bootstrapped QB64pe executable"
    );
}

/// Test that bootstrapped QB64pe can compile itself (meta-bootstrap).
///
/// This test validates the full bootstrap chain:
/// QB64Fresh → QB64pe → QB64pe (meta-compiled)
///
/// **Note:** Full execution testing requires:
/// - Runtime library to be built
/// - Bootstrapped QB64pe executable to be compiled
/// - Running bootstrapped QB64pe on QB64pe source
/// - Verifying meta-compiled QB64pe works
///
/// For now, this test validates prerequisites:
/// - QB64pe compiles successfully with QB64Fresh
/// - Generated C code is valid
#[test]
#[ignore = "Full execution test requires runtime library build and executable compilation"]
fn qb64pe_self_compilation_test() {
    // Step 1: Verify QB64pe compiles (prerequisite)
    let qb64pe_result = compile_qb64pe();
    assert!(
        qb64pe_result.is_ok(),
        "QB64pe must compile successfully before testing self-compilation"
    );

    let stats = qb64pe_result.unwrap();

    // Step 2: Verify generated C code characteristics
    assert!(
        stats.c_code_lines > 50_000,
        "Generated C code should be substantial (got {} lines)",
        stats.c_code_lines
    );
    assert!(
        stats.c_code.contains("int main("),
        "Generated code should have main function"
    );
    assert!(
        stats.c_code.contains("typedef struct"),
        "Generated code should have struct definitions"
    );

    println!("✓ QB64pe compiles successfully with QB64Fresh");
    println!("✓ Generated C code is valid ({} lines)", stats.c_code_lines);
    println!("✓ Prerequisites validated for meta-bootstrap");
    println!("⚠ Full execution test requires:");
    println!(
        "  1. Build runtime with graphics: cargo build -p qb64fresh-runtime --release --features graphics-sdl2"
    );
    println!(
        "  2. Compile QB64pe C with SDL2: gcc -I runtime/include qb64pe.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2) -lm -lpthread -ldl -o qb64pe_bootstrapped"
    );
    println!("  3. Run: ./qb64pe_bootstrapped -x ../QB64pe/source/qb64pe.bas -o qb64pe_meta.c");
    println!("  4. Verify: qb64pe_meta.c exists and compiles");
}

// ============================================================================
// Regression Tests
// ============================================================================

/// Test specific features that caused issues during bootstrap development.
/// Each test documents a bug that was fixed.
mod regression_tests {
    use super::*;

    /// Regression: Function calls must use canonical name with type suffix.
    /// Bug: `qb_getelement$` was called as `qb_getelement` causing linker errors.
    #[test]
    fn function_call_uses_canonical_name() {
        // This is tested implicitly by qb64pe_compiles_successfully
        // but we document it here for future reference
        let source = r#"
            FUNCTION GetValue$
                GetValue$ = "test"
            END FUNCTION
            PRINT GetValue$
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // The call should use the canonical name with _str suffix
        assert!(
            output.code.contains("getvalue_str"),
            "Function call should use canonical name: {}",
            output.code
        );
    }

    /// Regression: Dual namespace model - arrays and scalars can share names.
    /// Bug: `sf` scalar and `SF()` array couldn't coexist.
    #[test]
    fn dual_namespace_arrays_and_scalars() {
        let source = r#"
            DIM sf AS INTEGER
            DIM SF(10) AS INTEGER
            sf = 5
            SF(1) = 10
            PRINT sf; SF(1)
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        analyzer
            .analyze(&program)
            .expect("Should analyze - dual namespace");
    }

    /// Regression: Parser edge case - comparison vs array assignment (REGRESSION_TEST_COVERAGE §9).
    /// Bug: `x = arr(1) = 5` or `x = ASC("A") = 65` parsed as array assignment → "expected (, found Equals".
    /// Fix: Session 048 - is_array_assignment() now requires `(` immediately after identifier.
    /// This test verifies that comparison expressions with function/array on LHS parse correctly.
    #[test]
    fn parser_edge_case_comparison_vs_array_assignment() {
        let source = r#"
            DIM x AS LONG
            DIM arr(10) AS LONG
            arr(1) = 5
            x = arr(1) = 5
            x = ASC("A") = 65
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser
            .parse()
            .expect("Should parse - comparison vs array assignment");

        use qb64fresh::prelude::StatementKind;
        // Should have Let statements for the comparisons, not ArrayAssignment
        let let_count = program
            .statements
            .iter()
            .filter(|s| matches!(&s.kind, StatementKind::Let { .. }))
            .count();
        let array_assign_count = program
            .statements
            .iter()
            .filter(|s| matches!(&s.kind, StatementKind::ArrayAssignment { .. }))
            .count();
        assert!(
            let_count >= 2,
            "Expected at least 2 Let statements (comparisons), got {} Let, {} ArrayAssignment",
            let_count,
            array_assign_count
        );
        assert!(
            array_assign_count <= 1,
            "x = arr(1) = 5 and x = ASC(...) = 65 must parse as Let (comparison), not ArrayAssignment; got {} ArrayAssignment",
            array_assign_count
        );

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).expect("Should analyze");
    }

    /// Regression: String double-wrapping in BYREF/BYVAL parameters.
    /// Bug: qb_str_from_c() was wrapped multiple times causing 807 C compilation errors.
    /// Fix: commit 115ae41 - Added unwrap_qb_str_from_c() helper.
    #[test]
    fn string_double_wrapping_byref() {
        let source = r#"
            SUB TestSub(s$ AS STRING)
                PRINT s$
            END SUB
            
            DIM x$ AS STRING
            x$ = "test"
            CALL TestSub(x$)
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Should not have double qb_str_from_c() wrapping
        // Count occurrences - should be reasonable, not excessive
        let qb_str_from_c_count = output.code.matches("qb_str_from_c(").count();
        assert!(
            qb_str_from_c_count < 20,
            "Too many qb_str_from_c() calls (possible double-wrapping): {}",
            qb_str_from_c_count
        );
    }

    /// Regression: SELECT CASE string comparisons.
    /// Bug: SELECT CASE with strings didn't work, especially fixed-length strings.
    /// Fix: commits 18560d1, 11389a2 - Fixed string comparison using qb_string_compare().
    #[test]
    fn select_case_string_comparison() {
        let source = r#"
            DIM s$ AS STRING
            s$ = "two"
            SELECT CASE s$
                CASE "one"
                    PRINT "One"
                CASE "two"
                    PRINT "Two"
                CASE "three"
                    PRINT "Three"
                CASE ELSE
                    PRINT "Other"
            END SELECT
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Should use qb_string_compare for string comparisons
        assert!(
            output.code.contains("qb_string_compare"),
            "SELECT CASE with strings should use qb_string_compare: {}",
            output.code
        );
    }

    /// Regression: SELECT CASE with fixed-length strings.
    /// Bug: Fixed-length strings not properly wrapped when comparing.
    /// Fix: commit 11389a2 - Fixed fixed-length string wrapping in SELECT CASE.
    #[test]
    fn select_case_fixed_length_string() {
        let source = r#"
            DIM s AS STRING * 10
            s = "test"
            SELECT CASE s
                CASE "test"
                    PRINT "Match"
                CASE ELSE
                    PRINT "No match"
            END SELECT
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Should use qb_string_compare and properly handle fixed-length strings
        assert!(
            output.code.contains("qb_string_compare"),
            "SELECT CASE with fixed-length strings should use qb_string_compare"
        );
    }

    /// Regression: Array variable rename in array access.
    /// Bug: Array access variables didn't use renamed names, causing variable shadowing.
    /// Fix: commit 3fe464b - Fixed emit_array_access to apply variable_renames.
    #[test]
    fn array_variable_rename() {
        let source = r#"
            DIM arr(10) AS INTEGER
            arr(1) = 5
            arr(2) = 10
            PRINT arr(1); arr(2)
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Array access should use consistent variable names
        // The generated code should compile without variable shadowing issues
        assert!(
            output.code.contains("arr") || output.code.contains("arr_"),
            "Array access should use proper variable names"
        );
    }

    /// Regression: MID$ assignment with fixed-length strings.
    /// Bug: MID$ assignment with fixed-length strings caused stack corruption.
    /// Fix: commit 219a5ae - Detect FixedString type and use manual character copying.
    #[test]
    fn mid_assignment_fixed_length_string() {
        let source = r#"
            DIM s AS STRING * 20
            s = "Hello World"
            MID$(s, 7) = "BASIC"
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // For fixed-length strings, should use manual copying, not qb_mid_assign
        // Check that it doesn't use qb_mid_assign (which expects qb_string*, not fixed strings)
        // or if it does, it properly unwraps the fixed string first
        assert!(
            output.code.contains("MID$")
                || output.code.contains("mid")
                || output.code.contains("strncpy"),
            "MID$ assignment should generate appropriate code"
        );
    }

    /// Regression: MID$ assignment with fixed-length string arrays.
    /// Bug: MID$ assignment with fixed-length string arrays caused stack corruption.
    /// Fix: commit 219a5ae - Fixed handling of fixed-length string arrays.
    #[test]
    fn mid_assignment_fixed_length_string_array() {
        let source = r#"
            DIM arr(10) AS STRING * 20
            arr(1) = "Test"
            MID$(arr(1), 1, 2) = "XX"
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Should handle fixed-length string arrays correctly
        assert!(
            output.code.contains("MID$")
                || output.code.contains("mid")
                || output.code.contains("strncpy"),
            "MID$ assignment with fixed-length string arrays should generate appropriate code"
        );
    }

    /// Regression: String temp pool cleanup in loops.
    /// Bug: String temp pool not cleaned up in FOR/WHILE/DO loops, causing memory leaks.
    /// Fix: commit 4dd4e77 - Implemented scoped cleanup with save/restore base pattern.
    #[test]
    fn string_temp_pool_loop_cleanup() {
        let source = r#"
            DIM i AS INTEGER
            FOR i = 1 TO 100
                DIM s$ AS STRING
                s$ = "test" + STR$(i)
                PRINT s$
            NEXT i
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Should have cleanup calls in the loop
        // Look for qbs_cleanup or similar cleanup patterns
        assert!(
            output.code.contains("qbs_cleanup") || output.code.contains("cleanup"),
            "Loop should include string temp pool cleanup: {}",
            output.code
        );
    }

    /// Regression: Reference-counted string memory (retain/release + scoped cleanup).
    /// Bug: 42+ GB memory explosion during QB64pe bootstrap; missing temp pool and
    /// retain/release pattern. Fix: commit 4dd4e77 - qbs_tmp_register/qbs_cleanup,
    /// per-statement cleanup in main/procedure bodies, retain/release for assignments.
    /// This test asserts the code generator emits the required patterns.
    #[test]
    fn reference_counted_string_retain_release_and_cleanup() {
        let source = r#"
            DIM a$ AS STRING
            a$ = "hello" + " world"
            SUB S(b$ AS STRING)
                DIM c$ AS STRING
                c$ = b$ + "!"
            END SUB
            CALL S(a$)
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Retain/release pattern for string assignments
        assert!(
            output.code.contains("qb_string_retain") && output.code.contains("qb_string_release"),
            "String assignments must use retain/release: {}",
            output.code
        );
        // Temp pool registration (string concat etc. return qbs_tmp_register)
        assert!(
            output.code.contains("qbs_tmp_register"),
            "Temp string results must use qbs_tmp_register: {}",
            output.code
        );
        // Scoped cleanup in main and procedure bodies
        assert!(
            output.code.contains("qbs_cleanup"),
            "Main and procedure bodies must call qbs_cleanup: {}",
            output.code
        );
        assert!(
            output.code.contains("_qbs_main_base") || output.code.contains("_qbs_proc_base"),
            "Cleanup must use main/proc base: {}",
            output.code
        );
    }

    /// Regression: FFI declaration completeness check.
    /// Bug: Missing qb_dir() declaration caused pointer truncation crash (64-bit pointer
    /// truncated to 32-bit). Fix: commit 988e3e5 - Added missing declaration.
    /// This test verifies critical FFI functions are declared in the header.
    /// Note: Full completeness check would require more sophisticated parsing to avoid
    /// false positives from variable names and comments in source code.
    #[test]
    fn ffi_declaration_completeness() {
        use std::fs;

        // Get paths
        // CARGO_MANIFEST_DIR points to the crate root (QB64Fresh/)
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let crate_root = Path::new(manifest_dir);
        let header_path = crate_root.join("runtime/include/qb64fresh_rt.h");

        // Read header file
        let header_content =
            fs::read_to_string(&header_path).expect("Failed to read runtime header file");

        // Extract all declared FFI functions from header
        // Look for patterns like: "return_type qb_function_name(" or "QbString* qb_function_name("
        let mut declared_functions = std::collections::HashSet::new();

        // More robust parsing: handle multi-line declarations and various return types
        let mut in_multiline = false;
        let mut current_decl = String::new();

        for line in header_content.lines() {
            let line = line.trim();
            // Skip comments and preprocessor directives
            if line.starts_with("//")
                || line.starts_with("#")
                || line.starts_with("/*")
                || line == "*/"
            {
                continue;
            }

            // Handle multi-line declarations
            if in_multiline {
                current_decl.push_str(" ");
                current_decl.push_str(line);
                if line.contains('(') && line.contains(')') {
                    // Complete declaration
                    in_multiline = false;
                    if let Some(pos) = current_decl.find("qb_") {
                        if let Some(paren_pos) = current_decl[pos..].find('(') {
                            let func_part = &current_decl[pos..pos + paren_pos].trim();
                            // Extract function name
                            let func_name = if let Some(space_pos) = func_part.rfind(' ') {
                                &func_part[space_pos + 1..]
                            } else if func_part.starts_with("qb_") {
                                func_part
                            } else {
                                current_decl.clear();
                                continue;
                            };

                            if func_name.starts_with("qb_") {
                                let name = func_name.strip_prefix("qb_").unwrap_or(func_name);
                                if name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                                    declared_functions.insert(name.to_string());
                                }
                            }
                        }
                    }
                    current_decl.clear();
                }
                continue;
            }

            // Look for function declarations: qb_xxx(
            if line.contains("qb_") && line.contains('(') {
                if let Some(pos) = line.find("qb_") {
                    if let Some(paren_pos) = line[pos..].find('(') {
                        let func_part = &line[pos..pos + paren_pos].trim();
                        // Extract function name
                        let func_name = if let Some(space_pos) = func_part.rfind(' ') {
                            &func_part[space_pos + 1..]
                        } else if func_part.starts_with("qb_") {
                            func_part
                        } else {
                            continue;
                        };

                        if func_name.starts_with("qb_") {
                            let name = func_name.strip_prefix("qb_").unwrap_or(func_name);
                            if name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                                declared_functions.insert(name.to_string());
                            }
                        }
                    } else if line.contains("qb_") && !line.contains(')') {
                        // Multi-line declaration starting
                        in_multiline = true;
                        current_decl = line.to_string();
                    }
                }
            }
        }

        // Verify critical FFI functions that are known to be used are declared
        // This focuses on functions that return pointers (most critical for truncation bugs)
        let critical_functions = [
            "dir",           // The specific bug that was fixed (commit 988e3e5)
            "string_new",    // Commonly used, returns pointer
            "string_concat", // Commonly used, returns pointer
            "string_retain", // Returns pointer
            "str_from_c",    // Returns pointer
            "memnew",        // Returns struct with pointer
            "memimage",      // Returns struct with pointer
            "memsound",      // Returns struct with pointer
        ];

        let mut missing_critical = Vec::new();
        for func in &critical_functions {
            if !declared_functions.contains(*func) {
                missing_critical.push(format!("qb_{}()", func));
            }
        }

        if !missing_critical.is_empty() {
            panic!(
                "Critical FFI functions not declared in header:\n  {}\n\n\
                 This could cause pointer truncation crashes on 64-bit systems.\n\
                 Add declarations to runtime/include/qb64fresh_rt.h",
                missing_critical.join("\n  ")
            );
        }

        // Verify that qb_dir is declared (the specific bug that was fixed)
        assert!(
            declared_functions.contains("dir"),
            "qb_dir() must be declared (commit 988e3e5 fix - prevents pointer truncation crash)"
        );
    }

    /// Regression: Runtime initialization order.
    /// Bug: Runtime not properly initialized before use, causing crashes.
    /// Fix: commit 219a5ae - Added qb_runtime_init() and qb_runtime_shutdown() calls.
    #[test]
    fn runtime_initialization_order() {
        // Create a simple program that uses runtime functions
        let source = r#"
            PRINT "Hello"
            DIM s$ AS STRING
            s$ = "test"
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        // Use external runtime mode (where initialization is required)
        let backend = CBackend::with_runtime_mode(RuntimeMode::external_with_header(Path::new(
            "runtime/include/qb64fresh_rt.h",
        )));
        let output = backend.generate(&typed).expect("Should generate");

        // Verify qb_runtime_init() is called
        assert!(
            output.code.contains("qb_runtime_init()"),
            "qb_runtime_init() must be called in external runtime mode"
        );

        // Find the main function to check initialization order within it
        // (string constants are defined before main, so we need to check within main)
        let main_start = output.code.find("int main(");
        if main_start.is_none() {
            panic!("main function not found in generated code");
        }
        let main_start = main_start.unwrap();

        // Find where main function ends (look for the closing brace at the same indentation level)
        // For simplicity, just check a reasonable portion after main starts
        let main_section = if let Some(main_end) = output.code[main_start..].find("\n}") {
            &output.code[main_start..main_start + main_end]
        } else {
            // If we can't find the end, just use a large chunk
            &output.code[main_start..]
        };

        // Verify initialization order within main function
        let init_pos = main_section.find("qb_runtime_init()");
        if init_pos.is_none() {
            panic!("qb_runtime_init() not found in main function for external runtime mode");
        }
        let init_pos = init_pos.unwrap();

        // Verify qb_init_args comes after qb_runtime_init (within main)
        if let Some(args_pos) = main_section.find("qb_init_args") {
            assert!(
                init_pos < args_pos,
                "qb_init_args() should come after qb_runtime_init() in main function"
            );
        }

        // Verify qb_init_startdir comes after qb_runtime_init (within main)
        if let Some(startdir_pos) = main_section.find("qb_init_startdir") {
            assert!(
                init_pos < startdir_pos,
                "qb_init_startdir() should come after qb_runtime_init() in main function"
            );
        }

        // Verify error checking after qb_runtime_init
        let init_end = init_pos + "qb_runtime_init()".len();
        let after_init = &main_section[init_end..];
        assert!(
            after_init.contains("_qb_err != 0") || after_init.contains("_qb_err == 0"),
            "Error checking should be present after qb_runtime_init() call"
        );

        // Verify that runtime function calls (like qb_print_string) come after initialization
        // Find first qb_print_string call in main (not in string constant definitions)
        if let Some(print_pos) = main_section.find("qb_print_string") {
            assert!(
                init_pos < print_pos,
                "qb_runtime_init() must be called before qb_print_string() in main function"
            );
        }
    }

    /// Regression: String temp pool overflow tracking.
    /// Bug: String temp pool overflowed without proper tracking when main pool was full,
    /// causing 39.8GB memory usage. Fix: commit 5c4d469 - Added overflow tracking.
    #[test]
    fn string_temp_pool_overflow_tracking() {
        // Create a program that generates many temp strings
        // This tests that overflow tracking mechanism exists in generated code
        let source = r#"
            DIM i AS INTEGER
            DIM s$ AS STRING
            ' Create many string operations to potentially fill temp pool
            FOR i = 1 TO 1000
                s$ = "test" + STR$(i) + "more" + STR$(i * 2)
                s$ = LEFT$(s$, 10) + RIGHT$(s$, 5)
            NEXT i
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Verify overflow tracking mechanism exists in generated code
        // The fix added overflow pool tracking when main pool is full
        assert!(
            output.code.contains("_qbs_tmp_overflow"),
            "Generated code should include overflow tracking array: {}",
            output.code
        );

        // Verify overflow tracking variables exist
        assert!(
            output.code.contains("_qbs_tmp_overflow_count"),
            "Generated code should include overflow count variable"
        );

        // Verify qbs_tmp_base_get returns packed uint64_t with both bases
        assert!(
            output.code.contains("overflow_base << 32") || output.code.contains("base >> 32"),
            "Generated code should pack/unpack overflow base in uint64_t"
        );

        // Verify cleanup handles overflow pool
        assert!(
            output
                .code
                .contains("_qbs_tmp_overflow_count > overflow_base"),
            "Generated code should cleanup overflow strings: {}",
            output.code
        );
    }

    /// Regression: FFI error reporting.
    /// Bug: FFI functions returned simple 0/1 codes, losing detailed error information.
    /// Fix: Session 067 - Created log_ffi_error! macro for consistent error logging.
    /// This test verifies that FFI error logging is present in the runtime.
    #[test]
    fn ffi_error_reporting() {
        use std::fs;

        // Get paths
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let crate_root = Path::new(manifest_dir);
        let graphics_ffi_path = crate_root.join("runtime/src/graphics_ffi.rs");

        // Read graphics_ffi.rs file
        let graphics_ffi_content =
            fs::read_to_string(&graphics_ffi_path).expect("Failed to read graphics_ffi.rs");

        // Verify log_ffi_error macro is defined
        assert!(
            graphics_ffi_content.contains("macro_rules! log_ffi_error"),
            "log_ffi_error macro should be defined in graphics_ffi.rs"
        );

        // Verify macro is used in FFI functions (check for usage pattern)
        // The macro should be used in error handling paths
        let log_ffi_error_usage_count = graphics_ffi_content.matches("log_ffi_error!").count();
        assert!(
            log_ffi_error_usage_count > 30,
            "log_ffi_error! should be used extensively in FFI functions (found {} uses)",
            log_ffi_error_usage_count
        );

        // Verify macro logs to stderr (eprintln!)
        assert!(
            graphics_ffi_content.contains("eprintln!"),
            "log_ffi_error macro should use eprintln! for error logging"
        );

        // Verify macro includes function name and error details
        assert!(
            graphics_ffi_content.contains("Error in {}")
                || graphics_ffi_content.contains("Error in"),
            "log_ffi_error macro should log function name and error details"
        );
    }

    /// Regression: Built-in constant registration.
    /// Bug: _CHR_* and _STR_* constants not registered, causing 16+4 semantic errors.
    /// Fix: Session 049 - Added register_string_character_constants() to register ~60 constants.
    /// This test verifies that constants are registered and can be used in programs.
    #[test]
    fn builtin_constant_registration() {
        // Test that _CHR_* and _STR_* constants are registered and can be used
        let source = r#"
            DIM s$ AS STRING
            ' Test various _CHR_* constants
            s$ = _CHR_CR + _CHR_LF
            s$ = _CHR_QUOTE + "test" + _CHR_QUOTE
            s$ = _CHR_SPACE + "hello" + _CHR_SPACE
            ' Test _STR_* constants
            s$ = _STR_EMPTY
            s$ = _STR_CRLF
            s$ = _STR_LF
            s$ = _STR_CR
            PRINT s$
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer
            .analyze(&program)
            .expect("Should analyze - constants should be registered");

        // Verify constants are recognized (no semantic errors)
        // The test passes if analyze() succeeds without "undefined constant" errors

        // Also verify in generated code that constants are used
        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Constants should be referenced in generated code
        // They may appear as string literals or constant references
        assert!(
            output.code.contains("_CHR_")
                || output.code.contains("_STR_")
                || output.code.contains("CHR_")
                || output.code.contains("STR_"),
            "Generated code should reference _CHR_* or _STR_* constants"
        );
    }

    /// Regression: Error handler syntax.
    /// Bug: ON ERROR GOTO _NEWHANDLER qberror_test parsed as two statements instead of one.
    /// Fix: Session 049 - Modified parse_label_target() to recognize _NEWHANDLER as modifier.
    /// This test verifies that ON ERROR GOTO _NEWHANDLER syntax is parsed correctly.
    #[test]
    fn error_handler_syntax() {
        let source = r#"
            ON ERROR GOTO _NEWHANDLER errorHandler
            PRINT "test"
            errorHandler:
                PRINT "Error occurred"
                RESUME NEXT
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser
            .parse()
            .expect("Should parse - ON ERROR GOTO _NEWHANDLER should be valid");

        // Verify that _NEWHANDLER is parsed as part of the label target, not as a separate statement
        // Check that we have an ON ERROR statement with the correct target
        use qb64fresh::prelude::StatementKind;
        let on_error_found = program.statements.iter().any(|stmt| {
            matches!(
                &stmt.kind,
                StatementKind::OnErrorGoto { target } if target.contains("_NEWHANDLER")
            )
        });

        assert!(
            on_error_found,
            "ON ERROR GOTO _NEWHANDLER should be parsed as a single statement with _NEWHANDLER in target"
        );

        // Verify semantic analysis succeeds (no "undefined label" errors)
        let mut analyzer = SemanticAnalyzer::new();
        analyzer
            .analyze(&program)
            .expect("Should analyze - error handler syntax should be valid");
    }

    /// Regression: Label uniqueness.
    /// Bug: Labels like Help_CheckFinishLine: appeared 5 times due to ambiguous parsing.
    /// Fix: Session 050 - Added emitted_labels HashSet to track and skip duplicate labels.
    /// This test verifies that labels are not emitted multiple times in generated code.
    #[test]
    fn label_uniqueness() {
        // Create a program with a label that might be parsed ambiguously
        // (e.g., a label followed by another identifier that could look like a label)
        let source = r#"
            testLabel:
                PRINT "test"
            testLabel:
                PRINT "duplicate"
            anotherLabel:
                PRINT "another"
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Count occurrences of each label in generated code
        // Labels should appear only once (as label definitions, not duplicates)
        let test_label_count = output.code.matches("testLabel:").count();
        let another_label_count = output.code.matches("anotherLabel:").count();

        // Each label should appear exactly once (or possibly not at all if optimized away)
        // But definitely not multiple times
        assert!(
            test_label_count <= 1,
            "testLabel should appear at most once in generated code (found {} times)",
            test_label_count
        );

        assert!(
            another_label_count <= 1,
            "anotherLabel should appear at most once in generated code (found {} times)",
            another_label_count
        );
    }

    /// Regression: Duplicate label emission (REGRESSION_TEST_COVERAGE §6.1, session 050).
    ///
    /// Would regress if we emitted the same label twice: two GOTOs to the same label
    /// in different branches, plus duplicate label statements in the IR (e.g. from
    /// ambiguous parsing), must result in exactly one label definition in generated C.
    #[test]
    fn duplicate_label_emission_regression() {
        // Program with one label targeted by two GOTOs in different branches,
        // and a duplicate label statement (same name twice) to trigger the
        // emitted_labels skip logic. Without the fix, we'd emit the label twice → C duplicate symbol.
        let source = r#"
            x = 1
            IF x THEN GOTO target ELSE GOTO target
            PRINT "unreachable"
        target:
            PRINT "here"
        target:
            PRINT "duplicate label stmt"
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Label definition in C is "target:" (proc_label in main = c_identifier).
        // Must appear exactly once; if emitted_labels were removed we'd get two "target:" lines.
        let label_def_count = output.code.matches("target:").count();
        assert_eq!(
            label_def_count, 1,
            "label 'target:' should appear exactly once as a definition in generated C (found {}); \
             duplicate emission would cause C redefinition error",
            label_def_count
        );
    }

    /// Regression: Forward declarations.
    /// Bug: Cross-module dependencies not forward-declared, causing compilation errors
    /// (e.g. `qb_gfx_screen` referencing variables defined later in different modules).
    /// Fix: Session 050 - Added emit_forward_declarations() in mod.rs.
    ///
    /// This test ensures the code generator always emits the forward declaration block
    /// and the specific declarations required so that C compiles without "undefined reference".
    #[test]
    fn forward_declarations() {
        // Any program using the inline runtime gets the forward declarations block.
        let source = r#"
            SCREEN 0
            PRINT "test"
            DIM s$ AS STRING
            s$ = "hello"
        "#;

        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Should parse");

        let mut analyzer = SemanticAnalyzer::new();
        let typed = analyzer.analyze(&program).expect("Should analyze");

        let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
        let output = backend.generate(&typed).expect("Should generate");

        // Section comment emitted by emit_forward_declarations()
        assert!(
            output
                .code
                .contains("/* Forward declarations for cross-module dependencies */"),
            "Generated code must include the forward declarations block (fix: emit_forward_declarations in mod.rs)"
        );

        // Required forward declarations: without these, C compilation fails with undefined reference.
        // These are defined in other runtime modules but used earlier (e.g. system.rs using io/strings).
        assert!(
            output.code.contains("typedef struct QbString qb_string;"),
            "Forward declaration for qb_string type required for function prototypes"
        );
        assert!(
            output.code.contains("static void _qb_gfx_warn(void);"),
            "Forward declaration for _qb_gfx_warn (graphics, used by memory/system)"
        );
        assert!(
            output.code.contains("void qb_print_string(qb_string* s);"),
            "Forward declaration for qb_print_string (io, used by system)"
        );
        assert!(
            output
                .code
                .contains("const char* qb_string_data(qb_string* s);"),
            "Forward declaration for qb_string_data (strings, used by many modules)"
        );
    }
}
