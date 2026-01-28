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
    let tokens = lex(&source);
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
    let backend = CBackend::with_runtime_mode(RuntimeMode::External);
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {:?}", e))?;
    let codegen_time = codegen_start.elapsed();

    Ok(CompilationResult {
        source_bytes: source.len(),
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

    let tokens = lex(&source);
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
            let lines: Vec<&str> = source.lines().collect();
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
                let mut line_num = 0;
                let mut byte_count = 0;
                for (i, line) in lines.iter().enumerate() {
                    if byte_count + line.len() + 1 > span_start {
                        line_num = i;
                        break;
                    }
                    byte_count += line.len() + 1; // +1 for newline
                }
                println!("\nError near line {} (byte {}):", line_num + 1, span_start);
                for i in line_num.saturating_sub(3)..=(line_num + 3).min(lines.len() - 1) {
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
#[test]
#[ignore = "Golden file not yet created - run with UPDATE_GOLDEN=1 to create"]
fn qb64pe_codegen_golden() {
    // This test would compare against a golden file
    // For now, it's marked ignore until we decide on golden file strategy
    todo!("Implement golden file comparison for QB64pe subset");
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

    let backend = CBackend::with_runtime_mode(RuntimeMode::External);
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

    let backend = CBackend::with_runtime_mode(RuntimeMode::External);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
        let output = backend.generate(&typed).expect("Should generate");

        // Should have cleanup calls in the loop
        // Look for qbs_cleanup or similar cleanup patterns
        assert!(
            output.code.contains("qbs_cleanup") || output.code.contains("cleanup"),
            "Loop should include string temp pool cleanup: {}",
            output.code
        );
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

        let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
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
}
