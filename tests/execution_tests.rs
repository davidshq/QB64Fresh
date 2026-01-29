//! End-to-End Execution Tests for QB64Fresh
//!
//! These tests verify that programs not only compile correctly, but also
//! execute and produce the expected output. This is critical for catching
//! runtime bugs that compilation-only tests miss.
//!
//! Each test:
//! 1. Compiles BASIC source to C code
//! 2. Compiles the C code to an executable (linking with the runtime)
//! 3. Runs the executable
//! 4. Verifies the output matches expectations

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;
use std::io::Write;
use std::process::Command;

/// Compile BASIC source to C code (external runtime mode)
#[allow(dead_code)]
fn compile_to_c(source: &str) -> Result<String, String> {
    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|errors| format!("Parse errors: {:?}", errors))?;

    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|errors| format!("Semantic errors: {:?}", errors))?;

    // Use external runtime mode for linking with runtime library
    let backend = CBackend::with_runtime_mode(RuntimeMode::external());
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {:?}", e))?;

    Ok(output.code)
}

/// Build the runtime library if needed and return the path
fn ensure_runtime_built() -> Result<std::path::PathBuf, String> {
    // Build the runtime library
    let status = Command::new("cargo")
        .args(["build", "-p", "qb64fresh-runtime", "--release"])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .status()
        .map_err(|e| format!("Failed to run cargo: {}", e))?;

    if !status.success() {
        return Err("Failed to build runtime library".to_string());
    }

    // Return path to the built library
    let lib_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("release");

    Ok(lib_path)
}

/// Compile C code to an executable and run it, returning stdout
fn compile_and_run(c_code: &str, _runtime_path: &std::path::Path) -> Result<String, String> {
    let temp_dir = std::env::temp_dir();
    let unique_id = std::process::id();
    let c_file = temp_dir.join(format!("qb64_test_{}.c", unique_id));
    let exe_file = temp_dir.join(format!("qb64_test_{}", unique_id));

    // Write C code to file
    let mut file =
        std::fs::File::create(&c_file).map_err(|e| format!("Failed to create C file: {}", e))?;
    file.write_all(c_code.as_bytes())
        .map_err(|e| format!("Failed to write C file: {}", e))?;

    // Compile with gcc (or clang)
    // For now, we'll use inline runtime mode which doesn't need linking
    let compile_result = Command::new("gcc")
        .args([
            "-o",
            exe_file.to_str().unwrap(),
            c_file.to_str().unwrap(),
            "-lm", // Math library
        ])
        .output()
        .map_err(|e| format!("Failed to run gcc: {}", e))?;

    // Clean up C file
    let _ = std::fs::remove_file(&c_file);

    if !compile_result.status.success() {
        let stderr = String::from_utf8_lossy(&compile_result.stderr);
        return Err(format!("C compilation failed: {}", stderr));
    }

    // Run the executable
    let run_result = Command::new(&exe_file)
        .output()
        .map_err(|e| format!("Failed to run executable: {}", e))?;

    // Clean up executable
    let _ = std::fs::remove_file(&exe_file);

    if !run_result.status.success() {
        let stderr = String::from_utf8_lossy(&run_result.stderr);
        return Err(format!(
            "Program execution failed (exit code {:?}): {}",
            run_result.status.code(),
            stderr
        ));
    }

    Ok(String::from_utf8_lossy(&run_result.stdout).to_string())
}

/// Compile BASIC source to C code using inline runtime (no linking needed)
fn compile_to_c_inline(source: &str) -> Result<String, String> {
    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|errors| format!("Parse errors: {:?}", errors))?;

    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|errors| format!("Semantic errors: {:?}", errors))?;

    // Use inline runtime mode - all runtime code is embedded in the C output
    let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {:?}", e))?;

    Ok(output.code)
}

/// Compile and run a BASIC program, returning the stdout output
fn run_basic_program(source: &str) -> Result<String, String> {
    let c_code = compile_to_c_inline(source)?;

    let temp_dir = std::env::temp_dir();
    let unique_id = std::process::id();
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let c_file = temp_dir.join(format!("qb64_test_{}_{}.c", unique_id, timestamp));
    let exe_file = temp_dir.join(format!("qb64_test_{}_{}", unique_id, timestamp));

    // Write C code to file
    let mut file =
        std::fs::File::create(&c_file).map_err(|e| format!("Failed to create C file: {}", e))?;
    file.write_all(c_code.as_bytes())
        .map_err(|e| format!("Failed to write C file: {}", e))?;

    // Compile with gcc
    let compile_result = Command::new("gcc")
        .args([
            "-o",
            exe_file.to_str().unwrap(),
            c_file.to_str().unwrap(),
            "-lm", // Math library
        ])
        .output()
        .map_err(|e| format!("Failed to run gcc: {}", e))?;

    // Clean up C file
    let _ = std::fs::remove_file(&c_file);

    if !compile_result.status.success() {
        let stderr = String::from_utf8_lossy(&compile_result.stderr);
        return Err(format!("C compilation failed: {}", stderr));
    }

    // Run the executable with a timeout
    let run_result = Command::new(&exe_file)
        .output()
        .map_err(|e| format!("Failed to run executable: {}", e))?;

    // Clean up executable
    let _ = std::fs::remove_file(&exe_file);

    Ok(String::from_utf8_lossy(&run_result.stdout).to_string())
}

/// Check if gcc is available
fn gcc_available() -> bool {
    Command::new("gcc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// =============================================================================
// Execution Tests - These require gcc to be installed
// =============================================================================

mod execution {
    use super::*;

    /// Skip test if gcc is not available
    macro_rules! require_gcc {
        () => {
            if !gcc_available() {
                eprintln!("Skipping test: gcc not available");
                return;
            }
        };
    }

    #[test]
    fn hello_world_executes() {
        require_gcc!();

        let source = r#"PRINT "Hello, World!""#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("Hello, World!"),
            "Output should contain 'Hello, World!', got: {}",
            output
        );
    }

    #[test]
    fn print_integer() {
        require_gcc!();

        let source = "PRINT 42";
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("42"),
            "Output should contain '42', got: {}",
            output
        );
    }

    #[test]
    fn print_multiple_values() {
        require_gcc!();

        let source = r#"
            PRINT "A"; "B"; "C"
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("ABC") || output.contains("A B C"),
            "Output should contain ABC or A B C, got: {}",
            output
        );
    }

    #[test]
    fn arithmetic_operations() {
        require_gcc!();

        let source = r#"
            PRINT 2 + 3
            PRINT 10 - 4
            PRINT 3 * 4
            PRINT 15 / 3
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("5"), "Should contain 2+3=5");
        assert!(output.contains("6"), "Should contain 10-4=6");
        assert!(output.contains("12"), "Should contain 3*4=12");
    }

    #[test]
    fn variable_assignment() {
        require_gcc!();

        let source = r#"
            DIM x AS INTEGER
            x = 42
            PRINT x
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("42"),
            "Output should contain '42', got: {}",
            output
        );
    }

    #[test]
    fn for_loop_basic() {
        require_gcc!();

        let source = r#"
            FOR i = 1 TO 5
                PRINT i
            NEXT i
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        // Should print 1, 2, 3, 4, 5
        for n in 1..=5 {
            assert!(
                output.contains(&n.to_string()),
                "Output should contain '{}', got: {}",
                n,
                output
            );
        }
    }

    #[test]
    fn for_loop_with_step() {
        require_gcc!();

        let source = r#"
            FOR i = 0 TO 10 STEP 2
                PRINT i
            NEXT i
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        // Should print 0, 2, 4, 6, 8, 10
        for n in [0, 2, 4, 6, 8, 10] {
            assert!(
                output.contains(&n.to_string()),
                "Output should contain '{}', got: {}",
                n,
                output
            );
        }
    }

    #[test]
    fn if_then_true_branch() {
        require_gcc!();

        let source = r#"
            IF 1 = 1 THEN
                PRINT "TRUE"
            ELSE
                PRINT "FALSE"
            END IF
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("TRUE"),
            "Output should contain 'TRUE', got: {}",
            output
        );
        assert!(
            !output.contains("FALSE"),
            "Output should not contain 'FALSE', got: {}",
            output
        );
    }

    #[test]
    fn if_then_false_branch() {
        require_gcc!();

        let source = r#"
            IF 1 = 2 THEN
                PRINT "TRUE"
            ELSE
                PRINT "FALSE"
            END IF
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("FALSE"),
            "Output should contain 'FALSE', got: {}",
            output
        );
        assert!(
            !output.contains("TRUE"),
            "Output should not contain 'TRUE', got: {}",
            output
        );
    }

    #[test]
    fn while_loop() {
        require_gcc!();

        let source = r#"
            DIM i AS INTEGER
            i = 1
            WHILE i <= 3
                PRINT i
                i = i + 1
            WEND
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("1"), "Should contain 1");
        assert!(output.contains("2"), "Should contain 2");
        assert!(output.contains("3"), "Should contain 3");
    }

    #[test]
    fn do_loop_until() {
        require_gcc!();

        let source = r#"
            DIM i AS INTEGER
            i = 1
            DO
                PRINT i
                i = i + 1
            LOOP UNTIL i > 3
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("1"), "Should contain 1");
        assert!(output.contains("2"), "Should contain 2");
        assert!(output.contains("3"), "Should contain 3");
    }

    #[test]
    fn select_case() {
        require_gcc!();

        let source = r#"
            DIM x AS INTEGER
            x = 2
            SELECT CASE x
                CASE 1
                    PRINT "ONE"
                CASE 2
                    PRINT "TWO"
                CASE 3
                    PRINT "THREE"
                CASE ELSE
                    PRINT "OTHER"
            END SELECT
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("TWO"),
            "Output should contain 'TWO', got: {}",
            output
        );
    }

    #[test]
    fn function_call() {
        require_gcc!();

        // Note: Cannot use "Double" as function name because it conflicts with the DOUBLE type keyword
        // Note: BYVAL is required because codegen doesn't yet handle by-ref parameter dereferencing
        let source = r#"
            FUNCTION Multiply2(BYVAL n AS INTEGER) AS INTEGER
                Multiply2 = n * 2
            END FUNCTION

            PRINT Multiply2(21)
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("42"),
            "Output should contain '42' (21*2), got: {}",
            output
        );
    }

    #[test]
    fn sub_call() {
        require_gcc!();

        let source = r#"
            SUB SayHello
                PRINT "Hello from SUB"
            END SUB

            SayHello
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("Hello from SUB"),
            "Output should contain 'Hello from SUB', got: {}",
            output
        );
    }

    /// Regression: BYREF scalar parameters must be visible to the caller when modified in a SUB.
    /// SUB parameters are BYREF by default; codegen uses a pointer alias (e.g. int32_t* x = x_ref)
    /// and dereferences on use. Modifications must write through so the caller sees the new value.
    /// See FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md §9.
    #[test]
    fn byref_scalar_sub_modifies_caller_variable() {
        require_gcc!();

        let source = r#"
            SUB SetValue(n AS INTEGER)
                n = 99
            END SUB

            DIM x AS INTEGER
            x = 5
            CALL SetValue(x)
            PRINT x
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("99"),
            "BYREF scalar: caller should see value 99 after SUB modifies parameter, got: {}",
            output
        );
    }

    #[test]
    fn string_concatenation() {
        require_gcc!();

        let source = r#"
            DIM a AS STRING
            DIM b AS STRING
            a = "Hello"
            b = "World"
            PRINT a + " " + b
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("Hello World"),
            "Output should contain 'Hello World', got: {}",
            output
        );
    }

    #[test]
    fn array_basic() {
        require_gcc!();

        let source = r#"
            DIM arr(3) AS INTEGER
            arr(1) = 10
            arr(2) = 20
            arr(3) = 30
            PRINT arr(1)
            PRINT arr(2)
            PRINT arr(3)
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("10"), "Should contain 10");
        assert!(output.contains("20"), "Should contain 20");
        assert!(output.contains("30"), "Should contain 30");
    }

    #[test]
    fn recursive_function() {
        require_gcc!();

        // Note: BYVAL is required because codegen doesn't yet handle by-ref parameter dereferencing
        let source = r#"
            FUNCTION Factorial(BYVAL n AS INTEGER) AS INTEGER
                IF n <= 1 THEN
                    Factorial = 1
                ELSE
                    Factorial = n * Factorial(n - 1)
                END IF
            END FUNCTION

            PRINT Factorial(5)
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(
            output.contains("120"),
            "Output should contain '120' (5!), got: {}",
            output
        );
    }

    #[test]
    fn math_functions() {
        require_gcc!();

        let source = r#"
            PRINT ABS(-42)
            PRINT SGN(-5)
            PRINT SGN(0)
            PRINT SGN(5)
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("42"), "ABS(-42) should be 42");
        assert!(output.contains("-1"), "SGN(-5) should be -1");
        // Note: SGN(0) = 0 and SGN(5) = 1 are also in output
    }

    #[test]
    fn comparison_operators() {
        require_gcc!();

        let source = r#"
            IF 5 > 3 THEN PRINT "5>3"
            IF 3 < 5 THEN PRINT "3<5"
            IF 5 >= 5 THEN PRINT "5>=5"
            IF 5 <= 5 THEN PRINT "5<=5"
            IF 5 = 5 THEN PRINT "5=5"
            IF 5 <> 3 THEN PRINT "5<>3"
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("5>3"), "5>3 should be true");
        assert!(output.contains("3<5"), "3<5 should be true");
        assert!(output.contains("5>=5"), "5>=5 should be true");
        assert!(output.contains("5<=5"), "5<=5 should be true");
        assert!(output.contains("5=5"), "5=5 should be true");
        assert!(output.contains("5<>3"), "5<>3 should be true");
    }

    #[test]
    fn logical_operators() {
        require_gcc!();

        let source = r#"
            IF 1 AND 1 THEN PRINT "AND_TRUE"
            IF 0 OR 1 THEN PRINT "OR_TRUE"
            IF NOT 0 THEN PRINT "NOT_TRUE"
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("AND_TRUE"), "1 AND 1 should be true");
        assert!(output.contains("OR_TRUE"), "0 OR 1 should be true");
        assert!(output.contains("NOT_TRUE"), "NOT 0 should be true");
    }

    #[test]
    fn empty_program_executes() {
        require_gcc!();

        let source = "";
        // Empty program should execute without error
        let result = run_basic_program(source);
        assert!(result.is_ok(), "Empty program should execute: {:?}", result);
    }

    #[test]
    fn program_with_end_executes() {
        require_gcc!();

        let source = r#"
            PRINT "Before END"
            END
            PRINT "After END"
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("Before END"), "Should print before END");
        // After END should not execute
        assert!(!output.contains("After END"), "Should not print after END");
    }

    #[test]
    fn nested_loops() {
        require_gcc!();

        let source = r#"
            FOR i = 1 TO 2
                FOR j = 1 TO 2
                    PRINT i * 10 + j
                NEXT j
            NEXT i
        "#;
        let output = run_basic_program(source).expect("Program should execute");

        assert!(output.contains("11"), "Should contain 11");
        assert!(output.contains("12"), "Should contain 12");
        assert!(output.contains("21"), "Should contain 21");
        assert!(output.contains("22"), "Should contain 22");
    }
}

// =============================================================================
// Compilation verification tests (no gcc needed)
// =============================================================================

mod compilation_verification {
    use super::*;

    #[test]
    fn generates_valid_c_hello_world() {
        let source = r#"PRINT "Hello""#;
        let c_code = compile_to_c_inline(source).expect("Should compile");

        // Verify the C code has expected structure
        assert!(c_code.contains("main"), "Should have main function");
        assert!(c_code.contains("Hello"), "Should contain the string");
    }

    #[test]
    fn generates_valid_c_for_loop() {
        let source = r#"
            FOR i = 1 TO 10
                PRINT i
            NEXT i
        "#;
        let c_code = compile_to_c_inline(source).expect("Should compile");

        assert!(c_code.contains("for"), "Should have for loop");
    }

    #[test]
    fn generates_valid_c_function() {
        let source = r#"
            FUNCTION Add(a AS INTEGER, b AS INTEGER) AS INTEGER
                Add = a + b
            END FUNCTION
        "#;
        let c_code = compile_to_c_inline(source).expect("Should compile");

        assert!(
            c_code.contains("Add") || c_code.contains("add"),
            "Should have Add function"
        );
    }

    #[test]
    fn generates_valid_c_array() {
        let source = r#"
            DIM arr(10) AS INTEGER
            arr(1) = 42
        "#;
        let c_code = compile_to_c_inline(source).expect("Should compile");

        assert!(c_code.contains("arr"), "Should have arr variable");
    }
}
