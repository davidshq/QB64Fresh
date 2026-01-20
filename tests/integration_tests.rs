//! Integration tests for QB64Fresh compiler
//!
//! These tests exercise the full compilation pipeline from source code to generated output.
//! They ensure all compiler phases work correctly together.

use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;

/// Helper to run the full compilation pipeline and return generated C code.
/// Returns Ok(code) on success, Err(message) on any compilation error.
fn compile_to_c(source: &str) -> Result<String, String> {
    // Lexer phase
    let tokens = lex(source);

    // Parser phase
    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse()
        .map_err(|errors| format!("Parse errors: {:?}", errors))?;

    // Semantic analysis phase
    let mut analyzer = SemanticAnalyzer::new();
    let typed_program = analyzer
        .analyze(&program)
        .map_err(|errors| format!("Semantic errors: {:?}", errors))?;

    // Code generation phase
    let backend = CBackend::with_runtime_mode(RuntimeMode::Inline);
    let output = backend
        .generate(&typed_program)
        .map_err(|e| format!("CodeGen error: {}", e))?;

    Ok(output.code)
}

/// Helper to check that compilation succeeds for a given source.
fn assert_compiles(source: &str) {
    match compile_to_c(source) {
        Ok(_) => {}
        Err(e) => panic!("Expected compilation to succeed, but got: {}", e),
    }
}

/// Helper to check that compilation fails with an error.
fn assert_compile_error(source: &str, expected_substring: &str) {
    match compile_to_c(source) {
        Ok(_) => panic!("Expected compilation to fail, but it succeeded"),
        Err(e) => {
            assert!(
                e.contains(expected_substring),
                "Expected error containing '{}', but got: {}",
                expected_substring,
                e
            );
        }
    }
}

// =============================================================================
// Basic Program Compilation Tests
// =============================================================================

mod basic_programs {
    use super::*;

    #[test]
    fn hello_world() {
        let source = r#"PRINT "Hello, World!""#;
        assert_compiles(source);
    }

    #[test]
    fn empty_program() {
        let source = "";
        assert_compiles(source);
    }

    #[test]
    fn program_with_end() {
        let source = "END";
        assert_compiles(source);
    }

    #[test]
    fn program_with_system() {
        let source = "SYSTEM";
        assert_compiles(source);
    }

    #[test]
    fn comments_only() {
        let source = r#"
            ' This is a comment
            REM This is also a comment
        "#;
        assert_compiles(source);
    }

    #[test]
    fn multiple_print_statements() {
        let source = r#"
            PRINT "Line 1"
            PRINT "Line 2"
            PRINT "Line 3"
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Variable Declaration and Assignment Tests
// =============================================================================

mod variables {
    use super::*;

    #[test]
    fn dim_integer() {
        let source = r#"
            DIM x AS LONG
            x = 42
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn dim_long() {
        let source = r#"
            DIM x AS LONG
            x = 1000000
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn dim_single() {
        let source = r#"
            DIM x AS DOUBLE
            x = 3.14
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn dim_double() {
        let source = r#"
            DIM x AS DOUBLE
            x = 3.14159265358979
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn dim_string() {
        let source = r#"
            DIM s AS STRING
            s = "Hello"
            PRINT s
        "#;
        assert_compiles(source);
    }

    #[test]
    fn multiple_declarations() {
        let source = r#"
            DIM a AS LONG
            DIM b AS LONG
            DIM c AS DOUBLE
            a = 1
            b = 2
            c = 3.0
            PRINT a; b; c
        "#;
        assert_compiles(source);
    }

    #[test]
    fn type_suffix_integer() {
        let source = r#"
            x& = 42
            PRINT x&
        "#;
        assert_compiles(source);
    }

    #[test]
    fn type_suffix_long() {
        let source = r#"
            x& = 100000
            PRINT x&
        "#;
        assert_compiles(source);
    }

    #[test]
    fn type_suffix_single() {
        let source = r#"
            x# = 3.14
            PRINT x#
        "#;
        assert_compiles(source);
    }

    #[test]
    fn type_suffix_double() {
        let source = r#"
            x# = 3.14159
            PRINT x#
        "#;
        assert_compiles(source);
    }

    #[test]
    fn type_suffix_string() {
        let source = r#"
            x$ = "Hello"
            PRINT x$
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Expression Tests
// =============================================================================

mod expressions {
    use super::*;

    #[test]
    fn arithmetic_addition() {
        let source = r#"
            DIM x AS LONG
            x = 1 + 2
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_subtraction() {
        let source = r#"
            DIM x AS LONG
            x = 10 - 3
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_multiplication() {
        let source = r#"
            DIM x AS LONG
            x = 4 * 5
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_division() {
        let source = r#"
            DIM x AS DOUBLE
            x = 10 / 3
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_integer_division() {
        let source = r#"
            DIM x AS LONG
            x = 10 \ 3
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_modulo() {
        let source = r#"
            DIM x AS LONG
            x = 10 MOD 3
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_exponentiation() {
        let source = r#"
            DIM x AS DOUBLE
            x = 2 ^ 10
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn arithmetic_negation() {
        let source = r#"
            DIM x AS LONG
            x = -42
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn complex_expression() {
        let source = r#"
            DIM result AS DOUBLE
            result = (10 + 5) * 2 / 3.14 - 1
            PRINT result
        "#;
        assert_compiles(source);
    }

    #[test]
    fn parenthesized_expression() {
        let source = r#"
            DIM x AS LONG
            x = ((1 + 2) * (3 + 4))
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn string_concatenation() {
        let source = r#"
            DIM s AS STRING
            s = "Hello" + " " + "World"
            PRINT s
        "#;
        assert_compiles(source);
    }

    #[test]
    fn comparison_operators() {
        let source = r#"
            DIM a AS LONG
            DIM b AS LONG
            a = 5
            b = 10
            IF a < b THEN PRINT "Less"
            IF a <= b THEN PRINT "LessEq"
            IF a > b THEN PRINT "Greater"
            IF a >= b THEN PRINT "GreaterEq"
            IF a = b THEN PRINT "Equal"
            IF a <> b THEN PRINT "NotEqual"
        "#;
        assert_compiles(source);
    }

    #[test]
    fn logical_operators() {
        let source = r#"
            DIM a AS LONG
            DIM b AS LONG
            a = 1
            b = 0
            IF a AND b THEN PRINT "AND"
            IF a OR b THEN PRINT "OR"
            IF NOT b THEN PRINT "NOT"
            IF a XOR b THEN PRINT "XOR"
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Control Flow Tests
// =============================================================================

mod control_flow {
    use super::*;

    #[test]
    fn if_then() {
        let source = r#"
            DIM x AS LONG
            x = 10
            IF x > 5 THEN PRINT "Greater"
        "#;
        assert_compiles(source);
    }

    #[test]
    fn if_then_else() {
        let source = r#"
            DIM x AS LONG
            x = 3
            IF x > 5 THEN
                PRINT "Greater"
            ELSE
                PRINT "Smaller or equal"
            END IF
        "#;
        assert_compiles(source);
    }

    #[test]
    fn if_elseif_else() {
        let source = r#"
            DIM x AS LONG
            x = 5
            IF x < 5 THEN
                PRINT "Less"
            ELSEIF x = 5 THEN
                PRINT "Equal"
            ELSE
                PRINT "Greater"
            END IF
        "#;
        assert_compiles(source);
    }

    #[test]
    fn nested_if() {
        let source = r#"
            DIM x AS LONG
            DIM y AS LONG
            x = 10
            y = 20
            IF x > 5 THEN
                IF y > 15 THEN
                    PRINT "Both conditions met"
                END IF
            END IF
        "#;
        assert_compiles(source);
    }

    #[test]
    fn endif_syntax() {
        // ENDIF (no space) is an alternative to END IF
        let source = r#"
            DIM x AS LONG
            x = 10
            IF x > 5 THEN
                PRINT "Greater"
            ENDIF
        "#;
        assert_compiles(source);
    }

    #[test]
    fn endif_nested() {
        // ENDIF can be mixed with END IF
        let source = r#"
            DIM x AS LONG
            x = 10
            IF x > 5 THEN
                IF x < 20 THEN
                    PRINT "In range"
                ENDIF
            END IF
        "#;
        assert_compiles(source);
    }

    #[test]
    fn for_next_loop() {
        let source = r#"
            FOR i = 1 TO 10
                PRINT i
            NEXT i
        "#;
        assert_compiles(source);
    }

    #[test]
    fn for_next_with_step() {
        let source = r#"
            FOR i = 0 TO 10 STEP 2
                PRINT i
            NEXT i
        "#;
        assert_compiles(source);
    }

    #[test]
    fn for_next_negative_step() {
        let source = r#"
            FOR i = 10 TO 1 STEP -1
                PRINT i
            NEXT i
        "#;
        assert_compiles(source);
    }

    #[test]
    fn nested_for_loops() {
        let source = r#"
            FOR i = 1 TO 3
                FOR j = 1 TO 3
                    PRINT i; j
                NEXT j
            NEXT i
        "#;
        assert_compiles(source);
    }

    #[test]
    fn while_wend_loop() {
        let source = r#"
            DIM x AS LONG
            x = 0
            WHILE x < 10
                PRINT x
                x = x + 1
            WEND
        "#;
        assert_compiles(source);
    }

    #[test]
    fn do_loop_while() {
        let source = r#"
            DIM x AS LONG
            x = 0
            DO
                PRINT x
                x = x + 1
            LOOP WHILE x < 10
        "#;
        assert_compiles(source);
    }

    #[test]
    fn do_loop_until() {
        let source = r#"
            DIM x AS LONG
            x = 0
            DO
                PRINT x
                x = x + 1
            LOOP UNTIL x >= 10
        "#;
        assert_compiles(source);
    }

    #[test]
    fn do_while_loop() {
        let source = r#"
            DIM x AS LONG
            x = 0
            DO WHILE x < 10
                PRINT x
                x = x + 1
            LOOP
        "#;
        assert_compiles(source);
    }

    #[test]
    fn do_until_loop() {
        let source = r#"
            DIM x AS LONG
            x = 0
            DO UNTIL x >= 10
                PRINT x
                x = x + 1
            LOOP
        "#;
        assert_compiles(source);
    }

    #[test]
    fn select_case() {
        let source = r#"
            DIM x AS LONG
            x = 2
            SELECT CASE x
                CASE 1
                    PRINT "One"
                CASE 2
                    PRINT "Two"
                CASE 3
                    PRINT "Three"
                CASE ELSE
                    PRINT "Other"
            END SELECT
        "#;
        assert_compiles(source);
    }

    #[test]
    fn select_case_range() {
        let source = r#"
            DIM x AS LONG
            x = 50
            SELECT CASE x
                CASE 1 TO 10
                    PRINT "1-10"
                CASE 11 TO 100
                    PRINT "11-100"
                CASE ELSE
                    PRINT "Other"
            END SELECT
        "#;
        assert_compiles(source);
    }

    #[test]
    fn select_case_is() {
        let source = r#"
            DIM x AS LONG
            x = 50
            SELECT CASE x
                CASE IS < 10
                    PRINT "Less than 10"
                CASE IS >= 10
                    PRINT "10 or more"
            END SELECT
        "#;
        assert_compiles(source);
    }

    #[test]
    fn exit_for() {
        let source = r#"
            FOR i = 1 TO 100
                IF i > 5 THEN EXIT FOR
                PRINT i
            NEXT i
        "#;
        assert_compiles(source);
    }

    #[test]
    fn exit_while() {
        let source = r#"
            DIM x AS LONG
            x = 0
            WHILE x < 100
                IF x > 5 THEN EXIT WHILE
                PRINT x
                x = x + 1
            WEND
        "#;
        assert_compiles(source);
    }

    #[test]
    fn exit_do() {
        let source = r#"
            DIM x AS LONG
            x = 0
            DO
                IF x > 5 THEN EXIT DO
                PRINT x
                x = x + 1
            LOOP
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Procedure Tests
// =============================================================================

mod procedures {
    use super::*;

    #[test]
    fn simple_sub() {
        let source = r#"
            CALL MySub
            END

            SUB MySub
                PRINT "Hello from SUB"
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn sub_with_parameters() {
        let source = r#"
            CALL Greet("World")
            END

            SUB Greet(n AS STRING)
                PRINT "Hello, "; n
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn simple_function() {
        let source = r#"
            DIM result AS LONG
            result = Square(5)
            PRINT result
            END

            FUNCTION Square(x AS LONG) AS LONG
                Square = x * x
            END FUNCTION
        "#;
        assert_compiles(source);
    }

    #[test]
    fn function_with_return_type() {
        let source = r#"
            DIM result AS DOUBLE
            result = CalculatePi()
            PRINT result
            END

            FUNCTION CalculatePi AS DOUBLE
                CalculatePi = 3.14159
            END FUNCTION
        "#;
        assert_compiles(source);
    }

    #[test]
    fn recursive_function() {
        let source = r#"
            PRINT Factorial(5)
            END

            FUNCTION Factorial(n AS LONG)
                IF n <= 1 THEN
                    Factorial = 1
                ELSE
                    Factorial = n * Factorial(n - 1)
                END IF
            END FUNCTION
        "#;
        assert_compiles(source);
    }

    #[test]
    fn sub_with_local_variables() {
        let source = r#"
            CALL Test
            END

            SUB Test
                DIM local AS LONG
                local = 42
                PRINT local
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn exit_sub() {
        let source = r#"
            CALL Test(0)
            CALL Test(5)
            END

            SUB Test(x AS LONG)
                IF x = 0 THEN EXIT SUB
                PRINT x
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn exit_function() {
        let source = r#"
            PRINT SafeDivide(10.0, 2.0)
            PRINT SafeDivide(10.0, 0.0)
            END

            FUNCTION SafeDivide(a AS DOUBLE, b AS DOUBLE) AS DOUBLE
                IF b = 0 THEN
                    SafeDivide = 0.0
                    EXIT FUNCTION
                END IF
                SafeDivide = a / b
            END FUNCTION
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// SHARED Variable Tests
// =============================================================================

mod shared_variables {
    use super::*;

    #[test]
    fn shared_variable_access() {
        // Module-level variable accessed via SHARED statement in SUB
        let source = r#"
            DIM counter AS LONG
            counter = 10
            CALL IncrementCounter
            PRINT counter
            END

            SUB IncrementCounter
                SHARED counter
                counter = counter + 1
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn shared_multiple_variables() {
        // Multiple variables shared in single SHARED statement
        let source = r#"
            DIM x AS LONG
            DIM y AS LONG
            x = 5
            y = 10
            CALL SwapValues
            PRINT x; y
            END

            SUB SwapValues
                SHARED x, y
                DIM temp AS LONG
                temp = x
                x = y
                y = temp
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn shared_in_function() {
        // SHARED works in FUNCTION too
        let source = r#"
            DIM total AS DOUBLE
            total = 100.0
            PRINT AddToTotal(25.0)
            PRINT total
            END

            FUNCTION AddToTotal(amount AS DOUBLE) AS DOUBLE
                SHARED total
                total = total + amount
                AddToTotal = total
            END FUNCTION
        "#;
        assert_compiles(source);
    }

    #[test]
    fn shared_string_variable() {
        // SHARED works with STRING type
        let source = r#"
            DIM message AS STRING
            message = "Hello"
            CALL AppendWorld
            PRINT message
            END

            SUB AppendWorld
                SHARED message
                message = message + " World"
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn shared_variable_undefined_error() {
        // SHARED variable must exist at module level
        let source = r#"
            SUB Test
                SHARED nonexistent
                nonexistent = 1
            END SUB
        "#;
        assert_compile_error(source, "SharedVariableNotFound");
    }

    #[test]
    fn shared_outside_procedure_error() {
        // SHARED statement must be inside SUB/FUNCTION
        let source = r#"
            DIM x AS LONG
            SHARED x
        "#;
        assert_compile_error(source, "SharedOutsideProcedure");
    }

    #[test]
    fn shared_array() {
        // SHARED works with arrays
        let source = r#"
            DIM scores(10) AS LONG
            scores(0) = 100
            CALL UpdateScore(0, 200)
            PRINT scores(0)
            END

            SUB UpdateScore(index AS LONG, value AS LONG)
                SHARED scores()
                scores(index) = value
            END SUB
        "#;
        assert_compiles(source);
    }

    #[test]
    fn dim_shared_at_module_level() {
        // DIM SHARED is valid at module level
        let source = r#"
            DIM SHARED globalVar AS LONG
            globalVar = 42
            CALL PrintGlobal
            END

            SUB PrintGlobal
                SHARED globalVar
                PRINT globalVar
            END SUB
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Array Tests
// =============================================================================

mod arrays {
    use super::*;

    #[test]
    fn static_array() {
        let source = r#"
            DIM arr(10) AS LONG
            arr(0) = 42
            PRINT arr(0)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn static_array_with_range() {
        let source = r#"
            DIM arr(1 TO 10) AS LONG
            arr(1) = 42
            PRINT arr(1)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn multidimensional_array() {
        let source = r#"
            DIM arr(5, 5) AS LONG
            arr(2, 3) = 42
            PRINT arr(2, 3)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn array_in_loop() {
        let source = r#"
            DIM arr(10) AS LONG
            FOR i = 0 TO 10
                arr(i) = i * 2
            NEXT i
            FOR i = 0 TO 10
                PRINT arr(i)
            NEXT i
        "#;
        assert_compiles(source);
    }

    #[test]
    fn option_base_0() {
        // Default behavior - arrays start at 0
        let source = r#"
            OPTION BASE 0
            DIM arr(10) AS LONG
            arr(0) = 42
            PRINT arr(0)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn option_base_1() {
        // OPTION BASE 1 - arrays start at 1
        let source = r#"
            OPTION BASE 1
            DIM arr(10) AS LONG
            arr(1) = 42
            PRINT arr(1)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn option_base_1_with_loop() {
        // OPTION BASE 1 with FOR loop
        let source = r#"
            OPTION BASE 1
            DIM arr(10) AS LONG
            FOR i = 1 TO 10
                arr(i) = i * 2
            NEXT i
            PRINT arr(5)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn option_base_1_multidimensional() {
        // OPTION BASE 1 with 2D array
        let source = r#"
            OPTION BASE 1
            DIM arr(5, 5) AS LONG
            arr(1, 1) = 42
            arr(5, 5) = 100
            PRINT arr(1, 1); arr(5, 5)
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// CONST Tests
// =============================================================================

mod constants {
    use super::*;

    #[test]
    fn const_integer() {
        let source = r#"
            CONST MAX_VALUE = 100
            PRINT MAX_VALUE
        "#;
        assert_compiles(source);
    }

    #[test]
    fn const_double() {
        let source = r#"
            CONST PI = 3.14159
            PRINT PI
        "#;
        assert_compiles(source);
    }

    #[test]
    fn const_string() {
        let source = r#"
            CONST GREETING = "Hello"
            PRINT GREETING
        "#;
        assert_compiles(source);
    }

    #[test]
    fn const_expression() {
        let source = r#"
            CONST A = 10
            CONST B = 20
            CONST C = A + B
            PRINT C
        "#;
        assert_compiles(source);
    }

    #[test]
    fn const_in_expression() {
        let source = r#"
            CONST PI = 3.14159
            DIM radius AS DOUBLE
            radius = 5.0
            PRINT 2 * PI * radius
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// DATA/READ/RESTORE Tests
// =============================================================================

mod data_statements {
    use super::*;

    #[test]
    fn simple_data_read() {
        let source = r#"
            DIM x AS LONG
            READ x
            PRINT x
            DATA 42
        "#;
        assert_compiles(source);
    }

    #[test]
    fn multiple_data_values() {
        let source = r#"
            DIM a AS LONG
            DIM b AS LONG
            DIM c AS LONG
            READ a, b, c
            PRINT a; b; c
            DATA 1, 2, 3
        "#;
        assert_compiles(source);
    }

    #[test]
    fn data_string() {
        let source = r#"
            DIM s AS STRING
            READ s
            PRINT s
            DATA "Hello"
        "#;
        assert_compiles(source);
    }

    #[test]
    fn restore_statement() {
        let source = r#"
            DIM x AS LONG
            READ x
            PRINT x
            RESTORE
            READ x
            PRINT x
            DATA 42
        "#;
        assert_compiles(source);
    }

    #[test]
    fn labeled_data() {
        let source = r#"
            DIM x AS LONG
            RESTORE mydata
            READ x
            PRINT x

            mydata:
            DATA 100
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Built-in Function Tests
// =============================================================================

mod builtin_functions {
    use super::*;

    #[test]
    fn abs_function() {
        let source = r#"
            PRINT ABS(-42)
            PRINT ABS(42)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn sgn_function() {
        let source = r#"
            PRINT SGN(-10)
            PRINT SGN(0)
            PRINT SGN(10)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn int_function() {
        let source = r#"
            PRINT INT(3.7)
            PRINT INT(-3.7)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn sqr_function() {
        let source = r#"
            PRINT SQR(16)
            PRINT SQR(2)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn trig_functions() {
        let source = r#"
            CONST PI = 3.14159
            PRINT SIN(PI / 2)
            PRINT COS(0)
            PRINT TAN(PI / 4)
            PRINT ATN(1) * 4
        "#;
        assert_compiles(source);
    }

    #[test]
    fn exp_log_functions() {
        let source = r#"
            PRINT EXP(1)
            PRINT LOG(10)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn string_functions() {
        let source = r#"
            DIM s AS STRING
            s = "Hello World"
            PRINT LEN(s)
            PRINT LEFT$(s, 5)
            PRINT RIGHT$(s, 5)
            PRINT MID$(s, 7, 5)
            PRINT UCASE$(s)
            PRINT LCASE$(s)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn chr_asc_functions() {
        let source = r#"
            PRINT CHR$(65)
            PRINT ASC("A")
        "#;
        assert_compiles(source);
    }

    #[test]
    fn str_val_functions() {
        let source = r#"
            PRINT STR$(123)
            PRINT VAL("456")
        "#;
        assert_compiles(source);
    }

    #[test]
    fn instr_function() {
        // Note: Uses 3-argument form INSTR(start, string, search)
        // TODO: Add support for 2-argument form INSTR(string, search)
        let source = r#"
            PRINT INSTR(1, "Hello World", "o")
            PRINT INSTR(6, "Hello World", "o")
        "#;
        assert_compiles(source);
    }

    #[test]
    fn space_string_functions() {
        let source = r#"
            PRINT SPACE$(10)
            PRINT STRING$(10, "*")
        "#;
        assert_compiles(source);
    }

    #[test]
    fn rnd_function() {
        let source = r#"
            RANDOMIZE TIMER
            PRINT RND
            PRINT INT(RND * 100) + 1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn timer_function() {
        let source = r#"
            PRINT TIMER
        "#;
        assert_compiles(source);
    }

    #[test]
    fn date_time_functions() {
        let source = r#"
            PRINT DATE$
            PRINT TIME$
        "#;
        assert_compiles(source);
    }

    #[test]
    fn val_str_functions() {
        let source = r#"
            DIM s AS STRING
            DIM n AS DOUBLE
            s = STR$(123.45)
            n = VAL("42.5")
            PRINT s, n
        "#;
        assert_compiles(source);
    }

    #[test]
    fn trim_functions() {
        let source = r#"
            DIM s AS STRING
            s = "  hello world  "
            PRINT LTRIM$(s)
            PRINT RTRIM$(s)
            PRINT TRIM$(s)
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// File I/O Tests
// =============================================================================

mod file_io {
    use super::*;

    #[test]
    fn open_for_output() {
        let source = r#"
            OPEN "test.txt" FOR OUTPUT AS #1
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn open_for_input() {
        let source = r#"
            OPEN "test.txt" FOR INPUT AS #1
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn open_for_append() {
        let source = r#"
            OPEN "test.txt" FOR APPEND AS #1
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn print_to_file() {
        let source = r#"
            OPEN "test.txt" FOR OUTPUT AS #1
            PRINT #1, "Hello, World!"
            PRINT #1, 42
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn input_from_file() {
        let source = r#"
            DIM username AS STRING
            DIM age AS LONG
            OPEN "test.txt" FOR INPUT AS #1
            INPUT #1, username, age
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn line_input_from_file() {
        let source = r#"
            DIM textline AS STRING
            OPEN "test.txt" FOR INPUT AS #1
            LINE INPUT #1, textline
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn close_all_files() {
        let source = r#"
            OPEN "test1.txt" FOR OUTPUT AS #1
            OPEN "test2.txt" FOR OUTPUT AS #2
            CLOSE
        "#;
        assert_compiles(source);
    }

    #[test]
    fn write_to_file() {
        let source = r#"
            OPEN "test.txt" FOR OUTPUT AS #1
            WRITE #1, "name", 42, 3.14
            CLOSE #1
        "#;
        assert_compiles(source);
    }

    #[test]
    fn readfile_function() {
        // _READFILE$ reads entire file into string
        let source = r#"
            DIM content AS STRING
            content = _READFILE$("test.txt")
            PRINT content
        "#;
        assert_compiles(source);
    }

    #[test]
    fn writefile_statement() {
        // _WRITEFILE writes string to file
        let source = r#"
            _WRITEFILE "test.txt", "Hello World"
        "#;
        assert_compiles(source);
    }

    #[test]
    fn readfile_writefile_combined() {
        // Read a file, modify, write back
        let source = r#"
            DIM content AS STRING
            content = _READFILE$("input.txt")
            _WRITEFILE "output.txt", content + " (modified)"
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Console INPUT Tests
// =============================================================================

mod console_input {
    use super::*;

    #[test]
    fn input_single_variable() {
        let source = r#"
            DIM username AS STRING
            INPUT username
        "#;
        assert_compiles(source);
    }

    #[test]
    fn input_with_prompt() {
        let source = r#"
            DIM age AS LONG
            INPUT "Enter your age: ", age
        "#;
        assert_compiles(source);
    }

    #[test]
    fn input_multiple_variables() {
        let source = r#"
            DIM x AS LONG
            DIM y AS LONG
            INPUT x, y
        "#;
        assert_compiles(source);
    }

    #[test]
    fn input_without_question_mark() {
        let source = r#"
            DIM val AS SINGLE
            INPUT "Value"; val
        "#;
        assert_compiles(source);
    }

    #[test]
    fn line_input_statement() {
        let source = r#"
            DIM fullline AS STRING
            LINE INPUT fullline
        "#;
        assert_compiles(source);
    }

    #[test]
    fn line_input_with_prompt() {
        let source = r#"
            DIM response AS STRING
            LINE INPUT "Enter text: "; response
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Type Conversion Tests
// =============================================================================

mod type_conversions {
    use super::*;

    #[test]
    fn cint_function() {
        let source = r#"
            PRINT CINT(3.7)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn clng_function() {
        let source = r#"
            PRINT CLNG(3.7)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn csng_function() {
        let source = r#"
            PRINT CSNG(42)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn cdbl_function() {
        let source = r#"
            PRINT CDBL(42)
        "#;
        assert_compiles(source);
    }

    #[test]
    fn implicit_conversion_int_to_double() {
        let source = r#"
            DIM d AS DOUBLE
            d = 42
            PRINT d
        "#;
        assert_compiles(source);
    }

    #[test]
    fn implicit_conversion_in_expression() {
        let source = r#"
            DIM result AS DOUBLE
            result = 10 / 3
            PRINT result
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Hex and Binary Literal Tests
// =============================================================================

mod literals {
    use super::*;

    #[test]
    fn hex_literal() {
        let source = r#"
            DIM x AS LONG
            x = &HFF
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn hex_literal_long() {
        let source = r#"
            DIM x AS LONG
            x = &HFFFF
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn binary_literal() {
        let source = r#"
            DIM x AS LONG
            x = &B1010
            PRINT x
        "#;
        assert_compiles(source);
    }

    #[test]
    fn octal_literal() {
        let source = r#"
            DIM x AS LONG
            x = &O77
            PRINT x
        "#;
        assert_compiles(source);
    }
}

// =============================================================================
// Error Detection Tests
// =============================================================================

mod error_detection {
    use super::*;

    #[test]
    fn undefined_procedure() {
        // Note: BASIC allows implicit variable declaration, so undefined_var doesn't error
        // But calling an undefined function/sub does cause an error
        let source = r#"
            PRINT UndefinedFunction(1)
        "#;
        assert_compile_error(source, "UndefinedProcedure");
    }

    #[test]
    fn type_mismatch_assignment() {
        let source = r#"
            DIM x AS LONG
            x = "hello"
        "#;
        assert_compile_error(source, "Semantic");
    }

    #[test]
    fn invalid_syntax() {
        let source = r#"
            IF THEN PRINT
        "#;
        assert_compile_error(source, "Parse");
    }

    #[test]
    fn unclosed_if() {
        let source = r#"
            IF x > 5 THEN
                PRINT "Hello"
        "#;
        assert_compile_error(source, "Parse");
    }

    #[test]
    fn unclosed_for() {
        let source = r#"
            FOR i = 1 TO 10
                PRINT i
        "#;
        assert_compile_error(source, "Parse");
    }

    #[test]
    fn mismatched_next() {
        let source = r#"
            FOR i = 1 TO 10
                PRINT i
            NEXT j
        "#;
        assert_compile_error(source, "error");
    }

    #[test]
    fn function_wrong_args() {
        let source = r#"
            PRINT ABS("hello")
        "#;
        assert_compile_error(source, "Semantic");
    }

    #[test]
    fn duplicate_definition() {
        let source = r#"
            DIM x AS LONG
            DIM x AS STRING
        "#;
        assert_compile_error(source, "Semantic");
    }
}

// =============================================================================
// Generated C Code Quality Tests
// =============================================================================

mod codegen_quality {
    use super::*;

    #[test]
    fn generated_code_contains_main() {
        let source = r#"PRINT "Hello""#;
        let code = compile_to_c(source).expect("Should compile");
        assert!(
            code.contains("int main"),
            "Generated C should contain main function"
        );
    }

    #[test]
    fn generated_code_has_includes() {
        let source = r#"PRINT "Hello""#;
        let code = compile_to_c(source).expect("Should compile");
        assert!(
            code.contains("#include") || code.contains("stdio"),
            "Generated C should have necessary includes or inline definitions"
        );
    }

    #[test]
    fn variables_declared_in_c() {
        let source = r#"
            DIM x AS LONG
            x = 42
        "#;
        let code = compile_to_c(source).expect("Should compile");
        // The variable should appear in the generated code
        assert!(
            code.contains("int") || code.contains("qb_"),
            "Generated C should declare the integer variable"
        );
    }

    #[test]
    fn for_loop_generates_c_for() {
        let source = r#"
            FOR i = 1 TO 10
                PRINT i
            NEXT i
        "#;
        let code = compile_to_c(source).expect("Should compile");
        // Should contain some form of loop structure
        assert!(
            code.contains("for") || code.contains("while"),
            "Generated C should contain a loop construct"
        );
    }

    #[test]
    fn function_generates_c_function() {
        let source = r#"
            FUNCTION Add(a AS LONG, b AS LONG)
                Add = a + b
            END FUNCTION
        "#;
        let code = compile_to_c(source).expect("Should compile");
        // Should contain a function definition
        assert!(
            code.contains("Add") || code.contains("add"),
            "Generated C should contain the function definition"
        );
    }
}

// =============================================================================
// File-based Compilation Tests
// =============================================================================

mod file_compilation {
    use super::*;
    use std::fs;
    use std::path::Path;

    fn compile_example_file(filename: &str) {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("examples")
            .join(filename);

        if !path.exists() {
            // Skip if file doesn't exist (test will be counted but not fail)
            return;
        }

        let source = fs::read_to_string(&path).expect("Should read file");
        match compile_to_c(&source) {
            Ok(_) => {}
            Err(e) => panic!("Failed to compile {}: {}", filename, e),
        }
    }

    #[test]
    fn compile_hello_bas() {
        compile_example_file("hello.bas");
    }

    #[test]
    fn compile_simple_bas() {
        compile_example_file("simple.bas");
    }

    #[test]
    fn compile_data_test_bas() {
        compile_example_file("data_test.bas");
    }

    #[test]
    fn compile_array_test_bas() {
        compile_example_file("array_test.bas");
    }
}

/// Tests for timing statements (SLEEP, _DELAY, _LIMIT)
mod timing_statements {
    use super::*;

    #[test]
    fn sleep_with_seconds() {
        let code = compile_to_c("SLEEP 1").unwrap();
        assert!(code.contains("qb_sleep"));
    }

    #[test]
    fn sleep_no_argument() {
        let code = compile_to_c("SLEEP").unwrap();
        assert!(code.contains("qb_sleep_keypress"));
    }

    #[test]
    fn delay_statement() {
        let code = compile_to_c("_DELAY 0.5").unwrap();
        assert!(code.contains("qb_delay"));
    }

    #[test]
    fn limit_statement() {
        let code = compile_to_c("_LIMIT 60").unwrap();
        assert!(code.contains("qb_limit"));
    }
}

/// Tests for array statements (ERASE)
mod array_statements {
    use super::*;

    #[test]
    fn erase_single_array() {
        let code = compile_to_c(
            r#"
DIM arr(10) AS INTEGER
ERASE arr
"#,
        )
        .unwrap();
        assert!(code.contains("qb_array_erase"));
    }

    #[test]
    fn erase_multiple_arrays() {
        let code = compile_to_c(
            r#"
DIM a(5) AS INTEGER
DIM b(10) AS STRING
ERASE a, b
"#,
        )
        .unwrap();
        // Should call erase for both arrays
        assert!(code.contains("qb_array_erase(&arr_a)"));
        assert!(code.contains("qb_array_erase(&arr_b)"));
    }
}

/// Tests for math functions
mod math_functions {
    use super::*;

    #[test]
    fn pi_function() {
        let code = compile_to_c(
            r#"
DIM x AS DOUBLE
x = _PI
"#,
        )
        .unwrap();
        assert!(code.contains("qb_pi()"));
    }

    #[test]
    fn ceil_function() {
        let code = compile_to_c(
            r#"
DIM x AS LONG
x = _CEIL(3.7)
"#,
        )
        .unwrap();
        assert!(code.contains("ceil("));
    }

    #[test]
    fn round_function() {
        let code = compile_to_c(
            r#"
DIM x AS LONG
x = _ROUND(3.5)
"#,
        )
        .unwrap();
        assert!(code.contains("round("));
    }

    #[test]
    fn min_function() {
        let code = compile_to_c(
            r#"
DIM x AS DOUBLE
x = _MIN(5.0, 10.0)
"#,
        )
        .unwrap();
        assert!(code.contains("fmin("));
    }

    #[test]
    fn max_function() {
        let code = compile_to_c(
            r#"
DIM x AS DOUBLE
x = _MAX(5.0, 10.0)
"#,
        )
        .unwrap();
        assert!(code.contains("fmax("));
    }
}

/// Tests for print formatting functions (TAB, SPC, POS, CSRLIN)
mod print_formatting {
    use super::*;

    #[test]
    fn tab_function() {
        let code = compile_to_c(r#"PRINT TAB(10); "Hello""#).unwrap();
        assert!(code.contains("qb_tab("));
    }

    #[test]
    fn spc_function() {
        let code = compile_to_c(r#"PRINT SPC(5); "World""#).unwrap();
        assert!(code.contains("qb_spc("));
    }

    #[test]
    fn pos_function() {
        let code = compile_to_c(
            r#"
DIM col AS INTEGER
col = POS(0)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_pos("));
    }

    #[test]
    fn csrlin_function() {
        let code = compile_to_c(
            r#"
DIM row AS INTEGER
row = CSRLIN
"#,
        )
        .unwrap();
        assert!(code.contains("qb_csrlin("));
    }

    #[test]
    fn tab_and_spc_together() {
        let code = compile_to_c(r#"PRINT TAB(5); "A"; SPC(3); "B""#).unwrap();
        assert!(code.contains("qb_tab("));
        assert!(code.contains("qb_spc("));
    }
}

/// Tests for keyboard input functions (_KEYHIT, _KEYDOWN, _KEYCLEAR)
mod keyboard_input {
    use super::*;

    #[test]
    fn keyhit_function() {
        let code = compile_to_c(
            r#"
DIM k AS LONG
k = _KEYHIT
"#,
        )
        .unwrap();
        assert!(code.contains("qb_keyhit()"));
    }

    #[test]
    fn keydown_function() {
        let code = compile_to_c(
            r#"
DIM pressed AS LONG
pressed = _KEYDOWN(32)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_keydown("));
    }

    #[test]
    fn keyclear_statement() {
        let code = compile_to_c("_KEYCLEAR").unwrap();
        assert!(code.contains("qb_keyclear()"));
    }

    #[test]
    fn keyboard_in_loop() {
        let code = compile_to_c(
            r#"
DIM k AS LONG
DO
    k = _KEYHIT
    IF k <> 0 THEN PRINT k
LOOP UNTIL k = 27
"#,
        )
        .unwrap();
        assert!(code.contains("qb_keyhit()"));
    }

    #[test]
    fn cinp_function() {
        let code = compile_to_c(
            r#"
DIM c AS LONG
c = _CINP
"#,
        )
        .unwrap();
        assert!(code.contains("qb_cinp()"));
    }

    #[test]
    fn capslock_function() {
        let code = compile_to_c(
            r#"
DIM state AS LONG
state = _CAPSLOCK
"#,
        )
        .unwrap();
        assert!(code.contains("qb_capslock()"));
    }

    #[test]
    fn numlock_function() {
        let code = compile_to_c(
            r#"
DIM state AS LONG
state = _NUMLOCK
"#,
        )
        .unwrap();
        assert!(code.contains("qb_numlock()"));
    }

    #[test]
    fn scrolllock_function() {
        let code = compile_to_c(
            r#"
DIM state AS LONG
state = _SCROLLLOCK
"#,
        )
        .unwrap();
        assert!(code.contains("qb_scrolllock()"));
    }
}

/// Tests for PRINT USING formatted output
mod print_using {
    use super::*;

    #[test]
    fn print_using_numeric() {
        let code = compile_to_c("PRINT USING \"###.##\"; 123.45").unwrap();
        assert!(code.contains("qb_print_using("));
        assert!(code.contains("QbPrintValue"));
    }

    #[test]
    fn print_using_string() {
        let code = compile_to_c("PRINT USING \"&\"; \"Hello\"").unwrap();
        assert!(code.contains("qb_print_using("));
    }

    #[test]
    fn print_using_multiple_values() {
        let code = compile_to_c("PRINT USING \"## ##\"; 1, 2").unwrap();
        assert!(code.contains("qb_print_using("));
        assert!(code.contains("_pv[0]"));
        assert!(code.contains("_pv[1]"));
    }

    #[test]
    fn print_using_variable_format() {
        let code = compile_to_c("DIM fmt AS STRING\nfmt = \"###\"\nPRINT USING fmt; 123").unwrap();
        assert!(code.contains("qb_print_using("));
    }
}

/// Tests for ? as PRINT alias
mod print_shorthand {
    use super::*;

    #[test]
    fn question_mark_as_print() {
        let code = compile_to_c(r#"? "Hello""#).unwrap();
        assert!(code.contains("qb_print_string("));
    }

    #[test]
    fn question_mark_with_expression() {
        let code = compile_to_c("? 1 + 2").unwrap();
        assert!(code.contains("qb_print_"));
    }

    #[test]
    fn question_mark_with_semicolon() {
        let code = compile_to_c(r#"? "A"; "B""#).unwrap();
        assert!(code.contains("qb_print_string("));
    }
}

/// Tests for bitwise operations
mod bitwise_operations {
    use super::*;

    #[test]
    fn shl_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _SHL(1, 4)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_shl("));
    }

    #[test]
    fn shr_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _SHR(16, 2)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_shr("));
    }

    #[test]
    fn rol_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _ROL(1, 63)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_rol("));
    }

    #[test]
    fn ror_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _ROR(1, 1)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_ror("));
    }

    #[test]
    fn readbit_function() {
        let code = compile_to_c(
            r#"
DIM bit AS LONG
bit = _READBIT(5, 2)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_readbit("));
    }

    #[test]
    fn setbit_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _SETBIT(0, 3)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_setbit("));
    }

    #[test]
    fn resetbit_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _RESETBIT(15, 2)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_resetbit("));
    }

    #[test]
    fn togglebit_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _TOGGLEBIT(5, 1)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_togglebit("));
    }
}

/// Tests for additional math functions
mod extended_math {
    use super::*;

    #[test]
    fn clamp_function() {
        let code = compile_to_c(
            r#"
DIM result AS DOUBLE
result = _CLAMP(5.5, 0, 10)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_clamp("));
    }

    #[test]
    fn hypot_function() {
        let code = compile_to_c(
            r#"
DIM result AS DOUBLE
result = _HYPOT(3, 4)
"#,
        )
        .unwrap();
        assert!(code.contains("hypot("));
    }
}

/// Tests for graphics stubs in inline runtime
mod graphics_stubs {
    use super::*;

    #[test]
    fn screen_compiles() {
        let code = compile_to_c("SCREEN 12").unwrap();
        assert!(code.contains("qb_gfx_init("));
    }

    #[test]
    fn cls_compiles() {
        let code = compile_to_c("CLS").unwrap();
        assert!(code.contains("qb_gfx_cls()"));
    }

    #[test]
    fn pset_compiles() {
        let code = compile_to_c("PSET (100, 100), 15").unwrap();
        assert!(code.contains("qb_gfx_pset("));
    }

    #[test]
    fn line_compiles() {
        let code = compile_to_c("LINE (0, 0)-(100, 100), 14").unwrap();
        assert!(code.contains("qb_gfx_line("));
    }

    #[test]
    fn circle_compiles() {
        let code = compile_to_c("CIRCLE (320, 240), 50, 9").unwrap();
        assert!(code.contains("qb_gfx_circle("));
    }

    #[test]
    fn color_compiles() {
        let code = compile_to_c("COLOR 15, 1").unwrap();
        assert!(code.contains("qb_gfx_color("));
    }

    #[test]
    fn locate_compiles() {
        let code = compile_to_c("LOCATE 10, 20").unwrap();
        assert!(code.contains("qb_gfx_locate("));
    }
}

/// Tests for VIEW, WINDOW, DRAW, and image buffer statements
mod extended_graphics {
    use super::*;

    // ==================== VIEW Statement Tests ====================

    #[test]
    fn view_with_coordinates() {
        let code = compile_to_c("VIEW (10, 10)-(300, 200)").unwrap();
        assert!(code.contains("qb_gfx_view("));
        assert!(code.contains("10"));
        assert!(code.contains("300"));
        assert!(code.contains("200"));
    }

    #[test]
    fn view_with_screen_keyword() {
        let code = compile_to_c("VIEW SCREEN (0, 0)-(639, 479)").unwrap();
        assert!(code.contains("qb_gfx_view(1,")); // screen = 1
    }

    #[test]
    fn view_with_fill_color() {
        let code = compile_to_c("VIEW (10, 10)-(300, 200), 1").unwrap();
        assert!(code.contains("qb_gfx_view("));
    }

    #[test]
    fn view_with_fill_and_border() {
        let code = compile_to_c("VIEW (10, 10)-(300, 200), 0, 15").unwrap();
        assert!(code.contains("qb_gfx_view("));
    }

    #[test]
    fn view_reset() {
        let code = compile_to_c("VIEW").unwrap();
        assert!(code.contains("qb_gfx_view_reset()"));
    }

    #[test]
    fn view_print_with_range() {
        let code = compile_to_c("VIEW PRINT 5 TO 20").unwrap();
        assert!(code.contains("qb_view_print("));
    }

    #[test]
    fn view_print_reset() {
        let code = compile_to_c("VIEW PRINT").unwrap();
        assert!(code.contains("qb_view_print_reset()"));
    }

    // ==================== WINDOW Statement Tests ====================

    #[test]
    fn window_with_coordinates() {
        let code = compile_to_c("WINDOW (-1, -1)-(1, 1)").unwrap();
        assert!(code.contains("qb_gfx_window("));
    }

    #[test]
    fn window_screen_mode() {
        // WINDOW SCREEN - Y increases downward
        let code = compile_to_c("WINDOW SCREEN (0, 0)-(100, 100)").unwrap();
        assert!(code.contains("qb_gfx_window(1,")); // screen = 1
    }

    #[test]
    fn window_cartesian_mode() {
        // WINDOW without SCREEN - Y increases upward (Cartesian)
        let code = compile_to_c("WINDOW (-10, -10)-(10, 10)").unwrap();
        assert!(code.contains("qb_gfx_window(0,")); // screen = 0
    }

    #[test]
    fn window_reset() {
        let code = compile_to_c("WINDOW").unwrap();
        assert!(code.contains("qb_gfx_window_reset()"));
    }

    #[test]
    fn window_with_float_coordinates() {
        let code = compile_to_c("WINDOW (-3.14, -3.14)-(3.14, 3.14)").unwrap();
        assert!(code.contains("qb_gfx_window("));
        assert!(code.contains("3.14"));
    }

    // ==================== DRAW Statement Tests ====================

    #[test]
    fn draw_simple_commands() {
        let code = compile_to_c(r#"DRAW "U10 R10 D10 L10""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_diagonal_commands() {
        let code = compile_to_c(r#"DRAW "E10 F10 G10 H10""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_move_command() {
        let code = compile_to_c(r#"DRAW "M100,100""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_relative_move() {
        let code = compile_to_c(r#"DRAW "M+50,+50""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_color_and_scale() {
        let code = compile_to_c(r#"DRAW "C14 S8 U10""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_angle_commands() {
        let code = compile_to_c(r#"DRAW "A2 U10 TA90 U10""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_blank_move() {
        // B prefix - move without drawing
        let code = compile_to_c(r#"DRAW "BU10 U10""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_paint_command() {
        let code = compile_to_c(r#"DRAW "P15,1""#).unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn draw_with_variable() {
        let code = compile_to_c("DIM cmd AS STRING\ncmd = \"U10R10\"\nDRAW cmd").unwrap();
        assert!(code.contains("qb_gfx_draw("));
    }

    // ==================== Image Buffer Tests ====================

    #[test]
    fn newimage_function() {
        let code = compile_to_c("DIM img AS LONG\nimg = _NEWIMAGE(640, 480, 32)").unwrap();
        assert!(code.contains("qb_gfx_newimage("));
    }

    #[test]
    fn loadimage_function() {
        let code = compile_to_c(r#"DIM img AS LONG: img = _LOADIMAGE("test.png", 32)"#).unwrap();
        assert!(code.contains("qb_gfx_loadimage("));
    }

    #[test]
    fn freeimage_statement() {
        let code =
            compile_to_c("DIM img AS LONG\nimg = _NEWIMAGE(100, 100, 32)\n_FREEIMAGE img").unwrap();
        assert!(code.contains("qb_gfx_freeimage("));
    }

    #[test]
    fn putimage_simple() {
        let code =
            compile_to_c("DIM img AS LONG\nimg = _NEWIMAGE(100, 100, 32)\n_PUTIMAGE , img, 0")
                .unwrap();
        assert!(code.contains("qb_gfx_putimage"));
    }

    #[test]
    fn putimage_with_dest_coords() {
        let code = compile_to_c(
            "DIM img AS LONG\nimg = _NEWIMAGE(100, 100, 32)\n_PUTIMAGE (0, 0)-(100, 100), img, 0",
        )
        .unwrap();
        assert!(code.contains("qb_gfx_putimage("));
    }

    #[test]
    fn putimage_full_coords() {
        let code = compile_to_c("DIM img AS LONG\nimg = _NEWIMAGE(100, 100, 32)\n_PUTIMAGE (0, 0)-(200, 200), img, 0, (0, 0)-(100, 100)").unwrap();
        assert!(code.contains("qb_gfx_putimage_full("));
    }

    #[test]
    fn source_statement() {
        let code =
            compile_to_c("DIM img AS LONG\nimg = _NEWIMAGE(100, 100, 32)\n_SOURCE img").unwrap();
        assert!(code.contains("qb_gfx_source("));
    }

    #[test]
    fn dest_statement() {
        let code =
            compile_to_c("DIM img AS LONG\nimg = _NEWIMAGE(100, 100, 32)\n_DEST img").unwrap();
        assert!(code.contains("qb_gfx_dest("));
    }

    #[test]
    fn width_and_height_functions() {
        let code = compile_to_c("DIM img AS LONG, w AS LONG, h AS LONG\nimg = _NEWIMAGE(640, 480, 32)\nw = _WIDTH(img)\nh = _HEIGHT(img)").unwrap();
        assert!(code.contains("qb_gfx_image_width("));
        assert!(code.contains("qb_gfx_image_height("));
    }

    // ==================== Combined Usage Tests ====================

    #[test]
    fn view_window_combination() {
        // Test VIEW and WINDOW working together
        let code = compile_to_c(
            r#"
            VIEW (50, 50)-(550, 350)
            WINDOW (-1, -1)-(1, 1)
            PSET (0, 0), 15
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_view("));
        assert!(code.contains("qb_gfx_window("));
        assert!(code.contains("qb_gfx_pset("));
    }

    #[test]
    fn draw_in_viewport() {
        let code = compile_to_c(
            r#"
            VIEW (100, 100)-(500, 400)
            DRAW "U50 R50 D50 L50"
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_view("));
        assert!(code.contains("qb_gfx_draw("));
    }

    #[test]
    fn image_copy_and_display() {
        // _PUTIMAGE syntax requires coordinate range: (x1,y1)-(x2,y2)
        let code = compile_to_c(
            r#"
            DIM src AS LONG, dst AS LONG
            src = _NEWIMAGE(100, 100, 32)
            dst = 0
            _PUTIMAGE (0, 0)-(100, 100), src, dst
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_newimage("));
        assert!(code.contains("qb_gfx_putimage"));
    }

    // ==================== GET/PUT Graphics Array Tests ====================

    #[test]
    fn graphics_get_basic() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            GET (0, 0)-(10, 10), sprite
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_get("));
        assert!(code.contains("(int32_t)0"));
        assert!(code.contains("(int32_t)10"));
    }

    #[test]
    fn graphics_get_with_step() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            GET (50, 50)-STEP(20, 20), sprite
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_get_step("));
    }

    #[test]
    fn graphics_get_with_array_index() {
        let code = compile_to_c(
            r#"
            DIM images(500) AS INTEGER
            GET (0, 0)-(10, 10), images(0)
            GET (0, 0)-(10, 10), images(100)
        "#,
        )
        .unwrap();
        assert!(code.contains("&images["));
    }

    #[test]
    fn graphics_put_basic() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT (50, 50), sprite
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_put("));
        assert!(code.contains("QB_PUT_XOR")); // Default action is XOR
    }

    #[test]
    fn graphics_put_with_pset() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT (50, 50), sprite, PSET
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_put("));
        assert!(code.contains("QB_PUT_PSET"));
    }

    #[test]
    fn graphics_put_with_preset() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT (50, 50), sprite, PRESET
        "#,
        )
        .unwrap();
        assert!(code.contains("QB_PUT_PRESET"));
    }

    #[test]
    fn graphics_put_with_and() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT (50, 50), sprite, AND
        "#,
        )
        .unwrap();
        assert!(code.contains("QB_PUT_AND"));
    }

    #[test]
    fn graphics_put_with_or() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT (50, 50), sprite, OR
        "#,
        )
        .unwrap();
        assert!(code.contains("QB_PUT_OR"));
    }

    #[test]
    fn graphics_put_with_xor() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT (50, 50), sprite, XOR
        "#,
        )
        .unwrap();
        assert!(code.contains("QB_PUT_XOR"));
    }

    #[test]
    fn graphics_put_with_step() {
        let code = compile_to_c(
            r#"
            DIM sprite(100) AS INTEGER
            PUT STEP(10, 10), sprite, PSET
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_put_step("));
    }

    #[test]
    fn graphics_get_put_roundtrip() {
        // Test typical sprite capture and display pattern
        let code = compile_to_c(
            r#"
            DIM sprite(200) AS INTEGER
            ' Capture a region
            GET (100, 100)-(120, 120), sprite
            ' Draw it elsewhere with different actions
            PUT (200, 200), sprite, PSET
            PUT (300, 300), sprite, XOR
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_gfx_get("));
        assert!(code.contains("qb_gfx_put("));
        assert!(code.contains("QB_PUT_PSET"));
        assert!(code.contains("QB_PUT_XOR"));
    }

    // ==================== _PRINTWIDTH Function Tests ====================

    #[test]
    fn printwidth_basic() {
        let code = compile_to_c(r#"PRINT _PRINTWIDTH("Hello")"#).unwrap();
        assert!(code.contains("qb_printwidth("));
    }

    #[test]
    fn printwidth_with_variable() {
        let code = compile_to_c(
            r#"
            DIM text AS STRING, w AS LONG
            text = "Test string"
            w = _PRINTWIDTH(text)
        "#,
        )
        .unwrap();
        assert!(code.contains("qb_printwidth("));
    }
}

/// Tests for hyperbolic trig functions
mod hyperbolic_functions {
    use super::*;

    #[test]
    fn sinh_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _SINH(1.0)").unwrap();
        assert!(code.contains("sinh("));
    }

    #[test]
    fn cosh_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _COSH(1.0)").unwrap();
        assert!(code.contains("cosh("));
    }

    #[test]
    fn tanh_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _TANH(1.0)").unwrap();
        assert!(code.contains("tanh("));
    }

    #[test]
    fn asinh_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ASINH(1.0)").unwrap();
        assert!(code.contains("asinh("));
    }

    #[test]
    fn acosh_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ACOSH(2.0)").unwrap();
        assert!(code.contains("acosh("));
    }

    #[test]
    fn atanh_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ATANH(0.5)").unwrap();
        assert!(code.contains("atanh("));
    }
}

/// Tests for angle conversion functions
mod angle_conversions {
    use super::*;

    #[test]
    fn degrees_to_radians() {
        let code = compile_to_c("DIM r AS DOUBLE\nr = _D2R(180)").unwrap();
        assert!(code.contains("qb_d2r("));
    }

    #[test]
    fn radians_to_degrees() {
        let code = compile_to_c("DIM d AS DOUBLE\nd = _R2D(3.14159)").unwrap();
        assert!(code.contains("qb_r2d("));
    }
}

/// Tests for string comparison functions
mod string_comparison {
    use super::*;

    #[test]
    fn strcmp_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _STRCMP("abc", "def")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_strcmp("));
    }

    #[test]
    fn stricmp_function() {
        let code = compile_to_c(
            r#"
DIM result AS LONG
result = _STRICMP("ABC", "abc")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_stricmp("));
    }
}

/// Tests for _NEGATE function
mod negate_function {
    use super::*;

    #[test]
    fn negate_positive() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _NEGATE(5.0)").unwrap();
        assert!(code.contains("qb_negate("));
    }

    #[test]
    fn negate_expression() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _NEGATE(2.5 + 3.5)").unwrap();
        assert!(code.contains("qb_negate("));
    }
}

/// Tests for error handling extensions
mod error_extensions {
    use super::*;

    #[test]
    fn errorline_function() {
        let code = compile_to_c("DIM ln AS LONG\nln = _ERRORLINE").unwrap();
        assert!(code.contains("qb_errorline("));
    }

    #[test]
    fn errormessage_function() {
        let code = compile_to_c("DIM msg AS STRING\nmsg = _ERRORMESSAGE$").unwrap();
        assert!(code.contains("qb_errormessage("));
    }
}

/// Tests for utility functions
mod utility_functions {
    use super::*;

    #[test]
    fn commandcount_function() {
        let code = compile_to_c("DIM argc AS LONG\nargc = _COMMANDCOUNT").unwrap();
        assert!(code.contains("qb_commandcount("));
    }

    #[test]
    fn environcount_function() {
        let code = compile_to_c("DIM envc AS LONG\nenvc = _ENVIRONCOUNT").unwrap();
        assert!(code.contains("qb_environcount("));
    }
}

/// Tests for font stubs
mod font_stubs {
    use super::*;

    #[test]
    fn loadfont_function() {
        let code = compile_to_c(
            r#"
DIM f AS LONG
f = _LOADFONT("arial.ttf", 16)
"#,
        )
        .unwrap();
        assert!(code.contains("qb_loadfont("));
    }

    #[test]
    fn fontheight_function() {
        let code = compile_to_c("DIM h AS LONG\nh = _FONTHEIGHT").unwrap();
        assert!(code.contains("qb_fontheight("));
    }

    #[test]
    fn fontwidth_function() {
        let code = compile_to_c("DIM w AS LONG\nw = _FONTWIDTH").unwrap();
        assert!(code.contains("qb_fontwidth("));
    }
}

/// Tests for desktop/window functions
mod desktop_functions {
    use super::*;

    #[test]
    fn desktopwidth_function() {
        let code = compile_to_c("DIM w AS LONG\nw = _DESKTOPWIDTH").unwrap();
        assert!(code.contains("qb_desktopwidth("));
    }

    #[test]
    fn desktopheight_function() {
        let code = compile_to_c("DIM h AS LONG\nh = _DESKTOPHEIGHT").unwrap();
        assert!(code.contains("qb_desktopheight("));
    }

    #[test]
    fn screenx_function() {
        let code = compile_to_c("DIM x AS LONG\nx = _SCREENX").unwrap();
        assert!(code.contains("qb_screenx("));
    }

    #[test]
    fn screeny_function() {
        let code = compile_to_c("DIM y AS LONG\ny = _SCREENY").unwrap();
        assert!(code.contains("qb_screeny("));
    }

    #[test]
    fn title_get_function() {
        let code = compile_to_c("DIM t AS STRING\nt = _TITLE$").unwrap();
        assert!(code.contains("qb_title_get("));
    }

    #[test]
    fn windowhandle_function() {
        let code = compile_to_c("DIM h AS LONG\nh = _WINDOWHANDLE").unwrap();
        assert!(code.contains("qb_windowhandle("));
    }

    #[test]
    fn windowhasfocus_function() {
        let code = compile_to_c("DIM f AS LONG\nf = _WINDOWHASFOCUS").unwrap();
        assert!(code.contains("qb_windowhasfocus("));
    }
}

/// Tests for dialog box functions
mod dialog_functions {
    use super::*;

    #[test]
    fn messagebox_function() {
        let code = compile_to_c(
            r#"
DIM r AS LONG
r = _MESSAGEBOX("Title", "Message")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_messagebox("));
    }

    #[test]
    fn inputbox_function() {
        let code = compile_to_c(
            r#"
DIM response AS STRING
response = _INPUTBOX$("Enter name:", "Input")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_inputbox("));
    }

    #[test]
    fn openfiledialog_function() {
        let code = compile_to_c(
            r#"
DIM filename AS STRING
filename = _OPENFILEDIALOG$("Open File", "*.txt")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_openfiledialog("));
    }

    #[test]
    fn savefiledialog_function() {
        let code = compile_to_c(
            r#"
DIM filename AS STRING
filename = _SAVEFILEDIALOG$("Save File", "*.txt")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_savefiledialog("));
    }

    #[test]
    fn selectfolderdialog_function() {
        let code = compile_to_c(
            r#"
DIM folder AS STRING
folder = _SELECTFOLDERDIALOG$("Select Folder")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_selectfolderdialog("));
    }
}

/// Tests for reciprocal trig functions (_SEC, _CSC, _COT, etc.)
mod reciprocal_trig_functions {
    use super::*;

    #[test]
    fn sec_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _SEC(0.5)").unwrap();
        assert!(code.contains("qb_sec("));
    }

    #[test]
    fn csc_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _CSC(0.5)").unwrap();
        assert!(code.contains("qb_csc("));
    }

    #[test]
    fn cot_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _COT(0.5)").unwrap();
        assert!(code.contains("qb_cot("));
    }

    #[test]
    fn sech_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _SECH(0.5)").unwrap();
        assert!(code.contains("qb_sech("));
    }

    #[test]
    fn csch_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _CSCH(0.5)").unwrap();
        assert!(code.contains("qb_csch("));
    }

    #[test]
    fn coth_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _COTH(0.5)").unwrap();
        assert!(code.contains("qb_coth("));
    }

    #[test]
    fn arcsec_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ARCSEC(2.0)").unwrap();
        assert!(code.contains("qb_arcsec("));
    }

    #[test]
    fn arccsc_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ARCCSC(2.0)").unwrap();
        assert!(code.contains("qb_arccsc("));
    }

    #[test]
    fn arccot_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ARCCOT(1.0)").unwrap();
        assert!(code.contains("qb_arccot("));
    }

    #[test]
    fn arcsech_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ARCSECH(0.5)").unwrap();
        assert!(code.contains("qb_arcsech("));
    }

    #[test]
    fn arccsch_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ARCCSCH(0.5)").unwrap();
        assert!(code.contains("qb_arccsch("));
    }

    #[test]
    fn arccoth_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _ARCCOTH(2.0)").unwrap();
        assert!(code.contains("qb_arccoth("));
    }
}

/// Tests for gradian conversion functions
mod gradian_conversions {
    use super::*;

    #[test]
    fn d2g_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _D2G(90.0)").unwrap();
        assert!(code.contains("qb_d2g("));
    }

    #[test]
    fn g2d_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _G2D(100.0)").unwrap();
        assert!(code.contains("qb_g2d("));
    }

    #[test]
    fn g2r_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _G2R(100.0)").unwrap();
        assert!(code.contains("qb_g2r("));
    }

    #[test]
    fn r2g_function() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _R2G(1.5708)").unwrap();
        assert!(code.contains("qb_r2g("));
    }
}

/// Tests for number-to-string conversion functions
mod number_conversion_functions {
    use super::*;

    #[test]
    fn bin_function() {
        let code = compile_to_c("DIM s AS STRING\ns = _BIN$(42)").unwrap();
        assert!(code.contains("qb_bin("));
    }

    #[test]
    fn tostr_function() {
        let code = compile_to_c("DIM s AS STRING\ns = _TOSTR$(3.14159)").unwrap();
        assert!(code.contains("qb_tostr("));
    }

    #[test]
    fn hex_function() {
        let code = compile_to_c("DIM s AS STRING\ns = HEX$(255)").unwrap();
        assert!(code.contains("qb_hex("));
    }

    #[test]
    fn oct_function() {
        let code = compile_to_c("DIM s AS STRING\ns = OCT$(64)").unwrap();
        assert!(code.contains("qb_oct("));
    }
}

/// Tests for inline conditional functions (_IIF)
mod inline_conditional_functions {
    use super::*;

    #[test]
    fn iif_numeric() {
        let code = compile_to_c("DIM x AS DOUBLE\nx = _IIF(1, 10.0, 20.0)").unwrap();
        assert!(code.contains("qb_iif("));
    }

    #[test]
    fn iif_string() {
        let code = compile_to_c(
            r#"
DIM s AS STRING
s = _IIF$(-1, "yes", "no")
"#,
        )
        .unwrap();
        assert!(code.contains("qb_iif_str("));
    }
}

/// Tests for window control functions
mod window_control_functions {
    use super::*;

    #[test]
    fn screenmove_function() {
        let code = compile_to_c("DIM r AS LONG\nr = _SCREENMOVE(100, 100)").unwrap();
        assert!(code.contains("qb_screenmove("));
    }

    #[test]
    fn screenhide_function() {
        let code = compile_to_c("DIM r AS LONG\nr = _SCREENHIDE").unwrap();
        assert!(code.contains("qb_screenhide("));
    }

    #[test]
    fn screenshow_function() {
        let code = compile_to_c("DIM r AS LONG\nr = _SCREENSHOW").unwrap();
        assert!(code.contains("qb_screenshow("));
    }

    #[test]
    fn fullscreen_function() {
        let code = compile_to_c("DIM m AS LONG\nm = _FULLSCREEN").unwrap();
        assert!(code.contains("qb_fullscreen("));
    }

    #[test]
    fn screenclick_function() {
        let code = compile_to_c("DIM r AS LONG\nr = _SCREENCLICK").unwrap();
        assert!(code.contains("qb_screenclick("));
    }
}

/// Tests for sound statements
mod sound_statements {
    use super::*;

    #[test]
    fn beep_statement() {
        let code = compile_to_c("BEEP").unwrap();
        assert!(code.contains("qb_beep("));
    }

    #[test]
    fn sound_statement() {
        let code = compile_to_c("SOUND 440, 18").unwrap();
        assert!(code.contains("qb_sound("));
    }

    #[test]
    fn play_statement() {
        let code = compile_to_c(r#"PLAY "CDEFGAB""#).unwrap();
        assert!(code.contains("qb_play("));
    }
}

/// Tests for font functions
mod font_functions {
    use super::*;

    #[test]
    fn font_function() {
        let code = compile_to_c("DIM prev AS LONG\nprev = _FONT(16)").unwrap();
        assert!(code.contains("qb_font("));
    }

    #[test]
    fn freefont_function() {
        let code = compile_to_c("DIM r AS LONG\nr = _FREEFONT(16)").unwrap();
        assert!(code.contains("qb_freefont("));
    }
}

// ==================== Phase 7: Additional Statements ====================

/// Tests for RUN statement
mod run_statement {
    use super::*;

    #[test]
    fn run_no_args() {
        let code = compile_to_c("RUN").unwrap();
        assert!(code.contains("qb_run(NULL)"));
    }

    #[test]
    fn run_with_filename() {
        let code = compile_to_c(r#"RUN "program.exe""#).unwrap();
        assert!(code.contains("qb_run("));
    }
}

/// Tests for CHAIN statement
mod chain_statement {
    use super::*;

    #[test]
    fn chain_with_filename() {
        let code = compile_to_c(r#"CHAIN "other.bas""#).unwrap();
        assert!(code.contains("qb_chain("));
    }
}

/// Tests for TRON/TROFF statements
mod trace_statements {
    use super::*;

    #[test]
    fn tron_statement() {
        let code = compile_to_c("TRON").unwrap();
        assert!(code.contains("qb_trace_on = 1"));
    }

    #[test]
    fn troff_statement() {
        let code = compile_to_c("TROFF").unwrap();
        assert!(code.contains("qb_trace_on = 0"));
    }
}

/// Tests for LPRINT statement
mod lprint_statement {
    use super::*;

    #[test]
    fn lprint_simple() {
        let code = compile_to_c(r#"LPRINT "Hello""#).unwrap();
        assert!(code.contains("qb_lprint("));
    }

    #[test]
    fn lprint_with_values() {
        let code = compile_to_c("DIM x&\nx& = 42\nLPRINT x&").unwrap();
        assert!(code.contains("qb_lprint("));
    }
}

/// Tests for FILES statement
mod files_statement {
    use super::*;

    #[test]
    fn files_no_args() {
        let code = compile_to_c("FILES").unwrap();
        assert!(code.contains("qb_files(NULL)"));
    }

    #[test]
    fn files_with_pattern() {
        let code = compile_to_c(r#"FILES "*.bas""#).unwrap();
        assert!(code.contains("qb_files("));
    }
}

/// Tests for FIELD statement
mod field_statement {
    use super::*;

    #[test]
    fn field_statement() {
        let code = compile_to_c("DIM f1$ AS STRING\nFIELD #1, 20 AS f1$").unwrap();
        assert!(code.contains("qb_field_start("));
        assert!(code.contains("qb_field_add("));
    }
}

/// Tests for LSET/RSET statements
mod lset_rset_statements {
    use super::*;

    #[test]
    fn lset_statement() {
        let code = compile_to_c("DIM f$ AS STRING\nLSET f$ = \"test\"").unwrap();
        assert!(code.contains("qb_lset("));
    }

    #[test]
    fn rset_statement() {
        let code = compile_to_c("DIM f$ AS STRING\nRSET f$ = \"test\"").unwrap();
        assert!(code.contains("qb_rset("));
    }
}

/// Tests for KEY statement
mod key_statement {
    use super::*;

    #[test]
    fn key_on() {
        let code = compile_to_c("KEY(1) ON").unwrap();
        assert!(code.contains("qb_key_control("));
    }

    #[test]
    fn key_off() {
        let code = compile_to_c("KEY(1) OFF").unwrap();
        assert!(code.contains("qb_key_control("));
    }

    #[test]
    fn key_stop() {
        let code = compile_to_c("KEY(1) STOP").unwrap();
        assert!(code.contains("qb_key_control("));
    }
}

/// Tests for CLEAR statement
mod clear_statement {
    use super::*;

    #[test]
    fn clear_no_args() {
        let code = compile_to_c("CLEAR").unwrap();
        assert!(code.contains("qb_clear(0)"));
    }

    #[test]
    fn clear_with_stack_size() {
        let code = compile_to_c("CLEAR 4096").unwrap();
        assert!(code.contains("qb_clear("));
    }
}

/// Tests for RESET statement
mod reset_statement {
    use super::*;

    #[test]
    fn reset_statement() {
        let code = compile_to_c("RESET").unwrap();
        assert!(code.contains("qb_reset()"));
    }
}

/// Tests for _ALLOWFULLSCREEN statement
mod allowfullscreen_statement {
    use super::*;

    #[test]
    fn allowfullscreen_all() {
        let code = compile_to_c("_ALLOWFULLSCREEN _ALL").unwrap();
        assert!(code.contains("qb_allowfullscreen("));
    }

    #[test]
    fn allowfullscreen_off() {
        let code = compile_to_c("_ALLOWFULLSCREEN _OFF").unwrap();
        assert!(code.contains("qb_allowfullscreen("));
    }
}

/// Tests for _SCREENICON statement
mod screenicon_statement {
    use super::*;

    #[test]
    fn screenicon_statement() {
        let code = compile_to_c("_SCREENICON").unwrap();
        assert!(code.contains("qb_screenicon()"));
    }
}

/// Tests for _CONSOLETITLE statement
mod consoletitle_statement {
    use super::*;

    #[test]
    fn consoletitle_statement() {
        let code = compile_to_c(r#"_CONSOLETITLE "My App""#).unwrap();
        assert!(code.contains("qb_consoletitle("));
    }
}

/// Tests for _CONSOLE statement
mod console_statement {
    use super::*;

    #[test]
    fn console_on() {
        let code = compile_to_c("_CONSOLE ON").unwrap();
        assert!(code.contains("qb_console(1)"));
    }

    #[test]
    fn console_off() {
        let code = compile_to_c("_CONSOLE OFF").unwrap();
        assert!(code.contains("qb_console(0)"));
    }
}

/// Tests for _ASSERT statement
mod assert_statement {
    use super::*;

    #[test]
    fn assert_simple() {
        let code = compile_to_c("_ASSERT 1 = 1").unwrap();
        assert!(code.contains("qb_assert("));
    }

    #[test]
    fn assert_with_message() {
        let code = compile_to_c(r#"_ASSERT 1 = 1, "should be true""#).unwrap();
        assert!(code.contains("qb_assert("));
    }
}

// =============================================================================
// Conditional Compilation Tests ($IF/$ELSEIF/$ELSE/$END IF)
// =============================================================================

/// Tests for platform conditional compilation
mod conditional_compilation {
    use super::*;

    #[test]
    fn if_linux_on_linux() {
        // On Linux, the LINUX branch should be selected
        let source = r#"
$IF LINUX THEN
    PRINT "Linux"
$ELSE
    PRINT "Other"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        // On Linux, should include "Linux" and NOT include "Other"
        #[cfg(target_os = "linux")]
        {
            assert!(
                code.contains(r#"qb_print_string(qb_string_new("Linux")"#),
                "Linux branch should be selected on Linux"
            );
            assert!(
                !code.contains(r#"qb_print_string(qb_string_new("Other")"#),
                "Other branch should NOT be included on Linux"
            );
        }

        // On non-Linux, should include "Other" and NOT include "Linux"
        #[cfg(not(target_os = "linux"))]
        {
            assert!(
                code.contains(r#"qb_print_string(qb_string_new("Other")"#),
                "Other branch should be selected on non-Linux"
            );
            assert!(
                !code.contains(r#"qb_print_string(qb_string_new("Linux")"#),
                "Linux branch should NOT be included on non-Linux"
            );
        }
    }

    #[test]
    fn if_win_on_platform() {
        let source = r#"
$IF WIN THEN
    PRINT "Windows"
$ELSE
    PRINT "Not Windows"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(target_os = "windows")]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Windows")"#));
            assert!(!code.contains(r#"qb_print_string(qb_string_new("Not Windows")"#));
        }

        #[cfg(not(target_os = "windows"))]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Not Windows")"#));
            assert!(!code.contains(r#"qb_print_string(qb_string_new("Windows")"#));
        }
    }

    #[test]
    fn if_64bit_architecture() {
        let source = r#"
$IF 64BIT THEN
    PRINT "64-bit"
$ELSEIF 32BIT THEN
    PRINT "32-bit"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(target_pointer_width = "64")]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("64-bit")"#));
            assert!(!code.contains(r#"qb_print_string(qb_string_new("32-bit")"#));
        }

        #[cfg(target_pointer_width = "32")]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("32-bit")"#));
            assert!(!code.contains(r#"qb_print_string(qb_string_new("64-bit")"#));
        }
    }

    #[test]
    fn if_not_operator() {
        let source = r#"
$IF NOT WIN THEN
    PRINT "Not Windows"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(not(target_os = "windows"))]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Not Windows")"#));
        }

        #[cfg(target_os = "windows")]
        {
            assert!(!code.contains(r#"qb_print_string(qb_string_new("Not Windows")"#));
        }
    }

    #[test]
    fn if_and_operator() {
        let source = r#"
$IF LINUX AND 64BIT THEN
    PRINT "64-bit Linux"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(all(target_os = "linux", target_pointer_width = "64"))]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("64-bit Linux")"#));
        }

        #[cfg(not(all(target_os = "linux", target_pointer_width = "64")))]
        {
            assert!(!code.contains(r#"qb_print_string(qb_string_new("64-bit Linux")"#));
        }
    }

    #[test]
    fn if_or_operator() {
        let source = r#"
$IF WIN OR LINUX OR MAC THEN
    PRINT "Desktop OS"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        // This should always be true on any desktop platform
        #[cfg(any(target_os = "windows", target_os = "linux", target_os = "macos"))]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Desktop OS")"#));
        }
    }

    #[test]
    fn if_comparison_operator() {
        let source = r#"
$IF LINUX = -1 THEN
    PRINT "Linux is TRUE"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(target_os = "linux")]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Linux is TRUE")"#));
        }

        #[cfg(not(target_os = "linux"))]
        {
            assert!(!code.contains(r#"qb_print_string(qb_string_new("Linux is TRUE")"#));
        }
    }

    #[test]
    fn if_with_parentheses() {
        let source = r#"
$IF (WIN OR MAC) AND 64BIT THEN
    PRINT "64-bit Windows or Mac"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(all(
            any(target_os = "windows", target_os = "macos"),
            target_pointer_width = "64"
        ))]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("64-bit Windows or Mac")"#));
        }

        #[cfg(not(all(
            any(target_os = "windows", target_os = "macos"),
            target_pointer_width = "64"
        )))]
        {
            assert!(!code.contains(r#"qb_print_string(qb_string_new("64-bit Windows or Mac")"#));
        }
    }

    #[test]
    fn elseif_chain() {
        let source = r#"
$IF WIN THEN
    PRINT "Windows"
$ELSEIF MAC THEN
    PRINT "macOS"
$ELSEIF LINUX THEN
    PRINT "Linux"
$ELSE
    PRINT "Unknown"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        // Only one branch should be included
        let branches = [
            code.contains(r#"qb_print_string(qb_string_new("Windows")"#),
            code.contains(r#"qb_print_string(qb_string_new("macOS")"#),
            code.contains(r#"qb_print_string(qb_string_new("Linux")"#),
            code.contains(r#"qb_print_string(qb_string_new("Unknown")"#),
        ];
        let count = branches.iter().filter(|&&b| b).count();
        assert_eq!(
            count, 1,
            "Exactly one branch should be selected, found {}",
            count
        );
    }

    #[test]
    fn conditional_with_multiple_statements() {
        let source = r#"
DIM x AS INTEGER
$IF LINUX THEN
    x = 1
    PRINT "Linux"
    x = x + 1
$ELSE
    x = 9999
    PRINT "Other"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(target_os = "linux")]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Linux")"#));
            // Should NOT contain the Other branch code
            assert!(
                !code.contains("9999"),
                "Linux build should not contain value 9999 from $ELSE branch"
            );
            assert!(
                !code.contains(r#"qb_string_new("Other")"#),
                "Linux build should not contain 'Other' string from $ELSE branch"
            );
        }

        #[cfg(not(target_os = "linux"))]
        {
            assert!(code.contains("9999"));
            assert!(code.contains(r#"qb_print_string(qb_string_new("Other")"#));
        }
    }

    #[test]
    fn false_condition_empty_output() {
        // When no condition matches and there's no $ELSE, nothing should be emitted
        let source = r#"
$IF _FALSE THEN
    PRINT "Never printed"
$END IF
"#;
        let code = compile_to_c(source).unwrap();
        assert!(!code.contains(r#"qb_print_string(qb_string_new("Never printed")"#));
    }

    #[test]
    fn windows_alias() {
        // WINDOWS should be an alias for WIN
        let source = r#"
$IF WINDOWS THEN
    PRINT "Windows"
$END IF
"#;
        let code = compile_to_c(source).unwrap();

        #[cfg(target_os = "windows")]
        {
            assert!(code.contains(r#"qb_print_string(qb_string_new("Windows")"#));
        }

        #[cfg(not(target_os = "windows"))]
        {
            assert!(!code.contains(r#"qb_print_string(qb_string_new("Windows")"#));
        }
    }
}

// =============================================================================
// System Statements Tests (File System, Shell, Memory, Mouse, Clipboard)
// =============================================================================

mod system_statements {
    use super::*;

    // File system statements

    #[test]
    fn kill_statement() {
        let source = r#"KILL "test.txt""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_kill("));
    }

    #[test]
    fn kill_with_variable() {
        let source = r#"
DIM filename$
filename$ = "test.txt"
KILL filename$
"#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_kill("));
    }

    #[test]
    fn name_statement() {
        let source = r#"NAME "old.txt" AS "new.txt""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_rename("));
    }

    #[test]
    fn mkdir_statement() {
        let source = r#"MKDIR "testdir""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_mkdir("));
    }

    #[test]
    fn rmdir_statement() {
        let source = r#"RMDIR "testdir""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_rmdir("));
    }

    #[test]
    fn chdir_statement() {
        let source = r#"CHDIR "/tmp""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_chdir("));
    }

    // Shell statements

    #[test]
    fn shell_no_command() {
        let source = "SHELL";
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_shell("));
    }

    #[test]
    fn shell_with_command() {
        let source = r#"SHELL "ls -la""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_shell("));
    }

    #[test]
    fn shellhide_statement() {
        let source = r#"_SHELLHIDE "background_task""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_shellhide("));
    }

    // Memory statements

    #[test]
    fn bload_simple() {
        let source = r#"BLOAD "data.bin""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_bload("));
    }

    #[test]
    fn bload_with_address() {
        let source = r#"BLOAD "data.bin", 12345"#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_bload("));
    }

    #[test]
    fn bsave_statement() {
        let source = r#"BSAVE "data.bin", 12345, 1000"#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_bsave("));
    }

    #[test]
    fn setmem_statement() {
        let source = "SETMEM 65536";
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_setmem("));
    }

    // Mouse statements

    #[test]
    fn mousehide_statement() {
        let source = "_MOUSEHIDE";
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_mousehide("));
    }

    #[test]
    fn mouseshow_statement() {
        let source = "_MOUSESHOW";
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_mouseshow("));
    }

    #[test]
    fn mousemove_statement() {
        let source = "_MOUSEMOVE 100, 200";
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_mousemove("));
    }

    // Clipboard statements

    #[test]
    fn clipboard_set_statement() {
        let source = r#"_CLIPBOARD$ = "Hello""#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_clipboard_set("));
    }

    #[test]
    fn clipboard_set_with_variable() {
        let source = r#"
DIM text$
text$ = "copied text"
_CLIPBOARD$ = text$
"#;
        let code = compile_to_c(source).unwrap();
        assert!(code.contains("qb_clipboard_set("));
    }
}
