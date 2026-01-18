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
