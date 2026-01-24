//! Parser tests for QB64Fresh BASIC.
//!
//! This module contains comprehensive tests for the parser, organized by feature area:
//! - Basic parsing (literals, expressions, assignments)
//! - File I/O statements
//! - Error handling
//! - Control flow
//! - Graphics statements
//! - Audio statements
//! - System statements

use super::*;
use crate::ast::StatementKind;
use crate::lexer::lex;

fn parse(source: &str) -> Result<Program, Vec<ParseError>> {
    let tokens = lex(source);
    let mut parser = Parser::new(&tokens);
    parser.parse()
}

#[test]
fn test_parse_integer_literal() {
    let program = parse("PRINT 42").unwrap();
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_parse_string_literal() {
    let program = parse(r#"PRINT "Hello""#).unwrap();
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_parse_binary_expression() {
    let program = parse("PRINT 1 + 2").unwrap();
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_parse_precedence() {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    let program = parse("x = 1 + 2 * 3").unwrap();
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_parse_assignment() {
    let program = parse("x = 5").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::Let { .. }
    ));
}

#[test]
fn test_parse_dim() {
    let program = parse("DIM x AS INTEGER").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::Dim { .. }
    ));
}

#[test]
fn test_parse_if_single_line() {
    let program = parse("IF x > 0 THEN PRINT x").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::If { .. }
    ));
}

#[test]
fn test_parse_for_loop() {
    let program = parse(
        r#"
FOR i = 1 TO 10
    PRINT i
NEXT
"#,
    )
    .unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::For { .. }
    ));
}

#[test]
fn test_parse_while_loop() {
    let program = parse(
        r#"
WHILE x > 0
    x = x - 1
WEND
"#,
    )
    .unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::While { .. }
    ));
}

#[test]
fn test_parse_multiple_statements() {
    let program = parse(
        r#"
PRINT "Hello"
x = 5
PRINT x
"#,
    )
    .unwrap();
    assert_eq!(program.statements.len(), 3);
}

#[test]
fn test_parse_unterminated_string() {
    // Unterminated string (missing closing quote)
    let result = parse(r#"PRINT "Hello"#);
    assert!(result.is_err());

    // The error should be an UnterminatedString error
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0], ParseError::UnterminatedString { .. }));
}

#[test]
fn test_parse_input_with_prompt_semicolon() {
    // INPUT "Name"; x$ - valid, shows "?"
    let program = parse(r#"INPUT "Enter name"; name$"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::Input {
            prompt: Some(_),
            show_question_mark: true,
            ..
        }
    ));
}

#[test]
fn test_parse_input_with_prompt_comma() {
    // INPUT "Name", x$ - valid, no "?"
    let program = parse(r#"INPUT "Enter name", name$"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::Input {
            prompt: Some(_),
            show_question_mark: false,
            ..
        }
    ));
}

#[test]
fn test_parse_input_prompt_missing_separator() {
    // INPUT "Name" x$ - invalid, missing separator
    let result = parse(r#"INPUT "Enter name" name$"#);
    assert!(result.is_err());
}

#[test]
fn test_parse_input_no_prompt() {
    // INPUT x$ - valid, no prompt
    let program = parse("INPUT name$").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::Input { prompt: None, .. }
    ));
}

// ========================================================
// Phase 1 Feature Tests: File I/O
// ========================================================

#[test]
fn test_parse_open_for_input() {
    let program = parse(r#"OPEN "test.txt" FOR INPUT AS #1"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OpenFile { .. }
    ));
}

#[test]
fn test_parse_open_for_output() {
    let program = parse(r#"OPEN "output.dat" FOR OUTPUT AS #2"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OpenFile { .. }
    ));
}

#[test]
fn test_parse_open_for_append() {
    let program = parse(r#"OPEN "log.txt" FOR APPEND AS #3"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OpenFile { .. }
    ));
}

#[test]
fn test_parse_open_for_binary() {
    let program = parse(r#"OPEN "data.bin" FOR BINARY AS #1"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OpenFile { .. }
    ));
}

#[test]
fn test_parse_open_for_random() {
    let program = parse(r#"OPEN "records.dat" FOR RANDOM AS #1 LEN = 100"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OpenFile { .. }
    ));
}

#[test]
fn test_parse_close() {
    let program = parse("CLOSE #1").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::CloseFile { .. }
    ));
}

#[test]
fn test_parse_close_multiple() {
    let program = parse("CLOSE #1, #2, #3").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::CloseFile { file_nums } = &program.statements[0].kind {
        assert_eq!(file_nums.len(), 3);
    } else {
        panic!("Expected CloseFile statement");
    }
}

#[test]
fn test_parse_print_to_file() {
    let program = parse(r#"PRINT #1, "Hello""#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FilePrint { .. }
    ));
}

#[test]
fn test_parse_input_from_file() {
    let program = parse("INPUT #1, x, y$").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FileInput { .. }
    ));
}

#[test]
fn test_parse_line_input_from_file() {
    let program = parse("LINE INPUT #1, line$").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FileLineInput { .. }
    ));
}

#[test]
fn test_parse_get() {
    let program = parse("GET #1, 10, record").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FileGet { .. }
    ));
}

#[test]
fn test_parse_put() {
    let program = parse("PUT #1, 10, record").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FilePut { .. }
    ));
}

#[test]
fn test_parse_seek() {
    let program = parse("SEEK #1, 100").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FileSeek { .. }
    ));
}

// ========================================================
// Phase 1 Feature Tests: Error Handling
// ========================================================

#[test]
fn test_parse_on_error_goto() {
    let program = parse("ON ERROR GOTO errorHandler").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OnErrorGoto { .. }
    ));
}

#[test]
fn test_parse_on_error_goto_zero() {
    let program = parse("ON ERROR GOTO 0").unwrap();
    assert_eq!(program.statements.len(), 1);
    // "0" means disable error handling - stored as target "0"
    if let StatementKind::OnErrorGoto { target } = &program.statements[0].kind {
        assert_eq!(target, "0");
    } else {
        panic!("Expected OnErrorGoto statement");
    }
}

#[test]
fn test_parse_on_error_resume_next() {
    let program = parse("ON ERROR RESUME NEXT").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::OnErrorResumeNext
    ));
}

#[test]
fn test_parse_resume() {
    let program = parse("RESUME").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::ResumeStmt { .. }
    ));
}

#[test]
fn test_parse_resume_next() {
    let program = parse("RESUME NEXT").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::ResumeStmt { .. }
    ));
}

#[test]
fn test_parse_resume_label() {
    let program = parse("RESUME startOver").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::ResumeStmt { .. }
    ));
}

#[test]
fn test_parse_error_statement() {
    let program = parse("ERROR 53").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::ErrorStmt { .. }
    ));
}

#[test]
fn test_parse_option_base_0() {
    let program = parse("OPTION BASE 0").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::OptionBase { base } = &program.statements[0].kind {
        assert_eq!(*base, 0);
    } else {
        panic!("Expected OptionBase statement");
    }
}

#[test]
fn test_parse_option_base_1() {
    let program = parse("OPTION BASE 1").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::OptionBase { base } = &program.statements[0].kind {
        assert_eq!(*base, 1);
    } else {
        panic!("Expected OptionBase statement");
    }
}

#[test]
fn test_parse_option_base_invalid() {
    // Should fail - OPTION BASE can only be 0 or 1
    let result = parse("OPTION BASE 2");
    assert!(result.is_err());
}

// ========================================================
// Phase 1 Feature Tests: Computed Control Flow
// ========================================================

#[test]
fn test_parse_on_goto() {
    let program = parse("ON choice GOTO label1, label2, label3").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::OnGoto { targets, .. } = &program.statements[0].kind {
        assert_eq!(targets.len(), 3);
    } else {
        panic!("Expected OnGoto statement");
    }
}

#[test]
fn test_parse_on_gosub() {
    let program = parse("ON menuItem GOSUB sub1, sub2").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::OnGosub { targets, .. } = &program.statements[0].kind {
        assert_eq!(targets.len(), 2);
    } else {
        panic!("Expected OnGosub statement");
    }
}

// ========================================================
// Phase 1 Feature Tests: DEF FN
// ========================================================

#[test]
fn test_parse_def_fn_single_line() {
    let program = parse("DEF FNdouble(x) = x * 2").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::DefFn { .. }
    ));
}

#[test]
fn test_parse_def_fn_with_type() {
    let program = parse("DEF FNsquare%(n%) = n% * n%").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::DefFn { .. }
    ));
}

// ========================================================
// Phase 1 Feature Tests: Variable/Scope Enhancements
// ========================================================

#[test]
fn test_parse_common() {
    let program = parse("COMMON x, y$, z%").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::CommonStmt { variables, .. } = &program.statements[0].kind {
        assert_eq!(variables.len(), 3);
    } else {
        panic!("Expected CommonStmt");
    }
}

#[test]
fn test_parse_common_shared() {
    let program = parse("COMMON SHARED counter%").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::CommonStmt { shared: true, .. }
    ));
}

#[test]
fn test_parse_redim() {
    let program = parse("REDIM array(100)").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::Redim {
            preserve: false,
            ..
        }
    ));
}

#[test]
fn test_parse_redim_preserve() {
    let program = parse("REDIM _PRESERVE buffer$(newSize)").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::Redim { preserve: true, .. }
    ));
}

#[test]
fn test_parse_write_to_file() {
    let program = parse(r#"WRITE #1, "data", 42, 3.14"#).unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::FileWrite { .. }
    ));
}

// Phase 2: Conditional Compilation tests

#[test]
fn test_parse_meta_let() {
    let program = parse("$LET DEBUG = 1").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::MetaLet { name, value } = &program.statements[0].kind {
        assert_eq!(name, "DEBUG");
        assert_eq!(*value, 1);
    } else {
        panic!("Expected MetaLet statement");
    }
}

#[test]
fn test_parse_meta_let_negative() {
    let program = parse("$LET VERSION = -5").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::MetaLet { name, value } = &program.statements[0].kind {
        assert_eq!(name, "VERSION");
        assert_eq!(*value, -5);
    } else {
        panic!("Expected MetaLet statement");
    }
}

#[test]
fn test_parse_meta_checking_on() {
    let program = parse("$CHECKING:ON").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::MetaChecking { enabled } = &program.statements[0].kind {
        assert!(*enabled);
    } else {
        panic!("Expected MetaChecking statement");
    }
}

#[test]
fn test_parse_meta_checking_off() {
    let program = parse("$CHECKING:OFF").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::MetaChecking { enabled } = &program.statements[0].kind {
        assert!(!*enabled);
    } else {
        panic!("Expected MetaChecking statement");
    }
}

#[test]
fn test_parse_meta_console() {
    let program = parse("$CONSOLE").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::MetaConsole { only } = &program.statements[0].kind {
        assert!(!*only);
    } else {
        panic!("Expected MetaConsole statement");
    }
}

#[test]
#[ignore = "$CONSOLE:ONLY syntax not yet implemented in parser"]
fn test_parse_meta_console_only() {
    let program = parse("$CONSOLE:ONLY").unwrap();
    assert_eq!(program.statements.len(), 1);
    if let StatementKind::MetaConsole { only } = &program.statements[0].kind {
        assert!(*only);
    } else {
        panic!("Expected MetaConsole statement with only=true");
    }
}

#[test]
fn test_parse_meta_screenhide() {
    let program = parse("$SCREENHIDE").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::MetaScreenHide
    ));
}

#[test]
fn test_parse_meta_screenshow() {
    let program = parse("$SCREENSHOW").unwrap();
    assert_eq!(program.statements.len(), 1);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::MetaScreenShow
    ));
}

// ========================================================
// Graphics Module Tests
// ========================================================

mod graphics_tests {
    use super::*;

    // ----- Screen Management -----

    #[test]
    fn test_parse_screen_basic() {
        let program = parse("SCREEN 13").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Screen { mode: Some(_), .. }
        ));
    }

    #[test]
    fn test_parse_screen_no_args() {
        let program = parse("SCREEN").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Screen {
                mode: None,
                color_switch: None,
                active_page: None,
                visual_page: None
            }
        ));
    }

    #[test]
    fn test_parse_screen_with_pages() {
        let program = parse("SCREEN 0, , 1, 0").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Screen {
                mode: Some(_),
                color_switch: None,
                active_page: Some(_),
                visual_page: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_screen_all_args() {
        let program = parse("SCREEN 12, 0, 1, 0").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Screen {
                mode: Some(_),
                color_switch: Some(_),
                active_page: Some(_),
                visual_page: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_cls_no_args() {
        let program = parse("CLS").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Cls { mode: None }
        ));
    }

    #[test]
    fn test_parse_cls_with_mode() {
        let program = parse("CLS 2").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Cls { mode: Some(_) }
        ));
    }

    #[test]
    fn test_parse_color_basic() {
        let program = parse("COLOR 7").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Color {
                foreground: Some(_),
                background: None,
                border: None
            }
        ));
    }

    #[test]
    fn test_parse_color_with_background() {
        let program = parse("COLOR 14, 1").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Color {
                foreground: Some(_),
                background: Some(_),
                border: None
            }
        ));
    }

    #[test]
    fn test_parse_color_all_params() {
        let program = parse("COLOR 15, 0, 4").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Color {
                foreground: Some(_),
                background: Some(_),
                border: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_color_omit_foreground() {
        let program = parse("COLOR , 4").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Color {
                foreground: None,
                background: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_locate_basic() {
        let program = parse("LOCATE 10, 20").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Locate {
                row: Some(_),
                col: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_locate_row_only() {
        let program = parse("LOCATE 5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Locate {
                row: Some(_),
                col: None
            }
        ));
    }

    #[test]
    fn test_parse_locate_col_only() {
        let program = parse("LOCATE , 15").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Locate {
                row: None,
                col: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_width_basic() {
        let program = parse("WIDTH 80").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Width { rows: None, .. }
        ));
    }

    #[test]
    fn test_parse_width_with_rows() {
        let program = parse("WIDTH 80, 25").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Width { rows: Some(_), .. }
        ));
    }

    // ----- Drawing Primitives -----

    #[test]
    fn test_parse_pset_basic() {
        let program = parse("PSET (100, 50)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Pset {
                step: false,
                color: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_pset_with_color() {
        let program = parse("PSET (100, 50), 14").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Pset {
                step: false,
                color: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_pset_with_step() {
        let program = parse("PSET STEP(10, 5)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Pset { step: true, .. }
        ));
    }

    #[test]
    fn test_parse_preset_basic() {
        let program = parse("PRESET (200, 100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Preset { step: false, .. }
        ));
    }

    #[test]
    fn test_parse_preset_with_step() {
        let program = parse("PRESET STEP(5, 5)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Preset { step: true, .. }
        ));
    }

    #[test]
    fn test_parse_line_basic() {
        let program = parse("LINE (0, 0)-(100, 100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Line {
                x1: Some(_),
                y1: Some(_),
                color: None,
                box_style: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_line_with_color() {
        let program = parse("LINE (0, 0)-(100, 100), 4").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Line {
                color: Some(_),
                box_style: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_line_box() {
        let program = parse("LINE (10, 10)-(50, 50), 15, B").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::Line { box_style, .. } = &program.statements[0].kind {
            assert_eq!(*box_style, Some(false)); // B = unfilled box
        } else {
            panic!("Expected Line statement");
        }
    }

    #[test]
    fn test_parse_line_filled_box() {
        let program = parse("LINE (10, 10)-(50, 50), 15, BF").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::Line { box_style, .. } = &program.statements[0].kind {
            assert_eq!(*box_style, Some(true)); // BF = filled box
        } else {
            panic!("Expected Line statement");
        }
    }

    #[test]
    fn test_parse_line_no_start_point() {
        let program = parse("LINE -(100, 100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Line {
                x1: None,
                y1: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_circle_basic() {
        let program = parse("CIRCLE (160, 100), 50").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Circle {
                step: false,
                color: None,
                filled: false,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_circle_with_color() {
        let program = parse("CIRCLE (160, 100), 50, 14").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Circle { color: Some(_), .. }
        ));
    }

    #[test]
    fn test_parse_circle_with_step() {
        let program = parse("CIRCLE STEP(0, 0), 25").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Circle { step: true, .. }
        ));
    }

    #[test]
    fn test_parse_paint_basic() {
        let program = parse("PAINT (100, 100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Paint {
                step: false,
                color: None,
                border: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_paint_with_color() {
        let program = parse("PAINT (100, 100), 4").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Paint {
                color: Some(_),
                border: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_paint_with_border() {
        let program = parse("PAINT (100, 100), 4, 15").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Paint {
                color: Some(_),
                border: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_draw_basic() {
        let program = parse(r#"DRAW "U10 R10 D10 L10""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::DrawCmd { .. }
        ));
    }

    // ----- Viewport Control -----

    #[test]
    fn test_parse_view_reset() {
        let program = parse("VIEW").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::View {
                screen: false,
                coords: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_view_with_coords() {
        let program = parse("VIEW (0, 0)-(319, 199)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::View {
                screen: false,
                coords: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_view_screen() {
        let program = parse("VIEW SCREEN (10, 10)-(300, 180)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::View {
                screen: true,
                coords: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_view_print_reset() {
        let program = parse("VIEW PRINT").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ViewPrint {
                top: None,
                bottom: None
            }
        ));
    }

    #[test]
    fn test_parse_view_print_range() {
        let program = parse("VIEW PRINT 5 TO 20").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ViewPrint {
                top: Some(_),
                bottom: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_window_reset() {
        let program = parse("WINDOW").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::WindowCoords {
                screen: false,
                coords: None
            }
        ));
    }

    #[test]
    fn test_parse_window_with_coords() {
        let program = parse("WINDOW (0, 0)-(100, 100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::WindowCoords {
                screen: false,
                coords: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_window_screen() {
        let program = parse("WINDOW SCREEN (0, 0)-(640, 480)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::WindowCoords {
                screen: true,
                coords: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_palette_reset() {
        let program = parse("PALETTE").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Palette {
                attribute: None,
                color: None
            }
        ));
    }

    #[test]
    fn test_parse_palette_set() {
        let program = parse("PALETTE 1, 63").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Palette {
                attribute: Some(_),
                color: Some(_)
            }
        ));
    }

    #[test]
    fn test_parse_pcopy() {
        let program = parse("PCOPY 0, 1").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Pcopy { .. }
        ));
    }

    // ----- QB64 Graphics Extensions -----

    #[test]
    fn test_parse_display() {
        let program = parse("_DISPLAY").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::GfxDisplay
        ));
    }

    #[test]
    fn test_parse_freeimage() {
        let program = parse("_FREEIMAGE img&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FreeImage { .. }
        ));
    }

    #[test]
    fn test_parse_putimage_basic() {
        let program = parse("_PUTIMAGE (0, 0)-(100, 100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::PutImage {
                dest_coords: Some(_),
                source: None,
                dest: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_putimage_with_handles() {
        let program = parse("_PUTIMAGE (0, 0)-(100, 100), src&, dest&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::PutImage {
                dest_coords: Some(_),
                source: Some(_),
                dest: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_source() {
        let program = parse("_SOURCE img&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SourceImg { .. }
        ));
    }

    #[test]
    fn test_parse_dest() {
        let program = parse("_DEST img&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::DestImg { .. }
        ));
    }

    #[test]
    fn test_parse_printstring() {
        let program = parse(r#"_PRINTSTRING (10, 20), "Hello""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::PrintStringStmt { .. }
        ));
    }

    #[test]
    fn test_parse_autodisplay_on() {
        let program = parse("_AUTODISPLAY ON").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::AutoDisplay { enabled } = &program.statements[0].kind {
            assert!(*enabled);
        } else {
            panic!("Expected AutoDisplay statement");
        }
    }

    #[test]
    fn test_parse_autodisplay_off() {
        let program = parse("_AUTODISPLAY OFF").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::AutoDisplay { enabled } = &program.statements[0].kind {
            assert!(!*enabled);
        } else {
            panic!("Expected AutoDisplay statement");
        }
    }

    #[test]
    fn test_parse_autodisplay_default() {
        let program = parse("_AUTODISPLAY").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::AutoDisplay { enabled } = &program.statements[0].kind {
            assert!(*enabled); // Default is ON
        } else {
            panic!("Expected AutoDisplay statement");
        }
    }
}

// ========================================================
// Audio Module Tests
// ========================================================

mod audio_tests {
    use super::*;

    // ----- Classic BASIC Audio -----

    #[test]
    fn test_parse_beep() {
        let program = parse("BEEP").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(&program.statements[0].kind, StatementKind::Beep));
    }

    #[test]
    fn test_parse_sound() {
        let program = parse("SOUND 440, 18").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SoundStmt { .. }
        ));
    }

    #[test]
    fn test_parse_play() {
        let program = parse(r#"PLAY "O4 C D E F G""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::PlayStmt { .. }
        ));
    }

    // ----- QB64 Sound Extensions -----

    #[test]
    fn test_parse_sndclose() {
        let program = parse("_SNDCLOSE snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndClose { .. }
        ));
    }

    #[test]
    fn test_parse_sndplay() {
        let program = parse("_SNDPLAY snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndPlay { .. }
        ));
    }

    #[test]
    fn test_parse_sndstop() {
        let program = parse("_SNDSTOP snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndStop { .. }
        ));
    }

    #[test]
    fn test_parse_sndpause() {
        let program = parse("_SNDPAUSE snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndPause { .. }
        ));
    }

    #[test]
    fn test_parse_sndloop() {
        let program = parse("_SNDLOOP snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndLoop { .. }
        ));
    }

    #[test]
    fn test_parse_sndvol() {
        let program = parse("_SNDVOL snd&, 0.5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndVol { .. }
        ));
    }

    #[test]
    fn test_parse_sndbal_basic() {
        let program = parse("_SNDBAL snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndBal {
                x: None,
                y: None,
                z: None,
                channel: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_sndbal_with_position() {
        let program = parse("_SNDBAL snd&, 1.0").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndBal { x: Some(_), .. }
        ));
    }

    #[test]
    fn test_parse_sndbal_sparse_params() {
        let program = parse("_SNDBAL snd&, , , , 1").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndBal {
                x: None,
                y: None,
                z: None,
                channel: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_sndraw_mono() {
        let program = parse("_SNDRAW 0.5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndRaw { right: None, .. }
        ));
    }

    #[test]
    fn test_parse_sndraw_stereo() {
        let program = parse("_SNDRAW 0.3, 0.7").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndRaw { right: Some(_), .. }
        ));
    }

    #[test]
    fn test_parse_sndplayfile_basic() {
        let program = parse(r#"_SNDPLAYFILE "music.mp3""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndPlayFile {
                volume: None,
                x: None,
                y: None,
                z: None,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_sndplayfile_with_volume() {
        let program = parse(r#"_SNDPLAYFILE "music.mp3", 0.8"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndPlayFile {
                volume: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_sndplaycopy_basic() {
        let program = parse("_SNDPLAYCOPY snd&").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndPlayCopy { volume: None, .. }
        ));
    }

    #[test]
    fn test_parse_sndplaycopy_with_volume() {
        let program = parse("_SNDPLAYCOPY snd&, 0.5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndPlayCopy {
                volume: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_sndsetpos() {
        let program = parse("_SNDSETPOS snd&, 5.5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::SndSetPos { .. }
        ));
    }
}

// ========================================================
// System Module Tests
// ========================================================

mod system_tests {
    use super::*;

    // ----- File System Statements -----

    #[test]
    fn test_parse_kill() {
        let program = parse(r#"KILL "tempfile.tmp""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Kill { .. }
        ));
    }

    #[test]
    fn test_parse_name() {
        let program = parse(r#"NAME "old.txt" AS "new.txt""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Rename { .. }
        ));
    }

    #[test]
    fn test_parse_mkdir() {
        let program = parse(r#"MKDIR "subdir""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Mkdir { .. }
        ));
    }

    #[test]
    fn test_parse_rmdir() {
        let program = parse(r#"RMDIR "subdir""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Rmdir { .. }
        ));
    }

    #[test]
    fn test_parse_chdir() {
        let program = parse(r#"CHDIR "C:\Users""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Chdir { .. }
        ));
    }

    // ----- Shell Commands -----

    #[test]
    fn test_parse_shell_no_args() {
        let program = parse("SHELL").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ShellCmd { command: None }
        ));
    }

    #[test]
    fn test_parse_shell_with_command() {
        let program = parse(r#"SHELL "dir""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ShellCmd { command: Some(_) }
        ));
    }

    #[test]
    fn test_parse_shellhide() {
        let program = parse(r#"_SHELLHIDE "command""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ShellHide { .. }
        ));
    }

    // ----- Memory Operations -----

    #[test]
    fn test_parse_bload_basic() {
        let program = parse(r#"BLOAD "image.dat""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Bload { address: None, .. }
        ));
    }

    #[test]
    fn test_parse_bload_with_address() {
        let program = parse(r#"BLOAD "image.dat", &HA000"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Bload {
                address: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn test_parse_bsave() {
        let program = parse(r#"BSAVE "screen.dat", &HA000, 64000"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Bsave { .. }
        ));
    }

    #[test]
    fn test_parse_setmem() {
        let program = parse("SETMEM 65536").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Setmem { .. }
        ));
    }

    // ----- Mouse Statements -----

    #[test]
    fn test_parse_mousehide() {
        let program = parse("_MOUSEHIDE").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::MouseHide
        ));
    }

    #[test]
    fn test_parse_mouseshow() {
        let program = parse("_MOUSESHOW").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::MouseShow
        ));
    }

    #[test]
    fn test_parse_mousemove() {
        let program = parse("_MOUSEMOVE 320, 240").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::MouseMoveStmt { .. }
        ));
    }

    // ----- Clipboard Statement -----

    #[test]
    fn test_parse_clipboard_set() {
        let program = parse(r#"_CLIPBOARD$ = "Hello""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ClipboardSet { .. }
        ));
    }
}

// ========================================================
// File I/O Module Tests
// ========================================================

mod file_io_tests {
    use super::*;
    use crate::ast::{FileAccess, FileLock, FileMode, PutAction};

    // ----- OPEN Statement -----

    #[test]
    fn test_parse_open_with_access() {
        let program = parse(r#"OPEN "data.bin" FOR BINARY ACCESS READ AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { access, .. } = &program.statements[0].kind {
            assert_eq!(*access, Some(FileAccess::Read));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    #[test]
    fn test_parse_open_with_read_write_access() {
        let program = parse(r#"OPEN "data.bin" FOR BINARY ACCESS READ WRITE AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { access, .. } = &program.statements[0].kind {
            assert_eq!(*access, Some(FileAccess::ReadWrite));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    #[test]
    fn test_parse_open_with_shared_lock() {
        let program = parse(r#"OPEN "data.dat" FOR RANDOM SHARED AS #1 LEN = 128"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { lock, .. } = &program.statements[0].kind {
            assert_eq!(*lock, Some(FileLock::Shared));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    #[test]
    fn test_parse_open_with_lock_read() {
        let program = parse(r#"OPEN "file.txt" FOR INPUT LOCK READ AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { lock, .. } = &program.statements[0].kind {
            assert_eq!(*lock, Some(FileLock::LockRead));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    #[test]
    fn test_parse_open_with_lock_write() {
        let program = parse(r#"OPEN "file.txt" FOR OUTPUT LOCK WRITE AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { lock, .. } = &program.statements[0].kind {
            assert_eq!(*lock, Some(FileLock::LockWrite));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    #[test]
    fn test_parse_open_with_lock_read_write() {
        let program = parse(r#"OPEN "file.txt" FOR BINARY LOCK READ WRITE AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { lock, .. } = &program.statements[0].kind {
            assert_eq!(*lock, Some(FileLock::LockReadWrite));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    #[test]
    fn test_parse_open_modes() {
        // Test all file modes
        let modes = [
            ("INPUT", FileMode::Input),
            ("OUTPUT", FileMode::Output),
            ("APPEND", FileMode::Append),
            ("BINARY", FileMode::Binary),
            ("RANDOM", FileMode::Random),
        ];

        for (mode_str, expected_mode) in modes {
            let source = format!(r#"OPEN "file.txt" FOR {} AS #1"#, mode_str);
            let program = parse(&source).unwrap();
            if let StatementKind::OpenFile { mode, .. } = &program.statements[0].kind {
                assert_eq!(
                    *mode, expected_mode,
                    "Mode {} should parse correctly",
                    mode_str
                );
            } else {
                panic!("Expected OpenFile statement for mode {}", mode_str);
            }
        }
    }

    // ----- CLOSE Statement -----

    #[test]
    fn test_parse_close_no_args() {
        let program = parse("CLOSE").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::CloseFile { file_nums } = &program.statements[0].kind {
            assert_eq!(file_nums.len(), 0);
        } else {
            panic!("Expected CloseFile statement");
        }
    }

    #[test]
    fn test_parse_close_without_hash() {
        let program = parse("CLOSE 1").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::CloseFile { file_nums } = &program.statements[0].kind {
            assert_eq!(file_nums.len(), 1);
        } else {
            panic!("Expected CloseFile statement");
        }
    }

    // ----- WRITE # Statement -----

    #[test]
    fn test_parse_write_single_value() {
        let program = parse(r#"WRITE #1, "test""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::FileWrite { values, .. } = &program.statements[0].kind {
            assert_eq!(values.len(), 1);
        } else {
            panic!("Expected FileWrite statement");
        }
    }

    #[test]
    fn test_parse_write_multiple_values() {
        let program = parse(r#"WRITE #1, "name", 42, 3.14, "end""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::FileWrite { values, .. } = &program.statements[0].kind {
            assert_eq!(values.len(), 4);
        } else {
            panic!("Expected FileWrite statement");
        }
    }

    // ----- GET Statement (File) -----

    #[test]
    fn test_parse_get_file_no_position() {
        let program = parse("GET #1, , record").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileGet { position: None, .. }
        ));
    }

    #[test]
    fn test_parse_get_file_with_array_index() {
        let program = parse("GET #1, 5, buffer(0)").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::FileGet { position, .. } = &program.statements[0].kind {
            assert!(position.is_some());
        } else {
            panic!("Expected FileGet statement");
        }
    }

    // ----- GET Statement (Graphics) -----

    #[test]
    fn test_parse_graphics_get_basic() {
        let program = parse("GET (0, 0)-(100, 100), sprite()").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::GraphicsGet {
                step1: false,
                step2: false,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_graphics_get_with_step() {
        let program = parse("GET STEP(0, 0)-STEP(50, 50), sprite()").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::GraphicsGet {
                step1: true,
                step2: true,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_graphics_get_with_index() {
        let program = parse("GET (10, 10)-(60, 60), sprite(idx%)").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::GraphicsGet { array_indices, .. } = &program.statements[0].kind {
            assert_eq!(array_indices.len(), 1);
        } else {
            panic!("Expected GraphicsGet statement");
        }
    }

    // ----- PUT Statement (File) -----

    #[test]
    fn test_parse_put_file_no_position() {
        let program = parse("PUT #1, , record").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FilePut { position: None, .. }
        ));
    }

    #[test]
    fn test_parse_put_file_with_array_index() {
        let program = parse("PUT #1, 10, buffer(0)").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::FilePut { position, .. } = &program.statements[0].kind {
            assert!(position.is_some());
        } else {
            panic!("Expected FilePut statement");
        }
    }

    // ----- PUT Statement (Graphics) -----

    #[test]
    fn test_parse_graphics_put_basic() {
        let program = parse("PUT (100, 50), sprite(), PSET").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::GraphicsPut { action, step, .. } = &program.statements[0].kind {
            assert_eq!(*action, PutAction::Pset);
            assert!(!*step);
        } else {
            panic!("Expected GraphicsPut statement");
        }
    }

    #[test]
    fn test_parse_graphics_put_with_step() {
        let program = parse("PUT STEP(10, 10), sprite(), XOR").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::GraphicsPut { action, step, .. } = &program.statements[0].kind {
            assert_eq!(*action, PutAction::Xor);
            assert!(*step);
        } else {
            panic!("Expected GraphicsPut statement");
        }
    }

    #[test]
    fn test_parse_graphics_put_actions() {
        let actions = [
            ("PSET", PutAction::Pset),
            ("PRESET", PutAction::Preset),
            ("AND", PutAction::And),
            ("OR", PutAction::Or),
            ("XOR", PutAction::Xor),
        ];

        for (action_str, expected_action) in actions {
            let source = format!("PUT (0, 0), sprite(), {}", action_str);
            let program = parse(&source).unwrap();
            if let StatementKind::GraphicsPut { action, .. } = &program.statements[0].kind {
                assert_eq!(
                    *action, expected_action,
                    "Action {} should parse correctly",
                    action_str
                );
            } else {
                panic!("Expected GraphicsPut statement for action {}", action_str);
            }
        }
    }

    // ----- SEEK Statement -----

    #[test]
    fn test_parse_seek_without_hash() {
        let program = parse("SEEK 1, 100").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileSeek { .. }
        ));
    }

    #[test]
    fn test_parse_seek_expression_position() {
        let program = parse("SEEK #1, recnum * 128 + 1").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileSeek { .. }
        ));
    }

    // ----- OPEN with ONLY lock (QB4.5 syntax) -----

    #[test]
    fn test_parse_open_with_only_lock() {
        let program = parse(r#"OPEN "data.dat" FOR RANDOM ONLY AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OpenFile { lock, .. } = &program.statements[0].kind {
            assert_eq!(*lock, Some(FileLock::Only));
        } else {
            panic!("Expected OpenFile statement");
        }
    }

    // ----- Graphics PUT with _CLIP and transparent color -----

    #[test]
    fn test_parse_graphics_put_with_clip() {
        // Note: The lexer uses _CLIP (with underscore) for the CLIP modifier
        let program = parse("PUT (0, 0), sprite(), _CLIP PSET").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::GraphicsPut { clip, action, .. } = &program.statements[0].kind {
            assert!(*clip);
            assert_eq!(*action, PutAction::Pset);
        } else {
            panic!("Expected GraphicsPut statement");
        }
    }

    #[test]
    fn test_parse_graphics_put_with_clip_and_transparent() {
        // Note: The lexer uses _CLIP (with underscore) for the CLIP modifier
        let program = parse("PUT (0, 0), sprite(), _CLIP PSET, 0").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::GraphicsPut {
            clip,
            transparent_color,
            ..
        } = &program.statements[0].kind
        {
            assert!(*clip);
            assert!(transparent_color.is_some());
        } else {
            panic!("Expected GraphicsPut statement");
        }
    }

    // ----- Graphics GET with multi-dimensional array -----

    #[test]
    fn test_parse_graphics_get_multidim_array() {
        let program = parse("GET (0, 0)-(50, 50), sprite(x%, y%)").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::GraphicsGet { array_indices, .. } = &program.statements[0].kind {
            assert_eq!(array_indices.len(), 2);
        } else {
            panic!("Expected GraphicsGet statement");
        }
    }
}

// ========================================================
// Additional Edge Case Tests for Parser Modules
// ========================================================

mod edge_case_tests {
    use super::*;
    use crate::ast::ImageScaleMode;

    // ----- Graphics: LINE with style pattern -----

    #[test]
    fn test_parse_line_with_style_pattern() {
        let program = parse("LINE (0, 0)-(100, 100), 15, B, &HCCCC").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::Line {
            box_style, style, ..
        } = &program.statements[0].kind
        {
            assert_eq!(*box_style, Some(false)); // B = unfilled
            assert!(style.is_some()); // Has style pattern
        } else {
            panic!("Expected Line statement");
        }
    }

    #[test]
    fn test_parse_line_filled_box_with_style() {
        let program = parse("LINE (10, 10)-(90, 90), 4, BF, &HAAAA").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::Line {
            box_style, style, ..
        } = &program.statements[0].kind
        {
            assert_eq!(*box_style, Some(true)); // BF = filled
            assert!(style.is_some());
        } else {
            panic!("Expected Line statement");
        }
    }

    // ----- Graphics: LINE with STEP on second coordinate -----

    #[test]
    fn test_parse_line_with_step() {
        let program = parse("LINE (0, 0)-STEP(50, 50)").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::Line { step2, .. } = &program.statements[0].kind {
            assert!(*step2);
        } else {
            panic!("Expected Line statement");
        }
    }

    // ----- Graphics: CIRCLE with filled flag -----

    #[test]
    fn test_parse_circle_filled() {
        let program = parse("CIRCLE (100, 100), 30, 4, , , , F").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::Circle { filled, .. } = &program.statements[0].kind {
            assert!(*filled);
        } else {
            panic!("Expected Circle statement");
        }
    }

    // ----- Graphics: PAINT with STEP -----

    #[test]
    fn test_parse_paint_with_step() {
        let program = parse("PAINT STEP(0, 0), 4, 15").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Paint { step: true, .. }
        ));
    }

    // ----- Graphics: _PUTIMAGE with source coordinates -----

    #[test]
    fn test_parse_putimage_with_source_coords() {
        let program = parse("_PUTIMAGE (0, 0)-(100, 100), src&, dest&, (10, 10)-(50, 50)").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::PutImage {
            source_coords,
            dest_coords,
            ..
        } = &program.statements[0].kind
        {
            assert!(dest_coords.is_some());
            assert!(source_coords.is_some());
        } else {
            panic!("Expected PutImage statement");
        }
    }

    #[test]
    fn test_parse_putimage_with_smooth() {
        // Note: The lexer recognizes SMOOTH (not _SMOOTH) as a keyword
        let program =
            parse("_PUTIMAGE (0, 0)-(100, 100), src&, dest&, (0, 0)-(50, 50), SMOOTH").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::PutImage { scale_mode, .. } = &program.statements[0].kind {
            assert_eq!(*scale_mode, ImageScaleMode::Smooth);
        } else {
            panic!("Expected PutImage statement");
        }
    }

    #[test]
    fn test_parse_putimage_with_stretch() {
        // Note: The lexer recognizes STRETCH (not _STRETCH) as a keyword
        let program =
            parse("_PUTIMAGE (0, 0)-(100, 100), src&, dest&, (0, 0)-(50, 50), STRETCH").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::PutImage { scale_mode, .. } = &program.statements[0].kind {
            assert_eq!(*scale_mode, ImageScaleMode::Stretch);
        } else {
            panic!("Expected PutImage statement");
        }
    }

    // ----- Audio: _SNDPLAYFILE with 3D position -----

    #[test]
    fn test_parse_sndplayfile_with_3d_position() {
        let program = parse(r#"_SNDPLAYFILE "sound.wav", 0.8, 1.0, 2.0, 3.0"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::SndPlayFile {
            volume, x, y, z, ..
        } = &program.statements[0].kind
        {
            assert!(volume.is_some());
            assert!(x.is_some());
            assert!(y.is_some());
            assert!(z.is_some());
        } else {
            panic!("Expected SndPlayFile statement");
        }
    }

    // ----- Audio: _SNDBAL with all parameters -----

    #[test]
    fn test_parse_sndbal_all_params() {
        let program = parse("_SNDBAL snd&, 1.0, 0.5, 0.0, 2").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::SndBal {
            x, y, z, channel, ..
        } = &program.statements[0].kind
        {
            assert!(x.is_some());
            assert!(y.is_some());
            assert!(z.is_some());
            assert!(channel.is_some());
        } else {
            panic!("Expected SndBal statement");
        }
    }

    // ----- System: KILL with variable expression -----

    #[test]
    fn test_parse_kill_with_expression() {
        let program = parse(r#"KILL "file" + num$"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Kill { .. }
        ));
    }

    // ----- System: NAME with variable expressions -----

    #[test]
    fn test_parse_name_with_variables() {
        let program = parse("NAME old$ AS new$").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Rename { .. }
        ));
    }

    // ----- Graphics: VIEW with colors but no SCREEN -----

    #[test]
    fn test_parse_view_with_colors() {
        let program = parse("VIEW (0, 0)-(319, 199), 1, 15").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::View {
            screen,
            fill_color,
            border_color,
            ..
        } = &program.statements[0].kind
        {
            assert!(!*screen);
            assert!(fill_color.is_some());
            assert!(border_color.is_some());
        } else {
            panic!("Expected View statement");
        }
    }

    // ----- Multiple statements on same line (colon separated) -----

    #[test]
    fn test_parse_colon_separated_graphics() {
        let program = parse("CLS: COLOR 15, 1: LOCATE 10, 10").unwrap();
        assert_eq!(program.statements.len(), 3);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Cls { .. }
        ));
        assert!(matches!(
            &program.statements[1].kind,
            StatementKind::Color { .. }
        ));
        assert!(matches!(
            &program.statements[2].kind,
            StatementKind::Locate { .. }
        ));
    }
}
