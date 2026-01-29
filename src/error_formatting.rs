//! Error formatting with ariadne for rich diagnostics.
//!
//! This module provides utilities to format parse and semantic errors using
//! ariadne for beautiful, context-rich error messages with source code snippets.

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

use crate::parser::ParseError;
use crate::semantic::SemanticError;

/// Formats parse errors using ariadne for rich diagnostics.
///
/// Returns a formatted string with source context, error messages, and suggestions.
pub fn format_parse_errors(source: &str, errors: &[ParseError], file_path: &str) -> String {
    let mut output = Vec::new();
    let mut colors = ColorGenerator::new();

    for error in errors {
        let report = match error {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => {
                let mut report = Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("expected {}, found {}", expected, found))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message(format!("found `{}`", found))
                            .with_color(colors.next()),
                    );

                // Add suggestion if it looks like a typo
                if let Some(suggestion) = suggest_keyword(found, expected) {
                    report = report.with_note(format!("did you mean `{}`?", suggestion));
                }

                report.finish()
            }
            ParseError::UnexpectedEof { expected } => {
                Report::build(ReportKind::Error, file_path, source.len())
                    .with_message(format!("unexpected end of file, expected {}", expected))
                    .with_label(
                        Label::new((file_path, source.len()..source.len()))
                            .with_message("reached end of file here")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
            ParseError::InvalidExpression { span, message } => {
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("invalid expression: {}", message))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("problematic expression")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
            ParseError::InvalidStatement { span, message } => {
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("invalid statement: {}", message))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("problematic statement")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
            ParseError::InvalidNumber { span, message } => {
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("invalid number: {}", message))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("invalid number")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
            ParseError::UnterminatedString { span } => {
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message("unterminated string literal")
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("string starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("strings must be closed with a double quote on the same line")
                    .finish()
            }
            ParseError::MissingEndIf { if_span } => {
                Report::build(ReportKind::Error, file_path, if_span.start)
                    .with_message("missing END IF")
                    .with_label(
                        Label::new((file_path, if_span.start..if_span.end))
                            .with_message("IF statement here")
                            .with_color(colors.next()),
                    )
                    .with_note("block IF statements must be closed with END IF")
                    .finish()
            }
            ParseError::MissingNext { for_span } => {
                Report::build(ReportKind::Error, file_path, for_span.start)
                    .with_message("missing NEXT for FOR loop")
                    .with_label(
                        Label::new((file_path, for_span.start..for_span.end))
                            .with_message("FOR loop starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("every FOR must have a matching NEXT")
                    .finish()
            }
            ParseError::MissingWend { while_span } => {
                Report::build(ReportKind::Error, file_path, while_span.start)
                    .with_message("missing WEND for WHILE loop")
                    .with_label(
                        Label::new((file_path, while_span.start..while_span.end))
                            .with_message("WHILE loop starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("every WHILE must be closed with WEND")
                    .finish()
            }
            ParseError::MissingLoop { do_span } => {
                Report::build(ReportKind::Error, file_path, do_span.start)
                    .with_message("missing LOOP for DO")
                    .with_label(
                        Label::new((file_path, do_span.start..do_span.end))
                            .with_message("DO loop starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("every DO must be closed with LOOP")
                    .finish()
            }
            ParseError::MissingEndSelect { select_span } => {
                Report::build(ReportKind::Error, file_path, select_span.start)
                    .with_message("missing END SELECT")
                    .with_label(
                        Label::new((file_path, select_span.start..select_span.end))
                            .with_message("SELECT CASE starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("SELECT CASE must be closed with END SELECT")
                    .finish()
            }
            ParseError::MissingEndSub { sub_span } => {
                Report::build(ReportKind::Error, file_path, sub_span.start)
                    .with_message("missing END SUB")
                    .with_label(
                        Label::new((file_path, sub_span.start..sub_span.end))
                            .with_message("SUB starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("SUB must be closed with END SUB")
                    .finish()
            }
            ParseError::MissingEndFunction { function_span } => {
                Report::build(ReportKind::Error, file_path, function_span.start)
                    .with_message("missing END FUNCTION")
                    .with_label(
                        Label::new((file_path, function_span.start..function_span.end))
                            .with_message("FUNCTION starts here")
                            .with_color(colors.next()),
                    )
                    .with_note("FUNCTION must be closed with END FUNCTION")
                    .finish()
            }
            ParseError::DuplicateLabel { name, span } => {
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("duplicate label: {}", name))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("duplicate definition")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
            ParseError::SyntaxError { span, message } => {
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(message.clone())
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("syntax error")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
        };

        // Write report - handle errors gracefully
        if let Err(e) = report.write((file_path, Source::from(source)), &mut output) {
            // Fallback to simple error message if ariadne fails
            use std::io::Write;
            let _ = writeln!(output, "Error formatting error message: {}", e);
            let _ = writeln!(output, "{}", error);
        }
    }

    // Convert output to string - should always be valid UTF-8 from ariadne
    String::from_utf8(output).unwrap_or_else(|_| {
        // Fallback if somehow invalid UTF-8 (shouldn't happen with ariadne)
        "Error formatting errors (invalid UTF-8 in output)".to_string()
    })
}

/// Formats semantic errors using ariadne for rich diagnostics.
///
/// Returns a formatted string with source context, error messages, and suggestions.
pub fn format_semantic_errors(source: &str, errors: &[SemanticError], file_path: &str) -> String {
    let mut output = Vec::new();
    let mut colors = ColorGenerator::new();

    for error in errors {
        let span = error.span();
        let report = match error {
            SemanticError::UndefinedVariable {
                name,
                suggestion,
                suggestions,
                ..
            } => {
                let mut report = Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("undefined variable `{}`", name))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message(format!("variable `{}` not found", name))
                            .with_color(colors.next()),
                    );

                if let Some(sug) = suggestion {
                    report = report.with_note(format!("did you mean `{}`?", sug));
                } else if let Some(sugs) = suggestions
                    && !sugs.is_empty()
                {
                    let similar = sugs
                        .iter()
                        .take(3)
                        .map(|s| format!("`{}`", s))
                        .collect::<Vec<_>>()
                        .join(", ");
                    report = report.with_note(format!("similar names: {}", similar));
                }

                report.finish()
            }
            SemanticError::UndefinedLabel {
                name,
                suggestion,
                suggestions,
                ..
            } => {
                let mut report = Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("undefined label `{}`", name))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message(format!("label `{}` not found", name))
                            .with_color(colors.next()),
                    );

                if let Some(sug) = suggestion {
                    report = report.with_note(format!("did you mean `{}`?", sug));
                } else if let Some(sugs) = suggestions
                    && !sugs.is_empty()
                {
                    let similar = sugs
                        .iter()
                        .take(3)
                        .map(|s| format!("`{}`", s))
                        .collect::<Vec<_>>()
                        .join(", ");
                    report = report.with_note(format!("similar names: {}", similar));
                }

                report.finish()
            }
            SemanticError::UndefinedProcedure {
                name,
                suggestion,
                suggestions,
                ..
            } => {
                let mut report = Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(format!("undefined procedure `{}`", name))
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message(format!("procedure `{}` not found", name))
                            .with_color(colors.next()),
                    );

                if let Some(sug) = suggestion {
                    report = report.with_note(format!("did you mean `{}`?", sug));
                } else if let Some(sugs) = suggestions
                    && !sugs.is_empty()
                {
                    let similar = sugs
                        .iter()
                        .take(3)
                        .map(|s| format!("`{}`", s))
                        .collect::<Vec<_>>()
                        .join(", ");
                    report = report.with_note(format!("similar names: {}", similar));
                }

                report.finish()
            }
            SemanticError::TypeMismatch {
                expected, found, ..
            } => Report::build(ReportKind::Error, file_path, span.start)
                .with_message(format!(
                    "type mismatch: expected {}, found {}",
                    expected, found
                ))
                .with_label(
                    Label::new((file_path, span.start..span.end))
                        .with_message(format!("found {}", found))
                        .with_color(colors.next()),
                )
                .with_note(format!("expected type: {}", expected))
                .finish(),
            SemanticError::InvalidBinaryOp {
                op,
                left_type,
                right_type,
                ..
            } => Report::build(ReportKind::Error, file_path, span.start)
                .with_message(format!(
                    "operator `{}` cannot be applied to types {} and {}",
                    op, left_type, right_type
                ))
                .with_label(
                    Label::new((file_path, span.start..span.end))
                        .with_message("invalid operator usage")
                        .with_color(colors.next()),
                )
                .finish(),
            _ => {
                // Fallback for other error types - use basic formatting
                Report::build(ReportKind::Error, file_path, span.start)
                    .with_message(error.to_string())
                    .with_label(
                        Label::new((file_path, span.start..span.end))
                            .with_message("error location")
                            .with_color(colors.next()),
                    )
                    .finish()
            }
        };

        // Write report - handle errors gracefully
        if let Err(e) = report.write((file_path, Source::from(source)), &mut output) {
            // Fallback to simple error message if ariadne fails
            use std::io::Write;
            let _ = writeln!(output, "Error formatting error message: {}", e);
            let _ = writeln!(output, "{}", error);
        }
    }

    // Convert output to string - should always be valid UTF-8 from ariadne
    String::from_utf8(output).unwrap_or_else(|_| {
        // Fallback if somehow invalid UTF-8 (shouldn't happen with ariadne)
        "Error formatting errors (invalid UTF-8 in output)".to_string()
    })
}

/// Suggests a keyword if the found token looks like a typo of the expected keyword.
fn suggest_keyword(found: &str, expected: &str) -> Option<String> {
    // Simple heuristic: if found is very similar to expected (1-2 char difference)
    // and expected is a known keyword, suggest it
    let found_upper = found.to_uppercase();
    let expected_upper = expected.to_uppercase();

    // Only suggest if they're close (Levenshtein distance <= 2)
    if levenshtein_distance(&found_upper, &expected_upper) <= 2 && found_upper != expected_upper {
        Some(expected.to_string())
    } else {
        None
    }
}

/// Simple Levenshtein distance for keyword suggestions.
///
/// Note: This duplicates the implementation in `semantic::suggestions` but is kept
/// here to avoid a dependency cycle. Consider extracting to a shared utility if needed.
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1_chars: Vec<char> = s1.chars().collect();
    let s2_chars: Vec<char> = s2.chars().collect();
    let s1_len = s1_chars.len();
    let s2_len = s2_chars.len();

    if s1_len == 0 {
        return s2_len;
    }
    if s2_len == 0 {
        return s1_len;
    }

    let mut matrix = vec![vec![0; s2_len + 1]; s1_len + 1];

    #[allow(clippy::needless_range_loop)]
    for i in 0..=s1_len {
        matrix[i][0] = i;
    }
    #[allow(clippy::needless_range_loop)]
    for j in 0..=s2_len {
        matrix[0][j] = j;
    }

    for i in 1..=s1_len {
        for j in 1..=s2_len {
            let cost = if s1_chars[i - 1] == s2_chars[j - 1] {
                0
            } else {
                1
            };
            matrix[i][j] = (matrix[i - 1][j] + 1)
                .min(matrix[i][j - 1] + 1)
                .min(matrix[i - 1][j - 1] + cost);
        }
    }

    matrix[s1_len][s2_len]
}
