//! Control flow code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for control flow statements:
//! - `IF`/`ELSEIF`/`ELSE` conditionals
//! - `SELECT CASE` (including `SELECT EVERYCASE`)
//! - `FOR`/`NEXT` loops
//! - `WHILE`/`WEND` loops
//! - `DO`/`LOOP` (with `WHILE`/`UNTIL` conditions)
//! - `EXIT` statements
//!
//! All methods modify `StmtEmitter` state (indent level, loop_stack) as needed
//! for proper code generation and EXIT statement handling.

use std::fmt::Write;

use crate::ast::ExitType;
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{
    TypedCaseClause, TypedCaseCompareOp, TypedCaseMatch, TypedDoCondition, TypedExpr,
    TypedStatement,
};
use crate::semantic::types::BasicType;

use super::super::expr::emit_expr;
use super::super::types::{c_identifier, c_type};
use super::LoopContext;

impl super::StmtEmitter {
    /// Emits a STRIG event check point.
    ///
    /// This inserts code that checks for pending STRIG events and dispatches
    /// to the appropriate handler. Each check point has a unique return label
    /// so execution resumes at the right place after the handler returns.
    ///
    /// The check uses a global `_qb_strig_event_id` variable to pass the event ID
    /// to the dispatch switch, and calls `qb_strig_event_done()` after the handler
    /// returns to allow new events to fire.
    fn emit_strig_check(&mut self, indent: &str, output: &mut String) {
        let return_label = self.next_label("strig_ret");

        writeln!(output, "{}/* STRIG event check */", indent).unwrap();
        writeln!(
            output,
            "{}_qb_strig_event_id = qb_strig_check_event();",
            indent
        )
        .unwrap();
        writeln!(output, "{}if (_qb_strig_event_id) {{", indent).unwrap();
        writeln!(
            output,
            "{}    _gosub_stack[_gosub_sp++] = &&{};",
            indent, return_label
        )
        .unwrap();
        writeln!(output, "{}    goto _qb_strig_dispatch;", indent).unwrap();
        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output, "{}{}:;", indent, return_label).unwrap();
        writeln!(output, "{}qb_strig_event_done();", indent).unwrap();
    }

    /// Emits an IF/ELSEIF/ELSE statement.
    ///
    /// Generates C `if`/`else if`/`else` blocks with proper indentation
    /// and recursively emits statements in each branch.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `condition` - The IF condition expression
    /// * `then_branch` - Statements to execute if condition is true
    /// * `elseif_branches` - Zero or more (condition, body) pairs for ELSEIF
    /// * `else_branch` - Optional statements for ELSE clause
    /// * `output` - Output buffer for generated C code
    pub fn emit_if(
        &mut self,
        indent: &str,
        condition: &TypedExpr,
        then_branch: &[TypedStatement],
        elseif_branches: &[(TypedExpr, Vec<TypedStatement>)],
        else_branch: &Option<Vec<TypedStatement>>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let cond_code = emit_expr(condition)?;
        writeln!(output, "{}if ({}) {{", indent, cond_code).unwrap();

        self.indent += 1;
        for stmt in then_branch {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        for (elseif_cond, elseif_body) in elseif_branches {
            let elseif_code = emit_expr(elseif_cond)?;
            writeln!(output, "{}}} else if ({}) {{", indent, elseif_code).unwrap();

            self.indent += 1;
            for stmt in elseif_body {
                self.emit_stmt(stmt, output)?;
            }
            self.indent -= 1;
        }

        if let Some(else_body) = else_branch {
            writeln!(output, "{}}} else {{", indent).unwrap();

            self.indent += 1;
            for stmt in else_body {
                self.emit_stmt(stmt, output)?;
            }
            self.indent -= 1;
        }

        writeln!(output, "{}}}", indent).unwrap();
        Ok(())
    }

    /// Emits a SELECT CASE statement.
    ///
    /// Handles both standard SELECT CASE (first match only) and
    /// SELECT EVERYCASE (all matching cases executed).
    ///
    /// The test expression is evaluated once and stored in a temporary variable.
    /// Each CASE clause generates conditions that compare against this variable.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `test_expr` - The expression being tested
    /// * `cases` - List of CASE clauses with match conditions and body
    /// * `case_else` - Optional CASE ELSE body
    /// * `is_everycase` - True for SELECT EVERYCASE variant
    /// * `output` - Output buffer for generated C code
    pub fn emit_select_case(
        &mut self,
        indent: &str,
        test_expr: &TypedExpr,
        cases: &[TypedCaseClause],
        case_else: &Option<Vec<TypedStatement>>,
        is_everycase: bool,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let test_var = self.next_label("select");
        let test_code = emit_expr(test_expr)?;
        let c_ty = c_type(&test_expr.basic_type);

        writeln!(output, "{}{} {} = {};", indent, c_ty, test_var, test_code).unwrap();

        if is_everycase {
            // SELECT EVERYCASE: evaluate ALL cases and execute ALL matching ones
            // Also track if any case matched for CASE ELSE
            let matched_var = self.next_label("matched");
            writeln!(output, "{}int {} = 0;", indent, matched_var).unwrap();

            for case in cases {
                let condition =
                    self.emit_case_condition(&test_var, &case.matches, &test_expr.basic_type)?;
                writeln!(output, "{}if ({}) {{", indent, condition).unwrap();
                writeln!(output, "{}    {} = 1;", indent, matched_var).unwrap();

                self.indent += 1;
                for stmt in &case.body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;
                writeln!(output, "{}}}", indent).unwrap();
            }

            // CASE ELSE: only execute if no cases matched
            if let Some(else_body) = case_else {
                writeln!(output, "{}if (!{}) {{", indent, matched_var).unwrap();

                self.indent += 1;
                for stmt in else_body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;
                writeln!(output, "{}}}", indent).unwrap();
            }
        } else {
            // Standard SELECT CASE: execute first matching case only
            let mut first = true;
            for case in cases {
                let condition =
                    self.emit_case_condition(&test_var, &case.matches, &test_expr.basic_type)?;

                if first {
                    writeln!(output, "{}if ({}) {{", indent, condition).unwrap();
                    first = false;
                } else {
                    writeln!(output, "{}}} else if ({}) {{", indent, condition).unwrap();
                }

                self.indent += 1;
                for stmt in &case.body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;
            }

            if let Some(else_body) = case_else {
                writeln!(output, "{}}} else {{", indent).unwrap();

                self.indent += 1;
                for stmt in else_body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;
            }

            if !first {
                writeln!(output, "{}}}", indent).unwrap();
            }
        }
        Ok(())
    }

    /// Emits a FOR/NEXT loop.
    ///
    /// The loop variable retains its value after the loop ends (BASIC semantics).
    /// Start, end, and step values are evaluated once before the loop begins.
    /// A break label is generated for EXIT FOR support.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `variable` - Loop counter variable name
    /// * `var_type` - Type of the loop variable
    /// * `start` - Initial value expression
    /// * `end` - End value expression
    /// * `step` - Optional step value (defaults to 1)
    /// * `body` - Statements inside the loop
    /// * `output` - Output buffer for generated C code
    #[allow(clippy::too_many_arguments)]
    pub fn emit_for(
        &mut self,
        indent: &str,
        variable: &str,
        var_type: &BasicType,
        start: &TypedExpr,
        end: &TypedExpr,
        step: &Option<TypedExpr>,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_var = c_identifier(variable);
        let c_ty = c_type(var_type);
        let start_code = emit_expr(start)?;
        let end_code = emit_expr(end)?;
        let step_code = match step {
            Some(s) => emit_expr(s)?,
            None => "1".to_string(),
        };

        let break_label = self.next_label("for_end");
        self.loop_stack.push(LoopContext {
            break_label: break_label.clone(),
            loop_type: ExitType::For,
        });

        let end_var = self.next_label("for_end_val");
        let step_var = self.next_label("for_step");
        writeln!(output, "{}{} {} = {};", indent, c_ty, end_var, end_code).unwrap();
        writeln!(output, "{}{} {} = {};", indent, c_ty, step_var, step_code).unwrap();

        // In BASIC, the FOR loop variable retains its value after the loop ends.
        // We assign the start value before the loop and use the existing variable,
        // rather than declaring a new variable in the for statement (which would
        // create a shadowing local that loses its value after the loop).
        writeln!(output, "{}{} = {};", indent, c_var, start_code).unwrap();
        writeln!(
            output,
            "{}for (; ({} > 0) ? ({} <= {}) : ({} >= {}); {} += {}) {{",
            indent, step_var, c_var, end_var, c_var, end_var, c_var, step_var
        )
        .unwrap();

        self.indent += 1;
        // STRIG event check at loop iteration
        let inner_indent = "    ".repeat(self.indent);
        self.emit_strig_check(&inner_indent, output);
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output, "{}{}:;", indent, break_label).unwrap();

        self.loop_stack.pop();
        Ok(())
    }

    /// Emits a WHILE/WEND loop.
    ///
    /// Generates a C `while` loop with a break label for EXIT WHILE support.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `condition` - Loop condition expression
    /// * `body` - Statements inside the loop
    /// * `output` - Output buffer for generated C code
    pub fn emit_while(
        &mut self,
        indent: &str,
        condition: &TypedExpr,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let cond_code = emit_expr(condition)?;
        let break_label = self.next_label("while_end");

        self.loop_stack.push(LoopContext {
            break_label: break_label.clone(),
            loop_type: ExitType::While,
        });

        writeln!(output, "{}while ({}) {{", indent, cond_code).unwrap();

        self.indent += 1;
        // STRIG event check at loop iteration
        let inner_indent = "    ".repeat(self.indent);
        self.emit_strig_check(&inner_indent, output);
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output, "{}{}:;", indent, break_label).unwrap();

        self.loop_stack.pop();
        Ok(())
    }

    /// Emits a DO/LOOP statement.
    ///
    /// Handles all DO loop variants:
    /// - `DO WHILE ... LOOP` (pre-condition)
    /// - `DO UNTIL ... LOOP` (pre-condition, inverted)
    /// - `DO ... LOOP WHILE` (post-condition)
    /// - `DO ... LOOP UNTIL` (post-condition, inverted)
    /// - `DO ... LOOP` (infinite loop)
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `pre_condition` - Optional condition checked before loop body
    /// * `body` - Statements inside the loop
    /// * `post_condition` - Optional condition checked after loop body
    /// * `output` - Output buffer for generated C code
    pub fn emit_do_loop(
        &mut self,
        indent: &str,
        pre_condition: &Option<TypedDoCondition>,
        body: &[TypedStatement],
        post_condition: &Option<TypedDoCondition>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let break_label = self.next_label("do_end");

        self.loop_stack.push(LoopContext {
            break_label: break_label.clone(),
            loop_type: ExitType::Do,
        });

        match (pre_condition, post_condition) {
            (Some(pre), None) => {
                let cond = self.emit_do_condition(pre)?;
                writeln!(output, "{}while ({}) {{", indent, cond).unwrap();
            }
            (None, Some(_post)) => {
                writeln!(output, "{}do {{", indent).unwrap();
            }
            (None, None) => {
                writeln!(output, "{}for (;;) {{", indent).unwrap();
            }
            (Some(_), Some(_)) => {
                return Err(CodeGenError::internal(
                    "DO loop cannot have both pre and post conditions",
                ));
            }
        }

        self.indent += 1;
        // STRIG event check at loop iteration
        let inner_indent = "    ".repeat(self.indent);
        self.emit_strig_check(&inner_indent, output);
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        if let Some(post) = post_condition {
            let cond = self.emit_do_condition(post)?;
            writeln!(output, "{}}} while ({});", indent, cond).unwrap();
        } else {
            writeln!(output, "{}}}", indent).unwrap();
        }

        writeln!(output, "{}{}:;", indent, break_label).unwrap();
        self.loop_stack.pop();
        Ok(())
    }

    /// Emits an EXIT statement.
    ///
    /// EXIT FOR/WHILE/DO generates a `goto` to the appropriate break label.
    /// EXIT FUNCTION returns the function's return variable.
    /// EXIT SUB generates a plain `return`.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `exit_type` - The type of construct to exit (For, While, Do, Sub, Function)
    /// * `output` - Output buffer for generated C code
    pub fn emit_exit(
        &self,
        indent: &str,
        exit_type: &ExitType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let label = self
            .loop_stack
            .iter()
            .rev()
            .find(|ctx| ctx.loop_type == *exit_type)
            .map(|ctx| ctx.break_label.clone());

        if let Some(label) = label {
            writeln!(output, "{}goto {};", indent, label).unwrap();
        } else if let Some(ret_var) = &self.current_func_ret_var {
            // EXIT FUNCTION - return the function's return variable
            writeln!(output, "{}return {};", indent, ret_var).unwrap();
        } else {
            // EXIT SUB - just return
            writeln!(output, "{}return;", indent).unwrap();
        }
        Ok(())
    }

    /// Emits a DO loop condition.
    ///
    /// For WHILE conditions, returns the condition as-is.
    /// For UNTIL conditions, wraps the condition with `!()`.
    ///
    /// # Arguments
    ///
    /// * `cond` - The DO condition with its type (WHILE or UNTIL)
    ///
    /// # Returns
    ///
    /// The C condition expression string.
    pub fn emit_do_condition(&self, cond: &TypedDoCondition) -> Result<String, CodeGenError> {
        let cond_code = emit_expr(&cond.condition)?;
        if cond.is_while {
            Ok(cond_code)
        } else {
            Ok(format!("!({})", cond_code))
        }
    }

    /// Emits CASE match conditions.
    ///
    /// Combines multiple CASE matches with `||` (OR).
    /// Each match can be a single value, range, or comparison.
    ///
    /// # Arguments
    ///
    /// * `test_var` - Name of the temporary variable holding the SELECT expression
    /// * `matches` - List of match conditions for this CASE
    /// * `test_type` - Type of the test expression (affects string comparison)
    ///
    /// # Returns
    ///
    /// A C condition expression string.
    pub fn emit_case_condition(
        &self,
        test_var: &str,
        matches: &[TypedCaseMatch],
        test_type: &BasicType,
    ) -> Result<String, CodeGenError> {
        let conditions: Result<Vec<_>, _> = matches
            .iter()
            .map(|m| self.emit_single_case_match(test_var, m, test_type))
            .collect();
        Ok(conditions?.join(" || "))
    }

    /// Emits a single CASE match.
    ///
    /// Handles three match types:
    /// - `CASE value` - equality comparison
    /// - `CASE value1 TO value2` - range comparison
    /// - `CASE IS op value` - relational comparison
    ///
    /// String comparisons use `qb_string_compare()` runtime function.
    ///
    /// # Arguments
    ///
    /// * `test_var` - Name of the temporary variable holding the SELECT expression
    /// * `case_match` - The match condition
    /// * `test_type` - Type of the test expression
    ///
    /// # Returns
    ///
    /// A C condition expression string.
    pub fn emit_single_case_match(
        &self,
        test_var: &str,
        case_match: &TypedCaseMatch,
        test_type: &BasicType,
    ) -> Result<String, CodeGenError> {
        let is_string = test_type.is_string();

        match case_match {
            TypedCaseMatch::Single(expr) => {
                let val = emit_expr(expr)?;
                if is_string {
                    // String comparison: use qb_string_compare
                    Ok(format!("(qb_string_compare({}, {}) == 0)", test_var, val))
                } else {
                    Ok(format!("({} == {})", test_var, val))
                }
            }
            TypedCaseMatch::Range { from, to } => {
                let from_code = emit_expr(from)?;
                let to_code = emit_expr(to)?;
                if is_string {
                    // String range: lexicographic comparison
                    Ok(format!(
                        "(qb_string_compare({}, {}) >= 0 && qb_string_compare({}, {}) <= 0)",
                        test_var, from_code, test_var, to_code
                    ))
                } else {
                    Ok(format!(
                        "({} >= {} && {} <= {})",
                        test_var, from_code, test_var, to_code
                    ))
                }
            }
            TypedCaseMatch::Comparison { op, value } => {
                let val = emit_expr(value)?;
                if is_string {
                    // String comparison: use qb_string_compare result
                    let cmp_expr = format!("qb_string_compare({}, {})", test_var, val);
                    let c_op = match op {
                        TypedCaseCompareOp::Equal => "== 0",
                        TypedCaseCompareOp::NotEqual => "!= 0",
                        TypedCaseCompareOp::LessThan => "< 0",
                        TypedCaseCompareOp::LessEqual => "<= 0",
                        TypedCaseCompareOp::GreaterThan => "> 0",
                        TypedCaseCompareOp::GreaterEqual => ">= 0",
                    };
                    Ok(format!("({} {})", cmp_expr, c_op))
                } else {
                    let c_op = match op {
                        TypedCaseCompareOp::Equal => "==",
                        TypedCaseCompareOp::NotEqual => "!=",
                        TypedCaseCompareOp::LessThan => "<",
                        TypedCaseCompareOp::LessEqual => "<=",
                        TypedCaseCompareOp::GreaterThan => ">",
                        TypedCaseCompareOp::GreaterEqual => ">=",
                    };
                    Ok(format!("({} {} {})", test_var, c_op, val))
                }
            }
        }
    }
}
