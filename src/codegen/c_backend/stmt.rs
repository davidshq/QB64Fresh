//! Statement code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all statement types,
//! including control flow, I/O operations, procedure definitions, and more.
//!
//! # Loop Handling
//!
//! Loops are tracked on a stack to support EXIT statements. Each loop
//! type (FOR, WHILE, DO) generates a break label that EXIT can target.

use std::collections::HashMap;
use std::fmt::Write;

use crate::ast::{ExitType, PrintSeparator};
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{
    TypedArrayDimension, TypedCaseCompareOp, TypedCaseMatch, TypedDoCondition, TypedMember,
    TypedParameter, TypedPrintItem, TypedStatement, TypedStatementKind,
};
use crate::semantic::types::BasicType;

use super::expr::{c_function_name, emit_expr, escape_string};
use super::types::{c_identifier, c_type, default_init};

/// Context for the current loop (for EXIT statement handling).
#[derive(Clone)]
pub(super) struct LoopContext {
    /// Label to break to.
    pub break_label: String,
    /// Type of loop (For, While, Do).
    pub loop_type: ExitType,
}

/// State required for statement emission.
///
/// This is passed through recursive statement emission to track
/// indentation, loop context, and label generation.
pub(super) struct StmtEmitter {
    /// Counter for generating unique labels.
    pub label_counter: u32,
    /// Current indentation level.
    pub indent: usize,
    /// Stack of loop labels for EXIT statements.
    pub loop_stack: Vec<LoopContext>,
    /// Map of DATA labels to their indices (for RESTORE with label).
    pub data_label_indices: HashMap<String, usize>,
}

impl StmtEmitter {
    /// Creates a new statement emitter.
    pub fn new() -> Self {
        Self {
            label_counter: 0,
            indent: 0,
            loop_stack: Vec::new(),
            data_label_indices: HashMap::new(),
        }
    }

    /// Generates a unique label name.
    pub fn next_label(&mut self, prefix: &str) -> String {
        let label = format!("_qb_{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Returns the current indentation string.
    fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }

    /// Emits a statement.
    pub fn emit_stmt(
        &mut self,
        stmt: &TypedStatement,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();

        match &stmt.kind {
            TypedStatementKind::Assignment {
                name,
                value,
                target_type,
            } => {
                self.emit_assignment(&indent, name, value, target_type, output)?;
            }

            TypedStatementKind::ArrayAssignment {
                name,
                indices,
                value,
                dimensions,
                element_type,
            } => {
                self.emit_array_assignment(
                    &indent,
                    name,
                    indices,
                    value,
                    dimensions,
                    element_type,
                    output,
                )?;
            }

            TypedStatementKind::Print { items, newline } => {
                for item in items {
                    self.emit_print_item(item, output)?;
                }
                if *newline {
                    writeln!(output, "{}qb_print_newline();", indent).unwrap();
                }
            }

            TypedStatementKind::Input {
                prompt,
                show_question_mark,
                variables,
            } => {
                self.emit_input(&indent, prompt, *show_question_mark, variables, output)?;
            }

            TypedStatementKind::LineInput { prompt, variable } => {
                let c_name = c_identifier(variable);
                let prompt_arg = match prompt {
                    Some(p) => format!("\"{}\"", escape_string(p)),
                    None => "NULL".to_string(),
                };
                writeln!(
                    output,
                    "{}qb_input_string({}, &{});",
                    indent, prompt_arg, c_name
                )
                .unwrap();
            }

            TypedStatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                self.emit_if(
                    &indent,
                    condition,
                    then_branch,
                    elseif_branches,
                    else_branch,
                    output,
                )?;
            }

            TypedStatementKind::SelectCase {
                test_expr,
                cases,
                case_else,
            } => {
                self.emit_select_case(&indent, test_expr, cases, case_else, output)?;
            }

            TypedStatementKind::For {
                variable,
                var_type,
                start,
                end,
                step,
                body,
            } => {
                self.emit_for(&indent, variable, var_type, start, end, step, body, output)?;
            }

            TypedStatementKind::While { condition, body } => {
                self.emit_while(&indent, condition, body, output)?;
            }

            TypedStatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            } => {
                self.emit_do_loop(&indent, pre_condition, body, post_condition, output)?;
            }

            TypedStatementKind::Goto { target } => {
                let c_label = c_identifier(target);
                writeln!(output, "{}goto {};", indent, c_label).unwrap();
            }

            TypedStatementKind::Gosub { target } => {
                let c_label = c_identifier(target);
                writeln!(
                    output,
                    "{}/* GOSUB {} - treating as function call */",
                    indent, target
                )
                .unwrap();
                writeln!(output, "{}{}();", indent, c_label).unwrap();
            }

            TypedStatementKind::Return => {
                writeln!(output, "{}return;", indent).unwrap();
            }

            TypedStatementKind::Exit { exit_type } => {
                self.emit_exit(&indent, exit_type, output)?;
            }

            TypedStatementKind::End => {
                writeln!(output, "{}exit(0);", indent).unwrap();
            }

            TypedStatementKind::Stop => {
                writeln!(output, "{}/* STOP */", indent).unwrap();
                writeln!(output, "{}exit(1);", indent).unwrap();
            }

            TypedStatementKind::Call { name, args } => {
                let args_code: Result<Vec<_>, _> = args.iter().map(emit_expr).collect();
                let args_str = args_code?.join(", ");
                let c_name = format!("qb_sub_{}", c_identifier(name).to_lowercase());
                writeln!(output, "{}{}({});", indent, c_name, args_str).unwrap();
            }

            TypedStatementKind::SubDefinition {
                name,
                params,
                body,
                is_static: _,
            } => {
                self.emit_sub_definition(&indent, name, params, body, output)?;
            }

            TypedStatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static: _,
            } => {
                self.emit_function_definition(&indent, name, params, return_type, body, output)?;
            }

            TypedStatementKind::Dim {
                name,
                basic_type,
                dimensions,
                shared: _,
            } => {
                self.emit_dim(&indent, name, basic_type, dimensions, output)?;
            }

            TypedStatementKind::Const {
                name,
                value,
                basic_type: _,
            } => {
                let c_name = c_identifier(name);
                let value_code = emit_expr(value)?;
                writeln!(
                    output,
                    "{}const {} {} = {};",
                    indent,
                    c_type(&value.basic_type),
                    c_name,
                    value_code
                )
                .unwrap();
            }

            TypedStatementKind::Label { name } => {
                let c_label = c_identifier(name);
                writeln!(output, "{}:", c_label).unwrap();
            }

            TypedStatementKind::Comment(text) => {
                writeln!(output, "{}/* {} */", indent, text).unwrap();
            }

            TypedStatementKind::Expression(expr) => {
                let expr_code = emit_expr(expr)?;
                writeln!(output, "{}{};", indent, expr_code).unwrap();
            }

            TypedStatementKind::IncludeDirective { path } => {
                writeln!(output, "{}/* $INCLUDE: '{}' */", indent, path).unwrap();
            }

            TypedStatementKind::ConditionalBlock {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                writeln!(output, "{}/* $IF {} */", indent, condition).unwrap();
                for s in then_branch {
                    self.emit_stmt(s, output)?;
                }
                for (elseif_cond, elseif_body) in elseif_branches {
                    writeln!(output, "{}/* $ELSEIF {} */", indent, elseif_cond).unwrap();
                    for s in elseif_body {
                        self.emit_stmt(s, output)?;
                    }
                }
                if let Some(else_body) = else_branch {
                    writeln!(output, "{}/* $ELSE */", indent).unwrap();
                    for s in else_body {
                        self.emit_stmt(s, output)?;
                    }
                }
                writeln!(output, "{}/* $END IF */", indent).unwrap();
            }

            TypedStatementKind::MetaCommand { command, args } => {
                let args_str = args.as_deref().unwrap_or("");
                writeln!(output, "{}/* ${} {} */", indent, command, args_str).unwrap();
            }

            TypedStatementKind::Swap { left, right } => {
                let left_code = emit_expr(left)?;
                let right_code = emit_expr(right)?;
                let c_ty = c_type(&left.basic_type);

                let temp_var = self.next_label("swap_temp");
                writeln!(output, "{}{} {} = {};", indent, c_ty, temp_var, left_code).unwrap();
                writeln!(output, "{}{} = {};", indent, left_code, right_code).unwrap();
                writeln!(output, "{}{} = {};", indent, right_code, temp_var).unwrap();
            }

            TypedStatementKind::Continue { continue_type } => {
                let _ = continue_type;
                writeln!(output, "{}continue;", indent).unwrap();
            }

            TypedStatementKind::TypeDefinition { name, members } => {
                self.emit_type_definition(&indent, name, members, output)?;
            }

            TypedStatementKind::Data { .. } => {
                // DATA statements are handled in collect_data_values during global emission
            }

            TypedStatementKind::Read { variables } => {
                self.emit_read(&indent, variables, output)?;
            }

            TypedStatementKind::Restore { label } => {
                self.emit_restore(&indent, label, output)?;
            }
        }

        Ok(())
    }

    // Helper methods for complex statements

    fn emit_assignment(
        &self,
        indent: &str,
        name: &str,
        value: &crate::semantic::typed_ir::TypedExpr,
        target_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let value_code = emit_expr(value)?;

        if value.basic_type != *target_type {
            let c_ty = c_type(target_type);
            writeln!(output, "{}{} = ({})({});", indent, c_name, c_ty, value_code).unwrap();
        } else {
            writeln!(output, "{}{} = {};", indent, c_name, value_code).unwrap();
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_array_assignment(
        &self,
        indent: &str,
        name: &str,
        indices: &[crate::semantic::typed_ir::TypedExpr],
        value: &crate::semantic::typed_ir::TypedExpr,
        dimensions: &[TypedArrayDimension],
        element_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let value_code = emit_expr(value)?;

        let indices_code: Result<Vec<_>, _> = indices.iter().map(emit_expr).collect();
        let indices_code = indices_code?;

        let index_expr = if dimensions.is_empty() || indices_code.len() == 1 {
            if let Some(dim) = dimensions.first() {
                format!("{} - {}", indices_code[0], dim.lower)
            } else {
                indices_code[0].clone()
            }
        } else {
            let mut linear_parts = Vec::new();
            for (i, (idx, dim)) in indices_code.iter().zip(dimensions.iter()).enumerate() {
                let adjusted = format!("({} - {})", idx, dim.lower);
                if i < dimensions.len() - 1 {
                    let stride: i64 = dimensions[i + 1..]
                        .iter()
                        .map(|d| d.upper - d.lower + 1)
                        .product();
                    linear_parts.push(format!("{} * {}", adjusted, stride));
                } else {
                    linear_parts.push(adjusted);
                }
            }
            linear_parts.join(" + ")
        };

        if value.basic_type != *element_type {
            let c_ty = c_type(element_type);
            writeln!(
                output,
                "{}{}[{}] = ({})({});",
                indent, c_name, index_expr, c_ty, value_code
            )
            .unwrap();
        } else {
            writeln!(
                output,
                "{}{}[{}] = {};",
                indent, c_name, index_expr, value_code
            )
            .unwrap();
        }
        Ok(())
    }

    fn emit_input(
        &self,
        indent: &str,
        prompt: &Option<String>,
        show_question_mark: bool,
        variables: &[(String, BasicType)],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let full_prompt = match prompt {
            Some(p) => {
                if show_question_mark {
                    format!("{}? ", p)
                } else {
                    p.clone()
                }
            }
            None => {
                if show_question_mark {
                    "? ".to_string()
                } else {
                    String::new()
                }
            }
        };

        for (i, (var_name, var_type)) in variables.iter().enumerate() {
            let c_name = c_identifier(var_name);
            let prompt_arg = if i == 0 && !full_prompt.is_empty() {
                format!("\"{}\"", escape_string(&full_prompt))
            } else {
                "NULL".to_string()
            };

            if var_type.is_string() {
                writeln!(
                    output,
                    "{}qb_input_string({}, &{});",
                    indent, prompt_arg, c_name
                )
                .unwrap();
            } else if var_type.is_float() {
                writeln!(
                    output,
                    "{}qb_input_float({}, &{});",
                    indent, prompt_arg, c_name
                )
                .unwrap();
            } else {
                writeln!(
                    output,
                    "{}qb_input_int({}, &{});",
                    indent, prompt_arg, c_name
                )
                .unwrap();
            }
        }
        Ok(())
    }

    fn emit_if(
        &mut self,
        indent: &str,
        condition: &crate::semantic::typed_ir::TypedExpr,
        then_branch: &[TypedStatement],
        elseif_branches: &[(crate::semantic::typed_ir::TypedExpr, Vec<TypedStatement>)],
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

    fn emit_select_case(
        &mut self,
        indent: &str,
        test_expr: &crate::semantic::typed_ir::TypedExpr,
        cases: &[crate::semantic::typed_ir::TypedCaseClause],
        case_else: &Option<Vec<TypedStatement>>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let test_var = self.next_label("select");
        let test_code = emit_expr(test_expr)?;
        let c_ty = c_type(&test_expr.basic_type);

        writeln!(output, "{}{} {} = {};", indent, c_ty, test_var, test_code).unwrap();

        let mut first = true;
        for case in cases {
            let condition = self.emit_case_condition(&test_var, &case.matches)?;

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
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_for(
        &mut self,
        indent: &str,
        variable: &str,
        var_type: &BasicType,
        start: &crate::semantic::typed_ir::TypedExpr,
        end: &crate::semantic::typed_ir::TypedExpr,
        step: &Option<crate::semantic::typed_ir::TypedExpr>,
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

        writeln!(
            output,
            "{}for ({} {} = {}; ({} > 0) ? ({} <= {}) : ({} >= {}); {} += {}) {{",
            indent,
            c_ty,
            c_var,
            start_code,
            step_var,
            c_var,
            end_var,
            c_var,
            end_var,
            c_var,
            step_var
        )
        .unwrap();

        self.indent += 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output, "{}{}:;", indent, break_label).unwrap();

        self.loop_stack.pop();
        Ok(())
    }

    fn emit_while(
        &mut self,
        indent: &str,
        condition: &crate::semantic::typed_ir::TypedExpr,
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
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output, "{}{}:;", indent, break_label).unwrap();

        self.loop_stack.pop();
        Ok(())
    }

    fn emit_do_loop(
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

    fn emit_exit(
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
        } else {
            // EXIT SUB or EXIT FUNCTION
            writeln!(output, "{}return;", indent).unwrap();
        }
        Ok(())
    }

    fn emit_sub_definition(
        &mut self,
        indent: &str,
        name: &str,
        params: &[TypedParameter],
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = format!("qb_sub_{}", c_identifier(name).to_lowercase());
        let params_str = emit_params(params);

        writeln!(output, "{}void {}({}) {{", indent, c_name, params_str).unwrap();

        self.indent += 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output).unwrap();
        Ok(())
    }

    fn emit_function_definition(
        &mut self,
        indent: &str,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_function_name(name);
        let c_ret_type = c_type(return_type);
        let params_str = emit_params(params);

        writeln!(
            output,
            "{}{} {}({}) {{",
            indent, c_ret_type, c_name, params_str
        )
        .unwrap();

        let ret_var = c_identifier(name);
        writeln!(
            output,
            "    {} {} = {};",
            c_ret_type,
            ret_var,
            default_init(return_type)
        )
        .unwrap();

        self.indent += 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "    return {};", ret_var).unwrap();
        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output).unwrap();
        Ok(())
    }

    fn emit_dim(
        &self,
        indent: &str,
        name: &str,
        basic_type: &BasicType,
        dimensions: &[TypedArrayDimension],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let c_ty = c_type(basic_type);

        if dimensions.is_empty() {
            let init = default_init(basic_type);
            writeln!(output, "{}{} {} = {};", indent, c_ty, c_name, init).unwrap();
        } else {
            let sizes: Vec<String> = dimensions
                .iter()
                .map(|d| format!("({})", d.upper - d.lower + 1))
                .collect();
            let size_expr = sizes.join(" * ");
            writeln!(
                output,
                "{}{}* {} = malloc(sizeof({}) * {});",
                indent, c_ty, c_name, c_ty, size_expr
            )
            .unwrap();
        }
        Ok(())
    }

    fn emit_type_definition(
        &self,
        indent: &str,
        name: &str,
        members: &[TypedMember],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        writeln!(output, "{}typedef struct {} {{", indent, c_name).unwrap();

        for member in members {
            let c_member_type = c_type(&member.basic_type);
            let c_member_name = c_identifier(&member.name);

            if let BasicType::FixedString(len) = &member.basic_type {
                writeln!(output, "{}    char {}[{}];", indent, c_member_name, len + 1).unwrap();
            } else {
                writeln!(output, "{}    {} {};", indent, c_member_type, c_member_name).unwrap();
            }
        }

        writeln!(output, "{}}} {};", indent, c_name).unwrap();
        writeln!(output).unwrap();
        Ok(())
    }

    fn emit_read(
        &self,
        indent: &str,
        variables: &[(String, BasicType)],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        for (var_name, var_type) in variables {
            let c_var = c_identifier(var_name);

            match var_type {
                BasicType::String | BasicType::FixedString(_) => {
                    writeln!(
                        output,
                        "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 's') {{",
                        indent
                    )
                    .unwrap();
                    writeln!(
                        output,
                        "{}    {} = qb_str_from_c(_qb_data[_qb_data_ptr].v.s);",
                        indent, c_var
                    )
                    .unwrap();
                    writeln!(
                        output,
                        "{}}} else if (_qb_data_ptr < _qb_data_count) {{",
                        indent
                    )
                    .unwrap();
                    writeln!(
                        output,
                        "{}    {} = qb_str_float(_qb_data[_qb_data_ptr].v.n);",
                        indent, c_var
                    )
                    .unwrap();
                    writeln!(output, "{}}}", indent).unwrap();
                    writeln!(output, "{}_qb_data_ptr++;", indent).unwrap();
                }
                _ => {
                    let c_ty = c_type(var_type);
                    writeln!(
                        output,
                        "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 'd') {{",
                        indent
                    )
                    .unwrap();
                    writeln!(
                        output,
                        "{}    {} = ({})_qb_data[_qb_data_ptr].v.n;",
                        indent, c_var, c_ty
                    )
                    .unwrap();
                    writeln!(output, "{}}}", indent).unwrap();
                    writeln!(output, "{}_qb_data_ptr++;", indent).unwrap();
                }
            }
        }
        Ok(())
    }

    fn emit_restore(
        &self,
        indent: &str,
        label: &Option<String>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        match label {
            None => {
                writeln!(output, "{}_qb_data_ptr = 0;", indent).unwrap();
            }
            Some(lbl) => {
                let label_upper = lbl.to_uppercase();
                if let Some(&index) = self.data_label_indices.get(&label_upper) {
                    writeln!(
                        output,
                        "{}_qb_data_ptr = {}; /* RESTORE {} */",
                        indent, index, lbl
                    )
                    .unwrap();
                } else {
                    writeln!(
                        output,
                        "{}/* Warning: RESTORE label '{}' not associated with DATA */",
                        indent, lbl
                    )
                    .unwrap();
                    writeln!(output, "{}_qb_data_ptr = 0;", indent).unwrap();
                }
            }
        }
        Ok(())
    }

    /// Emits a PRINT item.
    fn emit_print_item(
        &self,
        item: &TypedPrintItem,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();
        let expr_code = emit_expr(&item.expr)?;

        if item.expr.basic_type.is_string() {
            writeln!(output, "{}qb_print_string({});", indent, expr_code).unwrap();
        } else if item.expr.basic_type.is_float() {
            writeln!(output, "{}qb_print_float({});", indent, expr_code).unwrap();
        } else {
            writeln!(output, "{}qb_print_int({});", indent, expr_code).unwrap();
        }

        if let Some(sep) = &item.separator {
            match sep {
                PrintSeparator::Comma => {
                    writeln!(output, "{}qb_print_tab();", indent).unwrap();
                }
                PrintSeparator::Semicolon => {
                    // No separator - items print adjacent
                }
            }
        }

        Ok(())
    }

    /// Emits a DO loop condition.
    fn emit_do_condition(&self, cond: &TypedDoCondition) -> Result<String, CodeGenError> {
        let cond_code = emit_expr(&cond.condition)?;
        if cond.is_while {
            Ok(cond_code)
        } else {
            Ok(format!("!({})", cond_code))
        }
    }

    /// Emits CASE match conditions.
    fn emit_case_condition(
        &self,
        test_var: &str,
        matches: &[TypedCaseMatch],
    ) -> Result<String, CodeGenError> {
        let conditions: Result<Vec<_>, _> = matches
            .iter()
            .map(|m| self.emit_single_case_match(test_var, m))
            .collect();
        Ok(conditions?.join(" || "))
    }

    /// Emits a single CASE match.
    fn emit_single_case_match(
        &self,
        test_var: &str,
        case_match: &TypedCaseMatch,
    ) -> Result<String, CodeGenError> {
        match case_match {
            TypedCaseMatch::Single(expr) => {
                let val = emit_expr(expr)?;
                Ok(format!("({} == {})", test_var, val))
            }
            TypedCaseMatch::Range { from, to } => {
                let from_code = emit_expr(from)?;
                let to_code = emit_expr(to)?;
                Ok(format!(
                    "({} >= {} && {} <= {})",
                    test_var, from_code, test_var, to_code
                ))
            }
            TypedCaseMatch::Comparison { op, value } => {
                let val = emit_expr(value)?;
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

/// Emits function/sub parameters.
pub(super) fn emit_params(params: &[TypedParameter]) -> String {
    if params.is_empty() {
        return "void".to_string();
    }

    params
        .iter()
        .map(|p| {
            let c_ty = c_type(&p.basic_type);
            let c_name = c_identifier(&p.name);
            if p.by_val {
                format!("{} {}", c_ty, c_name)
            } else {
                format!("{}* {}", c_ty, c_name)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}
