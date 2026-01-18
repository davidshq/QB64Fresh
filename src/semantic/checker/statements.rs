//! Statement type checking dispatcher.
//!
//! This module contains the main `check_statement` method that dispatches
//! to appropriate handlers based on statement type. Simple pass-through
//! statements are handled directly here, while complex statements delegate
//! to specialized modules.

use crate::ast::{ArrayDimension, DataValue, PrintItem, Statement, StatementKind, ViewCoords};
use crate::semantic::{
    error::SemanticError,
    symbols::{ConstValue, ScopeKind, Symbol, SymbolKind, UserTypeDefinition, UserTypeMember},
    typed_ir::*,
    types::{BasicType, from_type_spec, type_from_suffix},
};

use super::{ForLoopInfo, TypeChecker};

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // Statement Type Checking
    // ========================================================================

    /// Type checks a statement.
    pub fn check_statement(&mut self, stmt: &Statement) -> TypedStatement {
        match &stmt.kind {
            StatementKind::Let { name, value } => self.check_assignment(name, value, stmt.span),

            StatementKind::ArrayAssignment {
                name,
                indices,
                value,
            } => self.check_array_assignment(name, indices, value, stmt.span),

            StatementKind::Print { values, newline } => {
                self.check_print(values, *newline, stmt.span)
            }

            StatementKind::Input {
                prompt,
                show_question_mark,
                variables,
            } => self.check_input(prompt, *show_question_mark, variables, stmt.span),

            StatementKind::LineInput { prompt, variable } => {
                self.check_line_input(prompt, variable, stmt.span)
            }

            StatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => self.check_if(
                condition,
                then_branch,
                elseif_branches,
                else_branch,
                stmt.span,
            ),

            StatementKind::SelectCase {
                test_expr,
                cases,
                case_else,
            } => self.check_select_case(test_expr, cases, case_else, stmt.span),

            StatementKind::For {
                variable,
                start,
                end,
                step,
                body,
                next_variable,
            } => self.check_for(ForLoopInfo {
                variable,
                start,
                end,
                step,
                body,
                next_variable,
                span: stmt.span,
            }),

            StatementKind::While { condition, body } => {
                self.check_while(condition, body, stmt.span)
            }

            StatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            } => self.check_do_loop(pre_condition, body, post_condition, stmt.span),

            StatementKind::Goto { target } => self.check_goto(target, stmt.span),

            StatementKind::Gosub { target } => self.check_gosub(target, stmt.span),

            StatementKind::Return => self.check_return(stmt.span),

            StatementKind::Exit { exit_type } => self.check_exit(*exit_type, stmt.span),

            StatementKind::End => TypedStatement::new(TypedStatementKind::End, stmt.span),

            StatementKind::Stop => TypedStatement::new(TypedStatementKind::Stop, stmt.span),

            StatementKind::Call { name, args } => self.check_call(name, args, stmt.span),

            StatementKind::Dim {
                name,
                dimensions,
                type_spec,
                shared,
            } => self.check_dim(name, dimensions, type_spec, *shared, stmt.span),

            StatementKind::Const { name, value } => self.check_const(name, value, stmt.span),

            StatementKind::Label { name } => {
                TypedStatement::new(TypedStatementKind::Label { name: name.clone() }, stmt.span)
            }

            StatementKind::SubDefinition {
                name,
                params,
                body,
                is_static,
            } => self.check_sub_definition(name, params, body, *is_static, stmt.span),

            StatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static,
            } => self.check_function_definition(
                name,
                params,
                return_type,
                body,
                *is_static,
                stmt.span,
            ),

            StatementKind::Expression(expr) => {
                let typed_expr = self.check_expr(expr);
                TypedStatement::new(TypedStatementKind::Expression(typed_expr), stmt.span)
            }

            StatementKind::Comment(text) => {
                TypedStatement::new(TypedStatementKind::Comment(text.clone()), stmt.span)
            }

            // Preprocessor directives - pass through as-is for later processing
            StatementKind::IncludeDirective { path } => TypedStatement::new(
                TypedStatementKind::IncludeDirective { path: path.clone() },
                stmt.span,
            ),

            StatementKind::ConditionalBlock {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                // Type check statements in all branches
                let typed_then = then_branch
                    .iter()
                    .map(|s| self.check_statement(s))
                    .collect();
                let typed_elseif = elseif_branches
                    .iter()
                    .map(|(cond, body)| {
                        let typed_body = body.iter().map(|s| self.check_statement(s)).collect();
                        (cond.clone(), typed_body)
                    })
                    .collect();
                let typed_else = else_branch
                    .as_ref()
                    .map(|body| body.iter().map(|s| self.check_statement(s)).collect());

                TypedStatement::new(
                    TypedStatementKind::ConditionalBlock {
                        condition: condition.clone(),
                        then_branch: typed_then,
                        elseif_branches: typed_elseif,
                        else_branch: typed_else,
                    },
                    stmt.span,
                )
            }

            StatementKind::MetaCommand { command, args } => TypedStatement::new(
                TypedStatementKind::MetaCommand {
                    command: command.clone(),
                    args: args.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaLet { name, value } => TypedStatement::new(
                TypedStatementKind::MetaLet {
                    name: name.clone(),
                    value: *value,
                },
                stmt.span,
            ),

            StatementKind::MetaChecking { enabled } => TypedStatement::new(
                TypedStatementKind::MetaChecking { enabled: *enabled },
                stmt.span,
            ),

            StatementKind::Swap { left, right } => {
                let typed_left = self.check_expr(left);
                let typed_right = self.check_expr(right);

                // Check that both expressions are lvalues (variables or array elements)
                // For now, we'll verify at codegen; semantic check ensures exact type match
                // SWAP requires exact type match to prevent silent data loss
                // (e.g., swapping INTEGER and LONG would truncate the LONG value)
                if typed_left.basic_type != typed_right.basic_type {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: typed_left.basic_type.to_string(),
                        found: typed_right.basic_type.to_string(),
                        span: stmt.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::Swap {
                        left: typed_left,
                        right: typed_right,
                    },
                    stmt.span,
                )
            }

            StatementKind::Continue { continue_type } => {
                use crate::ast::ContinueType;

                // Validate that CONTINUE is inside the matching loop type
                let valid = match continue_type {
                    ContinueType::For => self.loop_context.for_depth > 0,
                    ContinueType::While => self.loop_context.while_depth > 0,
                    ContinueType::Do => self.loop_context.do_depth > 0,
                };

                if !valid {
                    let loop_name = match continue_type {
                        ContinueType::For => "FOR",
                        ContinueType::While => "WHILE",
                        ContinueType::Do => "DO",
                    };
                    self.errors.push(SemanticError::ContinueOutsideLoop {
                        loop_type: loop_name.to_string(),
                        span: stmt.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::Continue {
                        continue_type: *continue_type,
                    },
                    stmt.span,
                )
            }

            StatementKind::TypeDefinition { name, members } => {
                // Convert AST type members to semantic type members
                let typed_members: Vec<TypedMember> = members
                    .iter()
                    .map(|m| TypedMember {
                        name: m.name.clone(),
                        basic_type: from_type_spec(&m.type_spec),
                    })
                    .collect();

                // Register the type definition in the symbol table
                let user_type = UserTypeDefinition {
                    name: name.clone(),
                    members: typed_members
                        .iter()
                        .map(|m| UserTypeMember {
                            name: m.name.clone(),
                            basic_type: m.basic_type.clone(),
                        })
                        .collect(),
                    span: stmt.span,
                };

                if let Err(_existing) = self.symbols.define_user_type(user_type) {
                    self.errors.push(SemanticError::DuplicateType {
                        name: name.clone(),
                        original_span: stmt.span, // Could track the original definition span
                        duplicate_span: stmt.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::TypeDefinition {
                        name: name.clone(),
                        members: typed_members,
                    },
                    stmt.span,
                )
            }

            StatementKind::Data { values } => {
                // Convert AST data values to typed data values
                let typed_values: Vec<TypedDataValue> = values
                    .iter()
                    .map(|v| match v {
                        DataValue::Integer(n) => TypedDataValue::Integer(*n),
                        DataValue::Float(f) => TypedDataValue::Float(*f),
                        DataValue::String(s) => TypedDataValue::String(s.clone()),
                    })
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::Data {
                        values: typed_values,
                    },
                    stmt.span,
                )
            }

            StatementKind::Read { variables } => {
                // Look up each variable and get its type
                // Variables that don't exist are auto-declared based on suffix
                let typed_vars: Vec<(String, BasicType)> = variables
                    .iter()
                    .map(|var_name| {
                        let var_type = if let Some(symbol) = self.symbols.lookup_symbol(var_name) {
                            symbol.basic_type.clone()
                        } else {
                            // Infer type from suffix or default
                            let inferred = type_from_suffix(var_name)
                                .unwrap_or_else(|| self.symbols.default_type_for(var_name));

                            // Define the variable
                            let symbol = Symbol {
                                name: var_name.clone(),
                                kind: SymbolKind::Variable,
                                basic_type: inferred.clone(),
                                span: stmt.span,
                                is_mutable: true,
                            };
                            let _ = self.symbols.define_symbol(symbol);

                            inferred
                        };
                        (var_name.clone(), var_type)
                    })
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::Read {
                        variables: typed_vars,
                    },
                    stmt.span,
                )
            }

            StatementKind::Restore { label } => {
                // RESTORE with optional label - no semantic validation needed here
                // (label validation could be done during codegen or in a later pass)
                TypedStatement::new(
                    TypedStatementKind::Restore {
                        label: label.clone(),
                    },
                    stmt.span,
                )
            }

            // ==================== File I/O Statements ====================
            StatementKind::OpenFile {
                filename,
                mode,
                access,
                lock,
                file_num,
                record_len,
            } => {
                let typed_filename = self.check_expr(filename);
                let typed_file_num = self.check_expr(file_num);
                let typed_record_len = record_len.as_ref().map(|e| self.check_expr(e));

                // Filename should be a string
                if typed_filename.basic_type != BasicType::String {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "String".to_string(),
                        found: typed_filename.basic_type.to_string(),
                        span: typed_filename.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::OpenFile {
                        filename: typed_filename,
                        mode: *mode,
                        access: *access,
                        lock: *lock,
                        file_num: typed_file_num,
                        record_len: typed_record_len,
                    },
                    stmt.span,
                )
            }

            StatementKind::CloseFile { file_nums } => {
                let typed_file_nums: Vec<TypedExpr> =
                    file_nums.iter().map(|e| self.check_expr(e)).collect();

                TypedStatement::new(
                    TypedStatementKind::CloseFile {
                        file_nums: typed_file_nums,
                    },
                    stmt.span,
                )
            }

            StatementKind::FilePrint {
                file_num,
                values,
                newline,
            } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_items = self.check_print_items(values);

                TypedStatement::new(
                    TypedStatementKind::FilePrint {
                        file_num: typed_file_num,
                        items: typed_items,
                        newline: *newline,
                    },
                    stmt.span,
                )
            }

            StatementKind::FileWrite { file_num, values } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_values: Vec<TypedExpr> =
                    values.iter().map(|e| self.check_expr(e)).collect();

                TypedStatement::new(
                    TypedStatementKind::FileWrite {
                        file_num: typed_file_num,
                        values: typed_values,
                    },
                    stmt.span,
                )
            }

            StatementKind::FileInput {
                file_num,
                variables,
            } => {
                let typed_file_num = self.check_expr(file_num);

                // Look up each variable and get its type (similar to READ)
                let typed_vars: Vec<(String, BasicType)> = variables
                    .iter()
                    .map(|var_name| {
                        let var_type = if let Some(symbol) = self.symbols.lookup_symbol(var_name) {
                            symbol.basic_type.clone()
                        } else {
                            // Infer type from suffix or default
                            let inferred = type_from_suffix(var_name)
                                .unwrap_or_else(|| self.symbols.default_type_for(var_name));

                            // Define the variable
                            let symbol = Symbol {
                                name: var_name.clone(),
                                kind: SymbolKind::Variable,
                                basic_type: inferred.clone(),
                                span: stmt.span,
                                is_mutable: true,
                            };
                            let _ = self.symbols.define_symbol(symbol);

                            inferred
                        };
                        (var_name.clone(), var_type)
                    })
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::FileInput {
                        file_num: typed_file_num,
                        variables: typed_vars,
                    },
                    stmt.span,
                )
            }

            StatementKind::FileLineInput { file_num, variable } => {
                let typed_file_num = self.check_expr(file_num);

                // Ensure variable is defined as string or define it
                if self.symbols.lookup_symbol(variable).is_none() {
                    let symbol = Symbol {
                        name: variable.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: BasicType::String,
                        span: stmt.span,
                        is_mutable: true,
                    };
                    let _ = self.symbols.define_symbol(symbol);
                }

                TypedStatement::new(
                    TypedStatementKind::FileLineInput {
                        file_num: typed_file_num,
                        variable: variable.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::FileGet {
                file_num,
                position,
                variable,
            } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_position = position.as_ref().map(|e| self.check_expr(e));

                // Look up variable type
                let var_type = if let Some(symbol) = self.symbols.lookup_symbol(variable) {
                    symbol.basic_type.clone()
                } else {
                    // Infer and define
                    let inferred = type_from_suffix(variable)
                        .unwrap_or_else(|| self.symbols.default_type_for(variable));

                    let symbol = Symbol {
                        name: variable.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: inferred.clone(),
                        span: stmt.span,
                        is_mutable: true,
                    };
                    let _ = self.symbols.define_symbol(symbol);
                    inferred
                };

                TypedStatement::new(
                    TypedStatementKind::FileGet {
                        file_num: typed_file_num,
                        position: typed_position,
                        variable: variable.clone(),
                        var_type,
                    },
                    stmt.span,
                )
            }

            StatementKind::FilePut {
                file_num,
                position,
                variable,
            } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_position = position.as_ref().map(|e| self.check_expr(e));

                // Look up variable type - auto-declare if not found (consistent with FileGet)
                // In BASIC, variables don't need explicit declaration; PUT on an undefined
                // variable writes the default value (0 for numeric, "" for string)
                let var_type = if let Some(symbol) = self.symbols.lookup_symbol(variable) {
                    symbol.basic_type.clone()
                } else {
                    // Infer and define (consistent with FileGet behavior)
                    let inferred = type_from_suffix(variable)
                        .unwrap_or_else(|| self.symbols.default_type_for(variable));

                    let symbol = Symbol {
                        name: variable.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: inferred.clone(),
                        span: stmt.span,
                        is_mutable: true,
                    };
                    let _ = self.symbols.define_symbol(symbol);
                    inferred
                };

                TypedStatement::new(
                    TypedStatementKind::FilePut {
                        file_num: typed_file_num,
                        position: typed_position,
                        variable: variable.clone(),
                        var_type,
                    },
                    stmt.span,
                )
            }

            StatementKind::FileSeek { file_num, position } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_position = self.check_expr(position);

                TypedStatement::new(
                    TypedStatementKind::FileSeek {
                        file_num: typed_file_num,
                        position: typed_position,
                    },
                    stmt.span,
                )
            }

            // ==================== Error Handling Statements ====================
            StatementKind::OnErrorGoto { target } => TypedStatement::new(
                TypedStatementKind::OnErrorGoto {
                    target: target.clone(),
                },
                stmt.span,
            ),

            StatementKind::OnErrorResumeNext => {
                TypedStatement::new(TypedStatementKind::OnErrorResumeNext, stmt.span)
            }

            StatementKind::ResumeStmt { target } => TypedStatement::new(
                TypedStatementKind::ResumeStmt {
                    target: target.clone(),
                },
                stmt.span,
            ),

            StatementKind::ErrorStmt { code } => {
                let typed_code = self.check_expr(code);

                TypedStatement::new(
                    TypedStatementKind::ErrorStmt { code: typed_code },
                    stmt.span,
                )
            }

            // ==================== Computed Control Flow ====================
            StatementKind::OnGoto { selector, targets } => {
                let typed_selector = self.check_expr(selector);

                // Selector should be numeric
                if !typed_selector.basic_type.is_numeric() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "numeric".to_string(),
                        found: typed_selector.basic_type.to_string(),
                        span: typed_selector.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::OnGoto {
                        selector: typed_selector,
                        targets: targets.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::OnGosub { selector, targets } => {
                let typed_selector = self.check_expr(selector);

                if !typed_selector.basic_type.is_numeric() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "numeric".to_string(),
                        found: typed_selector.basic_type.to_string(),
                        span: typed_selector.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::OnGosub {
                        selector: typed_selector,
                        targets: targets.clone(),
                    },
                    stmt.span,
                )
            }

            // ==================== DEF FN ====================
            StatementKind::DefFn { name, params, body } => {
                // Enter a new scope for the function
                self.symbols.enter_scope(ScopeKind::Function);

                // Define parameters in the scope
                let typed_params: Vec<TypedParameter> = params
                    .iter()
                    .map(|p| {
                        let param_type = p
                            .type_spec
                            .as_ref()
                            .map(from_type_spec)
                            .or_else(|| type_from_suffix(&p.name))
                            .unwrap_or(BasicType::Single); // DEF FN defaults to Single
                        let symbol = Symbol {
                            name: p.name.clone(),
                            kind: SymbolKind::Variable,
                            basic_type: param_type.clone(),
                            span: stmt.span,
                            is_mutable: !p.by_val,
                        };
                        let _ = self.symbols.define_symbol(symbol);

                        TypedParameter {
                            name: p.name.clone(),
                            basic_type: param_type,
                            by_val: p.by_val,
                        }
                    })
                    .collect();

                // Check the body expression
                let typed_body = self.check_expr(body);

                // Return type is inferred from the function name suffix or body
                let return_type =
                    type_from_suffix(name).unwrap_or_else(|| typed_body.basic_type.clone());

                self.symbols.exit_scope();

                TypedStatement::new(
                    TypedStatementKind::DefFn {
                        name: name.clone(),
                        params: typed_params,
                        return_type,
                        body: typed_body,
                    },
                    stmt.span,
                )
            }

            // ==================== Variable/Scope Statements ====================
            StatementKind::CommonStmt { shared, variables } => {
                let typed_vars: Vec<TypedCommonVariable> = variables
                    .iter()
                    .map(|v| {
                        let var_type = v
                            .type_spec
                            .as_ref()
                            .map(from_type_spec)
                            .or_else(|| type_from_suffix(&v.name))
                            .unwrap_or_else(|| self.symbols.default_type_for(&v.name));

                        // Evaluate dimensions
                        let dims: Vec<TypedArrayDimension> = v
                            .dimensions
                            .iter()
                            .map(|d| self.evaluate_array_dimension(d, stmt.span))
                            .collect();

                        // Register the symbol
                        let symbol = Symbol {
                            name: v.name.clone(),
                            kind: SymbolKind::Variable,
                            basic_type: if dims.is_empty() {
                                var_type.clone()
                            } else {
                                BasicType::Array {
                                    element_type: Box::new(var_type.clone()),
                                    dimensions: dims.len(),
                                }
                            },
                            span: stmt.span,
                            is_mutable: true,
                        };
                        let _ = self.symbols.define_symbol(symbol);

                        TypedCommonVariable {
                            name: v.name.clone(),
                            basic_type: var_type,
                            dimensions: dims,
                        }
                    })
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::CommonStmt {
                        shared: *shared,
                        variables: typed_vars,
                    },
                    stmt.span,
                )
            }

            StatementKind::Redim {
                preserve,
                name,
                dimensions,
                type_spec,
            } => {
                // Determine element type
                let element_type = type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(name))
                    .unwrap_or_else(|| self.symbols.default_type_for(name));

                // Evaluate dimensions - REDIM allows runtime expressions
                let typed_dims: Vec<TypedArrayDimension> = dimensions
                    .iter()
                    .map(|d| self.evaluate_array_dimension_runtime(d))
                    .collect();

                // Update symbol table (or define if not exists)
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Variable,
                    basic_type: BasicType::Array {
                        element_type: Box::new(element_type.clone()),
                        dimensions: typed_dims.len(),
                    },
                    span: stmt.span,
                    is_mutable: true,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedStatement::new(
                    TypedStatementKind::Redim {
                        preserve: *preserve,
                        name: name.clone(),
                        element_type,
                        dimensions: typed_dims,
                    },
                    stmt.span,
                )
            }

            // ==================== Graphics Statements ====================
            StatementKind::Screen { mode } => {
                let typed_mode = self.check_expr(mode);
                TypedStatement::new(TypedStatementKind::Screen { mode: typed_mode }, stmt.span)
            }

            StatementKind::Cls => TypedStatement::new(TypedStatementKind::Cls, stmt.span),

            StatementKind::Color {
                foreground,
                background,
            } => {
                let typed_fg = self.check_expr(foreground);
                let typed_bg = background.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Color {
                        foreground: typed_fg,
                        background: typed_bg,
                    },
                    stmt.span,
                )
            }

            StatementKind::Locate { row, col } => {
                let typed_row = self.check_expr(row);
                let typed_col = self.check_expr(col);
                TypedStatement::new(
                    TypedStatementKind::Locate {
                        row: typed_row,
                        col: typed_col,
                    },
                    stmt.span,
                )
            }

            StatementKind::Pset { x, y, color } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Pset {
                        x: typed_x,
                        y: typed_y,
                        color: typed_color,
                    },
                    stmt.span,
                )
            }

            StatementKind::Preset { x, y } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                TypedStatement::new(
                    TypedStatementKind::Preset {
                        x: typed_x,
                        y: typed_y,
                    },
                    stmt.span,
                )
            }

            StatementKind::Line {
                x1,
                y1,
                x2,
                y2,
                color,
                box_style,
            } => {
                let typed_x1 = x1.as_ref().map(|e| self.check_expr(e));
                let typed_y1 = y1.as_ref().map(|e| self.check_expr(e));
                let typed_x2 = self.check_expr(x2);
                let typed_y2 = self.check_expr(y2);
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Line {
                        x1: typed_x1,
                        y1: typed_y1,
                        x2: typed_x2,
                        y2: typed_y2,
                        color: typed_color,
                        box_style: *box_style,
                    },
                    stmt.span,
                )
            }

            StatementKind::Circle {
                x,
                y,
                radius,
                color,
                filled,
            } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                let typed_radius = self.check_expr(radius);
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Circle {
                        x: typed_x,
                        y: typed_y,
                        radius: typed_radius,
                        color: typed_color,
                        filled: *filled,
                    },
                    stmt.span,
                )
            }

            StatementKind::Paint {
                x,
                y,
                color,
                border,
            } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                let typed_border = border.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Paint {
                        x: typed_x,
                        y: typed_y,
                        color: typed_color,
                        border: typed_border,
                    },
                    stmt.span,
                )
            }

            StatementKind::GfxDisplay => {
                TypedStatement::new(TypedStatementKind::GfxDisplay, stmt.span)
            }

            // ==================== Additional Graphics Statements ====================
            StatementKind::Width { columns, rows } => {
                let typed_columns = self.check_expr(columns);
                let typed_rows = rows.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Width {
                        columns: typed_columns,
                        rows: typed_rows,
                    },
                    stmt.span,
                )
            }

            StatementKind::View {
                screen,
                coords,
                fill_color,
                border_color,
            } => {
                let typed_coords = coords.as_ref().map(|c| self.check_view_coords(c));
                let typed_fill = fill_color.as_ref().map(|e| self.check_expr(e));
                let typed_border = border_color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::View {
                        screen: *screen,
                        coords: typed_coords,
                        fill_color: typed_fill,
                        border_color: typed_border,
                    },
                    stmt.span,
                )
            }

            StatementKind::WindowCoords { screen, coords } => {
                let typed_coords = coords.as_ref().map(|c| self.check_view_coords(c));
                TypedStatement::new(
                    TypedStatementKind::WindowCoords {
                        screen: *screen,
                        coords: typed_coords,
                    },
                    stmt.span,
                )
            }

            StatementKind::DrawCmd { commands } => {
                let typed_commands = self.check_expr(commands);
                TypedStatement::new(
                    TypedStatementKind::DrawCmd {
                        commands: typed_commands,
                    },
                    stmt.span,
                )
            }

            // ==================== QB64 Graphics Extensions ====================
            StatementKind::FreeImage { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::FreeImage {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::PutImage {
                dest_coords,
                source,
                dest,
                source_coords,
            } => {
                let typed_dest_coords = dest_coords
                    .as_ref()
                    .map(|c| Box::new(self.check_view_coords(c)));
                let typed_source = source.as_ref().map(|e| self.check_expr(e));
                let typed_dest = dest.as_ref().map(|e| self.check_expr(e));
                let typed_source_coords = source_coords
                    .as_ref()
                    .map(|c| Box::new(self.check_view_coords(c)));
                TypedStatement::new(
                    TypedStatementKind::PutImage {
                        dest_coords: typed_dest_coords,
                        source: typed_source,
                        dest: typed_dest,
                        source_coords: typed_source_coords,
                    },
                    stmt.span,
                )
            }

            StatementKind::SourceImg { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::SourceImg {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::DestImg { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::DestImg {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::PrintStringStmt { x, y, text } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                let typed_text = self.check_expr(text);
                TypedStatement::new(
                    TypedStatementKind::PrintStringStmt {
                        x: typed_x,
                        y: typed_y,
                        text: typed_text,
                    },
                    stmt.span,
                )
            }

            StatementKind::AutoDisplay { enabled } => TypedStatement::new(
                TypedStatementKind::AutoDisplay { enabled: *enabled },
                stmt.span,
            ),

            // ==================== Audio Statements ====================
            StatementKind::Beep => TypedStatement::new(TypedStatementKind::Beep, stmt.span),

            StatementKind::SoundStmt {
                frequency,
                duration,
            } => {
                let typed_freq = self.check_expr(frequency);
                let typed_dur = self.check_expr(duration);
                TypedStatement::new(
                    TypedStatementKind::SoundStmt {
                        frequency: typed_freq,
                        duration: typed_dur,
                    },
                    stmt.span,
                )
            }

            StatementKind::PlayStmt { commands } => {
                let typed_commands = self.check_expr(commands);
                TypedStatement::new(
                    TypedStatementKind::PlayStmt {
                        commands: typed_commands,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndClose { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::SndClose {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndPlay { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::SndPlay {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndStop { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::SndStop {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndPause { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::SndPause {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndLoop { handle } => {
                let typed_handle = self.check_expr(handle);
                TypedStatement::new(
                    TypedStatementKind::SndLoop {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndVol { handle, volume } => {
                let typed_handle = self.check_expr(handle);
                let typed_volume = self.check_expr(volume);
                TypedStatement::new(
                    TypedStatementKind::SndVol {
                        handle: typed_handle,
                        volume: typed_volume,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndBal { handle, balance } => {
                let typed_handle = self.check_expr(handle);
                let typed_balance = self.check_expr(balance);
                TypedStatement::new(
                    TypedStatementKind::SndBal {
                        handle: typed_handle,
                        balance: typed_balance,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndRaw { left, right } => {
                let typed_left = self.check_expr(left);
                let typed_right = right.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::SndRaw {
                        left: typed_left,
                        right: typed_right,
                    },
                    stmt.span,
                )
            }

            // ==================== System Integration Statements ====================
            StatementKind::Kill { filename } => {
                let typed_filename = self.check_expr(filename);
                TypedStatement::new(
                    TypedStatementKind::Kill {
                        filename: typed_filename,
                    },
                    stmt.span,
                )
            }

            StatementKind::Rename { old_name, new_name } => {
                let typed_old = self.check_expr(old_name);
                let typed_new = self.check_expr(new_name);
                TypedStatement::new(
                    TypedStatementKind::Rename {
                        old_name: typed_old,
                        new_name: typed_new,
                    },
                    stmt.span,
                )
            }

            StatementKind::Mkdir { path } => {
                let typed_path = self.check_expr(path);
                TypedStatement::new(TypedStatementKind::Mkdir { path: typed_path }, stmt.span)
            }

            StatementKind::Rmdir { path } => {
                let typed_path = self.check_expr(path);
                TypedStatement::new(TypedStatementKind::Rmdir { path: typed_path }, stmt.span)
            }

            StatementKind::Chdir { path } => {
                let typed_path = self.check_expr(path);
                TypedStatement::new(TypedStatementKind::Chdir { path: typed_path }, stmt.span)
            }

            StatementKind::ShellCmd { command } => {
                let typed_command = command.as_ref().map(|c| self.check_expr(c));
                TypedStatement::new(
                    TypedStatementKind::ShellCmd {
                        command: typed_command,
                    },
                    stmt.span,
                )
            }

            StatementKind::ShellHide { command } => {
                let typed_command = self.check_expr(command);
                TypedStatement::new(
                    TypedStatementKind::ShellHide {
                        command: typed_command,
                    },
                    stmt.span,
                )
            }

            // ==================== Mouse Input Statements ====================
            StatementKind::MouseHide => {
                TypedStatement::new(TypedStatementKind::MouseHide, stmt.span)
            }

            StatementKind::MouseShow => {
                TypedStatement::new(TypedStatementKind::MouseShow, stmt.span)
            }

            StatementKind::MouseMoveStmt { x, y } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                TypedStatement::new(
                    TypedStatementKind::MouseMoveStmt {
                        x: typed_x,
                        y: typed_y,
                    },
                    stmt.span,
                )
            }

            // ==================== Clipboard Statement ====================
            StatementKind::ClipboardSet { text } => {
                let typed_text = self.check_expr(text);
                TypedStatement::new(
                    TypedStatementKind::ClipboardSet { text: typed_text },
                    stmt.span,
                )
            }
        }
    }

    /// Type checks ViewCoords for VIEW/WINDOW statements.
    fn check_view_coords(&mut self, coords: &ViewCoords) -> TypedViewCoords {
        TypedViewCoords {
            x1: self.check_expr(&coords.x1),
            y1: self.check_expr(&coords.y1),
            x2: self.check_expr(&coords.x2),
            y2: self.check_expr(&coords.y2),
        }
    }

    /// Type checks print items (shared between PRINT and PRINT #).
    fn check_print_items(&mut self, values: &[PrintItem]) -> Vec<TypedPrintItem> {
        values
            .iter()
            .map(|item| TypedPrintItem {
                expr: self.check_expr(&item.expr),
                separator: item.separator,
            })
            .collect()
    }

    /// Evaluates array dimensions that must be constant expressions.
    fn evaluate_array_dimension(
        &mut self,
        dim: &ArrayDimension,
        _span: crate::ast::Span,
    ) -> TypedArrayDimension {
        // Evaluate lower bound (if provided)
        let lower = if let Some(lower_expr) = &dim.lower {
            let typed_lower = self.check_expr(lower_expr);
            match self.try_evaluate_const_expr(&typed_lower) {
                Some(ConstValue::Integer(v)) => v,
                Some(ConstValue::Float(v)) => v as i64,
                _ => {
                    self.errors.push(SemanticError::NonConstantExpression {
                        span: lower_expr.span,
                    });
                    0
                }
            }
        } else {
            self.symbols.option_base()
        };

        // Evaluate upper bound (required)
        let typed_upper = self.check_expr(&dim.upper);
        let upper = match self.try_evaluate_const_expr(&typed_upper) {
            Some(ConstValue::Integer(v)) => v,
            Some(ConstValue::Float(v)) => v as i64,
            _ => {
                self.errors.push(SemanticError::NonConstantExpression {
                    span: dim.upper.span,
                });
                10
            }
        };

        TypedArrayDimension { lower, upper }
    }

    /// Evaluates array dimensions for REDIM where runtime expressions are allowed.
    /// Since we can't know the values at compile time, we use placeholder values.
    fn evaluate_array_dimension_runtime(&mut self, dim: &ArrayDimension) -> TypedArrayDimension {
        // For REDIM, we just need to type-check the expressions
        // The actual values will be computed at runtime
        if let Some(lower_expr) = &dim.lower {
            let typed_lower = self.check_expr(lower_expr);
            if !typed_lower.basic_type.is_numeric() {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: typed_lower.basic_type.to_string(),
                    span: typed_lower.span,
                });
            }
        }

        let typed_upper = self.check_expr(&dim.upper);
        if !typed_upper.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_upper.basic_type.to_string(),
                span: typed_upper.span,
            });
        }

        // Return placeholder values - actual bounds are runtime-computed
        TypedArrayDimension { lower: 0, upper: 0 }
    }
}
