//! Statement type checking dispatcher.
//!
//! This module contains the main `check_statement` method that dispatches
//! to appropriate handlers based on statement type. Simple pass-through
//! statements are handled directly here, while complex statements delegate
//! to specialized modules.

use crate::ast::{ArrayDimension, DataValue, PrintItem, Statement, StatementKind};
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

            StatementKind::Swap { left, right } => {
                let typed_left = self.check_expr(left);
                let typed_right = self.check_expr(right);

                // Check that both expressions are lvalues (variables or array elements)
                // For now, we'll verify at codegen; semantic check just ensures type compatibility
                if typed_left.basic_type != typed_right.basic_type {
                    // Allow numeric type conversions but warn
                    if !typed_left
                        .basic_type
                        .is_convertible_to(&typed_right.basic_type)
                        && !typed_right
                            .basic_type
                            .is_convertible_to(&typed_left.basic_type)
                    {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: typed_left.basic_type.to_string(),
                            found: typed_right.basic_type.to_string(),
                            span: stmt.span,
                        });
                    }
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
                // Check that we're inside a matching loop
                // For now, we just pass through - full loop context checking would require
                // tracking the loop stack through semantic analysis
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

                // Look up variable type
                let var_type = self
                    .symbols
                    .lookup_symbol(variable)
                    .map(|s| s.basic_type.clone())
                    .unwrap_or_else(|| {
                        self.errors.push(SemanticError::UndefinedVariable {
                            name: variable.clone(),
                            span: stmt.span,
                        });
                        BasicType::Long
                    });

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
