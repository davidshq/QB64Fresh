//! Statement type checking dispatcher.
//!
//! This module contains the main `check_statement` method that dispatches
//! to appropriate handlers based on statement type. Simple pass-through
//! statements are handled directly here, while complex statements delegate
//! to specialized modules.

use crate::ast::{DataValue, Statement, StatementKind};
use crate::semantic::{
    error::SemanticError,
    symbols::{Symbol, SymbolKind, UserTypeDefinition, UserTypeMember},
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
        }
    }
}
