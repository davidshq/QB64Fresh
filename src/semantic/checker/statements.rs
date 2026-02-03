//! Statement type checking dispatcher.
//!
//! This module contains the main `check_statement` method that dispatches
//! to appropriate handlers based on statement type. Simple pass-through
//! statements are handled directly here, while complex statements delegate
//! to specialized modules.

use crate::ast::{
    ArrayDimension, DataValue, ExternalDeclaration, PrintItem, Span, Statement, StatementKind,
    ViewCoords,
};
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

            StatementKind::FieldAssignment {
                name,
                fields,
                value,
            } => self.check_field_assignment(name, fields, value, stmt.span),

            StatementKind::ArrayFieldAssignment {
                name,
                indices,
                fields,
                value,
            } => self.check_array_field_assignment(name, indices, fields, value, stmt.span),

            StatementKind::MidAssignment {
                target,
                start,
                length,
                value,
            } => self.check_mid_assignment(target, start, length.as_ref(), value, stmt.span),

            StatementKind::AscAssignment {
                target,
                position,
                value,
            } => self.check_asc_assignment(target, position, value, stmt.span),

            StatementKind::Print { values, newline } => {
                self.check_print(values, *newline, stmt.span)
            }

            StatementKind::PrintUsing {
                format,
                values,
                newline,
            } => {
                let typed_format = self.check_expr(format);
                // Format string should be a string type (STRING or STRING * N)
                if !typed_format.basic_type.is_string() {
                    self.errors.push(SemanticError::type_mismatch(
                        "STRING",
                        format!("{:?}", typed_format.basic_type),
                        stmt.span,
                    ));
                }
                let typed_values: Vec<_> = values.iter().map(|v| self.check_expr(v)).collect();
                TypedStatement::new(
                    TypedStatementKind::PrintUsing {
                        format: typed_format,
                        values: typed_values,
                        newline: *newline,
                    },
                    stmt.span,
                )
            }

            StatementKind::Input {
                prompt,
                show_question_mark,
                same_line,
                targets,
            } => self.check_input(prompt, *show_question_mark, *same_line, targets, stmt.span),

            StatementKind::LineInput {
                suppress_newline: _,
                prompt,
                target,
            } => self.check_line_input(prompt, target, stmt.span),

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
            } => self.check_select_case(test_expr, cases, case_else, false, stmt.span),

            StatementKind::SelectEveryCase {
                test_expr,
                cases,
                case_else,
            } => self.check_select_case(test_expr, cases, case_else, true, stmt.span),

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

            StatementKind::End { exit_code } => {
                let typed_exit_code = exit_code.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::End {
                        exit_code: typed_exit_code,
                    },
                    stmt.span,
                )
            }

            StatementKind::Stop => TypedStatement::new(TypedStatementKind::Stop, stmt.span),

            StatementKind::System { exit_code } => {
                let typed_exit_code = exit_code.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::System {
                        exit_code: typed_exit_code,
                    },
                    stmt.span,
                )
            }

            StatementKind::Sleep { seconds } => {
                let typed_seconds = seconds.as_ref().map(|s| self.check_expr(s));
                TypedStatement::new(
                    TypedStatementKind::Sleep {
                        seconds: typed_seconds,
                    },
                    stmt.span,
                )
            }

            StatementKind::Wait {
                port,
                and_mask,
                xor_mask,
            } => {
                let typed_port = self.check_expr(port);
                let typed_and = self.check_expr(and_mask);
                let typed_xor = xor_mask.as_ref().map(|x| self.check_expr(x));
                TypedStatement::new(
                    TypedStatementKind::Wait {
                        port: typed_port,
                        and_mask: typed_and,
                        xor_mask: typed_xor,
                    },
                    stmt.span,
                )
            }

            StatementKind::Delay { seconds } => {
                let typed_seconds = self.check_expr(seconds);
                TypedStatement::new(
                    TypedStatementKind::Delay {
                        seconds: typed_seconds,
                    },
                    stmt.span,
                )
            }

            StatementKind::Limit { fps } => {
                let typed_fps = self.check_expr(fps);
                TypedStatement::new(TypedStatementKind::Limit { fps: typed_fps }, stmt.span)
            }

            StatementKind::Erase { arrays } => TypedStatement::new(
                TypedStatementKind::Erase {
                    arrays: arrays.clone(),
                },
                stmt.span,
            ),

            StatementKind::KeyClear => TypedStatement::new(TypedStatementKind::KeyClear, stmt.span),

            StatementKind::Call { name, args } => self.check_call(name, args, stmt.span),

            StatementKind::Dim { variables, shared } => {
                self.check_dim(variables, *shared, stmt.span)
            }

            StatementKind::Const { definitions } => self.check_const(definitions, stmt.span),

            StatementKind::DefType { type_kind, ranges } => {
                self.check_deftype(type_kind, ranges, stmt.span)
            }

            StatementKind::Define { type_spec, ranges } => {
                self.check_define(type_spec, ranges, stmt.span)
            }

            StatementKind::OptionBase { base } => self.check_option_base(*base, stmt.span),

            StatementKind::OptionExplicit => {
                self.symbols.set_explicit_mode(true);
                TypedStatement::new(TypedStatementKind::OptionExplicit, stmt.span)
            }

            StatementKind::OptionExplicitArray => {
                self.symbols.set_explicit_array_mode(true);
                TypedStatement::new(TypedStatementKind::OptionExplicitArray, stmt.span)
            }

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
                // Evaluate the condition at compile time and only include the selected branch.
                // This is true conditional compilation - excluded code is not type-checked
                // or included in the output.

                // Check the main $IF condition
                if self.evaluate_meta_condition(condition) {
                    // Main condition is true - include then_branch statements
                    let typed_stmts: Vec<TypedStatement> = then_branch
                        .iter()
                        .map(|s| self.check_statement(s))
                        .collect();
                    return TypedStatement::new(
                        TypedStatementKind::ConditionalBlockResolved {
                            original_condition: condition.clone(),
                            statements: typed_stmts,
                        },
                        stmt.span,
                    );
                }

                // Check $ELSEIF conditions
                for (elseif_cond, elseif_body) in elseif_branches {
                    if self.evaluate_meta_condition(elseif_cond) {
                        let typed_stmts: Vec<TypedStatement> = elseif_body
                            .iter()
                            .map(|s| self.check_statement(s))
                            .collect();
                        return TypedStatement::new(
                            TypedStatementKind::ConditionalBlockResolved {
                                original_condition: elseif_cond.clone(),
                                statements: typed_stmts,
                            },
                            stmt.span,
                        );
                    }
                }

                // No conditions matched - use $ELSE branch if present
                if let Some(else_body) = else_branch {
                    let typed_stmts: Vec<TypedStatement> =
                        else_body.iter().map(|s| self.check_statement(s)).collect();
                    return TypedStatement::new(
                        TypedStatementKind::ConditionalBlockResolved {
                            original_condition: "$ELSE".to_string(),
                            statements: typed_stmts,
                        },
                        stmt.span,
                    );
                }

                // No branch selected - emit empty block
                TypedStatement::new(
                    TypedStatementKind::ConditionalBlockResolved {
                        original_condition: condition.clone(),
                        statements: Vec::new(),
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

            StatementKind::MetaConsole { only } => {
                TypedStatement::new(TypedStatementKind::MetaConsole { only: *only }, stmt.span)
            }

            StatementKind::MetaScreenHide => {
                TypedStatement::new(TypedStatementKind::MetaScreenHide, stmt.span)
            }

            StatementKind::MetaScreenShow => {
                TypedStatement::new(TypedStatementKind::MetaScreenShow, stmt.span)
            }

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
                    // Bare _CONTINUE is valid inside any loop
                    ContinueType::Innermost => {
                        self.loop_context.for_depth > 0
                            || self.loop_context.while_depth > 0
                            || self.loop_context.do_depth > 0
                    }
                };

                if !valid {
                    let loop_name = match continue_type {
                        ContinueType::For => "FOR",
                        ContinueType::While => "WHILE",
                        ContinueType::Do => "DO",
                        ContinueType::Innermost => "any",
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

            StatementKind::TypeDefinition {
                name,
                members,
                custom_type,
            } => {
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
                    custom_type: *custom_type,
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
                        custom_type: *custom_type,
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

            StatementKind::Read { targets } => {
                use crate::ast::ReadTarget;
                use crate::semantic::typed_ir::TypedReadTarget;

                // Process each target (variable or array element)
                let typed_targets: Vec<TypedReadTarget> = targets
                    .iter()
                    .map(|target| match target {
                        ReadTarget::Variable(var_name) => {
                            let var_type =
                                if let Some(symbol) = self.symbols.lookup_symbol(var_name) {
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
                            TypedReadTarget::Variable {
                                name: var_name.clone(),
                                basic_type: var_type,
                            }
                        }
                        ReadTarget::ArrayElement { name, indices } => {
                            // Type check indices
                            let typed_indices: Vec<_> =
                                indices.iter().map(|e| self.check_expr(e)).collect();

                            // Look up array and get element type
                            let basic_type = if let Some(symbol) = self.symbols.lookup_symbol(name)
                            {
                                symbol.basic_type.clone()
                            } else {
                                // Array not declared - error
                                self.errors.push(SemanticError::UndefinedVariable {
                                    name: name.clone(),
                                    span: stmt.span,
                                });
                                BasicType::Single // Default on error
                            };

                            TypedReadTarget::ArrayElement {
                                name: name.clone(),
                                indices: typed_indices,
                                basic_type,
                            }
                        }
                        ReadTarget::ArrayFieldElement {
                            name,
                            indices,
                            field,
                        } => {
                            // Type check indices
                            let typed_indices: Vec<_> =
                                indices.iter().map(|e| self.check_expr(e)).collect();

                            // Look up array and get element type, then resolve field type
                            let basic_type = if let Some(symbol) = self.symbols.lookup_symbol(name)
                            {
                                // Get the element type of the array
                                let element_type = match &symbol.basic_type {
                                    BasicType::Array { element_type, .. } => {
                                        (**element_type).clone()
                                    }
                                    other => other.clone(),
                                };

                                // Resolve the field type from the UDT
                                let field_type = self.resolve_field_chain_type(
                                    &element_type,
                                    std::slice::from_ref(field),
                                );

                                if field_type == BasicType::Unknown {
                                    // Fall back to suffix-based inference if UDT resolution fails
                                    type_from_suffix(field)
                                        .unwrap_or_else(|| self.symbols.default_type_for(field))
                                } else {
                                    field_type
                                }
                            } else {
                                // Array not declared - error
                                self.errors.push(SemanticError::UndefinedVariable {
                                    name: name.clone(),
                                    span: stmt.span,
                                });
                                BasicType::Single // Default on error
                            };

                            TypedReadTarget::ArrayFieldElement {
                                name: name.clone(),
                                indices: typed_indices,
                                field: field.clone(),
                                basic_type,
                            }
                        }
                    })
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::Read {
                        targets: typed_targets,
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

            StatementKind::Randomize { seed } => {
                // Type check the seed expression if provided
                let typed_seed = seed.as_ref().map(|s| self.check_expr(s));

                // Seed should be a numeric type (but we'll allow any for flexibility)
                if let Some(ref typed) = typed_seed
                    && typed.basic_type.is_string()
                {
                    self.errors
                        .push(SemanticError::type_mismatch("numeric", "STRING", stmt.span));
                }

                TypedStatement::new(
                    TypedStatementKind::Randomize { seed: typed_seed },
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

                // Filename should be a string (STRING or STRING * N)
                if !typed_filename.basic_type.is_string() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "STRING".to_string(),
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

            StatementKind::OpenFileLegacy {
                mode_expr,
                file_num,
                filename,
                record_len,
            } => {
                let typed_mode = self.check_expr(mode_expr);
                let typed_file_num = self.check_expr(file_num);
                let typed_filename = self.check_expr(filename);
                let typed_record_len = record_len.as_ref().map(|e| self.check_expr(e));

                // Mode should be a string (STRING or STRING * N)
                if !typed_mode.basic_type.is_string() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "STRING".to_string(),
                        found: typed_mode.basic_type.to_string(),
                        span: typed_mode.span,
                    });
                }

                // Filename should be a string (STRING or STRING * N)
                if !typed_filename.basic_type.is_string() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "STRING".to_string(),
                        found: typed_filename.basic_type.to_string(),
                        span: typed_filename.span,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::OpenFileLegacy {
                        mode_expr: typed_mode,
                        file_num: typed_file_num,
                        filename: typed_filename,
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

            StatementKind::FileInput { file_num, targets } => {
                use crate::ast::InputTarget;
                use crate::semantic::typed_ir::TypedInputTarget;

                let typed_file_num = self.check_expr(file_num);

                // Type-check each input target
                let typed_targets: Vec<TypedInputTarget> = targets
                    .iter()
                    .map(|target| match target {
                        InputTarget::Variable(name) => {
                            let var_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                                symbol.basic_type.clone()
                            } else {
                                // Infer type from suffix or default
                                let inferred = type_from_suffix(name)
                                    .unwrap_or_else(|| self.symbols.default_type_for(name));
                                let symbol = Symbol {
                                    name: name.clone(),
                                    kind: SymbolKind::Variable,
                                    basic_type: inferred.clone(),
                                    span: stmt.span,
                                    is_mutable: true,
                                };
                                let _ = self.symbols.define_symbol(symbol);
                                inferred
                            };
                            TypedInputTarget::Variable {
                                name: name.clone(),
                                basic_type: var_type,
                            }
                        }
                        InputTarget::ArrayElement { name, indices } => {
                            let typed_indices: Vec<_> =
                                indices.iter().map(|i| self.check_expr(i)).collect();
                            let element_type = if let Some(symbol) =
                                self.symbols.lookup_symbol(name)
                            {
                                if let BasicType::Array { element_type, .. } = &symbol.basic_type {
                                    (**element_type).clone()
                                } else {
                                    symbol.basic_type.clone()
                                }
                            } else {
                                type_from_suffix(name)
                                    .unwrap_or_else(|| self.symbols.default_type_for(name))
                            };
                            TypedInputTarget::ArrayElement {
                                name: name.clone(),
                                indices: typed_indices,
                                element_type,
                            }
                        }
                        InputTarget::ArrayElementField {
                            name,
                            indices,
                            fields,
                        } => {
                            let typed_indices: Vec<_> =
                                indices.iter().map(|i| self.check_expr(i)).collect();
                            // For now, assume the field type based on suffix of field name
                            let field_type = fields
                                .last()
                                .and_then(|f| type_from_suffix(f))
                                .unwrap_or(BasicType::Single);
                            TypedInputTarget::ArrayElementField {
                                name: name.clone(),
                                indices: typed_indices,
                                fields: fields.clone(),
                                field_type,
                            }
                        }
                        InputTarget::Field { name, fields } => {
                            let field_type = fields
                                .last()
                                .and_then(|f| type_from_suffix(f))
                                .unwrap_or(BasicType::Single);
                            TypedInputTarget::Field {
                                name: name.clone(),
                                fields: fields.clone(),
                                field_type,
                            }
                        }
                    })
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::FileInput {
                        file_num: typed_file_num,
                        targets: typed_targets,
                    },
                    stmt.span,
                )
            }

            StatementKind::FileLineInput { file_num, target } => {
                use crate::ast::InputTarget;
                use crate::semantic::typed_ir::TypedInputTarget;

                let typed_file_num = self.check_expr(file_num);

                // LINE INPUT # always reads into a string
                let typed_target = match target {
                    InputTarget::Variable(name) => {
                        if self.symbols.lookup_symbol(name).is_none() {
                            let symbol = Symbol {
                                name: name.clone(),
                                kind: SymbolKind::Variable,
                                basic_type: BasicType::String,
                                span: stmt.span,
                                is_mutable: true,
                            };
                            let _ = self.symbols.define_symbol(symbol);
                        }
                        TypedInputTarget::Variable {
                            name: name.clone(),
                            basic_type: BasicType::String,
                        }
                    }
                    InputTarget::ArrayElement { name, indices } => {
                        let typed_indices: Vec<_> =
                            indices.iter().map(|i| self.check_expr(i)).collect();
                        TypedInputTarget::ArrayElement {
                            name: name.clone(),
                            indices: typed_indices,
                            element_type: BasicType::String,
                        }
                    }
                    InputTarget::ArrayElementField {
                        name,
                        indices,
                        fields,
                    } => {
                        let typed_indices: Vec<_> =
                            indices.iter().map(|i| self.check_expr(i)).collect();
                        TypedInputTarget::ArrayElementField {
                            name: name.clone(),
                            indices: typed_indices,
                            fields: fields.clone(),
                            field_type: BasicType::String,
                        }
                    }
                    InputTarget::Field { name, fields } => TypedInputTarget::Field {
                        name: name.clone(),
                        fields: fields.clone(),
                        field_type: BasicType::String,
                    },
                };

                TypedStatement::new(
                    TypedStatementKind::FileLineInput {
                        file_num: typed_file_num,
                        target: typed_target,
                    },
                    stmt.span,
                )
            }

            StatementKind::FileGet {
                file_num,
                position,
                target,
            } => {
                use crate::ast::InputTarget;
                use crate::semantic::typed_ir::TypedInputTarget;

                let typed_file_num = self.check_expr(file_num);
                let typed_position = position.as_ref().map(|e| self.check_expr(e));

                // Convert InputTarget to TypedInputTarget, inferring types
                // When a symbol is found via suffix fallback, use the symbol's declared name
                let typed_target = match target {
                    InputTarget::Variable(name) => {
                        let (resolved_name, var_type) =
                            if let Some(symbol) = self.symbols.lookup_symbol(name) {
                                (symbol.name.clone(), symbol.basic_type.clone())
                            } else {
                                let inferred = type_from_suffix(name)
                                    .unwrap_or_else(|| self.symbols.default_type_for(name));
                                let symbol = Symbol {
                                    name: name.clone(),
                                    kind: SymbolKind::Variable,
                                    basic_type: inferred.clone(),
                                    span: stmt.span,
                                    is_mutable: true,
                                };
                                let _ = self.symbols.define_symbol(symbol);
                                (name.clone(), inferred)
                            };
                        TypedInputTarget::Variable {
                            name: resolved_name,
                            basic_type: var_type,
                        }
                    }
                    InputTarget::ArrayElement { name, indices } => {
                        let typed_indices: Vec<_> =
                            indices.iter().map(|i| self.check_expr(i)).collect();
                        let (resolved_name, element_type) =
                            if let Some(symbol) = self.symbols.lookup_array(name) {
                                (symbol.name.clone(), symbol.basic_type.clone())
                            } else {
                                let element_type = type_from_suffix(name)
                                    .unwrap_or_else(|| self.symbols.default_type_for(name));
                                (name.clone(), element_type)
                            };
                        TypedInputTarget::ArrayElement {
                            name: resolved_name,
                            indices: typed_indices,
                            element_type,
                        }
                    }
                    InputTarget::ArrayElementField {
                        name,
                        indices,
                        fields,
                    } => {
                        let typed_indices: Vec<_> =
                            indices.iter().map(|i| self.check_expr(i)).collect();
                        let resolved_name = if let Some(symbol) = self.symbols.lookup_array(name) {
                            symbol.name.clone()
                        } else {
                            name.clone()
                        };
                        // Field type would need UDT lookup; use SINGLE as placeholder
                        TypedInputTarget::ArrayElementField {
                            name: resolved_name,
                            indices: typed_indices,
                            fields: fields.clone(),
                            field_type: BasicType::Single,
                        }
                    }
                    InputTarget::Field { name, fields } => {
                        let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                            symbol.name.clone()
                        } else {
                            name.clone()
                        };
                        // Field type would need UDT lookup; use SINGLE as placeholder
                        TypedInputTarget::Field {
                            name: resolved_name,
                            fields: fields.clone(),
                            field_type: BasicType::Single,
                        }
                    }
                };

                TypedStatement::new(
                    TypedStatementKind::FileGet {
                        file_num: typed_file_num,
                        position: typed_position,
                        target: typed_target,
                    },
                    stmt.span,
                )
            }

            StatementKind::FilePut {
                file_num,
                position,
                target,
            } => {
                use crate::ast::InputTarget;
                use crate::semantic::typed_ir::TypedInputTarget;

                let typed_file_num = self.check_expr(file_num);
                let typed_position = position.as_ref().map(|e| self.check_expr(e));

                // Convert InputTarget to TypedInputTarget, inferring types
                // When a symbol is found via suffix fallback, use the symbol's declared name
                let typed_target = match target {
                    InputTarget::Variable(name) => {
                        let (resolved_name, var_type) =
                            if let Some(symbol) = self.symbols.lookup_symbol(name) {
                                (symbol.name.clone(), symbol.basic_type.clone())
                            } else {
                                let inferred = type_from_suffix(name)
                                    .unwrap_or_else(|| self.symbols.default_type_for(name));
                                let symbol = Symbol {
                                    name: name.clone(),
                                    kind: SymbolKind::Variable,
                                    basic_type: inferred.clone(),
                                    span: stmt.span,
                                    is_mutable: true,
                                };
                                let _ = self.symbols.define_symbol(symbol);
                                (name.clone(), inferred)
                            };
                        TypedInputTarget::Variable {
                            name: resolved_name,
                            basic_type: var_type,
                        }
                    }
                    InputTarget::ArrayElement { name, indices } => {
                        let typed_indices: Vec<_> =
                            indices.iter().map(|i| self.check_expr(i)).collect();
                        let (resolved_name, element_type) =
                            if let Some(symbol) = self.symbols.lookup_array(name) {
                                (symbol.name.clone(), symbol.basic_type.clone())
                            } else {
                                let element_type = type_from_suffix(name)
                                    .unwrap_or_else(|| self.symbols.default_type_for(name));
                                (name.clone(), element_type)
                            };
                        TypedInputTarget::ArrayElement {
                            name: resolved_name,
                            indices: typed_indices,
                            element_type,
                        }
                    }
                    InputTarget::ArrayElementField {
                        name,
                        indices,
                        fields,
                    } => {
                        let typed_indices: Vec<_> =
                            indices.iter().map(|i| self.check_expr(i)).collect();
                        let resolved_name = if let Some(symbol) = self.symbols.lookup_array(name) {
                            symbol.name.clone()
                        } else {
                            name.clone()
                        };
                        // Field type would need UDT lookup; use SINGLE as placeholder
                        TypedInputTarget::ArrayElementField {
                            name: resolved_name,
                            indices: typed_indices,
                            fields: fields.clone(),
                            field_type: BasicType::Single,
                        }
                    }
                    InputTarget::Field { name, fields } => {
                        let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                            symbol.name.clone()
                        } else {
                            name.clone()
                        };
                        // Field type would need UDT lookup; use SINGLE as placeholder
                        TypedInputTarget::Field {
                            name: resolved_name,
                            fields: fields.clone(),
                            field_type: BasicType::Single,
                        }
                    }
                };

                TypedStatement::new(
                    TypedStatementKind::FilePut {
                        file_num: typed_file_num,
                        position: typed_position,
                        target: typed_target,
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
                            is_array: false, // DEF FN doesn't support array params
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

            StatementKind::DefFnMultiLine { name, params, body } => {
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
                            .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                        // Define parameter as a local variable
                        let symbol = Symbol {
                            name: p.name.clone(),
                            kind: SymbolKind::Parameter { by_val: p.by_val },
                            basic_type: param_type.clone(),
                            span: stmt.span,
                            is_mutable: true,
                        };
                        let _ = self.symbols.define_symbol(symbol);

                        TypedParameter {
                            name: p.name.clone(),
                            basic_type: param_type,
                            by_val: p.by_val,
                            is_array: p.is_array,
                        }
                    })
                    .collect();

                // Check the body statements
                let typed_body: Vec<TypedStatement> =
                    body.iter().map(|s| self.check_statement(s)).collect();

                // Return type is inferred from the function name suffix or defaults to SINGLE
                let return_type = type_from_suffix(name).unwrap_or(BasicType::Single);

                self.symbols.exit_scope();

                TypedStatement::new(
                    TypedStatementKind::DefFnMultiLine {
                        name: name.clone(),
                        params: typed_params,
                        return_type,
                        body: typed_body,
                    },
                    stmt.span,
                )
            }

            StatementKind::DefSeg { segment } => {
                let typed_segment = segment.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::DefSeg {
                        segment: typed_segment,
                    },
                    stmt.span,
                )
            }

            StatementKind::Poke { address, value } => {
                let typed_address = self.check_expr(address);
                let typed_value = self.check_expr(value);
                TypedStatement::new(
                    TypedStatementKind::Poke {
                        address: typed_address,
                        value: typed_value,
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

            StatementKind::SharedStmt { variables } => {
                // SHARED statement inside SUB/FUNCTION declares access to module-level variables.
                // In classic BASIC, SHARED can also implicitly create variables at module level
                // if they don't already exist.

                // Must be inside a procedure
                if !self.symbols.in_procedure() {
                    self.errors
                        .push(SemanticError::SharedOutsideProcedure { span: stmt.span });
                } else {
                    // For each variable, check if it exists at module level
                    // If not, implicitly declare it (classic BASIC behavior)
                    for var_name in variables {
                        if self.symbols.lookup_global_symbol(var_name).is_some() {
                            // Variable exists at module level - register as shared
                            self.symbols.add_shared_var(var_name.clone());
                        } else {
                            // Classic BASIC: implicitly declare at module level
                            // Determine type from name suffix or default
                            let basic_type = type_from_suffix(var_name)
                                .unwrap_or_else(|| self.symbols.default_type_for(var_name));

                            let symbol = Symbol {
                                name: var_name.clone(),
                                kind: SymbolKind::Variable,
                                basic_type,
                                span: stmt.span,
                                is_mutable: true,
                            };

                            // Define at global scope and mark as module-shared
                            self.symbols.define_shared_symbol(symbol);
                            // Also register for this procedure's SHARED access
                            self.symbols.add_shared_var(var_name.clone());
                        }
                    }
                }

                TypedStatement::new(
                    TypedStatementKind::SharedStmt {
                        variables: variables.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::StaticStmt { variables } => {
                // STATIC statement inside SUB/FUNCTION declares static local variables.
                // These persist between calls (in C, they become `static` locals).
                let mut typed_vars = Vec::new();

                for var in variables {
                    // Determine type
                    let basic_type = var
                        .type_spec
                        .as_ref()
                        .map(from_type_spec)
                        .or_else(|| type_from_suffix(&var.name))
                        .unwrap_or_else(|| self.symbols.default_type_for(&var.name));

                    // Evaluate array dimensions
                    let typed_dims: Vec<TypedArrayDimension> = var
                        .dimensions
                        .iter()
                        .map(|d| {
                            let lower = if let Some(lower_expr) = &d.lower {
                                let typed_lower = self.check_expr(lower_expr);
                                match self.try_evaluate_const_expr(&typed_lower) {
                                    Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                                    Some(crate::semantic::symbols::ConstValue::Float(v)) => {
                                        v as i64
                                    }
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

                            let typed_upper = self.check_expr(&d.upper);
                            let upper = match self.try_evaluate_const_expr(&typed_upper) {
                                Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                                Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                                _ => {
                                    self.errors.push(SemanticError::NonConstantExpression {
                                        span: d.upper.span,
                                    });
                                    10
                                }
                            };

                            TypedArrayDimension { lower, upper }
                        })
                        .collect();

                    // Define symbol in current scope (it's a local, but static)
                    let symbol_kind = if var.dimensions.is_empty() {
                        SymbolKind::Variable
                    } else {
                        SymbolKind::ArrayVariable {
                            dimensions: typed_dims
                                .iter()
                                .map(|d| crate::semantic::symbols::ArrayDimInfo {
                                    lower_bound: d.lower,
                                    upper_bound: d.upper,
                                })
                                .collect(),
                        }
                    };

                    let symbol = Symbol {
                        name: var.name.clone(),
                        kind: symbol_kind,
                        basic_type: if typed_dims.is_empty() {
                            basic_type.clone()
                        } else {
                            BasicType::Array {
                                element_type: Box::new(basic_type.clone()),
                                dimensions: typed_dims.len(),
                            }
                        },
                        span: stmt.span,
                        is_mutable: true,
                    };

                    if let Err(existing) = self.symbols.define_symbol(symbol) {
                        self.errors.push(SemanticError::DuplicateVariable {
                            name: var.name.clone(),
                            original_span: existing.0.span,
                            duplicate_span: stmt.span,
                        });
                    }

                    typed_vars.push(TypedDimVariable {
                        name: var.name.clone(),
                        basic_type,
                        dimensions: typed_dims,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::StaticStmt {
                        variables: typed_vars,
                    },
                    stmt.span,
                )
            }

            StatementKind::Redim {
                preserve,
                shared,
                variables,
            } => {
                use crate::semantic::typed_ir::TypedRedimVariable;

                let mut typed_vars = Vec::new();

                for var in variables {
                    // For REDIM _PRESERVE, the array must already exist.
                    // Look up the existing array first (including SHARED arrays).
                    let existing_array = self.symbols.lookup_array(&var.name);

                    // Determine element type:
                    // 1. If explicit type spec provided, use it
                    // 2. If _PRESERVE and array exists, use existing type
                    // 3. If type suffix on name, use that
                    // 4. Fall back to default type
                    let element_type = var
                        .type_spec
                        .as_ref()
                        .map(from_type_spec)
                        .or_else(|| {
                            // For _PRESERVE, inherit type from existing array
                            if *preserve {
                                existing_array.map(|sym| sym.basic_type.clone())
                            } else {
                                None
                            }
                        })
                        .or_else(|| type_from_suffix(&var.name))
                        .unwrap_or_else(|| self.symbols.default_type_for(&var.name));

                    // Evaluate dimensions - REDIM allows runtime expressions
                    let typed_dims: Vec<TypedArrayDimension> = var
                        .dimensions
                        .iter()
                        .map(|d| self.evaluate_array_dimension_runtime(d))
                        .collect();

                    // Update symbol table (or define if not exists)
                    // Use ArrayVariable kind so array passing works correctly.
                    // REDIM can resize existing arrays (including array parameters),
                    // so we use update_or_define to replace any existing symbol.
                    // For SHARED arrays, define at module scope.
                    //
                    // IMPORTANT: If this is REDIM _PRESERVE on a module-level SHARED array,
                    // we should update the GLOBAL scope entry, not create a local copy.
                    let symbol = Symbol {
                        name: var.name.clone(),
                        kind: SymbolKind::ArrayVariable {
                            dimensions: typed_dims
                                .iter()
                                .map(|d| crate::semantic::symbols::ArrayDimInfo {
                                    lower_bound: d.lower,
                                    upper_bound: d.upper,
                                })
                                .collect(),
                        },
                        basic_type: element_type.clone(),
                        span: stmt.span,
                        is_mutable: true,
                    };
                    if *shared {
                        self.symbols.define_shared_symbol(symbol);
                    } else if *preserve && self.symbols.is_module_shared(&var.name) {
                        // REDIM _PRESERVE on a module-level SHARED array:
                        // Update the global scope entry, not create a local copy
                        self.symbols.update_shared_symbol(symbol);
                    } else {
                        self.symbols.update_or_define_symbol(symbol);
                    }

                    typed_vars.push(TypedRedimVariable {
                        name: var.name.clone(),
                        element_type,
                        dimensions: typed_dims,
                    });
                }

                TypedStatement::new(
                    TypedStatementKind::Redim {
                        preserve: *preserve,
                        shared: *shared,
                        variables: typed_vars,
                    },
                    stmt.span,
                )
            }

            // ==================== Graphics Statements ====================
            StatementKind::Screen {
                mode,
                color_switch,
                active_page,
                visual_page,
            } => {
                let typed_mode = mode.as_ref().map(|e| self.check_expr(e));
                let typed_color_switch = color_switch.as_ref().map(|e| self.check_expr(e));
                let typed_active_page = active_page.as_ref().map(|e| self.check_expr(e));
                let typed_visual_page = visual_page.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Screen {
                        mode: typed_mode,
                        color_switch: typed_color_switch,
                        active_page: typed_active_page,
                        visual_page: typed_visual_page,
                    },
                    stmt.span,
                )
            }

            StatementKind::Cls { mode } => {
                let typed_mode = mode.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(TypedStatementKind::Cls { mode: typed_mode }, stmt.span)
            }

            StatementKind::Color {
                foreground,
                background,
                border,
            } => {
                let typed_fg = foreground.as_ref().map(|e| self.check_expr(e));
                let typed_bg = background.as_ref().map(|e| self.check_expr(e));
                let typed_border = border.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Color {
                        foreground: typed_fg,
                        background: typed_bg,
                        border: typed_border,
                    },
                    stmt.span,
                )
            }

            StatementKind::Locate { row, col } => {
                let typed_row = row.as_ref().map(|e| self.check_expr(e));
                let typed_col = col.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Locate {
                        row: typed_row,
                        col: typed_col,
                    },
                    stmt.span,
                )
            }

            StatementKind::Pset { step, x, y, color } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Pset {
                        step: *step,
                        x: typed_x,
                        y: typed_y,
                        color: typed_color,
                    },
                    stmt.span,
                )
            }

            StatementKind::Preset { step, x, y } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                TypedStatement::new(
                    TypedStatementKind::Preset {
                        step: *step,
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
                step2,
                color,
                box_style,
                style,
            } => {
                let typed_x1 = x1.as_ref().map(|e| self.check_expr(e));
                let typed_y1 = y1.as_ref().map(|e| self.check_expr(e));
                let typed_x2 = self.check_expr(x2);
                let typed_y2 = self.check_expr(y2);
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                let typed_style = style.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Line {
                        x1: typed_x1,
                        y1: typed_y1,
                        x2: typed_x2,
                        y2: typed_y2,
                        step2: *step2,
                        color: typed_color,
                        box_style: *box_style,
                        style: typed_style,
                    },
                    stmt.span,
                )
            }

            StatementKind::Circle {
                step,
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
                        step: *step,
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
                step,
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
                        step: *step,
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

            StatementKind::ControlChr { enabled } => TypedStatement::new(
                TypedStatementKind::ControlChr { enabled: *enabled },
                stmt.span,
            ),

            StatementKind::MapUnicode {
                unicode_value,
                char_position,
            } => {
                let typed_unicode = self.check_expr(unicode_value);
                let typed_char = self.check_expr(char_position);
                TypedStatement::new(
                    TypedStatementKind::MapUnicode {
                        unicode_value: typed_unicode,
                        char_position: typed_char,
                    },
                    stmt.span,
                )
            }

            StatementKind::Palette { attribute, color } => {
                let typed_attr = attribute.as_ref().map(|e| self.check_expr(e));
                let typed_color = color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::Palette {
                        attribute: typed_attr,
                        color: typed_color,
                    },
                    stmt.span,
                )
            }

            StatementKind::GfxResize { enabled } => TypedStatement::new(
                TypedStatementKind::GfxResize { enabled: *enabled },
                stmt.span,
            ),

            StatementKind::Pcopy { source, dest } => {
                let typed_source = self.check_expr(source);
                let typed_dest = self.check_expr(dest);
                TypedStatement::new(
                    TypedStatementKind::Pcopy {
                        source: typed_source,
                        dest: typed_dest,
                    },
                    stmt.span,
                )
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

            StatementKind::ViewPrint { top, bottom } => {
                let typed_top = top.as_ref().map(|e| self.check_expr(e));
                let typed_bottom = bottom.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::ViewPrint {
                        top: typed_top,
                        bottom: typed_bottom,
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

            StatementKind::GraphicsGet {
                step1,
                x1,
                y1,
                step2,
                x2,
                y2,
                array_name,
                array_indices,
            } => {
                let typed_x1 = self.check_expr(x1);
                let typed_y1 = self.check_expr(y1);
                let typed_x2 = self.check_expr(x2);
                let typed_y2 = self.check_expr(y2);
                let typed_indices: Vec<_> =
                    array_indices.iter().map(|e| self.check_expr(e)).collect();
                TypedStatement::new(
                    TypedStatementKind::GraphicsGet {
                        step1: *step1,
                        x1: typed_x1,
                        y1: typed_y1,
                        step2: *step2,
                        x2: typed_x2,
                        y2: typed_y2,
                        array_name: array_name.clone(),
                        array_indices: typed_indices,
                    },
                    stmt.span,
                )
            }

            StatementKind::GraphicsPut {
                x,
                y,
                step,
                array_name,
                array_indices,
                clip,
                action,
                transparent_color,
            } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                let typed_indices: Vec<_> =
                    array_indices.iter().map(|e| self.check_expr(e)).collect();
                let typed_transparent = transparent_color.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::GraphicsPut {
                        x: typed_x,
                        y: typed_y,
                        step: *step,
                        array_name: array_name.clone(),
                        array_indices: typed_indices,
                        clip: *clip,
                        action: *action,
                        transparent_color: typed_transparent,
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
                scale_mode,
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
                        scale_mode: *scale_mode,
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

            StatementKind::SndBal {
                handle,
                x,
                y,
                z,
                channel,
            } => {
                let typed_handle = self.check_expr(handle);
                let typed_x = x.as_ref().map(|e| self.check_expr(e));
                let typed_y = y.as_ref().map(|e| self.check_expr(e));
                let typed_z = z.as_ref().map(|e| self.check_expr(e));
                let typed_channel = channel.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::SndBal {
                        handle: typed_handle,
                        x: typed_x,
                        y: typed_y,
                        z: typed_z,
                        channel: typed_channel,
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

            StatementKind::SndPlayFile {
                filename,
                volume,
                x,
                y,
                z,
            } => {
                let typed_filename = self.check_expr(filename);
                let typed_volume = volume.as_ref().map(|e| self.check_expr(e));
                let typed_x = x.as_ref().map(|e| self.check_expr(e));
                let typed_y = y.as_ref().map(|e| self.check_expr(e));
                let typed_z = z.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::SndPlayFile {
                        filename: typed_filename,
                        volume: typed_volume,
                        x: typed_x,
                        y: typed_y,
                        z: typed_z,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndPlayCopy { handle, volume } => {
                let typed_handle = self.check_expr(handle);
                let typed_volume = volume.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::SndPlayCopy {
                        handle: typed_handle,
                        volume: typed_volume,
                    },
                    stmt.span,
                )
            }

            StatementKind::SndSetPos { handle, position } => {
                let typed_handle = self.check_expr(handle);
                let typed_position = self.check_expr(position);
                TypedStatement::new(
                    TypedStatementKind::SndSetPos {
                        handle: typed_handle,
                        position: typed_position,
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

            StatementKind::Bload { filename, address } => {
                let typed_filename = self.check_expr(filename);
                let typed_address = address.as_ref().map(|a| self.check_expr(a));
                TypedStatement::new(
                    TypedStatementKind::Bload {
                        filename: typed_filename,
                        address: typed_address,
                    },
                    stmt.span,
                )
            }

            StatementKind::Bsave {
                filename,
                address,
                length,
            } => {
                let typed_filename = self.check_expr(filename);
                let typed_address = self.check_expr(address);
                let typed_length = self.check_expr(length);
                TypedStatement::new(
                    TypedStatementKind::Bsave {
                        filename: typed_filename,
                        address: typed_address,
                        length: typed_length,
                    },
                    stmt.span,
                )
            }

            StatementKind::Setmem { bytes } => {
                // SETMEM is obsolete - throw compile error matching QB64pe behavior
                self.errors.push(SemanticError::CommandNotImplemented {
                    name: "SETMEM".to_string(),
                    span: stmt.span,
                });
                let typed_bytes = self.check_expr(bytes);
                TypedStatement::new(TypedStatementKind::Setmem { bytes: typed_bytes }, stmt.span)
            }

            StatementKind::CallAbsolute { args, address } => {
                let typed_args: Vec<_> = args.iter().map(|e| self.check_expr(e)).collect();
                let typed_address = self.check_expr(address);
                TypedStatement::new(
                    TypedStatementKind::CallAbsolute {
                        args: typed_args,
                        address: typed_address,
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

            // ==================== C Library Integration ====================
            StatementKind::DeclareLibrary {
                library_name,
                is_dynamic,
                declarations,
            } => {
                // Register external functions in the symbol table
                let typed_declarations = declarations
                    .iter()
                    .map(|decl| self.register_external_function(decl))
                    .collect();

                TypedStatement::new(
                    TypedStatementKind::DeclareLibrary {
                        library_name: library_name.clone(),
                        is_dynamic: *is_dynamic,
                        declarations: typed_declarations,
                    },
                    stmt.span,
                )
            }

            // Forward declarations - parsed for compatibility but don't generate code
            StatementKind::DeclareSub { name, params: _ } => TypedStatement::new(
                TypedStatementKind::DeclareSub { name: name.clone() },
                stmt.span,
            ),

            StatementKind::DeclareFunction {
                name,
                params: _,
                return_type: _,
            } => TypedStatement::new(
                TypedStatementKind::DeclareFunction { name: name.clone() },
                stmt.span,
            ),

            // ==================== Phase 7: Additional Statements ====================
            StatementKind::Run { target } => {
                let typed_target = target.as_ref().map(|t| self.check_expr(t));
                TypedStatement::new(
                    TypedStatementKind::Run {
                        target: typed_target,
                    },
                    stmt.span,
                )
            }

            StatementKind::Chain { filename } => {
                let typed_filename = self.check_expr(filename);
                // Filename should be a string (STRING or STRING * N)
                if !typed_filename.basic_type.is_string() {
                    self.errors.push(SemanticError::type_mismatch(
                        "STRING",
                        format!("{:?}", typed_filename.basic_type),
                        stmt.span,
                    ));
                }
                TypedStatement::new(
                    TypedStatementKind::Chain {
                        filename: typed_filename,
                    },
                    stmt.span,
                )
            }

            StatementKind::Tron => TypedStatement::new(TypedStatementKind::Tron, stmt.span),

            StatementKind::Troff => TypedStatement::new(TypedStatementKind::Troff, stmt.span),

            StatementKind::Lprint { values, newline } => {
                let typed_values = self.check_print_items(values);
                TypedStatement::new(
                    TypedStatementKind::Lprint {
                        values: typed_values,
                        newline: *newline,
                    },
                    stmt.span,
                )
            }

            StatementKind::FilesStmt { filespec } => {
                let typed_filespec = filespec.as_ref().map(|f| self.check_expr(f));
                TypedStatement::new(
                    TypedStatementKind::FilesStmt {
                        filespec: typed_filespec,
                    },
                    stmt.span,
                )
            }

            StatementKind::FieldStmt { file_num, fields } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_fields: Vec<_> = fields
                    .iter()
                    .map(|f| TypedFieldSpec {
                        width: self.check_expr(&f.width),
                        variable: f.variable.clone(),
                    })
                    .collect();
                TypedStatement::new(
                    TypedStatementKind::FieldStmt {
                        file_num: typed_file_num,
                        fields: typed_fields,
                    },
                    stmt.span,
                )
            }

            StatementKind::Lset { variable, value } => {
                let typed_value = self.check_expr(value);
                // Resolve variable name through symbol lookup (handles suffix mismatch)
                let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(variable) {
                    symbol.name.clone()
                } else {
                    variable.clone()
                };
                TypedStatement::new(
                    TypedStatementKind::Lset {
                        variable: resolved_name,
                        value: typed_value,
                    },
                    stmt.span,
                )
            }

            StatementKind::Rset { variable, value } => {
                let typed_value = self.check_expr(value);
                // Resolve variable name through symbol lookup (handles suffix mismatch)
                let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(variable) {
                    symbol.name.clone()
                } else {
                    variable.clone()
                };
                TypedStatement::new(
                    TypedStatementKind::Rset {
                        variable: resolved_name,
                        value: typed_value,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnKey { key_num, target } => {
                let typed_key_num = self.check_expr(key_num);
                TypedStatement::new(
                    TypedStatementKind::OnKey {
                        key_num: typed_key_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::KeyControl { key_num, mode } => {
                let typed_key_num = self.check_expr(key_num);
                TypedStatement::new(
                    TypedStatementKind::KeyControl {
                        key_num: typed_key_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnTimer { interval, target } => {
                let typed_interval = self.check_expr(interval);
                TypedStatement::new(
                    TypedStatementKind::OnTimer {
                        interval: typed_interval,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::TimerControl { mode } => {
                TypedStatement::new(TypedStatementKind::TimerControl { mode: *mode }, stmt.span)
            }

            StatementKind::StrigControl { button_num, mode } => {
                let typed_button_num = self.check_expr(button_num);
                TypedStatement::new(
                    TypedStatementKind::StrigControl {
                        button_num: typed_button_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnStrig { button_num, target } => {
                let typed_button_num = self.check_expr(button_num);
                TypedStatement::new(
                    TypedStatementKind::OnStrig {
                        button_num: typed_button_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::OnCom { port_num, target } => {
                let typed_port_num = self.check_expr(port_num);
                TypedStatement::new(
                    TypedStatementKind::OnCom {
                        port_num: typed_port_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::ComControl { port_num, mode } => {
                let typed_port_num = self.check_expr(port_num);
                TypedStatement::new(
                    TypedStatementKind::ComControl {
                        port_num: typed_port_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnPen { target } => TypedStatement::new(
                TypedStatementKind::OnPen {
                    target: target.clone(),
                },
                stmt.span,
            ),

            StatementKind::PenControl { mode } => {
                TypedStatement::new(TypedStatementKind::PenControl { mode: *mode }, stmt.span)
            }

            StatementKind::OnUevent { target } => TypedStatement::new(
                TypedStatementKind::OnUevent {
                    target: target.clone(),
                },
                stmt.span,
            ),

            StatementKind::UeventControl { mode } => {
                TypedStatement::new(TypedStatementKind::UeventControl { mode: *mode }, stmt.span)
            }

            StatementKind::UeventTrigger => {
                TypedStatement::new(TypedStatementKind::UeventTrigger, stmt.span)
            }

            StatementKind::OnSignal { signal_num, target } => {
                let typed_signal_num = self.check_expr(signal_num);
                TypedStatement::new(
                    TypedStatementKind::OnSignal {
                        signal_num: typed_signal_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::SignalControl { signal_num, mode } => {
                let typed_signal_num = self.check_expr(signal_num);
                TypedStatement::new(
                    TypedStatementKind::SignalControl {
                        signal_num: typed_signal_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OutPort { port, value } => {
                let typed_port = self.check_expr(port);
                let typed_value = self.check_expr(value);
                TypedStatement::new(
                    TypedStatementKind::OutPort {
                        port: typed_port,
                        value: typed_value,
                    },
                    stmt.span,
                )
            }

            StatementKind::InterruptStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let typed_int_num = self.check_expr(int_num);
                TypedStatement::new(
                    TypedStatementKind::InterruptStmt {
                        int_num: typed_int_num,
                        in_regs: in_regs.clone(),
                        out_regs: out_regs.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::InterruptXStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let typed_int_num = self.check_expr(int_num);
                TypedStatement::new(
                    TypedStatementKind::InterruptXStmt {
                        int_num: typed_int_num,
                        in_regs: in_regs.clone(),
                        out_regs: out_regs.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::IoctlStmt {
                file_num,
                control_string,
            } => {
                // IOCTL is a stub (legacy DOS device control)
                // QB64pe also stubs this, so we just compile it to a no-op
                let typed_file_num = self.check_expr(file_num);
                let typed_control_string = self.check_expr(control_string);
                TypedStatement::new(
                    TypedStatementKind::IoctlStmt {
                        file_num: typed_file_num,
                        control_string: typed_control_string,
                    },
                    stmt.span,
                )
            }

            StatementKind::FreeStmt => TypedStatement::new(TypedStatementKind::FreeStmt, stmt.span),

            StatementKind::ClearStmt { stack_size } => {
                let typed_stack_size = stack_size.as_ref().map(|s| self.check_expr(s));
                TypedStatement::new(
                    TypedStatementKind::ClearStmt {
                        stack_size: typed_stack_size,
                    },
                    stmt.span,
                )
            }

            StatementKind::ResetStmt => {
                TypedStatement::new(TypedStatementKind::ResetStmt, stmt.span)
            }

            // Window/Desktop statements (QB64)
            StatementKind::TitleStmt { title } => {
                let typed_title = self.check_expr(title);
                TypedStatement::new(
                    TypedStatementKind::TitleStmt { title: typed_title },
                    stmt.span,
                )
            }

            StatementKind::ScreenMoveStmt { x, y, center } => {
                let typed_x = x.as_ref().map(|e| self.check_expr(e));
                let typed_y = y.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::ScreenMoveStmt {
                        x: typed_x,
                        y: typed_y,
                        center: *center,
                    },
                    stmt.span,
                )
            }

            StatementKind::FullScreenStmt { mode } => TypedStatement::new(
                TypedStatementKind::FullScreenStmt { mode: *mode },
                stmt.span,
            ),

            StatementKind::AllowFullScreenStmt { mode } => TypedStatement::new(
                TypedStatementKind::AllowFullScreenStmt { mode: *mode },
                stmt.span,
            ),

            StatementKind::ScreenIconStmt => {
                TypedStatement::new(TypedStatementKind::ScreenIconStmt, stmt.span)
            }

            StatementKind::IconStmt { handle } => {
                let typed_handle = handle.as_ref().map(|h| self.check_expr(h));
                TypedStatement::new(
                    TypedStatementKind::IconStmt {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::ScreenHideStmt => {
                TypedStatement::new(TypedStatementKind::ScreenHideStmt, stmt.span)
            }

            StatementKind::ScreenShowStmt => {
                TypedStatement::new(TypedStatementKind::ScreenShowStmt, stmt.span)
            }

            StatementKind::ConsoleTitleStmt { title } => {
                let typed_title = self.check_expr(title);
                TypedStatement::new(
                    TypedStatementKind::ConsoleTitleStmt { title: typed_title },
                    stmt.span,
                )
            }

            StatementKind::ConsoleStmt { visible } => TypedStatement::new(
                TypedStatementKind::ConsoleStmt { visible: *visible },
                stmt.span,
            ),

            StatementKind::AssertStmt { condition, message } => {
                let typed_condition = self.check_expr(condition);
                let typed_message = message.as_ref().map(|m| self.check_expr(m));
                TypedStatement::new(
                    TypedStatementKind::AssertStmt {
                        condition: typed_condition,
                        message: typed_message,
                    },
                    stmt.span,
                )
            }

            StatementKind::MetaAsserts => {
                TypedStatement::new(TypedStatementKind::MetaAsserts, stmt.span)
            }

            StatementKind::MetaNoPrefix => {
                TypedStatement::new(TypedStatementKind::MetaNoPrefix, stmt.span)
            }

            StatementKind::MetaColor { depth } => {
                TypedStatement::new(TypedStatementKind::MetaColor { depth: *depth }, stmt.span)
            }

            StatementKind::MetaResize { enabled } => TypedStatement::new(
                TypedStatementKind::MetaResize { enabled: *enabled },
                stmt.span,
            ),

            StatementKind::MetaResizeStretch => {
                TypedStatement::new(TypedStatementKind::MetaResizeStretch, stmt.span)
            }

            StatementKind::MetaResizeSmooth => {
                TypedStatement::new(TypedStatementKind::MetaResizeSmooth, stmt.span)
            }

            StatementKind::MetaStatic => {
                TypedStatement::new(TypedStatementKind::MetaStatic, stmt.span)
            }

            StatementKind::MetaDynamic => {
                TypedStatement::new(TypedStatementKind::MetaDynamic, stmt.span)
            }

            StatementKind::MetaDebug => {
                TypedStatement::new(TypedStatementKind::MetaDebug, stmt.span)
            }

            StatementKind::MetaIncludeOnce => {
                TypedStatement::new(TypedStatementKind::MetaIncludeOnce, stmt.span)
            }

            StatementKind::MetaExeIcon { filename } => TypedStatement::new(
                TypedStatementKind::MetaExeIcon {
                    filename: filename.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaVersionInfo { key, value } => TypedStatement::new(
                TypedStatementKind::MetaVersionInfo {
                    key: key.clone(),
                    value: value.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaErrorDirective { message } => TypedStatement::new(
                TypedStatementKind::MetaErrorDirective {
                    message: message.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaEmbed { filename } => TypedStatement::new(
                TypedStatementKind::MetaEmbed {
                    filename: filename.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaMidiSoundFont { filename } => TypedStatement::new(
                TypedStatementKind::MetaMidiSoundFont {
                    filename: filename.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaUnstable { feature } => TypedStatement::new(
                TypedStatementKind::MetaUnstable {
                    feature: feature.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaFormat => {
                TypedStatement::new(TypedStatementKind::MetaFormat, stmt.span)
            }

            StatementKind::MetaUseLibrary { library } => TypedStatement::new(
                TypedStatementKind::MetaUseLibrary {
                    library: library.clone(),
                },
                stmt.span,
            ),
        }
    }

    /// Registers an external function/sub in the symbol table.
    fn register_external_function(
        &mut self,
        decl: &ExternalDeclaration,
    ) -> TypedExternalDeclaration {
        // Determine the return type for functions
        let return_type = if decl.is_function {
            decl.return_type
                .as_ref()
                .map(from_type_spec)
                .unwrap_or(BasicType::Single) // Default return type
        } else {
            BasicType::Void
        };

        // Convert parameters to typed form
        let typed_params: Vec<TypedExternalParam> = decl
            .params
            .iter()
            .map(|p| TypedExternalParam {
                name: p.name.clone(),
                typ: from_type_spec(&p.type_spec),
                is_byval: p.is_byval,
            })
            .collect();

        // Build parameter type list for the symbol
        let param_types: Vec<BasicType> = typed_params.iter().map(|p| p.typ.clone()).collect();

        // Strip type suffix from function name for symbol lookup
        // e.g., "myabs&" -> "myabs" so calls can omit the suffix
        use crate::semantic::types::strip_suffix;
        let base_name = strip_suffix(&decl.name).to_string();

        // For C name, use ALIAS if provided, otherwise strip BASIC suffix
        // (C doesn't have type suffixes like & % $ in identifiers)
        let c_name = decl.alias.clone().unwrap_or_else(|| base_name.clone());

        // Register the function in the symbol table
        // Note: We use define_symbol which may fail if symbol already exists,
        // but we'll ignore duplicates for external functions (they can be redeclared)
        let _ = self.symbols.define_symbol(Symbol {
            name: base_name.clone(),
            kind: SymbolKind::ExternalFunction {
                c_name: c_name.clone(),
                params: param_types,
                return_type: return_type.clone(),
            },
            basic_type: return_type.clone(),
            span: Span::new(0, 0), // External functions don't have source location
            is_mutable: false,
        });

        TypedExternalDeclaration {
            name: base_name,
            c_name,
            params: typed_params,
            return_type,
            is_function: decl.is_function,
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

    /// Evaluates array dimensions for STATIC arrays.
    /// While STATIC arrays ideally have constant bounds, we allow variable bounds
    /// for compatibility with QB code that uses variables in DIM.
    fn evaluate_array_dimension(
        &mut self,
        dim: &ArrayDimension,
        _span: crate::ast::Span,
    ) -> TypedArrayDimension {
        // Evaluate lower bound (if provided)
        let lower = if let Some(lower_expr) = &dim.lower {
            let typed_lower = self.check_expr(lower_expr);
            // Ensure the expression is numeric
            if !typed_lower.basic_type.is_numeric() && typed_lower.basic_type != BasicType::Unknown
            {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: typed_lower.basic_type.to_string(),
                    span: lower_expr.span,
                });
            }
            // Try to evaluate as constant, but don't error if not possible
            match self.try_evaluate_const_expr(&typed_lower) {
                Some(ConstValue::Integer(v)) => v,
                Some(ConstValue::Float(v)) => v as i64,
                _ => 0, // Runtime bound - use 0 as placeholder
            }
        } else {
            self.symbols.option_base()
        };

        // Evaluate upper bound (required)
        let typed_upper = self.check_expr(&dim.upper);
        // Ensure the expression is numeric
        if !typed_upper.basic_type.is_numeric() && typed_upper.basic_type != BasicType::Unknown {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_upper.basic_type.to_string(),
                span: dim.upper.span,
            });
        }
        // Try to evaluate as constant, but don't error if not possible
        let upper = match self.try_evaluate_const_expr(&typed_upper) {
            Some(ConstValue::Integer(v)) => v,
            Some(ConstValue::Float(v)) => v as i64,
            _ => 10, // Runtime bound - use 10 as placeholder
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
