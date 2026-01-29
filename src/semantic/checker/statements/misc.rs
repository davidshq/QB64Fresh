//! Miscellaneous statement type checking dispatch.
//!
//! This module provides a dispatch function for miscellaneous statements that don't
//! fit into other categories, including PRINT, INPUT, system statements, meta directives,
//! and other simple statements.

use crate::ast::StatementKind;
use crate::semantic::{error::SemanticError, typed_ir::*};

use super::super::super::TypeChecker;

use crate::semantic::symbols::{Symbol, SymbolKind, UserTypeDefinition, UserTypeMember};
use crate::semantic::types::{BasicType, from_type_spec, type_from_suffix};

/// Type checks miscellaneous statements.
pub(super) fn check_misc_stmt(
    checker: &mut TypeChecker,
    kind: &StatementKind,
    span: crate::ast::Span,
) -> TypedStatement {
    match kind {
        // Note: Print, PrintUsing, Input, LineInput, System, Sleep, Wait, Delay,
        // Limit, Erase, and KeyClear are handled directly in statements.rs before
        // this dispatch, so they won't reach here.
        StatementKind::Expression(expr) => {
            let typed_expr = checker.check_expr(expr);
            TypedStatement::new(TypedStatementKind::Expression(typed_expr), span)
        }

        StatementKind::Comment(text) => {
            TypedStatement::new(TypedStatementKind::Comment(text.clone()), span)
        }

        // Preprocessor directives - pass through as-is for later processing
        StatementKind::IncludeDirective { path } => TypedStatement::new(
            TypedStatementKind::IncludeDirective { path: path.clone() },
            span,
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
            if checker.evaluate_meta_condition(condition) {
                // Main condition is true - include then_branch statements
                let typed_stmts: Vec<TypedStatement> = then_branch
                    .iter()
                    .map(|s| checker.check_statement(s))
                    .collect();
                return TypedStatement::new(
                    TypedStatementKind::ConditionalBlockResolved {
                        original_condition: condition.clone(),
                        statements: typed_stmts,
                    },
                    span,
                );
            }

            // Check $ELSEIF conditions
            for (elseif_cond, elseif_body) in elseif_branches {
                if checker.evaluate_meta_condition(elseif_cond) {
                    let typed_stmts: Vec<TypedStatement> = elseif_body
                        .iter()
                        .map(|s| checker.check_statement(s))
                        .collect();
                    return TypedStatement::new(
                        TypedStatementKind::ConditionalBlockResolved {
                            original_condition: elseif_cond.clone(),
                            statements: typed_stmts,
                        },
                        span,
                    );
                }
            }

            // No conditions matched - use $ELSE branch if present
            if let Some(else_body) = else_branch {
                let typed_stmts: Vec<TypedStatement> = else_body
                    .iter()
                    .map(|s| checker.check_statement(s))
                    .collect();
                return TypedStatement::new(
                    TypedStatementKind::ConditionalBlockResolved {
                        original_condition: "$ELSE".to_string(),
                        statements: typed_stmts,
                    },
                    span,
                );
            }

            // No branch selected - emit empty block
            TypedStatement::new(
                TypedStatementKind::ConditionalBlockResolved {
                    original_condition: condition.clone(),
                    statements: Vec::new(),
                },
                span,
            )
        }

        StatementKind::MetaCommand { command, args } => TypedStatement::new(
            TypedStatementKind::MetaCommand {
                command: command.clone(),
                args: args.clone(),
            },
            span,
        ),

        StatementKind::MetaLet { name, value } => {
            // Store $LET variable as a constant in the symbol table
            checker.symbols.define_meta_let(name, *value, span);
            TypedStatement::new(
                TypedStatementKind::MetaLet {
                    name: name.clone(),
                    value: *value,
                },
                span,
            )
        }

        StatementKind::MetaChecking { enabled } => {
            TypedStatement::new(TypedStatementKind::MetaChecking { enabled: *enabled }, span)
        }

        StatementKind::MetaConsole { only } => {
            TypedStatement::new(TypedStatementKind::MetaConsole { only: *only }, span)
        }

        StatementKind::MetaScreenHide => {
            TypedStatement::new(TypedStatementKind::MetaScreenHide, span)
        }

        StatementKind::MetaScreenShow => {
            TypedStatement::new(TypedStatementKind::MetaScreenShow, span)
        }
        StatementKind::Swap { left, right } => {
            let typed_left = checker.check_expr(left);
            let typed_right = checker.check_expr(right);

            // Check that both expressions are lvalues (variables or array elements)
            // For now, we'll verify at codegen; semantic check ensures exact type match
            // SWAP requires exact type match to prevent silent data loss
            // (e.g., swapping INTEGER and LONG would truncate the LONG value)
            if typed_left.basic_type != typed_right.basic_type {
                checker.errors.push(SemanticError::TypeMismatch {
                    expected: typed_left.basic_type.to_string(),
                    found: typed_right.basic_type.to_string(),
                    span,
                });
            }

            TypedStatement::new(
                TypedStatementKind::Swap {
                    left: typed_left,
                    right: typed_right,
                },
                span,
            )
        }

        StatementKind::Continue { continue_type } => {
            use crate::ast::ContinueType;

            // Validate that CONTINUE is inside the matching loop type
            let valid = match continue_type {
                ContinueType::For => checker.loop_context.for_depth > 0,
                ContinueType::While => checker.loop_context.while_depth > 0,
                ContinueType::Do => checker.loop_context.do_depth > 0,
                // Bare _CONTINUE is valid inside any loop
                ContinueType::Innermost => {
                    checker.loop_context.for_depth > 0
                        || checker.loop_context.while_depth > 0
                        || checker.loop_context.do_depth > 0
                }
            };

            if !valid {
                let loop_name = match continue_type {
                    ContinueType::For => "FOR",
                    ContinueType::While => "WHILE",
                    ContinueType::Do => "DO",
                    ContinueType::Innermost => "any",
                };
                checker.errors.push(SemanticError::ContinueOutsideLoop {
                    loop_type: loop_name.to_string(),
                    span,
                });
            }

            TypedStatement::new(
                TypedStatementKind::Continue {
                    continue_type: *continue_type,
                },
                span,
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
                span,
                custom_type: *custom_type,
            };

            if let Err(_existing) = checker.symbols.define_user_type(user_type) {
                checker.errors.push(SemanticError::DuplicateType {
                    name: name.clone(),
                    original_span: span, // Could track the original definition span
                    duplicate_span: span,
                });
            }

            TypedStatement::new(
                TypedStatementKind::TypeDefinition {
                    name: name.clone(),
                    members: typed_members,
                    custom_type: *custom_type,
                },
                span,
            )
        }

        StatementKind::Data { values } => checker.check_data(values, span),

        StatementKind::Read { targets } => checker.check_read(targets, span),

        StatementKind::Restore { label } => checker.check_restore(label.as_deref(), span),

        StatementKind::Randomize { seed } => checker.check_randomize(seed.as_ref(), span),

        // ==================== File I/O Statements ====================
        StatementKind::OpenFile {
            filename,
            mode,
            access,
            lock,
            file_num,
            record_len,
        } => checker.check_open_file(
            filename,
            *mode,
            *access,
            *lock,
            file_num,
            record_len.as_ref(),
            span,
        ),

        StatementKind::OpenFileLegacy {
            mode_expr,
            file_num,
            filename,
            record_len,
        } => {
            checker.check_open_file_legacy(mode_expr, file_num, filename, record_len.as_ref(), span)
        }

        StatementKind::CloseFile { file_nums } => checker.check_close_file(file_nums, span),

        StatementKind::FilePrint {
            file_num,
            values,
            newline,
        } => checker.check_file_print(file_num, values, *newline, span),

        StatementKind::FileWrite { file_num, values } => {
            checker.check_file_write(file_num, values, span)
        }

        StatementKind::FileInput { file_num, targets } => {
            checker.check_file_input(file_num, targets, span)
        }

        StatementKind::FileLineInput { file_num, target } => {
            checker.check_file_line_input(file_num, target, span)
        }

        StatementKind::FileGet {
            file_num,
            position,
            target,
        } => checker.check_file_get(file_num, position.as_ref(), target, span),

        StatementKind::FilePut {
            file_num,
            position,
            target,
        } => checker.check_file_put(file_num, position.as_ref(), target, span),

        StatementKind::FileSeek { file_num, position } => {
            checker.check_file_seek(file_num, position, span)
        }

        // ==================== Error Handling Statements ====================
        StatementKind::OnErrorGoto { target } => checker.check_on_error_goto(target, span),

        StatementKind::OnErrorResumeNext => checker.check_on_error_resume_next(span),

        StatementKind::ResumeStmt { target } => checker.check_resume_stmt(target, span),

        StatementKind::ErrorStmt { code } => checker.check_error_stmt(code, span),

        // ==================== Computed Control Flow ====================
        StatementKind::OnGoto { selector, targets } => {
            checker.check_on_goto(selector, targets, span)
        }

        StatementKind::OnGosub { selector, targets } => {
            checker.check_on_gosub(selector, targets, span)
        }

        // ==================== DEF FN ====================
        StatementKind::DefFn { name, params, body } => {
            checker.check_def_fn(name, params, body, span)
        }

        StatementKind::DefFnMultiLine { name, params, body } => {
            checker.check_def_fn_multi_line(name, params, body, span)
        }

        StatementKind::DefSeg { segment } => checker.check_def_seg(segment.as_ref(), span),

        StatementKind::Poke { address, value } => {
            let typed_address = checker.check_expr(address);
            let typed_value = checker.check_expr(value);
            TypedStatement::new(
                TypedStatementKind::Poke {
                    address: typed_address,
                    value: typed_value,
                },
                span,
            )
        }

        StatementKind::MemPutTyped {
            mem,
            offset,
            value,
            value_type,
        } => {
            let typed_mem = checker.check_expr(mem);
            let typed_offset = checker.check_expr(offset);
            let typed_value = checker.check_expr(value);
            let basic_type = checker.parse_type_name(value_type);
            TypedStatement::new(
                TypedStatementKind::MemPutTyped {
                    mem: typed_mem,
                    offset: typed_offset,
                    value: typed_value,
                    value_type: basic_type,
                },
                span,
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
                        .unwrap_or_else(|| checker.symbols.default_type_for(&v.name));

                    // Evaluate dimensions
                    let dims: Vec<TypedArrayDimension> = v
                        .dimensions
                        .iter()
                        .map(|d| checker.evaluate_array_dimension(d, span))
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
                        span,
                        is_mutable: true,
                    };
                    // Report duplicate variable errors for COMMON statements
                    if let Err(duplicate) = checker.symbols.define_symbol(symbol) {
                        let (existing, new) = *duplicate;
                        checker.errors.push(SemanticError::DuplicateVariable {
                            name: v.name.clone(),
                            original_span: existing.span,
                            duplicate_span: new.span,
                        });
                    }

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
                span,
            )
        }

        StatementKind::SharedStmt { variables } => {
            // SHARED statement inside SUB/FUNCTION declares access to module-level variables.
            // In classic BASIC, SHARED can also implicitly create variables at module level
            // if they don't already exist.

            // Must be inside a procedure
            if !checker.symbols.in_procedure() {
                checker
                    .errors
                    .push(SemanticError::SharedOutsideProcedure { span });
            } else {
                // For each variable, check if it exists at module level
                // If not, implicitly declare it (classic BASIC behavior)
                for var_name in variables {
                    if checker.symbols.lookup_global_symbol(var_name).is_some() {
                        // Variable exists at module level - register as shared
                        checker.symbols.add_shared_var(var_name.clone());
                    } else {
                        // Classic BASIC: implicitly declare at module level
                        // Determine type from name suffix or default
                        let basic_type = type_from_suffix(var_name)
                            .unwrap_or_else(|| checker.symbols.default_type_for(var_name));

                        let symbol = Symbol {
                            name: var_name.clone(),
                            kind: SymbolKind::Variable,
                            basic_type,
                            span,
                            is_mutable: true,
                        };

                        // Define at global scope and mark as module-shared
                        checker.symbols.define_shared_symbol(symbol);
                        // Also register for this procedure's SHARED access
                        checker.symbols.add_shared_var(var_name.clone());
                    }
                }
            }

            TypedStatement::new(
                TypedStatementKind::SharedStmt {
                    variables: variables.clone(),
                },
                span,
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
                    .unwrap_or_else(|| checker.symbols.default_type_for(&var.name));

                // Evaluate array dimensions
                let typed_dims: Vec<TypedArrayDimension> = var
                    .dimensions
                    .iter()
                    .map(|d| {
                        let lower = if let Some(lower_expr) = &d.lower {
                            let typed_lower = checker.check_expr(lower_expr);
                            match checker.try_evaluate_const_expr(&typed_lower) {
                                Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                                Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                                _ => {
                                    checker.errors.push(SemanticError::NonConstantExpression {
                                        span: lower_expr.span,
                                    });
                                    0
                                }
                            }
                        } else {
                            checker.symbols.option_base()
                        };

                        let typed_upper = checker.check_expr(&d.upper);
                        let upper = match checker.try_evaluate_const_expr(&typed_upper) {
                            Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                            Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                            _ => {
                                checker.errors.push(SemanticError::NonConstantExpression {
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
                    span,
                    is_mutable: true,
                };

                if let Err(existing) = checker.symbols.define_symbol(symbol) {
                    checker.errors.push(SemanticError::DuplicateVariable {
                        name: var.name.clone(),
                        original_span: existing.0.span,
                        duplicate_span: span,
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
                span,
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
                let existing_array = checker.symbols.lookup_array(&var.name);

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
                    .unwrap_or_else(|| checker.symbols.default_type_for(&var.name));

                // Evaluate dimensions - REDIM allows runtime expressions
                let typed_dims: Vec<crate::semantic::typed_ir::TypedRedimDimension> = var
                    .dimensions
                    .iter()
                    .map(|d| checker.evaluate_array_dimension_runtime(d))
                    .collect();

                // Update symbol table (or define if not exists)
                // Use ArrayVariable kind so array passing works correctly.
                // REDIM can resize existing arrays (including array parameters),
                // so we use update_or_define to replace any existing symbol.
                // For SHARED arrays, define at module scope.
                //
                // IMPORTANT: If this is REDIM _PRESERVE on a module-level SHARED array,
                // we should update the GLOBAL scope entry, not create a local copy.
                //
                // Note: For REDIM, bounds are runtime-determined, so we use placeholder
                // values (0, 0) for the symbol table. The actual bounds are in the
                // TypedRedimDimension expressions for codegen.
                let symbol = Symbol {
                    name: var.name.clone(),
                    kind: SymbolKind::ArrayVariable {
                        dimensions: typed_dims
                            .iter()
                            .map(|_d| crate::semantic::symbols::ArrayDimInfo {
                                lower_bound: 0, // Placeholder - bounds determined at runtime
                                upper_bound: 0, // Placeholder - bounds determined at runtime
                            })
                            .collect(),
                    },
                    basic_type: element_type.clone(),
                    span,
                    is_mutable: true,
                };
                if *shared {
                    checker.symbols.define_shared_symbol(symbol);
                } else if *preserve && checker.symbols.is_module_shared(&var.name) {
                    // REDIM _PRESERVE on a module-level SHARED array:
                    // Update the global scope entry, not create a local copy
                    checker.symbols.update_shared_symbol(symbol);
                } else {
                    checker.symbols.update_or_define_symbol(symbol);
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
                span,
            )
        }

        // ==================== Graphics Statements ====================
        _ => unreachable!("Not a misc statement"),
    }
}
