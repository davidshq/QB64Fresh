//! File I/O statement type checking.
//!
//! This module handles type checking for file I/O statements:
//! - OPEN, CLOSE
//! - PRINT #, WRITE #, INPUT #, LINE INPUT #
//! - GET, PUT, SEEK
//! - LOCK, UNLOCK

use crate::ast::{Expr, FileAccess, FileLock, FileMode, InputTarget, PrintItem, Span};
use crate::semantic::{
    error::SemanticError,
    symbols::{Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, type_from_suffix},
};

use super::super::TypeChecker;

impl<'a> TypeChecker<'a> {
    /// Type checks OPEN statement.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::semantic::checker) fn check_open_file(
        &mut self,
        filename: &Expr,
        mode: FileMode,
        access: Option<FileAccess>,
        lock: Option<FileLock>,
        file_num: &Expr,
        record_len: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_filename = self.check_expr(filename);
        let typed_file_num = self.check_expr(file_num);
        let typed_record_len = record_len.map(|e| self.check_expr(e));

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
                mode,
                access,
                lock,
                file_num: typed_file_num,
                record_len: typed_record_len,
            },
            span,
        )
    }

    /// Type checks legacy OPEN statement (OPEN mode$, file_num, filename$).
    pub(in crate::semantic::checker) fn check_open_file_legacy(
        &mut self,
        mode_expr: &Expr,
        file_num: &Expr,
        filename: &Expr,
        record_len: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_mode = self.check_expr(mode_expr);
        let typed_file_num = self.check_expr(file_num);
        let typed_filename = self.check_expr(filename);
        let typed_record_len = record_len.map(|e| self.check_expr(e));

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
            span,
        )
    }

    /// Type checks CLOSE statement.
    pub(in crate::semantic::checker) fn check_close_file(
        &mut self,
        file_nums: &[Expr],
        span: Span,
    ) -> TypedStatement {
        let typed_file_nums: Vec<TypedExpr> =
            file_nums.iter().map(|e| self.check_expr(e)).collect();

        TypedStatement::new(
            TypedStatementKind::CloseFile {
                file_nums: typed_file_nums,
            },
            span,
        )
    }

    /// Type checks LOCK # statement (stub: no-op in inline runtime).
    pub(in crate::semantic::checker) fn check_lock_file(
        &mut self,
        file_num: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        TypedStatement::new(
            TypedStatementKind::LockFile {
                file_num: typed_file_num,
            },
            span,
        )
    }

    /// Type checks UNLOCK # statement (stub: no-op in inline runtime).
    pub(in crate::semantic::checker) fn check_unlock_file(
        &mut self,
        file_num: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        TypedStatement::new(
            TypedStatementKind::UnlockFile {
                file_num: typed_file_num,
            },
            span,
        )
    }

    /// Type checks PRINT # statement.
    pub(in crate::semantic::checker) fn check_file_print(
        &mut self,
        file_num: &Expr,
        values: &[PrintItem],
        newline: bool,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        let typed_items = self.check_print_items(values);

        TypedStatement::new(
            TypedStatementKind::FilePrint {
                file_num: typed_file_num,
                items: typed_items,
                newline,
            },
            span,
        )
    }

    /// Type checks WRITE # statement.
    pub(in crate::semantic::checker) fn check_file_write(
        &mut self,
        file_num: &Expr,
        values: &[Expr],
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        let typed_values: Vec<TypedExpr> = values.iter().map(|e| self.check_expr(e)).collect();

        TypedStatement::new(
            TypedStatementKind::FileWrite {
                file_num: typed_file_num,
                values: typed_values,
            },
            span,
        )
    }

    /// Type checks INPUT # statement.
    pub(in crate::semantic::checker) fn check_file_input(
        &mut self,
        file_num: &Expr,
        targets: &[InputTarget],
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);

        // Type-check each input target
        let typed_targets: Vec<TypedInputTarget> = targets
            .iter()
            .map(|target| self.check_input_target(target, span))
            .collect();

        TypedStatement::new(
            TypedStatementKind::FileInput {
                file_num: typed_file_num,
                targets: typed_targets,
            },
            span,
        )
    }

    /// Type checks LINE INPUT # statement.
    pub(in crate::semantic::checker) fn check_file_line_input(
        &mut self,
        file_num: &Expr,
        target: &InputTarget,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);

        // LINE INPUT # always reads into a string
        let typed_target = match target {
            InputTarget::Variable(name) => {
                if let Some(existing) = self.symbols.lookup_symbol(name) {
                    // Variable exists - check for type conflict (LINE INPUT # requires STRING)
                    if !existing.basic_type.is_string() {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: "STRING".to_string(),
                            found: existing.basic_type.to_string(),
                            span,
                        });
                    }
                } else {
                    // Create new variable as STRING
                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: BasicType::String,
                        span,
                        is_mutable: true,
                    };
                    // Should never fail since we checked lookup_symbol, but handle it
                    if let Err(dup) = self.symbols.define_symbol(symbol) {
                        let (existing, _) = *dup;
                        self.errors.push(SemanticError::DuplicateVariable {
                            name: name.clone(),
                            original_span: existing.span,
                            duplicate_span: span,
                        });
                    }
                }
                TypedInputTarget::Variable {
                    name: name.clone(),
                    basic_type: BasicType::String,
                }
            }
            InputTarget::ArrayElement { name, indices } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let dimensions = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    if let SymbolKind::ArrayVariable {
                        dimensions: dim_info,
                    } = &symbol.kind
                    {
                        dim_info
                            .iter()
                            .map(|d| TypedArrayDimension {
                                lower: d.lower_bound,
                                upper: d.upper_bound,
                            })
                            .collect()
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };
                TypedInputTarget::ArrayElement {
                    name: name.clone(),
                    indices: typed_indices,
                    element_type: BasicType::String,
                    dimensions,
                }
            }
            InputTarget::ArrayElementField {
                name,
                indices,
                fields,
            } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let dimensions = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    if let SymbolKind::ArrayVariable {
                        dimensions: dim_info,
                    } = &symbol.kind
                    {
                        dim_info
                            .iter()
                            .map(|d| TypedArrayDimension {
                                lower: d.lower_bound,
                                upper: d.upper_bound,
                            })
                            .collect()
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };
                TypedInputTarget::ArrayElementField {
                    name: name.clone(),
                    indices: typed_indices,
                    fields: fields.clone(),
                    field_type: BasicType::String,
                    dimensions,
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
            span,
        )
    }

    /// Type checks GET # statement.
    pub(in crate::semantic::checker) fn check_file_get(
        &mut self,
        file_num: &Expr,
        position: Option<&Expr>,
        target: &InputTarget,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        let typed_position = position.map(|e| self.check_expr(e));

        // Convert InputTarget to TypedInputTarget with resolved names
        let typed_target = self.check_input_target_with_resolve(target, span);

        TypedStatement::new(
            TypedStatementKind::FileGet {
                file_num: typed_file_num,
                position: typed_position,
                target: typed_target,
            },
            span,
        )
    }

    /// Type checks PUT # statement.
    pub(in crate::semantic::checker) fn check_file_put(
        &mut self,
        file_num: &Expr,
        position: Option<&Expr>,
        target: &InputTarget,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        let typed_position = position.map(|e| self.check_expr(e));

        // Convert InputTarget to TypedInputTarget with resolved names
        let typed_target = self.check_input_target_with_resolve(target, span);

        TypedStatement::new(
            TypedStatementKind::FilePut {
                file_num: typed_file_num,
                position: typed_position,
                target: typed_target,
            },
            span,
        )
    }

    /// Type checks SEEK statement.
    pub(in crate::semantic::checker) fn check_file_seek(
        &mut self,
        file_num: &Expr,
        position: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_file_num = self.check_expr(file_num);
        let typed_position = self.check_expr(position);

        TypedStatement::new(
            TypedStatementKind::FileSeek {
                file_num: typed_file_num,
                position: typed_position,
            },
            span,
        )
    }

    /// Helper to type check an InputTarget.
    fn check_input_target(&mut self, target: &InputTarget, span: Span) -> TypedInputTarget {
        match target {
            InputTarget::Variable(name) => {
                let var_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    // Variable exists - use existing type
                    symbol.basic_type.clone()
                } else {
                    // Infer type from suffix or default
                    let inferred = type_from_suffix(name)
                        .unwrap_or_else(|| self.symbols.default_type_for(name));

                    // Define the variable
                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: inferred.clone(),
                        span,
                        is_mutable: true,
                    };
                    // Should never fail since we checked lookup_symbol, but handle it
                    if let Err(dup) = self.symbols.define_symbol(symbol) {
                        let (existing, _) = *dup;
                        // Type conflict - variable was defined elsewhere
                        self.errors.push(SemanticError::DuplicateVariable {
                            name: name.clone(),
                            original_span: existing.span,
                            duplicate_span: span,
                        });
                        // Use existing type to avoid cascading errors
                        existing.basic_type.clone()
                    } else {
                        inferred
                    }
                };
                TypedInputTarget::Variable {
                    name: name.clone(),
                    basic_type: var_type,
                }
            }
            InputTarget::ArrayElement { name, indices } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let (element_type, dimensions) =
                    if let Some(symbol) = self.symbols.lookup_symbol(name) {
                        if let SymbolKind::ArrayVariable {
                            dimensions: dim_info,
                        } = &symbol.kind
                        {
                            let typed_dims: Vec<TypedArrayDimension> = dim_info
                                .iter()
                                .map(|d| TypedArrayDimension {
                                    lower: d.lower_bound,
                                    upper: d.upper_bound,
                                })
                                .collect();
                            let element_type =
                                if let BasicType::Array { element_type, .. } = &symbol.basic_type {
                                    (**element_type).clone()
                                } else {
                                    symbol.basic_type.clone()
                                };
                            (element_type, typed_dims)
                        } else if let BasicType::Array { element_type, .. } = &symbol.basic_type {
                            ((**element_type).clone(), Vec::new())
                        } else {
                            (symbol.basic_type.clone(), Vec::new())
                        }
                    } else {
                        let inferred = type_from_suffix(name)
                            .unwrap_or_else(|| self.symbols.default_type_for(name));
                        (inferred, Vec::new())
                    };
                TypedInputTarget::ArrayElement {
                    name: name.clone(),
                    indices: typed_indices,
                    element_type,
                    dimensions,
                }
            }
            InputTarget::ArrayElementField {
                name,
                indices,
                fields,
            } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let (field_type, dimensions) =
                    if let Some(symbol) = self.symbols.lookup_symbol(name) {
                        if let SymbolKind::ArrayVariable {
                            dimensions: dim_info,
                        } = &symbol.kind
                        {
                            let typed_dims: Vec<TypedArrayDimension> = dim_info
                                .iter()
                                .map(|d| TypedArrayDimension {
                                    lower: d.lower_bound,
                                    upper: d.upper_bound,
                                })
                                .collect();
                            let field_type = fields
                                .last()
                                .and_then(|f| type_from_suffix(f))
                                .unwrap_or(BasicType::Single);
                            (field_type, typed_dims)
                        } else {
                            let field_type = fields
                                .last()
                                .and_then(|f| type_from_suffix(f))
                                .unwrap_or(BasicType::Single);
                            (field_type, Vec::new())
                        }
                    } else {
                        let field_type = fields
                            .last()
                            .and_then(|f| type_from_suffix(f))
                            .unwrap_or(BasicType::Single);
                        (field_type, Vec::new())
                    };
                TypedInputTarget::ArrayElementField {
                    name: name.clone(),
                    indices: typed_indices,
                    fields: fields.clone(),
                    field_type,
                    dimensions,
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
        }
    }

    /// Helper to type check an InputTarget with symbol resolution.
    fn check_input_target_with_resolve(
        &mut self,
        target: &InputTarget,
        span: Span,
    ) -> TypedInputTarget {
        match target {
            InputTarget::Variable(name) => {
                let (resolved_name, var_type) =
                    if let Some(symbol) = self.symbols.lookup_symbol(name) {
                        // Variable exists - use existing type
                        (symbol.name.clone(), symbol.basic_type.clone())
                    } else {
                        // Infer type from suffix or default
                        let inferred = type_from_suffix(name)
                            .unwrap_or_else(|| self.symbols.default_type_for(name));
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            basic_type: inferred.clone(),
                            span,
                            is_mutable: true,
                        };
                        // Should never fail since we checked lookup_symbol, but handle it
                        if let Err(dup) = self.symbols.define_symbol(symbol) {
                            let (existing, _) = *dup;
                            // Type conflict - variable was defined elsewhere
                            self.errors.push(SemanticError::DuplicateVariable {
                                name: name.clone(),
                                original_span: existing.span,
                                duplicate_span: span,
                            });
                            // Use existing type to avoid cascading errors
                            (existing.name.clone(), existing.basic_type.clone())
                        } else {
                            (name.clone(), inferred)
                        }
                    };
                TypedInputTarget::Variable {
                    name: resolved_name,
                    basic_type: var_type,
                }
            }
            InputTarget::ArrayElement { name, indices } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let (resolved_name, element_type, dimensions) =
                    if let Some(symbol) = self.symbols.lookup_array(name) {
                        let dims = if let SymbolKind::ArrayVariable {
                            dimensions: dim_info,
                        } = &symbol.kind
                        {
                            dim_info
                                .iter()
                                .map(|d| TypedArrayDimension {
                                    lower: d.lower_bound,
                                    upper: d.upper_bound,
                                })
                                .collect()
                        } else {
                            Vec::new()
                        };
                        (symbol.name.clone(), symbol.basic_type.clone(), dims)
                    } else {
                        let element_type = type_from_suffix(name)
                            .unwrap_or_else(|| self.symbols.default_type_for(name));
                        (name.clone(), element_type, Vec::new())
                    };
                TypedInputTarget::ArrayElement {
                    name: resolved_name,
                    indices: typed_indices,
                    element_type,
                    dimensions,
                }
            }
            InputTarget::ArrayElementField {
                name,
                indices,
                fields,
            } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let (resolved_name, dimensions) =
                    if let Some(symbol) = self.symbols.lookup_array(name) {
                        let dims = if let SymbolKind::ArrayVariable {
                            dimensions: dim_info,
                        } = &symbol.kind
                        {
                            dim_info
                                .iter()
                                .map(|d| TypedArrayDimension {
                                    lower: d.lower_bound,
                                    upper: d.upper_bound,
                                })
                                .collect()
                        } else {
                            Vec::new()
                        };
                        (symbol.name.clone(), dims)
                    } else {
                        (name.clone(), Vec::new())
                    };
                TypedInputTarget::ArrayElementField {
                    name: resolved_name,
                    indices: typed_indices,
                    fields: fields.clone(),
                    field_type: BasicType::Single,
                    dimensions,
                }
            }
            InputTarget::Field { name, fields } => {
                let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    symbol.name.clone()
                } else {
                    name.clone()
                };
                TypedInputTarget::Field {
                    name: resolved_name,
                    fields: fields.clone(),
                    field_type: BasicType::Single,
                }
            }
        }
    }
}

/// Dispatch function for file I/O statements.
pub(super) fn check_io_stmt(
    checker: &mut super::super::TypeChecker,
    kind: &crate::ast::StatementKind,
    span: crate::ast::Span,
) -> crate::semantic::typed_ir::TypedStatement {
    match kind {
        crate::ast::StatementKind::OpenFile {
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
        crate::ast::StatementKind::OpenFileLegacy {
            mode_expr,
            file_num,
            filename,
            record_len,
        } => {
            checker.check_open_file_legacy(mode_expr, file_num, filename, record_len.as_ref(), span)
        }
        crate::ast::StatementKind::CloseFile { file_nums } => {
            checker.check_close_file(file_nums, span)
        }
        crate::ast::StatementKind::LockFile { file_num } => checker.check_lock_file(file_num, span),
        crate::ast::StatementKind::UnlockFile { file_num } => {
            checker.check_unlock_file(file_num, span)
        }
        crate::ast::StatementKind::FilePrint {
            file_num,
            values,
            newline,
        } => checker.check_file_print(file_num, values, *newline, span),
        crate::ast::StatementKind::FileWrite { file_num, values } => {
            checker.check_file_write(file_num, values, span)
        }
        crate::ast::StatementKind::FileInput { file_num, targets } => {
            checker.check_file_input(file_num, targets, span)
        }
        crate::ast::StatementKind::FileLineInput { file_num, target } => {
            checker.check_file_line_input(file_num, target, span)
        }
        crate::ast::StatementKind::FileGet {
            file_num,
            position,
            target,
        } => checker.check_file_get(file_num, position.as_ref(), target, span),
        crate::ast::StatementKind::FilePut {
            file_num,
            position,
            target,
        } => checker.check_file_put(file_num, position.as_ref(), target, span),
        crate::ast::StatementKind::FileSeek { file_num, position } => {
            checker.check_file_seek(file_num, position, span)
        }
        _ => unreachable!("Not a file I/O statement"),
    }
}
