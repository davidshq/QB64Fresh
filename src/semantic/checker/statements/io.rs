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
                if self.symbols.lookup_symbol(name).is_none() {
                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: BasicType::String,
                        span,
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
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
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
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
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
                    let _ = self.symbols.define_symbol(symbol);

                    inferred
                };
                TypedInputTarget::Variable {
                    name: name.clone(),
                    basic_type: var_type,
                }
            }
            InputTarget::ArrayElement { name, indices } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let element_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    if let BasicType::Array { element_type, .. } = &symbol.basic_type {
                        (**element_type).clone()
                    } else {
                        symbol.basic_type.clone()
                    }
                } else {
                    type_from_suffix(name).unwrap_or_else(|| self.symbols.default_type_for(name))
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
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
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
                        (symbol.name.clone(), symbol.basic_type.clone())
                    } else {
                        let inferred = type_from_suffix(name)
                            .unwrap_or_else(|| self.symbols.default_type_for(name));
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            basic_type: inferred.clone(),
                            span,
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
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
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
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                let resolved_name = if let Some(symbol) = self.symbols.lookup_array(name) {
                    symbol.name.clone()
                } else {
                    name.clone()
                };
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
                TypedInputTarget::Field {
                    name: resolved_name,
                    fields: fields.clone(),
                    field_type: BasicType::Single,
                }
            }
        }
    }
}
