//! Type checker for QB64Fresh semantic analysis.
//!
//! The type checker validates expressions and statements, ensuring type
//! compatibility and producing a typed IR. It handles:
//!
//! - **Expression type inference**: Determining the type of each expression
//! - **Binary/unary operation validation**: Checking operator applicability
//! - **Assignment type checking**: Ensuring values match target types
//! - **Function call validation**: Argument count and type checking
//! - **Control flow validation**: EXIT context, FOR/NEXT matching
//!
//! # Error Recovery
//!
//! The type checker continues after errors when possible, collecting all
//! errors for batch reporting rather than stopping at the first problem.

use crate::ast::{
    BinaryOp, CaseCompareOp, CaseMatch, DataValue, DoCondition, Expr, ExprKind, PrintItem, Span,
    Statement, StatementKind, UnaryOp,
};
use crate::semantic::{
    error::SemanticError,
    symbols::{
        ArrayDimInfo, ProcedureKind, ScopeKind, Symbol, SymbolKind, SymbolTable,
        UserTypeDefinition, UserTypeMember,
    },
    typed_ir::*,
    types::{BasicType, from_type_spec, type_from_suffix},
};

/// Tracks loop nesting for EXIT validation.
#[derive(Debug, Clone, Default)]
struct LoopContext {
    /// Depth of FOR loops.
    for_depth: usize,
    /// Depth of WHILE loops.
    while_depth: usize,
    /// Depth of DO loops.
    do_depth: usize,
}

/// Bundles FOR loop components for type checking.
///
/// This struct groups the related parts of a FOR statement to reduce
/// the number of parameters passed to `check_for`.
struct ForLoopInfo<'a> {
    /// Loop counter variable name.
    variable: &'a str,
    /// Start value expression.
    start: &'a Expr,
    /// End value expression.
    end: &'a Expr,
    /// Optional step value expression.
    step: &'a Option<Expr>,
    /// Loop body statements.
    body: &'a [Statement],
    /// Optional variable name after NEXT (for validation).
    next_variable: &'a Option<String>,
    /// Source span for error reporting.
    span: Span,
}

/// The type checker validates and annotates the AST with types.
pub struct TypeChecker<'a> {
    /// Reference to the symbol table for lookups and definitions.
    symbols: &'a mut SymbolTable,
    /// Accumulated errors.
    pub errors: Vec<SemanticError>,
    /// Current loop nesting context.
    loop_context: LoopContext,
    /// Whether we're inside a FUNCTION (for EXIT FUNCTION / return checking).
    in_function: bool,
    /// Whether we're inside a SUB (for EXIT SUB).
    in_sub: bool,
    /// Current function name (for assigning return value).
    current_function_name: Option<String>,
}

impl<'a> TypeChecker<'a> {
    /// Creates a new type checker.
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            errors: Vec::new(),
            loop_context: LoopContext::default(),
            in_function: false,
            in_sub: false,
            current_function_name: None,
        }
    }

    // ========================================================================
    // Expression Type Checking
    // ========================================================================

    /// Type checks an expression and returns the typed expression.
    ///
    /// On error, records the error and returns a placeholder expression.
    pub fn check_expr(&mut self, expr: &Expr) -> TypedExpr {
        match &expr.kind {
            ExprKind::IntegerLiteral(val) => TypedExpr::integer(*val, expr.span),

            ExprKind::FloatLiteral(val) => TypedExpr::float(*val, expr.span),

            ExprKind::StringLiteral(val) => TypedExpr::string(val.clone(), expr.span),

            ExprKind::Identifier(name) => self.check_identifier(name, expr.span),

            ExprKind::Binary { left, op, right } => self.check_binary(left, *op, right, expr.span),

            ExprKind::Unary { op, operand } => self.check_unary(*op, operand, expr.span),

            ExprKind::Grouped(inner) => {
                let typed_inner = self.check_expr(inner);
                TypedExpr::new(
                    TypedExprKind::Grouped(Box::new(typed_inner.clone())),
                    typed_inner.basic_type,
                    expr.span,
                )
            }

            ExprKind::FunctionCall { name, args } => {
                self.check_function_call(name, args, expr.span)
            }

            ExprKind::FieldAccess { object, field } => {
                self.check_field_access(object, field, expr.span)
            }
        }
    }

    /// Type checks an identifier (variable reference).
    fn check_identifier(&mut self, name: &str, span: crate::ast::Span) -> TypedExpr {
        // Check if it's an existing variable
        if let Some(symbol) = self.symbols.lookup_symbol(name) {
            return TypedExpr::new(
                TypedExprKind::Variable(name.to_string()),
                symbol.basic_type.clone(),
                span,
            );
        }

        // Check if it's a parameterless function call
        if let Some(proc) = self.symbols.lookup_procedure(name)
            && proc.params.is_empty()
            && proc.return_type.is_some()
        {
            return TypedExpr::new(
                TypedExprKind::FunctionCall {
                    name: name.to_string(),
                    args: vec![],
                },
                proc.return_type.clone().unwrap(),
                span,
            );
        }

        // Implicit variable declaration (BASIC allows undeclared variables)
        let basic_type =
            type_from_suffix(name).unwrap_or_else(|| self.symbols.default_type_for(name));

        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Variable,
            basic_type: basic_type.clone(),
            span,
            is_mutable: true,
        };

        let _ = self.symbols.define_symbol(symbol);

        TypedExpr::new(TypedExprKind::Variable(name.to_string()), basic_type, span)
    }

    /// Type checks a field access expression (e.g., `person.name`).
    ///
    /// This validates that:
    /// - The object expression has a UserDefined type
    /// - The field exists in that type (when TYPE members are tracked)
    fn check_field_access(
        &mut self,
        object: &Expr,
        field: &str,
        span: crate::ast::Span,
    ) -> TypedExpr {
        let typed_object = self.check_expr(object);

        // Determine the field type based on the object's type
        let field_type = match &typed_object.basic_type {
            BasicType::UserDefined(type_name) => {
                // Look up the type definition to find the field's type
                // For now, we'll use a placeholder lookup - Phase 3 will add proper type member tracking
                if let Some(field_type) = self.lookup_type_field(type_name, field) {
                    field_type
                } else {
                    self.errors.push(SemanticError::UndefinedVariable {
                        name: format!("{}.{}", type_name, field),
                        span,
                    });
                    BasicType::Unknown
                }
            }
            _ => {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "user-defined type".to_string(),
                    found: typed_object.basic_type.to_string(),
                    span,
                });
                BasicType::Unknown
            }
        };

        TypedExpr::new(
            TypedExprKind::FieldAccess {
                object: Box::new(typed_object),
                field: field.to_string(),
            },
            field_type,
            span,
        )
    }

    /// Looks up a field's type in a user-defined TYPE.
    ///
    /// Returns None if the type or field doesn't exist.
    fn lookup_type_field(&self, type_name: &str, field: &str) -> Option<BasicType> {
        self.symbols.lookup_type_member(type_name, field)
    }

    /// Type checks a binary operation.
    fn check_binary(
        &mut self,
        left: &Expr,
        op: BinaryOp,
        right: &Expr,
        span: crate::ast::Span,
    ) -> TypedExpr {
        let left_typed = self.check_expr(left);
        let right_typed = self.check_expr(right);

        let result_type = match op {
            // Comparison operators always return Integer (boolean in BASIC)
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterEqual => {
                if !self.types_comparable(&left_typed.basic_type, &right_typed.basic_type) {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                BasicType::Integer
            }

            // Logical operators (bitwise on integers)
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::Eqv | BinaryOp::Imp => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                left_typed
                    .basic_type
                    .common_type(&right_typed.basic_type)
                    .unwrap_or(BasicType::Long)
            }

            // String concatenation with +
            BinaryOp::Add
                if left_typed.basic_type.is_string() && right_typed.basic_type.is_string() =>
            {
                BasicType::String
            }

            // Arithmetic operators
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                    BasicType::Single
                } else {
                    left_typed
                        .basic_type
                        .common_type(&right_typed.basic_type)
                        .unwrap_or(BasicType::Single)
                }
            }

            // Division and Power return at least Single
            BinaryOp::Divide | BinaryOp::Power => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                    BasicType::Single
                } else {
                    let common = left_typed
                        .basic_type
                        .common_type(&right_typed.basic_type)
                        .unwrap_or(BasicType::Single);
                    if common.is_integer() {
                        BasicType::Single
                    } else {
                        common
                    }
                }
            }

            // Integer division and modulo
            BinaryOp::IntDivide | BinaryOp::Modulo => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                BasicType::Long
            }
        };

        TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(left_typed),
                op,
                right: Box::new(right_typed),
            },
            result_type,
            span,
        )
    }

    /// Type checks a unary operation.
    fn check_unary(&mut self, op: UnaryOp, operand: &Expr, span: crate::ast::Span) -> TypedExpr {
        let operand_typed = self.check_expr(operand);

        match op {
            UnaryOp::Negate => {
                if !operand_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidUnaryOp {
                        op: "-".to_string(),
                        operand_type: operand_typed.basic_type.to_string(),
                        span,
                    });
                }
            }
            UnaryOp::Not => {
                if !operand_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidUnaryOp {
                        op: "NOT".to_string(),
                        operand_type: operand_typed.basic_type.to_string(),
                        span,
                    });
                }
            }
        }

        TypedExpr::new(
            TypedExprKind::Unary {
                op,
                operand: Box::new(operand_typed.clone()),
            },
            operand_typed.basic_type,
            span,
        )
    }

    /// Type checks a function call or array access.
    fn check_function_call(
        &mut self,
        name: &str,
        args: &[Expr],
        span: crate::ast::Span,
    ) -> TypedExpr {
        // First check if it's an array access
        if let Some(symbol) = self.symbols.lookup_symbol(name)
            && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
        {
            return self.check_array_access(
                name,
                args,
                dimensions.clone(),
                symbol.basic_type.clone(),
                span,
            );
        }

        // Look up procedure
        let proc = match self.symbols.lookup_procedure(name) {
            Some(p) => p.clone(),
            None => {
                self.errors.push(SemanticError::UndefinedProcedure {
                    name: name.to_string(),
                    span,
                });
                // Return Unknown type to continue checking
                return TypedExpr::new(
                    TypedExprKind::FunctionCall {
                        name: name.to_string(),
                        args: args.iter().map(|a| self.check_expr(a)).collect(),
                    },
                    BasicType::Unknown,
                    span,
                );
            }
        };

        // SUBs don't return values
        if proc.kind == ProcedureKind::Sub {
            self.errors.push(SemanticError::SubUsedAsFunction {
                name: name.to_string(),
                span,
            });
            return TypedExpr::new(
                TypedExprKind::FunctionCall {
                    name: name.to_string(),
                    args: args.iter().map(|a| self.check_expr(a)).collect(),
                },
                BasicType::Void,
                span,
            );
        }

        // Check argument count
        if args.len() != proc.params.len() {
            self.errors.push(SemanticError::ArgumentCountMismatch {
                name: name.to_string(),
                expected: proc.params.len(),
                found: args.len(),
                span,
            });
        }

        // Check argument types
        let mut typed_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let typed_arg = self.check_expr(arg);

            if i < proc.params.len() {
                let param = &proc.params[i];
                if !typed_arg.basic_type.is_convertible_to(&param.basic_type) {
                    self.errors.push(SemanticError::ArgumentTypeMismatch {
                        position: i + 1,
                        expected: param.basic_type.to_string(),
                        found: typed_arg.basic_type.to_string(),
                        span: arg.span,
                    });
                }
            }

            typed_args.push(typed_arg);
        }

        TypedExpr::new(
            TypedExprKind::FunctionCall {
                name: name.to_string(),
                args: typed_args,
            },
            proc.return_type.unwrap_or(BasicType::Void),
            span,
        )
    }

    /// Type checks an array access.
    fn check_array_access(
        &mut self,
        name: &str,
        indices: &[Expr],
        dim_info: Vec<ArrayDimInfo>,
        element_type: BasicType,
        span: crate::ast::Span,
    ) -> TypedExpr {
        if indices.len() != dim_info.len() {
            self.errors.push(SemanticError::ArrayDimensionMismatch {
                name: name.to_string(),
                expected: dim_info.len(),
                found: indices.len(),
                span,
            });
        }

        let mut typed_indices = Vec::new();
        for index in indices {
            let typed_index = self.check_expr(index);
            if !typed_index.basic_type.is_numeric() {
                self.errors.push(SemanticError::NonNumericIndex {
                    found: typed_index.basic_type.to_string(),
                    span: index.span,
                });
            }
            typed_indices.push(typed_index);
        }

        // Convert ArrayDimInfo to TypedArrayDimension for code generation
        let typed_dimensions: Vec<TypedArrayDimension> = dim_info
            .iter()
            .map(|d| TypedArrayDimension {
                lower: d.lower_bound,
                upper: d.upper_bound,
            })
            .collect();

        TypedExpr::new(
            TypedExprKind::ArrayAccess {
                name: name.to_string(),
                indices: typed_indices,
                dimensions: typed_dimensions,
            },
            element_type,
            span,
        )
    }

    /// Checks if two types are comparable (for comparison operators).
    fn types_comparable(&self, a: &BasicType, b: &BasicType) -> bool {
        (a.is_numeric() && b.is_numeric()) || (a.is_string() && b.is_string())
    }

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

    /// Type checks an assignment statement.
    fn check_assignment(
        &mut self,
        name: &str,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Check if assigning to a constant
        if let Some(symbol) = self.symbols.lookup_symbol(name)
            && !symbol.is_mutable
        {
            self.errors.push(SemanticError::AssignmentToConst {
                name: name.to_string(),
                span,
            });
        }

        let typed_value = self.check_expr(value);

        // Determine target type
        let target_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
            symbol.basic_type.clone()
        } else {
            // New variable, infer from suffix or default
            let inferred =
                type_from_suffix(name).unwrap_or_else(|| self.symbols.default_type_for(name));

            // Define the new variable
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Variable,
                basic_type: inferred.clone(),
                span,
                is_mutable: true,
            };
            let _ = self.symbols.define_symbol(symbol);

            inferred
        };

        // Check type compatibility
        if !typed_value.basic_type.is_convertible_to(&target_type) {
            self.errors.push(SemanticError::TypeMismatch {
                expected: target_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Assignment {
                name: name.to_string(),
                value: typed_value,
                target_type,
            },
            span,
        )
    }

    /// Type checks an array element assignment.
    fn check_array_assignment(
        &mut self,
        name: &str,
        indices: &[Expr],
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Look up the array
        let (element_type, dimensions) = if let Some(symbol) = self.symbols.lookup_symbol(name)
            && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
        {
            // Verify dimension count
            if indices.len() != dimensions.len() {
                self.errors.push(SemanticError::ArrayDimensionMismatch {
                    name: name.to_string(),
                    expected: dimensions.len(),
                    found: indices.len(),
                    span,
                });
            }

            let typed_dims: Vec<TypedArrayDimension> = dimensions
                .iter()
                .map(|d| TypedArrayDimension {
                    lower: d.lower_bound,
                    upper: d.upper_bound,
                })
                .collect();

            (symbol.basic_type.clone(), typed_dims)
        } else {
            self.errors.push(SemanticError::NotAnArray {
                name: name.to_string(),
                span,
            });
            (BasicType::Unknown, Vec::new())
        };

        // Check and type the indices
        let mut typed_indices = Vec::new();
        for idx in indices {
            let typed_idx = self.check_expr(idx);
            if !typed_idx.basic_type.is_numeric() {
                self.errors.push(SemanticError::NonNumericIndex {
                    found: typed_idx.basic_type.to_string(),
                    span: idx.span,
                });
            }
            typed_indices.push(typed_idx);
        }

        // Check the value
        let typed_value = self.check_expr(value);

        // Type compatibility check
        if !typed_value.basic_type.is_convertible_to(&element_type) {
            self.errors.push(SemanticError::TypeMismatch {
                expected: element_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::ArrayAssignment {
                name: name.to_string(),
                indices: typed_indices,
                value: typed_value,
                dimensions,
                element_type,
            },
            span,
        )
    }

    /// Type checks a PRINT statement.
    fn check_print(
        &mut self,
        values: &[PrintItem],
        newline: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_items: Vec<TypedPrintItem> = values
            .iter()
            .map(|item| TypedPrintItem {
                expr: self.check_expr(&item.expr),
                separator: item.separator,
            })
            .collect();

        TypedStatement::new(
            TypedStatementKind::Print {
                items: typed_items,
                newline,
            },
            span,
        )
    }

    /// Type checks an INPUT statement.
    fn check_input(
        &mut self,
        prompt: &Option<String>,
        show_question_mark: bool,
        variables: &[String],
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_vars: Vec<(String, BasicType)> = variables
            .iter()
            .map(|name| {
                let basic_type = if let Some(sym) = self.symbols.lookup_symbol(name) {
                    sym.basic_type.clone()
                } else {
                    // Implicitly declare
                    let t = type_from_suffix(name)
                        .unwrap_or_else(|| self.symbols.default_type_for(name));
                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: t.clone(),
                        span,
                        is_mutable: true,
                    };
                    let _ = self.symbols.define_symbol(symbol);
                    t
                };
                (name.clone(), basic_type)
            })
            .collect();

        TypedStatement::new(
            TypedStatementKind::Input {
                prompt: prompt.clone(),
                show_question_mark,
                variables: typed_vars,
            },
            span,
        )
    }

    /// Type checks a LINE INPUT statement.
    fn check_line_input(
        &mut self,
        prompt: &Option<String>,
        variable: &str,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // LINE INPUT always reads into a string
        if self.symbols.lookup_symbol(variable).is_none() {
            let symbol = Symbol {
                name: variable.to_string(),
                kind: SymbolKind::Variable,
                basic_type: BasicType::String,
                span,
                is_mutable: true,
            };
            let _ = self.symbols.define_symbol(symbol);
        }

        TypedStatement::new(
            TypedStatementKind::LineInput {
                prompt: prompt.clone(),
                variable: variable.to_string(),
            },
            span,
        )
    }

    /// Type checks an IF statement.
    fn check_if(
        &mut self,
        condition: &Expr,
        then_branch: &[Statement],
        elseif_branches: &[(Expr, Vec<Statement>)],
        else_branch: &Option<Vec<Statement>>,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_condition = self.check_expr(condition);

        if !typed_condition.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_condition.basic_type.to_string(),
                span: condition.span,
            });
        }

        let typed_then = self.check_statements(then_branch);

        let typed_elseifs: Vec<(TypedExpr, Vec<TypedStatement>)> = elseif_branches
            .iter()
            .map(|(cond, stmts)| {
                let tc = self.check_expr(cond);
                if !tc.basic_type.is_numeric() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "numeric".to_string(),
                        found: tc.basic_type.to_string(),
                        span: cond.span,
                    });
                }
                (tc, self.check_statements(stmts))
            })
            .collect();

        let typed_else = else_branch
            .as_ref()
            .map(|stmts| self.check_statements(stmts));

        TypedStatement::new(
            TypedStatementKind::If {
                condition: typed_condition,
                then_branch: typed_then,
                elseif_branches: typed_elseifs,
                else_branch: typed_else,
            },
            span,
        )
    }

    /// Type checks a SELECT CASE statement.
    fn check_select_case(
        &mut self,
        test_expr: &Expr,
        cases: &[crate::ast::CaseClause],
        case_else: &Option<Vec<Statement>>,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_test = self.check_expr(test_expr);

        let typed_cases: Vec<TypedCaseClause> = cases
            .iter()
            .map(|case| TypedCaseClause {
                matches: case
                    .matches
                    .iter()
                    .map(|m| self.check_case_match(m, &typed_test.basic_type))
                    .collect(),
                body: self.check_statements(&case.body),
            })
            .collect();

        let typed_else = case_else.as_ref().map(|stmts| self.check_statements(stmts));

        TypedStatement::new(
            TypedStatementKind::SelectCase {
                test_expr: typed_test,
                cases: typed_cases,
                case_else: typed_else,
            },
            span,
        )
    }

    /// Type checks a CASE match.
    fn check_case_match(&mut self, m: &CaseMatch, test_type: &BasicType) -> TypedCaseMatch {
        match m {
            CaseMatch::Single(expr) => {
                let typed = self.check_expr(expr);
                if !self.types_comparable(&typed.basic_type, test_type) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: test_type.to_string(),
                        found: typed.basic_type.to_string(),
                        span: expr.span,
                    });
                }
                TypedCaseMatch::Single(typed)
            }
            CaseMatch::Range { from, to } => {
                let typed_from = self.check_expr(from);
                let typed_to = self.check_expr(to);
                TypedCaseMatch::Range {
                    from: typed_from,
                    to: typed_to,
                }
            }
            CaseMatch::Comparison { op, value } => {
                let typed_value = self.check_expr(value);
                TypedCaseMatch::Comparison {
                    op: match op {
                        CaseCompareOp::Equal => TypedCaseCompareOp::Equal,
                        CaseCompareOp::NotEqual => TypedCaseCompareOp::NotEqual,
                        CaseCompareOp::LessThan => TypedCaseCompareOp::LessThan,
                        CaseCompareOp::LessEqual => TypedCaseCompareOp::LessEqual,
                        CaseCompareOp::GreaterThan => TypedCaseCompareOp::GreaterThan,
                        CaseCompareOp::GreaterEqual => TypedCaseCompareOp::GreaterEqual,
                    },
                    value: typed_value,
                }
            }
        }
    }

    /// Type checks a FOR loop.
    fn check_for(&mut self, info: ForLoopInfo<'_>) -> TypedStatement {
        let ForLoopInfo {
            variable,
            start,
            end,
            step,
            body,
            next_variable,
            span,
        } = info;

        // Validate NEXT variable matches FOR variable (case-insensitive)
        if let Some(next_var) = next_variable
            && !next_var.eq_ignore_ascii_case(variable)
        {
            self.errors.push(SemanticError::ForNextMismatch {
                expected: variable.to_string(),
                found: next_var.to_string(),
                for_span: span,
                next_span: span, // Ideally we'd have separate spans
            });
        }

        let typed_start = self.check_expr(start);
        let typed_end = self.check_expr(end);

        if !typed_start.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_start.basic_type.to_string(),
                span: start.span,
            });
        }

        if !typed_end.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_end.basic_type.to_string(),
                span: end.span,
            });
        }

        let typed_step = step.as_ref().map(|s| {
            let ts = self.check_expr(s);
            if !ts.basic_type.is_numeric() {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: ts.basic_type.to_string(),
                    span: s.span,
                });
            }
            ts
        });

        // Determine loop variable type
        let var_type = if let Some(sym) = self.symbols.lookup_symbol(variable) {
            sym.basic_type.clone()
        } else {
            let t = typed_start
                .basic_type
                .common_type(&typed_end.basic_type)
                .unwrap_or(BasicType::Single);
            let symbol = Symbol {
                name: variable.to_string(),
                kind: SymbolKind::Variable,
                basic_type: t.clone(),
                span,
                is_mutable: true,
            };
            let _ = self.symbols.define_symbol(symbol);
            t
        };

        // Check body with increased FOR depth
        self.loop_context.for_depth += 1;
        let typed_body = self.check_statements(body);
        self.loop_context.for_depth -= 1;

        TypedStatement::new(
            TypedStatementKind::For {
                variable: variable.to_string(),
                var_type,
                start: typed_start,
                end: typed_end,
                step: typed_step,
                body: typed_body,
            },
            span,
        )
    }

    /// Type checks a WHILE loop.
    fn check_while(
        &mut self,
        condition: &Expr,
        body: &[Statement],
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_condition = self.check_expr(condition);

        if !typed_condition.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_condition.basic_type.to_string(),
                span: condition.span,
            });
        }

        self.loop_context.while_depth += 1;
        let typed_body = self.check_statements(body);
        self.loop_context.while_depth -= 1;

        TypedStatement::new(
            TypedStatementKind::While {
                condition: typed_condition,
                body: typed_body,
            },
            span,
        )
    }

    /// Type checks a DO loop.
    fn check_do_loop(
        &mut self,
        pre_condition: &Option<DoCondition>,
        body: &[Statement],
        post_condition: &Option<DoCondition>,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_pre = pre_condition.as_ref().map(|c| {
            let tc = self.check_expr(&c.condition);
            TypedDoCondition {
                is_while: c.is_while,
                condition: tc,
            }
        });

        self.loop_context.do_depth += 1;
        let typed_body = self.check_statements(body);
        self.loop_context.do_depth -= 1;

        let typed_post = post_condition.as_ref().map(|c| {
            let tc = self.check_expr(&c.condition);
            TypedDoCondition {
                is_while: c.is_while,
                condition: tc,
            }
        });

        TypedStatement::new(
            TypedStatementKind::DoLoop {
                pre_condition: typed_pre,
                body: typed_body,
                post_condition: typed_post,
            },
            span,
        )
    }

    /// Type checks a GOTO statement.
    fn check_goto(&mut self, target: &str, span: crate::ast::Span) -> TypedStatement {
        if self.symbols.lookup_label(target).is_none() {
            self.errors.push(SemanticError::UndefinedLabel {
                name: target.to_string(),
                span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Goto {
                target: target.to_string(),
            },
            span,
        )
    }

    /// Type checks a GOSUB statement.
    fn check_gosub(&mut self, target: &str, span: crate::ast::Span) -> TypedStatement {
        if self.symbols.lookup_label(target).is_none() {
            self.errors.push(SemanticError::UndefinedLabel {
                name: target.to_string(),
                span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Gosub {
                target: target.to_string(),
            },
            span,
        )
    }

    /// Type checks a RETURN statement.
    fn check_return(&mut self, span: crate::ast::Span) -> TypedStatement {
        // RETURN is valid after GOSUB or in a FUNCTION
        // We can't fully validate GOSUB context at compile time
        TypedStatement::new(TypedStatementKind::Return, span)
    }

    /// Type checks an EXIT statement.
    fn check_exit(
        &mut self,
        exit_type: crate::ast::ExitType,
        span: crate::ast::Span,
    ) -> TypedStatement {
        use crate::ast::ExitType;

        let valid = match exit_type {
            ExitType::For => self.loop_context.for_depth > 0,
            ExitType::While => self.loop_context.while_depth > 0,
            ExitType::Do => self.loop_context.do_depth > 0,
            ExitType::Sub => self.in_sub,
            ExitType::Function => self.in_function,
        };

        if !valid {
            let exit_name = match exit_type {
                ExitType::For => "FOR",
                ExitType::While => "WHILE",
                ExitType::Do => "DO",
                ExitType::Sub => "SUB",
                ExitType::Function => "FUNCTION",
            };
            self.errors.push(SemanticError::ExitOutsideLoop {
                exit_type: exit_name.to_string(),
                span,
            });
        }

        TypedStatement::new(TypedStatementKind::Exit { exit_type }, span)
    }

    /// Type checks a CALL statement.
    fn check_call(&mut self, name: &str, args: &[Expr], span: crate::ast::Span) -> TypedStatement {
        let typed_args: Vec<TypedExpr> = args.iter().map(|a| self.check_expr(a)).collect();

        if let Some(proc) = self.symbols.lookup_procedure(name) {
            // Check argument count
            if args.len() != proc.params.len() {
                self.errors.push(SemanticError::ArgumentCountMismatch {
                    name: name.to_string(),
                    expected: proc.params.len(),
                    found: args.len(),
                    span,
                });
            }

            // Check argument types
            for (i, (arg, typed_arg)) in args.iter().zip(&typed_args).enumerate() {
                if i < proc.params.len() {
                    let param = &proc.params[i];
                    if !typed_arg.basic_type.is_convertible_to(&param.basic_type) {
                        self.errors.push(SemanticError::ArgumentTypeMismatch {
                            position: i + 1,
                            expected: param.basic_type.to_string(),
                            found: typed_arg.basic_type.to_string(),
                            span: arg.span,
                        });
                    }
                }
            }
        } else {
            self.errors.push(SemanticError::UndefinedProcedure {
                name: name.to_string(),
                span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Call {
                name: name.to_string(),
                args: typed_args,
            },
            span,
        )
    }

    /// Type checks a DIM statement.
    fn check_dim(
        &mut self,
        name: &str,
        dimensions: &[crate::ast::ArrayDimension],
        type_spec: &Option<crate::ast::TypeSpec>,
        shared: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Determine type
        let basic_type = type_spec
            .as_ref()
            .map(from_type_spec)
            .or_else(|| type_from_suffix(name))
            .unwrap_or_else(|| self.symbols.default_type_for(name));

        // Handle SHARED
        if shared {
            if !self.symbols.in_procedure() {
                self.errors
                    .push(SemanticError::SharedOutsideProcedure { span });
            } else {
                self.symbols.add_shared_var(name.to_string());
            }
        }

        // Evaluate array dimensions - bounds must be constant expressions
        let typed_dims: Vec<TypedArrayDimension> = dimensions
            .iter()
            .map(|d| {
                // Evaluate lower bound (if provided)
                let lower = if let Some(lower_expr) = &d.lower {
                    let typed_lower = self.check_expr(lower_expr);
                    match self.try_evaluate_const_expr(&typed_lower) {
                        Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                        Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                        _ => {
                            self.errors.push(SemanticError::NonConstantExpression {
                                span: lower_expr.span,
                            });
                            0 // Default on error
                        }
                    }
                } else {
                    self.symbols.option_base()
                };

                // Evaluate upper bound (required)
                let typed_upper = self.check_expr(&d.upper);
                let upper = match self.try_evaluate_const_expr(&typed_upper) {
                    Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                    Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                    _ => {
                        self.errors
                            .push(SemanticError::NonConstantExpression { span: d.upper.span });
                        10 // Default on error
                    }
                };

                TypedArrayDimension { lower, upper }
            })
            .collect();

        // Define symbol
        let symbol_kind = if dimensions.is_empty() {
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
            name: name.to_string(),
            kind: symbol_kind,
            basic_type: basic_type.clone(),
            span,
            is_mutable: true,
        };

        if let Err(err) = self.symbols.define_symbol(symbol) {
            let (existing, _) = *err;
            self.errors.push(SemanticError::DuplicateVariable {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Dim {
                name: name.to_string(),
                basic_type,
                dimensions: typed_dims,
                shared,
            },
            span,
        )
    }

    /// Type checks a CONST statement.
    fn check_const(&mut self, name: &str, value: &Expr, span: crate::ast::Span) -> TypedStatement {
        let typed_value = self.check_expr(value);
        let basic_type = typed_value.basic_type.clone();

        // Try to evaluate the expression as a compile-time constant
        let const_value = match self.try_evaluate_const_expr(&typed_value) {
            Some(cv) => cv,
            None => {
                self.errors
                    .push(SemanticError::NonConstantExpression { span: value.span });
                crate::semantic::symbols::ConstValue::Integer(0)
            }
        };

        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Constant { value: const_value },
            basic_type: basic_type.clone(),
            span,
            is_mutable: false,
        };

        if let Err(err) = self.symbols.define_symbol(symbol) {
            let (existing, _) = *err;
            self.errors.push(SemanticError::DuplicateVariable {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Const {
                name: name.to_string(),
                value: typed_value,
                basic_type,
            },
            span,
        )
    }

    /// Attempts to evaluate a typed expression as a compile-time constant.
    ///
    /// Returns `Some(ConstValue)` if the expression can be evaluated at compile time,
    /// `None` if it contains non-constant elements (like variable references).
    fn try_evaluate_const_expr(
        &self,
        expr: &TypedExpr,
    ) -> Option<crate::semantic::symbols::ConstValue> {
        use crate::semantic::symbols::ConstValue;

        match &expr.kind {
            // Direct literals
            TypedExprKind::IntegerLiteral(v) => Some(ConstValue::Integer(*v)),
            TypedExprKind::FloatLiteral(v) => Some(ConstValue::Float(*v)),
            TypedExprKind::StringLiteral(v) => Some(ConstValue::String(v.clone())),

            // Grouped expressions - just unwrap
            TypedExprKind::Grouped(inner) => self.try_evaluate_const_expr(inner),

            // Binary operations on constants
            TypedExprKind::Binary { left, op, right } => {
                let left_val = self.try_evaluate_const_expr(left)?;
                let right_val = self.try_evaluate_const_expr(right)?;
                self.evaluate_binary_const(*op, left_val, right_val)
            }

            // Unary operations on constants
            TypedExprKind::Unary { op, operand } => {
                let operand_val = self.try_evaluate_const_expr(operand)?;
                self.evaluate_unary_const(*op, operand_val)
            }

            // Variable references - check if it's a constant
            TypedExprKind::Variable(name) => {
                if let Some(symbol) = self.symbols.lookup_symbol(name)
                    && let SymbolKind::Constant { value } = &symbol.kind
                {
                    return Some(value.clone());
                }
                None // Not a constant
            }

            // Type conversions - evaluate inner and convert
            TypedExprKind::Convert {
                expr: inner,
                to_type,
            } => {
                let inner_val = self.try_evaluate_const_expr(inner)?;
                self.convert_const_value(inner_val, to_type)
            }

            // Function calls, array access, and field access are not constant
            TypedExprKind::FunctionCall { .. }
            | TypedExprKind::ArrayAccess { .. }
            | TypedExprKind::FieldAccess { .. } => None,
        }
    }

    /// Evaluates a binary operation on constant values.
    fn evaluate_binary_const(
        &self,
        op: BinaryOp,
        left: crate::semantic::symbols::ConstValue,
        right: crate::semantic::symbols::ConstValue,
    ) -> Option<crate::semantic::symbols::ConstValue> {
        use crate::semantic::symbols::ConstValue;

        match (left, right) {
            // Integer operations
            (ConstValue::Integer(l), ConstValue::Integer(r)) => {
                let result = match op {
                    BinaryOp::Add => l.checked_add(r)?,
                    BinaryOp::Subtract => l.checked_sub(r)?,
                    BinaryOp::Multiply => l.checked_mul(r)?,
                    BinaryOp::Divide => l.checked_div(r)?,
                    BinaryOp::IntDivide => l.checked_div(r)?,
                    BinaryOp::Modulo => l.checked_rem(r)?,
                    BinaryOp::Power => l.checked_pow(r.try_into().ok()?)?,
                    BinaryOp::And => l & r,
                    BinaryOp::Or => l | r,
                    BinaryOp::Xor => l ^ r,
                    BinaryOp::Eqv => !(l ^ r),
                    BinaryOp::Imp => !l | r,
                    BinaryOp::Equal => {
                        if l == r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::NotEqual => {
                        if l != r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LessThan => {
                        if l < r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LessEqual => {
                        if l <= r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l > r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::GreaterEqual => {
                        if l >= r {
                            -1
                        } else {
                            0
                        }
                    }
                };
                Some(ConstValue::Integer(result))
            }

            // Float operations
            (ConstValue::Float(l), ConstValue::Float(r)) => {
                let result = match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Subtract => l - r,
                    BinaryOp::Multiply => l * r,
                    BinaryOp::Divide => l / r,
                    BinaryOp::Power => l.powf(r),
                    BinaryOp::Equal => {
                        if l == r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::NotEqual => {
                        if l != r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::LessThan => {
                        if l < r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::LessEqual => {
                        if l <= r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l > r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::GreaterEqual => {
                        if l >= r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    _ => return None, // Bitwise ops not valid on floats
                };
                Some(ConstValue::Float(result))
            }

            // Mixed int/float - promote to float
            (ConstValue::Integer(l), ConstValue::Float(r)) => {
                self.evaluate_binary_const(op, ConstValue::Float(l as f64), ConstValue::Float(r))
            }
            (ConstValue::Float(l), ConstValue::Integer(r)) => {
                self.evaluate_binary_const(op, ConstValue::Float(l), ConstValue::Float(r as f64))
            }

            // String concatenation
            (ConstValue::String(l), ConstValue::String(r)) => match op {
                BinaryOp::Add => Some(ConstValue::String(l + &r)),
                BinaryOp::Equal => Some(ConstValue::Integer(if l == r { -1 } else { 0 })),
                BinaryOp::NotEqual => Some(ConstValue::Integer(if l != r { -1 } else { 0 })),
                BinaryOp::LessThan => Some(ConstValue::Integer(if l < r { -1 } else { 0 })),
                BinaryOp::LessEqual => Some(ConstValue::Integer(if l <= r { -1 } else { 0 })),
                BinaryOp::GreaterThan => Some(ConstValue::Integer(if l > r { -1 } else { 0 })),
                BinaryOp::GreaterEqual => Some(ConstValue::Integer(if l >= r { -1 } else { 0 })),
                _ => None,
            },

            // String + non-string not allowed
            _ => None,
        }
    }

    /// Evaluates a unary operation on a constant value.
    fn evaluate_unary_const(
        &self,
        op: UnaryOp,
        operand: crate::semantic::symbols::ConstValue,
    ) -> Option<crate::semantic::symbols::ConstValue> {
        use crate::semantic::symbols::ConstValue;

        match (op, operand) {
            (UnaryOp::Negate, ConstValue::Integer(v)) => Some(ConstValue::Integer(-v)),
            (UnaryOp::Negate, ConstValue::Float(v)) => Some(ConstValue::Float(-v)),
            (UnaryOp::Not, ConstValue::Integer(v)) => Some(ConstValue::Integer(!v)),
            _ => None,
        }
    }

    /// Converts a constant value to a different type.
    fn convert_const_value(
        &self,
        value: crate::semantic::symbols::ConstValue,
        to_type: &BasicType,
    ) -> Option<crate::semantic::symbols::ConstValue> {
        use crate::semantic::symbols::ConstValue;

        match (value, to_type) {
            // Integer to float
            (ConstValue::Integer(v), BasicType::Single | BasicType::Double) => {
                Some(ConstValue::Float(v as f64))
            }
            // Float to integer (truncate)
            (ConstValue::Float(v), BasicType::Integer | BasicType::Long) => {
                Some(ConstValue::Integer(v as i64))
            }
            // Same type - no conversion needed
            (v @ ConstValue::Integer(_), BasicType::Integer | BasicType::Long) => Some(v),
            (v @ ConstValue::Float(_), BasicType::Single | BasicType::Double) => Some(v),
            (v @ ConstValue::String(_), BasicType::String) => Some(v),
            // Unsigned types
            (ConstValue::Integer(v), BasicType::UnsignedInteger | BasicType::UnsignedLong) => {
                Some(ConstValue::Integer(v))
            }
            _ => None,
        }
    }

    /// Type checks a SUB definition.
    fn check_sub_definition(
        &mut self,
        name: &str,
        params: &[crate::ast::Parameter],
        body: &[Statement],
        is_static: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Enter SUB scope
        self.symbols.enter_scope(ScopeKind::Sub);
        self.in_sub = true;

        // Define parameters in local scope
        let typed_params: Vec<TypedParameter> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                let symbol = Symbol {
                    name: p.name.clone(),
                    kind: SymbolKind::Parameter { by_val: p.by_val },
                    basic_type: basic_type.clone(),
                    span,
                    is_mutable: true,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedParameter {
                    name: p.name.clone(),
                    basic_type,
                    by_val: p.by_val,
                }
            })
            .collect();

        // Check body
        let typed_body = self.check_statements(body);

        // Exit scope
        self.in_sub = false;
        self.symbols.exit_scope();

        TypedStatement::new(
            TypedStatementKind::SubDefinition {
                name: name.to_string(),
                params: typed_params,
                body: typed_body,
                is_static,
            },
            span,
        )
    }

    /// Type checks a FUNCTION definition.
    fn check_function_definition(
        &mut self,
        name: &str,
        params: &[crate::ast::Parameter],
        return_type: &Option<crate::ast::TypeSpec>,
        body: &[Statement],
        is_static: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Determine return type
        let ret_type = return_type
            .as_ref()
            .map(from_type_spec)
            .or_else(|| type_from_suffix(name))
            .unwrap_or_else(|| self.symbols.default_type_for(name));

        // Enter FUNCTION scope
        self.symbols.enter_scope(ScopeKind::Function);
        self.in_function = true;
        self.current_function_name = Some(name.to_string());

        // Define function name as local variable for return value
        let return_var = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Variable,
            basic_type: ret_type.clone(),
            span,
            is_mutable: true,
        };
        let _ = self.symbols.define_symbol(return_var);

        // Define parameters
        let typed_params: Vec<TypedParameter> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                let symbol = Symbol {
                    name: p.name.clone(),
                    kind: SymbolKind::Parameter { by_val: p.by_val },
                    basic_type: basic_type.clone(),
                    span,
                    is_mutable: true,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedParameter {
                    name: p.name.clone(),
                    basic_type,
                    by_val: p.by_val,
                }
            })
            .collect();

        // Check body
        let typed_body = self.check_statements(body);

        // Exit scope
        self.in_function = false;
        self.current_function_name = None;
        self.symbols.exit_scope();

        TypedStatement::new(
            TypedStatementKind::FunctionDefinition {
                name: name.to_string(),
                params: typed_params,
                return_type: ret_type,
                body: typed_body,
                is_static,
            },
            span,
        )
    }

    /// Type checks a list of statements.
    pub fn check_statements(&mut self, statements: &[Statement]) -> Vec<TypedStatement> {
        statements
            .iter()
            .map(|stmt| self.check_statement(stmt))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    fn make_int_expr(val: i64) -> Expr {
        Expr::new(ExprKind::IntegerLiteral(val), Span::new(0, 1))
    }

    fn make_str_expr(val: &str) -> Expr {
        Expr::new(ExprKind::StringLiteral(val.to_string()), Span::new(0, 1))
    }

    fn make_ident_expr(name: &str) -> Expr {
        Expr::new(ExprKind::Identifier(name.to_string()), Span::new(0, 1))
    }

    #[test]
    fn test_const_with_literal() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // CONST X = 42 should succeed
        let stmt = Statement::new(
            StatementKind::Const {
                name: "X".to_string(),
                value: make_int_expr(42),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CONST with literal should not error"
        );
    }

    #[test]
    fn test_const_with_variable_errors() {
        let mut symbols = SymbolTable::new();

        // First define a variable before creating the checker
        let _ = symbols.define_symbol(Symbol {
            name: "someVar".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Long,
            span: Span::new(0, 7),
            is_mutable: true,
        });

        let mut checker = TypeChecker::new(&mut symbols);

        // CONST X = someVar should error
        let stmt = Statement::new(
            StatementKind::Const {
                name: "X".to_string(),
                value: make_ident_expr("someVar"),
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            !checker.errors.is_empty(),
            "CONST with variable should error"
        );
        assert!(
            matches!(
                checker.errors[0],
                SemanticError::NonConstantExpression { .. }
            ),
            "Expected NonConstantExpression error"
        );
    }

    #[test]
    fn test_const_with_constant_expr() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // CONST X = 1 + 2 should succeed (constant expression)
        let stmt = Statement::new(
            StatementKind::Const {
                name: "X".to_string(),
                value: Expr::new(
                    ExprKind::Binary {
                        left: Box::new(make_int_expr(1)),
                        op: BinaryOp::Add,
                        right: Box::new(make_int_expr(2)),
                    },
                    Span::new(0, 5),
                ),
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CONST with constant expression should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_const_referencing_other_const() {
        let mut symbols = SymbolTable::new();

        // First define a constant A = 10
        let _ = symbols.define_symbol(Symbol {
            name: "A".to_string(),
            kind: SymbolKind::Constant {
                value: crate::semantic::symbols::ConstValue::Integer(10),
            },
            basic_type: BasicType::Long,
            span: Span::new(0, 10),
            is_mutable: false,
        });

        let mut checker = TypeChecker::new(&mut symbols);

        // CONST B = A should succeed (reference to another constant)
        let stmt = Statement::new(
            StatementKind::Const {
                name: "B".to_string(),
                value: make_ident_expr("A"),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "CONST referencing another CONST should not error: {:?}",
            checker.errors
        );
    }

    #[test]
    fn test_dim_with_constant_bounds() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(10) should succeed
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: None,
                    upper: make_int_expr(10),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 15),
        );

        let typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DIM with constant bounds should not error: {:?}",
            checker.errors
        );

        // Verify the dimensions were correctly evaluated
        if let TypedStatementKind::Dim { dimensions, .. } = &typed.kind {
            assert_eq!(dimensions.len(), 1);
            assert_eq!(dimensions[0].lower, 0); // Default OPTION BASE
            assert_eq!(dimensions[0].upper, 10);
        } else {
            panic!("Expected Dim statement");
        }
    }

    #[test]
    fn test_dim_with_expression_bounds() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(5 + 5) should succeed and evaluate to 10
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: None,
                    upper: Expr::new(
                        ExprKind::Binary {
                            left: Box::new(make_int_expr(5)),
                            op: BinaryOp::Add,
                            right: Box::new(make_int_expr(5)),
                        },
                        Span::new(0, 5),
                    ),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 20),
        );

        let typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DIM with constant expression bounds should not error: {:?}",
            checker.errors
        );

        // Verify the dimensions were correctly evaluated
        if let TypedStatementKind::Dim { dimensions, .. } = &typed.kind {
            assert_eq!(dimensions[0].upper, 10);
        } else {
            panic!("Expected Dim statement");
        }
    }

    #[test]
    fn test_dim_with_lower_and_upper_bounds() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(1 TO 10) should succeed
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: Some(make_int_expr(1)),
                    upper: make_int_expr(10),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 20),
        );

        let typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "DIM with lower TO upper should not error: {:?}",
            checker.errors
        );

        // Verify both bounds
        if let TypedStatementKind::Dim { dimensions, .. } = &typed.kind {
            assert_eq!(dimensions[0].lower, 1);
            assert_eq!(dimensions[0].upper, 10);
        } else {
            panic!("Expected Dim statement");
        }
    }

    #[test]
    fn test_dim_with_variable_bound_errors() {
        let mut symbols = SymbolTable::new();

        // Define a variable (not a constant)
        let _ = symbols.define_symbol(Symbol {
            name: "size".to_string(),
            kind: SymbolKind::Variable,
            basic_type: BasicType::Long,
            span: Span::new(0, 4),
            is_mutable: true,
        });

        let mut checker = TypeChecker::new(&mut symbols);

        // DIM arr(size) should error - variable bounds not allowed
        let stmt = Statement::new(
            StatementKind::Dim {
                name: "arr".to_string(),
                dimensions: vec![crate::ast::ArrayDimension {
                    lower: None,
                    upper: make_ident_expr("size"),
                }],
                type_spec: Some(crate::ast::TypeSpec::Integer),
                shared: false,
            },
            Span::new(0, 15),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            !checker.errors.is_empty(),
            "DIM with variable bound should error"
        );
        assert!(
            matches!(
                checker.errors[0],
                SemanticError::NonConstantExpression { .. }
            ),
            "Expected NonConstantExpression error"
        );
    }

    #[test]
    fn test_check_integer_literal() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = make_int_expr(42);
        let typed = checker.check_expr(&expr);

        assert_eq!(typed.basic_type, BasicType::Long);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_binary_add() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(make_int_expr(1)),
                op: BinaryOp::Add,
                right: Box::new(make_int_expr(2)),
            },
            Span::new(0, 5),
        );

        let typed = checker.check_expr(&expr);
        assert_eq!(typed.basic_type, BasicType::Long);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_string_concat() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(make_str_expr("hello")),
                op: BinaryOp::Add,
                right: Box::new(make_str_expr(" world")),
            },
            Span::new(0, 15),
        );

        let typed = checker.check_expr(&expr);
        assert_eq!(typed.basic_type, BasicType::String);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_invalid_binary_op() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // String + Integer should error
        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(make_str_expr("hello")),
                op: BinaryOp::Add,
                right: Box::new(make_int_expr(42)),
            },
            Span::new(0, 10),
        );

        let _typed = checker.check_expr(&expr);
        assert!(!checker.errors.is_empty());
        assert!(matches!(
            checker.errors[0],
            SemanticError::InvalidBinaryOp { .. }
        ));
    }

    #[test]
    fn test_implicit_variable() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // Reference to undeclared variable should implicitly declare it
        let expr = Expr::new(ExprKind::Identifier("x".to_string()), Span::new(0, 1));

        let typed = checker.check_expr(&expr);
        // Default type is SINGLE
        assert_eq!(typed.basic_type, BasicType::Single);
        assert!(checker.errors.is_empty());

        // Variable should now exist
        assert!(symbols.lookup_symbol("x").is_some());
    }

    #[test]
    fn test_variable_with_suffix() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        let expr = Expr::new(ExprKind::Identifier("name$".to_string()), Span::new(0, 5));

        let typed = checker.check_expr(&expr);
        assert_eq!(typed.basic_type, BasicType::String);
    }

    #[test]
    fn test_for_next_variable_mismatch() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // FOR i = 1 TO 10 ... NEXT j (should error - j != i)
        let stmt = Statement::new(
            StatementKind::For {
                variable: "i".to_string(),
                start: make_int_expr(1),
                end: make_int_expr(10),
                step: None,
                body: vec![],
                next_variable: Some("j".to_string()), // Mismatched!
            },
            Span::new(0, 20),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            !checker.errors.is_empty(),
            "FOR/NEXT variable mismatch should error"
        );
        assert!(
            matches!(checker.errors[0], SemanticError::ForNextMismatch { .. }),
            "Expected ForNextMismatch error"
        );
    }

    #[test]
    fn test_for_next_variable_match() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // FOR i = 1 TO 10 ... NEXT i (should be OK)
        let stmt = Statement::new(
            StatementKind::For {
                variable: "i".to_string(),
                start: make_int_expr(1),
                end: make_int_expr(10),
                step: None,
                body: vec![],
                next_variable: Some("i".to_string()), // Matches
            },
            Span::new(0, 20),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "FOR/NEXT with matching variable should not error"
        );
    }

    #[test]
    fn test_for_next_variable_case_insensitive() {
        let mut symbols = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symbols);

        // FOR i = 1 TO 10 ... NEXT I (should be OK - case insensitive)
        let stmt = Statement::new(
            StatementKind::For {
                variable: "counter".to_string(),
                start: make_int_expr(1),
                end: make_int_expr(10),
                step: None,
                body: vec![],
                next_variable: Some("COUNTER".to_string()), // Different case
            },
            Span::new(0, 20),
        );

        let _typed = checker.check_statement(&stmt);
        assert!(
            checker.errors.is_empty(),
            "FOR/NEXT with case-different variable should not error"
        );
    }
}
