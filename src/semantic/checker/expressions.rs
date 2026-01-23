//! Expression type checking.
//!
//! This module handles type checking for all expression types:
//! - Literals (integer, float, string)
//! - Identifiers (variable references)
//! - Binary and unary operations
//! - Function calls
//! - Array access
//! - Field access (for user-defined types)

use crate::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use crate::semantic::{
    error::SemanticError,
    symbols::{ArrayDimInfo, ProcedureKind, Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, type_from_suffix},
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
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

            ExprKind::ProcPtr { name } => {
                // _PROCPTR returns the address of a procedure as an _OFFSET
                // Look up the procedure to get its full name (including type suffix)
                if let Some(proc) = self.symbols.lookup_procedure(name) {
                    // Use the actual procedure name from the symbol table
                    // This includes any type suffix (e.g., "MyCompare&")
                    let actual_name = proc.name.clone();
                    // Generate wrapper function name for C callback
                    let wrapper_name = format!(
                        "qb_callback_{}",
                        actual_name
                            .to_lowercase()
                            .replace(['&', '%', '$', '!', '#'], "")
                    );
                    TypedExpr::new(
                        TypedExprKind::ProcPtr {
                            name: actual_name,
                            wrapper_name,
                        },
                        BasicType::Offset, // Function pointers are pointer-sized
                        expr.span,
                    )
                } else {
                    self.errors.push(SemanticError::UndefinedProcedure {
                        name: name.clone(),
                        span: expr.span,
                    });
                    // Return a placeholder expression on error
                    TypedExpr::new(
                        TypedExprKind::IntegerLiteral(0),
                        BasicType::Offset,
                        expr.span,
                    )
                }
            }

            ExprKind::CvFunc { target_type, value } => {
                let typed_value = self.check_expr(value);
                let basic_type = self.parse_type_name(target_type);
                TypedExpr::new(
                    TypedExprKind::CvFunc {
                        target_type: basic_type.clone(),
                        value: Box::new(typed_value),
                    },
                    basic_type,
                    expr.span,
                )
            }

            ExprKind::MkDollarFunc { source_type, value } => {
                let typed_value = self.check_expr(value);
                let basic_type = self.parse_type_name(source_type);
                // _MK$ always returns a String
                TypedExpr::new(
                    TypedExprKind::MkDollarFunc {
                        source_type: basic_type,
                        value: Box::new(typed_value),
                    },
                    BasicType::String,
                    expr.span,
                )
            }

            ExprKind::CastFunc { target_type, value } => {
                let typed_value = self.check_expr(value);
                let basic_type = self.parse_type_name(target_type);
                TypedExpr::new(
                    TypedExprKind::CastFunc {
                        target_type: basic_type.clone(),
                        value: Box::new(typed_value),
                    },
                    basic_type,
                    expr.span,
                )
            }

            ExprKind::ValWithType { value, target_type } => {
                let typed_value = self.check_expr(value);
                let basic_type = self.parse_type_name(target_type);
                // VAL with type spec returns the specified numeric type
                TypedExpr::new(
                    TypedExprKind::ValWithType {
                        value: Box::new(typed_value),
                        target_type: basic_type.clone(),
                    },
                    basic_type,
                    expr.span,
                )
            }
        }
    }

    /// Type checks an identifier (variable reference).
    ///
    /// This handles both regular identifiers and dotted identifiers.
    /// For dotted identifiers (e.g., `id.field`), we check if the prefix is a UDT variable.
    /// If so, we treat it as field access. Otherwise, it's a dotted variable name.
    ///
    /// When a symbol is found via suffix fallback (e.g., `x$` matches `x AS STRING`),
    /// we use the symbol's declared name to ensure consistent C code generation.
    fn check_identifier(&mut self, name: &str, span: crate::ast::Span) -> TypedExpr {
        // Check if it's an existing variable (exact match or suffix fallback)
        if let Some(symbol) = self.symbols.lookup_symbol(name) {
            // Use the symbol's declared name, not the reference name.
            // This ensures consistency when a suffixed reference (e.g., `x$`)
            // matches an unsuffixed declaration (e.g., `DIM x AS STRING`).
            return TypedExpr::new(
                TypedExprKind::Variable(symbol.name.clone()),
                symbol.basic_type.clone(),
                span,
            );
        }

        // If the name contains dots, check if any prefix is a UDT variable
        // This allows distinguishing `id.field` (field access) from `path.exe$` (dotted var name)
        if name.contains('.')
            && let Some(result) = self.try_resolve_dotted_field_access(name, span)
        {
            return result;
        }

        // Check if it's a parameterless function call
        if let Some(proc) = self.symbols.lookup_procedure(name)
            && proc.params.is_empty()
            && proc.return_type.is_some()
        {
            return TypedExpr::new(
                TypedExprKind::FunctionCall {
                    // Use canonical procedure name with type suffix
                    name: proc.name.clone(),
                    args: vec![],
                    params: vec![],
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

    /// Tries to resolve a dotted identifier as field access on a UDT variable.
    /// Returns Some(TypedExpr) if successful, None if the identifier is a plain dotted name.
    fn try_resolve_dotted_field_access(
        &mut self,
        name: &str,
        span: crate::ast::Span,
    ) -> Option<TypedExpr> {
        let parts: Vec<&str> = name.split('.').collect();

        // Try each prefix to see if it's a UDT variable
        for i in 1..parts.len() {
            let prefix = parts[..i].join(".");
            if let Some(symbol) = self.symbols.lookup_symbol(&prefix) {
                // Check if it's a UDT type
                if let BasicType::UserDefined(_) = &symbol.basic_type {
                    // This is a UDT variable, treat remaining parts as field access
                    // Build nested FieldAccess expressions for the chain
                    let fields: Vec<String> = parts[i..].iter().map(|s| s.to_string()).collect();

                    // Start with the base variable
                    let mut result = TypedExpr::new(
                        TypedExprKind::Variable(prefix),
                        symbol.basic_type.clone(),
                        span,
                    );

                    // Build nested FieldAccess for each field in the chain
                    let mut current_type = symbol.basic_type.clone();
                    for field_name in &fields {
                        // Get the field type for this level
                        let this_field_type = self.resolve_field_chain_type(
                            &current_type,
                            std::slice::from_ref(field_name),
                        );
                        result = TypedExpr::new(
                            TypedExprKind::FieldAccess {
                                object: Box::new(result),
                                field: field_name.clone(),
                            },
                            this_field_type.clone(),
                            span,
                        );
                        current_type = this_field_type;
                    }

                    return Some(result);
                }
            }
        }

        // No UDT prefix found, it's a plain dotted variable name
        None
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
            // If object is Unknown, an error was already reported upstream - don't cascade
            BasicType::Unknown => BasicType::Unknown,
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

    /// Type checks a binary operation.
    pub(super) fn check_binary(
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

            // Short-circuit logical operators (QB64)
            // _ANDALSO and _ORELSE always return Integer (boolean result)
            BinaryOp::AndAlso | BinaryOp::OrElse => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                BasicType::Integer
            }

            // String concatenation with + (both strings)
            BinaryOp::Add
                if left_typed.basic_type.is_string() && right_typed.basic_type.is_string() =>
            {
                BasicType::String
            }

            // String + numeric: implicit STR$() conversion (BASIC allows this)
            BinaryOp::Add
                if left_typed.basic_type.is_string() && right_typed.basic_type.is_numeric() =>
            {
                // Convert right operand to string implicitly via STR$()
                // The code generator will emit: qb_string_concat(left, qb_str(right))
                BasicType::String
            }

            // Numeric + string: implicit STR$() conversion (BASIC allows this)
            BinaryOp::Add
                if left_typed.basic_type.is_numeric() && right_typed.basic_type.is_string() =>
            {
                // Convert left operand to string implicitly via STR$()
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
    ///
    /// In BASIC, `name(args)` syntax can be:
    /// 1. Array access - if `name` is an array variable
    /// 2. External function call - if `name` is from DECLARE LIBRARY
    /// 3. Built-in or user-defined function call
    ///
    /// Due to BASIC's dual namespace model, a scalar `x` and array `x()` can coexist.
    /// When we see `x(args)`, we must check the array namespace first.
    pub(super) fn check_function_call(
        &mut self,
        name: &str,
        args: &[Expr],
        span: crate::ast::Span,
    ) -> TypedExpr {
        // First check the ARRAY namespace - `name(...)` syntax should find arrays first
        // This is critical for BASIC's dual namespace model where `x` (scalar) and
        // `x()` (array) can coexist.
        if let Some(symbol) = self.symbols.lookup_array(name) {
            // Clone values upfront to release borrow before calling check_array_access
            let resolved_name = symbol.name.clone();
            let element_type = symbol.basic_type.clone();
            let dimensions = if let SymbolKind::ArrayVariable { dimensions } = &symbol.kind {
                dimensions.clone()
            } else {
                vec![]
            };
            // Use resolved_name (from symbol) instead of raw 'name' for consistent C code generation
            // This handles suffix mismatch: separgslayout2$(i) -> separgslayout2[i] when array is STRING
            return self.check_array_access(&resolved_name, args, dimensions, element_type, span);
        }

        // Check for external functions (these are stored as scalars but have special handling)
        if let Some(symbol) = self.symbols.lookup_scalar(name)
            && let SymbolKind::ExternalFunction {
                c_name,
                params,
                return_type,
            } = &symbol.kind
        {
            // External function from DECLARE LIBRARY
            return self.check_external_function_call(
                name,
                c_name.clone(),
                args,
                params.clone(),
                return_type.clone(),
                span,
            );
        }

        // Special handling for _IIF - it's polymorphic (accepts any type for args 2 and 3)
        if name.eq_ignore_ascii_case("_IIF") {
            return self.check_iif_call(args, span);
        }

        // Look up procedure
        let proc = match self.symbols.lookup_procedure(name) {
            Some(p) => p.clone(),
            None => {
                // Check if there's a scalar variable with this name (NotAnArray error)
                // We use lookup_scalar here because in the dual namespace model,
                // if there was an array `name()`, we would have found it above.
                // Clone the type early to avoid borrow issues
                if let Some(basic_type) = self
                    .symbols
                    .lookup_scalar(name)
                    .map(|s| s.basic_type.clone())
                {
                    self.errors.push(SemanticError::NotAnArray {
                        name: name.to_string(),
                        span,
                    });
                    return TypedExpr::new(
                        TypedExprKind::FunctionCall {
                            name: name.to_string(),
                            args: args.iter().map(|a| self.check_expr(a)).collect(),
                            params: vec![],
                        },
                        basic_type,
                        span,
                    );
                }

                // Classic BASIC: implicitly declare array on first use with default bounds (0-10)
                // Determine type from name suffix (e.g., A$ -> String, X% -> Integer)
                let element_type = type_from_suffix(name).unwrap_or(BasicType::Single);

                // Create dimensions with default bounds (0 TO 10) for each index
                let dimensions: Vec<ArrayDimInfo> = args
                    .iter()
                    .map(|_| ArrayDimInfo {
                        lower_bound: 0,
                        upper_bound: 10,
                    })
                    .collect();

                // Define the implicit array
                let implicit_array = Symbol {
                    name: name.to_string(),
                    kind: SymbolKind::ArrayVariable {
                        dimensions: dimensions.clone(),
                    },
                    basic_type: element_type.clone(),
                    span,
                    is_mutable: true,
                };
                // Use update_or_define to handle any race conditions
                self.symbols.update_or_define_symbol(implicit_array);

                // Now treat as array access
                return self.check_array_access(name, args, dimensions, element_type, span);
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
                    name: proc.name.clone(),
                    args: args.iter().map(|a| self.check_expr(a)).collect(),
                    params: vec![],
                },
                BasicType::Void,
                span,
            );
        }

        // Check argument count (considering optional parameters)
        let required_count = proc.required_param_count();
        let max_count = proc.params.len();
        if args.len() < required_count || args.len() > max_count {
            self.errors.push(SemanticError::ArgumentCountMismatch {
                name: name.to_string(),
                expected_min: required_count,
                expected_max: max_count,
                found: args.len(),
                span,
            });
        }

        // Check argument types, handling array arguments specially
        let mut typed_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            // Check if this argument is an array reference (arr() syntax)
            if i < proc.params.len()
                && proc.params[i].is_array
                && let Some(typed_arg) = self.try_check_array_ref(arg)
            {
                typed_args.push(typed_arg);
                continue;
            }

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

        // Convert procedure parameters to typed parameters for BYREF handling
        let typed_params: Vec<TypedParameter> = proc
            .params
            .iter()
            .map(|p| TypedParameter {
                name: p.name.clone(),
                basic_type: p.basic_type.clone(),
                by_val: p.by_val,
                is_array: p.is_array,
            })
            .collect();

        TypedExpr::new(
            TypedExprKind::FunctionCall {
                // Use proc.name (the canonical name with type suffix) instead of caller's name
                // e.g., if caller uses "getelement" but function is "GETELEMENT$", use the latter
                name: proc.name.clone(),
                args: typed_args,
                params: typed_params,
            },
            proc.return_type.unwrap_or(BasicType::Void),
            span,
        )
    }

    /// Special handling for _IIF (polymorphic inline conditional).
    ///
    /// _IIF(condition, true_value, false_value) is QB64's ternary operator.
    /// Unlike regular functions, it accepts any type for true_value and false_value,
    /// as long as they are compatible. The return type is the common type of both.
    fn check_iif_call(&mut self, args: &[Expr], span: crate::ast::Span) -> TypedExpr {
        // Must have exactly 3 arguments
        if args.len() != 3 {
            self.errors.push(SemanticError::ArgumentCountMismatch {
                name: "_IIF".to_string(),
                expected_min: 3,
                expected_max: 3,
                found: args.len(),
                span,
            });
            return TypedExpr::new(
                TypedExprKind::FunctionCall {
                    name: "_IIF".to_string(),
                    args: args.iter().map(|a| self.check_expr(a)).collect(),
                    params: vec![],
                },
                BasicType::Double,
                span,
            );
        }

        // Check condition (first arg) - must be numeric (treated as boolean)
        let cond_typed = self.check_expr(&args[0]);
        if !cond_typed.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: cond_typed.basic_type.to_string(),
                span: args[0].span,
            });
        }

        // Check true and false values
        let true_typed = self.check_expr(&args[1]);
        let false_typed = self.check_expr(&args[2]);

        // Determine result type - both must be compatible
        let result_type =
            if true_typed.basic_type.is_string_like() && false_typed.basic_type.is_string_like() {
                // Both are strings - result is string
                BasicType::String
            } else if true_typed.basic_type.is_numeric() && false_typed.basic_type.is_numeric() {
                // Both are numeric - promote to wider type
                crate::semantic::types::promote_numeric_types(
                    &true_typed.basic_type,
                    &false_typed.basic_type,
                )
            } else {
                // Type mismatch between true and false parts
                self.errors.push(SemanticError::IifTypeMismatch {
                    true_type: true_typed.basic_type.to_string(),
                    false_type: false_typed.basic_type.to_string(),
                    span,
                });
                // Default to Double for error recovery
                BasicType::Double
            };

        TypedExpr::new(
            TypedExprKind::FunctionCall {
                name: "_IIF".to_string(),
                args: vec![cond_typed, true_typed, false_typed],
                params: vec![], // Built-in, all args are BYVAL
            },
            result_type,
            span,
        )
    }

    /// Attempts to check an expression as an array reference (arr() syntax).
    ///
    /// In BASIC, `arr()` means "pass the entire array" to a procedure.
    /// This is recognized when the expression is a FunctionCall with empty args
    /// and the name matches a declared array.
    ///
    /// Returns `Some(TypedExpr)` with `ArrayRef` kind if this is an array reference,
    /// or `None` if the expression should be checked normally.
    pub(super) fn try_check_array_ref(&mut self, expr: &Expr) -> Option<TypedExpr> {
        // Array reference syntax: name with empty parentheses, e.g., arr()
        if let ExprKind::FunctionCall { name, args } = &expr.kind
            && args.is_empty()
        {
            // Check if this name refers to a declared array (use array namespace)
            if let Some(symbol) = self.symbols.lookup_array(name)
                && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
            {
                let typed_dimensions: Vec<TypedArrayDimension> = dimensions
                    .iter()
                    .map(|d| TypedArrayDimension {
                        lower: d.lower_bound,
                        upper: d.upper_bound,
                    })
                    .collect();

                return Some(TypedExpr::new(
                    TypedExprKind::ArrayRef {
                        name: name.to_string(),
                        element_type: symbol.basic_type.clone(),
                        dimensions: typed_dimensions,
                    },
                    // The type of an array reference is a pointer/reference type,
                    // but for simplicity we use the element type since BASIC
                    // doesn't have explicit pointer types
                    symbol.basic_type.clone(),
                    expr.span,
                ));
            }
        }
        None
    }

    /// Type checks an array access.
    pub(super) fn check_array_access(
        &mut self,
        name: &str,
        indices: &[Expr],
        dim_info: Vec<ArrayDimInfo>,
        element_type: BasicType,
        span: crate::ast::Span,
    ) -> TypedExpr {
        // Only check dimension count if we know the dimensions (not empty).
        // Array parameters and REDIM arrays may have unknown dimensions at compile time.
        if !dim_info.is_empty() && indices.len() != dim_info.len() {
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
        // For dynamic arrays (empty dim_info), use placeholder dimensions
        let typed_dimensions: Vec<TypedArrayDimension> = if dim_info.is_empty() {
            // Dynamic array - create placeholder dimensions based on indices
            indices
                .iter()
                .map(|_| TypedArrayDimension { lower: 0, upper: 0 })
                .collect()
        } else {
            dim_info
                .iter()
                .map(|d| TypedArrayDimension {
                    lower: d.lower_bound,
                    upper: d.upper_bound,
                })
                .collect()
        };

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

    /// Type checks an external function call (from DECLARE LIBRARY).
    ///
    /// External functions need special handling because:
    /// - The C function name may differ from the BASIC name (ALIAS)
    /// - String arguments need marshalling (qb_string* -> char*)
    /// - BYVAL parameters pass by value
    fn check_external_function_call(
        &mut self,
        name: &str,
        c_name: String,
        args: &[Expr],
        params: Vec<BasicType>,
        return_type: BasicType,
        span: crate::ast::Span,
    ) -> TypedExpr {
        // Check argument count
        if args.len() != params.len() {
            self.errors.push(SemanticError::ArgumentCountMismatch {
                name: name.to_string(),
                expected_min: params.len(),
                expected_max: params.len(),
                found: args.len(),
                span,
            });
        }

        // Type-check arguments
        let mut typed_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let typed_arg = self.check_expr(arg);

            if i < params.len() {
                let expected_type = &params[i];
                if !typed_arg.basic_type.is_convertible_to(expected_type) {
                    self.errors.push(SemanticError::ArgumentTypeMismatch {
                        position: i + 1,
                        expected: expected_type.to_string(),
                        found: typed_arg.basic_type.to_string(),
                        span: arg.span,
                    });
                }
            }

            typed_args.push(typed_arg);
        }

        // Build parameter info for codegen marshalling
        // Note: We don't have is_byval info stored in SymbolKind::ExternalFunction
        // For now, assume all external params are BYVAL (required for C interop)
        let param_info: Vec<ExternalParamInfo> = params
            .iter()
            .map(|typ| ExternalParamInfo {
                typ: typ.clone(),
                is_byval: true, // External functions default to BYVAL for C compatibility
            })
            .collect();

        TypedExpr::new(
            TypedExprKind::ExternalFunctionCall {
                name: name.to_string(),
                c_name,
                args: typed_args,
                params: param_info,
            },
            return_type,
            span,
        )
    }

    /// Parses a type name string into a BasicType.
    ///
    /// Handles type names like "INTEGER", "_INTEGER64", "_UNSIGNED INTEGER", etc.
    fn parse_type_name(&self, type_name: &str) -> BasicType {
        let upper = type_name.to_uppercase();

        // Check for _UNSIGNED prefix
        let (is_unsigned, base) = if upper.starts_with("_UNSIGNED ") {
            (true, upper.trim_start_matches("_UNSIGNED "))
        } else {
            (false, upper.as_str())
        };

        let base_type = match base {
            "INTEGER" => BasicType::Integer,
            "LONG" => BasicType::Long,
            "SINGLE" => BasicType::Single,
            "DOUBLE" => BasicType::Double,
            "STRING" => BasicType::String,
            "_BYTE" => BasicType::Byte,
            "_BIT" => BasicType::Bit,
            "_INTEGER64" => BasicType::Integer64,
            "_FLOAT" => BasicType::Float,
            "_OFFSET" => BasicType::Offset,
            _ => BasicType::Integer, // Default fallback
        };

        if is_unsigned {
            match base_type {
                BasicType::Integer => BasicType::UnsignedInteger,
                BasicType::Long => BasicType::UnsignedLong,
                BasicType::Byte => BasicType::UnsignedByte,
                BasicType::Integer64 => BasicType::UnsignedInteger64,
                _ => base_type, // No unsigned variant
            }
        } else {
            base_type
        }
    }
}
