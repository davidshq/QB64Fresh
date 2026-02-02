//! Function calls, array access, and field access code generation.
//!
//! This module handles built-in and user-defined function calls, array
//! indexing, UDT field access, external (DECLARE LIBRARY) calls, and
//! procedure pointers.

use crate::codegen::error::{CodeGenError, CodeGenErrorKind};
use crate::semantic::typed_ir::{
    ExternalParamInfo, TypedArrayDimension, TypedExpr, TypedExprKind, TypedParameter,
};
use crate::semantic::types::BasicType;

use super::super::types::{c_identifier, c_type};
use super::helpers::{c_function_name, needs_fixed_string_conversion, unwrap_qb_str_from_c};
use super::ExprEmitCtx;
/// Emits C code for a function call (built-in or user-defined).
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_function_call(
    ctx: &ExprEmitCtx,
    expr: &TypedExpr,
    name: &str,
    args: &[TypedExpr],
    params: &[TypedParameter],
    wrap_string_temps: bool,
) -> Result<String, CodeGenError> {
            let upper_name = name.to_uppercase();
            if ctx.no_shell && (upper_name == "SHELL" || upper_name == "_SHELLHIDE") {
                return Err(CodeGenError::new(CodeGenErrorKind::ShellDisabled).with_span(expr.span));
            }
            // Special case: _IIF is polymorphic - use appropriate variant based on return type
            if upper_name == "_IIF" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                let c_name = if expr.basic_type.is_string() {
                    "qb_iif_str".to_string()
                } else {
                    "qb_iif".to_string()
                };
                return Ok(format!("{}({})", c_name, args_str));
            }

            // Special case: MID$ with 2 arguments (no length) uses qb_mid2
            if upper_name == "MID$" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_mid2({})", args_str));
            }

            // Special case: INSTR with 2 arguments (no start) uses qb_instr2
            if upper_name == "INSTR" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_instr2({})", args_str));
            }

            // Special case: _INSTRREV with 3 arguments (start, source, search) uses qb_instrrev3
            // C function signature: qb_instrrev3(source, search, start) - arguments reordered
            if upper_name == "_INSTRREV" && args.len() == 3 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_vec = args_code?;
                // Reorder: BASIC (start, source, search) -> C (source, search, start)
                return Ok(format!(
                    "qb_instrrev3({}, {}, {})",
                    args_vec[1], args_vec[2], args_vec[0]
                ));
            }

            // Special case: LBOUND with 1 argument (array) uses qb_lbound
            // Don't convert fixed-length strings - pass array pointer directly
            if upper_name == "LBOUND" && args.len() == 1 {
                let arg_code = ctx.emit(&args[0], false)?;
                // For fixed-length strings, unwrap qb_str_from_c() if present
                // LBOUND needs the raw array pointer, not a converted string
                let unwrapped = if arg_code.starts_with("qb_str_from_c(") {
                    unwrap_qb_str_from_c(&arg_code)
                } else {
                    arg_code
                };
                return Ok(format!("qb_lbound({})", unwrapped));
            }

            // Special case: LBOUND with 2 arguments (array, dimension) uses qb_lbound2
            if upper_name == "LBOUND" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_lbound2({})", args_str));
            }

            // Special case: UBOUND with 1 argument (array) uses qb_ubound
            // Don't convert fixed-length strings - pass array pointer directly
            if upper_name == "UBOUND" && args.len() == 1 {
                let arg_code = ctx.emit(&args[0], false)?;
                // For fixed-length strings, unwrap qb_str_from_c() if present
                // UBOUND needs the raw array pointer, not a converted string
                let unwrapped = if arg_code.starts_with("qb_str_from_c(") {
                    unwrap_qb_str_from_c(&arg_code)
                } else {
                    arg_code
                };
                return Ok(format!("qb_ubound({})", unwrapped));
            }

            // Special case: UBOUND with 2 arguments (array, dimension) uses qb_ubound2
            if upper_name == "UBOUND" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_ubound2({})", args_str));
            }

            // Special case: CSNG — type-dependent wrapper (overflow error 6)
            if upper_name == "CSNG" && args.len() == 1 {
                let arg_code = ctx.emit(&args[0], false)?;
                let c_fn = match args[0].basic_type {
                    BasicType::Single => "qb_csng_float",
                    _ => "qb_csng_double",
                };
                return Ok(format!("{}({})", c_fn, arg_code));
            }

            // Special case: CDBL — use qb_cdbl_float when arg is Single
            if upper_name == "CDBL" && args.len() == 1 {
                let arg_code = ctx.emit(&args[0], false)?;
                return Ok(match args[0].basic_type {
                    BasicType::Single => format!("qb_cdbl_float({})", arg_code),
                    _ => format!("(double)({})", arg_code),
                });
            }

            // Special case: LEN - use qb_len_str for strings, sizeof for numeric types
            if upper_name == "LEN" && args.len() == 1 {
                let arg_code = ctx.emit(&args[0], false)?;
                if args[0].basic_type.is_string() {
                    return Ok(format!("qb_len_str({})", arg_code));
                } else {
                    // Numeric types: use sizeof to get byte size
                    return Ok(format!("(int32_t)sizeof({})", arg_code));
                }
            }

            // Special case: HEX$/OCT$/_BIN$ with float arg use _float variant (bit pattern)
            if (upper_name == "HEX$" || upper_name == "OCT$" || upper_name == "_BIN$")
                && args.len() == 1
            {
                let arg_code = ctx.emit(&args[0], false)?;
                let use_float = matches!(args[0].basic_type, BasicType::Double | BasicType::Single);
                let c_name = if use_float {
                    match upper_name.as_str() {
                        "HEX$" => "qb_hex_float",
                        "OCT$" => "qb_oct_float",
                        "_BIN$" => "qb_bin_float",
                        _ => unreachable!(),
                    }
                } else {
                    match upper_name.as_str() {
                        "HEX$" => "qb_hex",
                        "OCT$" => "qb_oct",
                        "_BIN$" => "qb_bin",
                        _ => unreachable!(),
                    }
                };
                return Ok(format!("{}({})", c_name, arg_code));
            }

            // Special case: VARPTR - returns address of variable as integer
            // Must pass &variable, not the value
            if upper_name == "VARPTR" && args.len() == 1 {
                let arg = &args[0];
                // Check if this is an addressable lvalue
                let is_lvalue = matches!(
                    arg.kind,
                    TypedExprKind::Variable { .. }
                        | TypedExprKind::ArrayAccess { .. }
                        | TypedExprKind::FieldAccess { .. }
                );
                if is_lvalue {
                    let arg_code = ctx.emit(arg, false)?;
                    return Ok(format!("((int32_t)(intptr_t)&({}))", arg_code));
                } else {
                    // Non-lvalue - can't take address, return 0
                    return Ok("0".to_string());
                }
            }

            // Special case: VARPTR$ - returns binary string of variable's address
            if upper_name == "VARPTR$" && args.len() == 1 {
                let arg = &args[0];
                let is_lvalue = matches!(
                    arg.kind,
                    TypedExprKind::Variable { .. }
                        | TypedExprKind::ArrayAccess { .. }
                        | TypedExprKind::FieldAccess { .. }
                );
                if is_lvalue {
                    let arg_code = ctx.emit(arg, false)?;
                    return Ok(format!("qb_varptr_str(&({}))", arg_code));
                } else {
                    return Ok("qb_string_new(\"\")".to_string());
                }
            }

            // Special case: VARSEG - returns segment (always 0 in flat memory model)
            if upper_name == "VARSEG" && args.len() == 1 {
                // In modern flat memory model, segment is meaningless - return 0
                return Ok("0".to_string());
            }

            // Special case: SADD - returns address of string data
            if upper_name == "SADD" && args.len() == 1 {
                let arg_code = ctx.emit(&args[0], false)?;
                return Ok(format!("((int32_t)(intptr_t)({}).data)", arg_code));
            }

            // Special case: _MESSAGEBOX with different argument counts
            if upper_name == "_MESSAGEBOX" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    1 => Ok(format!("qb_messagebox1({})", args_str)),
                    2 => Ok(format!("qb_messagebox2({})", args_str)),
                    3 => Ok(format!("qb_messagebox({})", args_str)),
                    4 => Ok(format!("qb_messagebox4({})", args_str)),
                    _ => Ok(format!("qb_messagebox({})", args_str)),
                };
            }

            // Special case: _LOADFONT with different argument counts
            if upper_name == "_LOADFONT" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    2 => Ok(format!("qb_loadfont({})", args_str)),
                    3 => Ok(format!("qb_loadfont3({})", args_str)),
                    4 => Ok(format!("qb_loadfont4({})", args_str)),
                    _ => Ok(format!("qb_loadfont({})", args_str)),
                };
            }

            // Special case: _WIDTH without arguments uses current destination
            if upper_name == "_WIDTH" && args.is_empty() {
                return Ok("qb_gfx_image_width(0)".to_string());
            }

            // Special case: _HEIGHT without arguments uses current destination
            if upper_name == "_HEIGHT" && args.is_empty() {
                return Ok("qb_gfx_image_height(0)".to_string());
            }

            // Special case: _RGB32 with different argument counts
            // 3 args: _RGB32(r, g, b) -> qb__rgb32
            // 4 args: _RGB32(r, g, b, a) or _RGB32(gray, gray, gray, a) -> qb__rgb32_4
            if upper_name == "_RGB32" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    3 => Ok(format!("qb__rgb32({})", args_str)),
                    4 => Ok(format!("qb__rgb32_4({})", args_str)),
                    _ => Ok(format!("qb__rgb32({})", args_str)),
                };
            }

            // Special case: SCREEN function (reads screen char/attr)
            // 2 args: SCREEN(row, col) -> qb_screen
            // 3 args: SCREEN(row, col, attr_flag) -> qb_screen3
            if upper_name == "SCREEN" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    2 => Ok(format!("qb_screen({})", args_str)),
                    3 => Ok(format!("qb_screen3({})", args_str)),
                    _ => Ok(format!("qb_screen({})", args_str)),
                };
            }

            // Special case: STRING$ with 2 args - use numeric variant if second arg is not a string
            // STRING$(n, code) fills with ASCII code, STRING$(n, c$) fills with first char of string
            // For external runtime, qb_string_fill takes int32_t, so we use qb_string_fill_str for string args
            if upper_name == "STRING$" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_vec = args_code?;
                if !args[1].basic_type.is_string() {
                    return Ok(format!(
                        "qb_string_fill_code({}, {})",
                        args_vec[0], args_vec[1]
                    ));
                } else {
                    // For string argument, use qb_string_fill_str which extracts first char
                    return Ok(format!(
                        "qb_string_fill_str({}, {})",
                        args_vec[0], args_vec[1]
                    ));
                }
            }

            // Special case: _SAVEFILEDIALOG$ expects const char* arguments, not QbString*
            if upper_name == "_SAVEFILEDIALOG$" {
                let mut args_codes = Vec::new();
                for arg in args.iter() {
                    let arg_code = ctx.emit(arg, false)?;
                    // Convert QbString* to const char* using qb_string_data()
                    let arg_data = if arg_code.starts_with("qb_str_from_c(") {
                        // Fixed-length string - unwrap and use directly (it's already const char*)
                        unwrap_qb_str_from_c(&arg_code)
                    } else {
                        // Dynamic string - use qb_string_data() to get const char*
                        format!("qb_string_data({})", arg_code)
                    };
                    args_codes.push(arg_data);
                }
                let args_str = args_codes.join(", ");
                // qb_savefiledialog takes 4 const char* arguments: title, initial_dir, default_name, filter
                return Ok(format!("qb_savefiledialog({})", args_str));
            }

            // Special case: _OPENFILEDIALOG$ with different argument counts
            if upper_name == "_OPENFILEDIALOG$" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    2 => Ok(format!("qb_openfiledialog({})", args_str)),
                    3 => Ok(format!("qb_openfiledialog3({})", args_str)),
                    4 => Ok(format!("qb_openfiledialog4({})", args_str)),
                    5 => Ok(format!("qb_openfiledialog5({})", args_str)),
                    _ => Ok(format!("qb_openfiledialog({})", args_str)),
                };
            }

            // Special case: _SELECTFOLDERDIALOG$ with different argument counts
            // External runtime: qb_selectfolderdialog(const char* title, const char* initial_dir)
            // Inline runtime: qb_selectfolderdialog(qb_string* title) or qb_selectfolderdialog2(qb_string* title, qb_string* initial_dir)
            if upper_name == "_SELECTFOLDERDIALOG$" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_vec = args_code?;
                return match args_vec.len() {
                    0 => Ok("qb_selectfolderdialog(NULL, NULL)".to_string()), // No args - use NULL for both
                    1 => Ok(format!(
                        "qb_selectfolderdialog(qb_string_data({}), NULL)",
                        args_vec[0]
                    )), // 1 arg - convert to const char* and pass NULL for initial_dir
                    2 => {
                        // 2 args - convert both to const char*
                        Ok(format!(
                            "qb_selectfolderdialog(qb_string_data({}), qb_string_data({}))",
                            args_vec[0], args_vec[1]
                        ))
                    }
                    _ => Ok(format!(
                        "qb_selectfolderdialog(qb_string_data({}), NULL)",
                        args_vec[0]
                    )), // Default to first arg
                };
            }

            // Special case: _INPUTBOX$(title, message, default_input) — 3 optional strings → qb_inputbox(const char*, const char*, const char*)
            if upper_name == "_INPUTBOX$" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_vec = args_code?;
                let a0 = args_vec
                    .first()
                    .map(|c| format!("qb_string_data({})", c))
                    .unwrap_or_else(|| "NULL".to_string());
                let a1 = args_vec
                    .get(1)
                    .map(|c| format!("qb_string_data({})", c))
                    .unwrap_or_else(|| "NULL".to_string());
                let a2 = args_vec
                    .get(2)
                    .map(|c| format!("qb_string_data({})", c))
                    .unwrap_or_else(|| "NULL".to_string());
                return Ok(format!("qb_inputbox({}, {}, {})", a0, a1, a2));
            }

            // Special case: _COLORCHOOSERDIALOG(title, defaultRGB) — 2 optional args → qb_colorchooserdialog(const char*, uint32_t)
            if upper_name == "_COLORCHOOSERDIALOG" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_vec = args_code?;
                let a0 = args_vec
                    .first()
                    .map(|c| format!("qb_string_data({})", c))
                    .unwrap_or_else(|| "NULL".to_string());
                let a1 = args_vec.get(1).map(|c| c.as_str()).unwrap_or("0");
                return Ok(format!("qb_colorchooserdialog({}, (uint32_t)({}))", a0, a1));
            }

            // Special case: _SCREENIMAGE - provide default 0,0,0,0 for full screen capture
            if upper_name == "_SCREENIMAGE" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    0 => Ok("qb_screenimage(0, 0, 0, 0)".to_string()),
                    4 => Ok(format!("qb_screenimage({})", args_str)),
                    _ => Ok(format!("qb_screenimage({})", args_str)),
                };
            }

            // Special case: COMMAND$ with argument uses qb_command_n
            if upper_name == "COMMAND$" && !args.is_empty() {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_command_n({})", args_str));
            }

            // Special case: ENVIRON$(index) uses qb_environ_by_index, ENVIRON$(name$) uses qb_environ
            if upper_name == "ENVIRON$" && args.len() == 1 {
                let arg = &args[0];
                let arg_code = ctx.emit(arg, false)?;
                let use_index = matches!(
                    arg.basic_type,
                    BasicType::Integer
                        | BasicType::Long
                        | BasicType::Integer64
                        | BasicType::Single
                        | BasicType::Double
                );
                return Ok(if use_index {
                    format!("qb_environ_by_index({})", arg_code)
                } else {
                    format!("qb_environ({})", arg_code)
                });
            }

            // Special case: ASC with 2 arguments (position) uses qb_asc2
            if upper_name == "ASC" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_asc2({})", args_str));
            }

            // Special case: STRIG with 2 arguments (controller) uses qb_strig2
            // QB64 extension: STRIG(button, controller) overrides implicit controller
            if upper_name == "STRIG" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_strig2({})", args_str));
            }

            // Special case: TIMER with argument (accuracy) uses qb_timer_n
            if upper_name == "TIMER" && !args.is_empty() {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_timer_n({})", args_str));
            }

            // Special case: _CONSOLE - use qb_console_get() for no args, qb_console(mode) with args
            if upper_name == "_CONSOLE" {
                if args.is_empty() {
                    return Ok("qb_console_get()".to_string());
                } else {
                    let args_code: Result<Vec<_>, _> = args
                        .iter()
                        .map(|e| {
                            ctx.emit(e, false)
                        })
                        .collect();
                    let args_str = args_code?.join(", ");
                    return Ok(format!("qb_console({})", args_str));
                }
            }

            // Special case: _MAPUNICODE - use different functions based on arg count
            if upper_name == "_MAPUNICODE" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    1 => Ok(format!("qb__mapunicode1({})", args_str)),
                    2 => Ok(format!("qb__mapunicode2({})", args_str)),
                    3 => Ok(format!("qb__mapunicode({})", args_str)),
                    _ => Ok(format!("qb__mapunicode({})", args_str)),
                };
            }

            // Special case: _PALETTECOLOR function form (GET)
            // When used as function: _PALETTECOLOR(attr%) or _PALETTECOLOR(attr%, handle&)
            // Always returns the palette color at the given index
            if upper_name == "_PALETTECOLOR" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_vec = args_code?;
                return match args.len() {
                    1 => Ok(format!("qb_palettecolor_get({}, 0)", args_vec[0])),
                    2 => Ok(format!(
                        "qb_palettecolor_get({}, {})",
                        args_vec[0], args_vec[1]
                    )),
                    // 3 args in function context shouldn't happen, but handle gracefully
                    _ => Ok(format!(
                        "qb_palettecolor({}, {}, {})",
                        args_vec.first().map(String::as_str).unwrap_or("0"),
                        args_vec.get(1).map(String::as_str).unwrap_or("0"),
                        args_vec.get(2).map(String::as_str).unwrap_or("0")
                    )),
                };
            }

            // Special case: _ICON - use different functions based on arg count
            if upper_name == "_ICON" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    0 => Ok("qb_icon()".to_string()),
                    1 => Ok(format!("qb_icon1({})", args_str)),
                    2 => Ok(format!("qb_icon2({})", args_str)),
                    _ => Ok(format!("qb_icon({})", args_str)),
                };
            }

            // Special case: _ACCEPTFILEDROP - use different functions based on arg count
            if upper_name == "_ACCEPTFILEDROP" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        ctx.emit(e, false)
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    0 => Ok("qb_acceptfiledrop()".to_string()),
                    1 => Ok(format!("qb_acceptfiledrop1({})", args_str)),
                    _ => Ok(format!("qb_acceptfiledrop({})", args_str)),
                };
            }

            // Special case: SHELL function expects const char*, not QbString*
            // _SHELLHIDE uses inline runtime which takes QbString*, so no conversion needed
            // Must be checked before c_function_name() is called
            if upper_name == "SHELL" {
                // SHELL always takes 1 string argument
                let cmd_code = ctx.emit(&args[0], false)?;
                // Convert QbString* to const char* using qb_string_data()
                // qb_shell expects const char*, so we need to extract the data pointer
                let cmd_data = if cmd_code.starts_with("qb_str_from_c(") {
                    // Fixed-length string - unwrap and use directly (it's already const char*)
                    unwrap_qb_str_from_c(&cmd_code)
                } else {
                    // Dynamic string - use qb_string_data() to get const char*
                    format!("qb_string_data({})", cmd_code)
                };
                return Ok(format!("qb_shell({})", cmd_data));
            }

            // Special case: _OPENHOST expects const char* connection string, not QbString*
            if upper_name == "_OPENHOST" {
                // _OPENHOST takes 1 string argument (connection string like "TCP/IP:port")
                let conn_code = ctx.emit(&args[0], false)?;
                // Convert QbString* to const char* using qb_string_data()
                let conn_data = if conn_code.starts_with("qb_str_from_c(") {
                    // Fixed-length string - unwrap and use directly (it's already const char*)
                    unwrap_qb_str_from_c(&conn_code)
                } else {
                    // Dynamic string - use qb_string_data() to get const char*
                    format!("qb_string_data({})", conn_code)
                };
                return Ok(format!("qb_net_openhost({})", conn_data));
            }

            // Special case: _OPENCLIENT expects const char* connection string, not QbString*
            if upper_name == "_OPENCLIENT" {
                // _OPENCLIENT takes 1 string argument (connection string like "TCP/IP:port:host")
                let conn_code = ctx.emit(&args[0], false)?;
                // Convert QbString* to const char* using qb_string_data()
                let conn_data = if conn_code.starts_with("qb_str_from_c(") {
                    // Fixed-length string - unwrap and use directly (it's already const char*)
                    unwrap_qb_str_from_c(&conn_code)
                } else {
                    // Dynamic string - use qb_string_data() to get const char*
                    format!("qb_string_data({})", conn_code)
                };
                return Ok(format!("qb_net_openclient({})", conn_data));
            }

            // Special case: qb_removestringenclosingpair_str expects QbString** (pointer to pointer)
            // This is a QB64pe-specific runtime function that takes addresses of QbString* variables
            if c_function_name(name) == "qb_removestringenclosingpair_str" {
                let mut args_codes = Vec::new();
                for arg in args {
                    let arg_code = ctx.emit(arg, false)?;
                    // Check if expression is an lvalue (can take address of)
                    let is_lvalue = matches!(
                        arg.kind,
                        TypedExprKind::Variable { .. }
                            | TypedExprKind::ArrayAccess { .. }
                            | TypedExprKind::FieldAccess { .. }
                    );

                    if is_lvalue {
                        // It's a variable/array/field - take address: &variable
                        // For const variables, we need to cast away const: (QbString**)&variable
                        // Check if the variable name suggests it might be const (common pattern)
                        let needs_const_cast = if let TypedExprKind::Variable(var_name) = &arg.kind
                        {
                            arg_code.contains("const ")
                                || (var_name.contains("METACOMMAND")
                                    && var_name.contains("ENCLOSING"))
                        } else {
                            arg_code.contains("const ")
                        };

                        if needs_const_cast {
                            let arg_code_clean = arg_code.replace("const ", "");
                            args_codes.push(format!("(QbString**)&{}", arg_code_clean));
                        } else {
                            args_codes.push(format!("&{}", arg_code));
                        }
                    } else {
                        // It's a function call or expression - use statement-expression to create temporary
                        let temp_name = format!("_tmp_qbstr_{}", args_codes.len());
                        args_codes.push(format!(
                            "({{ QbString* {} = {}; &{}; }})",
                            temp_name, arg_code, temp_name
                        ));
                    }
                }
                let args_str = args_codes.join(", ");
                return Ok(format!("qb_removestringenclosingpair_str({})", args_str));
            }

            // Special case: qb_hasstringenclosingpair_int_int expects QbString** (pointer to pointer)
            // Same handling as qb_removestringenclosingpair_str
            if c_function_name(name) == "qb_hasstringenclosingpair_int_int" {
                let mut args_codes = Vec::new();
                for arg in args {
                    let arg_code = ctx.emit(arg, false)?;
                    // Check if expression is an lvalue (can take address of)
                    let is_lvalue = matches!(
                        arg.kind,
                        TypedExprKind::Variable { .. }
                            | TypedExprKind::ArrayAccess { .. }
                            | TypedExprKind::FieldAccess { .. }
                    );

                    if is_lvalue {
                        // It's a variable/array/field - take address: &variable
                        // For const variables, we need to cast away const: (QbString**)&variable
                        let needs_const_cast = if let TypedExprKind::Variable(var_name) = &arg.kind
                        {
                            arg_code.contains("const ")
                                || (var_name.contains("METACOMMAND")
                                    && var_name.contains("ENCLOSING"))
                        } else {
                            arg_code.contains("const ")
                        };

                        if needs_const_cast {
                            let arg_code_clean = arg_code.replace("const ", "");
                            args_codes.push(format!("(QbString**)&{}", arg_code_clean));
                        } else {
                            args_codes.push(format!("&{}", arg_code));
                        }
                    } else {
                        // It's a function call or expression - use statement-expression to create temporary
                        let temp_name = format!("_tmp_qbstr_{}", args_codes.len());
                        args_codes.push(format!(
                            "({{ QbString* {} = {}; &{}; }})",
                            temp_name, arg_code, temp_name
                        ));
                    }
                }
                let args_str = args_codes.join(", ");
                return Ok(format!("qb_hasstringenclosingpair_int_int({})", args_str));
            }

            let c_name = c_function_name(name);

            // Special case: RND without arguments defaults to RND(1)
            if name == "RND" && args.is_empty() {
                return Ok(format!("{}(1.0f)", c_name));
            }

            // For user-defined functions with BYREF parameters, add & for lvalue args
            if !params.is_empty() {
                let mut args_codes = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let arg_code = ctx.emit(arg, wrap_string_temps)?;

                    // Check if the argument is a UDT variable that's already a pointer
                    // (from being a BYREF UDT parameter in the current function)
                    let arg_is_udt_pointer = matches!(&arg.kind, TypedExprKind::Variable(name)
                    if {
                        let c_arg = c_identifier(name).to_lowercase();
                        ctx.byref_udt_names.iter().any(|s| s.to_lowercase() == c_arg)
                    });

                    // Check if this parameter is byref (and not an array)
                    // For UDT arguments that are already pointers, we don't need to add &
                    let is_byref = !arg_is_udt_pointer
                        && params
                            .get(i)
                            .map(|p| !p.by_val && !p.is_array)
                            .unwrap_or(false);

                    if is_byref {
                        // Check if expression is an lvalue (can take address of)
                        // Note: Built-in constants like _TRUE, _FALSE are Variables in the AST
                        // but expand to C macros, so they're not true lvalues
                        let is_builtin_const = matches!(&arg.kind, TypedExprKind::Variable(name)
                            if name.starts_with('_') && name.chars().all(|c| c.is_uppercase() || c == '_'));

                        // Fixed-length string variables are wrapped with qb_str_from_c(),
                        // making them function call results (not lvalues)
                        let is_fixed_string = matches!(arg.basic_type, BasicType::FixedString(_));

                        let is_lvalue = !is_builtin_const
                            && !is_fixed_string
                            && matches!(
                                arg.kind,
                                TypedExprKind::Variable { .. }
                                    | TypedExprKind::ArrayAccess { .. }
                                    | TypedExprKind::FieldAccess { .. }
                            );

                        // Check if argument type matches parameter type
                        // In BASIC, passing a LONG to a function expecting INTEGER% creates
                        // an implicit temporary. The function gets a pointer to the temp.
                        let param_type = params.get(i).map(|p| &p.basic_type);
                        let types_match =
                            param_type.map(|pt| pt == &arg.basic_type).unwrap_or(true);

                        if is_lvalue && types_match {
                            // For simple variable names, use &variable (no parentheses needed)
                            // For complex expressions, use &(expr)
                            // Special case: If the variable is const (like HASHFLAG_*, DEPENDENCY_*),
                            // we need to cast away const to match function signatures that expect non-const pointers
                            let needs_parens = arg_code.contains(' ')
                                || arg_code.contains('(')
                                || arg_code.contains('[');

                            // Check if this looks like a const variable (all uppercase with underscores)
                            // Common patterns: HASHFLAG_*, DEPENDENCY_*
                            let is_likely_const = !needs_parens
                                && arg_code
                                    .chars()
                                    .all(|c| c.is_uppercase() || c == '_' || c.is_ascii_digit())
                                && (arg_code.starts_with("HASHFLAG_")
                                    || arg_code.starts_with("DEPENDENCY_")
                                    || arg_code.contains("_FLAG")
                                    || arg_code.contains("_DEPENDENCY"));

                            if is_likely_const {
                                // Cast away const: (int32_t*)&CONSTANT
                                let c_ty = param_type
                                    .map(c_type)
                                    .unwrap_or_else(|| "int32_t".to_string());
                                args_codes.push(format!("({}*)&{}", c_ty, arg_code));
                            } else if needs_parens {
                                args_codes.push(format!("&({})", arg_code));
                            } else {
                                args_codes.push(format!("&{}", arg_code));
                            }
                        } else {
                            // Non-lvalue expression OR type mismatch - use C compound literal
                            // Format: &(type){expr} creates a temporary that can be addressed
                            let c_ty = param_type
                                .map(c_type)
                                .unwrap_or_else(|| "int32_t".to_string());

                            // Use C99 compound literals for all types including pointer types.
                            // Compound literals have block-scope lifetime, so they remain valid
                            // for the entire function call.
                            // Format: &(type){expr}
                            if c_ty.ends_with('*') {
                                // For pointer types (including QbString*), use compound literal
                                if needs_fixed_string_conversion(arg) {
                                    // Fixed-length string - convert first
                                    let mut inner_code = unwrap_qb_str_from_c(&arg_code);
                                    while inner_code.starts_with("qb_str_from_c(") {
                                        inner_code = unwrap_qb_str_from_c(&inner_code);
                                    }
                                    args_codes.push(format!(
                                        "&({}){{qb_str_from_c({})}}",
                                        c_ty, inner_code
                                    ));
                                } else {
                                    // Dynamic string or other pointer type
                                    args_codes.push(format!("&({}){{{}}}", c_ty, arg_code));
                                }
                            } else {
                                // Non-pointer type - can use compound literal
                                if needs_fixed_string_conversion(arg) {
                                    // Fixed-length string - convert first
                                    let mut inner_code = unwrap_qb_str_from_c(&arg_code);
                                    while inner_code.starts_with("qb_str_from_c(") {
                                        inner_code = unwrap_qb_str_from_c(&inner_code);
                                    }
                                    args_codes.push(format!("&({}){{{}}}", c_ty, inner_code));
                                } else {
                                    // Cast value to parameter type to handle type mismatches
                                    args_codes.push(format!("&({}){{{}}}", c_ty, arg_code));
                                }
                            }
                        }
                    } else {
                        // BYVAL parameter
                        // Fixed-length strings need conversion to qb_string*
                        if needs_fixed_string_conversion(arg) {
                            // Check if already wrapped to avoid double wrapping
                            // Unwrap multiple levels if needed
                            let mut inner_code = unwrap_qb_str_from_c(&arg_code);
                            while inner_code.starts_with("qb_str_from_c(") {
                                inner_code = unwrap_qb_str_from_c(&inner_code);
                            }
                            args_codes.push(format!("qb_str_from_c({})", inner_code));
                        } else {
                            args_codes.push(arg_code);
                        }
                    }
                }
                let args_str = args_codes.join(", ");
                let call_code = format!("{}({})", c_name, args_str);
                // For external runtime, wrap string-returning user-defined functions
                if wrap_string_temps && expr.basic_type.is_string() {
                    return Ok(format!("qbs_tmp_register({})", call_code));
                } else {
                    return Ok(call_code);
                }
            }

            // Built-in functions - all args are BYVAL
            // Fixed-length string arguments need conversion to qb_string*
            let args_code: Result<Vec<_>, _> = args
                .iter()
                .map(|arg| {
                    let code = ctx.emit(arg, wrap_string_temps)?;
                    // Check if this is a fixed-length string that needs conversion
                    // This includes FieldAccess of fixed-length string fields
                    if needs_fixed_string_conversion(arg) {
                        // Check if already wrapped to avoid double wrapping
                        let inner_code = unwrap_qb_str_from_c(&code);
                        Ok(format!("qb_str_from_c({})", inner_code))
                    } else {
                        Ok(code)
                    }
                })
                .collect();
            let args_str = args_code?.join(", ");

            let call_code = format!("{}({})", c_name, args_str);
            // For external runtime, wrap string-returning function calls with qbs_tmp_register
            if wrap_string_temps && expr.basic_type.is_string() {
                Ok(format!("qbs_tmp_register({})", call_code))
            } else {
                Ok(call_code)
            }
}

/// Emits C code for array access.
pub(super) fn emit_array_access(
    ctx: &ExprEmitCtx,
    name: &str,
    indices: &[TypedExpr],
    dimensions: &[TypedArrayDimension],
) -> Result<String, CodeGenError> {
    let variable_renames = ctx.variable_renames;
    let mut c_name = c_identifier(name);

    let base_prefix = if name.contains('.') {
        let parts: Vec<&str> = name.splitn(2, '.').collect();
        if parts.len() == 2 {
            let base = parts[0];
            let field = parts[1];
            let base_c = c_identifier(base);
            let base_c = variable_renames.get(&base_c).cloned().unwrap_or(base_c);
            let field_c: String = field
                .split('.')
                .map(c_identifier)
                .collect::<Vec<_>>()
                .join(".");
            format!("{}.{}", base_c, field_c)
        } else {
            c_name.clone()
        }
    } else {
        c_name.clone()
    };

    if !name.contains('.')
        && let Some(renamed) = variable_renames.get(&c_name)
        && !renamed.ends_with("_scalar")
    {
        c_name = renamed.clone();
    }
    let final_prefix = if name.contains('.') {
        base_prefix
    } else {
        c_name
    };

    let indices_code: Result<Vec<_>, _> = indices
        .iter()
        .map(|idx| {
            let code = ctx.emit(idx, false)?;
            Ok(format!("(int64_t)({})", code))
        })
        .collect();
    let indices_code = indices_code?;

    if dimensions.is_empty() || indices_code.len() == 1 {
        if let Some(dim) = dimensions.first() {
            Ok(format!(
                "{}[{} - {}]",
                final_prefix, indices_code[0], dim.lower
            ))
        } else {
            Ok(format!("{}[{}]", final_prefix, indices_code[0]))
        }
    } else if name.contains('.') {
        let brackets: String = dimensions
            .iter()
            .zip(indices_code.iter())
            .map(|(dim, idx)| format!("[{} - {}]", idx, dim.lower))
            .collect();
        Ok(format!("{}{}", final_prefix, brackets))
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
        Ok(format!("{}[{}]", final_prefix, linear_parts.join(" + ")))
    }
}

/// Emits an external (DECLARE LIBRARY) function call with argument marshalling.
pub(super) fn emit_external_function_call(
    ctx: &ExprEmitCtx,
    c_name: &str,
    args: &[TypedExpr],
    params: &[ExternalParamInfo],
) -> Result<String, CodeGenError> {
    let mut marshalled_args = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let arg_code = ctx.emit(arg, false)?;
        let needs_marshalling = if i < params.len() {
            arg.basic_type.is_string() && !params[i].typ.is_string()
        } else {
            false
        };
        if needs_marshalling || arg.basic_type.is_string() {
            marshalled_args.push(format!("qb_string_data({})", arg_code));
        } else {
            marshalled_args.push(arg_code);
        }
    }
    Ok(format!("{}({})", c_name, marshalled_args.join(", ")))
}

/// Emits C code for UDT field access.
pub(super) fn emit_field_access(
    ctx: &ExprEmitCtx,
    object: &TypedExpr,
    field: &str,
    expr_basic_type: &BasicType,
) -> Result<String, CodeGenError> {
    let obj_code = ctx.emit(object, false)?;
    let c_field = c_identifier(field);
    let sep = if let TypedExprKind::Variable(name) = &object.kind {
        if ctx.byref_udt_names.contains(&c_identifier(name)) {
            "->"
        } else {
            "."
        }
    } else {
        "."
    };
    let field_access = format!("{}{}{}", obj_code, sep, c_field);
    if matches!(expr_basic_type, BasicType::FixedString(_)) {
        Ok(format!("qb_str_from_c({})", field_access))
    } else {
        Ok(field_access)
    }
}

/// Emits C code for array reference (passing array to procedure).
pub(super) fn emit_array_ref(name: &str) -> String {
    c_identifier(name)
}

/// Emits C code for procedure pointer (_PROCPTR).
pub(super) fn emit_proc_ptr(wrapper_name: &str) -> String {
    format!("((intptr_t)&{})", wrapper_name)
}
