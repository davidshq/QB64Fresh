//! SUB (CALL) statement code generation.
//!
//! Emits C code for BASIC SUB procedure calls, including byref argument handling,
//! built-in SUB name mapping, and OpenGL call wrapping.

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedExpr, TypedExprKind, TypedParameter};
use crate::writeln_code;

use super::StmtEmitter;
use crate::codegen::c_backend::expr::c_function_name;
use crate::codegen::c_backend::types::{c_identifier, c_type};

/// Emits code for a CALL (SUB) statement.
///
/// Generates argument list with `&` for byref parameters, handles built-in SUB
/// name mapping (e.g. _ICON, _PALETTECOLOR), and wraps OpenGL calls in
/// `#ifdef QB64FRESH_OPENGL`.
pub(super) fn emit_call_stmt(
    emitter: &mut StmtEmitter,
    name: &str,
    args: &[TypedExpr],
    params: &[TypedParameter],
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    let mut args_codes = Vec::new();
    let mut temp_decls = Vec::new();
    let mut temp_counter = 0;

    for (i, arg) in args.iter().enumerate() {
        let arg_code = emitter.emit_expr(arg)?;
        let arg_is_udt_pointer = matches!(&arg.kind, TypedExprKind::Variable(n)
            if {
                let c_arg = c_identifier(n).to_lowercase();
                emitter.procedure.current_func_byref_udt_names.iter()
                    .any(|s| s.to_lowercase() == c_arg)
            });
        let is_byref = !arg_is_udt_pointer
            && params
                .get(i)
                .map(|p| !p.by_val && !p.is_array)
                .unwrap_or(false);
        if is_byref {
            let is_builtin_const = matches!(&arg.kind, TypedExprKind::Variable(n)
                if n.starts_with('_') && n.chars().all(|c| c.is_uppercase() || c == '_'));

            let is_lvalue = !is_builtin_const
                && matches!(
                    arg.kind,
                    TypedExprKind::Variable { .. }
                        | TypedExprKind::ArrayAccess { .. }
                        | TypedExprKind::FieldAccess { .. }
                );

            let param_type = params.get(i).map(|p| &p.basic_type);
            let types_match =
                param_type.map(|pt| pt == &arg.basic_type).unwrap_or(true);

            if is_lvalue && types_match {
                let needs_parens = arg_code.contains(' ')
                    || arg_code.contains('(')
                    || arg_code.contains('[');

                let is_likely_const = !needs_parens
                    && arg_code
                        .chars()
                        .all(|c| c.is_uppercase() || c == '_' || c.is_ascii_digit())
                    && (arg_code.starts_with("HASHFLAG_")
                        || arg_code.starts_with("DEPENDENCY_")
                        || arg_code.contains("_FLAG")
                        || arg_code.contains("_DEPENDENCY"));

                if is_likely_const {
                    let c_ty = param_type
                        .map(c_type)
                        .unwrap_or_else(|| "int32_t".to_string());
                    args_codes.push(format!("({}*)&{}", c_ty, arg_code));
                } else {
                    args_codes.push(format!("&({})", arg_code));
                }
            } else if is_lvalue && !types_match {
                let c_ty = param_type
                    .map(c_type)
                    .unwrap_or_else(|| "int32_t".to_string());
                let temp_name = format!("_tmp_arg_{}", temp_counter);
                temp_counter += 1;
                temp_decls.push(format!(
                    "{} {} = ({})({});",
                    c_ty, temp_name, c_ty, arg_code
                ));
                args_codes.push(format!("&{}", temp_name));
            } else {
                let param_type = params.get(i).map(|p| &p.basic_type);
                let c_ty = param_type
                    .map(c_type)
                    .unwrap_or_else(|| "int32_t".to_string());
                let temp_name = format!("_tmp_arg_{}", temp_counter);
                temp_counter += 1;
                temp_decls.push(format!("{} {} = {};", c_ty, temp_name, arg_code));
                args_codes.push(format!("&{}", temp_name));
            }
        } else {
            args_codes.push(arg_code);
        }
    }
    let args_str = args_codes.join(", ");
    let upper_name = name.to_uppercase();

    let c_name = match upper_name.as_str() {
        "_ICON" => match args.len() {
            0 => "qb_icon".to_string(),
            1 => "qb_icon1".to_string(),
            _ => "qb_icon2".to_string(),
        },
        "_ACCEPTFILEDROP" => match args.len() {
            0 => "qb_acceptfiledrop".to_string(),
            _ => "qb_acceptfiledrop1".to_string(),
        },
        "_WRITEFILE" => "qb_writefile".to_string(),
        "_EXIT" => "qb_exit".to_string(),
        "_FINISHDROP" => "qb_finishdrop".to_string(),
        "EVNT" => {
            // Poll events and exit if window is closed (qb_gfx_poll_events returns 0 on close).
            // This matches QB64pe behavior where closing the window terminates the program.
            writeln_code!(output, "{}if (qb_gfx_poll_events() == 0) exit(0);", indent)?;
            writeln_code!(output, "{}qb_sleep(0.01);", indent)?;
            return Ok(());
        }
        "_CONSOLECURSOR" => "qb_consolecursor".to_string(),
        "_CONSOLEFONT" => "qb_consolefont".to_string(),
        "_CONTROLCHR" => "qb_controlchr".to_string(),
        "_SETALPHA" => "qb_setalpha".to_string(),
        "_PALETTECOLOR" => {
            if args.len() == 2 {
                let full_args = format!("{}, 0", args_str);
                writeln_code!(output, "{}qb_palettecolor({});", indent, full_args)?;
                return Ok(());
            }
            "qb_palettecolor".to_string()
        }
        "_COPYPALETTE" => "qb_copypalette".to_string(),
        "_BLEND" => "qb_blend".to_string(),
        "_DONTBLEND" => "qb_dontblend".to_string(),
        "_CLEARCOLOR" => "qb_clearcolor".to_string(),
        "_DEPTHBUFFER" => "qb_depthbuffer".to_string(),
        "_DISPLAYORDER" => "qb_displayorder".to_string(),
        "_SNDLIMIT" => "qb_sndlimit".to_string(),
        "_HIDE" => "qb_hide".to_string(),
        "_SHOW" => "qb_show".to_string(),
        "_ONTOP" => "qb_ontop".to_string(),
        "_PRINTMODE" => "qb_printmode".to_string(),
        "_SAVEIMAGE" => "qb_saveimage".to_string(),
        "_SCREENPRINT" => "qb_screenprint".to_string(),
        "_UPRINTSTRING" => "qb_uprintstring".to_string(),
        "_MAPUNICODE" => "qb_mapunicode".to_string(),
        "_LOGTRACE" => "qb_logtrace".to_string(),
        "_LOGINFO" => "qb_loginfo".to_string(),
        "_LOGWARN" => "qb_logwarn".to_string(),
        "_LOGERROR" => "qb_logerror".to_string(),
        "_LOGMINLEVEL" => "qb_logminlevel".to_string(),
        "_SNDRAWBATCH" => "qb_sndrawbatch".to_string(),
        "_MIDISOUNDBANK" => "qb_midisoundbank".to_string(),
        "_NEWHANDLER" => "qb_newhandler".to_string(),
        "_PRINTIMAGE" => "qb_printimage".to_string(),
        "_CLEAR" => "qb_clear_resource".to_string(),
        "_TOGGLE" => "qb_toggle".to_string(),
        "_MAPTRIANGLE" => "qb_maptriangle".to_string(),
        "_GLRENDER" => "qb_glrender".to_string(),
        _ if (upper_name.starts_with("_GL") || upper_name.starts_with("_GLU")) => {
            c_function_name(name)
        }
        "_ECHO" => "qb_echo".to_string(),
        "_CONSOLETITLE" => "qb_consoletitle".to_string(),
        "_CLIPBOARD" => "qb_clipboard_set".to_string(),
        "_ANTIALIASING" => "qb_antialiasing".to_string(),
        "_ALLOWFULLSCREEN" => "qb_allowfullscreen".to_string(),
        "_FULLSCREEN" => "qb_fullscreen".to_string(),
        "_SCREENMOVE" => "qb_screenmove".to_string(),
        "_SCREENSHOW" => "qb_screenshow".to_string(),
        "_SCREENHIDE" => "qb_screenhide".to_string(),
        "_SCREENCLICK" => "qb_screenclick".to_string(),
        "_SCREENIMAGE" => "qb_screenimage".to_string(),
        "_DELAY" => "qb_delay".to_string(),
        "_MEMPUT" => "qb_memput".to_string(),
        "_MEMFILL" => "qb_memfill".to_string(),
        "_MEMCOPY" => "qb_memcopy".to_string(),
        "_MEMFREE" => "qb_memfree".to_string(),
        "_SCREENICON" => "qb_screenicon".to_string(),
        "_KEYDOWN" => {
            if args.len() == 1 {
                writeln_code!(
                    output,
                    "{}qb_keydown_vk((uint32_t){});",
                    indent,
                    args_codes.first().cloned().unwrap_or_default()
                )?;
                return Ok(());
            }
            "qb_keydown_vk".to_string()
        }
        "_KEYUP" => {
            if args.len() == 1 {
                writeln_code!(
                    output,
                    "{}qb_keyup_vk((uint32_t){});",
                    indent,
                    args_codes.first().cloned().unwrap_or_default()
                )?;
                return Ok(());
            }
            "qb_keyup_vk".to_string()
        }
        "_NOTIFYPOPUP" => {
            let a0 = if args.is_empty() {
                "NULL".to_string()
            } else {
                let c = emitter.emit_expr(&args[0])?;
                format!("qb_string_data({})", c)
            };
            let a1 = if args.len() < 2 {
                "NULL".to_string()
            } else {
                let c = emitter.emit_expr(&args[1])?;
                format!("qb_string_data({})", c)
            };
            let a2 = if args.len() < 3 {
                "NULL".to_string()
            } else {
                let c = emitter.emit_expr(&args[2])?;
                format!("qb_string_data({})", c)
            };
            writeln_code!(output, "{}qb_notifypopup({}, {}, {});", indent, a0, a1, a2)?;
            return Ok(());
        }
        _ => format!("qb_sub_{}", c_identifier(name).to_lowercase()),
    };

    let is_opengl_call = c_name.starts_with("call_gl") && emitter.uses_opengl;
    if is_opengl_call {
        writeln_code!(output, "#ifdef QB64FRESH_OPENGL")?;
    }
    if temp_decls.is_empty() {
        writeln_code!(output, "{}{}({});", indent, c_name, args_str)?;
    } else {
        writeln_code!(output, "{}{{ ", indent)?;
        for decl in temp_decls {
            writeln_code!(output, "{}    {}", indent, decl)?;
        }
        writeln_code!(output, "{}    {}({});", indent, c_name, args_str)?;
        writeln_code!(output, "{}}}", indent)?;
    }
    if is_opengl_call {
        writeln_code!(output, "#endif")?;
    }
    Ok(())
}
