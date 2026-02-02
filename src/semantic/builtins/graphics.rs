//! Graphics, window, image, and related built-in function registration.
//!
//! Registers font (_LOADFONT, _FONTHEIGHT, etc.), desktop/window (_DESKTOPWIDTH,
//! _SCREENX, _TITLE$, etc.), image (_NEWIMAGE, _LOADIMAGE, _WIDTH, _HEIGHT),
//! dialogs (_MESSAGEBOX, _INPUTBOX$, etc.), networking (_OPENHOST, etc.),
//! PMAP, POINT, colors (_RGB, _RED, etc.), and legacy (LPOS, KEY, STICK,
//! FRE, PEEK, VARPTR, FILEATTR, CVSMBF, etc.).

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers graphics-related built-in functions.
pub(super) fn register_graphics_builtins(analyzer: &mut SemanticAnalyzer) {
    // Font support
    analyzer.register_builtin_function_with_optionals(
        "_LOADFONT",
        &[
            ("file", BasicType::String, false),
            ("size", BasicType::Long, false),
            ("style", BasicType::String, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_FONTHEIGHT", &[], BasicType::Long);
    analyzer.register_builtin_function("_FONTWIDTH", &[], BasicType::Long);
    analyzer.register_builtin_function(
        "_PRINTWIDTH",
        &[("text", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_UPRINTWIDTH",
        &[("text", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_UCHARPOS",
        &[("text", BasicType::String), ("pos", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_UFONTHEIGHT",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_ULINESPACING", &[], BasicType::Long);
    analyzer.register_builtin_function("_FONT", &[("handle", BasicType::Long)], BasicType::Long);
    analyzer.register_builtin_function(
        "_FREEFONT",
        &[("handle", BasicType::Long)],
        BasicType::Long,
    );

    // Desktop/Window functions
    analyzer.register_builtin_function("_DESKTOPWIDTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_DESKTOPHEIGHT", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCREENX", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCREENY", &[], BasicType::Long);
    analyzer.register_builtin_function("_TITLE$", &[], BasicType::String);
    analyzer.register_builtin_function_with_optionals(
        "_TITLE",
        &[("title", BasicType::String, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_ICON",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_WINDOWHANDLE", &[], BasicType::Long);
    analyzer.register_builtin_function("_WINDOWHASFOCUS", &[], BasicType::Long);

    // Window control
    analyzer.register_builtin_function(
        "_SCREENMOVE",
        &[("x", BasicType::Long), ("y", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_SCREENHIDE", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCREENSHOW", &[], BasicType::Long);
    analyzer.register_builtin_function_with_optionals(
        "_FULLSCREEN",
        &[("mode", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_SCREENCLICK",
        &[
            ("x", BasicType::Long, false),
            ("y", BasicType::Long, false),
            ("button", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_SCREENPRINT",
        &[("text", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_SCREENIMAGE",
        &[
            ("x1", BasicType::Long, true),
            ("y1", BasicType::Long, true),
            ("x2", BasicType::Long, true),
            ("y2", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    // Alpha blending
    analyzer.register_builtin_function_with_optionals(
        "_BLEND",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_DONTBLEND",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_CLEARCOLOR",
        &[
            ("color", BasicType::Long, true),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    // Dialog boxes
    analyzer.register_builtin_function_with_optionals(
        "_MESSAGEBOX",
        &[
            ("title", BasicType::String, true),
            ("message", BasicType::String, true),
            ("dialogType", BasicType::String, true),
            ("iconType", BasicType::String, true),
            ("defaultButton", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_INPUTBOX$",
        &[
            ("title", BasicType::String, true),
            ("message", BasicType::String, true),
            ("defaultInput", BasicType::String, true),
        ],
        BasicType::String,
    );
    analyzer.register_builtin_function_with_optionals(
        "_OPENFILEDIALOG$",
        &[
            ("title", BasicType::String, false),
            ("filter", BasicType::String, false),
            ("defaultDir", BasicType::String, true),
            ("defaultFile", BasicType::String, true),
            ("flags", BasicType::Long, true),
        ],
        BasicType::String,
    );
    analyzer.register_builtin_function_with_optionals(
        "_SAVEFILEDIALOG$",
        &[
            ("title", BasicType::String, false),
            ("filter", BasicType::String, false),
            ("defaultDir", BasicType::String, true),
            ("defaultFile", BasicType::String, true),
        ],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "_SELECTFOLDERDIALOG$",
        &[("title", BasicType::String)],
        BasicType::String,
    );

    // Networking
    analyzer.register_builtin_function(
        "_OPENHOST",
        &[("info", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_OPENCONNECTION",
        &[("host_handle", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_OPENCLIENT",
        &[("connection_string", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_CONNECTED",
        &[("handle", BasicType::Long)],
        BasicType::Integer,
    );
    // _STATUSCODE(handle) for network HTTP status; optional handle so _STATUSCODE() (no args) also resolves.
    analyzer.register_builtin_function_with_optionals(
        "_STATUSCODE",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );

    // Image buffer functions
    analyzer.register_builtin_function(
        "_NEWIMAGE",
        &[
            ("width", BasicType::Long),
            ("height", BasicType::Long),
            ("mode", BasicType::Long),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_LOADIMAGE",
        &[("filename", BasicType::String), ("mode", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_COPYIMAGE",
        &[("source", BasicType::Long), ("mode", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_WIDTH",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_HEIGHT",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );

    // Coordinate mapping and POINT
    analyzer.register_builtin_function(
        "PMAP",
        &[
            ("coordinate", BasicType::Double),
            ("function_code", BasicType::Long),
        ],
        BasicType::Double,
    );
    analyzer.register_builtin_function_with_optionals(
        "POINT",
        &[
            ("x_or_function", BasicType::Long, false),
            ("y", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    // Legacy (LPOS, KEY, STICK, STRIG, FRE, PEEK, INP, PEN, ERDEV, IOCTL$, VARPTR, SADD, FILEATTR, MBF)
    analyzer.register_builtin_function("LPOS", &[("n", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function("KEY", &[("n", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function("STICK", &[("n", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function_with_optionals(
        "STRIG",
        &[
            ("n", BasicType::Long, false),
            ("controller", BasicType::Long, true),
        ],
        BasicType::Integer,
    );
    analyzer.register_builtin_function("FRE", &[("n", BasicType::Long)], BasicType::Long);
    analyzer.register_builtin_function("PEEK", &[("address", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function("INP", &[("port", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function("PEN", &[("n", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function("ERDEV", &[], BasicType::Integer);
    analyzer.register_builtin_function("ERDEV$", &[], BasicType::String);
    analyzer.register_builtin_function(
        "IOCTL$",
        &[("filenum", BasicType::Long)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "VARPTR",
        &[("variable", BasicType::Unknown)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "VARPTR$",
        &[("variable", BasicType::Unknown)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "VARSEG",
        &[("variable", BasicType::Unknown)],
        BasicType::Long,
    );
    analyzer.register_builtin_function("SADD", &[("s", BasicType::String)], BasicType::Long);
    analyzer.register_builtin_function(
        "FILEATTR",
        &[
            ("filenum", BasicType::Integer),
            ("attribute", BasicType::Integer),
        ],
        BasicType::Integer,
    );
    analyzer.register_builtin_function("CVSMBF", &[("s", BasicType::String)], BasicType::Single);
    analyzer.register_builtin_function("CVDMBF", &[("s", BasicType::String)], BasicType::Double);
    analyzer.register_builtin_function("MKSMBF$", &[("n", BasicType::Single)], BasicType::String);
    analyzer.register_builtin_function("MKDMBF$", &[("n", BasicType::Double)], BasicType::String);

    // Color creation and extraction
    analyzer.register_builtin_function_with_optionals(
        "_RGB",
        &[
            ("red", BasicType::Long, false),
            ("green", BasicType::Long, false),
            ("blue", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_RGB32",
        &[
            ("red", BasicType::Long, false),
            ("green", BasicType::Long, false),
            ("blue", BasicType::Long, false),
            ("alpha", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_RGBA",
        &[
            ("red", BasicType::Long, false),
            ("green", BasicType::Long, false),
            ("blue", BasicType::Long, false),
            ("alpha", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_RGBA32",
        &[
            ("red", BasicType::Long),
            ("green", BasicType::Long),
            ("blue", BasicType::Long),
            ("alpha", BasicType::Long),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_RED",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_GREEN",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_BLUE",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_ALPHA",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_RED32",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_GREEN32",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_BLUE32",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_ALPHA32",
        &[
            ("color", BasicType::Long, false),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    analyzer.register_builtin_function_with_optionals(
        "_PIXELSIZE",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_PALETTECOLOR",
        &[
            ("attribute", BasicType::Long, false),
            ("color_or_handle", BasicType::Long, true),
            ("handle", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    analyzer.register_builtin_function("_SCREENEXISTS", &[], BasicType::Integer);
    analyzer.register_builtin_function_with_optionals(
        "_EXIT",
        &[("code", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_DEFAULTCOLOR", &[], BasicType::Long);
    analyzer.register_builtin_function("_BACKGROUNDCOLOR", &[], BasicType::Long);
    analyzer.register_builtin_function(
        "_FULLPATH$",
        &[("path", BasicType::String)],
        BasicType::String,
    );
    analyzer.register_builtin_function("_FPS", &[], BasicType::Double);
}
