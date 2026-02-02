//! Extended QB64 built-in function registration.
//!
//! Registers hash/encoding (_CRC32, _MD5$, _BASE64ENCODE$, etc.), memory
//! extended (_MEMEXISTS, _MEMELEMENT, etc.), device input (_DEVICES,
//! _AXIS, _BUTTON, etc.), drag/drop, resize, HSB colors, networking,
//! graphics constants (_SMOOTH, _HARDWARE, etc.), date/time (_YEAR,
//! _MONTH, etc.), and then calls register_builtin_subs.

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

use super::subs;

/// Registers extended QB64 built-in functions, then built-in SUBs.
pub(super) fn register_extended_builtins(analyzer: &mut SemanticAnalyzer) {
    // Hash and encoding
    analyzer.register_builtin_function("_CRC32", &[("data", BasicType::String)], BasicType::Long);
    analyzer.register_builtin_function("_MD5$", &[("data", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("_ADLER32", &[("data", BasicType::String)], BasicType::Long);

    analyzer.register_builtin_function(
        "_BASE64ENCODE$",
        &[("data", BasicType::String)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "_BASE64DECODE$",
        &[("data", BasicType::String)],
        BasicType::String,
    );

    analyzer.register_builtin_function(
        "_ENCODEURL$",
        &[("url", BasicType::String)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "_DECODEURL$",
        &[("url", BasicType::String)],
        BasicType::String,
    );

    analyzer.register_builtin_function(
        "_DEFLATE$",
        &[("data", BasicType::String)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "_INFLATE$",
        &[("data", BasicType::String)],
        BasicType::String,
    );

    // Memory extended
    analyzer.register_builtin_function(
        "_MEMEXISTS",
        &[("mem", BasicType::Mem)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function(
        "_MEMELEMENT",
        &[("mem", BasicType::Mem), ("index", BasicType::Offset)],
        BasicType::Mem,
    );
    analyzer.register_builtin_function("_MEMIMAGE", &[("handle", BasicType::Long)], BasicType::Mem);
    analyzer.register_builtin_function("_MEMSOUND", &[("handle", BasicType::Long)], BasicType::Mem);

    // Default color overloads
    analyzer.register_builtin_function_with_optionals(
        "_DEFAULTCOLOR",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "_BACKGROUNDCOLOR",
        &[("handle", BasicType::Long, true)],
        BasicType::Long,
    );

    // Short-circuit logical operators
    analyzer.register_builtin_function(
        "_ANDALSO",
        &[("a", BasicType::Long), ("b", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_ORELSE",
        &[("a", BasicType::Long), ("b", BasicType::Long)],
        BasicType::Long,
    );

    analyzer.register_builtin_function("_FREETIMER", &[], BasicType::Long);

    analyzer.register_builtin_function("_CONSOLEINPUT", &[], BasicType::Long);
    analyzer.register_builtin_function("_ECHO", &[("text", BasicType::String)], BasicType::Long);

    analyzer.register_builtin_function("_MOUSEHIDDEN", &[], BasicType::Integer);

    analyzer.register_builtin_function("_CLIPBOARDIMAGE", &[], BasicType::Long);

    // Device input (gamepad/joystick)
    analyzer.register_builtin_function("_DEVICES", &[], BasicType::Long);
    analyzer.register_builtin_function("_DEVICE$", &[("n", BasicType::Long)], BasicType::String);
    analyzer.register_builtin_function("_DEVICEINPUT", &[], BasicType::Long);
    analyzer.register_builtin_function(
        "_LASTAXIS",
        &[("device", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_LASTBUTTON",
        &[("device", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_LASTWHEEL",
        &[("device", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_AXIS",
        &[("device", BasicType::Long), ("axis", BasicType::Long)],
        BasicType::Single,
    );
    analyzer.register_builtin_function(
        "_BUTTON",
        &[("device", BasicType::Long), ("button", BasicType::Long)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function(
        "_BUTTONCHANGE",
        &[("device", BasicType::Long), ("button", BasicType::Long)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function(
        "_WHEEL",
        &[("device", BasicType::Long), ("wheel", BasicType::Long)],
        BasicType::Single,
    );

    // Drag and drop
    analyzer.register_builtin_function("_TOTALDROPPEDFILES", &[], BasicType::Long);
    analyzer.register_builtin_function(
        "_DROPPEDFILE",
        &[("index", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_DROPPEDFILE$",
        &[("index", BasicType::Long)],
        BasicType::String,
    );

    // Resize
    analyzer.register_builtin_function("_RESIZE", &[], BasicType::Integer);
    analyzer.register_builtin_function("_RESIZEWIDTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_RESIZEHEIGHT", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCALEDWIDTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCALEDHEIGHT", &[], BasicType::Long);

    analyzer.register_builtin_function_with_optionals(
        "_COLORCHOOSERDIALOG",
        &[
            ("title", BasicType::String, true),
            ("defaultRGB", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    // HSB colors
    analyzer.register_builtin_function(
        "_HSB32",
        &[
            ("hue", BasicType::Single),
            ("saturation", BasicType::Single),
            ("brightness", BasicType::Single),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_HSBA32",
        &[
            ("hue", BasicType::Single),
            ("saturation", BasicType::Single),
            ("brightness", BasicType::Single),
            ("alpha", BasicType::Single),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_HUE32", &[("color", BasicType::Long)], BasicType::Single);
    analyzer.register_builtin_function(
        "_SATURATION32",
        &[("color", BasicType::Long)],
        BasicType::Single,
    );
    analyzer.register_builtin_function(
        "_BRIGHTNESS32",
        &[("color", BasicType::Long)],
        BasicType::Single,
    );

    // Networking
    analyzer.register_builtin_function(
        "_CONNECTIONADDRESS",
        &[("handle", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_CONNECTIONADDRESS$",
        &[("handle", BasicType::Long)],
        BasicType::String,
    );

    // File I/O extended
    analyzer.register_builtin_function(
        "_FILES$",
        &[("spec", BasicType::String)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "_EMBEDDED$",
        &[("name", BasicType::String)],
        BasicType::String,
    );

    analyzer.register_builtin_function("_LASTHANDLER", &[], BasicType::Long);

    analyzer.register_builtin_function(
        "_SNDNEW",
        &[
            ("frames", BasicType::Long),
            ("channels", BasicType::Long),
            ("bits", BasicType::Long),
        ],
        BasicType::Long,
    );

    analyzer.register_builtin_function("_INCLERRORFILE$", &[], BasicType::String);
    analyzer.register_builtin_function("_INCLERRORLINE", &[], BasicType::Long);
    // _STATUSCODE: registered in graphics.rs with optional handle (covers both no-arg and (handle) uses).

    analyzer.register_builtin_function_with_optionals(
        "_MAPUNICODE",
        &[
            ("unicode_or_ascii", BasicType::Long, false),
            ("ascii", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    // Unicode font: _UCHARPOS, _UFONTHEIGHT, _UPRINTWIDTH are registered in graphics.rs (font section).

    // Graphics mode constants
    analyzer.register_builtin_function("_SMOOTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_SMOOTHSHRUNK", &[], BasicType::Long);
    analyzer.register_builtin_function("_SMOOTHSTRETCHED", &[], BasicType::Long);
    analyzer.register_builtin_function("_HARDWARE", &[], BasicType::Long);
    analyzer.register_builtin_function("_HARDWARE1", &[], BasicType::Long);
    analyzer.register_builtin_function("_SOFTWARE", &[], BasicType::Long);
    analyzer.register_builtin_function("_STRETCH", &[], BasicType::Long);
    analyzer.register_builtin_function("_SEAMLESS", &[], BasicType::Long);
    analyzer.register_builtin_function("_SQUAREPIXELS", &[], BasicType::Long);
    analyzer.register_builtin_function("_BEHIND", &[], BasicType::Long);

    analyzer.register_builtin_function("_ANTICLOCKWISE", &[], BasicType::Long);
    analyzer.register_builtin_function("_CLOCKWISE", &[], BasicType::Long);

    analyzer.register_builtin_function("_KEEPBACKGROUND", &[], BasicType::Long);
    analyzer.register_builtin_function("_FILLBACKGROUND", &[], BasicType::Long);
    analyzer.register_builtin_function("_ONLYBACKGROUND", &[], BasicType::Long);

    analyzer.register_builtin_function("_MIDDLE", &[], BasicType::Long);
    analyzer.register_builtin_function("_AUTO", &[], BasicType::Long);

    analyzer.register_builtin_function("_ALL", &[], BasicType::Long);
    analyzer.register_builtin_function("_BLINK", &[], BasicType::Long);
    analyzer.register_builtin_function("_OFF", &[], BasicType::Long);
    analyzer.register_builtin_function("_ONLY", &[], BasicType::Long);

    analyzer.register_builtin_function("_WAVE", &[], BasicType::Long);
    analyzer.register_builtin_function("_DONTWAIT", &[], BasicType::Long);

    analyzer.register_builtin_function("_CONSOLETITLE$", &[], BasicType::String);
    analyzer.register_builtin_function("_SCREENBUFFER", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCINKEY$", &[], BasicType::String);

    analyzer.register_builtin_function("_ASSERTERROR$", &[], BasicType::String);
    analyzer.register_builtin_function("_GLCOMPAT", &[], BasicType::Long);

    analyzer.register_builtin_function("_FULLSCREENSMOOTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_DISPLAYWIDTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_DISPLAYHEIGHT", &[], BasicType::Long);

    analyzer.register_builtin_function("_YEAR", &[], BasicType::Long);
    analyzer.register_builtin_function("_MONTH", &[], BasicType::Long);
    analyzer.register_builtin_function("_DAY", &[], BasicType::Long);
    analyzer.register_builtin_function("_HOUR", &[], BasicType::Long);
    analyzer.register_builtin_function("_MINUTE", &[], BasicType::Long);
    analyzer.register_builtin_function("_SECOND", &[], BasicType::Long);
    analyzer.register_builtin_function("_WEEKDAY", &[], BasicType::Long);

    // Register built-in SUBs
    subs::register_builtin_subs(analyzer);
}
