//! Built-in SUB (statement) registration.
//!
//! Registers _ACCEPTFILEDROP, _CONSOLECURSOR, _KEYUP, _SETALPHA, _BLEND,
//! _ICON, _HIDE, _PRINTMODE, _NOTIFYPOPUP, _CLIPBOARD, _DELAY, _MEMPUT,
//! _LOGTRACE, _SAVEIMAGE, _GLRENDER, _CLEAR, _SNDRAWBATCH, _NEWHANDLER, etc.

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers built-in SUBs (statements with no return value).
pub(super) fn register_builtin_subs(analyzer: &mut SemanticAnalyzer) {
    analyzer.register_builtin_sub_with_optionals(
        "_ACCEPTFILEDROP",
        &[("enable", BasicType::Integer, true)],
    );
    analyzer.register_builtin_sub("_FINISHDROP", &[]);
    // evnt - process window/input events (IDE event loop; maps to qb_gfx_poll_events).
    analyzer.register_builtin_sub("evnt", &[]);

    analyzer.register_builtin_sub("_CONSOLECURSOR", &[("visible", BasicType::Integer)]);
    analyzer.register_builtin_sub(
        "_CONSOLEFONT",
        &[("font", BasicType::String), ("size", BasicType::Long)],
    );
    analyzer.register_builtin_sub("_CONTROLCHR", &[("mode", BasicType::Integer)]);

    analyzer.register_builtin_sub("_KEYUP", &[("code", BasicType::Long)]);

    analyzer.register_builtin_sub(
        "_SETALPHA",
        &[
            ("alpha", BasicType::Long),
            ("color1", BasicType::Long),
            ("color2", BasicType::Long),
        ],
    );
    analyzer.register_builtin_sub(
        "_COPYPALETTE",
        &[
            ("srcHandle", BasicType::Long),
            ("destHandle", BasicType::Long),
        ],
    );
    analyzer.register_builtin_sub("_BLEND", &[("handle", BasicType::Long)]);
    analyzer.register_builtin_sub("_DONTBLEND", &[("handle", BasicType::Long)]);
    analyzer.register_builtin_sub(
        "_CLEARCOLOR",
        &[("color", BasicType::Long), ("handle", BasicType::Long)],
    );
    analyzer.register_builtin_sub("_DEPTHBUFFER", &[("mode", BasicType::Integer)]);
    analyzer.register_builtin_sub(
        "_DISPLAYORDER",
        &[
            ("layer1", BasicType::Long),
            ("layer2", BasicType::Long),
            ("layer3", BasicType::Long),
            ("layer4", BasicType::Long),
        ],
    );

    analyzer.register_builtin_sub(
        "_MAPTRIANGLE",
        &[
            ("sx1", BasicType::Single),
            ("sy1", BasicType::Single),
            ("sx2", BasicType::Single),
            ("sy2", BasicType::Single),
            ("sx3", BasicType::Single),
            ("sy3", BasicType::Single),
            ("dx1", BasicType::Single),
            ("dy1", BasicType::Single),
            ("dx2", BasicType::Single),
            ("dy2", BasicType::Single),
            ("dx3", BasicType::Single),
            ("dy3", BasicType::Single),
        ],
    );

    analyzer.register_builtin_sub(
        "_SNDLIMIT",
        &[("handle", BasicType::Long), ("seconds", BasicType::Single)],
    );

    analyzer.register_builtin_sub("_ICON", &[("handle", BasicType::Long)]);

    analyzer.register_builtin_sub("_HIDE", &[]);
    analyzer.register_builtin_sub("_SHOW", &[]);
    analyzer.register_builtin_sub("_ONTOP", &[("mode", BasicType::Integer)]);

    analyzer.register_builtin_sub("_PRINTMODE", &[("mode", BasicType::Integer)]);

    analyzer.register_builtin_sub(
        "_UPRINTSTRING",
        &[
            ("x", BasicType::Long),
            ("y", BasicType::Long),
            ("text", BasicType::String),
        ],
    );

    analyzer.register_builtin_sub_with_optionals(
        "_NOTIFYPOPUP",
        &[
            ("title", BasicType::String, true),
            ("message", BasicType::String, true),
            ("iconType", BasicType::String, true),
        ],
    );
    analyzer.register_builtin_sub("_ECHO", &[("text", BasicType::String)]);
    analyzer.register_builtin_sub("_CONSOLETITLE", &[("title", BasicType::String)]);

    analyzer.register_builtin_sub("_CLIPBOARD", &[("text", BasicType::String)]);

    analyzer.register_builtin_sub("_DELAY", &[("seconds", BasicType::Single)]);

    analyzer.register_builtin_sub(
        "_MEMPUT",
        &[
            ("block", BasicType::Offset),
            ("offset", BasicType::Offset),
            ("value", BasicType::Long),
        ],
    );
    analyzer.register_builtin_sub(
        "_MEMFILL",
        &[
            ("block", BasicType::Offset),
            ("offset", BasicType::Offset),
            ("size", BasicType::Long),
            ("value", BasicType::Long),
        ],
    );
    analyzer.register_builtin_sub(
        "_MEMCOPY",
        &[
            ("src", BasicType::Offset),
            ("srcoff", BasicType::Offset),
            ("bytes", BasicType::Long),
            ("dst", BasicType::Offset),
            ("dstoff", BasicType::Offset),
        ],
    );
    analyzer.register_builtin_sub("_MEMFREE", &[("block", BasicType::Offset)]);

    analyzer.register_builtin_sub("_SCREENICON", &[]);

    analyzer.register_builtin_sub("_LOGTRACE", &[("message", BasicType::String)]);
    analyzer.register_builtin_sub("_LOGINFO", &[("message", BasicType::String)]);
    analyzer.register_builtin_sub("_LOGWARN", &[("message", BasicType::String)]);
    analyzer.register_builtin_sub("_LOGERROR", &[("message", BasicType::String)]);
    analyzer.register_builtin_sub("_LOGMINLEVEL", &[("level", BasicType::Long)]);

    analyzer.register_builtin_sub(
        "_SAVEIMAGE",
        &[("filename", BasicType::String), ("handle", BasicType::Long)],
    );
    analyzer.register_builtin_sub("_SCREENPRINT", &[("text", BasicType::String)]);
    analyzer.register_builtin_sub("_PRINTIMAGE", &[("handle", BasicType::Long)]);
    analyzer.register_builtin_sub("_GLRENDER", &[("mode", BasicType::Long)]);

    analyzer.register_builtin_sub("_CLEAR", &[("handle", BasicType::Long)]);

    analyzer.register_builtin_sub("_TOGGLE", &[("handle", BasicType::Long)]);

    analyzer.register_builtin_sub(
        "_SNDRAWBATCH",
        &[
            ("handle", BasicType::Long),
            ("data", BasicType::Long),
            ("frames", BasicType::Long),
        ],
    );
    analyzer.register_builtin_sub("_MIDISOUNDBANK", &[("filename", BasicType::String)]);

    analyzer.register_builtin_sub("_NEWHANDLER", &[("handler", BasicType::Long)]);
}
