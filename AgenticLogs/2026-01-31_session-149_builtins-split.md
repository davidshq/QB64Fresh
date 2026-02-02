# Session 149: builtins.rs file splitting (FILE_SPLITTING_ANALYSIS)

**Date:** 2026-01-31

## Goal

Split `src/semantic/builtins.rs` (~2,271 lines) by category per `docs/ThingsToDo/FILE_SPLITTING_ANALYSIS.md` §4: `builtins/string.rs`, `builtins/math.rs`, `builtins/io.rs`, `builtins/graphics.rs`, etc., with `builtins/mod.rs` aggregating and re-exporting.

## What was done

- **builtins/mod.rs** — Orchestration (`register_builtins()` calling category modules), helpers (`register_builtin_function`, `register_builtin_function_with_optionals`, `register_builtin_sub`, `register_builtin_sub_with_optionals`, `register_builtin_constant`), and all constant registration (_TRUE/_FALSE/_NONE, character constants, error constants, platform constants, keyboard constants).
- **builtins/string.rs** — String and conversion: LEN, CHR$, ASC, LEFT$/RIGHT$/MID$, INSTR, UCASE$/LCASE$/LTRIM$/RTRIM$/TRIM$, STR$/VAL, STRING$/SPACE$, _STRCMP/_STRICMP, _INSTRREV/_TRIM$, MKI$/MKL$/MKS$/MKD$, CVI/CVL/CVS/CVD, HEX$/OCT$/_BIN$/_TOSTR$.
- **builtins/math.rs** — Math: ABS, SGN, INT, FIX, CINT/CLNG/CSNG/CDBL, SQR, LOG, EXP, SIN/COS/TAN/ATN, RND, _PI, _ASIN/_ACOS/_ATAN2/_HYPOT, _CEIL/_ROUND/_MIN/_MAX/_CLAMP, hyperbolic and reciprocal trig, angle conversions (_D2R/_R2D, etc.), _NEGATE.
- **builtins/misc.rs** — Bitwise (_SHL/_SHR/_ROL/_ROR, _READBIT/_SETBIT/_RESETBIT/_TOGGLEBIT), _IIF/_IIF$, LBOUND/UBOUND, memory (_MEMNEW/_MEMFREE/_MEMGET/_MEMPUT/_MEMCOPY/_MEMFILL/_OFFSET/_MEM), system (_FILEEXISTS/_DIREXISTS/_DIR$, SHELL/_SHELLHIDE, _READFILE$/_WRITEFILE), mouse (_MOUSEX/_MOUSEY/_MOUSEBUTTON, etc.), _CLIPBOARD$.
- **builtins/io.rs** — I/O and environment: TIMER, DATE$/TIME$, TAB/SPC/POS/CSRLIN, SCREEN, EOF/LOF/LOC/SEEK/FREEFILE, INKEY$/INPUT$, _KEYHIT/_KEYDOWN/_CINP, _CAPSLOCK/_NUMLOCK/_SCROLLLOCK, ERR/ERL/_ERRORLINE/_ERRORMESSAGE$, _COMMANDCOUNT/_ENVIRONCOUNT, ENVIRON$/COMMAND$, _CWD$/_OS$/_STARTDIR$, Version$, _DATE$/_TIME$.
- **builtins/graphics.rs** — Font (_LOADFONT/_FONTHEIGHT/_FONTWIDTH/_PRINTWIDTH, _UPRINTWIDTH/_UCHARPOS/_UFONTHEIGHT/_ULINESPACING/_FONT/_FREEFONT), desktop/window (_DESKTOPWIDTH/_DESKTOPHEIGHT/_SCREENX/_SCREENY/_TITLE$/_TITLE/_ICON/_WINDOWHANDLE/_WINDOWHASFOCUS, _SCREENMOVE/_SCREENHIDE/_SCREENSHOW/_FULLSCREEN/_SCREENCLICK/_SCREENPRINT/_SCREENIMAGE), alpha (_BLEND/_DONTBLEND/_CLEARCOLOR), dialogs (_MESSAGEBOX/_INPUTBOX$/_OPENFILEDIALOG$/_SAVEFILEDIALOG$/_SELECTFOLDERDIALOG$), networking (_OPENHOST/_OPENCONNECTION/_OPENCLIENT/_CONNECTED/_STATUSCODE), image (_NEWIMAGE/_LOADIMAGE/_COPYIMAGE/_WIDTH/_HEIGHT), PMAP/POINT, legacy (LPOS/KEY/STICK/STRIG/FRE/PEEK/INP/PEN/ERDEV/IOCTL$/VARPTR/VARSEG/SADD/FILEATTR, CVSMBF/CVDMBF/MKSMBF$/MKDMBF$), colors (_RGB/_RGB32/_RGBA/_RGBA32, _RED/_GREEN/_BLUE/_ALPHA and 32 variants, _PIXELSIZE/_PALETTECOLOR), _SCREENEXISTS/_EXIT/_DEFAULTCOLOR/_BACKGROUNDCOLOR/_FULLPATH$/_FPS.
- **builtins/audio.rs** — _SNDOPEN/_SNDOPENRAW/_SNDCOPY/_SNDPLAYING/_SNDPAUSED/_SNDGETPOS/_SNDLEN/_SNDRATE/_SNDRAWLEN/_SNDRAWDONE.
- **builtins/extended.rs** — Hash/encoding (_CRC32/_MD5$/_ADLER32, _BASE64ENCODE$/_BASE64DECODE$, _ENCODEURL$/_DECODEURL$, _DEFLATE$/_INFLATE$), memory extended (_MEMEXISTS/_MEMELEMENT/_MEMIMAGE/_MEMSOUND), default color overloads, _ANDALSO/_ORELSE, _FREETIMER, _CONSOLEINPUT/_ECHO, _MOUSEHIDDEN/_CLIPBOARDIMAGE, device input (_DEVICES/_DEVICE$/_AXIS/_BUTTON/_WHEEL, etc.), drag/drop, resize, _COLORCHOOSERDIALOG, HSB colors, networking extended, _FILES$/_EMBEDDED$, _SNDNEW, error extended, _MAPUNICODE, unicode font, graphics mode constants (_SMOOTH/_HARDWARE/_SOFTWARE, etc.), date/time (_YEAR/_MONTH/_DAY, etc.). Ends by calling `subs::register_builtin_subs(analyzer)`.
- **builtins/subs.rs** — All built-in SUBs (_ACCEPTFILEDROP/_FINISHDROP, _CONSOLECURSOR/_CONSOLEFONT/_CONTROLCHR/_KEYUP, _SETALPHA/_COPYPALETTE/_BLEND/_DONTBLEND/_CLEARCOLOR/_DEPTHBUFFER/_DISPLAYORDER, _MAPTRIANGLE, _SNDLIMIT/_ICON/_HIDE/_SHOW/_ONTOP/_PRINTMODE/_UPRINTSTRING/_NOTIFYPOPUP/_ECHO/_CONSOLETITLE/_CLIPBOARD/_DELAY, _MEMPUT/_MEMFILL/_MEMCOPY/_MEMFREE/_SCREENICON, _LOGTRACE/_LOGINFO/_LOGWARN/_LOGERROR/_LOGMINLEVEL, _SAVEIMAGE/_SCREENPRINT/_PRINTIMAGE/_GLRENDER/_CLEAR/_TOGGLE/_SNDRAWBATCH/_MIDISOUNDBANK/_NEWHANDLER).

Each category module exports a single `pub(super) fn register_*_builtins(analyzer: &mut SemanticAnalyzer)` that calls `analyzer.register_builtin_function(...)` etc. Helpers remain on `SemanticAnalyzer` in `builtins/mod.rs`. Constant registration (which uses `self.symbols.define_symbol`) stays in `mod.rs` because `symbols` is private.

## Other

- Fixed a pre-existing syntax error in `src/codegen/c_backend/stmt/mod.rs`: the long misc arm was missing `k @ (` and had `DefSeg { segment }`; changed to `k @ (TypedStatementKind::DefSeg { .. } | ...` so the match compiles and all patterns bind the same way.

## Status

Done. `src/semantic/builtins.rs` was removed; `src/semantic/builtins/` now contains `mod.rs` plus the eight category modules. `docs/ThingsToDo/FILE_SPLITTING_ANALYSIS.md` §4 updated to mark this item complete.
