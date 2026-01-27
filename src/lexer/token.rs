//! Token definitions for the QB64Fresh lexer.
//!
//! This module defines all tokens recognized by the BASIC lexer, including:
//! - Keywords (IF, THEN, PRINT, etc.)
//! - Operators (+, -, AND, OR, etc.)
//! - Literals (numbers, strings)
//! - Punctuation and delimiters
//!
//! ## Design Notes
//!
//! We use the `logos` crate for lexical analysis. Logos generates a fast,
//! table-driven lexer from token definitions using procedural macros.
//!
//! BASIC is case-insensitive, so keywords are matched with `(?i:...)` regex.

use logos::Logos;
use std::fmt;

use crate::ast::Span;

/// A token with its location in the source code.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token
    pub kind: TokenKind,
    /// Source location span (byte offsets and line number)
    pub span: Span,
    /// The original text of the token (useful for identifiers, literals)
    pub text: String,
}

impl Token {
    /// Create a new token with the given kind, span, and text.
    pub fn new(kind: TokenKind, span: Span, text: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            text: text.into(),
        }
    }
}

/// All possible token types in QB64/QBasic.
///
/// Tokens are grouped into categories:
/// - Keywords (control flow, declarations, I/O, etc.)
/// - Operators (arithmetic, comparison, logical)
/// - Literals (numbers, strings)
/// - Punctuation and delimiters
/// - Special tokens (comments, whitespace, errors)
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r]+")] // Skip horizontal whitespace (but not newlines!)
#[logos(skip r"_[ \t]*\r?\n")] // Skip line continuations (underscore at end of line)
pub enum TokenKind {
    // ==================== Control Flow Keywords ====================
    /// IF keyword - begins conditional statement
    #[token("IF", ignore(ascii_case))]
    If,

    /// THEN keyword - follows IF condition
    #[token("THEN", ignore(ascii_case))]
    Then,

    /// ELSE keyword - alternative branch
    #[token("ELSE", ignore(ascii_case))]
    Else,

    /// ELSEIF keyword - chained conditional
    #[token("ELSEIF", ignore(ascii_case))]
    ElseIf,

    /// END keyword - used with IF, SUB, FUNCTION, TYPE, SELECT
    #[token("END", ignore(ascii_case))]
    End,

    /// ENDIF keyword - alternative syntax for END IF (no space)
    #[token("ENDIF", ignore(ascii_case))]
    EndIf,

    /// FOR keyword - begins FOR loop
    #[token("FOR", ignore(ascii_case))]
    For,

    /// TO keyword - FOR loop range
    #[token("TO", ignore(ascii_case))]
    To,

    /// STEP keyword - FOR loop increment
    #[token("STEP", ignore(ascii_case))]
    Step,

    /// NEXT keyword - ends FOR loop
    #[token("NEXT", ignore(ascii_case))]
    Next,

    /// WHILE keyword - begins WHILE loop or DO WHILE
    #[token("WHILE", ignore(ascii_case))]
    While,

    /// WEND keyword - ends WHILE loop
    #[token("WEND", ignore(ascii_case))]
    Wend,

    /// DO keyword - begins DO loop
    #[token("DO", ignore(ascii_case))]
    Do,

    /// LOOP keyword - ends DO loop
    #[token("LOOP", ignore(ascii_case))]
    Loop,

    /// UNTIL keyword - loop condition
    #[token("UNTIL", ignore(ascii_case))]
    Until,

    /// SELECT keyword - begins SELECT CASE
    #[token("SELECT", ignore(ascii_case))]
    Select,

    /// CASE keyword - SELECT CASE branch
    #[token("CASE", ignore(ascii_case))]
    Case,

    /// EVERYCASE keyword (QB64) - used in SELECT EVERYCASE
    #[token("EVERYCASE", ignore(ascii_case))]
    EveryCase,

    /// GOTO keyword - unconditional jump
    #[token("GOTO", ignore(ascii_case))]
    Goto,

    /// GOSUB keyword - subroutine call (legacy)
    #[token("GOSUB", ignore(ascii_case))]
    Gosub,

    /// RETURN keyword - return from GOSUB or FUNCTION
    #[token("RETURN", ignore(ascii_case))]
    Return,

    /// EXIT keyword - early exit from loop/sub/function
    #[token("EXIT", ignore(ascii_case))]
    Exit,

    /// STOP keyword - stop execution for debugging
    #[token("STOP", ignore(ascii_case))]
    Stop,

    /// SYSTEM keyword - exit program immediately
    #[token("SYSTEM", ignore(ascii_case))]
    System,

    /// SLEEP keyword - pause execution
    #[token("SLEEP", ignore(ascii_case))]
    Sleep,

    /// WAIT keyword - wait for hardware port condition
    #[token("WAIT", ignore(ascii_case))]
    Wait,

    /// _DELAY keyword - pause execution (float seconds, QB64)
    #[token("_DELAY", ignore(ascii_case))]
    Delay,

    /// _LIMIT keyword - limit frame rate (QB64)
    #[token("_LIMIT", ignore(ascii_case))]
    Limit,

    /// _KEYCLEAR keyword - clear keyboard buffer (QB64)
    #[token("_KEYCLEAR", ignore(ascii_case))]
    KeyClear,

    /// CALL keyword - call a subroutine
    #[token("CALL", ignore(ascii_case))]
    Call,

    /// IS keyword - used in SELECT CASE IS comparisons
    #[token("IS", ignore(ascii_case))]
    Is,

    /// BYVAL keyword - pass parameter by value
    #[token("BYVAL", ignore(ascii_case))]
    ByVal,

    /// SWAP keyword - exchange values of two variables
    #[token("SWAP", ignore(ascii_case))]
    Swap,

    /// _CONTINUE keyword (QB64) - continue to next loop iteration
    #[token("_CONTINUE", ignore(ascii_case))]
    Continue,

    /// RUN keyword - restart program or run another program
    #[token("RUN", ignore(ascii_case))]
    Run,

    /// CHAIN keyword - run another program, optionally passing variables
    #[token("CHAIN", ignore(ascii_case))]
    Chain,

    /// TRON keyword - enable trace mode (debugging)
    #[token("TRON", ignore(ascii_case))]
    Tron,

    /// TROFF keyword - disable trace mode (debugging)
    #[token("TROFF", ignore(ascii_case))]
    Troff,

    /// LPRINT keyword - print to printer (LPT1)
    #[token("LPRINT", ignore(ascii_case))]
    Lprint,

    /// FILES keyword - display directory listing
    #[token("FILES", ignore(ascii_case))]
    Files,

    /// FIELD keyword - define fields for random access file buffer
    #[token("FIELD", ignore(ascii_case))]
    Field,

    /// LSET keyword - left-align string in field buffer
    #[token("LSET", ignore(ascii_case))]
    Lset,

    /// RSET keyword - right-align string in field buffer
    #[token("RSET", ignore(ascii_case))]
    Rset,

    /// KEY keyword - for ON KEY and KEY statement
    #[token("KEY", ignore(ascii_case))]
    Key,

    /// STRIG keyword - joystick trigger
    #[token("STRIG", ignore(ascii_case))]
    Strig,

    /// STICK keyword - joystick position function
    #[token("STICK", ignore(ascii_case))]
    Stick,

    /// COM keyword - serial port event trapping
    #[token("COM", ignore(ascii_case))]
    Com,

    /// PEN keyword - light pen event trapping
    #[token("PEN", ignore(ascii_case))]
    Pen,

    /// UEVENT keyword - user-defined event
    #[token("UEVENT", ignore(ascii_case))]
    Uevent,

    /// SIGNAL keyword - signal handling
    #[token("SIGNAL", ignore(ascii_case))]
    Signal,

    /// INTERRUPT keyword - system interrupt call
    #[token("INTERRUPT", ignore(ascii_case))]
    Interrupt,

    /// INTERRUPTX keyword - extended system interrupt call
    #[token("INTERRUPTX", ignore(ascii_case))]
    InterruptX,

    /// ERDEV keyword - error device information
    #[token("ERDEV", ignore(ascii_case))]
    Erdev,

    /// IOCTL keyword - device control
    #[token("IOCTL", ignore(ascii_case))]
    Ioctl,

    /// FRE keyword - free memory function
    #[token("FRE", ignore(ascii_case))]
    Fre,

    /// FREE keyword - free string space statement
    #[token("FREE", ignore(ascii_case))]
    Free,

    /// INP keyword - port input function
    #[token("INP", ignore(ascii_case))]
    Inp,

    /// OUT keyword - port output statement
    #[token("OUT", ignore(ascii_case))]
    Out,

    /// OFF keyword - turn off event trapping
    #[token("OFF", ignore(ascii_case))]
    Off,

    /// ONLY keyword - exclusive file access
    #[token("ONLY", ignore(ascii_case))]
    Only,

    /// SMOOTH keyword - graphics smooth mode
    #[token("SMOOTH", ignore(ascii_case))]
    Smooth,

    /// STRETCH keyword - graphics stretch mode
    #[token("STRETCH", ignore(ascii_case))]
    Stretch,

    /// CUSTOMTYPE keyword - TYPE declaration modifier
    #[token("CUSTOMTYPE", ignore(ascii_case))]
    CustomType,

    /// TIMER keyword - timer event control
    #[token("TIMER", ignore(ascii_case))]
    Timer,

    /// CLEAR keyword - clear variables and set memory
    #[token("CLEAR", ignore(ascii_case))]
    Clear,

    /// RESET keyword - close all files
    #[token("RESET", ignore(ascii_case))]
    Reset,

    // ==================== Declaration Keywords ====================
    /// DIM keyword - variable declaration
    #[token("DIM", ignore(ascii_case))]
    Dim,

    /// REDIM keyword - resize dynamic array
    #[token("REDIM", ignore(ascii_case))]
    Redim,

    /// ERASE keyword - clear/deallocate arrays
    #[token("ERASE", ignore(ascii_case))]
    Erase,

    /// AS keyword - type specification
    #[token("AS", ignore(ascii_case))]
    As,

    /// SHARED keyword - module-level variable
    #[token("SHARED", ignore(ascii_case))]
    Shared,

    /// STATIC keyword - static variable
    #[token("STATIC", ignore(ascii_case))]
    Static,

    /// CONST keyword - constant declaration
    #[token("CONST", ignore(ascii_case))]
    Const,

    /// SEG keyword - for DEF SEG statement (memory segment)
    #[token("SEG", ignore(ascii_case))]
    Seg,

    /// POKE keyword - write byte to memory address
    #[token("POKE", ignore(ascii_case))]
    Poke,

    /// DEFINT keyword - set default type to INTEGER for letter range
    #[token("DEFINT", ignore(ascii_case))]
    DefInt,

    /// DEFLNG keyword - set default type to LONG for letter range
    #[token("DEFLNG", ignore(ascii_case))]
    DefLng,

    /// DEFSNG keyword - set default type to SINGLE for letter range
    #[token("DEFSNG", ignore(ascii_case))]
    DefSng,

    /// DEFDBL keyword - set default type to DOUBLE for letter range
    #[token("DEFDBL", ignore(ascii_case))]
    DefDbl,

    /// DEFSTR keyword - set default type to STRING for letter range
    #[token("DEFSTR", ignore(ascii_case))]
    DefStr,

    /// TYPE keyword - begins user-defined type
    #[token("TYPE", ignore(ascii_case))]
    Type,

    /// DECLARE keyword - forward declaration
    #[token("DECLARE", ignore(ascii_case))]
    Declare,

    /// LIBRARY keyword - for DECLARE LIBRARY blocks
    #[token("LIBRARY", ignore(ascii_case))]
    Library,

    /// DYNAMIC keyword - for DECLARE DYNAMIC LIBRARY
    #[token("DYNAMIC", ignore(ascii_case))]
    Dynamic,

    /// ALIAS keyword - maps BASIC name to C function name
    #[token("ALIAS", ignore(ascii_case))]
    Alias,

    /// SUB keyword - subroutine definition
    #[token("SUB", ignore(ascii_case))]
    Sub,

    /// FUNCTION keyword - function definition
    #[token("FUNCTION", ignore(ascii_case))]
    Function,

    /// LET keyword - assignment (optional in most BASIC)
    #[token("LET", ignore(ascii_case))]
    Let,

    // ==================== Type Keywords ====================
    /// INTEGER type
    #[token("INTEGER", ignore(ascii_case))]
    Integer,

    /// LONG type
    #[token("LONG", ignore(ascii_case))]
    Long,

    /// SINGLE type (single-precision float)
    #[token("SINGLE", ignore(ascii_case))]
    Single,

    /// DOUBLE type (double-precision float)
    #[token("DOUBLE", ignore(ascii_case))]
    Double,

    /// STRING type
    #[token("STRING", ignore(ascii_case))]
    String_, // Underscore to avoid conflict with Rust's String

    // QB64 extended types
    /// _BIT type (QB64 - single bit integer)
    #[token("_BIT", ignore(ascii_case))]
    BitType,

    /// _BYTE type (QB64)
    #[token("_BYTE", ignore(ascii_case))]
    Byte,

    /// _INTEGER64 type (QB64)
    #[token("_INTEGER64", ignore(ascii_case))]
    Integer64,

    /// _FLOAT type (QB64)
    #[token("_FLOAT", ignore(ascii_case))]
    Float,

    /// _OFFSET type (QB64 - pointer-sized integer)
    #[token("_OFFSET", ignore(ascii_case))]
    Offset,

    /// _MEM type (QB64 - memory block descriptor)
    #[token("_MEM", ignore(ascii_case))]
    MemType,

    /// _UNSIGNED modifier (QB64)
    #[token("_UNSIGNED", ignore(ascii_case))]
    Unsigned,

    /// _CV function (QB64 - generic convert string to type)
    #[token("_CV", ignore(ascii_case))]
    CvFunc,

    /// _MK$ function (QB64 - generic convert value to string)
    #[regex(r"(?i:_MK\$)", priority = 4)]
    MkDollarFunc,

    /// _CAST function (QB64 - explicit type conversion)
    #[token("_CAST", ignore(ascii_case))]
    CastFunc,

    /// _DEFINE statement (QB64 - define default variable types)
    #[token("_DEFINE", ignore(ascii_case))]
    Define,

    /// _PROCPTR function (QB64 - get procedure pointer for callbacks)
    #[token("_PROCPTR", ignore(ascii_case))]
    ProcPtr,

    // ==================== I/O Keywords ====================
    /// PRINT statement (also recognized as ? for shorthand)
    #[token("PRINT", ignore(ascii_case))]
    #[token("?")]
    Print,

    /// INPUT statement
    #[token("INPUT", ignore(ascii_case))]
    Input,

    /// INPUT$ function (reads characters from keyboard or file)
    /// Must be higher priority than INPUT keyword to avoid keyword + $ tokenization
    #[regex(r"(?i:INPUT\$)", priority = 4)]
    InputDollar,

    /// OPEN statement
    #[token("OPEN", ignore(ascii_case))]
    Open,

    /// CLOSE statement
    #[token("CLOSE", ignore(ascii_case))]
    Close,

    /// READ statement (DATA)
    #[token("READ", ignore(ascii_case))]
    Read,

    /// DATA statement
    #[token("DATA", ignore(ascii_case))]
    Data,

    /// RESTORE statement
    #[token("RESTORE", ignore(ascii_case))]
    Restore,

    /// WRITE statement
    #[token("WRITE", ignore(ascii_case))]
    Write,

    /// GET statement (file/graphics)
    #[token("GET", ignore(ascii_case))]
    Get,

    /// PUT statement (file/graphics)
    #[token("PUT", ignore(ascii_case))]
    Put,

    /// LINE keyword (LINE INPUT, LINE graphics)
    #[token("LINE", ignore(ascii_case))]
    Line,

    /// SEEK statement - set file position
    #[token("SEEK", ignore(ascii_case))]
    Seek,

    /// ACCESS keyword - file access mode
    #[token("ACCESS", ignore(ascii_case))]
    Access,

    /// BINARY keyword - binary file mode
    #[token("BINARY", ignore(ascii_case))]
    Binary,

    /// RANDOM keyword - random access file mode
    #[token("RANDOM", ignore(ascii_case))]
    Random,

    /// RANDOMIZE keyword - seed random number generator
    #[token("RANDOMIZE", ignore(ascii_case))]
    Randomize,

    /// OUTPUT keyword - output file mode
    #[token("OUTPUT", ignore(ascii_case))]
    Output,

    /// APPEND keyword - append file mode
    #[token("APPEND", ignore(ascii_case))]
    Append,

    /// USING keyword - format string for PRINT USING
    #[token("USING", ignore(ascii_case))]
    Using,

    /// LEN keyword - record length for random access files
    #[token("LEN", ignore(ascii_case))]
    Len,

    /// LOCK keyword - file locking
    #[token("LOCK", ignore(ascii_case))]
    Lock,

    /// UNLOCK keyword - file unlocking
    #[token("UNLOCK", ignore(ascii_case))]
    Unlock,

    // ==================== Graphics Keywords ====================
    /// SCREEN statement - initialize graphics mode
    #[token("SCREEN", ignore(ascii_case))]
    Screen,

    /// CLS statement - clear screen
    #[token("CLS", ignore(ascii_case))]
    Cls,

    /// COLOR statement - set foreground/background colors
    #[token("COLOR", ignore(ascii_case))]
    Color,

    /// LOCATE statement - position cursor
    #[token("LOCATE", ignore(ascii_case))]
    Locate,

    /// PSET statement - plot pixel
    #[token("PSET", ignore(ascii_case))]
    Pset,

    /// PRESET statement - plot pixel with background color
    #[token("PRESET", ignore(ascii_case))]
    Preset,

    /// CIRCLE statement - draw circle
    #[token("CIRCLE", ignore(ascii_case))]
    Circle,

    /// PAINT statement - flood fill
    #[token("PAINT", ignore(ascii_case))]
    Paint,

    /// PALETTE statement - set palette colors
    #[token("PALETTE", ignore(ascii_case))]
    Palette,

    /// PCOPY statement - copy screen page
    #[token("PCOPY", ignore(ascii_case))]
    Pcopy,

    /// _DISPLAY statement - update screen
    #[token("_DISPLAY", ignore(ascii_case))]
    Display,

    /// _CONTROLCHR statement - control printing of control characters
    #[token("_CONTROLCHR", ignore(ascii_case))]
    ControlChr,

    /// _MAPUNICODE statement - map Unicode codepoint to character position
    #[token("_MAPUNICODE", ignore(ascii_case))]
    MapUnicode,

    /// _RESIZE statement - enable/disable window resizing at runtime
    #[token("_RESIZE", ignore(ascii_case))]
    Resize,

    /// WIDTH statement - set screen width/columns
    #[token("WIDTH", ignore(ascii_case))]
    Width,

    /// VIEW statement - define viewport
    #[token("VIEW", ignore(ascii_case))]
    View,

    /// WINDOW statement - define coordinate system
    #[token("WINDOW", ignore(ascii_case))]
    Window,

    /// DRAW statement - turtle graphics
    #[token("DRAW", ignore(ascii_case))]
    Draw,

    // ==================== QB64 Graphics Extensions ====================
    /// _NEWIMAGE function - create new image buffer
    #[token("_NEWIMAGE", ignore(ascii_case))]
    NewImage,

    /// _LOADIMAGE function - load image from file
    #[token("_LOADIMAGE", ignore(ascii_case))]
    LoadImage,

    /// _FREEIMAGE statement - release image buffer
    #[token("_FREEIMAGE", ignore(ascii_case))]
    FreeImage,

    /// _PUTIMAGE statement - copy image region
    #[token("_PUTIMAGE", ignore(ascii_case))]
    PutImage,

    /// _SOURCE statement - set source image for reading
    #[token("_SOURCE", ignore(ascii_case))]
    Source,

    /// _DEST statement - set destination image for drawing
    #[token("_DEST", ignore(ascii_case))]
    Dest,

    /// _COPYIMAGE function - duplicate image buffer
    #[token("_COPYIMAGE", ignore(ascii_case))]
    CopyImage,

    /// _SCREENIMAGE function - capture screen to image
    #[token("_SCREENIMAGE", ignore(ascii_case))]
    ScreenImage,

    /// _PRINTSTRING statement - draw text at pixel position
    #[token("_PRINTSTRING", ignore(ascii_case))]
    PrintString,

    /// _PRINTWIDTH function - get pixel width of text string
    #[token("_PRINTWIDTH", ignore(ascii_case))]
    PrintWidth,

    /// _AUTODISPLAY statement - control automatic display updates
    #[token("_AUTODISPLAY", ignore(ascii_case))]
    AutoDisplay,

    /// _WIDTH function - get image width (with handle argument)
    #[token("_WIDTH", ignore(ascii_case))]
    ImageWidth,

    /// _HEIGHT function - get image height (with handle argument)
    #[token("_HEIGHT", ignore(ascii_case))]
    ImageHeight,

    // ==================== Audio Keywords ====================
    /// BEEP statement - simple beep sound
    #[token("BEEP", ignore(ascii_case))]
    Beep,

    /// SOUND statement - play tone
    #[token("SOUND", ignore(ascii_case))]
    Sound,

    /// PLAY statement - play music string
    #[token("PLAY", ignore(ascii_case))]
    Play,

    /// _SNDOPEN function - open sound file
    #[token("_SNDOPEN", ignore(ascii_case))]
    SndOpen,

    /// _SNDCLOSE statement - close sound handle
    #[token("_SNDCLOSE", ignore(ascii_case))]
    SndClose,

    /// _SNDPLAY statement - play sound
    #[token("_SNDPLAY", ignore(ascii_case))]
    SndPlay,

    /// _SNDSTOP statement - stop sound
    #[token("_SNDSTOP", ignore(ascii_case))]
    SndStop,

    /// _SNDPAUSE statement - pause sound
    #[token("_SNDPAUSE", ignore(ascii_case))]
    SndPause,

    /// _SNDLOOP statement - play sound in loop
    #[token("_SNDLOOP", ignore(ascii_case))]
    SndLoop,

    /// _SNDVOL statement - set sound volume
    #[token("_SNDVOL", ignore(ascii_case))]
    SndVol,

    /// _SNDBAL statement - set stereo balance
    #[token("_SNDBAL", ignore(ascii_case))]
    SndBal,

    /// _SNDOPENRAW function - open raw audio stream
    #[token("_SNDOPENRAW", ignore(ascii_case))]
    SndOpenRaw,

    /// _SNDRAW statement - write raw audio samples
    #[token("_SNDRAW", ignore(ascii_case))]
    SndRaw,

    /// _SNDPLAYFILE statement - play sound file directly
    #[token("_SNDPLAYFILE", ignore(ascii_case))]
    SndPlayFile,

    /// _SNDPLAYCOPY statement - play a copy of a sound
    #[token("_SNDPLAYCOPY", ignore(ascii_case))]
    SndPlayCopy,

    /// _SNDSETPOS statement - set playback position
    #[token("_SNDSETPOS", ignore(ascii_case))]
    SndSetPos,

    /// _SNDCOPY function - copy sound handle
    #[token("_SNDCOPY", ignore(ascii_case))]
    SndCopy,

    /// _SNDPLAYING function - check if sound is playing
    #[token("_SNDPLAYING", ignore(ascii_case))]
    SndPlaying,

    /// _SNDGETPOS function - get playback position
    #[token("_SNDGETPOS", ignore(ascii_case))]
    SndGetPos,

    /// _SNDLEN function - get sound length
    #[token("_SNDLEN", ignore(ascii_case))]
    SndLen,

    /// _SNDPAUSED function - check if sound is paused
    #[token("_SNDPAUSED", ignore(ascii_case))]
    SndPaused,

    // ==================== System Integration Keywords ====================
    /// KILL statement - delete file
    #[token("KILL", ignore(ascii_case))]
    Kill,

    /// NAME statement - rename file
    #[token("NAME", ignore(ascii_case))]
    Name,

    /// MKDIR statement - create directory
    #[token("MKDIR", ignore(ascii_case))]
    Mkdir,

    /// RMDIR statement - remove directory
    #[token("RMDIR", ignore(ascii_case))]
    Rmdir,

    /// CHDIR statement - change directory
    #[token("CHDIR", ignore(ascii_case))]
    Chdir,

    /// ENVIRON statement - set environment variable
    #[token("ENVIRON", ignore(ascii_case))]
    Environ,

    /// SHELL statement - execute external command
    #[token("SHELL", ignore(ascii_case))]
    Shell,

    /// _SHELLHIDE statement - execute hidden command (QB64)
    #[token("_SHELLHIDE", ignore(ascii_case))]
    ShellHide,

    /// _FILEEXISTS function - check if file exists (QB64)
    #[token("_FILEEXISTS", ignore(ascii_case))]
    FileExists,

    /// _DIREXISTS function - check if directory exists (QB64)
    #[token("_DIREXISTS", ignore(ascii_case))]
    DirExists,

    /// _DIR$ function - directory listing (QB64)
    #[token("_DIR$", ignore(ascii_case))]
    Dir,

    /// BLOAD statement - binary load to memory
    #[token("BLOAD", ignore(ascii_case))]
    Bload,

    /// BSAVE statement - binary save from memory
    #[token("BSAVE", ignore(ascii_case))]
    Bsave,

    /// SETMEM statement - set available memory (legacy, stub)
    #[token("SETMEM", ignore(ascii_case))]
    Setmem,

    /// ABSOLUTE keyword - call machine language routine (legacy)
    #[token("ABSOLUTE", ignore(ascii_case))]
    Absolute,

    /// CALLS keyword - call with far pointers (legacy, stub)
    #[token("CALLS", ignore(ascii_case))]
    Calls,

    /// CDECL keyword - C calling convention (legacy)
    #[token("CDECL", ignore(ascii_case))]
    Cdecl,

    // ==================== Mouse Input Keywords ====================
    /// _MOUSEX function - get mouse X position (QB64)
    #[token("_MOUSEX", ignore(ascii_case))]
    MouseX,

    /// _MOUSEY function - get mouse Y position (QB64)
    #[token("_MOUSEY", ignore(ascii_case))]
    MouseY,

    /// _MOUSEBUTTON function - get mouse button state (QB64)
    #[token("_MOUSEBUTTON", ignore(ascii_case))]
    MouseButton,

    /// _MOUSEINPUT function - check for mouse input (QB64)
    #[token("_MOUSEINPUT", ignore(ascii_case))]
    MouseInput,

    /// _MOUSEMOVEMENTX function - get mouse X movement (QB64)
    #[token("_MOUSEMOVEMENTX", ignore(ascii_case))]
    MouseMovementX,

    /// _MOUSEMOVEMENTY function - get mouse Y movement (QB64)
    #[token("_MOUSEMOVEMENTY", ignore(ascii_case))]
    MouseMovementY,

    /// _MOUSEWHEEL function - get mouse wheel delta (QB64)
    #[token("_MOUSEWHEEL", ignore(ascii_case))]
    MouseWheel,

    /// _MOUSEHIDE statement - hide mouse cursor (QB64)
    #[token("_MOUSEHIDE", ignore(ascii_case))]
    MouseHide,

    /// _MOUSESHOW statement - show mouse cursor (QB64)
    #[token("_MOUSESHOW", ignore(ascii_case))]
    MouseShow,

    /// _MOUSEMOVE statement - move mouse cursor (QB64)
    #[token("_MOUSEMOVE", ignore(ascii_case))]
    MouseMove,

    // ==================== Graphics Modifiers (QB64) ====================
    /// _CLIP modifier for PUT statement - clip to screen boundaries (QB64)
    #[token("_CLIP", ignore(ascii_case))]
    Clip,

    // ==================== Clipboard Keywords ====================
    /// _CLIPBOARD$ function/statement - clipboard access (QB64)
    #[token("_CLIPBOARD$", ignore(ascii_case))]
    Clipboard,

    // ==================== Window/Desktop Keywords (QB64) ====================
    // Note: Many QB64 window functions like _SCREENMOVE, _FULLSCREEN, etc. work
    // both as statements AND as functions. To avoid breaking the expression parser,
    // these are handled as identifiers and checked contextually in the parser.
    // Only add statement-only keywords here.
    /// _ALLOWFULLSCREEN statement - allow/disallow fullscreen (QB64)
    #[token("_ALLOWFULLSCREEN", ignore(ascii_case))]
    AllowFullScreen,

    /// _SCREENICON statement - minimize window (QB64)
    #[token("_SCREENICON", ignore(ascii_case))]
    ScreenIcon,

    /// _CONSOLETITLE statement - set console title (QB64)
    #[token("_CONSOLETITLE", ignore(ascii_case))]
    ConsoleTitle,

    /// _CONSOLE statement - control console window (QB64)
    #[token("_CONSOLE", ignore(ascii_case))]
    Console,

    /// _ASSERT statement - assertion (QB64)
    #[token("_ASSERT", ignore(ascii_case))]
    Assert,

    // ==================== Networking Keywords ====================
    /// _OPENHOST function - open TCP server on port (QB64)
    #[token("_OPENHOST", ignore(ascii_case))]
    OpenHost,

    /// _OPENCONNECTION function - accept client connection (QB64)
    #[token("_OPENCONNECTION", ignore(ascii_case))]
    OpenConnection,

    /// _OPENCLIENT function - connect to TCP server (QB64)
    #[token("_OPENCLIENT", ignore(ascii_case))]
    OpenClient,

    /// _CONNECTED function - check connection status (QB64)
    #[token("_CONNECTED", ignore(ascii_case))]
    Connected,

    // Note: B and BF for LINE boxes are handled as identifiers in the parser
    // since they conflict with single-letter variable names.
    /// ON keyword - used in ON ERROR, ON...GOTO/GOSUB
    #[token("ON", ignore(ascii_case))]
    On,

    /// ERROR keyword - used in ON ERROR, ERROR statement
    #[token("ERROR", ignore(ascii_case))]
    ErrorKw,

    /// RESUME keyword - resume from error
    #[token("RESUME", ignore(ascii_case))]
    Resume,

    /// OPTION keyword - used in OPTION BASE
    #[token("OPTION", ignore(ascii_case))]
    Option,

    /// BASE keyword - used in OPTION BASE
    #[token("BASE", ignore(ascii_case))]
    Base,

    /// _EXPLICIT keyword (QB64) - requires explicit variable declarations
    #[token("_EXPLICIT", ignore(ascii_case))]
    Explicit,

    /// _EXPLICITARRAY keyword (QB64) - requires explicit array declarations
    #[token("_EXPLICITARRAY", ignore(ascii_case))]
    ExplicitArray,

    /// DEF keyword - used in DEF FN
    #[token("DEF", ignore(ascii_case))]
    Def,

    /// FN keyword - user-defined function prefix
    #[token("FN", ignore(ascii_case))]
    Fn,

    /// COMMON keyword - shared variables between modules
    #[token("COMMON", ignore(ascii_case))]
    Common,

    /// _PRESERVE keyword (QB64) - preserve array contents during REDIM
    #[token("_PRESERVE", ignore(ascii_case))]
    Preserve,

    // ==================== Logical Operators (Keywords) ====================
    /// AND operator
    #[token("AND", ignore(ascii_case))]
    And,

    /// OR operator
    #[token("OR", ignore(ascii_case))]
    Or,

    /// NOT operator
    #[token("NOT", ignore(ascii_case))]
    Not,

    /// XOR operator
    #[token("XOR", ignore(ascii_case))]
    Xor,

    /// EQV operator (equivalence)
    #[token("EQV", ignore(ascii_case))]
    Eqv,

    /// IMP operator (implication)
    #[token("IMP", ignore(ascii_case))]
    Imp,

    /// _ANDALSO operator (short-circuit AND - QB64)
    #[token("_ANDALSO", ignore(ascii_case))]
    AndAlso,

    /// _ORELSE operator (short-circuit OR - QB64)
    #[token("_ORELSE", ignore(ascii_case))]
    OrElse,

    /// MOD operator
    #[token("MOD", ignore(ascii_case))]
    Mod,

    // ==================== Arithmetic Operators ====================
    /// + addition or string concatenation
    #[token("+")]
    Plus,

    /// - subtraction or negation
    #[token("-")]
    Minus,

    /// * multiplication
    #[token("*")]
    Star,

    /// / division (floating point)
    #[token("/")]
    Slash,

    /// \ integer division
    #[token("\\")]
    Backslash,

    /// ^ exponentiation
    #[token("^")]
    Caret,

    // ==================== Comparison Operators ====================
    /// = equals (assignment or comparison)
    #[token("=")]
    Equals,

    /// <> not equals
    #[token("<>")]
    NotEquals,

    /// < less than
    #[token("<")]
    LessThan,

    /// > greater than
    #[token(">")]
    GreaterThan,

    /// <= less than or equal
    #[token("<=")]
    LessEquals,

    /// >= greater than or equal
    #[token(">=")]
    GreaterEquals,

    // Legacy comparison operators (QB4.5 compatibility)
    // These are alternate forms accepted by older BASIC versions
    /// >< not equals (legacy form of <>)
    #[token("><")]
    NotEqualsLegacy,

    /// =< less than or equal (legacy form of <=)
    #[token("=<")]
    LessEqualsLegacy,

    /// => greater than or equal (legacy form of >=)
    #[token("=>")]
    GreaterEqualsLegacy,

    // ==================== Punctuation ====================
    /// ( left parenthesis
    #[token("(")]
    LeftParen,

    /// ) right parenthesis
    #[token(")")]
    RightParen,

    /// , comma - argument separator
    #[token(",")]
    Comma,

    /// ; semicolon - PRINT separator
    #[token(";")]
    Semicolon,

    /// : colon - statement separator
    #[token(":")]
    Colon,

    /// . dot - member access
    #[token(".")]
    Dot,

    /// # hash - file number prefix
    #[token("#")]
    Hash,

    // ==================== Type Suffixes ====================
    // These appear at the end of identifiers to indicate type
    /// $ string type suffix
    #[token("$")]
    DollarSign,

    /// % integer type suffix
    #[token("%")]
    Percent,

    /// & long type suffix
    #[token("&")]
    Ampersand,

    /// ! single type suffix (standalone, low priority so identifier suffixes take precedence)
    #[token("!", priority = 1)]
    Exclamation,

    /// @ _OFFSET type suffix (QB64)
    // Note: In QB64, @ is used as OFFSET suffix, but we'll handle this in parsing

    // ==================== Literals ====================

    /// Integer literal (decimal) with optional type suffix
    /// Examples: 123, 0, 999999, 2000&, 100%, 50!, 1##, 5&&
    /// Suffixes: % (INTEGER), & (LONG), ! (SINGLE), # (DOUBLE)
    ///           %% (INTEGER16/BYTE), && (INTEGER64), ## (DOUBLE)
    ///           ~% (UNSIGNED INTEGER), ~& (UNSIGNED LONG), ~%%, ~&&, etc.
    /// Note: Multi-char suffixes must come before single-char in alternation
    #[regex(r"[0-9]+(~?&&|~?%%|~?##|~?%&|~?[%&!#`])?", priority = 2)]
    IntegerLiteral,

    /// Hexadecimal literal with optional type suffix
    /// Examples: &H1F, &HFF00, &HE0~%%, &HFFFFFFFF&&
    /// Suffixes same as integer literals: ~%%, ~&&, ~%, ~&, etc.
    #[regex(r"&[Hh][0-9A-Fa-f]+(~?&&|~?%%|~?##|~?%&|~?[%&!#`])?")]
    HexLiteral,

    /// Octal literal with optional type suffix
    /// Examples: &O17, &O777, &O377~%%
    #[regex(r"&[Oo][0-7]+(~?&&|~?%%|~?##|~?%&|~?[%&!#`])?")]
    OctalLiteral,

    /// Binary literal (QB64 extension) with optional type suffix
    /// Examples: &B1010, &B11110000, &B11111111~%%
    #[regex(r"&[Bb][01]+(~?&&|~?%%|~?##|~?%&|~?[%&!#`])?")]
    BinaryLiteral,

    /// Floating point literal with optional type suffix
    /// Examples: 1.5, .5, 1., 1.5E10, 1.5D-3, 3.14!, 2.71828#, 1.0##
    /// Suffixes: ! (SINGLE), # (DOUBLE), ## (DOUBLE explicit)
    #[regex(r"([0-9]*\.[0-9]+([EeDd][+-]?[0-9]+)?|[0-9]+[EeDd][+-]?[0-9]+)(##|[!#])?")]
    FloatLiteral,

    /// String literal
    /// Everything between double quotes on a single line
    /// In BASIC, strings cannot contain newlines - use UnterminatedString for that case
    #[regex(r#""[^"\n]*""#)]
    StringLiteral,

    /// Unterminated string literal (missing closing quote)
    /// Matches a quote followed by non-quote, non-newline chars without closing quote
    /// This is an error token - strings in BASIC cannot span lines
    #[regex(r#""[^"\n]*"#)]
    UnterminatedString,

    // ==================== Identifiers ====================
    /// Identifier (variable, function, or label name)
    /// Must start with letter, can contain letters, digits, underscores, and dots.
    /// Dots are allowed in classic BASIC for naming conventions (e.g., `path.exe$`).
    /// Field access is disambiguated at the semantic level based on UDT declarations.
    ///
    /// May end with type suffix:
    /// - Single char: $, %, &, !, #, `
    /// - Two char: %%, &&, ##, %& (QB64 extended types)
    /// - Unsigned: ~%, ~&, ~`, ~%%, ~&& (QB64 unsigned types)
    ///
    /// Priority 3 ensures type suffixes are captured as part of the identifier.
    /// Note: Multi-char suffixes (&&, %%, etc.) must come before single-char alternatives.
    #[regex(
        r"[A-Za-z_][A-Za-z0-9_.]*(&&|%%|##|%&|~&&|~%%|~##|~%&|~%|~&|~`|[$%&!#`])?",
        priority = 3
    )]
    Identifier,

    // ==================== Special Tokens ====================
    /// Comment - starts with '
    #[regex(r"'[^\n]*")]
    Comment,

    /// REM comment (traditional BASIC comment keyword)
    /// Note: Must be followed by space or end of line to distinguish from identifiers like REMOVE
    #[regex(r"(?i:REM)([ \t][^\n]*)?")]
    RemComment,

    /// Newline - significant in BASIC (ends statements)
    #[regex(r"\n")]
    Newline,

    // ==================== Preprocessor (QB64) ====================
    /// $INCLUDE directive
    #[regex(r"\$INCLUDE\s*:\s*'[^']*'", ignore(ascii_case))]
    IncludeDirective,

    /// $IF - conditional compilation start
    #[token("$IF", ignore(ascii_case))]
    MetaIf,

    /// $ELSE - conditional compilation else
    #[token("$ELSE", ignore(ascii_case))]
    MetaElse,

    /// $ELSEIF - conditional compilation elseif
    #[token("$ELSEIF", ignore(ascii_case))]
    MetaElseIf,

    /// $END IF - conditional compilation end
    #[regex(r"\$END\s+IF", ignore(ascii_case))]
    MetaEndIf,

    /// $LET - compile-time variable assignment
    #[token("$LET", ignore(ascii_case))]
    MetaLet,

    /// $CHECKING - enable/disable bounds checking
    #[token("$CHECKING", ignore(ascii_case))]
    MetaChecking,

    /// $CONSOLE:ONLY - enable console window only
    /// NOTE: Due to a logos bug, $CONSOLE patterns cause Error tokens instead of matching.
    /// The parser has a workaround that handles Error tokens containing "$CONSOLE".
    /// See parser/statements.rs parse_statement() for the workaround.
    // #[regex(r"\$CONSOLE\s*:\s*ONLY", ignore(ascii_case))]  // Disabled - causes logos bug
    MetaConsoleOnly,

    /// $CONSOLE - enable console window
    /// NOTE: Disabled due to logos bug - see MetaConsoleOnly comment above.
    // #[regex(r"\$CONSOLE", ignore(ascii_case))]  // Disabled - causes logos bug
    MetaConsole,

    /// $SCREENHIDE - hide graphics window on startup
    #[token("$SCREENHIDE", ignore(ascii_case))]
    MetaScreenHide,

    /// $SCREENSHOW - show graphics window on startup
    #[token("$SCREENSHOW", ignore(ascii_case))]
    MetaScreenShow,

    /// $RESIZE:ON - enable window resize events
    #[regex(r"\$RESIZE\s*:\s*ON", ignore(ascii_case))]
    MetaResizeOn,

    /// $RESIZE:OFF - disable window resize events
    #[regex(r"\$RESIZE\s*:\s*OFF", ignore(ascii_case))]
    MetaResizeOff,

    /// $RESIZE:STRETCH - stretch graphics when resizing
    #[regex(r"\$RESIZE\s*:\s*STRETCH", ignore(ascii_case))]
    MetaResizeStretch,

    /// $RESIZE:SMOOTH - smooth scaling when resizing
    #[regex(r"\$RESIZE\s*:\s*SMOOTH", ignore(ascii_case))]
    MetaResizeSmooth,

    /// $STATIC - use static arrays
    #[token("$STATIC", ignore(ascii_case))]
    MetaStatic,

    /// $DYNAMIC - use dynamic arrays
    #[token("$DYNAMIC", ignore(ascii_case))]
    MetaDynamic,

    /// $DEBUG - enable debug mode
    #[token("$DEBUG", ignore(ascii_case))]
    MetaDebug,

    /// $INCLUDEONCE - include file only once
    #[token("$INCLUDEONCE", ignore(ascii_case))]
    MetaIncludeOnce,

    /// $EXEICON - set executable icon (captures filename)
    #[regex(r"\$EXEICON\s*:\s*'[^']*'", ignore(ascii_case))]
    MetaExeIcon,

    /// $VERSIONINFO - set version info (captures key=value)
    /// Note: Key may include # suffix (e.g., FILEVERSION#, PRODUCTVERSION#)
    #[regex(r"\$VERSIONINFO\s*:\s*[A-Za-z]+#?\s*=\s*[^\r\n]+", ignore(ascii_case))]
    MetaVersionInfo,

    /// $ERROR - compiler error message
    #[regex(r"\$ERROR\s+[^\r\n]+", ignore(ascii_case))]
    MetaError,

    /// $EMBED - embed a file into the executable
    #[regex(r"\$EMBED\s*:\s*'[^']*'", ignore(ascii_case))]
    MetaEmbed,

    /// $MIDISOUNDFONT - set MIDI soundfont file
    #[regex(r"\$MIDISOUNDFONT\s*:\s*'[^']*'", ignore(ascii_case))]
    MetaMidiSoundFont,

    /// $UNSTABLE - enable unstable features
    #[regex(r"\$UNSTABLE\s*:\s*[A-Za-z][A-Za-z0-9_]*", ignore(ascii_case))]
    MetaUnstable,

    /// $FORMAT - code formatting directive
    #[token("$FORMAT", ignore(ascii_case))]
    MetaFormat,

    /// $USELIBRARY - use external library
    #[regex(r"\$USELIBRARY\s*:\s*'[^']*'", ignore(ascii_case))]
    MetaUseLibrary,

    /// Other $ directives (catch-all for unrecognized metacommands)
    #[regex(r"\$[A-Za-z][A-Za-z0-9]*")]
    MetaCommand,

    /// Error token for unrecognized characters
    /// This is generated by the lexer when it encounters input it cannot match
    Error,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Keywords display as uppercase
            TokenKind::If => write!(f, "IF"),
            TokenKind::Then => write!(f, "THEN"),
            TokenKind::Else => write!(f, "ELSE"),
            TokenKind::ElseIf => write!(f, "ELSEIF"),
            TokenKind::End => write!(f, "END"),
            TokenKind::Print => write!(f, "PRINT"),
            // ... add more as needed

            // Operators display as symbols
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Equals => write!(f, "="),

            // Default: use debug representation
            _ => write!(f, "{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    /// Helper to collect all tokens from source
    fn lex_all(source: &str) -> Vec<TokenKind> {
        TokenKind::lexer(source).filter_map(|r| r.ok()).collect()
    }

    #[test]
    fn test_keywords_case_insensitive() {
        assert_eq!(lex_all("IF"), vec![TokenKind::If]);
        assert_eq!(lex_all("if"), vec![TokenKind::If]);
        assert_eq!(lex_all("If"), vec![TokenKind::If]);
        assert_eq!(lex_all("iF"), vec![TokenKind::If]);
    }

    #[test]
    fn test_simple_print_statement() {
        let tokens = lex_all(r#"PRINT "Hello, World!""#);
        assert_eq!(tokens, vec![TokenKind::Print, TokenKind::StringLiteral,]);
    }

    #[test]
    fn test_arithmetic_expression() {
        let tokens = lex_all("1 + 2 * 3");
        assert_eq!(
            tokens,
            vec![
                TokenKind::IntegerLiteral,
                TokenKind::Plus,
                TokenKind::IntegerLiteral,
                TokenKind::Star,
                TokenKind::IntegerLiteral,
            ]
        );
    }

    #[test]
    fn test_if_statement() {
        let tokens = lex_all("IF x > 10 THEN PRINT x");
        assert_eq!(
            tokens,
            vec![
                TokenKind::If,
                TokenKind::Identifier,
                TokenKind::GreaterThan,
                TokenKind::IntegerLiteral,
                TokenKind::Then,
                TokenKind::Print,
                TokenKind::Identifier,
            ]
        );
    }

    #[test]
    fn test_hex_literal() {
        let tokens = lex_all("&HFF &h1a");
        assert_eq!(tokens, vec![TokenKind::HexLiteral, TokenKind::HexLiteral,]);
    }

    #[test]
    fn test_float_literals() {
        let tokens = lex_all("1.5 .5 1.5E10 1D-3");
        assert_eq!(
            tokens,
            vec![
                TokenKind::FloatLiteral,
                TokenKind::FloatLiteral,
                TokenKind::FloatLiteral,
                TokenKind::FloatLiteral,
            ]
        );
    }

    #[test]
    fn test_comment() {
        let tokens = lex_all("x = 1 ' this is a comment");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier,
                TokenKind::Equals,
                TokenKind::IntegerLiteral,
                TokenKind::Comment,
            ]
        );
    }

    #[test]
    fn test_type_suffixes() {
        let tokens = lex_all("name$ count% total& value! num#");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier, // name$ - suffix included in identifier
                TokenKind::Identifier, // count%
                TokenKind::Identifier, // total&
                TokenKind::Identifier, // value!
                TokenKind::Identifier, // num#
            ]
        );
    }

    #[test]
    fn test_unterminated_string() {
        // String without closing quote should be recognized as UnterminatedString
        let tokens = lex_all("PRINT \"hello");
        assert_eq!(
            tokens,
            vec![TokenKind::Print, TokenKind::UnterminatedString]
        );

        // Complete string should still work
        let tokens = lex_all("PRINT \"hello\"");
        assert_eq!(tokens, vec![TokenKind::Print, TokenKind::StringLiteral]);
    }

    #[test]
    fn test_error_token_for_invalid_chars() {
        // Invalid characters should return Error tokens instead of being silently skipped
        // Note: We use the Lexer wrapper (via crate::lexer::lex) instead of lex_all
        // because lex_all uses logos directly and filters out errors
        let tokens = crate::lexer::lex("x @ y");
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();

        assert!(
            kinds.contains(&&TokenKind::Error),
            "@ should produce an Error token, got: {:?}",
            kinds
        );

        // Verify the other tokens are still recognized
        assert!(kinds.contains(&&TokenKind::Identifier));
    }

    #[test]
    fn test_line_continuation() {
        // Line continuation should be transparent (underscore followed by newline is skipped)
        // "x = 1 + _\n2" should lex the same as "x = 1 + 2"
        let tokens = lex_all("x = 1 + _\n2");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier,     // x
                TokenKind::Equals,         // =
                TokenKind::IntegerLiteral, // 1
                TokenKind::Plus,           // +
                TokenKind::IntegerLiteral, // 2
            ]
        );

        // Line continuation with trailing spaces before newline
        let tokens = lex_all("x = 1 + _   \n2");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier,
                TokenKind::Equals,
                TokenKind::IntegerLiteral,
                TokenKind::Plus,
                TokenKind::IntegerLiteral,
            ]
        );

        // Normal newlines should still produce Newline tokens
        let tokens = lex_all("x = 1\ny = 2");
        assert!(
            tokens.contains(&TokenKind::Newline),
            "Normal newlines should produce tokens"
        );
    }

    #[test]
    fn test_legacy_comparison_operators() {
        // >< is legacy form of <>
        let tokens = lex_all("x >< y");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier,
                TokenKind::NotEqualsLegacy,
                TokenKind::Identifier,
            ]
        );

        // =< is legacy form of <=
        let tokens = lex_all("x =< y");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier,
                TokenKind::LessEqualsLegacy,
                TokenKind::Identifier,
            ]
        );

        // => is legacy form of >=
        let tokens = lex_all("x => y");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier,
                TokenKind::GreaterEqualsLegacy,
                TokenKind::Identifier,
            ]
        );
    }

    #[test]
    fn test_swap_keyword() {
        let tokens = lex_all("SWAP a, b");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Swap,
                TokenKind::Identifier,
                TokenKind::Comma,
                TokenKind::Identifier,
            ]
        );
    }

    #[test]
    fn test_continue_keyword() {
        let tokens = lex_all("_CONTINUE");
        assert_eq!(tokens, vec![TokenKind::Continue]);

        // Case insensitive
        let tokens = lex_all("_continue");
        assert_eq!(tokens, vec![TokenKind::Continue]);
    }
}
