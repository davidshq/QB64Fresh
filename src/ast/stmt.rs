//! Statement AST nodes.
//!
//! Statements are constructs that perform actions but don't produce values.
//! In BASIC, most lines of code are statements: PRINT, IF, FOR, assignments, etc.
//!
//! # Statement Categories
//!
//! - **I/O**: PRINT, INPUT, LINE INPUT
//! - **Assignment**: LET (optional), variable = expression
//! - **Control flow**: IF/THEN/ELSE, SELECT CASE, GOTO, GOSUB
//! - **Loops**: FOR/NEXT, WHILE/WEND, DO/LOOP
//! - **Declarations**: DIM, CONST, TYPE
//! - **Procedures**: SUB, FUNCTION, CALL

use super::{Expr, Span};

/// A statement with its source location.
#[derive(Debug, Clone)]
pub struct Statement {
    /// The kind of statement.
    pub kind: StatementKind,
    /// Source location of this statement.
    pub span: Span,
}

impl Statement {
    /// Creates a new statement with the given kind and span.
    pub fn new(kind: StatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// The different kinds of statements in BASIC.
#[derive(Debug, Clone)]
pub enum StatementKind {
    /// `PRINT expr1; expr2, expr3`
    ///
    /// The `newline` field indicates whether to print a newline at the end.
    /// A trailing semicolon suppresses the newline.
    Print {
        /// Values to print.
        values: Vec<PrintItem>,
        /// Whether to print a newline at the end (false if trailing `;`).
        newline: bool,
    },

    /// `PRINT USING format$; value1, value2, ...`
    ///
    /// Formatted output using a format string with placeholders.
    PrintUsing {
        /// The format string expression.
        format: Expr,
        /// Values to format and print.
        values: Vec<Expr>,
        /// Whether to print a newline at the end (false if trailing `;`).
        newline: bool,
    },

    /// `[LET] variable = expression`
    ///
    /// Assignment statement. The LET keyword is optional in modern BASIC.
    Let { name: String, value: Expr },

    /// `array(indices) = expression`
    ///
    /// Array element assignment statement.
    ArrayAssignment {
        /// Array name.
        name: String,
        /// Index expressions.
        indices: Vec<Expr>,
        /// Value to assign.
        value: Expr,
    },

    /// `array(indices).field = expression`
    ///
    /// Assignment to a field of a UDT stored in an array.
    ArrayFieldAssignment {
        /// Array name.
        name: String,
        /// Index expressions.
        indices: Vec<Expr>,
        /// Field access chain (e.g., ["R"] for `.R` or ["pos", "x"] for `.pos.x`).
        fields: Vec<String>,
        /// Value to assign.
        value: Expr,
    },

    /// `DIM variable AS type` or `DIM array(size) AS type`
    ///
    /// Multiple variables can be declared on one line: `DIM a, b(10), c AS STRING`
    Dim {
        /// List of variables to declare
        variables: Vec<DimVariable>,
        /// Whether SHARED was specified (applies to all variables)
        shared: bool,
    },

    /// `CONST name = value [, name2 = value2, ...]` - Constant definition(s)
    ///
    /// Multiple constants can be defined on the same line, separated by commas.
    Const {
        /// List of (name, value) pairs for constants defined on this line
        definitions: Vec<(String, Expr)>,
    },

    /// `DEFINT A-Z` - Set default type for variable names starting with letters in range
    ///
    /// DEFINT, DEFLNG, DEFSNG, DEFDBL, DEFSTR set the default type for variables
    /// whose names begin with letters in the specified range.
    DefType {
        /// The type keyword used (DefInt, DefLng, DefSng, DefDbl, DefStr)
        type_kind: DefTypeKind,
        /// List of letter ranges, e.g., [('A', 'Z')] or [('I', 'N'), ('X', 'X')]
        ranges: Vec<(char, char)>,
    },

    /// Single-line: `IF condition THEN statement [ELSE statement]`
    /// Multi-line: `IF condition THEN ... [ELSEIF ...] [ELSE ...] END IF`
    If {
        /// The condition to test.
        condition: Expr,
        /// Statements to execute if condition is true.
        then_branch: Vec<Statement>,
        /// ELSEIF clauses (condition + statements).
        elseif_branches: Vec<(Expr, Vec<Statement>)>,
        /// Statements to execute if condition is false (ELSE branch).
        else_branch: Option<Vec<Statement>>,
    },

    /// `SELECT CASE expression ... END SELECT`
    SelectCase {
        /// The expression to match against.
        test_expr: Expr,
        /// The CASE clauses.
        cases: Vec<CaseClause>,
        /// CASE ELSE clause (if present).
        case_else: Option<Vec<Statement>>,
    },

    /// `FOR var = start TO end [STEP step] ... NEXT [var]`
    For {
        /// Loop variable name.
        variable: String,
        /// Starting value.
        start: Expr,
        /// Ending value.
        end: Expr,
        /// Step value (defaults to 1 if not specified).
        step: Option<Expr>,
        /// Loop body.
        body: Vec<Statement>,
        /// Variable name after NEXT (if specified, must match loop variable).
        next_variable: Option<String>,
    },

    /// `WHILE condition ... WEND`
    While {
        /// Loop condition.
        condition: Expr,
        /// Loop body.
        body: Vec<Statement>,
    },

    /// `DO [WHILE|UNTIL condition] ... LOOP [WHILE|UNTIL condition]`
    DoLoop {
        /// Pre-condition (DO WHILE/UNTIL).
        pre_condition: Option<DoCondition>,
        /// Loop body.
        body: Vec<Statement>,
        /// Post-condition (LOOP WHILE/UNTIL).
        post_condition: Option<DoCondition>,
    },

    /// `GOTO label` or `GOTO lineNumber`
    Goto { target: String },

    /// `GOSUB label` or `GOSUB lineNumber`
    Gosub { target: String },

    /// `RETURN` - Return from GOSUB
    Return,

    /// `EXIT FOR`, `EXIT WHILE`, `EXIT DO`, `EXIT SUB`, `EXIT FUNCTION`
    Exit { exit_type: ExitType },

    /// `END` - End program execution
    End,

    /// `STOP` - Stop execution (for debugging)
    Stop,

    /// `SYSTEM` - Exit program immediately (returns to OS)
    System,

    /// `SLEEP [seconds]` - Pause execution
    Sleep {
        /// Optional duration in seconds (integer). If None, waits for keypress.
        seconds: Option<Expr>,
    },

    /// `_DELAY seconds` - Pause execution (QB64, float precision)
    Delay {
        /// Duration in seconds (float).
        seconds: Expr,
    },

    /// `_LIMIT fps` - Limit frame rate (QB64)
    Limit {
        /// Target frames per second.
        fps: Expr,
    },

    /// `ERASE arrayname [, arrayname...]` - Clear/deallocate arrays
    Erase {
        /// List of array names to erase.
        arrays: Vec<String>,
    },

    /// `_KEYCLEAR` - Clear keyboard buffer (QB64)
    KeyClear,

    /// `SWAP var1, var2` - Exchange values of two variables
    Swap {
        /// First variable to swap.
        left: Expr,
        /// Second variable to swap.
        right: Expr,
    },

    /// `_CONTINUE` - Continue to the next loop iteration (QB64)
    Continue {
        /// The type of loop to continue (For, While, Do).
        continue_type: ContinueType,
    },

    /// `INPUT [;]["prompt"{;|,}] variable[, variable...]`
    Input {
        /// Optional prompt string.
        prompt: Option<String>,
        /// Whether to show question mark after prompt.
        show_question_mark: bool,
        /// Variables to read into.
        variables: Vec<String>,
    },

    /// `LINE INPUT [;]["prompt";] variable$`
    LineInput {
        /// Optional prompt string.
        prompt: Option<String>,
        /// Variable to read into (must be string).
        variable: String,
    },

    /// `DATA value1, value2, ...` - compile-time data definition
    ///
    /// DATA statements define a pool of values that can be read using READ.
    /// Values can be numeric literals or string literals.
    Data {
        /// The literal values in this DATA statement.
        values: Vec<DataValue>,
    },

    /// `READ var1, var2, ...` - read from DATA pool
    ///
    /// READ statements consume values from the DATA pool in order.
    /// Targets can be simple variables or array elements.
    Read {
        /// Targets to read into (variables or array elements).
        targets: Vec<ReadTarget>,
    },

    /// `RESTORE [label]` - reset DATA pointer
    ///
    /// RESTORE resets the DATA read position. Optional label specifies
    /// a specific DATA statement to restore to.
    Restore {
        /// Optional label to restore to.
        label: Option<String>,
    },

    /// `RANDOMIZE [seed]` or `RANDOMIZE TIMER`
    ///
    /// Seeds the random number generator. If no argument, prompts user for seed.
    /// RANDOMIZE [seed] - Initialize random number generator.
    /// If seed is None, implementation may prompt user or use a default.
    /// TIMER is just a function call in the seed expression (e.g., RANDOMIZE TIMER).
    Randomize {
        /// Seed expression. None means no seed provided.
        seed: Option<Expr>,
    },

    /// Label definition: `labelName:`
    Label { name: String },

    /// `SUB name [(parameters)] ... END SUB`
    SubDefinition {
        name: String,
        params: Vec<Parameter>,
        body: Vec<Statement>,
        is_static: bool,
    },

    /// `FUNCTION name [(parameters)] ... END FUNCTION`
    FunctionDefinition {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeSpec>,
        body: Vec<Statement>,
        is_static: bool,
    },

    /// `TYPE TypeName ... END TYPE` - User-defined type definition
    ///
    /// Example:
    /// ```basic
    /// TYPE Person
    ///     name AS STRING * 20
    ///     age AS INTEGER
    /// END TYPE
    /// ```
    TypeDefinition {
        /// The name of the user-defined type.
        name: String,
        /// The members of the type.
        members: Vec<TypeMember>,
    },

    /// `CALL SubName(args)` or `SubName args`
    Call { name: String, args: Vec<Expr> },

    /// Expression used as a statement (e.g., function call with no return value used)
    Expression(Expr),

    /// Comment: `' text` or `REM text`
    Comment(String),

    // ==================== Preprocessor Directives (QB64) ====================
    /// `$INCLUDE: 'filename'` - Include another source file
    ///
    /// This is a compile-time directive that inserts the contents of another
    /// file at this location. The actual file inclusion is handled during
    /// preprocessing, not during parsing.
    IncludeDirective {
        /// The path to the file to include (without quotes).
        path: String,
    },

    /// `$IF condition THEN` ... `$ELSEIF` ... `$ELSE` ... `$END IF`
    ///
    /// Conditional compilation blocks. The condition is typically a compile-time
    /// constant like `WIN`, `LINUX`, `MAC`, or a user-defined symbol.
    ConditionalBlock {
        /// The condition expression (e.g., "WIN", "LINUX = -1").
        condition: String,
        /// Statements in the $IF block.
        then_branch: Vec<Statement>,
        /// $ELSEIF clauses (condition + statements).
        elseif_branches: Vec<(String, Vec<Statement>)>,
        /// $ELSE block statements.
        else_branch: Option<Vec<Statement>>,
    },

    /// Other `$metacommand` directives (e.g., $DYNAMIC, $STATIC, $ERROR, etc.)
    ///
    /// These are stored as-is for later processing.
    MetaCommand {
        /// The command name without the $ prefix (e.g., "DYNAMIC", "STATIC").
        command: String,
        /// Any arguments following the command.
        args: Option<String>,
    },

    // ==================== File I/O Statements ====================
    /// `OPEN filename FOR mode [ACCESS access] [lock] AS [#]filenum [LEN=reclen]`
    ///
    /// Opens a file for reading, writing, or both.
    OpenFile {
        /// The filename expression (usually a string).
        filename: Expr,
        /// The file mode (Input, Output, Append, Binary, Random).
        mode: FileMode,
        /// Optional access mode (Read, Write, ReadWrite).
        access: Option<FileAccess>,
        /// Optional lock mode (Shared, Read, Write, ReadWrite).
        lock: Option<FileLock>,
        /// The file number expression (1-511).
        file_num: Expr,
        /// Optional record length for random access files.
        record_len: Option<Expr>,
    },

    /// `CLOSE [[#]filenum [, [#]filenum]...]`
    ///
    /// Closes one or more files. If no file numbers specified, closes all files.
    CloseFile {
        /// File numbers to close. Empty means close all.
        file_nums: Vec<Expr>,
    },

    /// `PRINT #filenum, [expression [{;|,} expression]...]`
    ///
    /// Writes data to a sequential file.
    FilePrint {
        /// The file number.
        file_num: Expr,
        /// Values to write.
        values: Vec<PrintItem>,
        /// Whether to write a newline at the end.
        newline: bool,
    },

    /// `WRITE #filenum, [expression [{,} expression]...]`
    ///
    /// Writes data to a sequential file with delimiters (comma-separated, strings quoted).
    FileWrite {
        /// The file number.
        file_num: Expr,
        /// Values to write.
        values: Vec<Expr>,
    },

    /// `INPUT #filenum, variable [, variable]...`
    ///
    /// Reads data from a sequential file.
    FileInput {
        /// The file number.
        file_num: Expr,
        /// Variables to read into (can be simple vars, array elements, or field accesses).
        targets: Vec<InputTarget>,
    },

    /// `LINE INPUT #filenum, variable$`
    ///
    /// Reads an entire line from a sequential file.
    FileLineInput {
        /// The file number.
        file_num: Expr,
        /// Variable to read into (must be string).
        variable: String,
    },

    /// `GET [#]filenum, [position], variable` or `GET #filenum, , variable`
    ///
    /// Reads data from a binary or random access file.
    FileGet {
        /// The file number.
        file_num: Expr,
        /// Optional position (record number for random, byte position for binary).
        position: Option<Expr>,
        /// Variable to read into.
        variable: String,
    },

    /// `PUT [#]filenum, [position], variable` or `PUT #filenum, , variable`
    ///
    /// Writes data to a binary or random access file.
    FilePut {
        /// The file number.
        file_num: Expr,
        /// Optional position (record number for random, byte position for binary).
        position: Option<Expr>,
        /// Variable containing data to write.
        variable: String,
    },

    /// `SEEK [#]filenum, position`
    ///
    /// Sets the position for the next read/write operation.
    FileSeek {
        /// The file number.
        file_num: Expr,
        /// The position to seek to.
        position: Expr,
    },

    // ==================== Error Handling Statements ====================
    /// `ON ERROR GOTO label` or `ON ERROR GOTO 0` (disable error handling)
    ///
    /// Enables error trapping and specifies the error handler location.
    OnErrorGoto {
        /// The label to jump to on error. If "0", disables error handling.
        target: String,
    },

    /// `ON ERROR RESUME NEXT`
    ///
    /// Continues execution at the next statement after an error.
    OnErrorResumeNext,

    /// `RESUME [NEXT | label]`
    ///
    /// Resumes execution after error handling.
    ResumeStmt {
        /// Resume target: None = retry statement, Next = next statement, Some(label) = jump to label.
        target: Option<ResumeTarget>,
    },

    /// `ERROR code`
    ///
    /// Simulates an error with the specified error code.
    ErrorStmt {
        /// The error code to simulate.
        code: Expr,
    },

    // ==================== Computed Control Flow ====================
    /// `ON expression GOTO label1, label2, ...`
    ///
    /// Branches to one of several labels based on the expression value.
    OnGoto {
        /// The selector expression (1-based index).
        selector: Expr,
        /// List of target labels.
        targets: Vec<String>,
    },

    /// `ON expression GOSUB label1, label2, ...`
    ///
    /// Calls one of several subroutines based on the expression value.
    OnGosub {
        /// The selector expression (1-based index).
        selector: Expr,
        /// List of target labels.
        targets: Vec<String>,
    },

    // ==================== DEF FN ====================
    /// `DEF FNname[(parameters)] = expression` (single-line)
    ///
    /// Defines a user-defined function.
    DefFn {
        /// Function name (without FN prefix).
        name: String,
        /// Optional parameters.
        params: Vec<Parameter>,
        /// The function body expression.
        body: Expr,
    },

    /// `DEF SEG [= segment]` - Set memory segment for PEEK/POKE/BLOAD/BSAVE.
    ///
    /// Without an argument, resets to the default data segment.
    DefSeg {
        /// Optional segment address expression.
        segment: Option<Expr>,
    },

    // ==================== Variable/Scope Statements ====================
    /// `COMMON [SHARED] variable [, variable]...`
    ///
    /// Declares variables that are shared between modules.
    CommonStmt {
        /// Whether SHARED was specified.
        shared: bool,
        /// Variables to share.
        variables: Vec<CommonVariable>,
    },

    /// `SHARED var1[, var2, ...]` inside SUB/FUNCTION
    ///
    /// Declares that the procedure uses module-level shared variables.
    /// This makes variables declared with DIM SHARED at module level
    /// accessible within the procedure.
    SharedStmt {
        /// Names of shared variables to access.
        variables: Vec<String>,
    },

    /// `REDIM [_PRESERVE] array1(dims) [AS type], array2(dims) [AS type], ...`
    ///
    /// Resizes dynamic arrays, optionally preserving contents.
    /// Multiple arrays can be specified on one line.
    Redim {
        /// Whether to preserve existing contents.
        preserve: bool,
        /// List of arrays to redimension.
        variables: Vec<DimVariable>,
    },

    // ==================== Additional Conditional Compilation ====================
    /// `$LET variable = value` - compile-time variable assignment
    MetaLet {
        /// Variable name.
        name: String,
        /// Value (integer, typically -1 for true, 0 for false).
        value: i64,
    },

    /// `$CHECKING:ON` or `$CHECKING:OFF` - bounds checking control
    MetaChecking {
        /// Whether checking is enabled.
        enabled: bool,
    },

    // ==================== Graphics Statements ====================
    /// `SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]` - Initialize graphics mode
    ///
    /// Full QB45 syntax supports:
    /// - mode: Screen mode number (0=text, 1-13=various graphics modes)
    /// - colorswitch: Color/monochrome flag
    /// - apage: Active page number
    /// - vpage: Visual page number
    Screen {
        /// Screen mode number (optional, can be omitted with empty first argument)
        mode: Option<Expr>,
        /// Color switch (optional)
        color_switch: Option<Expr>,
        /// Active page number (optional)
        active_page: Option<Expr>,
        /// Visual page number (optional)
        visual_page: Option<Expr>,
    },

    /// `CLS [mode]` - Clear screen
    /// mode: 0=clear graphics and text, 1=clear graphics only, 2=clear text only
    Cls {
        /// Optional clear mode (0, 1, or 2)
        mode: Option<Expr>,
    },

    /// `COLOR foreground[, background]` - Set text/drawing colors
    Color {
        /// Foreground color
        foreground: Expr,
        /// Background color (optional)
        background: Option<Expr>,
    },

    /// `LOCATE row, col` - Position cursor
    Locate {
        /// Row (1-based)
        row: Expr,
        /// Column (1-based)
        col: Expr,
    },

    /// `VIEW PRINT [top TO bottom]` - Set text viewport
    ///
    /// Restricts PRINT output to a range of screen rows.
    /// - `VIEW PRINT` alone resets to full screen.
    /// - `VIEW PRINT 1 TO 20` restricts printing to rows 1-20.
    ViewPrint {
        /// Top row (if specified)
        top: Option<Expr>,
        /// Bottom row (if specified)
        bottom: Option<Expr>,
    },

    /// `PSET (x, y)[, color]` - Plot pixel
    Pset {
        /// X coordinate
        x: Expr,
        /// Y coordinate
        y: Expr,
        /// Color (optional, uses foreground if not specified)
        color: Option<Expr>,
    },

    /// `PRESET (x, y)` - Plot pixel with background color
    Preset {
        /// X coordinate
        x: Expr,
        /// Y coordinate
        y: Expr,
    },

    /// `LINE [(x1, y1)]-[STEP](x2, y2)[, color][, B|BF]` - Draw line or box
    Line {
        /// Start X (optional - uses last point if not specified)
        x1: Option<Expr>,
        /// Start Y
        y1: Option<Expr>,
        /// End X
        x2: Expr,
        /// End Y
        y2: Expr,
        /// Whether STEP keyword was used for end coordinates (relative to start)
        step2: bool,
        /// Color (optional)
        color: Option<Expr>,
        /// Box style: None = line, Some(false) = box outline, Some(true) = filled box
        box_style: Option<bool>,
    },

    /// `CIRCLE (x, y), radius[, color][, start][, end][, aspect][, F]`
    Circle {
        /// Center X
        x: Expr,
        /// Center Y
        y: Expr,
        /// Radius
        radius: Expr,
        /// Color (optional)
        color: Option<Expr>,
        /// Whether to fill the circle
        filled: bool,
    },

    /// `PAINT (x, y)[, color][, border]` - Flood fill
    Paint {
        /// Start X
        x: Expr,
        /// Start Y
        y: Expr,
        /// Fill color (optional)
        color: Option<Expr>,
        /// Border color (optional)
        border: Option<Expr>,
    },

    /// `_DISPLAY` - Update screen (for double-buffered graphics)
    GfxDisplay,

    /// `WIDTH columns[, rows]` - Set screen text width
    Width {
        /// Number of columns
        columns: Expr,
        /// Number of rows (optional)
        rows: Option<Expr>,
    },

    /// `VIEW [[SCREEN] (x1, y1)-(x2, y2)[, color[, border]]]` - Define viewport
    View {
        /// Whether SCREEN was specified (absolute coordinates)
        screen: bool,
        /// Viewport coordinates (None = reset to full screen)
        coords: Option<ViewCoords>,
        /// Fill color (optional)
        fill_color: Option<Expr>,
        /// Border color (optional)
        border_color: Option<Expr>,
    },

    /// `WINDOW [[SCREEN] (x1, y1)-(x2, y2)]` - Define world coordinate system
    WindowCoords {
        /// Whether SCREEN was specified (inverted Y axis)
        screen: bool,
        /// Window coordinates (None = reset to pixel coordinates)
        coords: Option<ViewCoords>,
    },

    /// `DRAW string$` - Turtle graphics drawing commands
    DrawCmd {
        /// Command string containing drawing instructions
        commands: Expr,
    },

    /// `GET (x1, y1)-(x2, y2), array[(index)]` - Capture screen region to array
    ///
    /// Captures a rectangular screen region into an array for later use with PUT.
    /// The array must be large enough to hold the captured image data.
    GraphicsGet {
        /// First corner X coordinate
        x1: Expr,
        /// First corner Y coordinate
        y1: Expr,
        /// Second corner X coordinate (or width if step2 is true)
        x2: Expr,
        /// Second corner Y coordinate (or height if step2 is true)
        y2: Expr,
        /// Whether x2,y2 are relative (STEP)
        step2: bool,
        /// Array name to store the captured image
        array_name: String,
        /// Optional array index for storing in array of arrays
        array_index: Option<Expr>,
    },

    /// `PUT (x, y), array[(index)][, action]` - Draw array contents to screen
    ///
    /// Draws a previously captured image (via GET) to the screen at the specified
    /// coordinates. The action parameter determines how pixels are combined:
    /// - PSET: Replace destination pixels
    /// - PRESET: Replace with inverted pixels
    /// - AND: Bitwise AND with destination
    /// - OR: Bitwise OR with destination
    /// - XOR: Bitwise XOR with destination (default)
    ///
    /// QB64 adds: `PUT (x,y), array, _CLIP action, transparent_color`
    GraphicsPut {
        /// X coordinate for placing the image
        x: Expr,
        /// Y coordinate for placing the image
        y: Expr,
        /// Whether coordinates are relative (STEP)
        step: bool,
        /// Array name containing the image data
        array_name: String,
        /// Optional array index
        array_index: Option<Expr>,
        /// QB64: _CLIP modifier to clip image at screen boundaries
        clip: bool,
        /// Action for combining pixels with existing screen content
        action: PutAction,
        /// QB64: Optional transparent color (after _CLIP)
        transparent_color: Option<Expr>,
    },

    // ==================== QB64 Graphics Extensions ====================
    /// `_FREEIMAGE handle&` - Release image buffer
    FreeImage {
        /// Image handle to free
        handle: Expr,
    },

    /// `_PUTIMAGE [(dx1,dy1)-(dx2,dy2)][, src&][, dest&][, (sx1,sy1)-(sx2,sy2)]`
    PutImage {
        /// Destination coordinates (optional) - boxed to reduce enum size
        dest_coords: Option<Box<ViewCoords>>,
        /// Source image handle (optional, defaults to _SOURCE)
        source: Option<Expr>,
        /// Destination image handle (optional, defaults to _DEST)
        dest: Option<Expr>,
        /// Source coordinates (optional) - boxed to reduce enum size
        source_coords: Option<Box<ViewCoords>>,
    },

    /// `_SOURCE handle&` - Set source image for reading operations
    SourceImg {
        /// Image handle to use as source
        handle: Expr,
    },

    /// `_DEST handle&` - Set destination image for drawing operations
    DestImg {
        /// Image handle to use as destination
        handle: Expr,
    },

    /// `_PRINTSTRING (x, y), text$` - Draw text at pixel position
    PrintStringStmt {
        /// X coordinate
        x: Expr,
        /// Y coordinate
        y: Expr,
        /// Text to print
        text: Expr,
    },

    /// `_AUTODISPLAY {ON|OFF}` - Control automatic display updates
    AutoDisplay {
        /// Whether auto-display is enabled
        enabled: bool,
    },

    // ==================== Audio Statements ====================
    /// `BEEP` - Play default beep sound
    Beep,

    /// `SOUND frequency, duration` - Play tone
    SoundStmt {
        /// Frequency in Hz
        frequency: Expr,
        /// Duration in clock ticks (18.2 ticks/second)
        duration: Expr,
    },

    /// `PLAY string$` - Play music using MML (Music Macro Language)
    PlayStmt {
        /// Music string containing MML commands
        commands: Expr,
    },

    /// `_SNDCLOSE handle&` - Close sound handle
    SndClose {
        /// Sound handle to close
        handle: Expr,
    },

    /// `_SNDPLAY handle&` - Play sound
    SndPlay {
        /// Sound handle to play
        handle: Expr,
    },

    /// `_SNDSTOP handle&` - Stop playing sound
    SndStop {
        /// Sound handle to stop
        handle: Expr,
    },

    /// `_SNDPAUSE handle&` - Pause sound playback
    SndPause {
        /// Sound handle to pause
        handle: Expr,
    },

    /// `_SNDLOOP handle&` - Play sound in continuous loop
    SndLoop {
        /// Sound handle to loop
        handle: Expr,
    },

    /// `_SNDVOL handle&, volume!` - Set sound volume
    SndVol {
        /// Sound handle
        handle: Expr,
        /// Volume (0.0 to 1.0)
        volume: Expr,
    },

    /// `_SNDBAL handle&, balance!` - Set stereo balance
    SndBal {
        /// Sound handle
        handle: Expr,
        /// Balance (-1.0 left to 1.0 right)
        balance: Expr,
    },

    /// `_SNDRAW sample!` or `_SNDRAW left!, right!` - Write raw audio samples
    SndRaw {
        /// Left channel sample (or mono)
        left: Expr,
        /// Right channel sample (optional for stereo)
        right: Option<Expr>,
    },

    // ==================== System Integration Statements ====================
    /// `KILL filename$` - Delete a file
    Kill {
        /// The filename to delete
        filename: Expr,
    },

    /// `NAME oldname$ AS newname$` - Rename a file
    Rename {
        /// The current filename
        old_name: Expr,
        /// The new filename
        new_name: Expr,
    },

    /// `MKDIR path$` - Create a directory
    Mkdir {
        /// The directory path to create
        path: Expr,
    },

    /// `RMDIR path$` - Remove a directory
    Rmdir {
        /// The directory path to remove
        path: Expr,
    },

    /// `CHDIR path$` - Change current directory
    Chdir {
        /// The directory to change to
        path: Expr,
    },

    /// `SHELL [command$]` - Execute an external command
    ///
    /// If no command is specified, opens an interactive shell.
    ShellCmd {
        /// The command to execute (optional)
        command: Option<Expr>,
    },

    /// `_SHELLHIDE command$` - Execute a command without showing console (QB64)
    ShellHide {
        /// The command to execute
        command: Expr,
    },

    // ==================== Mouse Input Statements ====================
    /// `_MOUSEHIDE` - Hide the mouse cursor
    MouseHide,

    /// `_MOUSESHOW` - Show the mouse cursor
    MouseShow,

    /// `_MOUSEMOVE x%, y%` - Move mouse cursor to position
    MouseMoveStmt {
        /// X coordinate
        x: Expr,
        /// Y coordinate
        y: Expr,
    },

    // ==================== Clipboard Statement ====================
    /// `_CLIPBOARD$ = text$` - Set clipboard contents
    ClipboardSet {
        /// The text to set in the clipboard
        text: Expr,
    },

    // ==================== C Library Integration ====================
    /// `DECLARE LIBRARY "name" ... END DECLARE` or `DECLARE DYNAMIC LIBRARY "name" ... END DECLARE`
    ///
    /// Declares external C functions from a library.
    /// Static libraries are linked at compile time, dynamic libraries at runtime.
    ///
    /// Example:
    /// ```basic
    /// DECLARE LIBRARY "mylib"
    ///     FUNCTION add_values& (BYVAL a AS LONG, BYVAL b AS LONG)
    /// END DECLARE
    /// ```
    DeclareLibrary {
        /// Library name/path (without extension). None for header-only declarations.
        library_name: Option<String>,
        /// Whether this is a dynamic library (loaded at runtime).
        is_dynamic: bool,
        /// External function/sub declarations within the block.
        declarations: Vec<ExternalDeclaration>,
    },

    // ==================== Forward Declarations ====================
    /// `DECLARE SUB name [(parameters)]`
    ///
    /// Forward declaration of a subroutine. In classic BASIC, these declare
    /// SUB signatures before their definition. Used for documentation and
    /// to allow calls before the SUB is defined.
    ///
    /// Example:
    /// ```basic
    /// DECLARE SUB PrintMessage (msg AS STRING)
    /// ```
    DeclareSub {
        /// The name of the subroutine.
        name: String,
        /// Parameter declarations.
        params: Vec<DeclareParam>,
    },

    /// `DECLARE FUNCTION name [(parameters)]`
    ///
    /// Forward declaration of a function. Similar to DECLARE SUB but for
    /// functions that return values.
    ///
    /// Example:
    /// ```basic
    /// DECLARE FUNCTION AddNumbers% (a AS INTEGER, b AS INTEGER)
    /// ```
    DeclareFunction {
        /// The name of the function.
        name: String,
        /// Parameter declarations.
        params: Vec<DeclareParam>,
    },
}

/// File mode for OPEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileMode {
    /// FOR INPUT - sequential read
    Input,
    /// FOR OUTPUT - sequential write (creates/truncates)
    Output,
    /// FOR APPEND - sequential write (creates/appends)
    Append,
    /// FOR BINARY - binary read/write
    Binary,
    /// FOR RANDOM - random access read/write
    Random,
}

/// File access mode for OPEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileAccess {
    /// ACCESS READ
    Read,
    /// ACCESS WRITE
    Write,
    /// ACCESS READ WRITE
    ReadWrite,
}

/// File lock mode for OPEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileLock {
    /// SHARED - other processes can read and write
    Shared,
    /// LOCK READ - other processes cannot read
    LockRead,
    /// LOCK WRITE - other processes cannot write
    LockWrite,
    /// LOCK READ WRITE - exclusive access
    LockReadWrite,
}

/// Resume target for RESUME statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResumeTarget {
    /// RESUME NEXT - continue at next statement
    Next,
    /// RESUME label - jump to specific label
    Label(String),
}

/// Target for INPUT statement (lvalue).
#[derive(Debug, Clone)]
pub enum InputTarget {
    /// Simple variable: `x`
    Variable(String),
    /// Array element: `arr(i)` or `arr(i, j)`
    ArrayElement { name: String, indices: Vec<Expr> },
    /// Array element field access: `arr(i).field` or `arr(i).field.subfield`
    ArrayElementField {
        name: String,
        indices: Vec<Expr>,
        fields: Vec<String>,
    },
    /// Simple UDT field access: `udt.field`
    Field { name: String, fields: Vec<String> },
}

/// Variable declaration in COMMON statement.
#[derive(Debug, Clone)]
pub struct CommonVariable {
    /// Variable name.
    pub name: String,
    /// Array dimensions (empty if not an array).
    pub dimensions: Vec<ArrayDimension>,
    /// Type specification.
    pub type_spec: Option<TypeSpec>,
}

/// An item in a PRINT statement.
///
/// PRINT can have expressions separated by `;` (no spacing) or `,` (tab to next zone).
#[derive(Debug, Clone)]
pub struct PrintItem {
    /// The expression to print.
    pub expr: Expr,
    /// The separator after this item (if any).
    pub separator: Option<PrintSeparator>,
}

/// Separator between PRINT items.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintSeparator {
    /// `;` - Print next item immediately after
    Semicolon,
    /// `,` - Tab to next 14-column print zone
    Comma,
}

/// Array dimension specification.
#[derive(Debug, Clone)]
pub struct ArrayDimension {
    /// Lower bound (defaults to 0 or OPTION BASE setting).
    pub lower: Option<Expr>,
    /// Upper bound.
    pub upper: Expr,
}

/// A single variable in a DIM statement.
#[derive(Debug, Clone)]
pub struct DimVariable {
    /// Variable name.
    pub name: String,
    /// Array dimensions (empty if not an array).
    pub dimensions: Vec<ArrayDimension>,
    /// Type specification (if AS clause present).
    pub type_spec: Option<TypeSpec>,
}

/// Type specification for DIM statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    // Standard QBasic types
    Integer,
    Long,
    Single,
    Double,
    String,
    /// Fixed-length string: `STRING * n`
    FixedString(usize),

    // QB64 extended types
    Byte,
    Bit,
    Integer64,
    Float,
    Offset,
    Unsigned(Box<TypeSpec>),

    /// User-defined type reference
    UserDefined(String),
}

impl TypeSpec {
    /// Returns the type suffix character(s) for this type.
    pub fn suffix(&self) -> Option<&'static str> {
        match self {
            TypeSpec::Integer => Some("%"),
            TypeSpec::Long => Some("&"),
            TypeSpec::Single => Some("!"),
            TypeSpec::Double => Some("#"),
            TypeSpec::String | TypeSpec::FixedString(_) => Some("$"),
            TypeSpec::Byte => Some("%%"),
            TypeSpec::Integer64 => Some("&&"),
            TypeSpec::Float => Some("##"),
            TypeSpec::Offset => Some("%&"),
            _ => None,
        }
    }
}

/// A CASE clause in SELECT CASE.
#[derive(Debug, Clone)]
pub struct CaseClause {
    /// The values/conditions to match.
    pub matches: Vec<CaseMatch>,
    /// Statements to execute if matched.
    pub body: Vec<Statement>,
}

/// A single match condition in a CASE clause.
#[derive(Debug, Clone)]
pub enum CaseMatch {
    /// Single value: `CASE 1`
    Single(Expr),
    /// Range: `CASE 1 TO 10`
    Range { from: Expr, to: Expr },
    /// Comparison: `CASE IS > 5`
    Comparison { op: CaseCompareOp, value: Expr },
}

/// Comparison operators allowed in CASE IS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseCompareOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// DO loop condition type.
#[derive(Debug, Clone)]
pub struct DoCondition {
    /// Whether this is WHILE (true) or UNTIL (false).
    pub is_while: bool,
    /// The condition expression.
    pub condition: Expr,
}

/// Exit statement type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExitType {
    For,
    While,
    Do,
    Sub,
    Function,
}

/// Continue statement type (QB64 _CONTINUE).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContinueType {
    For,
    While,
    Do,
}

/// Target for READ statement - can be a variable or array element.
#[derive(Debug, Clone)]
pub enum ReadTarget {
    /// Simple variable: `READ x`
    Variable(String),
    /// Array element: `READ arr(i, j)`
    ArrayElement { name: String, indices: Vec<Expr> },
    /// UDT array element field: `READ arr(i).field`
    ArrayFieldElement {
        name: String,
        indices: Vec<Expr>,
        field: String,
    },
}

/// Action mode for graphics PUT statement.
///
/// Determines how pixel values are combined with existing screen content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PutAction {
    /// XOR pixels (default) - toggleable sprites
    #[default]
    Xor,
    /// Replace destination with source pixels
    Pset,
    /// Replace destination with inverted source pixels
    Preset,
    /// Bitwise AND - useful for masks
    And,
    /// Bitwise OR - additive sprites
    Or,
}

/// Parameter definition for SUB/FUNCTION.
#[derive(Debug, Clone)]
pub struct Parameter {
    /// Parameter name.
    pub name: String,
    /// Parameter type (if specified with AS).
    pub type_spec: Option<TypeSpec>,
    /// Whether this is a BYVAL parameter.
    pub by_val: bool,
    /// Whether this is an array parameter (e.g., `arr() AS INTEGER`).
    pub is_array: bool,
}

/// Parameter definition for DECLARE SUB/FUNCTION forward declarations.
///
/// Unlike `Parameter`, this uses a string for the type since forward
/// declarations often specify types by name (e.g., `DECLARE SUB Foo(x AS INTEGER)`).
#[derive(Debug, Clone)]
pub struct DeclareParam {
    /// Parameter name.
    pub name: String,
    /// Parameter type name (if specified with AS).
    pub param_type: Option<String>,
    /// Whether this is an array parameter (e.g., `arr() AS INTEGER`).
    pub is_array: bool,
}

/// Member definition for TYPE (user-defined type).
///
/// Represents a field within a TYPE...END TYPE block.
#[derive(Debug, Clone)]
pub struct TypeMember {
    /// Member name (field name).
    pub name: String,
    /// Member type specification.
    pub type_spec: TypeSpec,
}

/// A literal value in a DATA statement.
#[derive(Debug, Clone)]
pub enum DataValue {
    /// Integer literal.
    Integer(i64),
    /// Floating-point literal.
    Float(f64),
    /// String literal.
    String(String),
}

/// Coordinates for VIEW and WINDOW statements.
///
/// Represents a rectangular region defined by two corner points.
#[derive(Debug, Clone)]
pub struct ViewCoords {
    /// X coordinate of first corner.
    pub x1: Expr,
    /// Y coordinate of first corner.
    pub y1: Expr,
    /// X coordinate of second corner.
    pub x2: Expr,
    /// Y coordinate of second corner.
    pub y2: Expr,
}

/// External function or sub declaration within DECLARE LIBRARY.
///
/// Represents a C function that can be called from BASIC code.
#[derive(Debug, Clone)]
pub struct ExternalDeclaration {
    /// BASIC name for the function/sub.
    pub name: String,
    /// C library name (if different from BASIC name, specified via ALIAS).
    pub alias: Option<String>,
    /// Parameters of the external function.
    pub params: Vec<ExternalParam>,
    /// Return type for functions (None for SUBs).
    pub return_type: Option<TypeSpec>,
    /// Whether this is a FUNCTION (true) or SUB (false).
    pub is_function: bool,
}

/// Parameter for an external function in DECLARE LIBRARY.
#[derive(Debug, Clone)]
pub struct ExternalParam {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub type_spec: TypeSpec,
    /// Whether passed by value (BYVAL). Required for C interop.
    pub is_byval: bool,
}

/// The kind of DEFxxx statement (DEFINT, DEFLNG, DEFSNG, DEFDBL, DEFSTR).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefTypeKind {
    /// DEFINT - default to INTEGER (16-bit signed)
    Integer,
    /// DEFLNG - default to LONG (32-bit signed)
    Long,
    /// DEFSNG - default to SINGLE (32-bit float)
    Single,
    /// DEFDBL - default to DOUBLE (64-bit float)
    Double,
    /// DEFSTR - default to STRING
    String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_spec_suffix() {
        assert_eq!(TypeSpec::Integer.suffix(), Some("%"));
        assert_eq!(TypeSpec::String.suffix(), Some("$"));
        assert_eq!(TypeSpec::Double.suffix(), Some("#"));
        assert_eq!(TypeSpec::UserDefined("MyType".to_string()).suffix(), None);
    }

    #[test]
    fn test_create_print_statement() {
        let stmt = Statement::new(
            StatementKind::Print {
                values: vec![],
                newline: true,
            },
            Span::new(0, 5),
        );
        assert!(matches!(
            stmt.kind,
            StatementKind::Print { newline: true, .. }
        ));
    }
}
