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
    Let {
        /// Variable name.
        name: String,
        /// Expression to assign.
        value: Expr,
    },

    /// `variable.field = expression` or `variable.field.subfield = expression`
    ///
    /// Assignment to a field of a UDT variable.
    FieldAssignment {
        /// Variable name.
        name: String,
        /// Field access chain (e.g., ["R"] for `.R` or ["pos", "x"] for `.pos.x`).
        fields: Vec<String>,
        /// Value to assign.
        value: Expr,
    },

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

    /// `MID$(str$, start [, length]) = value$` - Substring assignment
    ///
    /// Replaces a portion of the string with a new value.
    /// If length is omitted, replaces from start to the end of the string
    /// or to the length of the replacement value, whichever is shorter.
    ///
    /// The target can be a simple variable, array element, or UDT field.
    MidAssignment {
        /// Target string expression (variable, array element, or field access).
        target: Expr,
        /// Start position (1-based).
        start: Expr,
        /// Optional length to replace.
        length: Option<Expr>,
        /// Replacement value.
        value: Expr,
    },

    /// `ASC(string$, position) = value` - Set character at position in string
    ///
    /// This statement modifies a character in a string by setting its ASCII value.
    /// The position is 1-based, and value should be an ASCII code (0-255).
    ///
    /// Example: `ASC(name$, 1) = 65` sets first character to 'A'
    AscAssignment {
        /// Target string expression (variable, array element, or field access).
        target: Expr,
        /// Position in string (1-based).
        position: Expr,
        /// ASCII value to set (0-255).
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

    /// `_DEFINE A-Z AS type` - QB64 extended default type declaration
    ///
    /// Like DEFxxx but allows specifying any QB64 type including extended types
    /// like _INTEGER64, _BYTE, _FLOAT, _OFFSET, and _UNSIGNED variants.
    Define {
        /// The type specifier (e.g., "_INTEGER64", "_UNSIGNED _BYTE")
        type_spec: String,
        /// List of letter ranges
        ranges: Vec<(char, char)>,
    },

    /// `OPTION BASE 0` or `OPTION BASE 1`
    ///
    /// Sets the default lower bound for array subscripts. Must appear before
    /// any array declarations. Default is 0.
    OptionBase {
        /// The base value (0 or 1).
        base: i64,
    },

    /// `OPTION _EXPLICIT` - Require all variables to be explicitly declared
    ///
    /// When enabled, any variable used without prior DIM statement causes
    /// a compile-time error. This helps catch typos in variable names.
    OptionExplicit,

    /// `OPTION _EXPLICITARRAY` - Require all arrays to be explicitly declared
    ///
    /// When enabled, any array used without prior DIM statement causes
    /// a compile-time error, but scalar variables can still be implicitly declared.
    OptionExplicitArray,

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

    /// `SELECT EVERYCASE expression ... END SELECT` (QB64)
    ///
    /// Unlike SELECT CASE, SELECT EVERYCASE evaluates ALL cases and
    /// executes ALL matching ones, not just the first match.
    SelectEveryCase {
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
    Goto {
        /// Target label or line number.
        target: String,
    },

    /// `GOSUB label` or `GOSUB lineNumber`
    Gosub {
        /// Target label or line number.
        target: String,
    },

    /// `RETURN` - Return from GOSUB
    Return,

    /// `EXIT FOR`, `EXIT WHILE`, `EXIT DO`, `EXIT SUB`, `EXIT FUNCTION`
    Exit {
        /// Type of exit (FOR, WHILE, DO, SUB, FUNCTION).
        exit_type: ExitType,
    },

    /// `END [exit_code]` - End program execution with optional exit code
    End {
        /// Optional exit code expression.
        exit_code: Option<Expr>,
    },

    /// `STOP` - Stop execution (for debugging)
    Stop,

    /// `SYSTEM [exit_code]` - Exit program immediately with optional exit code
    System {
        /// Optional exit code expression.
        exit_code: Option<Expr>,
    },

    /// `SLEEP [seconds]` - Pause execution
    Sleep {
        /// Optional duration in seconds (integer). If None, waits for keypress.
        seconds: Option<Expr>,
    },

    /// `WAIT port, and_mask[, xor_mask]` - Wait for hardware port condition
    Wait {
        /// Port address to check.
        port: Expr,
        /// AND mask for port value.
        and_mask: Expr,
        /// Optional XOR mask (invert bits).
        xor_mask: Option<Expr>,
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
    ///
    /// The optional leading semicolon keeps the cursor on the same line after input.
    Input {
        /// Optional prompt string.
        prompt: Option<String>,
        /// Whether to show question mark after prompt.
        show_question_mark: bool,
        /// Keep cursor on same line after input (leading semicolon).
        same_line: bool,
        /// Targets to read into (variables, array elements, or fields).
        targets: Vec<InputTarget>,
    },

    /// `LINE INPUT [;]["prompt";] variable$`
    LineInput {
        /// If true, suppress the newline after user presses Enter (the leading semicolon).
        suppress_newline: bool,
        /// Optional prompt string.
        prompt: Option<String>,
        /// Target to read into (variable or array element, must be string type).
        target: InputTarget,
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
    /// TIMER is just a function call in the seed expression (e.g., RANDOMIZE TIMER).
    Randomize {
        /// Seed expression. None means no seed provided (implementation may prompt user or use a default).
        seed: Option<Expr>,
    },

    /// Label definition: `labelName:`
    Label {
        /// Label name.
        name: String,
    },

    /// `SUB name [(parameters)] ... END SUB`
    SubDefinition {
        /// Subroutine name.
        name: String,
        /// Parameter list.
        params: Vec<Parameter>,
        /// Subroutine body statements.
        body: Vec<Statement>,
        /// Whether this is a STATIC SUB (variables persist between calls).
        is_static: bool,
    },

    /// `FUNCTION name [(parameters)] ... END FUNCTION`
    FunctionDefinition {
        /// Function name.
        name: String,
        /// Parameter list.
        params: Vec<Parameter>,
        /// Optional return type specification (if not specified, inferred from name suffix).
        return_type: Option<TypeSpec>,
        /// Function body statements.
        body: Vec<Statement>,
        /// Whether this is a STATIC FUNCTION (variables persist between calls).
        is_static: bool,
    },

    /// `TYPE TypeName [CUSTOMTYPE] ... END TYPE` - User-defined type definition
    ///
    /// Example:
    /// ```basic
    /// TYPE Person
    ///     name AS STRING * 20
    ///     age AS INTEGER
    /// END TYPE
    ///
    /// TYPE CStruct CUSTOMTYPE
    ///     value AS LONG
    /// END TYPE
    /// ```
    TypeDefinition {
        /// The name of the user-defined type.
        name: String,
        /// The members of the type.
        members: Vec<TypeMember>,
        /// QB4.5 CUSTOMTYPE modifier - indicates C-compatible memory layout
        custom_type: bool,
    },

    /// `CALL SubName(args)` or `SubName args`
    Call {
        /// Name of the SUB to call.
        name: String,
        /// Arguments passed to the SUB.
        args: Vec<Expr>,
    },

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
    /// `OPEN filename [FOR mode] [ACCESS access] [lock] AS [#]filenum [LEN=reclen]`
    ///
    /// Opens a file for reading, writing, or both.
    /// FOR mode may be omitted for COM ports: `OPEN "COM1:9600,N,8,1" AS #1`.
    OpenFile {
        /// The filename expression (usually a string, or COM port spec).
        filename: Expr,
        /// The file mode (Input, Output, Append, Binary, Random). None when FOR is omitted (e.g. COM).
        mode: Option<FileMode>,
        /// Optional access mode (Read, Write, ReadWrite).
        access: Option<FileAccess>,
        /// Optional lock mode (Shared, Read, Write, ReadWrite).
        lock: Option<FileLock>,
        /// The file number expression (1-511).
        file_num: Expr,
        /// Optional record length for random access files.
        record_len: Option<Expr>,
    },

    /// Legacy `OPEN mode$, [#]filenum, filename[, reclen]` syntax
    ///
    /// Opens a file using the GW-BASIC shorthand syntax.
    /// mode$ is a string: "O" (OUTPUT), "I" (INPUT), "A" (APPEND), "R" (RANDOM), "B" (BINARY)
    OpenFileLegacy {
        /// The mode expression (typically a string literal like "O").
        mode_expr: Expr,
        /// The file number expression (1-511).
        file_num: Expr,
        /// The filename expression.
        filename: Expr,
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

    /// `LOCK [#]filenum` - file locking (stub: no-op in inline runtime).
    LockFile {
        /// File number to lock.
        file_num: Expr,
    },

    /// `UNLOCK [#]filenum` - file unlocking (stub: no-op in inline runtime).
    UnlockFile {
        /// File number to unlock.
        file_num: Expr,
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
        /// Target to read into (variable or array element, must be string type).
        target: InputTarget,
    },

    /// `GET [#]filenum, [position], variable` or `GET #filenum, , variable`
    ///
    /// Reads data from a binary or random access file.
    FileGet {
        /// The file number.
        file_num: Expr,
        /// Optional position (record number for random, byte position for binary).
        position: Option<Expr>,
        /// Target to read into (variable, array element, or field).
        target: InputTarget,
    },

    /// `PUT [#]filenum, [position], variable` or `PUT #filenum, , variable`
    ///
    /// Writes data to a binary or random access file.
    FilePut {
        /// The file number.
        file_num: Expr,
        /// Optional position (record number for random, byte position for binary).
        position: Option<Expr>,
        /// Target variable containing data to write.
        target: InputTarget,
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

    /// `DEF FNname[(parameters)] ... END DEF` (multi-line, QB64 extension)
    ///
    /// Multi-line user-defined function. The return value is set by assigning
    /// to the function name within the body (e.g., `FNname = value`).
    DefFnMultiLine {
        /// Function name (without FN prefix).
        name: String,
        /// Optional parameters.
        params: Vec<Parameter>,
        /// The function body statements.
        body: Vec<Statement>,
    },

    /// `DEF SEG [= segment]` - Set memory segment for PEEK/POKE/BLOAD/BSAVE.
    ///
    /// Without an argument, resets to the default data segment.
    DefSeg {
        /// Optional segment address expression.
        segment: Option<Expr>,
    },

    /// `POKE address, value` - Write a byte to memory at the specified address.
    ///
    /// Writes the low byte of `value` to the memory address within the
    /// current segment (set by DEF SEG). The address is a 16-bit offset.
    Poke {
        /// Memory address (offset within current segment).
        address: Expr,
        /// Value to write (only low byte is used, 0-255).
        value: Expr,
    },

    /// `_MEMPUT mem, offset, value AS type` - Write typed value to memory (QB64).
    ///
    /// Writes a value to a memory block at the given offset, interpreting
    /// the value as the specified type.
    MemPutTyped {
        /// The memory block (_MEM) to write to.
        mem: Expr,
        /// The byte offset within the memory block.
        offset: Expr,
        /// The value to write.
        value: Expr,
        /// The type to interpret the value as (as a string like "INTEGER", "_INTEGER64").
        value_type: String,
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

    /// `STATIC var1[, var2, ...] [AS type]` inside SUB/FUNCTION
    ///
    /// Declares static local variables that persist between procedure calls.
    /// Unlike regular local variables, static variables retain their values.
    StaticStmt {
        /// Variables to declare as static
        variables: Vec<DimVariable>,
    },

    /// `REDIM [_PRESERVE] array1(dims) [AS type], array2(dims) [AS type], ...`
    ///
    /// Resizes dynamic arrays, optionally preserving contents.
    /// Multiple arrays can be specified on one line.
    Redim {
        /// Whether to preserve existing contents.
        preserve: bool,
        /// Whether SHARED was specified (module-level visibility).
        shared: bool,
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

    /// `$CONSOLE` or `$CONSOLE:ONLY` - enable console window
    ///
    /// - `$CONSOLE` enables a console window in addition to the graphics window
    /// - `$CONSOLE:ONLY` runs as a console-only application (no graphics window)
    MetaConsole {
        /// If true, run as console-only (no graphics window)
        only: bool,
    },

    /// `$SCREENHIDE` - hide the graphics window on startup
    ///
    /// Used with `$CONSOLE:ONLY` to create console-only programs
    MetaScreenHide,

    /// `$SCREENSHOW` - show the graphics window on startup (default behavior)
    MetaScreenShow,

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

    /// `COLOR [foreground][, background][, border]]` - Set text/drawing colors
    ///
    /// All parameters are optional. Omitting a parameter keeps the current value.
    /// In text mode, the third parameter sets the border color (CGA/EGA).
    Color {
        /// Foreground color (optional - omit to keep current)
        foreground: Option<Expr>,
        /// Background color (optional)
        background: Option<Expr>,
        /// Border color (optional, text mode only)
        border: Option<Expr>,
    },

    /// `LOCATE [row][, col][, cursor][, start, stop]` - Position cursor
    ///
    /// All parameters are optional. If row/col omitted, they are unchanged.
    Locate {
        /// Row (1-based, optional)
        row: Option<Expr>,
        /// Column (1-based, optional)
        col: Option<Expr>,
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

    /// `PSET [STEP](x, y)[, color]` - Plot pixel
    Pset {
        /// Whether coordinates are relative (STEP)
        step: bool,
        /// X coordinate
        x: Expr,
        /// Y coordinate
        y: Expr,
        /// Color (optional, uses foreground if not specified)
        color: Option<Expr>,
    },

    /// `PRESET [STEP](x, y)` - Plot pixel with background color
    Preset {
        /// Whether coordinates are relative (STEP)
        step: bool,
        /// X coordinate
        x: Expr,
        /// Y coordinate
        y: Expr,
    },

    /// `LINE [(x1, y1)]-[STEP](x2, y2)[, color][, B|BF[, style]]` - Draw line or box
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
        /// Line style pattern (16-bit, e.g., &HCCCC for dashed) - only valid with B/BF
        style: Option<Expr>,
    },

    /// `CIRCLE [STEP](x, y), radius[, color][, start][, end][, aspect][, F]`
    Circle {
        /// Whether STEP was used (relative to last graphics position)
        step: bool,
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

    /// `PAINT [STEP](x, y)[, color][, border]` - Flood fill
    Paint {
        /// Whether STEP was used (relative to last graphics position)
        step: bool,
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

    /// `_CONTROLCHR ON|OFF` - Control printing of control characters
    ///
    /// When OFF, CHR$(0-31) values are printed as characters instead of
    /// performing their control functions (like cursor movement).
    ControlChr {
        /// True for ON (normal behavior), false for OFF
        enabled: bool,
    },

    /// `_MAPUNICODE unicode_code TO character_position` - Map Unicode codepoint
    ///
    /// Maps a Unicode codepoint to a position in the current font's character set.
    MapUnicode {
        /// Unicode codepoint value
        unicode_value: Expr,
        /// Character position (0-255)
        char_position: Expr,
    },

    /// `_RESIZE ON|OFF` - Enable/disable window resizing at runtime
    ///
    /// Controls whether the user can resize the graphics window.
    /// Unlike `$RESIZE:ON` which is compile-time, this is runtime control.
    GfxResize {
        /// True for ON (enable resizing), false for OFF (disable)
        enabled: bool,
    },

    /// `PALETTE [attribute, color]` - Set palette colors
    ///
    /// In screen modes that use palettes (e.g., SCREEN 12, 13), this statement
    /// maps a color attribute to an actual color value.
    /// - `PALETTE` alone resets all palette entries to defaults
    /// - `PALETTE attr, color` sets a single palette entry
    Palette {
        /// Color attribute/index (0-255 depending on mode)
        attribute: Option<Expr>,
        /// Color value to assign to this attribute
        color: Option<Expr>,
    },

    /// `PCOPY source%, dest%` - Copy screen page
    ///
    /// Copies the contents of one video page to another.
    /// Useful for double-buffering and animation.
    Pcopy {
        /// Source page number
        source: Expr,
        /// Destination page number
        dest: Expr,
    },

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

    /// `GET [STEP](x1, y1)-[STEP](x2, y2), array[(index)]` - Capture screen region to array
    ///
    /// Captures a rectangular screen region into an array for later use with PUT.
    /// The array must be large enough to hold the captured image data.
    GraphicsGet {
        /// Whether x1,y1 are relative to last graphics point (STEP)
        step1: bool,
        /// First corner X coordinate
        x1: Expr,
        /// First corner Y coordinate
        y1: Expr,
        /// Whether x2,y2 are relative (STEP)
        step2: bool,
        /// Second corner X coordinate (or width if step2 is true)
        x2: Expr,
        /// Second corner Y coordinate (or height if step2 is true)
        y2: Expr,
        /// Array name to store the captured image
        array_name: String,
        /// Optional array indices for storing in array (supports multi-dimensional)
        array_indices: Vec<Expr>,
    },

    /// `PUT (x, y), array[(indices)][, action]` - Draw array contents to screen
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
        /// Optional array indices (supports multi-dimensional arrays)
        array_indices: Vec<Expr>,
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

    /// `_PUTIMAGE [(dx1,dy1)-(dx2,dy2)][, src&][, dest&][, (sx1,sy1)-(sx2,sy2)][, _SMOOTH|_STRETCH]`
    PutImage {
        /// Destination coordinates (optional) - boxed to reduce enum size
        dest_coords: Option<Box<ViewCoords>>,
        /// Source image handle (optional, defaults to _SOURCE)
        source: Option<Expr>,
        /// Destination image handle (optional, defaults to _DEST)
        dest: Option<Expr>,
        /// Source coordinates (optional) - boxed to reduce enum size
        source_coords: Option<Box<ViewCoords>>,
        /// Scaling mode: _SMOOTH (bilinear) or _STRETCH (nearest-neighbor)
        scale_mode: ImageScaleMode,
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

    /// `_SNDBAL handle&, [x!], [y!], [z!], [channel&]` - Set stereo balance/3D position
    SndBal {
        /// Sound handle
        handle: Expr,
        /// X position (or simple balance if y, z not given)
        x: Option<Expr>,
        /// Y position (3D positioning)
        y: Option<Expr>,
        /// Z position (3D positioning)
        z: Option<Expr>,
        /// Optional channel
        channel: Option<Expr>,
    },

    /// `_SNDRAW sample!` or `_SNDRAW left!, right!` - Write raw audio samples
    SndRaw {
        /// Left channel sample (or mono)
        left: Expr,
        /// Right channel sample (optional for stereo)
        right: Option<Expr>,
    },

    /// `_SNDPLAYFILE filename$[, volume!][, x!][, y!][, z!]` - Play a sound file directly
    SndPlayFile {
        /// The filename to play
        filename: Expr,
        /// Optional volume (0.0 to 1.0)
        volume: Option<Expr>,
        /// Optional 3D x position
        x: Option<Expr>,
        /// Optional 3D y position
        y: Option<Expr>,
        /// Optional 3D z position
        z: Option<Expr>,
    },

    /// `_SNDPLAYCOPY handle&[, volume!]` - Play a copy of a sound
    SndPlayCopy {
        /// Sound handle to copy and play
        handle: Expr,
        /// Optional volume
        volume: Option<Expr>,
    },

    /// `_SNDSETPOS handle&, position!` - Set playback position
    SndSetPos {
        /// Sound handle
        handle: Expr,
        /// Position in seconds
        position: Expr,
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

    /// `ENVIRON "name=value"` - Set an environment variable
    ///
    /// Sets an environment variable for the current process.
    /// The argument should be a string in the format "name=value".
    Environ {
        /// The environment variable assignment string (e.g., "PATH=/usr/bin")
        env_string: Expr,
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

    /// `BLOAD filename$[, address]` - Load binary file to memory
    ///
    /// Loads a file previously created with BSAVE into memory.
    /// In the original BASIC, this loaded directly into video memory or a segment.
    /// In modern systems, this is primarily useful for loading binary data.
    Bload {
        /// The filename to load from
        filename: Expr,
        /// Optional memory address/offset (if not specified, uses BSAVE header)
        address: Option<Expr>,
    },

    /// `BSAVE filename$, address, length` - Save memory to binary file
    ///
    /// Saves a region of memory to a binary file.
    /// Originally used for saving screen contents or program data.
    Bsave {
        /// The filename to save to
        filename: Expr,
        /// Starting memory address
        address: Expr,
        /// Number of bytes to save
        length: Expr,
    },

    /// `SETMEM bytes` - Set available memory for BASIC strings
    ///
    /// In original BASIC, this controlled memory allocation for string space.
    /// In modern systems with virtual memory, this is a no-op stub for compatibility.
    Setmem {
        /// Number of bytes to reserve (ignored)
        bytes: Expr,
    },

    /// `CALL ABSOLUTE address` - Call machine language routine
    ///
    /// Legacy statement for calling machine code at a specific memory address.
    /// This is unsafe and not meaningfully implementable in modern systems.
    /// Parsed for compatibility but generates a warning at runtime.
    /// Syntax: `CALL ABSOLUTE(arg1, arg2, ..., address)`
    CallAbsolute {
        /// Arguments to pass (excluding the address)
        args: Vec<Expr>,
        /// Memory address of the routine to call (last argument)
        address: Expr,
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

    /// `DECLARE FUNCTION name [(parameters)] [AS type]`
    ///
    /// Forward declaration of a function. Similar to DECLARE SUB but for
    /// functions that return values.
    ///
    /// Example:
    /// ```basic
    /// DECLARE FUNCTION AddNumbers% (a AS INTEGER, b AS INTEGER)
    /// DECLARE FUNCTION GetName (id AS LONG) AS STRING
    /// ```
    DeclareFunction {
        /// The name of the function.
        name: String,
        /// Parameter declarations.
        params: Vec<DeclareParam>,
        /// Optional explicit return type (AS type).
        return_type: Option<TypeSpec>,
    },

    // ==================== Phase 7: Additional QB4.5/QB64 Statements ====================
    /// `RUN [filename$]` or `RUN [linenumber]` - Restart program or run another program
    ///
    /// Without arguments, restarts the current program from the beginning.
    /// With a filename, runs that program. With a line number, restarts at that line.
    Run {
        /// Optional target: filename string or line number/label
        target: Option<Expr>,
    },

    /// `CHAIN filename$` - Run another program, optionally passing COMMON variables
    ///
    /// Runs another BASIC program. Variables declared with COMMON can be passed
    /// to the chained program.
    Chain {
        /// The filename of the program to chain to
        filename: Expr,
    },

    /// `TRON` - Enable trace mode for debugging
    ///
    /// When trace mode is enabled, line numbers are printed as they execute.
    Tron,

    /// `TROFF` - Disable trace mode
    Troff,

    /// `LPRINT [expression [{;|,} expression]...]` - Print to printer (LPT1)
    ///
    /// Similar to PRINT but sends output to the printer instead of screen.
    Lprint {
        /// Values to print
        values: Vec<PrintItem>,
        /// Whether to print a newline at the end
        newline: bool,
    },

    /// `FILES [filespec$]` - Display directory listing
    ///
    /// Displays files matching the filespec (supports wildcards).
    /// Without arguments, displays files in the current directory.
    FilesStmt {
        /// Optional file specification (e.g., "*.BAS")
        filespec: Option<Expr>,
    },

    /// `FIELD [#]filenum, width AS var$ [, width AS var$]...` - Define random file fields
    ///
    /// Allocates space in a random access file buffer for field variables.
    FieldStmt {
        /// File number
        file_num: Expr,
        /// List of (width, variable_name) pairs
        fields: Vec<FieldSpec>,
    },

    /// `LSET var$ = string$` - Left-align string in field buffer
    ///
    /// Assigns a string value left-aligned within a field variable, padding with spaces.
    Lset {
        /// Target field variable
        variable: String,
        /// Value to assign
        value: Expr,
    },

    /// `RSET var$ = string$` - Right-align string in field buffer
    ///
    /// Assigns a string value right-aligned within a field variable, padding with spaces.
    Rset {
        /// Target field variable
        variable: String,
        /// Value to assign
        value: Expr,
    },

    /// `ON KEY(n) GOSUB label` - Set up key event handler
    ///
    /// Defines a subroutine to call when a specific key is pressed.
    OnKey {
        /// Key number (1-14 for function keys, 15-25 for user keys)
        key_num: Expr,
        /// Label to GOSUB when key is pressed
        target: String,
    },

    /// `KEY(n) ON|OFF|STOP` - Enable/disable/suspend key event trapping
    KeyControl {
        /// Key number
        key_num: Expr,
        /// Control mode
        mode: EventControlMode,
    },

    /// `ON TIMER(seconds) GOSUB label` - Set up timer event handler
    ///
    /// Defines a subroutine to call after the specified interval.
    OnTimer {
        /// Timer interval in seconds
        interval: Expr,
        /// Label to GOSUB when timer fires
        target: String,
    },

    /// `TIMER ON|OFF|STOP` - Enable/disable/suspend timer event trapping
    TimerControl {
        /// Control mode
        mode: EventControlMode,
    },

    /// `STRIG(n) ON|OFF|STOP` - Enable/disable joystick trigger events
    StrigControl {
        /// Button number
        button_num: Expr,
        /// Control mode
        mode: EventControlMode,
    },

    /// `ON STRIG(n) GOSUB label` - Set up joystick trigger event handler
    OnStrig {
        /// Button number
        button_num: Expr,
        /// Label to GOSUB
        target: String,
    },

    /// `ON COM(n) GOSUB label` - Set up serial port event handler
    ///
    /// Defines a subroutine to call when data is received on a serial port.
    OnCom {
        /// COM port number (1-4)
        port_num: Expr,
        /// Label to GOSUB when data is received
        target: String,
    },

    /// `COM(n) ON|OFF|STOP` - Enable/disable/suspend serial port event trapping
    ComControl {
        /// COM port number
        port_num: Expr,
        /// Control mode
        mode: EventControlMode,
    },

    /// `ON PEN GOSUB label` - Set up light pen event handler
    ///
    /// Defines a subroutine to call when the light pen is activated.
    OnPen {
        /// Label to GOSUB when pen is activated
        target: String,
    },

    /// `PEN ON|OFF|STOP` - Enable/disable/suspend light pen event trapping
    PenControl {
        /// Control mode
        mode: EventControlMode,
    },

    /// `ON UEVENT GOSUB label` - Set up user-defined event handler
    ///
    /// Defines a subroutine to call when a user event is triggered.
    OnUevent {
        /// Label to GOSUB when event fires
        target: String,
    },

    /// `UEVENT ON|OFF|STOP` - Enable/disable/suspend user event trapping
    UeventControl {
        /// Control mode
        mode: EventControlMode,
    },

    /// `UEVENT` - Trigger a user-defined event
    UeventTrigger,

    /// `ON SIGNAL(n) GOSUB label` - Set up signal event handler
    ///
    /// Defines a subroutine to call when a system signal is received.
    OnSignal {
        /// Signal number
        signal_num: Expr,
        /// Label to GOSUB when signal is received
        target: String,
    },

    /// `SIGNAL(n) ON|OFF|STOP` - Enable/disable/suspend signal event trapping
    SignalControl {
        /// Signal number
        signal_num: Expr,
        /// Control mode
        mode: EventControlMode,
    },

    /// `OUT port, value` - Write byte to I/O port
    ///
    /// Writes a byte to a hardware I/O port. This is a legacy feature that
    /// may be sandboxed or disabled for security reasons.
    OutPort {
        /// Port address
        port: Expr,
        /// Value to write (0-255)
        value: Expr,
    },

    /// `INTERRUPT intnum, inregs, outregs` - Call system interrupt
    ///
    /// Legacy statement for invoking DOS/BIOS interrupts. Primarily for
    /// compatibility; most interrupts are not meaningfully implementable
    /// on modern systems.
    InterruptStmt {
        /// Interrupt number
        int_num: Expr,
        /// Input registers (TYPE variable)
        in_regs: String,
        /// Output registers (TYPE variable)
        out_regs: String,
    },

    /// `INTERRUPTX intnum, inregs, outregs` - Extended system interrupt call
    ///
    /// Like INTERRUPT but uses extended registers. Legacy compatibility feature.
    InterruptXStmt {
        /// Interrupt number
        int_num: Expr,
        /// Input registers (TYPE variable)
        in_regs: String,
        /// Output registers (TYPE variable)
        out_regs: String,
    },

    /// `IOCTL [#]filenum, string$` - Send device control string
    ///
    /// Sends control data to a device driver.
    IoctlStmt {
        /// File number of the device
        file_num: Expr,
        /// Control string to send
        control_string: Expr,
    },

    /// `FREE` - Free unused string space
    ///
    /// Releases memory used by discarded strings. In modern systems with
    /// automatic garbage collection, this is a no-op stub.
    FreeStmt,

    /// `CLEAR [stack_size]` - Clear all variables and optionally set stack size
    ///
    /// Resets all variables to their default values.
    ClearStmt {
        /// Optional stack size in bytes
        stack_size: Option<Expr>,
    },

    /// `RESET` - Close all open files
    ///
    /// Equivalent to CLOSE with no arguments.
    ResetStmt,

    // ==================== Window/Desktop Statements (QB64) ====================
    /// `_TITLE text$` - Set window title (QB64)
    TitleStmt {
        /// The title text
        title: Expr,
    },

    /// `_SCREENMOVE x%, y%` or `_SCREENMOVE _MIDDLE` - Move window (QB64)
    ScreenMoveStmt {
        /// X position (or _MIDDLE for centering)
        x: Option<Expr>,
        /// Y position
        y: Option<Expr>,
        /// Whether to center the window
        center: bool,
    },

    /// `_FULLSCREEN [_SQUAREPIXELS|_STRETCH|_OFF]` - Control fullscreen mode (QB64)
    FullScreenStmt {
        /// Fullscreen mode
        mode: FullScreenMode,
    },

    /// `_ALLOWFULLSCREEN [_SQUAREPIXELS|_STRETCH|_ALL|_OFF]` - Set allowed fullscreen modes (QB64)
    AllowFullScreenStmt {
        /// Allowed mode
        mode: AllowFullScreenMode,
    },

    /// `_SCREENICON` - Minimize window to taskbar (QB64)
    ScreenIconStmt,

    /// `_ICON [handle&]` - Set window icon from image handle (QB64)
    IconStmt {
        /// Optional image handle (uses embedded icon if not specified)
        handle: Option<Expr>,
    },

    /// `_SCREENHIDE` - Hide graphics window (QB64)
    ScreenHideStmt,

    /// `_SCREENSHOW` - Show graphics window (QB64)
    ScreenShowStmt,

    /// `_CONSOLETITLE text$` - Set console window title (QB64)
    ConsoleTitleStmt {
        /// The title text
        title: Expr,
    },

    /// `_CONSOLE ON|OFF` - Show/hide console window (QB64)
    ConsoleStmt {
        /// Whether console should be visible
        visible: bool,
    },

    /// `_ASSERT condition [, message$]` - Debug assertion (QB64)
    ///
    /// Checks a condition and triggers an error if false (when $ASSERTS is enabled).
    AssertStmt {
        /// Condition to check
        condition: Expr,
        /// Optional error message
        message: Option<Expr>,
    },

    /// `$ASSERTS` or `$ASSERTS:CONSOLE` - Enable assertion checking
    ///
    /// - `$ASSERTS`: Enables assertions, sets `_ASSERTS_` preprocessor variable to 1
    /// - `$ASSERTS:CONSOLE`: Enables assertions with console output, sets both `_ASSERTS_` and `_CONSOLE_` to 1
    MetaAsserts {
        /// If true, assertion failures are sent to console (stderr)
        console: bool,
    },

    /// `$NOPREFIX` - Allow QB64 keywords without underscore prefix
    MetaNoPrefix,

    /// `$COLOR` - Include named color constants
    MetaColor {
        /// Color depth mode (0 for EGA, 32 for RGBA)
        depth: Option<i64>,
    },

    /// `$RESIZE:ON` / `$RESIZE:OFF` - Enable/disable window resize events
    MetaResize {
        /// If true, resize events are enabled
        enabled: bool,
    },

    /// `$RESIZE:STRETCH` - Stretch graphics when window is resized
    MetaResizeStretch,

    /// `$RESIZE:SMOOTH` - Use smooth scaling when window is resized
    MetaResizeSmooth,

    /// `$STATIC` - Use static arrays (allocated at compile time)
    MetaStatic,

    /// `$DYNAMIC` - Use dynamic arrays (allocated at runtime)
    MetaDynamic,

    /// `$DEBUG` - Enable debug mode
    MetaDebug,

    /// `$INCLUDEONCE` - Include this file only once (prevents multiple inclusion)
    MetaIncludeOnce,

    /// `$EXEICON:'filename'` - Set the executable icon
    MetaExeIcon {
        /// Path to the icon file
        filename: String,
    },

    /// `$VERSIONINFO:key=value` - Set version info for executable
    MetaVersionInfo {
        /// Version info key (e.g., "CompanyName", "ProductName")
        key: String,
        /// Version info value
        value: String,
    },

    /// `$ERROR message` - Generate a compiler error
    MetaErrorDirective {
        /// Error message to display
        message: String,
    },

    /// `$EMBED:'filename'` - Embed a file into the executable
    MetaEmbed {
        /// Path to the file to embed
        filename: String,
    },

    /// `$MIDISOUNDFONT:'file.sf2'` - Set MIDI soundfont file
    MetaMidiSoundFont {
        /// Path to the soundfont file
        filename: String,
    },

    /// `$UNSTABLE:feature` - Enable an unstable/experimental feature
    MetaUnstable {
        /// Name of the feature to enable
        feature: String,
    },

    /// `$FORMAT` - Code formatting directive (no-op, for IDE support)
    MetaFormat,

    /// `$USELIBRARY:'library'` - Use an external library
    MetaUseLibrary {
        /// Path or name of the library
        library: String,
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
    /// ONLY - exclusive file access (QB4.5 syntax)
    /// Prevents any other process from opening the file
    Only,
}

/// Field specification for FIELD statement.
#[derive(Debug, Clone)]
pub struct FieldSpec {
    /// Width of the field in bytes.
    pub width: Expr,
    /// Variable name for this field.
    pub variable: String,
}

/// Image scaling mode for _PUTIMAGE statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ImageScaleMode {
    /// Default scaling (no explicit mode specified)
    #[default]
    Default,
    /// _SMOOTH - bilinear interpolation for smooth scaling
    Smooth,
    /// _STRETCH - nearest-neighbor scaling (no interpolation)
    Stretch,
}

/// Event control mode for KEY(n), TIMER, STRIG, etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventControlMode {
    /// Enable event trapping
    On,
    /// Disable event trapping
    Off,
    /// Suspend event trapping (events are remembered)
    Stop,
}

/// Fullscreen mode for _FULLSCREEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FullScreenMode {
    /// Stretch to fill screen (may distort aspect ratio)
    Stretch,
    /// Maintain square pixels (letterbox if needed)
    SquarePixels,
    /// Exit fullscreen mode
    Off,
}

/// Allowed fullscreen modes for _ALLOWFULLSCREEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllowFullScreenMode {
    /// Allow stretch mode only
    Stretch,
    /// Allow square pixels mode only
    SquarePixels,
    /// Allow all fullscreen modes
    All,
    /// Disallow fullscreen
    Off,
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
    ArrayElement {
        /// Array variable name.
        name: String,
        /// Index expressions for each dimension.
        indices: Vec<Expr>,
    },
    /// Array element field access: `arr(i).field` or `arr(i).field.subfield`
    ArrayElementField {
        /// Array variable name.
        name: String,
        /// Index expressions for each dimension.
        indices: Vec<Expr>,
        /// Field path (e.g., ["x", "y"] for `arr(i).x.y`).
        fields: Vec<String>,
    },
    /// Simple UDT field access: `udt.field`
    Field {
        /// UDT variable name.
        name: String,
        /// Field path (e.g., ["x", "y"] for `udt.x.y`).
        fields: Vec<String>,
    },
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

/// A single variable in a DIM or REDIM statement.
#[derive(Debug, Clone)]
pub struct DimVariable {
    /// Variable name.
    pub name: String,
    /// Array dimensions (empty if not an array, or dynamic array `DIM a()`).
    pub dimensions: Vec<ArrayDimension>,
    /// Type specification (if AS clause present).
    pub type_spec: Option<TypeSpec>,
    /// True when parentheses were present but empty (`DIM a() AS LONG` = dynamic array).
    /// Distinguishes dynamic array from scalar when `dimensions` is empty.
    pub is_dynamic_array: bool,
}

/// Type specification for DIM statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    // Standard QBasic types
    /// 16-bit signed integer.
    Integer,
    /// 32-bit signed integer.
    Long,
    /// 32-bit floating point.
    Single,
    /// 64-bit floating point.
    Double,
    /// Variable-length string.
    String,
    /// Fixed-length string: `STRING * n`
    FixedString(usize),

    // QB64 extended types
    /// 8-bit unsigned integer.
    Byte,
    /// Single bit (0 or 1).
    Bit,
    /// 64-bit signed integer.
    Integer64,
    /// Alias for Single (32-bit float).
    Float,
    /// Memory offset type.
    Offset,
    /// Unsigned variant of the wrapped type.
    Unsigned(Box<TypeSpec>),
    /// _MEM - Memory block descriptor
    Mem,

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
    Range {
        /// Lower bound of the range.
        from: Expr,
        /// Upper bound of the range.
        to: Expr,
    },
    /// Comparison: `CASE IS > 5`
    Comparison {
        /// Comparison operator.
        op: CaseCompareOp,
        /// Value to compare against.
        value: Expr,
    },
}

/// Comparison operators allowed in CASE IS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseCompareOp {
    /// `=`
    Equal,
    /// `<>`
    NotEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessEqual,
    /// `>`
    GreaterThan,
    /// `>=`
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
    /// `EXIT FOR`
    For,
    /// `EXIT WHILE`
    While,
    /// `EXIT DO`
    Do,
    /// `EXIT SUB`
    Sub,
    /// `EXIT FUNCTION`
    Function,
    /// `EXIT SELECT`
    Select,
}

/// Continue statement type (QB64 _CONTINUE).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContinueType {
    /// Continue the innermost FOR loop
    For,
    /// Continue the innermost WHILE loop
    While,
    /// Continue the innermost DO loop
    Do,
    /// Continue the innermost loop of any type (bare _CONTINUE)
    Innermost,
}

/// Target for READ statement - can be a variable or array element.
#[derive(Debug, Clone)]
pub enum ReadTarget {
    /// Simple variable: `READ x`
    Variable(String),
    /// Array element: `READ arr(i, j)`
    ArrayElement {
        /// Array variable name.
        name: String,
        /// Index expressions for each dimension.
        indices: Vec<Expr>,
    },
    /// UDT array element field: `READ arr(i).field`
    ArrayFieldElement {
        /// Array variable name.
        name: String,
        /// Index expressions for each dimension.
        indices: Vec<Expr>,
        /// Field name to access.
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
    /// Array dimensions (empty if scalar field). e.g. `arr(1 TO 3) AS LONG` has one dimension.
    pub dimensions: Vec<ArrayDimension>,
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
            Span::new(0, 5, 1),
        );
        assert!(matches!(
            stmt.kind,
            StatementKind::Print { newline: true, .. }
        ));
    }
}
