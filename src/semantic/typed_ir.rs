//! Typed intermediate representation for code generation.
//!
//! This module defines the output of semantic analysis - an AST annotated with
//! type information. Each expression carries its inferred type, and the IR is
//! ready for consumption by the code generator.
//!
//! # Design Principles
//!
//! - **Mirrors the AST structure**: Easy to traverse similarly to the parser output
//! - **Type-annotated**: Every expression knows its type
//! - **Explicit conversions**: Implicit conversions become explicit `Convert` nodes
//! - **Ready for codegen**: No further analysis needed

use crate::ast::{
    AllowFullScreenMode, BinaryOp, ContinueType, EventControlMode, ExitType, FileAccess, FileLock,
    FileMode, FullScreenMode, ImageScaleMode, PrintSeparator, ResumeTarget, Span, UnaryOp,
};
use crate::semantic::types::BasicType;

/// A type-annotated expression.
///
/// Every expression in the typed IR carries its inferred type and source location.
#[derive(Debug, Clone)]
pub struct TypedExpr {
    /// The expression kind with typed sub-expressions.
    pub kind: TypedExprKind,
    /// The type of this expression's value.
    pub basic_type: BasicType,
    /// Source location for error reporting and debugging.
    pub span: Span,
}

/// The different kinds of typed expressions.
#[derive(Debug, Clone)]
pub enum TypedExprKind {
    /// Integer literal: `42`, `&HFF`
    IntegerLiteral(i64),

    /// Floating-point literal: `3.14`
    FloatLiteral(f64),

    /// String literal: `"Hello"`
    StringLiteral(String),

    /// Variable reference (resolved to a symbol).
    Variable(String),

    /// Binary operation with typed operands.
    Binary {
        left: Box<TypedExpr>,
        op: BinaryOp,
        right: Box<TypedExpr>,
    },

    /// Unary operation with typed operand.
    Unary {
        op: UnaryOp,
        operand: Box<TypedExpr>,
    },

    /// Parenthesized expression (preserved for source fidelity).
    Grouped(Box<TypedExpr>),

    /// Function call with typed arguments.
    FunctionCall { name: String, args: Vec<TypedExpr> },

    /// Array access with typed indices.
    ///
    /// For multi-dimensional arrays, the dimensions field contains the
    /// bounds of each dimension, which is needed for calculating the
    /// linear index in row-major order.
    ArrayAccess {
        name: String,
        indices: Vec<TypedExpr>,
        /// Dimension bounds for index calculation (empty for 1D arrays).
        dimensions: Vec<TypedArrayDimension>,
    },

    /// Array reference (passing whole array to a procedure).
    ///
    /// In BASIC, `arr()` means "the entire array", used when passing
    /// arrays as arguments to SUB/FUNCTION. This is different from
    /// `arr(i)` which accesses a single element.
    ArrayRef {
        name: String,
        /// The element type of the array.
        element_type: BasicType,
        /// Dimension information for the array.
        dimensions: Vec<TypedArrayDimension>,
    },

    /// Explicit type conversion (inserted by type checker for implicit conversions).
    ///
    /// The inner expression's type is converted to `to_type`.
    Convert {
        expr: Box<TypedExpr>,
        to_type: BasicType,
    },

    /// Field access on a user-defined type.
    ///
    /// Example: `person.name`, `record.data.value`
    FieldAccess {
        /// The object expression being accessed.
        object: Box<TypedExpr>,
        /// The field name.
        field: String,
    },
}

impl TypedExpr {
    /// Creates a new typed expression.
    pub fn new(kind: TypedExprKind, basic_type: BasicType, span: Span) -> Self {
        Self {
            kind,
            basic_type,
            span,
        }
    }

    /// Creates an integer literal expression.
    pub fn integer(value: i64, span: Span) -> Self {
        Self::new(TypedExprKind::IntegerLiteral(value), BasicType::Long, span)
    }

    /// Creates a float literal expression.
    pub fn float(value: f64, span: Span) -> Self {
        Self::new(TypedExprKind::FloatLiteral(value), BasicType::Double, span)
    }

    /// Creates a string literal expression.
    pub fn string(value: String, span: Span) -> Self {
        Self::new(TypedExprKind::StringLiteral(value), BasicType::String, span)
    }

    /// Wraps this expression in a conversion node if needed.
    pub fn convert_to(self, target: BasicType) -> Self {
        if self.basic_type == target {
            self
        } else {
            let span = self.span;
            Self::new(
                TypedExprKind::Convert {
                    expr: Box::new(self),
                    to_type: target.clone(),
                },
                target,
                span,
            )
        }
    }
}

/// A type-checked statement.
#[derive(Debug, Clone)]
pub struct TypedStatement {
    /// The statement kind with typed sub-expressions.
    pub kind: TypedStatementKind,
    /// Source location.
    pub span: Span,
}

/// The different kinds of typed statements.
#[derive(Debug, Clone)]
pub enum TypedStatementKind {
    /// Variable assignment with target type.
    Assignment {
        name: String,
        value: TypedExpr,
        /// The type of the target variable (for codegen to emit conversion).
        target_type: BasicType,
    },

    /// Array element assignment with target type.
    ArrayAssignment {
        name: String,
        indices: Vec<TypedExpr>,
        value: TypedExpr,
        /// Dimension info for linear index calculation.
        dimensions: Vec<TypedArrayDimension>,
        /// The element type.
        element_type: BasicType,
    },

    /// Array field assignment (UDT member in array).
    ArrayFieldAssignment {
        name: String,
        indices: Vec<TypedExpr>,
        /// Field access chain.
        fields: Vec<String>,
        value: TypedExpr,
        /// Dimension info for linear index calculation.
        dimensions: Vec<TypedArrayDimension>,
        /// The type of the array element (the UDT type).
        element_type: BasicType,
        /// The type of the final field being assigned.
        field_type: BasicType,
    },

    /// MID$ assignment - substring replacement.
    /// `MID$(str$, start [, length]) = value$`
    /// The target can be a simple variable, array element, or UDT field.
    MidAssignment {
        /// Target string expression (variable, array element, or field access).
        target: TypedExpr,
        /// Start position (1-based).
        start: TypedExpr,
        /// Optional length to replace.
        length: Option<TypedExpr>,
        /// Replacement value.
        value: TypedExpr,
    },

    /// PRINT statement with typed items.
    Print {
        items: Vec<TypedPrintItem>,
        newline: bool,
    },

    /// PRINT USING formatted output.
    PrintUsing {
        /// The format string expression.
        format: TypedExpr,
        /// Values to format.
        values: Vec<TypedExpr>,
        /// Whether to print newline.
        newline: bool,
    },

    /// INPUT statement with typed targets.
    Input {
        prompt: Option<String>,
        show_question_mark: bool,
        /// Input targets (variables, array elements, fields) with their types.
        targets: Vec<TypedInputTarget>,
    },

    /// LINE INPUT statement.
    LineInput {
        prompt: Option<String>,
        variable: String,
    },

    /// IF/ELSEIF/ELSE statement.
    If {
        condition: TypedExpr,
        then_branch: Vec<TypedStatement>,
        elseif_branches: Vec<(TypedExpr, Vec<TypedStatement>)>,
        else_branch: Option<Vec<TypedStatement>>,
    },

    /// SELECT CASE statement.
    SelectCase {
        test_expr: TypedExpr,
        cases: Vec<TypedCaseClause>,
        case_else: Option<Vec<TypedStatement>>,
    },

    /// FOR/NEXT loop.
    For {
        variable: String,
        var_type: BasicType,
        start: TypedExpr,
        end: TypedExpr,
        step: Option<TypedExpr>,
        body: Vec<TypedStatement>,
    },

    /// WHILE/WEND loop.
    While {
        condition: TypedExpr,
        body: Vec<TypedStatement>,
    },

    /// DO/LOOP.
    DoLoop {
        pre_condition: Option<TypedDoCondition>,
        body: Vec<TypedStatement>,
        post_condition: Option<TypedDoCondition>,
    },

    /// GOTO statement.
    Goto { target: String },

    /// GOSUB statement.
    Gosub { target: String },

    /// RETURN statement.
    Return,

    /// EXIT statement.
    Exit { exit_type: ExitType },

    /// END statement.
    End,

    /// STOP statement.
    Stop,

    /// SYSTEM statement (exit immediately).
    System,

    /// SLEEP statement (pause execution).
    Sleep {
        /// Optional duration in seconds. If None, waits for keypress.
        seconds: Option<TypedExpr>,
    },

    /// WAIT statement (wait for hardware port condition).
    Wait {
        /// Port address to check.
        port: TypedExpr,
        /// AND mask for port value.
        and_mask: TypedExpr,
        /// Optional XOR mask (invert bits).
        xor_mask: Option<TypedExpr>,
    },

    /// _DELAY statement (pause execution, QB64).
    Delay {
        /// Duration in seconds (float).
        seconds: TypedExpr,
    },

    /// _LIMIT statement (limit frame rate, QB64).
    Limit {
        /// Target frames per second.
        fps: TypedExpr,
    },

    /// ERASE statement (clear/deallocate arrays).
    Erase {
        /// List of array names to erase.
        arrays: Vec<String>,
    },

    /// _KEYCLEAR statement (clear keyboard buffer, QB64).
    KeyClear,

    /// SUB procedure call.
    Call { name: String, args: Vec<TypedExpr> },

    /// SUB definition.
    SubDefinition {
        name: String,
        params: Vec<TypedParameter>,
        body: Vec<TypedStatement>,
        is_static: bool,
    },

    /// FUNCTION definition.
    FunctionDefinition {
        name: String,
        params: Vec<TypedParameter>,
        return_type: BasicType,
        body: Vec<TypedStatement>,
        is_static: bool,
    },

    /// DIM statement (variable/array declaration) - may declare multiple variables.
    Dim {
        /// List of variables declared by this DIM statement
        variables: Vec<TypedDimVariable>,
        /// Whether SHARED was specified
        shared: bool,
    },

    /// CONST statement(s) - may define multiple constants on one line.
    Const {
        /// List of (name, typed_value, basic_type) for each constant
        definitions: Vec<(String, TypedExpr, BasicType)>,
    },

    /// DEFxxx statement (DEFINT, DEFLNG, DEFSNG, DEFDBL, DEFSTR).
    /// This statement only affects the symbol table and generates no code.
    DefType,

    /// OPTION BASE statement - sets default array lower bound.
    /// This statement only affects the symbol table and generates no code.
    OptionBase,

    /// DEF SEG statement - set memory segment for PEEK/POKE/BLOAD/BSAVE.
    /// In modern QB64, this is largely a no-op but must be parsed for compatibility.
    DefSeg {
        /// Optional segment address expression.
        segment: Option<TypedExpr>,
    },

    /// POKE statement - write a byte to memory address.
    Poke {
        /// Memory address (offset within current segment).
        address: TypedExpr,
        /// Value to write (only low byte is used, 0-255).
        value: TypedExpr,
    },

    /// Label definition.
    Label { name: String },

    /// Comment (preserved for documentation).
    Comment(String),

    /// Expression used as statement (e.g., bare function call).
    Expression(TypedExpr),

    // ==================== Preprocessor Directives ====================
    /// $INCLUDE directive (file inclusion).
    IncludeDirective {
        /// Path to include.
        path: String,
    },

    /// $IF conditional compilation block (unevaluated - all branches preserved).
    /// This variant is used when conditions cannot be evaluated at compile time.
    ConditionalBlock {
        /// The compile-time condition.
        condition: String,
        /// Statements in the $IF block.
        then_branch: Vec<TypedStatement>,
        /// $ELSEIF clauses.
        elseif_branches: Vec<(String, Vec<TypedStatement>)>,
        /// $ELSE block.
        else_branch: Option<Vec<TypedStatement>>,
    },

    /// $IF conditional compilation block (evaluated - only selected branch).
    /// This variant is used when the condition was evaluated at compile time
    /// and only the matching branch's statements are included.
    ConditionalBlockResolved {
        /// The original condition that was true (for debug comments).
        original_condition: String,
        /// Only the statements from the selected branch.
        statements: Vec<TypedStatement>,
    },

    /// Other meta-command directive.
    MetaCommand {
        /// Command name without $.
        command: String,
        /// Command arguments.
        args: Option<String>,
    },

    /// $LET compile-time variable assignment.
    MetaLet {
        /// Variable name.
        name: String,
        /// Value (integer).
        value: i64,
    },

    /// $CHECKING:ON/OFF directive.
    MetaChecking {
        /// Whether bounds checking is enabled.
        enabled: bool,
    },

    /// $CONSOLE or $CONSOLE:ONLY directive.
    MetaConsole {
        /// If true, console-only mode (no graphics window).
        only: bool,
    },

    /// $SCREENHIDE directive - hide graphics window on startup.
    MetaScreenHide,

    /// $SCREENSHOW directive - show graphics window on startup.
    MetaScreenShow,

    /// SWAP statement - exchange values of two variables.
    Swap {
        /// First variable/expression to swap.
        left: TypedExpr,
        /// Second variable/expression to swap.
        right: TypedExpr,
    },

    /// _CONTINUE statement - skip to next loop iteration.
    Continue {
        /// The type of loop to continue.
        continue_type: ContinueType,
    },

    /// TYPE definition - user-defined type.
    TypeDefinition {
        /// The type name.
        name: String,
        /// The type members with their types.
        members: Vec<TypedMember>,
        /// QB4.5 CUSTOMTYPE modifier - indicates C-compatible memory layout.
        custom_type: bool,
    },

    /// DATA statement - compile-time data values.
    ///
    /// DATA values are collected into a global pool that READ consumes.
    Data {
        /// The literal values in this DATA statement.
        values: Vec<TypedDataValue>,
    },

    /// READ statement - read from DATA pool.
    ///
    /// Each READ consumes values from the DATA pool in order.
    Read {
        /// Targets to read into (variables or array elements).
        targets: Vec<TypedReadTarget>,
    },

    /// RESTORE statement - reset DATA pointer.
    ///
    /// RESTORE resets the DATA read position to the beginning
    /// or to a labeled DATA statement.
    Restore {
        /// Optional label to restore to.
        label: Option<String>,
    },

    /// RANDOMIZE statement to seed the random number generator.
    ///
    /// RANDOMIZE [seed] - Initialize random number generator.
    /// - `RANDOMIZE` - no seed (implementation defined behavior)
    /// - `RANDOMIZE TIMER` - seed is the TIMER function call
    /// - `RANDOMIZE expr` - seed with specific value
    Randomize {
        /// The seed expression (if provided).
        seed: Option<TypedExpr>,
    },

    // ==================== File I/O Statements ====================
    /// OPEN statement for file operations.
    OpenFile {
        /// The filename expression.
        filename: TypedExpr,
        /// The file mode.
        mode: FileMode,
        /// Optional access mode.
        access: Option<FileAccess>,
        /// Optional lock mode.
        lock: Option<FileLock>,
        /// The file number.
        file_num: TypedExpr,
        /// Optional record length.
        record_len: Option<TypedExpr>,
    },

    /// CLOSE statement.
    CloseFile {
        /// File numbers to close (empty = close all).
        file_nums: Vec<TypedExpr>,
    },

    /// PRINT # statement (file output).
    FilePrint {
        /// The file number.
        file_num: TypedExpr,
        /// Values to print.
        items: Vec<TypedPrintItem>,
        /// Whether to print newline.
        newline: bool,
    },

    /// WRITE # statement (file output with delimiters).
    FileWrite {
        /// The file number.
        file_num: TypedExpr,
        /// Values to write.
        values: Vec<TypedExpr>,
    },

    /// INPUT # statement (file input).
    FileInput {
        /// The file number.
        file_num: TypedExpr,
        /// Typed input targets (variables, array elements, field accesses).
        targets: Vec<TypedInputTarget>,
    },

    /// LINE INPUT # statement (file line input).
    FileLineInput {
        /// The file number.
        file_num: TypedExpr,
        /// Variable to read into.
        variable: String,
    },

    /// GET statement (binary/random file read).
    FileGet {
        /// The file number.
        file_num: TypedExpr,
        /// Optional position.
        position: Option<TypedExpr>,
        /// Variable to read into.
        variable: String,
        /// Variable type.
        var_type: BasicType,
    },

    /// PUT statement (binary/random file write).
    FilePut {
        /// The file number.
        file_num: TypedExpr,
        /// Optional position.
        position: Option<TypedExpr>,
        /// Variable to write.
        variable: String,
        /// Variable type.
        var_type: BasicType,
        /// Optional array index for `PUT #1, , arr(i)`.
        index: Option<TypedExpr>,
    },

    /// SEEK statement (set file position).
    FileSeek {
        /// The file number.
        file_num: TypedExpr,
        /// The position.
        position: TypedExpr,
    },

    // ==================== Error Handling Statements ====================
    /// ON ERROR GOTO statement.
    OnErrorGoto {
        /// Target label (or "0" to disable).
        target: String,
    },

    /// ON ERROR RESUME NEXT statement.
    OnErrorResumeNext,

    /// RESUME statement.
    ResumeStmt {
        /// Resume target.
        target: Option<ResumeTarget>,
    },

    /// ERROR statement (simulate error).
    ErrorStmt {
        /// Error code.
        code: TypedExpr,
    },

    // ==================== Computed Control Flow ====================
    /// ON...GOTO statement.
    OnGoto {
        /// Selector expression.
        selector: TypedExpr,
        /// Target labels.
        targets: Vec<String>,
    },

    /// ON...GOSUB statement.
    OnGosub {
        /// Selector expression.
        selector: TypedExpr,
        /// Target labels.
        targets: Vec<String>,
    },

    // ==================== DEF FN ====================
    /// DEF FN statement.
    DefFn {
        /// Function name (without FN prefix).
        name: String,
        /// Parameters.
        params: Vec<TypedParameter>,
        /// Return type.
        return_type: BasicType,
        /// Function body expression.
        body: TypedExpr,
    },

    /// Multi-line DEF FN statement (QB64 extension).
    DefFnMultiLine {
        /// Function name (without FN prefix).
        name: String,
        /// Parameters.
        params: Vec<TypedParameter>,
        /// Return type.
        return_type: BasicType,
        /// Function body statements.
        body: Vec<TypedStatement>,
    },

    // ==================== Variable/Scope Statements ====================
    /// COMMON statement.
    CommonStmt {
        /// Whether SHARED was specified.
        shared: bool,
        /// Variables with their types.
        variables: Vec<TypedCommonVariable>,
    },

    /// SHARED statement inside SUB/FUNCTION.
    SharedStmt {
        /// Names of shared variables to access.
        variables: Vec<String>,
    },

    /// STATIC statement inside SUB/FUNCTION - declares static local variables.
    StaticStmt {
        /// Variables to declare as static (persist between calls).
        variables: Vec<TypedDimVariable>,
    },

    /// REDIM statement.
    Redim {
        /// Whether to preserve contents.
        preserve: bool,
        /// Whether SHARED was specified (module-level visibility).
        shared: bool,
        /// Arrays to redimension.
        variables: Vec<TypedRedimVariable>,
    },

    // ==================== Graphics Statements ====================
    /// SCREEN statement - sets graphics mode.
    ///
    /// Full QB45 syntax: `SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]`
    Screen {
        /// Screen mode number (optional).
        mode: Option<TypedExpr>,
        /// Color switch (optional).
        color_switch: Option<TypedExpr>,
        /// Active page number (optional).
        active_page: Option<TypedExpr>,
        /// Visual page number (optional).
        visual_page: Option<TypedExpr>,
    },

    /// CLS statement - clears the screen.
    /// mode: 0=clear graphics and text (default), 1=clear graphics only, 2=clear text only
    Cls {
        /// Optional clear mode (0, 1, or 2).
        mode: Option<TypedExpr>,
    },

    /// COLOR statement - sets foreground/background colors.
    Color {
        /// Foreground color.
        foreground: TypedExpr,
        /// Optional background color.
        background: Option<TypedExpr>,
        /// Optional border color (text mode only, CGA/EGA legacy).
        border: Option<TypedExpr>,
    },

    /// LOCATE statement - positions the cursor.
    Locate {
        /// Row (1-based, optional).
        row: Option<TypedExpr>,
        /// Column (1-based, optional).
        col: Option<TypedExpr>,
    },

    /// PSET statement - plots a point.
    Pset {
        /// Whether coordinates are relative (STEP).
        step: bool,
        /// X coordinate.
        x: TypedExpr,
        /// Y coordinate.
        y: TypedExpr,
        /// Optional color.
        color: Option<TypedExpr>,
    },

    /// PRESET statement - plots a point in background color.
    Preset {
        /// Whether coordinates are relative (STEP).
        step: bool,
        /// X coordinate.
        x: TypedExpr,
        /// Y coordinate.
        y: TypedExpr,
    },

    /// LINE statement - draws a line or box.
    Line {
        /// Starting X (None uses last point).
        x1: Option<TypedExpr>,
        /// Starting Y (None uses last point).
        y1: Option<TypedExpr>,
        /// Ending X.
        x2: TypedExpr,
        /// Ending Y.
        y2: TypedExpr,
        /// Whether STEP keyword was used (relative coordinates).
        step2: bool,
        /// Optional color.
        color: Option<TypedExpr>,
        /// Box style: None = line, Some(false) = box, Some(true) = filled box.
        box_style: Option<bool>,
    },

    /// CIRCLE statement - draws a circle.
    Circle {
        /// Whether STEP was used (relative to last graphics position).
        step: bool,
        /// Center X.
        x: TypedExpr,
        /// Center Y.
        y: TypedExpr,
        /// Radius.
        radius: TypedExpr,
        /// Optional color.
        color: Option<TypedExpr>,
        /// Whether filled.
        filled: bool,
    },

    /// PAINT statement - flood fills an area.
    Paint {
        /// Whether STEP was used (relative to last graphics position).
        step: bool,
        /// Starting X.
        x: TypedExpr,
        /// Starting Y.
        y: TypedExpr,
        /// Fill color (None uses current foreground).
        color: Option<TypedExpr>,
        /// Border color to stop at.
        border: Option<TypedExpr>,
    },

    /// _DISPLAY statement - updates the screen.
    GfxDisplay,

    /// PALETTE statement - sets palette colors.
    Palette {
        /// Color attribute/index (0-255).
        attribute: Option<TypedExpr>,
        /// Color value to assign.
        color: Option<TypedExpr>,
    },

    /// PCOPY statement - copies screen page.
    Pcopy {
        /// Source page number.
        source: TypedExpr,
        /// Destination page number.
        dest: TypedExpr,
    },

    /// WIDTH statement - sets screen width.
    Width {
        /// Number of columns.
        columns: TypedExpr,
        /// Optional number of rows.
        rows: Option<TypedExpr>,
    },

    /// VIEW statement - defines viewport.
    View {
        /// Whether SCREEN was specified.
        screen: bool,
        /// Viewport coordinates.
        coords: Option<TypedViewCoords>,
        /// Fill color.
        fill_color: Option<TypedExpr>,
        /// Border color.
        border_color: Option<TypedExpr>,
    },

    /// VIEW PRINT statement - text viewport.
    ViewPrint {
        /// Top row (if specified).
        top: Option<TypedExpr>,
        /// Bottom row (if specified).
        bottom: Option<TypedExpr>,
    },

    /// WINDOW statement - defines coordinate system.
    WindowCoords {
        /// Whether SCREEN was specified.
        screen: bool,
        /// Window coordinates.
        coords: Option<TypedViewCoords>,
    },

    /// DRAW statement - turtle graphics.
    DrawCmd {
        /// Drawing commands string.
        commands: TypedExpr,
    },

    /// GET graphics statement - capture screen region to array.
    GraphicsGet {
        /// First corner X coordinate.
        x1: TypedExpr,
        /// First corner Y coordinate.
        y1: TypedExpr,
        /// Second corner X coordinate (or width if step2).
        x2: TypedExpr,
        /// Second corner Y coordinate (or height if step2).
        y2: TypedExpr,
        /// Whether second coordinate pair is relative (STEP).
        step2: bool,
        /// Array name to store captured image.
        array_name: String,
        /// Array indices (supports multi-dimensional).
        array_indices: Vec<TypedExpr>,
    },

    /// PUT graphics statement - draw array contents to screen.
    GraphicsPut {
        /// X coordinate for placing image.
        x: TypedExpr,
        /// Y coordinate for placing image.
        y: TypedExpr,
        /// Whether coordinates are relative (STEP).
        step: bool,
        /// Array name containing image data.
        array_name: String,
        /// Array indices (supports multi-dimensional).
        array_indices: Vec<TypedExpr>,
        /// QB64: _CLIP modifier to clip at screen boundaries.
        clip: bool,
        /// Action for combining pixels.
        action: crate::ast::PutAction,
        /// QB64: Optional transparent color.
        transparent_color: Option<TypedExpr>,
    },

    // ==================== QB64 Graphics Extensions ====================
    /// _FREEIMAGE statement.
    FreeImage {
        /// Image handle.
        handle: TypedExpr,
    },

    /// _PUTIMAGE statement.
    PutImage {
        /// Destination coordinates.
        dest_coords: Option<Box<TypedViewCoords>>,
        /// Source image handle.
        source: Option<TypedExpr>,
        /// Destination image handle.
        dest: Option<TypedExpr>,
        /// Source coordinates.
        source_coords: Option<Box<TypedViewCoords>>,
        /// Scaling mode (_SMOOTH or _STRETCH).
        scale_mode: ImageScaleMode,
    },

    /// _SOURCE statement.
    SourceImg {
        /// Image handle.
        handle: TypedExpr,
    },

    /// _DEST statement.
    DestImg {
        /// Image handle.
        handle: TypedExpr,
    },

    /// _PRINTSTRING statement.
    PrintStringStmt {
        /// X coordinate.
        x: TypedExpr,
        /// Y coordinate.
        y: TypedExpr,
        /// Text to print.
        text: TypedExpr,
    },

    /// _AUTODISPLAY statement.
    AutoDisplay {
        /// Whether enabled.
        enabled: bool,
    },

    // ==================== Audio Statements ====================
    /// BEEP statement.
    Beep,

    /// SOUND statement.
    SoundStmt {
        /// Frequency in Hz.
        frequency: TypedExpr,
        /// Duration.
        duration: TypedExpr,
    },

    /// PLAY statement.
    PlayStmt {
        /// Music commands string.
        commands: TypedExpr,
    },

    /// _SNDCLOSE statement.
    SndClose {
        /// Sound handle.
        handle: TypedExpr,
    },

    /// _SNDPLAY statement.
    SndPlay {
        /// Sound handle.
        handle: TypedExpr,
    },

    /// _SNDSTOP statement.
    SndStop {
        /// Sound handle.
        handle: TypedExpr,
    },

    /// _SNDPAUSE statement.
    SndPause {
        /// Sound handle.
        handle: TypedExpr,
    },

    /// _SNDLOOP statement.
    SndLoop {
        /// Sound handle.
        handle: TypedExpr,
    },

    /// _SNDVOL statement.
    SndVol {
        /// Sound handle.
        handle: TypedExpr,
        /// Volume (0.0-1.0).
        volume: TypedExpr,
    },

    /// _SNDBAL statement.
    SndBal {
        /// Sound handle.
        handle: TypedExpr,
        /// Balance (-1.0 to 1.0).
        balance: TypedExpr,
    },

    /// _SNDRAW statement.
    SndRaw {
        /// Left/mono sample.
        left: TypedExpr,
        /// Right sample (optional).
        right: Option<TypedExpr>,
    },

    // ==================== System Integration Statements ====================
    /// KILL statement - delete a file.
    Kill {
        /// The filename to delete.
        filename: TypedExpr,
    },

    /// NAME statement - rename a file.
    Rename {
        /// The current filename.
        old_name: TypedExpr,
        /// The new filename.
        new_name: TypedExpr,
    },

    /// MKDIR statement - create a directory.
    Mkdir {
        /// The directory path.
        path: TypedExpr,
    },

    /// RMDIR statement - remove a directory.
    Rmdir {
        /// The directory path.
        path: TypedExpr,
    },

    /// CHDIR statement - change current directory.
    Chdir {
        /// The directory path.
        path: TypedExpr,
    },

    /// SHELL statement - execute external command.
    ShellCmd {
        /// The command to execute (optional).
        command: Option<TypedExpr>,
    },

    /// _SHELLHIDE statement - execute hidden command.
    ShellHide {
        /// The command to execute.
        command: TypedExpr,
    },

    /// BLOAD statement - load binary file to memory.
    Bload {
        /// The filename to load from.
        filename: TypedExpr,
        /// Optional memory address/offset.
        address: Option<TypedExpr>,
    },

    /// BSAVE statement - save memory to binary file.
    Bsave {
        /// The filename to save to.
        filename: TypedExpr,
        /// Starting memory address.
        address: TypedExpr,
        /// Number of bytes to save.
        length: TypedExpr,
    },

    /// SETMEM statement - set available memory (legacy stub).
    Setmem {
        /// Number of bytes (ignored).
        bytes: TypedExpr,
    },

    /// CALL ABSOLUTE statement - call machine language routine (legacy stub).
    CallAbsolute {
        /// Memory address (ignored, generates warning).
        address: TypedExpr,
    },

    // ==================== Mouse Input Statements ====================
    /// _MOUSEHIDE statement.
    MouseHide,

    /// _MOUSESHOW statement.
    MouseShow,

    /// _MOUSEMOVE statement.
    MouseMoveStmt {
        /// X coordinate.
        x: TypedExpr,
        /// Y coordinate.
        y: TypedExpr,
    },

    // ==================== Clipboard Statement ====================
    /// _CLIPBOARD$ = text$ statement.
    ClipboardSet {
        /// The text to set.
        text: TypedExpr,
    },

    // ==================== C Library Integration ====================
    /// DECLARE LIBRARY block for C interop.
    DeclareLibrary {
        /// Library name/path (without extension).
        library_name: Option<String>,
        /// Whether this is a dynamic library.
        is_dynamic: bool,
        /// External function/sub declarations.
        declarations: Vec<TypedExternalDeclaration>,
    },

    // ==================== Forward Declarations ====================
    /// Forward declaration of a subroutine (DECLARE SUB).
    /// These are parsed for compatibility but don't generate code.
    DeclareSub {
        /// The name of the subroutine.
        name: String,
    },

    /// Forward declaration of a function (DECLARE FUNCTION).
    /// These are parsed for compatibility but don't generate code.
    DeclareFunction {
        /// The name of the function.
        name: String,
    },

    // ==================== Phase 7: Additional Statements ====================
    /// RUN statement - restart program or run another program.
    Run {
        /// Optional target (filename or line number).
        target: Option<TypedExpr>,
    },

    /// CHAIN statement - run another program.
    Chain {
        /// Filename of program to chain to.
        filename: TypedExpr,
    },

    /// TRON statement - enable trace mode.
    Tron,

    /// TROFF statement - disable trace mode.
    Troff,

    /// LPRINT statement - print to printer.
    Lprint {
        /// Values to print.
        values: Vec<TypedPrintItem>,
        /// Whether to print newline.
        newline: bool,
    },

    /// FILES statement - display directory listing.
    FilesStmt {
        /// Optional file specification.
        filespec: Option<TypedExpr>,
    },

    /// FIELD statement - define random file fields.
    FieldStmt {
        /// File number.
        file_num: TypedExpr,
        /// Field specifications.
        fields: Vec<TypedFieldSpec>,
    },

    /// LSET statement - left-align string in field.
    Lset {
        /// Target field variable.
        variable: String,
        /// Value to assign.
        value: TypedExpr,
    },

    /// RSET statement - right-align string in field.
    Rset {
        /// Target field variable.
        variable: String,
        /// Value to assign.
        value: TypedExpr,
    },

    /// ON KEY(n) GOSUB statement.
    OnKey {
        /// Key number.
        key_num: TypedExpr,
        /// Target label.
        target: String,
    },

    /// KEY(n) ON|OFF|STOP statement.
    KeyControl {
        /// Key number.
        key_num: TypedExpr,
        /// Control mode.
        mode: EventControlMode,
    },

    /// ON TIMER statement.
    OnTimer {
        /// Timer interval.
        interval: TypedExpr,
        /// Target label.
        target: String,
    },

    /// TIMER ON|OFF|STOP statement.
    TimerControl {
        /// Control mode.
        mode: EventControlMode,
    },

    /// STRIG(n) ON|OFF|STOP statement.
    StrigControl {
        /// Button number.
        button_num: TypedExpr,
        /// Control mode.
        mode: EventControlMode,
    },

    /// ON STRIG statement.
    OnStrig {
        /// Button number.
        button_num: TypedExpr,
        /// Target label.
        target: String,
    },

    /// ON COM(n) GOSUB statement.
    OnCom {
        /// COM port number.
        port_num: TypedExpr,
        /// Target label.
        target: String,
    },

    /// COM(n) ON|OFF|STOP statement.
    ComControl {
        /// COM port number.
        port_num: TypedExpr,
        /// Control mode.
        mode: EventControlMode,
    },

    /// ON PEN GOSUB statement.
    OnPen {
        /// Target label.
        target: String,
    },

    /// PEN ON|OFF|STOP statement.
    PenControl {
        /// Control mode.
        mode: EventControlMode,
    },

    /// ON UEVENT GOSUB statement.
    OnUevent {
        /// Target label.
        target: String,
    },

    /// UEVENT ON|OFF|STOP statement.
    UeventControl {
        /// Control mode.
        mode: EventControlMode,
    },

    /// UEVENT statement - trigger user event.
    UeventTrigger,

    /// ON SIGNAL(n) GOSUB statement.
    OnSignal {
        /// Signal number.
        signal_num: TypedExpr,
        /// Target label.
        target: String,
    },

    /// SIGNAL(n) ON|OFF|STOP statement.
    SignalControl {
        /// Signal number.
        signal_num: TypedExpr,
        /// Control mode.
        mode: EventControlMode,
    },

    /// OUT port, value statement.
    OutPort {
        /// Port address.
        port: TypedExpr,
        /// Value to write.
        value: TypedExpr,
    },

    /// INTERRUPT statement.
    InterruptStmt {
        /// Interrupt number.
        int_num: TypedExpr,
        /// Input registers variable.
        in_regs: String,
        /// Output registers variable.
        out_regs: String,
    },

    /// INTERRUPTX statement.
    InterruptXStmt {
        /// Interrupt number.
        int_num: TypedExpr,
        /// Input registers variable.
        in_regs: String,
        /// Output registers variable.
        out_regs: String,
    },

    /// IOCTL statement.
    IoctlStmt {
        /// File number.
        file_num: TypedExpr,
        /// Control string.
        control_string: TypedExpr,
    },

    /// FREE statement.
    FreeStmt,

    /// CLEAR statement - clear variables.
    ClearStmt {
        /// Optional stack size.
        stack_size: Option<TypedExpr>,
    },

    /// RESET statement - close all files.
    ResetStmt,

    // ==================== Window/Desktop Statements (QB64) ====================
    /// _TITLE statement - set window title.
    TitleStmt {
        /// The title text.
        title: TypedExpr,
    },

    /// _SCREENMOVE statement - move window.
    ScreenMoveStmt {
        /// X position (or None if centering).
        x: Option<TypedExpr>,
        /// Y position (or None if centering).
        y: Option<TypedExpr>,
        /// Whether to center the window.
        center: bool,
    },

    /// _FULLSCREEN statement.
    FullScreenStmt {
        /// Fullscreen mode.
        mode: FullScreenMode,
    },

    /// _ALLOWFULLSCREEN statement.
    AllowFullScreenStmt {
        /// Allowed mode.
        mode: AllowFullScreenMode,
    },

    /// _SCREENICON statement - minimize window.
    ScreenIconStmt,

    /// _ICON statement - set window icon.
    IconStmt {
        /// Optional image handle.
        handle: Option<TypedExpr>,
    },

    /// _SCREENHIDE statement.
    ScreenHideStmt,

    /// _SCREENSHOW statement.
    ScreenShowStmt,

    /// _CONSOLETITLE statement.
    ConsoleTitleStmt {
        /// The title text.
        title: TypedExpr,
    },

    /// _CONSOLE ON|OFF statement.
    ConsoleStmt {
        /// Whether console should be visible.
        visible: bool,
    },

    /// _ASSERT statement.
    AssertStmt {
        /// Condition to check.
        condition: TypedExpr,
        /// Optional error message.
        message: Option<TypedExpr>,
    },

    /// $ASSERTS metacommand.
    MetaAsserts,

    /// $NOPREFIX metacommand.
    MetaNoPrefix,

    /// $COLOR metacommand.
    MetaColor {
        /// Color depth (0 for EGA, 32 for RGBA).
        depth: Option<i64>,
    },
}

/// Typed coordinates for VIEW and WINDOW statements.
#[derive(Debug, Clone)]
pub struct TypedViewCoords {
    /// X1 coordinate.
    pub x1: TypedExpr,
    /// Y1 coordinate.
    pub y1: TypedExpr,
    /// X2 coordinate.
    pub x2: TypedExpr,
    /// Y2 coordinate.
    pub y2: TypedExpr,
}

/// Typed field specification for FIELD statement.
#[derive(Debug, Clone)]
pub struct TypedFieldSpec {
    /// Width of the field in bytes.
    pub width: TypedExpr,
    /// Variable name for this field.
    pub variable: String,
}

/// A typed variable in a COMMON statement.
#[derive(Debug, Clone)]
pub struct TypedCommonVariable {
    /// Variable name.
    pub name: String,
    /// Variable type.
    pub basic_type: BasicType,
    /// Array dimensions (empty if scalar).
    pub dimensions: Vec<TypedArrayDimension>,
}

/// A typed member of a TYPE definition.
#[derive(Debug, Clone)]
pub struct TypedMember {
    /// Member name.
    pub name: String,
    /// Member type.
    pub basic_type: BasicType,
}

/// A typed target for INPUT statement (lvalue with type info).
#[derive(Debug, Clone)]
pub enum TypedInputTarget {
    /// Simple variable: `x`
    Variable { name: String, basic_type: BasicType },
    /// Array element: `arr(i)` or `arr(i, j)`
    ArrayElement {
        name: String,
        indices: Vec<TypedExpr>,
        element_type: BasicType,
    },
    /// Array element field access: `arr(i).field`
    ArrayElementField {
        name: String,
        indices: Vec<TypedExpr>,
        fields: Vec<String>,
        field_type: BasicType,
    },
    /// Simple UDT field access: `udt.field`
    Field {
        name: String,
        fields: Vec<String>,
        field_type: BasicType,
    },
}

/// A typed target for READ statement.
///
/// READ can read into simple variables or array elements.
#[derive(Debug, Clone)]
pub enum TypedReadTarget {
    /// Simple variable: `READ x`
    Variable { name: String, basic_type: BasicType },
    /// Array element: `READ arr(i, j)`
    ArrayElement {
        name: String,
        indices: Vec<TypedExpr>,
        basic_type: BasicType,
    },
    /// UDT array element field: `READ arr(i).field`
    ArrayFieldElement {
        name: String,
        indices: Vec<TypedExpr>,
        field: String,
        basic_type: BasicType,
    },
}

/// A typed value from a DATA statement.
///
/// DATA values are typed during semantic analysis based on their literal form.
#[derive(Debug, Clone)]
pub enum TypedDataValue {
    /// An integer literal.
    Integer(i64),
    /// A floating-point literal.
    Float(f64),
    /// A string literal.
    String(String),
}

impl TypedStatement {
    /// Creates a new typed statement.
    pub fn new(kind: TypedStatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// A typed print item with its separator.
#[derive(Debug, Clone)]
pub struct TypedPrintItem {
    /// The expression to print.
    pub expr: TypedExpr,
    /// Separator after this item (Semicolon, Comma, or None).
    pub separator: Option<PrintSeparator>,
}

/// A typed DO loop condition.
#[derive(Debug, Clone)]
pub struct TypedDoCondition {
    /// True for WHILE, false for UNTIL.
    pub is_while: bool,
    /// The condition expression.
    pub condition: TypedExpr,
}

/// A typed CASE clause.
#[derive(Debug, Clone)]
pub struct TypedCaseClause {
    /// Match conditions.
    pub matches: Vec<TypedCaseMatch>,
    /// Body statements.
    pub body: Vec<TypedStatement>,
}

/// A typed CASE match condition.
#[derive(Debug, Clone)]
pub enum TypedCaseMatch {
    /// Single value: `CASE 1`
    Single(TypedExpr),
    /// Range: `CASE 1 TO 10`
    Range { from: TypedExpr, to: TypedExpr },
    /// Comparison: `CASE IS > 5`
    Comparison {
        op: TypedCaseCompareOp,
        value: TypedExpr,
    },
}

/// Comparison operators for CASE IS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedCaseCompareOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// A typed procedure parameter.
#[derive(Debug, Clone)]
pub struct TypedParameter {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub basic_type: BasicType,
    /// Whether BYVAL was specified.
    pub by_val: bool,
}

/// A typed array dimension.
#[derive(Debug, Clone)]
pub struct TypedArrayDimension {
    /// Lower bound (evaluated to constant).
    pub lower: i64,
    /// Upper bound (evaluated to constant).
    pub upper: i64,
}

/// A typed variable in a DIM statement.
#[derive(Debug, Clone)]
pub struct TypedDimVariable {
    /// Variable name.
    pub name: String,
    /// Basic type of the variable.
    pub basic_type: BasicType,
    /// Array dimensions (empty if scalar).
    pub dimensions: Vec<TypedArrayDimension>,
}

/// A typed variable in a REDIM statement.
#[derive(Debug, Clone)]
pub struct TypedRedimVariable {
    /// Array name.
    pub name: String,
    /// Element type.
    pub element_type: BasicType,
    /// Dimensions.
    pub dimensions: Vec<TypedArrayDimension>,
}

/// The complete typed program - output of semantic analysis.
///
/// This structure is ready for code generation.
#[derive(Debug)]
pub struct TypedProgram {
    /// The type-checked statements.
    pub statements: Vec<TypedStatement>,
}

impl TypedProgram {
    /// Creates a new typed program.
    pub fn new(statements: Vec<TypedStatement>) -> Self {
        Self { statements }
    }
}

/// A typed external function declaration from DECLARE LIBRARY.
#[derive(Debug, Clone)]
pub struct TypedExternalDeclaration {
    /// The BASIC name for the function/sub.
    pub name: String,
    /// The C library name (may be same as name or from ALIAS).
    pub c_name: String,
    /// The typed parameters.
    pub params: Vec<TypedExternalParam>,
    /// Return type (Void for SUBs).
    pub return_type: BasicType,
    /// Whether this is a FUNCTION (true) or SUB (false).
    pub is_function: bool,
}

/// A typed parameter for an external function.
#[derive(Debug, Clone)]
pub struct TypedExternalParam {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub typ: BasicType,
    /// Whether this is BYVAL (pass by value).
    pub is_byval: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typed_expr_creation() {
        let expr = TypedExpr::integer(42, Span::new(0, 2));
        assert_eq!(expr.basic_type, BasicType::Long);
        assert!(matches!(expr.kind, TypedExprKind::IntegerLiteral(42)));
    }

    #[test]
    fn test_convert_to() {
        let int_expr = TypedExpr::integer(42, Span::new(0, 2));
        let converted = int_expr.convert_to(BasicType::Double);

        assert_eq!(converted.basic_type, BasicType::Double);
        assert!(matches!(converted.kind, TypedExprKind::Convert { .. }));
    }

    #[test]
    fn test_convert_to_same_type() {
        let int_expr = TypedExpr::integer(42, Span::new(0, 2));
        let not_converted = int_expr.convert_to(BasicType::Long);

        // Should NOT wrap in Convert since types match
        assert!(matches!(
            not_converted.kind,
            TypedExprKind::IntegerLiteral(42)
        ));
    }

    #[test]
    fn test_typed_statement() {
        let stmt = TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "x".to_string(),
                value: TypedExpr::integer(5, Span::new(4, 5)),
                target_type: BasicType::Integer,
            },
            Span::new(0, 5),
        );

        assert!(matches!(stmt.kind, TypedStatementKind::Assignment { .. }));
    }
}
