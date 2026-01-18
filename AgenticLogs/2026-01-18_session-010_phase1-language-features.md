# Session 010: Phase 1 Language Feature Implementation

**Date:** 2026-01-18
**Objective:** Implement all Phase 1 language features from TODO.md

## Goals

Implement the following features:
1. File I/O (OPEN, CLOSE, PRINT #, INPUT #, LINE INPUT #, GET, PUT, SEEK, etc.)
2. Keyboard Input (INKEY$, INPUT$, _KEYHIT, _KEYDOWN)
3. Error Handling (ON ERROR GOTO, RESUME, ERR, ERL)
4. Computed Control Flow (ON n GOTO/GOSUB)
5. Missing Built-in Functions (ENVIRON$, COMMAND$, _OS$, etc.)
6. Variable/Scope Enhancements (COMMON, STATIC arrays, REDIM _PRESERVE)
7. DEF FN Support

## Progress

### Implementation Pattern

For each feature, followed a consistent pattern through the compiler pipeline:
1. Add tokens (if needed) in `src/lexer/token.rs`
2. Add AST statement types in `src/ast/stmt.rs`
3. Add parser support in `src/parser/statements.rs`
4. Add typed IR variants in `src/semantic/typed_ir.rs`
5. Add type checking in `src/semantic/checker/statements.rs`
6. Add code generation in `src/codegen/c_backend/stmt.rs`
7. Add runtime functions in `src/codegen/c_backend/runtime.rs`
8. Write tests

### Completed Features

#### 1. File I/O
- **Tokens added:** `SEEK`, `ACCESS`, `BINARY`, `RANDOM`, `OUTPUT`, `APPEND`, `LEN`, `LOCK`, `UNLOCK`
- **AST types:** `OpenFile`, `CloseFile`, `FilePrint`, `FileInput`, `FileLineInput`, `FileGet`, `FilePut`, `FileSeek`, `FileWrite`
- **Enums:** `FileMode`, `FileAccess`, `FileLock`
- **Runtime functions:** `qb_file_open`, `qb_file_close`, `qb_file_print_*`, `qb_eof`, `qb_lof`, `qb_loc`, `qb_freefile`, etc.

#### 2. Keyboard Input
- **Built-in functions registered:** `INKEY$`, `INPUT$`
- **Runtime:** Platform-specific `qb_inkey()` (uses termios on Unix, kbhit/getch on Windows)

#### 3. Error Handling
- **Tokens added:** `ON`, `ERROR` (as `ErrorKw`), `RESUME`
- **AST types:** `OnErrorGoto`, `OnErrorResumeNext`, `ResumeStmt`, `ErrorStmt`
- **Enum:** `ResumeTarget`
- **Runtime:** `_qb_err`, `_qb_erl`, `_qb_error_handler`, `qb_error()`, `qb_err_code()`, `qb_err_line()`

#### 4. Computed Control Flow
- **AST types:** `OnGoto`, `OnGosub`
- **Code generation:** Generates C switch statement for computed jumps

#### 5. Built-in Functions
- **File I/O:** `EOF`, `LOF`, `LOC`, `FREEFILE`
- **Environment:** `ENVIRON$`, `COMMAND$`, `_CWD$`, `_OS$`, `_STARTDIR$`
- **Error:** `ERR`, `ERL`
- **Keyboard:** `INKEY$`, `INPUT$`

#### 6. Variable/Scope Enhancements
- **Tokens added:** `COMMON`, `_PRESERVE`
- **AST types:** `CommonStmt`, `Redim`
- **Struct:** `CommonVariable`

#### 7. DEF FN Support
- **Tokens added:** `DEF`, `FN`
- **AST types:** `DefFn`
- **Code generation:** Generates inline C function-like macros

### Tests Added

Added comprehensive tests for Phase 1 features:

**Parser tests (src/parser/mod.rs):**
- `test_parse_open_for_input/output/append/binary/random` - All file modes
- `test_parse_close`, `test_parse_close_multiple` - File closing
- `test_parse_print_to_file`, `test_parse_input_from_file`, `test_parse_line_input_from_file` - File I/O
- `test_parse_get`, `test_parse_put`, `test_parse_seek` - Binary file operations
- `test_parse_write_to_file` - WRITE # statement
- `test_parse_on_error_goto`, `test_parse_on_error_goto_zero`, `test_parse_on_error_resume_next` - Error handling
- `test_parse_resume`, `test_parse_resume_next`, `test_parse_resume_label` - RESUME statements
- `test_parse_error_statement` - ERROR statement
- `test_parse_on_goto`, `test_parse_on_gosub` - Computed control flow
- `test_parse_def_fn_single_line`, `test_parse_def_fn_with_type` - DEF FN
- `test_parse_common`, `test_parse_common_shared` - COMMON statement
- `test_parse_redim`, `test_parse_redim_preserve` - REDIM statement

**Semantic tests (src/semantic/checker/mod.rs):**
- `test_file_close_accepts_numeric`, `test_open_file_basic` - File I/O type checking
- `test_on_error_goto`, `test_error_statement_numeric` - Error handling type checking
- `test_on_goto_numeric_selector`, `test_on_gosub_numeric_selector` - Computed control flow
- `test_def_fn_basic` - DEF FN type checking
- `test_common_statement`, `test_redim_statement`, `test_redim_preserve` - Variable/scope statements

### Build Status

All 159 tests pass:
- 149 unit tests
- 10 doc tests (9 passing, 1 ignored)

### Key Decisions

1. **Error handling uses global state:** The runtime uses global `_qb_err` and `_qb_erl` variables for error tracking, matching QB64's behavior
2. **Platform-specific keyboard input:** Uses different implementations for Unix (termios non-blocking read) vs Windows (kbhit/getch)
3. **DEF FN as inline functions:** Generated as inline C functions to preserve QB64 semantics of inline macro-like behavior

---

*Session completed successfully.*
