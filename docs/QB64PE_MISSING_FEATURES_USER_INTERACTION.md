# QB64PE Missing Features - Items Requiring User Interaction

**Created:** 2026-01-29  
**Purpose:** Document missing features from `QB64PE_MISSING_FEATURES.md` that require user decisions or interaction before implementation.

**Related Documents:**
- [QB64PE_MISSING_FEATURES.md](QB64PE_MISSING_FEATURES.md) — Complete list of missing features
- [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md) — Tricky implementation items

---

## Overview

This document lists features that need design decisions, tooling choices, or user input before they can be implemented. These are not blockers for basic functionality, but they do need attention for full QB64pe compatibility.

---

## 1. Preprocessor Directives Requiring Design Decisions

### 1.1 `$USELIBRARY:'author/library'`

**Status:** ✅ Implemented (2026-01-29)

**What it does:** QB64pe's library system allows including external libraries with:
- `AtTop.bas` files (included at top of program)
- `AfterMain.bas` files (included after main program)
- Library metadata and dependencies

**QB64pe Implementation:**
- **Library discovery:** Searches for library descriptor at `libraries/descriptors/{author/library}.ini`
- **Library structure:** Each library has:
  - Descriptor file: `libraries/descriptors/{author/library}.ini` (INI format)
  - Source directory: `libraries/includes/{author/library}/`
  - Three inclusion points (from descriptor `[LIBRARY INCLUDES]` section):
    - `IncAtTop` — included at top of program
    - `IncAfterMain` — included after main program
    - `IncAtBottom` — included at bottom
- **Dependency tracking:** Maintains `useLibList$` array tracking:
  - Library name
  - Referrer (file/line that requested it)
  - Paths to each inclusion file
- **Duplicate prevention:** Checks if library already registered for same referrer before adding
- **Error handling:** Validates descriptor and source files exist before registration
- **Code location:** `source/qb64pe.bas` lines 1786-1847

**Implementation:**
- **Library discovery:** File-based discovery from `libraries/descriptors/{author/library}.ini`
- **Library format:** Uses QB64pe's format (INI descriptors with `[LIBRARY INCLUDES]` section)
- **Inclusion points:** All three points supported (IncAtTop, IncAfterMain, IncAtBottom)
- **Duplicate prevention:** Tracks library + referrer (file:line) to prevent duplicates
- **Error handling:** Validates descriptor and source files exist before registration
- **Integration:** Libraries are included during preprocessing at appropriate points:
  - AtTop files included at the very beginning (reverse order for dependencies)
  - AfterMain files included after all user code
  - AtBottom files included at the very end

**Files modified:**
- `src/library.rs` — New library management module (468 lines)
- `src/preprocessor.rs` — Added `$USELIBRARY` directive parsing and library inclusion
- `src/lib.rs` — Added `library` module export

**Future enhancements:**
- Dependency resolution (libraries depending on other libraries)
- Package registry support
- Library versioning

---

### 1.2 `$EMBED:'filename'`

**Status:** ⚠️ Parsed but not implemented

**What it does:** Embeds binary files into the compiled executable. The `_EMBEDDED$` function can retrieve embedded content at runtime.

**QB64pe Implementation:**
- **Syntax:** `$EMBED:'filename','handle'` (requires both filename and handle identifier)
- **Embedding mechanism:** 
  - Reads file during preprocessing
  - Generates separate `embedded.cpp` file in temp directory
  - Creates `func__embedded(qbs *handle)` function that returns embedded data by handle
  - Data is stored as binary in the generated C++ code
- **Runtime function:** `_EMBEDDED$(handle$)` calls `func__embedded()` with handle string
- **Error handling:** 
  - Validates file exists during preprocessing
  - Checks for duplicate file embeddings (error if same file embedded twice)
  - Runtime error if handle not found: `"Embed-Handle 'X' is undefined (check your $EMBED lines)"`
- **Code location:** 
  - Preprocessing: `source/qb64pe.bas` (parsing around line 97568)
  - Code generation: generates `embedded.cpp` file
  - Runtime: `func__embedded()` function in generated code

**What needs to be decided:**
1. **Embedding mechanism:** 
   - Option A: Embed as C string literals (simple, but large files bloat source)
   - Option B: Embed as binary data in C arrays (better for binary files)
   - Option C: Use linker sections (requires platform-specific tooling)
2. **Runtime retrieval:** How does `_EMBEDDED$` access embedded data?
3. **File size limits:** Should there be a maximum embedded file size?

**Recommendation:** Use C binary arrays (`unsigned char embed_data[] = { ... }`) for binary files, and add a runtime function `qb_embedded(const char* filename)` that returns a pointer to the embedded data.

**Files to modify:**
- `src/preprocessor.rs` — Read and encode binary files during preprocessing
- `src/codegen/c_backend/runtime/` — Add `qb_embedded()` function
- `src/semantic/builtins.rs` — Register `_EMBEDDED$` builtin (if not already)
- `src/codegen/c_backend/expr.rs` — Codegen for `_EMBEDDED$`

---

### 1.3 `$VERSIONINFO:key=value` and `$EXEICON:'filename'`

**Status:** ⚠️ Parsed but not emitted to C

**What it does:** Windows resource file generation for executable metadata (version, icon, description, etc.).

**QB64pe Implementation:**
- **$VERSIONINFO keys supported:**
  - `FILEVERSION#` and `PRODUCTVERSION#` — numeric versions (e.g., `4,3,0,0`)
  - `CompanyName`, `FileDescription`, `FileVersion`, `InternalName`
  - `LegalCopyright`, `LegalTrademarks`, `OriginalFilename`
  - `ProductName`, `ProductVersion`, `Comments`, `Web`
- **Resource file generation:**
  - Generates `icon.rc` file in temp directory
  - For `$EXEICON`: Copies icon file to `tmpdir/icon.ico`, adds `0 ICON "icon.ico"` to `.rc`
  - For `$VERSIONINFO`: Generates full `VERSIONINFO` block with all string values
  - Also generates `manifest.h` and `.manifest` XML file for Windows manifest
  - Uses absolute paths (copies icon to temp dir to ensure absolute path works)
- **Error handling:**
  - `$EXEICON` can only be set once (error if already defined)
  - Validates icon file exists and can be copied
- **Code location:** `source/qb64pe.bas` lines 12499-12588

**What needs to be decided:**
1. **Platform support:** Windows-only, or cross-platform metadata?
2. **Tooling:** 
   - Option A: Generate `.rc` files and use `windres` (GCC) or `rc.exe` (MSVC)
   - Option B: Use `llvm-rc` (cross-platform, but requires LLVM)
   - Option C: Embed resources directly in C code (limited, but portable)
3. **Build integration:** Should QB64Fresh call the resource compiler, or just emit `.rc` files?

**Recommendation:** Emit Windows `.rc` files and document that users need to compile them separately, or integrate with build system to call `windres`/`rc.exe` automatically.

**Files to modify:**
- `src/codegen/c_backend/stmt/meta.rs` — Emit `.rc` file content or call resource compiler
- Potentially create `src/codegen/c_backend/resources.rs` for resource file generation

---

### 1.4 `$COLOR:0` and `$COLOR:32`

**Status:** ✅ Implemented (metadata for LSP)

**What it does:** IDE syntax highlighting mode (0 = no color, 32 = 32-bit color).

**QB64pe Implementation:**
- **Syntax:** `$COLOR:0` or `$COLOR:32` (exact match required)
- **Mutual exclusivity:** Cannot use both — error if one is set after the other: `"$COLOR:32 already set, cannot use both color sets together"`
- **State management:** Sets `ColorSet` state variable (0, 1, or 2)
- **IDE-only:** This directive is for IDE display purposes only, not runtime
- **Code location:** `source/qb64pe.bas` lines 1780-1784, 97333-97344

**QB64Fresh Implementation:**
- **Parser:** `src/parser/directives.rs` — Parses `$COLOR:0` and `$COLOR:32`, validates values (only 0 or 32 accepted)
- **AST:** Stored as `MetaColor { depth: Option<i64> }` in the AST
- **LSP Metadata:** Extracted during analysis and stored in `AnalysisCache.color_mode`
- **Codegen:** Emitted as comment in generated C code (no-op at runtime)
- **Access:** Available via `AnalysisCache::color_mode()` method for LSP server use

**Note:** Currently, if both `$COLOR:0` and `$COLOR:32` are present, the last one wins (no error). This matches the extraction logic but differs from QB64pe's strict mutual exclusivity check. Future enhancement could add validation to error on conflicting directives.

---

### 1.5 `$ASSERTS` and `$ASSERTS:CONSOLE`

**Status:** ✅ Implemented

**What it does:** Enables debug assertions. `$ASSERTS:CONSOLE` sends assertion failures to console.

**QB64pe Implementation:**
- **Syntax:** `$ASSERTS` or `$ASSERTS:CONSOLE` (exact match)
- **State management:**
  - `$ASSERTS`: Sets `AssertsOn = 1`, defines `_ASSERTS_` preprocessor variable to `"1"`
  - `$ASSERTS:CONSOLE`: Sets both `AssertsOn = 1` and `ConsoleOn = 1`, defines both `_ASSERTS_` and `_CONSOLE_` to `"1"`
- **Preprocessor variables:** These are available as `$LET` variables that can be checked in `$IF` directives
- **Code location:** `source/qb64pe.bas` lines 1849-1861

**Implementation:**
1. **Assertion mechanism:** Custom `qb_assert()` function that checks runtime flags and aborts on failure
2. **Console output:** `$ASSERTS:CONSOLE` uses `fprintf(stderr, ...)` to print assertion failures before aborting
3. **Preprocessor variables:** `$ASSERTS` sets `_ASSERTS_` to 1, `$ASSERTS:CONSOLE` sets both `_ASSERTS_` and `_CONSOLE_` to 1 (available in `$IF` directives)
4. **Runtime variables:** `_qb_asserts_enabled` and `_qb_asserts_console` are set to 1 when directives are encountered

**Files modified:**
- `src/lexer/token.rs` — Added `MetaAsserts` and `MetaAssertsConsole` tokens
- `src/parser/directives.rs` — Added `parse_meta_asserts()` function
- `src/parser/statements/mod.rs` — Added parser dispatch for assertion directives
- `src/ast/stmt.rs` — Updated `MetaAsserts` to include `console` flag
- `src/semantic/typed_ir.rs` — Updated `MetaAsserts` to include `console` flag
- `src/semantic/checker/statements.rs` — Handle `MetaAsserts` to set preprocessor variables
- `src/semantic/symbols.rs` — Added `define_meta_let()` method to store `$LET` variables
- `src/semantic/checker/statements/misc.rs` — Store `$LET` variables in symbol table
- `src/codegen/c_backend/runtime/error.rs` — Added `qb_assert()` function and runtime flags
- `src/codegen/c_backend/stmt/meta.rs` — Emit code to set assertion flags when directive is encountered

---

### 1.6 `$STATIC` and `$DYNAMIC`

**Status:** ✅ Implemented (2026-01-29)

**What it does:** Controls whether arrays are statically or dynamically allocated.

**QB64pe Implementation:**
- **Syntax:** `$STATIC` or `$DYNAMIC` (exact match)
- **Scope:** Affects all arrays declared after the directive until the other directive is encountered
- **IDE integration:** These directives are recognized in IDE code formatting (case normalization in comments)
- **Default:** QB64pe defaults to dynamic arrays (all arrays are `REDIM`-able by default)
- **Usage:** Rarely used in QB64pe's own source code — primarily for performance optimization in specific cases
- **Code location:** Recognized in keyword lists and IDE formatting code

**Implementation Details:**
1. **Static array implementation:** Static arrays are implemented as fixed-size C arrays:
   - Global arrays: `type name[SIZE] = {0};` at global scope
   - Local arrays: `static type name[SIZE] = {0};` (persists between function calls)
2. **Default behavior:** Defaults to dynamic arrays (matches QB64pe default)
3. **Scope:** Directive affects all arrays declared after it until the other directive is encountered
4. **Array bounds:** Static arrays require compile-time constant sizes (evaluated during semantic analysis)

**Files modified:**
- `src/semantic/checker/mod.rs` — Added `array_mode_static` field to track current mode
- `src/semantic/checker/statements.rs` — Process `$STATIC`/`$DYNAMIC` to set array mode
- `src/semantic/typed_ir.rs` — Added `is_static` field to `TypedDimVariable`
- `src/semantic/checker/definitions.rs` — Set `is_static` flag when creating `TypedDimVariable`
- `src/semantic/checker/statements/misc.rs` — Handle `is_static` for STATIC statement arrays
- `src/codegen/c_backend/stmt/definitions.rs` — Emit static arrays as fixed-size C arrays
- `src/codegen/c_backend/stmt/mod.rs` — Pass `is_static` flag to `emit_dim`
- `src/codegen/c_backend/types.rs` — Updated `declare_array_var` to handle static arrays
- `src/codegen/c_backend/analysis.rs` — Collect static arrays at global scope

---

## 2. Event Trapping (Requires Runtime Integration)

### 2.1 `ON KEY ... GOTO`, `ON TIMER ... GOTO`, `ON UEVENT ... GOTO`, etc.

**Status:** ✅ Implemented (2026-01-29)

**What it does:** Sets up event handlers for keyboard, timer, user events, serial port, joystick, light pen events.

**QB64pe Implementation:**
- **ON KEY:**
  - Supports keys 1-31 (function keys, cursor keys, etc.)
  - Uses scancode-based lookup system
  - Handlers stored in `onkey[]` array with `id`, `active`, and `state` fields
  - Reset on `RUN` command (all handlers cleared)
  - Integrated with keyboard input system — events checked during input operations
  - Code location: `internal/c/libqb.cpp` lines 21491-21499 (reset), 30250+ (event handling)
- **ON TIMER:**
  - Timer-based event system
  - Events can fire during `_DELAY` operations (non-blocking)
  - Code location: `internal/c/parts/audio/audio.cpp` line 1167 (timer events during delay)
- **Event processing:**
  - Events are checked during blocking operations (input, delay)
  - Prevents new timer events during error handling
  - Uses callback mechanism — handlers are GOSUB targets or GOTO labels

**Implementation:**
1. **Event system architecture:** Polling-based with event queue
   - Events are queued by the runtime (keyboard from SDL2, timers from time checks, user events from UEVENT statement)
   - Generated C code polls for events and jumps to registered handlers using computed goto
   - Handlers are registered with label addresses (computed goto targets)

2. **Keyboard events (ON KEY):**
   - Integrated with SDL2 event loop in graphics backend
   - Key mapping: F1-F10 (keys 1-10), cursor keys (11-14: Up, Left, Right, Down)
   - Events queued when keys are pressed, consumed by generated C code
   - Supports KEY(n) ON/OFF/STOP control

3. **Timer events (ON TIMER):**
   - Timer registry tracks active timers with intervals
   - Generated C code checks timers periodically
   - Supports TIMER ON/OFF/STOP control
   - Multiple timers can be active simultaneously

4. **User events (ON UEVENT):**
   - Simple flag-based system
   - Triggered via UEVENT statement
   - Supports UEVENT ON/OFF/STOP control

**Files modified:**
- `runtime/src/events.rs` — Event system implementation (new, 600+ lines)
- `runtime/src/lib.rs` — Added events module export
- `runtime/include/qb64fresh_rt.h` — Added event handler FFI declarations
- `runtime/src/graphics/sdl2.rs` — Integrated keyboard event detection in poll_events()
- `src/codegen/c_backend/stmt/misc.rs` — Already emits event handler registration (was stubs)
- `src/codegen/c_backend/runtime/legacy.rs` — Stub implementations for inline runtime

**Runtime functions:**
- `qb_on_key(key_num, target)` — Register keyboard event handler
- `qb_key_control(key_num, mode)` — Control key event trapping (0=OFF, 1=ON, 2=STOP)
- `qb_on_timer(interval, target)` — Register timer event handler
- `qb_timer_control(mode)` — Control timer event trapping
- `qb_on_uevent(target)` — Register user event handler
- `qb_uevent_control(mode)` — Control user event trapping
- `qb_uevent_trigger()` — Trigger a user event
- `qb_check_key_event()` — Check for pending key event (returns key number)
- `qb_check_timer_event()` — Check for pending timer event (returns 1 if pending)
- `qb_check_uevent()` — Check for pending user event (returns 1 if pending)
- `qb_get_key_handler(key_num)` — Get handler label for key event
- `qb_get_timer_handler()` — Get handler label for timer event
- `qb_get_uevent_handler()` — Get handler label for user event
- `qb_events_clear_all()` — Clear all handlers (called on RUN)

**Note:** The generated C code needs to call the check functions and jump to handlers. This integration is handled by the code generator, which already emits the appropriate calls.

**Future enhancements:**
- Timer thread for more precise timing (currently relies on polling)
- Additional key mappings (keys 15-25, 30-31)
- ON COM, ON PEN, ON STRIG support (currently stubs with warnings)

---

## 3. Graphics Initialization Fixes

**Status:** ✅ Complete

**What it does:** QB64pe's IDE requires graphics window initialization. If this fails silently, the program may hang.

**QB64pe Implementation:**
- Graphics initialization is handled by the runtime library
- Window creation happens automatically when first graphics command is executed
- `$SCREENHIDE` directive can hide window at startup
- `_SCREENSHOW` statement can show/hide window at runtime
- QB64pe's IDE expects graphics window to be available for display

**Implementation:**
1. ✅ **Error handling:** Graphics initialization failures are now reported with error messages and the program exits with code 1
2. ✅ **$SCREENHIDE directive:** The directive is now properly implemented - after SCREEN statement initializes graphics, `qb_screenhide()` is called automatically if `$SCREENHIDE` was present
3. ✅ **_SCREENSHOW/_SCREENHIDE statements:** These runtime statements work correctly via `qb_screenshow()` and `qb_screenhide()` FFI functions

**Changes made:**
- Added `has_screen_hide()` function in `src/codegen/c_backend/analysis.rs` to detect `$SCREENHIDE` directive
- Modified `SCREEN` statement codegen in `src/codegen/c_backend/stmt/graphics.rs` to:
  - Check return value of `qb_gfx_screen()` and report errors
  - Call `qb_screenhide()` after initialization if `$SCREENHIDE` was requested
- Updated inline runtime `qb_gfx_screen()` to return `int` (not `void`) for error handling consistency
- Added `screen_hide_requested` field to `StmtEmitter` to track `$SCREENHIDE` directive

**Test file:** `examples/test_screenhide.bas` demonstrates the functionality.

---

## 4. File I/O for Internal Files

**Status:** ✅ Implemented

**What it does:** QB64pe reads from `internal/` directory. If paths are wrong, it may hang waiting for files.

**QB64pe Implementation:**
- QB64pe uses relative paths from the program's working directory
- `internal/` directory is expected to be relative to the executable or source file location
- File operations use standard C file I/O with path resolution
- QB64pe's IDE may set working directory differently than command-line execution

**Implementation:**
1. **Path resolution:** ✅ Implemented `resolve_file_path()` function that:
   - Detects paths starting with `internal/` (case-insensitive on Windows)
   - Resolves `internal/` paths relative to executable directory first
   - Falls back to current working directory if not found
   - For write operations, creates files in executable directory if path doesn't exist
   - Non-internal paths use current working directory (standard behavior)

2. **Working directory:** ✅ Program's working directory is correctly initialized via `qb_init_startdir()`

3. **File existence checks:** ✅ Added proper error handling:
   - Failed file opens log warnings but don't panic (matches QB64 behavior)
   - Errors are logged to stderr for debugging
   - Program continues execution even if file open fails

**Files modified:**
- `runtime/src/io/file.rs` — Added `resolve_file_path()` function and error handling

---

## 5. Summary of Action Items

### High Priority (Blocks QB64pe Execution)
1. ~~**Graphics initialization**~~ — ✅ **COMPLETE** (2026-01-29)
2. ~~**File I/O paths**~~ — ✅ **IMPLEMENTED** (2026-01-29)

### Medium Priority (Causes Incorrect Behavior)
1. ~~**Event trapping**~~ — ✅ **IMPLEMENTED** (2026-01-29)
2. ~~**`$USELIBRARY`**~~ — ✅ **IMPLEMENTED** (2026-01-29)
3. **`$EMBED`** — ⚠️ **PARSED BUT NOT IMPLEMENTED** — Requires embedding mechanism decision

### Low Priority (Nice to Have)
1. **`$VERSIONINFO` / `$EXEICON`** — ⚠️ **PARSED BUT NOT EMITTED** — Windows resource generation
2. ~~**`$COLOR`**~~ — ✅ **IMPLEMENTED** (metadata for LSP)
3. ~~**`$ASSERTS`**~~ — ✅ **IMPLEMENTED** (2026-01-29)
4. ~~**`$STATIC` / `$DYNAMIC`**~~ — ✅ **IMPLEMENTED** (2026-01-29)

---

## Next Steps

1. ~~**Test graphics and file I/O**~~ — ✅ **COMPLETE** — Graphics initialization and file I/O path resolution are implemented
2. ~~**Design event trapping system**~~ — ✅ **COMPLETE** — Event system implemented with polling-based architecture
3. ~~**Design library system**~~ — ✅ **COMPLETE** — `$USELIBRARY` implemented with QB64pe-compatible format
4. **Implement `$EMBED`** — ⚠️ **PENDING** — Choose embedding mechanism (C binary arrays recommended) and implement
5. **Implement `$VERSIONINFO` / `$EXEICON`** — ⚠️ **PENDING** — Emit Windows `.rc` files or integrate resource compiler

---

## Notes

- **Status Update (2026-01-29):** Most high-priority items have been implemented:
  - Graphics initialization with error handling ✅
  - File I/O path resolution for `internal/` directory ✅
  - Event trapping system (ON KEY, ON TIMER, ON UEVENT) ✅
  - `$USELIBRARY` library system ✅
  - `$STATIC` / `$DYNAMIC` array modes ✅
  - `$ASSERTS` and `$ASSERTS:CONSOLE` ✅
  - `$COLOR` directives (LSP metadata) ✅
- **Remaining items:** Only `$EMBED` and `$VERSIONINFO`/`$EXEICON` remain as pending implementation
- **Testing:** Focus on testing with actual QB64pe source to verify all implemented features work correctly
