# QB64pe IDE Compilation and Execution Plan

**Created:** 2026-02-03  
**Goal:** Compile QB64pe (including its IDE) using QB64Fresh and successfully run the IDE

---

## Current Status

### ✅ What's Already Working

1. **QB64pe Compilation** ✅ COMPLETE
   - QB64pe source (~59,000 lines, 39 files) compiles successfully with QB64Fresh
   - All phases pass: preprocessing, lexer, parser, semantic analysis, code generation
   - C code generation produces ~86K lines of C code
   - GCC compilation succeeds (0 errors)
   - Executable builds and runs (starts without crashing)

2. **Runtime Infrastructure** ✅ MOSTLY COMPLETE
   - External runtime library exists (`runtime/`)
   - Graphics FFI layer implemented (`runtime/src/graphics_ffi.rs` - 2973 lines)
   - SDL2 graphics backend available (`runtime/src/graphics/sdl2.rs`)
   - Runtime can be built with graphics support: `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`

3. **Bootstrap Test Suite** ✅ COMPLETE
   - 27 bootstrap tests (23 run by default, 4 ignored)
   - Code generation validated
   - Regression tests passing

### ⚠️ What's Missing or Needs Work

1. **Full Execution Testing** ⚠️ PENDING
   - QB64pe executable compiles but full IDE functionality not yet tested
   - Runtime integration needs verification

2. **Graphics Runtime Functions** ⚠️ NEEDS VERIFICATION
   - Many graphics functions implemented, but need to verify all QB64pe IDE needs are present
   - Font rendering for IDE text display
   - Mouse input for IDE interaction
   - Window management (_SCREENSHOW, _SCREENHIDE)

3. **Runtime Compatibility** ⚠️ NEEDS CLARIFICATION
   - QB64pe-only compatibility modules exist (qbs_compat, mem_lock, etc.)
   - Need to determine if these are required for QB64pe IDE execution
   - Feature flag approach recommended (`qb64pe-compat`)

---

## What QB64pe IDE Needs

Based on the bootstrap plan and IDE functionality checklist, QB64pe IDE requires:

### 1. Graphics Functions (Critical)

**Screen Management:**
- `SCREEN` mode setting (text/graphics modes)
- `_NEWIMAGE` for creating image handles
- `_SCREENSHOW` / `_SCREENHIDE` for window visibility
- `_DISPLAY` for page flipping

**Text Rendering (Critical for IDE):**
- `_PRINTSTRING` for drawing text to screen
- Font support (8x8 and 16x16 built-in fonts)
- CP437 character encoding support
- Text coordinate system (1-based vs 0-based)

**Graphics Primitives:**
- `LINE` for drawing lines (menus, borders)
- `PSET` / `POINT` for pixel operations
- `CIRCLE` for UI elements
- `PAINT` for filling areas

**Image Operations:**
- `_PUTIMAGE` for copying image data
- `_LOADIMAGE` for loading resources
- `_FREEIMAGE` for cleanup

**Color Management:**
- `_RGB` / `_RGBA` for color values
- Color palette support

### 2. Input Functions (Critical)

**Keyboard:**
- `INKEY$` for character input
- `_KEYHIT` for key press detection
- `_KEYDOWN` for key state checking
- `_KEYCLEAR` for clearing input buffer

**Mouse:**
- `_MOUSEINPUT` for mouse event polling
- `_MOUSEX` / `_MOUSEY` for cursor position
- `_MOUSEBUTTON` for button state
- `_MOUSESHOW` / `_MOUSEHIDE` for cursor visibility
- `_MOUSEMOVEMENTX` / `_MOUSEMOVEMENTY` for relative movement

### 3. Runtime Initialization

**Critical:**
- `qb_runtime_init()` must be called before any graphics operations
- Proper initialization order (runtime → args → startdir → graphics)
- Error handling for initialization failures

### 4. String Operations

**Required:**
- String concatenation
- String comparison
- MID$ operations
- String arrays

### 5. File I/O

**Required:**
- `OPEN` / `CLOSE` for file operations
- `INPUT#` / `LINE INPUT#` for reading
- `PRINT#` / `WRITE#` for writing
- File existence checking

---

## Step-by-Step Plan

### Phase 1: Verify Runtime Build ✅ (Should be done)

1. **Build runtime with graphics:**
   ```bash
   cd runtime
   cargo build --release --features graphics-sdl2
   ```

2. **Verify runtime library exists:**
   - Check: `target/release/libqb64fresh_rt.a` (or `.so` on Linux)
   - Should be ~58MB with graphics support

### Phase 2: Compile QB64pe ✅ (Already working)

1. **Compile QB64pe source:**
   ```bash
   cd QB64Fresh
   cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --runtime external -o qb64pe.c
   ```

2. **Verify C code generation:**
   - Should produce ~86K lines of C code
   - Should use `#include "qb64fresh_rt.h"` (external runtime mode)

### Phase 3: Link and Build Executable ⚠️ (Needs verification)

1. **Compile C code with runtime:**
   ```bash
   gcc -I runtime/include \
       qb64pe.c \
       -L target/release \
       -lqb64fresh_rt \
       $(pkg-config --libs sdl2) \
       -lm -lpthread -ldl \
       -o qb64pe_bootstrapped
   ```

2. **Verify executable builds:**
   - Should produce `qb64pe_bootstrapped` executable
   - Should link without errors

### Phase 4: Test Basic Execution ⚠️ (Needs work)

1. **Test command-line mode:**
   ```bash
   ./qb64pe_bootstrapped -h
   ```
   - Should display help text ✅ (Already verified)

2. **Test console mode:**
   ```bash
   ./qb64pe_bootstrapped -x
   ```
   - Should run in console mode (if supported)

### Phase 5: Test IDE Launch 🔴 (Critical - Not yet done)

1. **Launch IDE:**
   ```bash
   ./qb64pe_bootstrapped
   ```
   - Should open graphics window
   - Should display IDE interface

2. **Verify graphics initialization:**
   - Window should appear
   - Screen should be initialized
   - No crashes on startup

### Phase 6: Test IDE Functionality 🔴 (Critical - Not yet done)

1. **Basic IDE operations:**
   - [ ] Window displays correctly
   - [ ] Text rendering works (_PRINTSTRING)
   - [ ] Mouse input works
   - [ ] Keyboard input works
   - [ ] Menu navigation works
   - [ ] File open dialog works
   - [ ] Editor displays code
   - [ ] Syntax highlighting (if applicable)

2. **Compilation from IDE:**
   - [ ] Can compile a simple BASIC program
   - [ ] Can run compiled program
   - [ ] Error messages display correctly

---

## Potential Issues and Solutions

### Issue 1: Missing Graphics Functions

**Symptoms:**
- IDE window doesn't appear
- Text doesn't render
- Graphics operations fail

**Solution:**
- Verify all required graphics functions are in `runtime/src/graphics_ffi.rs`
- Check `runtime/include/qb64fresh_rt.h` has all declarations
- Test individual graphics functions

### Issue 2: Font Rendering Problems

**Symptoms:**
- Text appears garbled
- Wrong character encoding
- Font size incorrect

**Solution:**
- Verify CP437 encoding support
- Check font loading (8x8 and 16x16)
- Verify coordinate system (1-based vs 0-based)

### Issue 3: Input Not Working

**Symptoms:**
- Keyboard input not captured
- Mouse clicks not detected
- Input buffer issues

**Solution:**
- Verify keyboard FFI functions (`runtime/src/io.rs`)
- Check mouse input implementation
- Test input event loop

### Issue 4: Runtime Initialization Order

**Symptoms:**
- Crashes on startup
- Graphics not initialized
- Runtime errors

**Solution:**
- Verify `qb_runtime_init()` called first
- Check initialization order in generated C code
- Ensure error handling present

### Issue 5: Missing Runtime Symbols

**Symptoms:**
- Linker errors for missing symbols
- Runtime function not found

**Solution:**
- Check if QB64pe-only compatibility modules needed
- Enable `qb64pe-compat` feature flag if required
- Verify all QB64pe-called functions are exported

---

## Testing Strategy

### 1. Incremental Testing

**Start simple:**
1. Test graphics initialization only
2. Test text rendering with simple program
3. Test mouse/keyboard input
4. Test full IDE launch
5. Test IDE functionality

### 2. Debugging Tools

**Use C profiler:**
- `gdb` for debugging crashes
- `valgrind` for memory issues
- `strace` for system call tracing

**Runtime logging:**
- Graphics FFI functions log errors to stderr
- Check error messages for clues

### 3. Comparison Testing

**Compare with original QB64pe:**
- Run same operations in original QB64pe
- Compare behavior
- Identify differences

---

## Success Criteria

### Minimum Viable (Phase 1)
- [ ] QB64pe compiles with QB64Fresh ✅ (Already done)
- [ ] Executable builds and links ✅ (Already done)
- [ ] Executable runs without crashing ✅ (Already done)
- [ ] Help text displays correctly ✅ (Already done)

### Basic IDE (Phase 2)
- [ ] IDE window opens
- [ ] Graphics display works
- [ ] Text rendering works
- [ ] Window can be closed

### Functional IDE (Phase 3)
- [ ] Can open files
- [ ] Can edit code
- [ ] Can compile programs
- [ ] Can run programs
- [ ] Error messages display

### Full Parity (Phase 4)
- [ ] All IDE features work
- [ ] Performance acceptable
- [ ] No crashes during normal use
- [ ] Can compile QB64pe itself (meta-bootstrap)

---

## Next Steps (Immediate Actions)

1. **Verify runtime build:**
   ```bash
   cd runtime && cargo build --release --features graphics-sdl2
   ```

2. **Compile QB64pe:**
   ```bash
   cd QB64Fresh
   cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --runtime external -o qb64pe.c
   ```

3. **Link executable:**
   ```bash
   gcc -I runtime/include qb64pe.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2) -lm -lpthread -ldl -o qb64pe_bootstrapped
   ```

4. **Test IDE launch:**
   ```bash
   ./qb64pe_bootstrapped
   ```

5. **Debug issues:**
   - Check stderr for error messages
   - Use gdb if crashes occur
   - Verify graphics functions are called correctly

---

## Resources

- **Bootstrap Plan:** `docs/archive/BOOTSTRAP_PLAN_FULL.md`
- **IDE Checklist:** `docs/QB64pe/QB64PE_IDE_FUNCTIONALITY_CHECKLIST.md`
- **Runtime Compatibility:** `docs/QB64PE_COMPATIBILITY_ANALYSIS.md`
- **Graphics FFI:** `runtime/src/graphics_ffi.rs`
- **Bootstrap Tests:** `tests/bootstrap_tests.rs`

---

## Notes

- QB64pe requires **external runtime with graphics** (not inline runtime)
- Runtime must be built with `--features graphics-sdl2`
- SDL2 must be installed on system (`pkg-config --libs sdl2` should work)
- Memory limit: Use `ulimit -v 4194304` (4GB) when running QB64pe
- QB64pe IDE is a GUI application - needs display/X11 on Linux
