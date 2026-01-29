# Session 087: Missing Features Fixes

**Date:** 2026-01-29

## $USELIBRARY Implementation

Implemented `$USELIBRARY:'author/library'` directive functionality to parity with QB64pe.

### Implementation Details

1. **Created `src/library.rs` module** for library management:
   - `LibraryManager` struct for library discovery and tracking
   - `LibraryInfo` struct containing three inclusion points (AtTop, AfterMain, AtBottom)
   - INI file parsing for library descriptors
   - Duplicate prevention (same library + same referrer)
   - Library structure: `libraries/descriptors/{author/library}.ini` and `libraries/includes/{author/library}/`

2. **Updated preprocessor** (`src/preprocessor.rs`):
   - Added `$USELIBRARY` directive parsing
   - Integrated library manager into preprocessing context
   - Library files included at three points:
     - **AtTop**: At the very beginning (in reverse order for dependencies)
     - **AfterMain**: After all user code
     - **AtBottom**: At the very end
   - Referrer tracking (file:line) for duplicate detection

3. **Features implemented**:
   - Library descriptor discovery from `libraries/descriptors/`
   - INI file parsing with `[LIBRARY INCLUDES]` section
   - Validation that all specified inclusion files exist
   - Duplicate prevention (same library + same referrer)
   - Error handling with clear messages
   - Support for optional inclusion points (not all three required)

### Testing

- Added unit tests for INI parsing (quotes, comments, sections)
- Added unit tests for library loading and registration
- Added integration test for full preprocessing with `$USELIBRARY`
- All tests passing ✓

### QB64pe Compatibility

Matches QB64pe's implementation:
- Same library directory structure
- Same INI format and section names
- Same inclusion point names (IncAtTop, IncAfterMain, IncAtBottom)
- Same duplicate prevention logic
- Same referrer tracking format

### Files Modified

- `src/lib.rs` - Added `library` module
- `src/library.rs` - New library management module (468 lines)
- `src/preprocessor.rs` - Added `$USELIBRARY` support and library inclusion

---  
**Focus:** Fixing implementable issues from QB64PE_MISSING_FEATURES.md

---

## Summary

Fixed several missing features from the QB64PE_MISSING_FEATURES.md document that could be implemented without user interaction. Created documentation for features that require design decisions or user input.

---

## Implemented Features

### 1. `Version$` Built-in Function ✅

**Status:** Implemented

**Changes:**
- Registered `Version$` as builtin function in `src/semantic/builtins.rs`
- Added codegen mapping in `src/codegen/c_backend/expr.rs`
- Implemented runtime function `qb_version()` in `src/codegen/c_backend/runtime/keyboard.rs`
- Returns "QB64Fresh 0.1.0" (can be updated later with actual version)

**Files Modified:**
- `src/semantic/builtins.rs`
- `src/codegen/c_backend/expr.rs`
- `src/codegen/c_backend/runtime/keyboard.rs`

---

### 2. `$INCLUDEONCE` Preprocessor Directive ✅

**Status:** Implemented

**Changes:**
- Added `include_once_visited` HashSet to `PreprocessContext` to track files included with `$INCLUDEONCE`
- Updated `parse_include_directive()` to return `Option<(String, bool)>` where bool indicates `$INCLUDEONCE`
- Modified `preprocess_internal()` to skip files already included with `$INCLUDEONCE`
- Updated tests to reflect new return type

**Behavior:**
- Files included with `$INCLUDE` can be included multiple times (existing behavior)
- Files included with `$INCLUDEONCE` are only included once, even if the directive appears multiple times
- Regular `$INCLUDE` and `$INCLUDEONCE` are tracked separately

**Files Modified:**
- `src/preprocessor.rs`

---

### 3. Verified Existing Implementations ✅

**DEFTYPE (DEFLNG, DEFINT, etc.):**
- Already fully implemented
- `collect_deftype_declarations()` processes DEFTYPE statements
- `SymbolTable::set_default_type()` sets default types for letter ranges
- `SymbolTable::default_type_for()` uses DEFTYPE when inferring variable types
- Used throughout semantic analysis for type inference

**FIELD/LSET/RSET:**
- Already fully implemented
- Parsed in `src/parser/statements/control_etc.rs`
- Runtime functions in `src/codegen/c_backend/runtime/file.rs`
- Codegen in `src/codegen/c_backend/stmt/misc.rs`

**LBOUND/UBOUND:**
- Already fully implemented
- Runtime functions in `src/codegen/c_backend/runtime/arrays.rs`
- Codegen handles both 1-arg and 2-arg forms

**MKI$/CVI and related:**
- Already fully implemented
- All variants (MKI$, MKL$, MKS$, MKD$, CVI, CVL, CVS, CVD) are registered and have runtime functions

**`_OS$`, `_DIREXISTS`, `_SCREENSHOW`:**
- Already fully implemented
- `_OS$` returns platform string in `[PLATFORM][BITS]` format
- `_DIREXISTS` checks directory existence
- `_SCREENSHOW` shows graphics window

**`_ERRORLINE`, `_ERRORMESSAGE$`:**
- Already fully implemented
- Runtime functions in `src/codegen/c_backend/runtime/error.rs`

---

## Documentation Created

### QB64PE_MISSING_FEATURES_USER_INTERACTION.md

Created comprehensive documentation for features that require user decisions or interaction:

**Features Documented:**
1. **`$USELIBRARY`** — Requires library system design (discovery, format, dependencies)
2. **`$EMBED`** — Requires embedding mechanism decision (C strings vs. binary arrays vs. linker sections)
3. **`$VERSIONINFO` / `$EXEICON`** — Requires Windows resource compiler integration
4. **`$COLOR`** — IDE-only, needs LSP integration decision
5. **`$ASSERTS`** — Requires assertion mechanism design
6. **`$STATIC` / `$DYNAMIC`** — Requires static array implementation
7. **Event Trapping** — Requires runtime architecture decision (polling vs. callbacks vs. signals)
8. **Graphics Initialization** — Needs testing/debugging
9. **File I/O Paths** — Needs testing/debugging

Each feature includes:
- Current status
- What it does
- What needs to be decided
- Recommendations
- Files that need modification

---

## Updated Documentation

### QB64PE_MISSING_FEATURES.md

Updated status for all fixed features:
- `$INCLUDEONCE` — ✅ Implemented
- `_OS$` — ✅ Implemented
- `Version$` — ✅ Implemented
- `DEFLNG A-Z` and all DEFTYPE variants — ✅ Implemented
- `FIELD`, `LSET`, `RSET` — ✅ Implemented
- `LBOUND` / `UBOUND` — ✅ Implemented
- `MKI$` / `CVI` and related — ✅ Implemented
- `_DIREXISTS`, `_SCREENSHOW` — ✅ Implemented
- `_ERRORLINE`, `_ERRORMESSAGE$` — ✅ Implemented

Updated priority list to reflect completed items and added link to user interaction document.

---

## Testing Notes

- All changes are backward compatible
- `$INCLUDEONCE` behavior matches QB64pe (skip if already included)
- `Version$` returns a placeholder version string (can be updated with actual version later)
- Existing implementations verified through code review

---

## Next Steps

1. **Test with QB64pe source** — Verify that fixed features work correctly when compiling QB64pe
2. **Review user interaction document** — Make decisions on features requiring design choices
3. **Update version string** — Replace "QB64Fresh 0.1.0" with actual version when available
4. **Test graphics and file I/O** — Verify paths and initialization work correctly

---

## Files Modified

1. `src/semantic/builtins.rs` — Added `Version$` registration
2. `src/codegen/c_backend/expr.rs` — Added `Version$` codegen mapping
3. `src/codegen/c_backend/runtime/keyboard.rs` — Implemented `qb_version()` function
4. `src/preprocessor.rs` — Implemented `$INCLUDEONCE` support
5. `docs/QB64PE_MISSING_FEATURES.md` — Updated status for fixed features
6. `docs/QB64PE_MISSING_FEATURES_USER_INTERACTION.md` — New document for features requiring decisions

---

## Decisions Made

1. **`Version$` implementation:** Simple string return for now, can be enhanced later with version from Cargo.toml
2. **`$INCLUDEONCE` behavior:** Skip duplicate includes silently (matches QB64pe behavior)
3. **Documentation approach:** Separate document for features requiring decisions to keep main document focused on status
