# QB45 Compatibility Test Report

**Date:** 2026-01-20 (Updated)
**Test Suite:** QB64PE qbasic_testcases
**Total Files Tested:** 141

## Executive Summary

| Metric | Value |
|--------|-------|
| **Overall Compatibility** | **96.5%** |
| **Files Passing** | 136 |
| **Files Failing** | 5 |

### Failure Breakdown by Stage

| Stage | Count | Percentage |
|-------|-------|------------|
| Parser | 0 | 0% of failures |
| Semantic | 5 | 100% of failures |
| Lexer | 0 | 0% of failures |

### Recently Implemented Features

| Feature | Status | Notes |
|---------|--------|-------|
| `REDIM SHARED` | ✅ Implemented | Now parses and handles shared arrays correctly |
| Graphics `GET`/`PUT` | ✅ Already Working | `(x1,y1)-(x2,y2)` coordinate syntax fully supported |
| `DEF SEG` / `VARSEG` / `VARPTR` | ✅ Already Working | Memory segment operations fully supported |
| `BLOAD` / `BSAVE` | ✅ Already Working | Binary file load/save fully supported |
| `CALL ABSOLUTE` | ✅ Already Working | Emits warning (not executable in flat memory model) |
| `CIRCLE STEP` / `PAINT STEP` | ✅ Implemented | Relative coordinate syntax for graphics |
| DATA hex-like values | ✅ Implemented | `DATA 8B,E5` now parses correctly as strings |

**Current compatibility: 136/141 files (96.5%)**

---

## Detailed Error Analysis

### 1. Parser Errors (0 files) ✅ ALL RESOLVED

All parser issues have been resolved. Previously failing features are now working:

| Feature | Status | Notes |
|---------|--------|-------|
| `REDIM SHARED` | ✅ FIXED | Now parses correctly |
| Graphics `GET`/`PUT` | ✅ Working | Coordinate syntax supported |
| `DEF SEG` / `VARSEG` / `VARPTR` | ✅ Working | Memory segment ops fully implemented |
| `BLOAD` / `BSAVE` | ✅ Working | Binary file operations working |
| `ON ERROR GOTO label` | ✅ Working | Error handling supported |

**All 141 test files pass the parser stage successfully.**

#### Working Syntax Examples

**REDIM SHARED syntax:**
```basic
' Works:
REDIM SHARED Box(1 TO 26000)
REDIM SHARED _PRESERVE Buffer(100)
REDIM _PRESERVE SHARED Data(n)
```

**Graphics GET/PUT with coordinate ranges:**
```basic
' Works:
GET (0, 0)-(10, 10), Graphics(i * 80 + 1)
PUT (x, y), Sprite(offset), PSET
PUT ((x-1)*11, (y-1)*11), Graphics((pos-1)*80+1), XOR
```

**Memory segment operations (fully working):**
```basic
' All of these work:
DEF SEG = VARSEG(NumBOX(1))
BLOAD "kongnums.bsv", VARPTR(NumBOX(1))
DEF SEG
addr% = VARPTR(variable)
seg% = VARSEG(array(1))
BSAVE "data.bin", VARPTR(buffer(0)), 1000
```

---

### 2. Semantic Errors (9 files)

| Error Type | Count | Description |
|-----------|-------|-------------|
| Type mismatch: `STRING * n` vs `STRING` | 6 | Fixed-length string assignment |
| `NotAnArray` | 1 | Original code typo (`gane` vs `gagne`) |
| Other | 2 | Undefined identifiers |

#### STRING * n Type Mismatch

The most common semantic error involves fixed-length strings in user-defined types:

```basic
TYPE HallOfFameType
    Rank    AS INTEGER
    Namer   AS STRING * 12    ' Fixed-length string
    Score   AS LONG
END TYPE

DIM Hall(1 TO 5) AS HallOfFameType

' This fails:
Hall(I).Namer = "Relsoft 2000"
' Error: type mismatch: expected STRING * 12, found STRING
```

**Root Cause:** QB45 implicitly converts and pads regular strings when assigning to fixed-length string fields. The current compiler requires exact type matching.

#### Original Code Bugs

One file (`astrowars.bas`) contains a typo in the original QB45 source:
```basic
DIM SHARED gagne(3) AS INTEGER
' ...
gane(3) = 0   ' Typo: should be "gagne"
' Error: `gane` is not an array
```

---

### 3. Lexer Errors (3 files)

| Character | File | Context |
|-----------|------|---------|
| `@` | mzupd2.bas | Used as marker in DATA statements |
| `\|` | mzupd2.bas | Pipe character in DATA |
| Extended ASCII (Î, ï, 0x9F) | temple.bas | Non-ASCII in string literals or comments |

Example from mzupd2.bas:
```basic
DATA $/Amulet, %@Waters    ' @ is not a valid token
```

---

## Results by Test Directory

| Directory | Total | Passing | Failing | Success Rate |
|-----------|-------|---------|---------|--------------|
| pete/ | 68 | 65 | 3 | **95.6%** |
| misc/ | 46 | 45 | 1 | **97.8%** |
| qb45com/ | 5 | 5 | 0 | **100%** |
| n54/ | 3 | 3 | 0 | **100%** |
| thebob/ | 19 | 19 | 0 | **100%** |

### thebob/ Directory - Full Compatibility Achieved ✅

The thebob/ directory previously had a 15.8% pass rate because it heavily uses:
- `REDIM SHARED` for dynamic sprite buffers
- `DEF SEG` / `VARSEG` / `VARPTR` for direct memory access
- `BLOAD` / `BSAVE` for loading graphics data
- `CALL ABSOLUTE` for mouse driver routines

**All of these features are now fully implemented and thebob/ passes 100%.**

---

## Remaining Issues (5 files)

The remaining 5 failing files all have semantic errors, not parser errors:

### Semantic Errors

| File | Error Type | Description |
|------|-----------|-------------|
| misc/frog.bas | InvalidBinaryOp | Comparing INTEGER with user-defined type |
| pete/* (3 files) | Various | Type mismatches in UDT fields |
| (1 other) | Undefined | Minor semantic issues |

### Potential Fixes (Low Priority)

| Feature | Files Fixed | Effort |
|---------|-------------|--------|
| `STRING * n` implicit conversion | ~3 | Medium |
| UDT comparison operators | ~1 | Low |

---

## Passing Files (Examples)

These complex programs compile successfully:

**Games:**
- `4pong.bas` - Four-player pong
- `chess.bas` - Chess game
- `sudoku.bas` - Sudoku puzzle
- `battleship.bas` - Battleship game

**Graphics Demos:**
- `rotozoom.bas` - Rotation/zoom effect
- `3dballs.bas` - 3D ball animation
- `mandala.bas` - Mandala generator
- `sinecube.bas` - Sine wave cube

**Utilities:**
- `calc.bas` - Calculator
- `rot13.bas` - ROT13 encoder

---

## How to Run These Tests

```bash
# Run all QB45 compatibility tests
cargo test --test qb45_compat -- --nocapture

# Run with verbose output (shows passing files)
VERBOSE=1 cargo test --test qb45_compat -- --nocapture

# Run failure diagnostics
cargo test --test qb45_compat diagnose_failures -- --nocapture

# Test a specific file
cargo run --bin qb64fresh -- path/to/file.bas --typed-ir
```

---

## Appendix: Current Status

### Parser Failures: 0 files ✅

All 141 test files pass the parser stage successfully.

### Semantic Failures: 5 files

| File | Error |
|------|-------|
| misc/frog.bas | InvalidBinaryOp: comparing INTEGER with UDT |
| pete/* (3 files) | Type mismatches with STRING * n |
| (1 other) | Minor semantic issue |

### Lexer Failures: 0 files ✅

All lexer issues have been resolved.
