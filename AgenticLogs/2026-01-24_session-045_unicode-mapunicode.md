# Session 045: Unicode Support (_MAPUNICODE Implementation)

**Date:** 2026-01-24
**Focus:** Implement _MAPUNICODE to achieve QB64PE Unicode parity

## Summary

Implemented functional `_MAPUNICODE` support to match QB64PE's Unicode handling. Based on research from the previous session, QB64PE's Unicode is **font-rendering-centric**, not string-handling-centric. The minimum parity required was making `_MAPUNICODE` work.

## Research Findings (from previous session)

QB64PE's Unicode support consists of:
1. **_MAPUNICODE** - Maps ASCII positions (0-255) to Unicode codepoints for font rendering
2. **_UPRINTSTRING**, **_UPRINTWIDTH**, **_UCHARPOS** - Unicode text rendering functions
3. String functions (LEN, LEFT$, MID$, etc.) remain **byte-based**

## Changes Made

### Inline Runtime (`src/codegen/c_backend/runtime/system.rs`)

**Before:** `_MAPUNICODE` was a no-op stub that ignored all parameters.

**After:** Full implementation with:

1. **Static CP437 Mapping Table:**
   ```c
   static int32_t _qb_unicode_map[256] = {
       /* 0x00-0x7F: Standard ASCII (maps to same Unicode) */
       /* 0x80-0xFF: Extended ASCII (CP437 to Unicode) */
   };
   ```
   - All 256 entries initialized with Code Page 437 defaults
   - Uses official Unicode Consortium CP437 mappings

2. **Statement Form:** `_MAPUNICODE unicode_codepoint%, ascii_position%`
   ```c
   void qb_mapunicode(int32_t unicode_code, int32_t ascii_pos) {
       if (ascii_pos >= 0 && ascii_pos < 256) {
           _qb_unicode_map[ascii_pos] = unicode_code;
       }
   }
   ```

3. **Function Form:** `_MAPUNICODE(ascii_position%)`
   ```c
   int32_t qb__mapunicode1(int32_t ascii_pos) {
       if (ascii_pos >= 0 && ascii_pos < 256) {
           return _qb_unicode_map[ascii_pos];
       }
       return 0;
   }
   ```

### CP437 Mapping Highlights

| ASCII Range | Unicode Mapping |
|-------------|-----------------|
| 0x00-0x7F | Direct 1:1 mapping |
| 0x80-0x9F | Accented letters (Ç, ü, é, â, ä, etc.) |
| 0xA0-0xAF | Spanish/math (á, í, ñ, ¿, ¡, etc.) |
| 0xB0-0xDF | Box-drawing characters (┌, ─, │, └, etc.) |
| 0xE0-0xEF | Greek letters (α, β, Γ, π, Σ, etc.) |
| 0xF0-0xFF | Math symbols (≡, ±, ≥, ≤, √, ², etc.) |

## Usage Example

```basic
' Check default CP437 mapping
PRINT _MAPUNICODE(128)  ' Returns 199 (0x00C7) - Ç

' Custom mapping for special font
_MAPUNICODE 9786, 1     ' Map smiley face (☺) to position 1
PRINT _MAPUNICODE(1)    ' Returns 9786

' Reset a mapping
_MAPUNICODE 1, 1        ' Restore position 1 to its ASCII value
```

## Files Modified

- `src/codegen/c_backend/runtime/system.rs` - Full _MAPUNICODE implementation with CP437 table

## Testing

- All 388 library tests pass
- Verified generated C code includes complete mapping table
- Verified statement and function forms work correctly

## QB64PE Parity Status

| Feature | Status | Notes |
|---------|--------|-------|
| _MAPUNICODE statement | ✅ Complete | Sets ASCII→Unicode mapping |
| _MAPUNICODE function | ✅ Complete | Returns Unicode for ASCII position |
| CP437 default mapping | ✅ Complete | Full 256-entry table |
| _UPRINTSTRING | ❌ Stub | Would need FreeType integration |
| _UPRINTWIDTH | ❌ Stub | Would need font metrics |
| Unicode string functions | N/A | QB64PE also byte-based |

This achieves the minimum Unicode parity with QB64PE - programs using `_MAPUNICODE` for codepage customization will work correctly.

## References

- [Unicode Consortium CP437 Mapping](https://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT)
- [Code Page 437 - Wikipedia](https://en.wikipedia.org/wiki/Code_page_437)
