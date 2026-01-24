# Session 046: Unicode QB64pe Parity Check

**Focus:** Assess whether QB64Fresh matches QB64pe Unicode functionality (feature parity only).

## Conclusion

**We do not have QB64pe Unicode parity.** `_MAPUNICODE` is fully implemented; the five Unicode font / print functions are not.

## QB64pe Unicode-Related Features

| Feature | QB64pe | QB64Fresh |
|---------|--------|-----------|
| `_MAPUNICODE` (statement + function) | ✓ `qb64pe.c` / runtime | ✓ Full (CP437, statement + function) |
| `_UPRINTSTRING` | ✓ `font.cpp` (FreeType) | Parser/codegen map to `qb_uprintstring`; **no runtime definition**; semantic may report `UndefinedProcedure` for some forms |
| `_UPRINTWIDTH` | ✓ `font.cpp` | Codegen → `qb_uprintwidth`; **no runtime definition** |
| `_UCHARPOS` | ✓ `font.cpp` | Codegen → `qb_ucharpos`; **no runtime definition** |
| `_UFONTHEIGHT` | ✓ `font.cpp` | Codegen → `qb_ufontheight`; **no runtime definition** |
| `_ULINESPACING` | ✓ `font.cpp` | Codegen → `qb_ulinespacing`; **no runtime definition** |

## Changes Made

- **FUTURE.md** (Unicode Support): Reworded to remove “`_UPRINTSTRING`, `_UPRINTWIDTH` remain stubs.” Clarified that QB64pe implements all five in its font layer (FreeType). QB64Fresh parses and emits `qb_*` for all five, but the **inline runtime does not define** `qb_uprintstring`, `qb_uprintwidth`, `qb_ucharpos`, `qb_ufontheight`, `qb_ulinespacing`. Added that for stub-level parity we need to add those five definitions (e.g. no-op or safe defaults).

## Note

Adding the five as runtime stubs would provide API/stub-level parity so that programs using them can link. Full behavioral parity would require a FreeType-based font layer similar to QB64pe’s `font.cpp`.
