# Future Work & Unsupported Features

This document tracks features that are not yet implemented or cannot be supported in QB64Fresh.

**Last Updated:** January 24, 2026

---

## DECLARE LIBRARY Support

QB64Fresh supports `DECLARE LIBRARY`, `DECLARE DYNAMIC LIBRARY`, and `DECLARE STATIC LIBRARY` for C interop. Since we generate C code, this integration is natural and efficient.

### What Works

| Feature | Status | Notes |
|---------|--------|-------|
| Static library linking | ✅ | `DECLARE LIBRARY "mylib"` |
| Dynamic library linking | ✅ | `DECLARE DYNAMIC LIBRARY "plugin"` |
| Header-only (system) | ✅ | `DECLARE LIBRARY` (no library name) |
| ALIAS support | ✅ | `FUNCTION foo ALIAS "c_foo"` |
| BYVAL parameters | ✅ | Pass by value (default for C) |
| BYREF parameters | ✅ | Pass by reference (pointer) |
| Automatic header parsing | ✅ | Parse `.h` files for declarations |

### Type Mapping

| BASIC Type | C Type | Works? |
|------------|--------|--------|
| `INTEGER` | `int16_t` | ✅ |
| `LONG` | `int32_t` | ✅ |
| `_INTEGER64` | `int64_t` | ✅ |
| `SINGLE` | `float` | ✅ |
| `DOUBLE` | `double` | ✅ |
| `STRING` (BYVAL) | `const char*` | ✅ |
| `_UNSIGNED INTEGER` | `uint16_t` | ✅ |
| `_UNSIGNED LONG` | `uint32_t` | ✅ |
| `_UNSIGNED _INTEGER64` | `uint64_t` | ✅ |
| `_OFFSET` | `void*` | ⚠️ Partial |
| `_MEM` | struct | ❌ Not yet |

### Example: System Library Calls

```basic
' Works: Call C standard library functions
DECLARE LIBRARY
    FUNCTION strlen& (BYVAL s AS STRING)
    FUNCTION atoi& (BYVAL s AS STRING)
    FUNCTION getenv$ (BYVAL name AS STRING)
END DECLARE

PRINT "PATH length:"; strlen(ENVIRON$("PATH"))
PRINT "Integer:"; atoi("42")
```

### Example: Custom Library

```basic
' Works: Link against a custom C library
DECLARE LIBRARY "mymath"
    FUNCTION fast_sqrt# ALIAS "sqrt_fast" (BYVAL x AS DOUBLE)
    SUB matrix_multiply (a AS _OFFSET, b AS _OFFSET, result AS _OFFSET, BYVAL size AS LONG)
END DECLARE
```

### Limitations

#### 1. QB64-Specific Bundled Libraries

QB64pe includes several bundled libraries that rely on its internal implementation:

| Library | Status | Workaround |
|---------|--------|------------|
| InForm GUI | ❌ | Use native GUI libraries via DECLARE LIBRARY |
| QB64 sound extensions | ✅ | Built into QB64Fresh runtime |
| QB64 OpenGL bindings | ❌ | Use SDL2 graphics instead |

#### 2. Header Parsing Limitations

The automatic header parser (`DECLARE LIBRARY "file.h"`) has limitations:

| Feature | Supported? |
|---------|------------|
| Simple function declarations | ✅ |
| Basic C types | ✅ |
| `stdint.h` types | ✅ |
| Preprocessor macros | ❌ |
| `#ifdef` conditionals | ❌ |
| Struct/union definitions | ❌ |
| Function-like macros | ❌ |
| C++ classes | ❌ |

**Workaround:** Manually declare functions instead of relying on header parsing:

```basic
' Instead of: DECLARE LIBRARY "complex_header.h"
' Manually declare what you need:
DECLARE LIBRARY "mylib"
    FUNCTION my_function& (BYVAL x AS LONG)
END DECLARE
```

#### 3. _OFFSET Limitations

`_OFFSET` (pointer type) has partial support:

| Operation | Supported? |
|-----------|------------|
| Pass to C functions | ✅ |
| Return from C functions | ✅ |
| `_OFFSET` variables | ✅ |
| Pointer arithmetic | ⚠️ Limited |
| Dereferencing | ❌ Use `_MEM` |
| Array element access | ❌ Use `_MEM` |

#### 4. _MEM Block Support

The `_MEM` type for direct memory access is not yet fully implemented:

```basic
' Not yet supported:
DIM m AS _MEM
m = _MEM(array())
_MEMPUT m, m.OFFSET, value
_MEMGET m, m.OFFSET, result
_MEMFREE m
```

**Workaround:** Use `_OFFSET` with C helper functions for memory operations.

#### 5. Callback Functions

Passing BASIC functions as callbacks to C code is not supported:

```basic
' Not supported:
DECLARE LIBRARY
    SUB qsort (base AS _OFFSET, BYVAL n AS LONG, BYVAL size AS LONG, compare AS _OFFSET)
END DECLARE

' Cannot pass a BASIC FUNCTION as the compare parameter
```

**Workaround:** Write the callback in C and link it.

#### 6. Platform-Specific Libraries

Libraries using platform-specific APIs work but require conditional compilation:

```basic
' Windows-only:
$IF WIN THEN
    DECLARE LIBRARY
        FUNCTION GetTickCount~& ()
    END DECLARE
$ELSE
    ' Use alternative on Linux/macOS
$END IF
```

### Safety Considerations

DECLARE LIBRARY enables unsafe operations. Be aware:

1. **No runtime type checking** - Type mismatches cause undefined behavior
2. **Manual memory management** - C allocations must be freed by C code
3. **String lifetime** - Temporary C strings become invalid after the call
4. **Platform differences** - Code may not be portable across OS/architectures

### See Also

- [ADR-0008: C Interoperability](../adrs/ADR-0008-c-interoperability.md) - Full design document
- [Migration Guide](../MIGRATION_GUIDE.md) - Porting QB64 programs

---

## Remaining Work

### Legacy Stubs (Very Low Priority)
- `ERDEV`/`ERDEV$` - DOS device errors (stub only)
- `ON COM`/`ON UEVENT`/`ON SIGNAL` - Event handlers (not in QB64pe either)
- `PEN` - Light pen (obsolete hardware)

### Future Enhancements
- Full `_MEM` block support for direct memory access
- Callback function support for C interop
- Enhanced `_OFFSET` operations

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 420+

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~415 | 99% |
| 🔨 Legacy Stubs | ~5 | 1% |

---

## Version History

- 2026-01-24: Expanded DECLARE LIBRARY documentation with limitations and workarounds
- 2026-01-24: _MAPTRIANGLE implemented with software rasterizer (barycentric texture mapping)
- 2026-01-24: _COPYPALETTE and _DISPLAYORDER implemented
- 2026-01-24: Windows-only features implemented (_SCREENPRINT, _SCREENCLICK, _SCREENIMAGE, _WINDOWHANDLE)
- 2026-01-24: Major update - audio complete, window control, alpha blending, INT 0x33 mouse emulation
- 2026-01-23: Updated with implementation status (infrastructure vs runtime)
- 2026-01-20: Initial creation, documented OpenGL limitations and feature roadmap
