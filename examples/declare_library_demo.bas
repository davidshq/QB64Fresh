' ============================================================================
' DECLARE LIBRARY Demo for QB64Fresh
' ============================================================================
' This demonstrates the C library integration feature that allows
' BASIC programs to call external C functions.
'
' DECLARE LIBRARY syntax:
'   DECLARE [DYNAMIC] LIBRARY ["library_name"]
'       FUNCTION name[(params)] [ALIAS "c_name"]
'       SUB name[(params)] [ALIAS "c_name"]
'   END DECLARE
'
' Key features:
' - BYVAL keyword passes parameters by value (required for C interop)
' - ALIAS maps BASIC names to C function names
' - DYNAMIC loads library at runtime (vs static linking)
' - Type suffixes (& for LONG, $ for STRING, etc.) specify return types
' ============================================================================

' Example 1: Declare standard C library functions
' Note: This declares functions from the C standard library
DECLARE LIBRARY
    ' isdigit returns non-zero if character is a digit
    FUNCTION isdigit& (BYVAL c AS LONG)
    ' toupper converts character to uppercase
    FUNCTION toupper& (BYVAL c AS LONG)
    ' abs returns absolute value
    FUNCTION abs& (BYVAL n AS LONG)
END DECLARE

PRINT "=== DECLARE LIBRARY Demo ==="
PRINT

' Test isdigit function
PRINT "Testing isdigit():"
PRINT "  isdigit('5') ="; isdigit&(ASC("5"))
PRINT "  isdigit('A') ="; isdigit&(ASC("A"))
PRINT

' Test toupper function
PRINT "Testing toupper():"
PRINT "  toupper('a') ="; CHR$(toupper&(ASC("a")))
PRINT "  toupper('z') ="; CHR$(toupper&(ASC("z")))
PRINT

' Test abs function
PRINT "Testing abs():"
PRINT "  abs(-42) ="; abs&(-42)
PRINT "  abs(100) ="; abs&(100)
PRINT

' Example 2: Using ALIAS for custom name mapping
DECLARE LIBRARY
    ' Map a friendlier BASIC name to C function
    FUNCTION GetAbsoluteValue& (BYVAL n AS LONG) ALIAS "abs"
END DECLARE

PRINT "Testing ALIAS (GetAbsoluteValue -> abs):"
PRINT "  GetAbsoluteValue(-123) ="; GetAbsoluteValue&(-123)
PRINT

' Example 3: DECLARE DYNAMIC LIBRARY (runtime loading)
' Note: This would load the library at runtime using dlopen/LoadLibrary
' DECLARE DYNAMIC LIBRARY "mylib"
'     FUNCTION custom_function& (BYVAL x AS LONG)
' END DECLARE

PRINT "=== Demo Complete ==="
