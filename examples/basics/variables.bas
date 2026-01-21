' ============================================================================
' Variables and Data Types
' ============================================================================
' Demonstrates: DIM, CONST, type suffixes, numeric literals
' ============================================================================

' Integer types
DIM i AS INTEGER          ' 16-bit signed (-32768 to 32767)
DIM l AS LONG             ' 32-bit signed
DIM i64 AS _INTEGER64     ' 64-bit signed (QB64 extension)

i = 100
l = 100000
i64 = 9000000000

PRINT "Integer types:"
PRINT "  INTEGER:    "; i
PRINT "  LONG:       "; l
PRINT "  _INTEGER64: "; i64
PRINT

' Floating point types
DIM s AS SINGLE           ' 32-bit float
DIM d AS DOUBLE           ' 64-bit float

s = 3.14159
d = 3.14159265358979

PRINT "Floating point types:"
PRINT "  SINGLE: "; s
PRINT "  DOUBLE: "; d
PRINT

' String type
DIM name AS STRING
name = "QB64Fresh"
PRINT "String: "; name
PRINT "Length: "; LEN(name)
PRINT

' Type suffixes (shorthand)
myInt% = 42               ' INTEGER
myLong& = 123456          ' LONG
mySingle! = 1.5           ' SINGLE
myDouble# = 2.5           ' DOUBLE
myString$ = "Hello"       ' STRING

PRINT "Using type suffixes:"
PRINT "  myInt% = "; myInt%
PRINT "  myLong& = "; myLong&
PRINT "  mySingle! = "; mySingle!
PRINT "  myDouble# = "; myDouble#
PRINT "  myString$ = "; myString$
PRINT

' Numeric literals
DIM hex_val AS LONG
DIM bin_val AS LONG

hex_val = &HFF            ' Hexadecimal (255)
bin_val = &B11111111      ' Binary (255) - QB64 extension

PRINT "Numeric literals:"
PRINT "  &HFF (hex) = "; hex_val
PRINT "  &B11111111 (binary) = "; bin_val
PRINT

' Constants
CONST PI = 3.14159265358979
CONST APP_NAME = "Variables Demo"
CONST MAX_VALUE = 1000

PRINT "Constants:"
PRINT "  PI = "; PI
PRINT "  APP_NAME = "; APP_NAME
PRINT "  MAX_VALUE = "; MAX_VALUE

END
