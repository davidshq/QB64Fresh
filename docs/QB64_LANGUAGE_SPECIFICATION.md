# QB64 Language Specification

**Purpose:** Comprehensive reference for implementing QB64Fresh compiler
**Source:** QB64 Phoenix Edition source code and test suite
**Created:** 2026-01-17
**Scope:** Complete QB4.5 compatibility + QB64 extensions

---

## Table of Contents

1. [Lexical Structure](#1-lexical-structure)
2. [Data Types](#2-data-types)
3. [Operators](#3-operators)
4. [Expressions](#4-expressions)
5. [Statements](#5-statements)
6. [Control Flow](#6-control-flow)
7. [Procedures](#7-procedures)
8. [User-Defined Types](#8-user-defined-types)
9. [Arrays](#9-arrays)
10. [File I/O](#10-file-io)
11. [Built-in Functions](#11-built-in-functions)
12. [Metacommands](#12-metacommands)
13. [QB64 Extensions](#13-qb64-extensions)
14. [Graphics and Sound](#14-graphics-and-sound)
15. [Memory Operations](#15-memory-operations)
16. [Implementation Priority](#16-implementation-priority)

---

## 1. Lexical Structure

### 1.1 Case Insensitivity

QB64 is **case-insensitive** for all keywords, identifiers, and built-in functions:
```basic
PRINT "hello"    ' Same as:
print "hello"    ' Same as:
Print "Hello"
```

### 1.2 Line Structure

```basic
' Single statement per line (standard)
PRINT "Hello"

' Multiple statements on one line (colon separator)
a = 1 : b = 2 : PRINT a + b

' Line continuation (QB64 extension, underscore at end)
longExpression = value1 + value2 + _
                 value3 + value4

' Line numbers (legacy, optional)
10 PRINT "Hello"
20 GOTO 10
```

### 1.3 Comments

```basic
' Single quote comment (preferred)
REM Traditional BASIC comment
x = 5 ' Inline comment after code
```

### 1.4 Identifiers

- Start with a letter (A-Z, a-z)
- Contain letters, digits (0-9), and periods (.)
- Optionally end with a type suffix
- Maximum length: 40 characters (traditional) or unlimited (QB64)
- Cannot be reserved keywords

```basic
myVariable
My.Namespaced.Variable
count%          ' INTEGER suffix
name$           ' STRING suffix
```

### 1.5 Numeric Literals

| Format | Example | Description |
|--------|---------|-------------|
| Integer | `42`, `-17` | Decimal integer |
| Float | `3.14`, `.5`, `5.` | Decimal point |
| Scientific (E) | `1.5E10`, `1.5e-3` | Single precision exponent |
| Scientific (D) | `1.5D10`, `1.5D-3` | Double precision exponent |
| Scientific (F) | `1.5F10` | Float precision exponent (QB64) |
| Hexadecimal | `&H1A2B`, `&HFF` | Prefix &H |
| Octal | `&O177`, `&O77` | Prefix &O |
| Binary | `&B1010`, `&B11110000` | Prefix &B (QB64) |

**Type suffixes on literals:**
```basic
100%      ' INTEGER
100&      ' LONG
100&&     ' _INTEGER64 (QB64)
100!      ' SINGLE
100#      ' DOUBLE
100##     ' _FLOAT (QB64)
100%%     ' _BYTE (QB64)
100~%%    ' _UNSIGNED _BYTE (QB64)
100~%     ' _UNSIGNED INTEGER (QB64)
100~&     ' _UNSIGNED LONG (QB64)
100~&&    ' _UNSIGNED _INTEGER64 (QB64)
100%&     ' _OFFSET (QB64)
100~%&    ' _UNSIGNED _OFFSET (QB64)
```

### 1.6 String Literals

```basic
"Hello, World!"
"She said ""Hello"""    ' Embedded quotes (doubled)
""                      ' Empty string
```

### 1.7 Reserved Keywords

**Control Flow:**
`IF`, `THEN`, `ELSE`, `ELSEIF`, `END IF`, `SELECT`, `CASE`, `END SELECT`, `GOTO`, `GOSUB`, `RETURN`, `ON`, `END`, `STOP`, `SYSTEM`

**Loops:**
`FOR`, `TO`, `STEP`, `NEXT`, `WHILE`, `WEND`, `DO`, `LOOP`, `UNTIL`, `EXIT`

**Declarations:**
`DIM`, `REDIM`, `AS`, `SHARED`, `STATIC`, `CONST`, `COMMON`, `TYPE`, `END TYPE`, `LET`, `OPTION`, `BASE`, `DECLARE`

**Types:**
`INTEGER`, `LONG`, `SINGLE`, `DOUBLE`, `STRING`

**Procedures:**
`SUB`, `END SUB`, `FUNCTION`, `END FUNCTION`, `CALL`, `BYVAL`

**Operators:**
`AND`, `OR`, `NOT`, `XOR`, `EQV`, `IMP`, `MOD`

**I/O:**
`PRINT`, `INPUT`, `LINE`, `WRITE`, `OPEN`, `CLOSE`, `GET`, `PUT`, `SEEK`

**Other:**
`DATA`, `READ`, `RESTORE`, `REM`, `DEF`, `FN`, `USING`, `IS`

---

## 2. Data Types

### 2.1 Numeric Types

| Type | Suffix | Size | Range | Notes |
|------|--------|------|-------|-------|
| `_BIT` | `` ` `` | 1/8 byte | -1 to 0 | QB64 |
| `_UNSIGNED _BIT` | `` ~` `` | 1/8 byte | 0 to 1 | QB64 |
| `_BYTE` | `%%` | 1 byte | -128 to 127 | QB64 |
| `_UNSIGNED _BYTE` | `~%%` | 1 byte | 0 to 255 | QB64 |
| `INTEGER` | `%` | 2 bytes | -32,768 to 32,767 | QB4.5 |
| `_UNSIGNED INTEGER` | `~%` | 2 bytes | 0 to 65,535 | QB64 |
| `LONG` | `&` | 4 bytes | -2,147,483,648 to 2,147,483,647 | QB4.5 |
| `_UNSIGNED LONG` | `~&` | 4 bytes | 0 to 4,294,967,295 | QB64 |
| `_INTEGER64` | `&&` | 8 bytes | ±9.22×10¹⁸ | QB64 |
| `_UNSIGNED _INTEGER64` | `~&&` | 8 bytes | 0 to 1.84×10¹⁹ | QB64 |
| `SINGLE` | `!` (default) | 4 bytes | ±3.40×10³⁸ | QB4.5 |
| `DOUBLE` | `#` | 8 bytes | ±1.79×10³⁰⁸ | QB4.5 |
| `_FLOAT` | `##` | 32 bytes | ±1.18×10⁴⁹³² | QB64 |

### 2.2 String Type

| Type | Suffix | Size |
|------|--------|------|
| `STRING` | `$` | Variable (0 to 2,147,483,647 chars) |
| `STRING * n` | `$` | Fixed length (n characters) |

### 2.3 Special Types (QB64)

| Type | Suffix | Description |
|------|--------|-------------|
| `_OFFSET` | `%&` | Memory offset (pointer-sized) |
| `_UNSIGNED _OFFSET` | `~%&` | Unsigned memory offset |
| `_MEM` | (none) | Memory block structure |

### 2.4 Default Type Rules

1. If no suffix and no `DEFxxx` applies: **SINGLE** is default
2. `DEFxxx` statements set defaults by first letter:
   ```basic
   DEFINT A-Z      ' All default to INTEGER
   DEFLNG A-Z      ' All default to LONG
   DEFSNG A-Z      ' All default to SINGLE
   DEFDBL A-Z      ' All default to DOUBLE
   DEFSTR A-Z      ' All default to STRING
   ```
3. `_DEFINE` (QB64) allows more control:
   ```basic
   _DEFINE A-Z AS LONG
   _DEFINE A-M AS INTEGER, N-Z AS STRING
   ```

### 2.5 Type Conversion

**Implicit conversions** (automatic):
- Numeric types widen automatically (INTEGER → LONG → SINGLE → DOUBLE)
- Narrowing may lose precision (DOUBLE → INTEGER truncates)

**Explicit conversions:**
```basic
CINT(x)     ' Convert to INTEGER
CLNG(x)     ' Convert to LONG
CSNG(x)     ' Convert to SINGLE
CDBL(x)     ' Convert to DOUBLE
_CAST(type, x)  ' QB64 explicit cast
```

---

## 3. Operators

### 3.1 Operator Precedence (Highest to Lowest)

| Level | Operators | Description |
|-------|-----------|-------------|
| 1 | `^` | Exponentiation |
| 2 | `-` (unary) | Negation |
| 3 | `*`, `/` | Multiplication, Division |
| 4 | `\` | Integer Division |
| 5 | `MOD` | Modulo |
| 6 | `+`, `-` | Addition, Subtraction |
| 7 | Comparison | `=`, `<>`, `<`, `>`, `<=`, `>=` |
| 8 | `NOT` | Logical NOT |
| 9 | `AND` | Logical AND |
| 10 | `OR` | Logical OR |
| 11 | `XOR` | Logical XOR |
| 12 | `EQV` | Equivalence |
| 13 | `IMP` | Implication |

**Note:** Parentheses override default precedence.

### 3.2 Arithmetic Operators

| Operator | Operation | Example | Result |
|----------|-----------|---------|--------|
| `+` | Addition | `5 + 3` | 8 |
| `-` | Subtraction | `5 - 3` | 2 |
| `*` | Multiplication | `5 * 3` | 15 |
| `/` | Division (float) | `5 / 3` | 1.666... |
| `\` | Integer Division | `5 \ 3` | 1 |
| `MOD` | Modulo | `5 MOD 3` | 2 |
| `^` | Exponentiation | `2 ^ 3` | 8 |
| `-` (unary) | Negation | `-5` | -5 |

### 3.3 Comparison Operators

| Operator | Meaning | Alternative |
|----------|---------|-------------|
| `=` | Equal | |
| `<>` | Not equal | `><` (legacy) |
| `<` | Less than | |
| `>` | Greater than | |
| `<=` | Less than or equal | `=<` (legacy) |
| `>=` | Greater than or equal | `=>` (legacy) |

**Note:** `><`, `=<`, and `=>` are legacy forms that should be supported for compatibility.

**String comparisons** are case-sensitive using ASCII values.

### 3.4 Logical/Bitwise Operators

| Operator | Operation | Notes |
|----------|-----------|-------|
| `NOT` | Bitwise NOT | `NOT 0` = -1 |
| `AND` | Bitwise AND | `5 AND 3` = 1 |
| `OR` | Bitwise OR | `5 OR 3` = 7 |
| `XOR` | Bitwise XOR | `5 XOR 3` = 6 |
| `EQV` | Equivalence | `5 EQV 3` = -7 |
| `IMP` | Implication | `5 IMP 3` = -5 |

**Boolean values:** TRUE = -1, FALSE = 0

**QB64 short-circuit operators:**
```basic
_ANDALSO    ' Short-circuit AND (stops if first is false)
_ORELSE     ' Short-circuit OR (stops if first is true)
```

### 3.5 String Operators

| Operator | Operation |
|----------|-----------|
| `+` | String concatenation |

---

## 4. Expressions

### 4.1 Expression Types

**Numeric expressions:**
```basic
5 + 3 * 2           ' = 11
(5 + 3) * 2         ' = 16
x ^ 2 + y ^ 2       ' Pythagorean
-a + b              ' Unary negation
```

**String expressions:**
```basic
"Hello" + " " + "World"     ' Concatenation
firstName$ + " " + lastName$
```

**Boolean expressions:**
```basic
x > 5 AND y < 10
NOT (a = b)
flag OR (count > 0)
```

### 4.2 Function Calls

```basic
SIN(angle)              ' Single argument
MID$(text$, 5, 3)       ' Multiple arguments
LEFT$(name$, 1)         ' String function
myFunction(a, b, c)     ' User function
```

### 4.3 Array Access

```basic
array(5)                ' Single dimension
matrix(row, col)        ' Multiple dimensions
data(i, j, k)           ' Three dimensions
udt.field(index)        ' UDT array field
```

### 4.4 Type Suffixes in Expressions

```basic
a% + b%                 ' INTEGER arithmetic
x! * y#                 ' Mixed SINGLE and DOUBLE
name$ + suffix$         ' STRING concatenation
```

---

## 5. Statements

### 5.1 Assignment

```basic
variable = expression
LET variable = expression       ' LET is optional
a% = 5
name$ = "John"
array(i) = value
udt.field = value
```

### 5.2 Variable Declaration

```basic
' Simple declaration
DIM variable AS type
DIM count AS INTEGER
DIM name AS STRING

' With type suffix (type inferred)
DIM count%
DIM name$

' Fixed-length string
DIM buffer AS STRING * 80

' Multiple declarations
DIM a AS INTEGER, b AS STRING, c AS DOUBLE

' QB64 alternative syntax
DIM AS INTEGER a, b, c

' Scope modifiers
DIM SHARED globalVar AS INTEGER     ' Module-level scope
STATIC localPersistent AS INTEGER   ' Preserves value between calls

' Dynamic arrays
REDIM dynamicArray(100) AS INTEGER
REDIM _PRESERVE arr(newSize)        ' Keep existing data (QB64)
```

### 5.3 Constants

```basic
CONST PI = 3.14159
CONST NAME$ = "QB64Fresh"
CONST MAX_SIZE = 1000, MIN_SIZE = 10
CONST DEBUG_MODE = -1               ' TRUE

' QB64: Type-specified constants
CONST B%% = 101%%                   ' _BYTE
CONST UL~& = 4000000001~&           ' _UNSIGNED LONG
```

### 5.4 DATA/READ/RESTORE

```basic
DATA 1, 2, 3, 4, 5, 6
DATA "John", "Jane", "Bob"

READ a, b, c
READ name$

RESTORE              ' Reset to first DATA
RESTORE labelName    ' Reset to specific DATA block

labelName:
DATA 100, 200, 300
```

### 5.5 SWAP

```basic
SWAP a, b            ' Exchange values of a and b
SWAP array(i), array(j)
```

---

## 6. Control Flow

### 6.1 IF...THEN

**Single-line:**
```basic
IF condition THEN statement
IF condition THEN statement ELSE statement
IF condition GOTO label
```

**Multi-line:**
```basic
IF condition THEN
    statements
END IF

IF condition THEN
    statements
ELSE
    statements
END IF

IF condition THEN
    statements
ELSEIF condition THEN
    statements
ELSEIF condition THEN
    statements
ELSE
    statements
END IF
```

### 6.2 SELECT CASE

```basic
SELECT CASE testExpression
    CASE value
        statements
    CASE value1, value2, value3         ' List of values
        statements
    CASE value1 TO value2               ' Range
        statements
    CASE IS > value                     ' Comparison
        statements
    CASE IS < value, IS > otherValue    ' Combined
        statements
    CASE ELSE
        statements
END SELECT
```

**QB64 extension:**
```basic
SELECT EVERYCASE testExpression         ' Checks ALL cases, not just first match
    CASE 1 TO 5
        PRINT "1-5"
    CASE 3 TO 7
        PRINT "3-7"                     ' Also executes if testExpression = 4
END SELECT
```

### 6.3 FOR...NEXT

```basic
FOR counter = start TO end
    statements
NEXT counter

FOR i = 1 TO 10 STEP 2              ' Custom step
    statements
NEXT i

FOR i = 10 TO 1 STEP -1             ' Countdown
    statements
NEXT i

' Nested loops
FOR i = 1 TO 10
    FOR j = 1 TO 10
        statements
    NEXT j
NEXT i

' Multiple counters in NEXT
FOR i = 1 TO 10
    FOR j = 1 TO 10
        statements
    NEXT j, i                        ' Innermost first

EXIT FOR                             ' Break out of loop
_CONTINUE                            ' Skip to next iteration (QB64)
```

### 6.4 WHILE...WEND

```basic
WHILE condition
    statements
WEND

EXIT WHILE                           ' QB64: Break out of loop
```

### 6.5 DO...LOOP

```basic
' Pre-test (may not execute)
DO WHILE condition
    statements
LOOP

DO UNTIL condition
    statements
LOOP

' Post-test (executes at least once)
DO
    statements
LOOP WHILE condition

DO
    statements
LOOP UNTIL condition

' Infinite loop with EXIT
DO
    statements
    IF exitCondition THEN EXIT DO
LOOP

_CONTINUE                            ' QB64: Skip to next iteration
```

### 6.6 GOTO / GOSUB

```basic
GOTO label
...
label:
    statements

GOSUB label
...
label:
    statements
    RETURN

' Computed GOTO/GOSUB
ON expression GOTO label1, label2, label3
ON expression GOSUB label1, label2, label3
```

### 6.7 Error Handling

```basic
ON ERROR GOTO errorHandler
ON ERROR RESUME NEXT                 ' Ignore errors
ON ERROR GOTO 0                      ' Disable handler

' Error properties
ERR                                  ' Error number
ERL                                  ' Line number of error
_ERRORLINE                           ' QB64: Line number
_ERRORMESSAGE$                       ' QB64: Error message

' Resume from error
RESUME                               ' Retry statement
RESUME NEXT                          ' Skip to next statement
RESUME label                         ' Jump to label

' Trigger error
ERROR errorNumber

errorHandler:
    PRINT "Error"; ERR; "at line"; ERL
    RESUME NEXT
```

---

## 7. Procedures

### 7.1 SUB (Subroutine)

```basic
SUB name
    statements
END SUB

SUB name (param1 AS type, param2 AS type)
    statements
END SUB

SUB Calculate (x AS INTEGER, y AS DOUBLE, result AS DOUBLE)
    result = x * y
END SUB

' With STATIC (preserve local values)
SUB Counter STATIC
    DIM count AS INTEGER
    count = count + 1
    PRINT count
END SUB
```

**Calling subroutines:**
```basic
name                                ' Direct call
name arg1, arg2                     ' With arguments
CALL name(arg1, arg2)               ' CALL keyword form
```

### 7.2 FUNCTION

```basic
FUNCTION name
    statements
    name = returnValue              ' Set return value
END FUNCTION

FUNCTION name (param1 AS type) AS returnType
    statements
    name = returnValue
END FUNCTION

' Return type via suffix
FUNCTION Add& (a AS INTEGER, b AS INTEGER)
    Add& = a + b
END FUNCTION

FUNCTION Greet$ (name AS STRING)
    Greet$ = "Hello, " + name
END FUNCTION
```

**Calling functions:**
```basic
result = FunctionName(args)
PRINT FunctionName(args)
```

### 7.3 Parameter Passing

**By reference (default):**
```basic
SUB Increment (x AS INTEGER)
    x = x + 1                       ' Modifies original
END SUB
```

**By value:**
```basic
SUB Display (BYVAL x AS INTEGER)
    x = x + 1                       ' Does NOT modify original
    PRINT x
END SUB
```

### 7.4 DECLARE (Optional in QB64)

```basic
DECLARE SUB MySub (x AS INTEGER, y AS STRING)
DECLARE FUNCTION MyFunc& (a AS INTEGER)
```

QB64 ignores DECLARE statements; parameter types are defined in the procedure itself.

### 7.5 Early Exit

```basic
EXIT SUB                            ' Return from SUB
EXIT FUNCTION                       ' Return from FUNCTION
```

### 7.6 Variable Scope

```basic
DIM SHARED globalVar AS INTEGER     ' Accessible everywhere

SUB Example
    DIM localVar AS INTEGER         ' Only in this SUB
    STATIC persistentVar AS INTEGER ' Preserved between calls
    SHARED globalVar                ' Access module-level variable
END SUB
```

---

## 8. User-Defined Types

### 8.1 TYPE Definition

```basic
TYPE PersonType
    firstName AS STRING * 20
    lastName AS STRING * 20
    age AS INTEGER
    salary AS DOUBLE
END TYPE

TYPE Point
    x AS SINGLE
    y AS SINGLE
END TYPE

TYPE Rectangle
    topLeft AS Point                ' Nested type
    bottomRight AS Point
    color AS LONG
END TYPE
```

### 8.2 Using Types

```basic
DIM person AS PersonType
person.firstName = "John"
person.lastName = "Doe"
person.age = 30

DIM employees(100) AS PersonType    ' Array of UDT

' Nested access
DIM rect AS Rectangle
rect.topLeft.x = 10
rect.topLeft.y = 20
```

### 8.3 QB64 Extended Types

```basic
TYPE MemoryBlock
    data AS _OFFSET                 ' Pointer
    size AS _UNSIGNED LONG
    flags AS _BYTE
END TYPE
```

---

## 9. Arrays

### 9.1 Declaration

```basic
' Single dimension
DIM array(10) AS INTEGER            ' 0 to 10 (11 elements)
DIM array(1 TO 10) AS INTEGER       ' 1 to 10 (10 elements)

' Multiple dimensions
DIM matrix(10, 10) AS DOUBLE        ' 11x11 matrix
DIM cube(5, 5, 5) AS SINGLE         ' 3D array

' Custom bounds
DIM arr(-5 TO 5) AS INTEGER         ' -5 to 5
DIM grid(1 TO 100, 1 TO 100) AS LONG
```

### 9.2 Dynamic Arrays

```basic
REDIM array(n) AS INTEGER           ' Size determined at runtime
REDIM _PRESERVE array(newSize)      ' QB64: Resize keeping data

ERASE array                         ' Clear/deallocate array
```

### 9.3 Array Bounds

```basic
LBOUND(array)                       ' Lower bound (first dimension)
LBOUND(array, dimension)            ' Lower bound of specific dimension
UBOUND(array)                       ' Upper bound (first dimension)
UBOUND(array, dimension)            ' Upper bound of specific dimension
```

### 9.4 OPTION BASE

```basic
OPTION BASE 0                       ' Arrays start at 0 (default)
OPTION BASE 1                       ' Arrays start at 1
```

---

## 10. File I/O

### 10.1 Opening Files

```basic
' Sequential access
OPEN filename$ FOR INPUT AS #filenum
OPEN filename$ FOR OUTPUT AS #filenum
OPEN filename$ FOR APPEND AS #filenum

' Random access
OPEN filename$ FOR RANDOM AS #filenum LEN = recordLength

' Binary access
OPEN filename$ FOR BINARY AS #filenum

' With FREEFILE
filenum = FREEFILE
OPEN filename$ FOR INPUT AS #filenum
```

### 10.2 Sequential I/O

```basic
' Writing
PRINT #filenum, expression
PRINT #filenum, expr1; expr2        ' No separator
PRINT #filenum, expr1, expr2        ' Tab separated
WRITE #filenum, expr1, expr2        ' Comma delimited with quotes

' Reading
INPUT #filenum, variable
INPUT #filenum, var1, var2
LINE INPUT #filenum, lineString$
```

### 10.3 Random Access I/O

```basic
TYPE Record
    id AS INTEGER
    name AS STRING * 30
END TYPE

DIM rec AS Record
OPEN "data.dat" FOR RANDOM AS #1 LEN = LEN(rec)

PUT #1, recordNumber, rec           ' Write record
GET #1, recordNumber, rec           ' Read record
```

### 10.4 Binary I/O

```basic
OPEN "file.bin" FOR BINARY AS #1

PUT #1, position, variable          ' Write at position
GET #1, position, variable          ' Read from position

' Position is 1-based
SEEK #1, position                   ' Set position
position = SEEK(1)                  ' Get position
```

### 10.5 File Functions

```basic
CLOSE #filenum                      ' Close specific file
CLOSE                               ' Close all files
RESET                               ' Close all files

EOF(filenum)                        ' End of file?
LOC(filenum)                        ' Current position
LOF(filenum)                        ' Length of file
FREEFILE                            ' Next available file number
```

### 10.6 File System Operations

```basic
NAME oldname$ AS newname$           ' Rename file
KILL filename$                      ' Delete file
KILL "*.tmp"                        ' Delete with wildcard

MKDIR dirname$                      ' Create directory
RMDIR dirname$                      ' Remove directory
CHDIR dirname$                      ' Change directory

FILES                               ' List files
FILES filespec$                     ' List matching files

' QB64 extensions
_FILEEXISTS(filename$)              ' Check if file exists
_DIREXISTS(dirname$)                ' Check if directory exists
_CWD$                               ' Current working directory
_STARTDIR$                          ' Starting directory
_DIR$("path")                       ' Directory listing
_FILES$("pattern")                  ' File listing (iterable)
```

---

## 11. Built-in Functions

### 11.1 Math Functions

| Function | Description |
|----------|-------------|
| `ABS(n)` | Absolute value |
| `SGN(n)` | Sign (-1, 0, or 1) |
| `INT(n)` | Floor (round toward negative infinity) |
| `FIX(n)` | Truncate (round toward zero) |
| `CINT(n)` | Round to INTEGER |
| `CLNG(n)` | Round to LONG |
| `SQR(n)` | Square root |
| `EXP(n)` | e^n |
| `LOG(n)` | Natural logarithm |
| `SIN(n)` | Sine (radians) |
| `COS(n)` | Cosine (radians) |
| `TAN(n)` | Tangent (radians) |
| `ATN(n)` | Arctangent (radians) |
| `RND` | Random number 0 to 1 |
| `RND(n)` | Random with seed control |

**QB64 Math Extensions:**
| Function | Description |
|----------|-------------|
| `_ACOS(n)` | Arc cosine |
| `_ASIN(n)` | Arc sine |
| `_ATAN2(y, x)` | Arc tangent of y/x |
| `_CEIL(n)` | Ceiling |
| `_ROUND(n)` | Round to nearest |
| `_HYPOT(x, y)` | Hypotenuse |
| `_D2R(degrees)` | Degrees to radians |
| `_R2D(radians)` | Radians to degrees |
| `_D2G(degrees)` | Degrees to gradians |
| `_G2D(gradians)` | Gradians to degrees |
| `_G2R(gradians)` | Gradians to radians |
| `_R2G(radians)` | Radians to gradians |
| `_SINH(n)` | Hyperbolic sine |
| `_COSH(n)` | Hyperbolic cosine |
| `_TANH(n)` | Hyperbolic tangent |
| `_SEC(n)` | Secant |
| `_CSC(n)` | Cosecant |
| `_COT(n)` | Cotangent |
| `_SECH(n)` | Hyperbolic secant |
| `_CSCH(n)` | Hyperbolic cosecant |
| `_COTH(n)` | Hyperbolic cotangent |
| `_MIN(a, b)` | Minimum of two values |
| `_MAX(a, b)` | Maximum of two values |
| `_CLAMP(val, min, max)` | Clamp value to range |
| `_PI` | Pi constant (3.14159...) |

### 11.2 String Functions

| Function | Description |
|----------|-------------|
| `LEN(s$)` | String length |
| `LEFT$(s$, n)` | Left n characters |
| `RIGHT$(s$, n)` | Right n characters |
| `MID$(s$, start, len)` | Substring |
| `MID$(s$, start)` | Substring to end |
| `INSTR(s$, find$)` | Find substring position |
| `INSTR(start, s$, find$)` | Find from position |
| `UCASE$(s$)` | Convert to uppercase |
| `LCASE$(s$)` | Convert to lowercase |
| `LTRIM$(s$)` | Remove leading spaces |
| `RTRIM$(s$)` | Remove trailing spaces |
| `SPACE$(n)` | String of n spaces |
| `STRING$(n, char$)` | Repeat character n times |
| `STRING$(n, code)` | Repeat ASCII code n times |
| `CHR$(code)` | ASCII code to character |
| `ASC(s$)` | Character to ASCII code |
| `ASC(s$, pos)` | QB64: ASCII at position |
| `VAL(s$)` | String to number |
| `STR$(n)` | Number to string |
| `HEX$(n)` | Number to hex string |
| `OCT$(n)` | Number to octal string |

**QB64 String Extensions:**
| Function | Description |
|----------|-------------|
| `_TRIM$(s$)` | Remove leading and trailing spaces |
| `_INSTRREV(s$, find$)` | Find last occurrence |
| `_BIN$(n)` | Number to binary string |
| `_STRCMP(a$, b$)` | Case-sensitive compare |
| `_STRICMP(a$, b$)` | Case-insensitive compare |
| `_TOSTR$(n)` | Number to string (no leading space) |

### 11.3 Conversion Functions

| Function | Description |
|----------|-------------|
| `CINT(n)` | Convert to INTEGER |
| `CLNG(n)` | Convert to LONG |
| `CSNG(n)` | Convert to SINGLE |
| `CDBL(n)` | Convert to DOUBLE |
| `VAL(s$)` | String to number |
| `VAL(s$, type)` | QB64: String to specific type |
| `STR$(n)` | Number to string |
| `_CAST(type, expr)` | QB64: Explicit type cast |
| `_MK$(type, value)` | QB64: Create typed string |
| `_CV(type, string$)` | QB64: Convert string to type |

### 11.4 I/O Functions

| Function | Description |
|----------|-------------|
| `INKEY$` | Get key without waiting |
| `INPUT$(n)` | Get n characters |
| `INPUT$(n, #filenum)` | Get n chars from file |
| `CSRLIN` | Current cursor row |
| `POS(0)` | Current cursor column |
| `TAB(n)` | Move to column n |
| `SPC(n)` | Output n spaces |
| `LPOS(n)` | Printer position |

**QB64 Input Extensions:**
| Function | Description |
|----------|-------------|
| `_KEYHIT` | Key code without waiting |
| `_KEYDOWN(code)` | Is key pressed? |
| `_KEYCLEAR` | Clear keyboard buffer |
| `_CINP` | Console input (raw) |

### 11.5 Date and Time

| Function | Description |
|----------|-------------|
| `DATE$` | Current date (MM-DD-YYYY) |
| `TIME$` | Current time (HH:MM:SS) |
| `TIMER` | Seconds since midnight |

**QB64 Time Extensions:**
| Function | Description |
|----------|-------------|
| `_DELAY(seconds)` | Pause execution |
| `_LIMIT(fps)` | Limit frame rate |

### 11.6 System Functions

| Function | Description |
|----------|-------------|
| `COMMAND$` | Command line arguments |
| `COMMAND$(n)` | QB64: Get argument n |
| `_COMMANDCOUNT` | QB64: Number of arguments |
| `ENVIRON$(var$)` | Get environment variable |
| `ENVIRON$(n)` | Get nth environment variable |
| `_ENVIRONCOUNT` | QB64: Number of environment vars |
| `_OS$` | QB64: Operating system name |
| `FRE(-1)` | Free memory |
| `SHELL command$` | Execute system command |
| `_SHELLHIDE` | QB64: Execute hidden |

---

## 12. Metacommands

Metacommands are compiler directives that start with `$`. They must appear at the start of a line (possibly after whitespace).

### 12.1 QB4.5 Metacommands

| Metacommand | Description |
|-------------|-------------|
| `$STATIC` | Arrays are static (fixed at compile time) |
| `$DYNAMIC` | Arrays are dynamic (can be resized) |
| `$INCLUDE: 'filename'` | Include source file |

### 12.2 QB64 Metacommands

| Metacommand | Description |
|-------------|-------------|
| `$CONSOLE` | Enable console output |
| `$CONSOLE:ONLY` | Console only (no graphics window) |
| `$SCREENHIDE` | Hide graphics window at start |
| `$SCREENSHOW` | Show graphics window |
| `$RESIZE:ON` | Allow window resizing |
| `$RESIZE:OFF` | Disable window resizing |
| `$RESIZE:STRETCH` | Stretch content when resizing |
| `$RESIZE:SMOOTH` | Smooth scaling when resizing |
| `$COLOR:0` | Use CGA palette |
| `$COLOR:32` | Use 32-bit color |
| `$EXEICON:'file.ico'` | Set executable icon |
| `$VERSIONINFO:key=value` | Set version info |
| `$CHECKING:OFF` | Disable bounds checking |
| `$CHECKING:ON` | Enable bounds checking |
| `$ASSERTS` | Enable assertions |
| `$ASSERTS:CONSOLE` | Assertions with console |
| `$ERROR message` | Compiler error |
| `$IF condition THEN` | Conditional compilation |
| `$ELSEIF condition THEN` | Conditional compilation |
| `$ELSE` | Conditional compilation |
| `$END IF` | Conditional compilation |
| `$LET variable = value` | Set compile-time variable |
| `$EMBED:'filename'` | Embed file in executable |
| `$UNSTABLE:feature` | Enable unstable feature |
| `$DEBUG` | Enable debug mode |
| `$INCLUDEONCE` | Include file only once |
| `$NOPREFIX` | Allow keywords without underscore |

---

## 13. QB64 Extensions

### 13.1 Extended Keywords

| Keyword | Description |
|---------|-------------|
| `OPTION _EXPLICIT` | Require variable declaration |
| `OPTION _EXPLICITARRAY` | Require array declaration |
| `_CONTINUE` | Continue to next loop iteration |
| `SELECT EVERYCASE` | Check all cases in SELECT |
| `_IIF(cond, true, false)` | Inline IF expression |
| `_TRUE` | Boolean true (-1) |
| `_FALSE` | Boolean false (0) |

### 13.2 Bitwise Operations

| Function | Description |
|----------|-------------|
| `_SHL(value, bits)` | Shift left |
| `_SHR(value, bits)` | Shift right |
| `_ROL(value, bits)` | Rotate left |
| `_ROR(value, bits)` | Rotate right |
| `_READBIT(value, bit)` | Read bit |
| `_SETBIT(value, bit)` | Set bit |
| `_RESETBIT(value, bit)` | Clear bit |
| `_TOGGLEBIT(value, bit)` | Toggle bit |

### 13.3 Hash and Encoding

| Function | Description |
|----------|-------------|
| `_CRC32(data$)` | CRC32 checksum |
| `_MD5$(data$)` | MD5 hash |
| `_ADLER32(data$)` | Adler32 checksum |
| `_BASE64ENCODE$(data$)` | Base64 encode |
| `_BASE64DECODE$(data$)` | Base64 decode |
| `_DEFLATE$(data$)` | Compress data |
| `_INFLATE$(data$)` | Decompress data |
| `_ENCODEURL$(url$)` | URL encode |
| `_DECODEURL$(url$)` | URL decode |

---

## 14. Graphics and Sound

### 14.1 Screen Modes

```basic
SCREEN mode                         ' Set graphics mode
SCREEN 0                            ' Text mode (80x25)
SCREEN 1                            ' 320x200, 4 colors
SCREEN 2                            ' 640x200, 2 colors
SCREEN 7                            ' 320x200, 16 colors
SCREEN 8                            ' 640x200, 16 colors
SCREEN 9                            ' 640x350, 16 colors
SCREEN 12                           ' 640x480, 16 colors
SCREEN 13                           ' 320x200, 256 colors

' QB64: Custom screen
SCREEN _NEWIMAGE(width, height, colorDepth)
' colorDepth: 0=text, 1=2-color, 2=4-color, 7-13=indexed, 32=32-bit
```

### 14.2 Graphics Primitives

```basic
PSET (x, y), color                  ' Set pixel
PRESET (x, y)                       ' Set pixel to background
POINT(x, y)                         ' Get pixel color

LINE (x1, y1)-(x2, y2), color       ' Draw line
LINE (x1, y1)-(x2, y2), color, B    ' Draw box
LINE (x1, y1)-(x2, y2), color, BF   ' Draw filled box

CIRCLE (x, y), radius, color        ' Draw circle
CIRCLE (x, y), r, c, start, end     ' Draw arc
CIRCLE (x, y), r, c, , , aspect     ' Draw ellipse

PAINT (x, y), fillColor, borderColor ' Flood fill

DRAW commands$                      ' Draw language
' Commands: U=up, D=down, L=left, R=right, M=move, etc.
```

### 14.3 QB64 Graphics Extensions

```basic
' Image handling
img& = _LOADIMAGE(file$, mode)      ' Load image file
_FREEIMAGE img&                     ' Free image memory
_PUTIMAGE (x, y), sourceImg&        ' Draw image
_PUTIMAGE (dx1,dy1)-(dx2,dy2), src&, dst&, (sx1,sy1)-(sx2,sy2)

' Display control
_DISPLAY                            ' Update screen
_AUTODISPLAY                        ' Auto-update screen
_DEST img&                          ' Set drawing destination
_SOURCE img&                        ' Set reading source
_DEST                               ' Get current destination
_SOURCE                             ' Get current source

' Screen capture
img& = _SCREENIMAGE                 ' Capture screen
_SAVEIMAGE file$, img&              ' Save image to file

' Window control
_FULLSCREEN                         ' Toggle fullscreen
_FULLSCREEN _SQUAREPIXELS           ' Fullscreen with aspect
_SCREENMOVE x, y                    ' Move window
_SCREENX                            ' Window X position
_SCREENY                            ' Window Y position
_TITLE text$                        ' Set window title
_ICON img&                          ' Set window icon
```

### 14.4 Colors

```basic
COLOR foreground, background        ' Set colors

' QB64 color functions
_RGB(r, g, b)                       ' Create color (palette mode)
_RGB32(r, g, b)                     ' Create 32-bit color
_RGB32(r, g, b, a)                  ' With alpha
_RGBA(r, g, b, a)                   ' Create with alpha (palette)
_RGBA32(r, g, b, a)                 ' Create 32-bit with alpha

_RED(color&)                        ' Get red component
_GREEN(color&)                      ' Get green component
_BLUE(color&)                       ' Get blue component
_ALPHA(color&)                      ' Get alpha component
_RED32(color&)                      ' Get red (32-bit)
_GREEN32(color&)                    ' Get green (32-bit)
_BLUE32(color&)                     ' Get blue (32-bit)
_ALPHA32(color&)                    ' Get alpha (32-bit)

_PALETTECOLOR index, color&         ' Set palette entry
_DEFAULTCOLOR                       ' Get default color
_BACKGROUNDCOLOR                    ' Get background color
```

### 14.5 Sound

```basic
' Legacy sound
BEEP                                ' System beep
SOUND frequency, duration           ' PC speaker (duration in ticks)
PLAY commands$                      ' Music macro language

' QB64 sound
handle& = _SNDOPEN(file$)           ' Load sound file
_SNDPLAY handle&                    ' Play sound
_SNDLOOP handle&                    ' Loop sound
_SNDSTOP handle&                    ' Stop sound
_SNDPAUSE handle&                   ' Pause sound
_SNDCLOSE handle&                   ' Close sound
_SNDVOL handle&, volume!            ' Set volume (0-1)
_SNDBAL handle&, balance!           ' Set balance (-1 to 1)
_SNDPLAYING(handle&)                ' Is playing?
_SNDPAUSED(handle&)                 ' Is paused?
_SNDLEN(handle&)                    ' Sound length
_SNDGETPOS(handle&)                 ' Current position
_SNDSETPOS handle&, position!       ' Set position
_SNDCOPY(handle&)                   ' Copy sound
_SNDPLAYCOPY handle&                ' Play copy
_SNDPLAYFILE file$                  ' Quick play file

' Raw audio
handle& = _SNDOPENRAW               ' Open raw audio output
_SNDRAW left!, right!               ' Output raw sample
_SNDRAWDONE                         ' Check if raw buffer done
_SNDRAWLEN                          ' Raw buffer length
_SNDRATE                            ' Sample rate (44100)
```

---

## 15. Memory Operations

### 15.1 Legacy Memory (Limited)

```basic
' These are limited in QB64 for safety
PEEK(address)                       ' Read byte from memory
POKE address, value                 ' Write byte to memory
VARPTR(variable)                    ' Get variable address
VARSEG(variable)                    ' Get variable segment
DEF SEG = segment                   ' Set current segment
```

### 15.2 QB64 Memory System

```basic
' _MEM structure
TYPE _MEM
    OFFSET AS _OFFSET               ' Pointer to memory
    SIZE AS _OFFSET                 ' Size in bytes
    TYPE AS _INTEGER64              ' Variable type info
    ELEMENTSIZE AS _OFFSET          ' Size of each element
    IMAGE AS LONG                   ' Image handle (if applicable)
END TYPE

' Memory allocation
DIM m AS _MEM
m = _MEMNEW(byteSize)               ' Allocate memory
_MEMFREE m                          ' Free memory
_MEMEXISTS(m)                       ' Check if valid

' Memory access
_MEMGET m, offset, variable         ' Read from memory
_MEMPUT m, offset, value            ' Write to memory
_MEMFILL m, offset, size, value     ' Fill memory
_MEMCOPY src, srcOff, bytes, dst, dstOff ' Copy memory

' Memory info for variables
m = _MEM(variable)                  ' Get memory block for variable
m = _MEM(array())                   ' Get memory block for array
_MEMELEMENT(m, index)               ' Get element offset

' Memory for images
m = _MEMIMAGE(imageHandle&)         ' Get image memory

' Memory for sound
m = _MEMSOUND(soundHandle&)         ' Get sound memory
```

### 15.3 Offset and Pointer Operations

```basic
' _OFFSET type
DIM ptr AS _OFFSET
ptr = _OFFSET(variable)             ' Get address of variable

' Pointer arithmetic
newPtr%& = ptr%& + byteOffset

' Converting to/from integers
intVal&& = ptr%&                    ' _OFFSET to _INTEGER64
ptr%& = intVal&&                    ' _INTEGER64 to _OFFSET
```

---

## 16. Implementation Priority

### Phase 1: Core Language (QB4.5 Compatible)

**Must Have:**
- All data types: INTEGER, LONG, SINGLE, DOUBLE, STRING
- All operators with correct precedence
- Control flow: IF/THEN/ELSE, SELECT CASE, FOR/NEXT, WHILE/WEND, DO/LOOP
- Procedures: SUB, FUNCTION with parameters
- Arrays: DIM, REDIM, multi-dimensional
- Basic I/O: PRINT, INPUT, LINE INPUT
- String functions: LEFT$, RIGHT$, MID$, LEN, INSTR, etc.
- Math functions: ABS, SIN, COS, TAN, ATN, SQR, LOG, EXP, etc.
- Conversion functions: VAL, STR$, CHR$, ASC, etc.
- Comments: ' and REM

### Phase 2: File I/O and Types

**Should Have:**
- File operations: OPEN, CLOSE, PRINT#, INPUT#, GET, PUT
- User-defined types: TYPE...END TYPE
- DATA/READ/RESTORE
- CONST
- SHARED, STATIC
- Error handling: ON ERROR, ERR, ERL

### Phase 3: QB64 Core Extensions

**Nice to Have:**
- Extended types: _BYTE, _INTEGER64, _FLOAT, _UNSIGNED variants
- _OFFSET and memory operations
- Additional math functions
- String extensions: _TRIM$, _INSTRREV, etc.
- _IIF, _MIN, _MAX, _CLAMP
- $CONSOLE metacommands
- OPTION _EXPLICIT

### Phase 4: Graphics and Sound

**Future:**
- SCREEN modes
- Graphics primitives
- _NEWIMAGE, _LOADIMAGE, _PUTIMAGE
- Sound functions
- Window management

### Phase 5: Advanced Features

**Extended:**
- _MEM system
- Networking (_OPENCLIENT, _OPENHOST)
- _GL* OpenGL commands
- Advanced metacommands

---

## References

- QB64 Phoenix Edition source code: `QB64pe/source/`
- QB64 test suite: `QB64pe/tests/`
- Syntax highlighter keywords: `QB64pe/source/subs_functions/syntax_highlighter_list.bas`
- [QB64 Phoenix Edition Wiki](https://qb64phoenix.com/qb64wiki/)

---

*This document serves as the authoritative reference for QB64Fresh compiler implementation. For implementation-specific details, see the existing `QB64_SYNTAX_REFERENCE.md` for parser quick reference and `ARCHITECTURE.md` for compiler design.*
