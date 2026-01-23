# QB64PE Language Specification

**Purpose:** Comprehensive reference document for the QB64 Phoenix Edition language
**Source:** QB64 Phoenix Edition source code and test suite
**Created:** 2026-01-17
**Scope:** Complete QB4.5 compatibility + QB64 extensions

**Note:** This document describes the QB64PE language specification. QB64Fresh is a ground-up rewrite that aims for compatibility with QB64PE, but may have implementation differences. This specification serves as the reference for what QB64Fresh should support.

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

' Legacy QB4.5: Absolute memory location (limited support in QB64)
DIM variable AS INTEGER ABSOLUTE segment:offset

' Dynamic arrays
REDIM dynamicArray(100) AS INTEGER
REDIM _PRESERVE arr(newSize)        ' Keep existing data (QB64)

' COMMON block (for CHAIN)
COMMON SHARED var1 AS INTEGER, var2 AS STRING
COMMON var1, var2, var3             ' Shared variables between chained programs
```

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

### 5.6 LSET / RSET

```basic
' Used with FIELD statements for random access files
LSET field$ = value$    ' Left-align string in field
RSET field$ = value$    ' Right-align string in field
```

### 5.7 PRINT USING (Formatted Output)

```basic
PRINT USING format$; expression1, expression2, ...
PRINT USING "###.##"; 123.456          ' Output: 123.46
PRINT USING "$$###.##"; 123.45         ' Output: $123.45
PRINT USING "**###.##"; 12.3           ' Output: **12.30
PRINT USING "##.##^^^^"; 1234.5        ' Output: 12.35E+02 (scientific)
PRINT USING "! "; "Hello"               ' Output: H (single character)
PRINT USING "\  \"; "Hello"            ' Output: Hello (string)
PRINT USING "&"; "Hello"                ' Output: Hello (variable-length string)
PRINT USING "##/##/####"; 1, 15, 2024  ' Output: 01/15/2024
PRINT USING "+##.##"; 12.3              ' Output: +12.30 (always show sign)
PRINT USING "-##.##"; 12.3              ' Output:  12.30 (space for +, - for negative)
PRINT USING "##.##"; -12.3              ' Output: -12.30 (minus sign if negative)
PRINT USING "^^^^"; 1234                ' Output: E+03 (exponent only)
PRINT USING "^^^^^"; 1234               ' Output: E+003 (exponent with leading zero)
PRINT USING "##.##^^^^^"; 1234.5       ' Output: 12.35E+003
PRINT USING "_,##.##"; 1234.5           ' Output: 1,234.50 (comma separator)
PRINT USING "**$##.##"; 12.3            ' Output: **$12.30 (fill with asterisks)
PRINT USING "$$$##.##"; 12.3             ' Output:  $12.30 (floating dollar sign)
PRINT USING "$$**##.##"; 12.3           ' Output: $**12.30 (dollar + asterisks)
```

**Format String Characters:**
- `#` - Digit placeholder
- `.` - Decimal point
- `,` - Thousands separator (when `_` precedes it)
- `+` - Always show sign (+ or -)
- `-` - Show sign only if negative, space if positive
- `$$` - Floating dollar sign
- `**` - Fill with asterisks
- `**$` - Asterisks + dollar sign
- `^^^^` - Scientific notation (E+nn)
- `^^^^^` - Scientific notation with leading zero (E+0nn)
- `!` - Single character
- `\  \` - Fixed-length string (spaces determine length)
- `&` - Variable-length string
- `_` - Literal character (escape for special chars)

### 5.8 CLEAR and CLS

```basic
CLEAR                                ' Clear all variables and arrays
CLEAR , , stackSize                  ' Set stack size (legacy QB4.5)
CLEAR , , , segment                  ' Set data segment (legacy QB4.5)

CLS                                  ' Clear screen
CLS 0                                ' Clear screen with color 0
CLS , color                          ' Clear screen with specified color
CLS , , imageHandle&                 ' Clear to image (QB64)
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

### 6.6 END / STOP

```basic
END                                 ' End program execution
STOP                                ' Stop program execution (can resume in IDE)
```

**Note:** `END` terminates the program immediately. `STOP` pauses execution and can be resumed in the IDE (QB64 feature).

### 6.7 GOTO / GOSUB

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

### 6.8 Event Trapping

**Error Handling:**
```basic
ON ERROR GOTO errorHandler
ON ERROR RESUME NEXT                 ' Ignore errors
ON ERROR GOTO 0                      ' Disable handler
```

**Keyboard Event Trapping (QB4.5):**
```basic
ON KEY(n) GOSUB label                ' Trap specific key press
KEY(n) ON/OFF/STOP                   ' Enable/disable key trapping
KEY n, keyString$                    ' Define function key (F1-F10)
KEY LIST                             ' List all function key definitions
KEY ON/OFF                           ' Enable/disable all function keys
```

**Timer Event Trapping (QB4.5):**
```basic
ON TIMER(seconds) GOSUB label        ' Trap timer event
TIMER ON/OFF/STOP                    ' Enable/disable timer trapping
```

**Play Event Trapping (QB4.5):**
```basic
ON PLAY(n) GOSUB label               ' Trap PLAY queue event
PLAY ON/OFF/STOP                     ' Enable/disable PLAY trapping
```

**Light Pen Event Trapping (QB4.5, Legacy Hardware):**
```basic
ON PEN GOSUB label                   ' Trap light pen event
PEN ON/OFF/STOP                      ' Enable/disable pen trapping
```

### 6.8 Error Handling Details

' Error properties
ERR                                  ' Error number
ERL                                  ' Line number of error
ERDEV                                ' Device error code (QB4.5)
ERDEV$                               ' Device error message (QB4.5)
_ERRORLINE                           ' QB64: Line number
_ERRORMESSAGE$                       ' QB64: Error message
_INCLERRORLINE                       ' QB64: Include file error line
_INCLERRORFILE$                      ' QB64: Include file with error

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
CALLS name(arg1, arg2)              ' CALL with SEG (legacy QB4.5)
' CALLS uses segment:offset addressing (requires DEF SEG)
```

**Calling Conventions (DECLARE):**
```basic
DECLARE SUB MySub CDECL (x AS INTEGER)  ' C calling convention
DECLARE FUNCTION MyFunc CDECL (x AS INTEGER) AS INTEGER
' CDECL: C calling convention (caller cleans stack)
' Default: BASIC calling convention (callee cleans stack)
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

**Legacy QB4.5 DEF FN (Single-line functions):**
```basic
DEF FNAdd (a, b) = a + b            ' Define single-line function
DEF FNGreet$ (name$) = "Hello, " + name$

result = FNAdd(5, 3)                ' Call with FN prefix
message$ = FNGreet$("World")
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

**DECLARE LIBRARY (C Interoperability):**

```basic
DECLARE LIBRARY "libraryname"
    FUNCTION CFunction& (x AS INTEGER)
    SUB CSubroutine (s AS STRING)
END DECLARE
```

Used to declare external C library functions. See `docs/adrs/ADR-0008-c-interoperability.md` for detailed documentation on C interoperability features including `$USELIBRARY` metacommand and `ALIAS` keyword.

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

' Legacy QB4.5: CUSTOMTYPE is an alias for TYPE
CUSTOMTYPE PersonType
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
FREE array                          ' Deallocate array (legacy QB4.5, same as ERASE)
```

### 9.3 Array Bounds

```basic
LBOUND(array)                       ' Lower bound (first dimension)
LBOUND(array, dimension)            ' Lower bound of specific dimension
UBOUND(array)                       ' Upper bound (first dimension)
UBOUND(array, dimension)            ' Upper bound of specific dimension
```

### 9.4 OPTION Statements

```basic
OPTION BASE 0                       ' Arrays start at 0 (default)
OPTION BASE 1                       ' Arrays start at 1

OPTION _EXPLICIT                    ' QB64: Require variable declaration
OPTION _EXPLICITARRAY                ' QB64: Require array declaration
```

### 9.5 Dual Namespace Model (Scalars vs Arrays)

BASIC uses a **dual namespace model** where scalar variables and arrays occupy separate namespaces. This means a scalar variable and an array can share the same base name but refer to different storage:

```basic
DIM x AS STRING                     ' Scalar variable "x"
DIM x(10) AS INTEGER                ' Array "x()" - DIFFERENT from scalar x!

x = "hello"                         ' Assigns to scalar x (STRING)
x(1) = 42                           ' Assigns to array element x(1) (INTEGER)

PRINT x                             ' Prints "hello" (scalar)
PRINT x(1)                          ' Prints 42 (array element)
? x(1)                              ' Shorthand for PRINT (QB4.5)
```

**Key Points:**

1. **Separate Storage:** `x` (scalar) and `x()` (array) are completely independent variables with potentially different types.

2. **Context-Based Resolution:**
   - `x` (no parentheses) always refers to the scalar
   - `x(i)` (with parentheses) always refers to the array

3. **Common Pattern in QB64pe:** This is frequently used when a procedure parameter has the same name as a local array:
   ```basic
   SUB ProcessData (data AS STRING)      ' Parameter: scalar STRING
       DIM data(100) AS INTEGER          ' Local array: different namespace!

       PRINT data                        ' Uses scalar parameter (STRING)
       data(1) = 5                       ' Uses local array (INTEGER)
   END SUB
   ```

4. **Scope Interaction:** When a procedure uses `SHARED` arrays from module level, local scalars with the same name don't interfere:
   ```basic
   REDIM SHARED T(100) AS INTEGER        ' Module-level shared array

   FUNCTION Example$
       T = 5                             ' Creates local scalar T (SINGLE by DEFLNG)
       T(1) = 10                         ' Uses shared array T()
       Example$ = STR$(T) + STR$(T(1))   ' "5" + "10"
   END FUNCTION
   ```

**Implementation Note:** QB64Fresh's semantic analyzer maintains separate symbol tables for scalars and arrays within each scope. When resolving `name`, it checks the scalar namespace; when resolving `name(...)`, it checks the array namespace first.

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

' With ACCESS and LOCK options (QB4.5)
OPEN filename$ FOR INPUT ACCESS READ AS #filenum
OPEN filename$ FOR OUTPUT ACCESS WRITE AS #filenum
OPEN filename$ FOR RANDOM ACCESS READ WRITE AS #filenum
OPEN filename$ FOR RANDOM LOCK READ AS #filenum
OPEN filename$ FOR RANDOM LOCK WRITE AS #filenum
OPEN filename$ FOR RANDOM LOCK READ WRITE AS #filenum
OPEN filename$ FOR RANDOM SHARED AS #filenum

' With FREEFILE
filenum = FREEFILE
OPEN filename$ FOR INPUT AS #filenum
```

**OPEN Statement Options:**
- `ACCESS READ` - Read-only access
- `ACCESS WRITE` - Write-only access
- `ACCESS READ WRITE` - Read and write access
- `LOCK READ` - Lock for reading (prevents others from writing)
- `LOCK WRITE` - Lock for writing (prevents others from reading/writing)
- `LOCK READ WRITE` - Exclusive lock
- `SHARED` - Allow shared access (no locking)

### 10.2 Sequential I/O

```basic
' Console/Screen Output (PRINT without file number)
PRINT expression                      ' Print to screen
? expression                          ' Shorthand for PRINT (QB4.5)
PRINT expr1; expr2                    ' No separator
? expr1; expr2                        ' Shorthand
PRINT expr1, expr2                    ' Tab separated
? expr1, expr2                        ' Shorthand
PRINT                                 ' Print blank line
?                                     ' Shorthand for blank line

' File Output
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

' File locking (QB4.5)
LOCK #1, recordNumber                ' Lock specific record
LOCK #1, start TO end                ' Lock range of records
LOCK #1                               ' Lock entire file
UNLOCK #1, recordNumber              ' Unlock specific record
UNLOCK #1, start TO end               ' Unlock range of records
UNLOCK #1                             ' Unlock entire file

' Legacy FIELD statement (QB4.5)
FIELD #1, 10 AS idField$, 30 AS nameField$
LSET idField$ = STR$(recordId)
LSET nameField$ = recordName$
PUT #1, recordNumber
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

**Legacy QB4.5 Binary File Operations:**
```basic
BSAVE "file.bin", offset, length    ' Save memory to binary file
BLOAD "file.bin", offset            ' Load binary file to memory
' offset is segment:offset or VARPTR(variable)
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
FILEATTR(filenum, attribute)        ' Get file attribute (QB4.5)
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
_READFILE$(filename$)               ' Read entire file as string
_WRITEFILE filename$, data$        ' Write string to file
_FULLPATH$(relativePath$)           ' Get absolute path
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
| `RANDOMIZE` | Initialize random number generator with system time |
| `RANDOMIZE seed` | Initialize random number generator with seed |

**QB64 Math Extensions:**
| Function | Description |
|----------|-------------|
| `_ACOS(n)` | Arc cosine |
| `_ACOSH(n)` | Hyperbolic arc cosine |
| `_ASIN(n)` | Arc sine |
| `_ASINH(n)` | Hyperbolic arc sine |
| `_ATAN2(y, x)` | Arc tangent of y/x |
| `_ATANH(n)` | Hyperbolic arc tangent |
| `_ARCCOT(n)` | Arc cotangent |
| `_ARCCSC(n)` | Arc cosecant |
| `_ARCSEC(n)` | Arc secant |
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
| `_CSCH(n)` | Hyperbolic cosecant |
| `_COT(n)` | Cotangent |
| `_COTH(n)` | Hyperbolic cotangent |
| `_SECH(n)` | Hyperbolic secant |
| `_MIN(a, b)` | Minimum of two values |
| `_MAX(a, b)` | Maximum of two values |
| `_CLAMP(val, min, max)` | Clamp value to range |
| `_PI` | Pi constant (3.14159...) |
| `_NEGATE(value)` | Negate value |
| `_MIDDLE(a, b, c)` | Middle value of three |

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
| `_LTRIM$(s$)` | Remove leading spaces |
| `_RTRIM$(s$)` | Remove trailing spaces |
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

**Binary File I/O Conversion Functions (QB4.5):**

| Function | Description |
|----------|-------------|
| `CVI(string$)` | Convert 2-byte string to INTEGER |
| `CVL(string$)` | Convert 4-byte string to LONG |
| `CVS(string$)` | Convert 4-byte string to SINGLE |
| `CVD(string$)` | Convert 8-byte string to DOUBLE |
| `CVSMBF(string$)` | Convert 4-byte MBF string to SINGLE |
| `CVDMBF(string$)` | Convert 8-byte MBF string to DOUBLE |
| `MKI$(integer)` | Convert INTEGER to 2-byte string |
| `MKL$(long)` | Convert LONG to 4-byte string |
| `MKS$(single)` | Convert SINGLE to 4-byte string |
| `MKD$(double)` | Convert DOUBLE to 8-byte string |
| `MKSMBF$(single)` | Convert SINGLE to 4-byte MBF string |
| `MKDMBF$(double)` | Convert DOUBLE to 8-byte MBF string |

**Note:** MBF (Microsoft Binary Format) functions are for compatibility with old QB4.5 binary files.

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

**Text Mode Screen Control:**
```basic
LOCATE row, column                   ' Move cursor position
LOCATE row, column, cursor           ' Move cursor, set visibility (0=off, 1=on)
LOCATE , , 0                         ' Hide cursor
LOCATE , , 1                         ' Show cursor
WIDTH columns                        ' Set screen width (40 or 80)
WIDTH columns, rows                  ' Set screen dimensions
WIDTH #filenum, columns              ' Set file output width
```

**QB64 Console Functions:**
```basic
_CONSOLE                            ' Enable console window
_CONSOLEINPUT                       ' Get console input
_CONSOLECURSOR                      ' Set console cursor visibility
_CONSOLETITLE title$                ' Set console window title
_CONSOLEFONT fontName$, fontSize    ' Set console font
```

**QB64 Clipboard Functions:**
```basic
_CLIPBOARD$                         ' Get clipboard text
_CLIPBOARD$ = text$                 ' Set clipboard text
_CLIPBOARDIMAGE                     ' Get clipboard image
_CLIPBOARDIMAGE = imageHandle&      ' Set clipboard image
```

**QB64 Dialog Functions:**
```basic
result = _MESSAGEBOX(title$, message$, type$, icon$) ' Show message box
' type$: "OK", "OKCANCEL", "YESNO", "YESNOCANCEL"
' icon$: "INFORMATION", "QUESTION", "WARNING", "ERROR"

result$ = _INPUTBOX$(prompt$, title$, default$) ' Show input dialog

color& = _COLORCHOOSERDIALOG(defaultColor&) ' Show color picker

filename$ = _OPENFILEDIALOG$(title$, filter$, defaultPath$) ' Open file dialog
filename$ = _SAVEFILEDIALOG$(title$, filter$, defaultPath$) ' Save file dialog
folder$ = _SELECTFOLDERDIALOG$(title$, defaultPath$) ' Select folder dialog
```

**Legacy QB4.5 Port I/O (Limited/No Support in QB64):**
```basic
value = INP(port)                    ' Read from I/O port
OUT port, value                      ' Write to I/O port
WAIT port, mask, xorMask             ' Wait for port bits
```

**Legacy QB4.5 Joystick Functions (Limited/No Support in QB64):**
```basic
STICK(n)                             ' Get joystick position
' n: 0=X axis stick 1, 1=Y axis stick 1, 2=X axis stick 2, 3=Y axis stick 2

STRIG(n)                             ' Get joystick trigger state
' n: 0=button 1 stick 1, 1=button 2 stick 1, 2=button 1 stick 2, 3=button 2 stick 2
' Returns: -1 if pressed, 0 if not pressed

STRIG ON/OFF/STOP                    ' Enable/disable joystick event trapping
ON STRIG(n) GOSUB label              ' Event handler for joystick button
UEVENT                                ' Get last user event (joystick/mouse)
```

**Legacy QB4.5 System Functions (Limited/No Support in QB64):**
```basic
' Serial port communication
OPEN "COM1:9600,N,8,1" AS #1         ' Open serial port
COM(n) ON/OFF/STOP                   ' Enable/disable COM event trapping
ON COM(n) GOSUB label                ' Event handler for serial port

' Interrupt handling
CALL INTERRUPT interruptNumber       ' Call DOS interrupt
CALL INTERRUPTX interruptNumber      ' Call DOS interrupt (extended)
ON SIGNAL(n) GOSUB label              ' Signal handler
SIGNAL ON/OFF/STOP                   ' Enable/disable signal trapping

' I/O control
IOCTL #filenum, command$             ' Send I/O control command
result$ = IOCTL$(#filenum)           ' Get I/O control response
```

**Legacy QB4.5 Statements:**
```basic
LPRINT expression                    ' Print to printer
LPRINT expr1; expr2                  ' Print to printer (no separator)
LPRINT expr1, expr2                  ' Print to printer (tab separated)

RUN                                  ' Restart current program
RUN "program.exe"                    ' Run another program

CHAIN "program.exe"                  ' Chain to another program (preserves COMMON variables)
CHAIN "program.exe", ALL             ' Chain with all variables preserved
CHAIN "program.exe", , DELETE "temp.bas" ' Chain and delete temp file
```

TRON                                 ' Enable program trace (debugging)
TROFF                                ' Disable program trace
LIST                                 ' List program (IDE only in QB64)

SLEEP seconds                        ' Pause execution (QB4.5, limited in QB64)
SLEEP                                ' Pause until keypress
```

**QB64 Input Extensions:**
| Function | Description |
|----------|-------------|
| `_KEYHIT` | Key code without waiting |
| `_KEYDOWN(code)` | Is key pressed? |
| `_KEYCLEAR` | Clear keyboard buffer |
| `_CINP` | Console input (raw) |

**QB64 File Drop Extensions:**
```basic
_ACCEPTFILEDROP                    ' Enable file drop on window
_TOTALDROPPEDFILES                  ' Number of files dropped
_DROPPEDFILE$(index)                ' Get dropped file name
_DROPPEDFILE(index)                 ' Get dropped file handle
_FINISHDROP                         ' Clear dropped files list
```

**QB64 Mouse Functions:**
```basic
_MOUSEX                             ' Mouse X position
_MOUSEY                             ' Mouse Y position
_MOUSEBUTTON(button)                 ' Is mouse button pressed? (1=left, 2=right, 3=middle)
_MOUSEWHEEL                         ' Mouse wheel movement
_MOUSEINPUT                         ' Has mouse state changed?
_MOUSEMOVE                          ' Has mouse moved?
_MOUSEMOVEMENTX                     ' Mouse X movement since last check
_MOUSEMOVEMENTY                     ' Mouse Y movement since last check
_MOUSESHOW                          ' Show mouse cursor
_MOUSEHIDE                          ' Hide mouse cursor
_MOUSEHIDDEN                         ' Is mouse hidden?
```

**QB64 Game Controller/Input Device Functions:**
```basic
_DEVICES                            ' Number of input devices
_DEVICE$(deviceNumber)              ' Get device name
_DEVICEINPUT                        ' Has any device input changed?
_BUTTON(deviceNumber, buttonNumber) ' Is button pressed?
_BUTTONCHANGE(deviceNumber, buttonNumber) ' Button state changed?
_LASTBUTTON(deviceNumber)           ' Last button pressed
_LASTAXIS(deviceNumber)             ' Last axis moved
_LASTWHEEL(deviceNumber)            ' Last wheel moved
_LASTHANDLER                        ' Last device that generated event
_NEWHANDLER                         ' Create new input handler
_HARDWARE                           ' Hardware device number
_HARDWARE1                          ' Hardware device number (alternative)
```

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
| `_FPS` | Get current frame rate |
| `timerHandle& = _FREETIMER` | Get free timer handle |
| `_FREETIMER timerHandle&` | Free timer handle |

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
| `FRE(0)` | Free string space |
| `FRE(-1)` | Free memory (QB64) |
| `FRE(-2)` | Largest free block (QB64) |
| `SETMEM(bytes)` | Set memory allocation (legacy QB4.5, limited in QB64) |
| `SHELL command$` | Execute system command |
| `_SHELLHIDE` | QB64: Execute hidden |
| `_ECHO` | QB64: Echo console output (on/off) |
| `_EMBEDDED$` | QB64: Get embedded file content |
| `_CONTROLCHR` | QB64: Enable/disable control character handling |
| `_NOTIFYPOPUP title$, message$` | QB64: Show system notification popup |
| `_NUMLOCK` | QB64: Get/set Num Lock state |
| `_CAPSLOCK` | QB64: Get/set Caps Lock state |
| `_SCROLLLOCK` | QB64: Get/set Scroll Lock state |

**QB64 Networking Functions:**
```basic
handle& = _OPENHOST(port)           ' Open network host
handle& = _OPENCLIENT(host$:port)   ' Open network client
handle& = _OPENCONNECTION(hostHandle&) ' Accept connection
_CONNECTED(handle&)                 ' Is connection active?
_CONNECTIONADDRESS(handle&)          ' Get connection address (numeric)
_CONNECTIONADDRESS$(handle&)        ' Get connection address (string)
_STATUSCODE(handle&)                 ' Get HTTP status code
GET #handle&, , data$               ' Receive data
PUT #handle&, , data$               ' Send data
```

---

## 12. Metacommands

Metacommands are compiler directives that start with `$`. They must appear at the start of a line (possibly after whitespace).

### 12.1 QB4.5 Metacommands

| Metacommand | Description |
|-------------|-------------|
| `$STATIC` | Arrays are static (fixed at compile time) |
| `$DYNAMIC` | Arrays are dynamic (can be resized) |
| `$INCLUDE: 'filename'` | Include source file |
| `$FORMAT:ON` | Enable IDE auto-formatting (IDE-only, not compiler directive) |
| `$FORMAT:OFF` | Disable IDE auto-formatting (IDE-only, not compiler directive) |

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

**QB64 Assertion and Logging:**
```basic
_ASSERT condition                    ' Assert condition (requires $ASSERTS)
_ASSERT condition, message$         ' Assert with message

_LOGTRACE message$                  ' Log trace message
_LOGINFO message$                   ' Log info message
_LOGWARN message$                   ' Log warning message
_LOGERROR message$                  ' Log error message
_LOGMINLEVEL level                  ' Set minimum log level
' level: 0=trace, 1=info, 2=warn, 3=error
```
| `$IF condition THEN` | Conditional compilation |
| `$ELSEIF condition THEN` | Conditional compilation |
| `$ELSE` | Conditional compilation |
| `$END IF` or `$ENDIF` or `$END` | End conditional compilation block |
| `$LET variable = value` | Set compile-time variable |
| `$EMBED:'filename'` | Embed file in executable |
| `$UNSTABLE:feature` | Enable unstable feature |
| `$DEBUG` | Enable debug mode |
| `$INCLUDEONCE` | Include file only once |
| `$NOPREFIX` | Allow keywords without underscore |
| `$USELIBRARY:'author/library'` | Include library from libraries system |
| `$MIDISOUNDFONT:'file'` | **Deprecated** - Use `_MIDISOUNDBANK` instead |

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

**Note:** `_TRUE` and `_FALSE` are not language keywords. Use `-1` for TRUE and `0` for FALSE, or define your own constants.

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
PSET STEP (dx, dy), color           ' Relative coordinates
PRESET (x, y)                       ' Set pixel to background
PRESET STEP (dx, dy)                ' Relative coordinates
POINT(x, y)                         ' Get pixel color

LINE (x1, y1)-(x2, y2), color       ' Draw line
LINE (x1, y1)-(x2, y2), color, B    ' Draw box
LINE (x1, y1)-(x2, y2), color, BF   ' Draw filled box
LINE -(x2, y2), color               ' Continue from last point
LINE STEP (dx, dy)-(x2, y2), color  ' Relative start point
LINE (x1, y1)-STEP (dx, dy), color  ' Relative end point

CIRCLE (x, y), radius, color        ' Draw circle
CIRCLE (x, y), r, c, start, end     ' Draw arc
CIRCLE (x, y), r, c, , , aspect     ' Draw ellipse
CIRCLE STEP (dx, dy), radius, color ' Relative center

PAINT (x, y), fillColor, borderColor ' Flood fill
PAINT STEP (dx, dy), fillColor, borderColor ' Relative coordinates

DRAW commands$                      ' Draw language
' Movement: U=up, D=down, L=left, R=right, E=up-right, F=down-right
'           G=down-left, H=up-left, M=move absolute, M+=move relative
' Rotation: A=angle (0-3 for 90° increments), TA=angle (degrees)
' Color: C=color number
' Scale: S=scale factor (1-255, default 4)
' Fill: P=color, boundary (fill with color to boundary)
' Substrings: X=substring$ (execute substring as DRAW command)
' Example: DRAW "U10 R10 D10 L10" ' Draw square
'          DRAW "TA45 U10"         ' Rotate 45°, move up 10

' Graphics GET/PUT (sprite operations)
GET (x1, y1)-(x2, y2), array()      ' Capture screen region to array
PUT (x, y), array(), action          ' Draw array to screen
' action: PSET (replace), PRESET (invert), AND, OR, XOR
' Example: PUT (100, 100), sprite(), XOR

' Legacy QB4.5 graphics functions
VIEW (x1, y1)-(x2, y2)              ' Define viewport (clipping rectangle)
VIEW                                ' Reset to full screen
WINDOW (x1, y1)-(x2, y2)            ' Define logical coordinate system
WINDOW                               ' Reset to physical coordinates
PMAP(x, function)                   ' Map between coordinate systems
' function: 0=logical to physical X, 1=logical to physical Y
'           2=physical to logical X, 3=physical to logical Y
PCOPY sourcePage, destPage          ' Copy graphics page
PALETTE index, color                ' Set palette entry (QB4.5)
PALETTE USING array()               ' Set multiple palette entries
PEN ON/OFF/STOP                     ' Enable/disable light pen
PEN(n)                              ' Get light pen coordinates
STRETCH                             ' Enable stretch mode for graphics (legacy QB4.5)
SMOOTH                              ' Enable smooth scaling (legacy QB4.5, QB64 uses _SMOOTH constant)
```

### 14.3 QB64 Graphics Extensions

```basic
' Image handling
img& = _LOADIMAGE(file$, mode)      ' Load image file
' mode: 0=auto, 1=2-color, 2=4-color, 7-13=indexed, 32=32-bit, 33=32-bit with alpha
_FREEIMAGE img&                     ' Free image memory
_COPYIMAGE sourceImg&               ' Copy image
_PUTIMAGE (x, y), sourceImg&        ' Draw image (default: PSET)
_PUTIMAGE (dx1,dy1)-(dx2,dy2), src&, dst&, (sx1,sy1)-(sx2,sy2)
_PUTIMAGE (x, y), sourceImg&, action ' Draw with action
' action: _PSET (replace), _PRESET (invert), _AND, _OR, _XOR, _BLEND, _ALPHA
' Graphics constants: _ALL, _NONE, _SHOW, _HIDE, _AUTO, _BLEND, _DONTBLEND
'                      _CLOCKWISE, _ANTICLOCKWISE, _SOFTWARE, _HARDWARE
'                      _SEAMLESS, _SMOOTH, _SMOOTHSHRUNK, _SMOOTHSTRETCHED
'                      _BEHIND, _BLINK, _AXIS, _ALLOWFULLSCREEN, _DONTWAIT
'                      _OFF, _ONLY, _ONTOP, _KEEPBACKGROUND, _ONLYBACKGROUND
'                      _DISPLAYORDER (for multi-window ordering)
'                      _STRETCH (QB64: enable stretch mode for graphics)
_PIXELSIZE                          ' Get pixel size (bytes per pixel)
_HEIGHT(img&)                       ' Get image height
_HEIGHT                             ' Get current screen/image height (no parameter = current)
_WIDTH(img&)                        ' Get image width
_WIDTH                              ' Get current screen/image width (no parameter = current)
_SCALEDHEIGHT(img&)                 ' Get scaled image height
_SCALEDWIDTH(img&)                  ' Get scaled image width
_DEPTHBUFFER                        ' Enable/disable depth buffer
_CLEARCOLOR color&                  ' Set clear color
_CLIP (x1, y1)-(x2, y2)             ' Set clipping region
_CLIP                                ' Reset clipping region
_FILLBACKGROUND                     ' Fill background on resize
_PRINTIMAGE img&                    ' Print image to printer
_PRINTMODE mode                     ' Set print mode
_SETALPHA img&, alpha               ' Set image alpha channel
_COPYPALETTE sourceImg&, destImg&   ' Copy palette between images
_MAPTRIANGLE (x1,y1)-(x2,y2)-(x3,y3), sourceImg&, (sx1,sy1)-(sx2,sy2)-(sx3,sy3) ' Map triangle
_MAPUNICODE codePoint               ' Map Unicode code point

' Display control
_DISPLAY                            ' Update screen
_AUTODISPLAY                        ' Auto-update screen
_DEST img&                          ' Set drawing destination
_SOURCE img&                        ' Set reading source
_DEST                               ' Get current destination
_SOURCE                             ' Get current source

' Font and text rendering
fontHandle& = _LOADFONT(fontName$, fontSize, style) ' Load font
' style: 0=normal, 1=bold, 2=italic, 4=underline, 8=strikethrough (can combine)
_FREEFONT fontHandle&               ' Free font memory
_FONT fontHandle&                   ' Set current font
_FONT                                ' Get current font handle
_FONTHEIGHT                          ' Get current font height
_FONTWIDTH                           ' Get current font width
_PRINTSTRING (x, y), text$          ' Print text at position
_PRINTWIDTH(text$)                   ' Get text width in pixels
_UPRINTSTRING (x, y), text$         ' Print Unicode text
_UPRINTWIDTH(text$)                  ' Get Unicode text width
_UFONTHEIGHT                         ' Get Unicode font height
_ULINESPACING                        ' Get Unicode line spacing
_UCHARPOS(text$, index)              ' Get Unicode character position
```

' Screen capture
img& = _SCREENIMAGE                 ' Capture screen
_SAVEIMAGE file$, img&              ' Save image to file

' Window control
_FULLSCREEN                         ' Toggle fullscreen
_FULLSCREEN _SQUAREPIXELS           ' Fullscreen with aspect
_SCREENMOVE x, y                    ' Move window
_SCREENX                            ' Window X position
_SCREENY                            ' Window Y position
_SCREENSHOW                         ' Show window
_SCREENHIDE                         ' Hide window
_TITLE text$                        ' Set window title
_TITLE$                             ' Get window title
_ICON img&                          ' Set window icon
_WINDOWHANDLE                       ' Get window handle (platform-specific)
_WINDOWHASFOCUS                     ' Is window focused?
_SCREENEXISTS                        ' Does screen exist?
_SCREENCLICK x, y                   ' Simulate screen click
_SCREENPRINT text$                  ' Print to screen (legacy compatibility)
_SCREENICON                          ' Get screen icon handle
_SCALEDWIDTH                         ' Get scaled screen width
_SCALEDHEIGHT                        ' Get scaled screen height
_DESKTOPWIDTH                       ' Get desktop width
_DESKTOPHEIGHT                       ' Get desktop height
_RESIZE                             ' Enable/disable window resizing
_RESIZEHEIGHT                       ' Get/set resize height
_RESIZEWIDTH                        ' Get/set resize width
_DISPLAYORDER                       ' Set display order for windows
_KEEPBACKGROUND                     ' Keep background on resize
_ONLYBACKGROUND                     ' Only background on resize
_ONTOP                              ' Keep window on top
_WHEEL                              ' Mouse wheel value
_TOGGLE                             ' Toggle state
_OFF                                ' Off state constant
_ONLY                               ' Only state constant
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

' Color space conversions
_HSB32(hue, saturation, brightness)  ' Create color from HSB
_HSBA32(hue, saturation, brightness, alpha) ' Create color from HSBA
_HUE32(color&)                      ' Get hue component
_SATURATION32(color&)               ' Get saturation component
_BRIGHTNESS32(color&)                ' Get brightness component

_PALETTECOLOR index, color&         ' Set palette entry
_DEFAULTCOLOR                       ' Get default color
_BACKGROUNDCOLOR                    ' Get background color
```

### 14.5 Sound

```basic
' Legacy sound
BEEP                                ' System beep
SOUND frequency, duration           ' PC speaker (duration in ticks)
' frequency: 37-32767 Hz, duration: in clock ticks (18.2 ticks/second)

PLAY commands$                      ' Music macro language
' Commands: A-G (notes), #/+ (sharp), - (flat), . (dot), >/< (octave)
'          Ln (length), On (octave), Tn (tempo), Pn (pause), Nn (note number)
'          MB (background), MF (foreground), MN (normal), ML (legato), MS (staccato)
' Example: PLAY "L4 CDEF GAB>C"

' QB64 sound
handle& = _SNDOPEN(file$)           ' Load sound file
handle& = _SNDNEW                   ' Create new sound buffer
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
_SNDLIMIT count                     ' Limit number of simultaneous sounds
_MIDISOUNDBANK file$                 ' Load MIDI sound bank (replaces deprecated $MIDISOUNDFONT)

' Raw audio
handle& = _SNDOPENRAW               ' Open raw audio output
_SNDRAW left!, right!               ' Output raw sample
_SNDRAWBATCH leftArray!(), rightArray!(), count ' Output batch of samples
_SNDRAWDONE                         ' Check if raw buffer done
_SNDRAWLEN                          ' Raw buffer length
_SNDRATE                            ' Sample rate (44100)
_WAVE                               ' Get/set wave output device
```

---

## 15. Memory Operations

### 15.1 Legacy Memory (Limited)

```basic
' These are limited in QB64 for safety
PEEK(address)                       ' Read byte from memory
POKE address, value                 ' Write byte to memory
VARPTR(variable)                    ' Get variable address
VARPTR$(array())                    ' Get string array address (QB64)
VARSEG(variable)                    ' Get variable segment
SADD(variable$)                     ' Get string address (QB64, variable-length strings only)
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
- OpenGL functions (see Section 16)
- Advanced metacommands

---

## 16. OpenGL Functions (QB64)

**Note:** QB64PE provides direct OpenGL 1.x function bindings. These functions map directly to OpenGL API calls. QB64Fresh may not implement these initially as it uses SDL2/winit for graphics, but they are documented here for completeness.

### 16.1 OpenGL Initialization and Control

```basic
' Enable OpenGL rendering
_GLRENDER                           ' Enable OpenGL rendering mode

' Begin/End drawing
_GLBEGIN(mode)                      ' Begin primitive drawing
' mode: GL_POINTS, GL_LINES, GL_LINE_STRIP, GL_LINE_LOOP, GL_TRIANGLES,
'       GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_QUADS, GL_QUAD_STRIP, GL_POLYGON
_GLEND                              ' End primitive drawing

' Clear buffers
_GLCLEAR(mask)                      ' Clear buffers (GL_COLOR_BUFFER_BIT, etc.)
_GLCLEARCOLOR(r, g, b, a)           ' Set clear color
_GLCLEARDEPTH(depth)                ' Set clear depth
_GLCLEARSTENCIL(s)                  ' Set clear stencil
_GLCLEARACCUM(r, g, b, a)           ' Set clear accumulation buffer
_GLCLEARINDEX(index)                ' Set clear index (color index mode)

' Alpha testing
_GLALPHAFUNC(func, ref)             ' Set alpha test function and reference value
```

### 16.2 OpenGL State Management

```basic
' Enable/Disable
_GLENABLE(cap)                      ' Enable capability
_GLDISABLE(cap)                     ' Disable capability
_GLISENABLED(cap)                   ' Check if enabled

' Matrix operations
_GLLOADIDENTITY                     ' Load identity matrix
_GLLOADMATRIXD(matrix())            ' Load double matrix
_GLLOADMATRIXF(matrix())             ' Load float matrix
_GLMULTMATRIXD(matrix())            ' Multiply by double matrix
_GLMULTMATRIXF(matrix())            ' Multiply by float matrix
_GLMATRIXMODE(mode)                 ' Set matrix mode
' mode: GL_MODELVIEW, GL_PROJECTION, GL_TEXTURE

' Matrix stack
_GLPUSHMATRIX                       ' Push matrix on stack
_GLPOPMATRIX                        ' Pop matrix from stack

' Attributes
_GLPUSHATTRIB(mask)                 ' Push attribute state
_GLPOPATTRIB                        ' Pop attribute state
_GLPUSHCLIENTATTRIB(mask)           ' Push client attribute state
_GLPOPCLIENTATTRIB                  ' Pop client attribute state
```

### 16.3 OpenGL Primitives and Vertices

```basic
' Vertex specification
_GLVERTEX2D(x, y)                   ' 2D double vertex
_GLVERTEX2F(x, y)                   ' 2D float vertex
_GLVERTEX2I(x, y)                   ' 2D integer vertex
_GLVERTEX2S(x, y)                   ' 2D short vertex
_GLVERTEX3D(x, y, z)                ' 3D double vertex
_GLVERTEX3F(x, y, z)                ' 3D float vertex
_GLVERTEX3I(x, y, z)                ' 3D integer vertex
_GLVERTEX3S(x, y, z)                ' 3D short vertex
_GLVERTEX4D(x, y, z, w)             ' 4D double vertex
_GLVERTEX4F(x, y, z, w)             ' 4D float vertex
_GLVERTEX4I(x, y, z, w)             ' 4D integer vertex
_GLVERTEX4S(x, y, z, w)             ' 4D short vertex

' Array variants (with V suffix) take array parameters
_GLVERTEX2DV(v())                   ' 2D double vertex array
_GLVERTEX2FV(v())                   ' 2D float vertex array
' ... (similar for all vertex types)

' Vertex arrays
_GLVERTEXPOINTER(size, type, stride, pointer) ' Define vertex array
_GLARRAYELEMENT(i)                  ' Specify array element for rendering
_GLENABLECLIENTSTATE(cap)           ' Enable client state
_GLDISABLECLIENTSTATE(cap)          ' Disable client state
_GLDRAWARRAYS(mode, first, count)   ' Draw from arrays
_GLDRAWELEMENTS(mode, count, type, indices) ' Draw indexed primitives
```

### 16.4 OpenGL Colors

```basic
' Color specification
_GLCOLOR3D(r, g, b)                 ' 3-component double color
_GLCOLOR3F(r, g, b)                 ' 3-component float color
_GLCOLOR3I(r, g, b)                 ' 3-component integer color
_GLCOLOR3S(r, g, b)                 ' 3-component short color
_GLCOLOR3UB(r, g, b)                ' 3-component unsigned byte color
_GLCOLOR3UI(r, g, b)                ' 3-component unsigned integer color
_GLCOLOR3US(r, g, b)                ' 3-component unsigned short color
_GLCOLOR4D(r, g, b, a)              ' 4-component double color
_GLCOLOR4F(r, g, b, a)              ' 4-component float color
_GLCOLOR4I(r, g, b, a)              ' 4-component integer color
_GLCOLOR4S(r, g, b, a)              ' 4-component short color
_GLCOLOR4UB(r, g, b, a)             ' 4-component unsigned byte color
_GLCOLOR4UI(r, g, b, a)             ' 4-component unsigned integer color
_GLCOLOR4US(r, g, b, a)             ' 4-component unsigned short color

' Array variants (with V suffix)
_GLCOLOR3DV(c())                    ' 3-component double color array
_GLCOLOR3FV(c())                    ' 3-component float color array
' ... (similar for all color types)

' Color arrays
_GLCOLORPOINTER(size, type, stride, pointer) ' Define color array
_GLCOLORMASK(red, green, blue, alpha) ' Color write mask
```

### 16.5 OpenGL Transformations

```basic
' Translation
_GLTRANSLATED(x, y, z)              ' Translate (double)
_GLTRANSLATEF(x, y, z)              ' Translate (float)

' Rotation
_GLROTATED(angle, x, y, z)          ' Rotate (double)
_GLROTATEF(angle, x, y, z)          ' Rotate (float)

' Scaling
_GLSCALED(x, y, z)                  ' Scale (double)
_GLSCALEF(x, y, z)                  ' Scale (float)

' Viewing
_GLORTHO(left, right, bottom, top, near, far) ' Orthographic projection
_GLFRUSTUM(left, right, bottom, top, near, far) ' Perspective projection
_GLUPERSPECTIVE(fovy, aspect, zNear, zFar) ' Perspective (alternative)
_GLVIEWPORT(x, y, width, height)    ' Set viewport
```

### 16.6 OpenGL Lighting

```basic
' Light sources
_GLLIGHTF(light, pname, param)      ' Set light parameter (float)
_GLLIGHTI(light, pname, param)      ' Set light parameter (integer)
_GLLIGHTFV(light, pname, params())   ' Set light parameter (float array)
_GLLIGHTIV(light, pname, params())   ' Set light parameter (integer array)

' Light model
_GLLIGHTMODELF(pname, param)        ' Set light model (float)
_GLLIGHTMODELI(pname, param)        ' Set light model (integer)
_GLLIGHTMODELFV(pname, params())    ' Set light model (float array)
_GLLIGHTMODELIV(pname, params())    ' Set light model (integer array)

' Materials
_GLMATERIALF(face, pname, param)     ' Set material (float)
_GLMATERIALI(face, pname, param)     ' Set material (integer)
_GLMATERIALFV(face, pname, params()) ' Set material (float array)
_GLMATERIALIV(face, pname, params()) ' Set material (integer array)

' Color material
_GLCOLORMATERIAL(face, mode)        ' Enable color tracking
```

### 16.7 OpenGL Textures

```basic
' Texture generation
_GLGENTEXTURES(n, textures())       ' Generate texture names
_GLDELETETEXTURES(n, textures())   ' Delete textures
_GLBINDTEXTURE(target, texture)     ' Bind texture
_GLISTEXTURE(texture)               ' Check if texture exists

' Texture images
_GLTEXIMAGE1D(target, level, internalFormat, width, border, format, type, pixels)
_GLTEXIMAGE2D(target, level, internalFormat, width, height, border, format, type, pixels)
_GLTEXSUBIMAGE1D(target, level, xoffset, width, format, type, pixels)
_GLTEXSUBIMAGE2D(target, level, xoffset, yoffset, width, height, format, type, pixels)

' Texture parameters
_GLTEXPARAMETERF(target, pname, param) ' Set texture parameter (float)
_GLTEXPARAMETERI(target, pname, param) ' Set texture parameter (integer)
_GLTEXPARAMETERFV(target, pname, params()) ' Set texture parameter (float array)
_GLTEXPARAMETERIV(target, pname, params()) ' Set texture parameter (integer array)

' Texture environment
_GLTEXENVF(target, pname, param)     ' Set texture environment (float)
_GLTEXENVI(target, pname, param)     ' Set texture environment (integer)
_GLTEXENVFV(target, pname, params()) ' Set texture environment (float array)
_GLTEXENVIV(target, pname, params()) ' Set texture environment (integer array)

' Texture coordinates
_GLTEXCOORD1D(s)                    ' 1D texture coordinate (double)
_GLTEXCOORD1F(s)                    ' 1D texture coordinate (float)
_GLTEXCOORD1I(s)                    ' 1D texture coordinate (integer)
_GLTEXCOORD1S(s)                    ' 1D texture coordinate (short)
_GLTEXCOORD2D(s, t)                 ' 2D texture coordinate (double)
_GLTEXCOORD2F(s, t)                 ' 2D texture coordinate (float)
_GLTEXCOORD2I(s, t)                 ' 2D texture coordinate (integer)
_GLTEXCOORD2S(s, t)                 ' 2D texture coordinate (short)
_GLTEXCOORD3D(s, t, r)              ' 3D texture coordinate (double)
_GLTEXCOORD3F(s, t, r)              ' 3D texture coordinate (float)
_GLTEXCOORD3I(s, t, r)              ' 3D texture coordinate (integer)
_GLTEXCOORD3S(s, t, r)              ' 3D texture coordinate (short)
_GLTEXCOORD4D(s, t, r, q)           ' 4D texture coordinate (double)
_GLTEXCOORD4F(s, t, r, q)           ' 4D texture coordinate (float)
_GLTEXCOORD4I(s, t, r, q)           ' 4D texture coordinate (integer)
_GLTEXCOORD4S(s, t, r, q)           ' 4D texture coordinate (short)

' Array variants (with V suffix)
_GLTEXCOORD1DV(tc())                ' 1D texture coordinate array
_GLTEXCOORD1FV(tc())                ' 1D texture coordinate array
' ... (similar for all texture coordinate types)

' Texture coordinate arrays
_GLTEXCOORDPOINTER(size, type, stride, pointer) ' Define texture coordinate array
```

### 16.8 OpenGL Display Lists

```basic
' Display list creation
list = _GLGENLISTS(range)            ' Generate display list names
_GLNEWLIST(list, mode)               ' Start new display list
' mode: GL_COMPILE, GL_COMPILE_AND_EXECUTE
_GLENDLIST                          ' End display list
_GLDELETELISTS(list, range)         ' Delete display lists
_GLISLIST(list)                     ' Check if display list exists

' Display list execution
_GLCALLLIST(list)                   ' Execute display list
_GLCALLLISTS(n, type, lists())      ' Execute multiple display lists
_GLLISTBASE(base)                   ' Set display list base
```

### 16.9 OpenGL Raster Operations

```basic
' Raster position
_GLRASTERPOS2D(x, y)                ' Set 2D raster position (double)
_GLRASTERPOS2F(x, y)                ' Set 2D raster position (float)
_GLRASTERPOS2I(x, y)                ' Set 2D raster position (integer)
_GLRASTERPOS2S(x, y)                ' Set 2D raster position (short)
_GLRASTERPOS3D(x, y, z)             ' Set 3D raster position (double)
_GLRASTERPOS3F(x, y, z)             ' Set 3D raster position (float)
_GLRASTERPOS3I(x, y, z)             ' Set 3D raster position (integer)
_GLRASTERPOS3S(x, y, z)             ' Set 3D raster position (short)
_GLRASTERPOS4D(x, y, z, w)          ' Set 4D raster position (double)
_GLRASTERPOS4F(x, y, z, w)          ' Set 4D raster position (float)
_GLRASTERPOS4I(x, y, z, w)          ' Set 4D raster position (integer)
_GLRASTERPOS4S(x, y, z, w)          ' Set 4D raster position (short)

' Array variants (with V suffix)
_GLRASTERPOS2DV(pos())              ' 2D raster position array
' ... (similar for all raster position types)

' Bitmaps and pixels
_GLBITMAP(width, height, xorig, yorig, xmove, ymove, bitmap())
_GLDRAWPIXELS(width, height, format, type, pixels)
_GLREADPIXELS(x, y, width, height, format, type, pixels)
_GLCOPYPIXELS(x, y, width, height, type)
```

### 16.10 OpenGL Normal Vectors

```basic
' Normal specification
_GLNORMAL3B(nx, ny, nz)             ' Normal (byte)
_GLNORMAL3D(nx, ny, nz)             ' Normal (double)
_GLNORMAL3F(nx, ny, nz)             ' Normal (float)
_GLNORMAL3I(nx, ny, nz)             ' Normal (integer)
_GLNORMAL3S(nx, ny, nz)             ' Normal (short)

' Array variants (with V suffix)
_GLNORMAL3BV(n())                   ' Normal array (byte)
_GLNORMAL3DV(n())                   ' Normal array (double)
_GLNORMAL3FV(n())                   ' Normal array (float)
_GLNORMAL3IV(n())                   ' Normal array (integer)
_GLNORMAL3SV(n())                   ' Normal array (short)

' Normal arrays
_GLNORMALPOINTER(type, stride, pointer) ' Define normal array
```

### 16.11 OpenGL Selection and Feedback

```basic
' Selection
_GLSELECTBUFFER(size, buffer())      ' Set selection buffer
_GLRENDERMODE(mode)                 ' Set render mode
' mode: GL_RENDER, GL_SELECT, GL_FEEDBACK
_GLINITNAMES                        ' Initialize name stack
_GLLOADNAME(name)                   ' Load name on stack
_GLPUSHNAME(name)                   ' Push name on stack
_GLPOPNAME                          ' Pop name from stack

' Feedback
_GLFEEDBACKBUFFER(size, type, buffer()) ' Set feedback buffer
_GLPASSTHROUGH(token)                ' Insert marker in feedback
```

### 16.12 OpenGL Additional Functions

```basic
' Flush and finish
_GLFLUSH                            ' Flush OpenGL commands
_GLFINISH                           ' Wait for OpenGL commands to complete

' Error checking
error = _GLGETERROR                 ' Get OpenGL error code

' State queries
_GLGETBOOLEANV(pname, params())     ' Get boolean state
_GLGETINTEGERV(pname, params())     ' Get integer state
_GLGETFLOATV(pname, params())       ' Get float state
_GLGETDOUBLEV(pname, params())      ' Get double state
_GLGETSTRING(name)                  ' Get string (version, vendor, etc.)
_GLGETPOINTERV(pname, params())     ' Get pointer state

' Clipping
_GLCLIPPLANE(plane, equation())     ' Define clipping plane
_GLGETCLIPPLANE(plane, equation()) ' Get clipping plane equation

' Hints
_GLHINT(target, mode)               ' Set hint

' Scissor test
_GLSCISSOR(x, y, width, height)     ' Define scissor box

' Stencil operations
_GLSTENCILFUNC(func, ref, mask)     ' Set stencil function
_GLSTENCILMASK(mask)                ' Set stencil mask
_GLSTENCILOP(fail, zfail, zpass)    ' Set stencil operations

' Depth operations
_GLDEPTHFUNC(func)                  ' Set depth function
_GLDEPTHMASK(flag)                  ' Set depth mask
_GLDEPTHRANGE(near, far)            ' Set depth range

' Polygon operations
_GLPOLYGONMODE(face, mode)          ' Set polygon rasterization mode
_GLPOLYGONOFFSET(factor, units)     ' Set polygon offset
_GLPOLYGONSTIPPLE(mask())           ' Set polygon stipple pattern
_GLGETPOLYGONSTIPPLE(mask())        ' Get polygon stipple pattern
_GLFRONTFACE(mode)                  ' Set front face winding
_GLCULLFACE(mode)                   ' Set cull face mode

' Line operations
_GLLINEWIDTH(width)                 ' Set line width
_GLLINESTIPPLE(factor, pattern)     ' Set line stipple

' Point operations
_GLPOINTSIZE(size)                  ' Set point size

' Pixel operations
_GLPIXELSTOREF(pname, param)        ' Set pixel storage (float)
_GLPIXELSTOREI(pname, param)        ' Set pixel storage (integer)
_GLPIXELTRANSFERF(pname, param)     ' Set pixel transfer (float)
_GLPIXELTRANSFERI(pname, param)     ' Set pixel transfer (integer)
_GLPIXELZOOM(xfactor, yfactor)      ' Set pixel zoom
_GLPIXELMAPFV(map, mapsize, values()) ' Set pixel map (float)
_GLPIXELMAPUIV(map, mapsize, values()) ' Set pixel map (unsigned integer)
_GLPIXELMAPUSV(map, mapsize, values()) ' Set pixel map (unsigned short)
_GLGETPIXELMAPFV(map, values())     ' Get pixel map (float)
_GLGETPIXELMAPUIV(map, values())     ' Get pixel map (unsigned integer)
_GLGETPIXELMAPUSV(map, values())     ' Get pixel map (unsigned short)

' Index operations
_GLINDEXD(c)                        ' Set color index (double)
_GLINDEXF(c)                        ' Set color index (float)
_GLINDEXI(c)                        ' Set color index (integer)
_GLINDEXS(c)                        ' Set color index (short)
_GLINDEXUB(c)                       ' Set color index (unsigned byte)
_GLINDEXMASK(mask)                  ' Set index mask
_GLINDEXPOINTER(type, stride, pointer) ' Define index array

' Array variants (with V suffix)
_GLINDEXDV(c())                     ' Color index array (double)
_GLINDEXFV(c())                     ' Color index array (float)
_GLINDEXIV(c())                     ' Color index array (integer)
_GLINDEXSV(c())                     ' Color index array (short)
_GLINDEXUBV(c())                    ' Color index array (unsigned byte)

' Edge flags
_GLEDGEFLAG(flag)                   ' Set edge flag
_GLEDGEFLAGV(flags())               ' Set edge flag array
_GLEDGEFLAGPOINTER(stride, pointer) ' Define edge flag array

' Shade model
_GLSHADEMODEL(mode)                 ' Set shade model
' mode: GL_FLAT, GL_SMOOTH

' Logic operations
_GLLOGICOP(opcode)                  ' Set logical operation

' Accumulation buffer
_GLACCUM(op, value)                  ' Accumulation buffer operation

' Texture generation
_GLTEXGEND(coord, pname, param)     ' Texture generation (double)
_GLTEXGENF(coord, pname, param)     ' Texture generation (float)
_GLTEXGENI(coord, pname, param)     ' Texture generation (integer)
_GLTEXGENDV(coord, pname, params()) ' Texture generation (double array)
_GLTEXGENFV(coord, pname, params()) ' Texture generation (float array)
_GLTEXGENIV(coord, pname, params()) ' Texture generation (integer array)
_GLGETTEXGENDV(coord, pname, params()) ' Get texture generation (double)
_GLGETTEXGENFV(coord, pname, params()) ' Get texture generation (float)
_GLGETTEXGENIV(coord, pname, params()) ' Get texture generation (integer)

' Evaluators
_GLEVALCOORD1D(u)                   ' Evaluate 1D coordinate (double)
_GLEVALCOORD1F(u)                   ' Evaluate 1D coordinate (float)
_GLEVALCOORD2D(u, v)                 ' Evaluate 2D coordinate (double)
_GLEVALCOORD2F(u, v)                 ' Evaluate 2D coordinate (float)
_GLMAP1D(target, u1, u2, stride, order, points()) ' Define 1D evaluator map
_GLMAP1F(target, u1, u2, stride, order, points()) ' Define 1D evaluator map
_GLMAP2D(target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points()) ' Define 2D evaluator map
_GLMAP2F(target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points()) ' Define 2D evaluator map
_GLMAPGRID1D(un, u1, u2)            ' Define 1D grid
_GLMAPGRID1F(un, u1, u2)            ' Define 1D grid
_GLMAPGRID2D(un, u1, u2, vn, v1, v2) ' Define 2D grid
_GLMAPGRID2F(un, u1, u2, vn, v1, v2) ' Define 2D grid
_GLEVALMESH1(mode, i1, i2)          ' Evaluate 1D mesh
_GLEVALMESH2(mode, i1, i2, j1, j2)  ' Evaluate 2D mesh
_GLEVALPOINT1(i)                    ' Evaluate 1D point
_GLEVALPOINT2(i, j)                  ' Evaluate 2D point

' Array variants (with V suffix)
_GLEVALCOORD1DV(u())                ' Evaluate 1D coordinate array
_GLEVALCOORD1FV(u())                ' Evaluate 1D coordinate array
_GLEVALCOORD2DV(uv())               ' Evaluate 2D coordinate array
_GLEVALCOORD2FV(uv())               ' Evaluate 2D coordinate array

' Interleaved arrays
_GLINTERLEAVEDARRAYS(format, stride, pointer) ' Define interleaved arrays

' Texture level parameters
_GLGETTEXLEVELPARAMETERFV(target, level, pname, params()) ' Get texture level (float)
_GLGETTEXLEVELPARAMETERIV(target, level, pname, params()) ' Get texture level (integer)

' Texture image queries
_GLGETTEXIMAGE(target, level, format, type, pixels) ' Get texture image

' Texture environment queries
_GLGETTEXENVFV(target, pname, params()) ' Get texture environment (float)
_GLGETTEXENVIV(target, pname, params()) ' Get texture environment (integer)

' Texture parameter queries
_GLGETTEXPARAMETERFV(target, pname, params()) ' Get texture parameter (float)
_GLGETTEXPARAMETERIV(target, pname, params()) ' Get texture parameter (integer)

' Material queries
_GLGETMATERIALFV(face, pname, params()) ' Get material (float)
_GLGETMATERIALIV(face, pname, params()) ' Get material (integer)

' Light queries
_GLGETLIGHTFV(light, pname, params()) ' Get light (float)
_GLGETLIGHTIV(light, pname, params()) ' Get light (integer)

' Light model queries
_GLGETLIGHTMODELFV(pname, params()) ' Get light model (float)
_GLGETLIGHTMODELIV(pname, params()) ' Get light model (integer)

' Map queries
_GLGETMAPDV(target, query, v())     ' Get evaluator map (double)
_GLGETMAPFV(target, query, v())      ' Get evaluator map (float)
_GLGETMAPIV(target, query, v())      ' Get evaluator map (integer)

' Rectangle drawing
_GLRECTD(x1, y1, x2, y2)             ' Draw rectangle (double)
_GLRECTF(x1, y1, x2, y2)             ' Draw rectangle (float)
_GLRECTI(x1, y1, x2, y2)             ' Draw rectangle (integer)
_GLRECTS(x1, y1, x2, y2)             ' Draw rectangle (short)

' Array variants (with V suffix)
_GLRECTDV(v1(), v2())                ' Draw rectangle array (double)
_GLRECTFV(v1(), v2())                ' Draw rectangle array (float)
_GLRECTIV(v1(), v2())                ' Draw rectangle array (integer)
_GLRECTSV(v1(), v2())                ' Draw rectangle array (short)

' Read buffer
_GLREADBUFFER(mode)                 ' Set read buffer

' Priority textures
_GLPRIORITIZETEXTURES(n, textures(), priorities()) ' Set texture priorities
_GLARETEXTURESRESIDENT(n, textures(), residences()) ' Check if textures resident
```

**Note:** This is a comprehensive list of OpenGL 1.x functions available in QB64PE. For detailed usage and parameters, refer to the OpenGL 1.x specification. QB64Fresh may not implement these functions initially, as it uses SDL2/winit for graphics rendering.

---

## References

- QB64 Phoenix Edition source code: `QB64pe/source/`
- QB64 test suite: `QB64pe/tests/`
- Syntax highlighter keywords: `QB64pe/source/subs_functions/syntax_highlighter_list.bas`
- [QB64 Phoenix Edition Wiki](https://qb64phoenix.com/qb64wiki/)

---

*This document describes the QB64 Phoenix Edition (QB64PE) language specification. It serves as a reference for QB64Fresh compiler implementation, which aims for compatibility with QB64PE. For QB64Fresh implementation-specific details, see `QB64_SYNTAX_REFERENCE.md` for parser quick reference and `ARCHITECTURE.md` for compiler design.*
