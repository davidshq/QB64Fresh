# QB64 Syntax Reference

**Source:** [QB64 Phoenix Edition Wiki](https://qb64phoenix.com/qb64wiki/)
**Created:** 2026-01-16
**Purpose:** Quick reference for parser implementation

---

## Table of Contents

1. [Data Types](#data-types)
2. [Operators](#operators)
3. [Control Flow](#control-flow)
4. [Loops](#loops)
5. [Variables & Declarations](#variables--declarations)
6. [Input/Output](#inputoutput)
7. [Procedures](#procedures)
8. [Comments](#comments)
9. [Keywords by Category](#keywords-by-category)

---

## Data Types

### Numeric Types

| Type | Suffix | Size | Range |
|------|--------|------|-------|
| `_BIT` | `` ` `` | 1/8 byte | -1 to 0 |
| `_UNSIGNED _BIT` | `~`` ` `` | 1/8 byte | 0 to 1 |
| `_BYTE` | `%%` | 1 byte | -128 to 127 |
| `_UNSIGNED _BYTE` | `~%%` | 1 byte | 0 to 255 |
| `INTEGER` | `%` | 2 bytes | -32,768 to 32,767 |
| `_UNSIGNED INTEGER` | `~%` | 2 bytes | 0 to 65,535 |
| `LONG` | `&` | 4 bytes | -2,147,483,648 to 2,147,483,647 |
| `_UNSIGNED LONG` | `~&` | 4 bytes | 0 to 4,294,967,295 |
| `_INTEGER64` | `&&` | 8 bytes | ±9.22×10¹⁸ |
| `_UNSIGNED _INTEGER64` | `~&&` | 8 bytes | 0 to 1.84×10¹⁹ |
| `SINGLE` | `!` (or none) | 4 bytes | ±3.40×10³⁸ |
| `DOUBLE` | `#` | 8 bytes | ±1.79×10³⁰⁸ |
| `_FLOAT` | `##` | 32 bytes | ±1.18×10⁴⁹³² |

### String Type

| Type | Suffix | Size |
|------|--------|------|
| `STRING` | `$` | Variable (0 to 2,147,483,647 chars) |
| `STRING * n` | `$` | Fixed length (n characters) |

### Special Types (QB64)

| Type | Suffix | Description |
|------|--------|-------------|
| `_OFFSET` | `%&` | Memory offset (pointer size) |
| `_MEM` | (none) | Memory block structure |

### Default Type Rule

If no suffix is used and no `DEFxxx` or `_DEFINE` command applies, **the default type is SINGLE**.

---

## Operators

### Operator Precedence (Highest to Lowest)

| Level | Operators | Description |
|-------|-----------|-------------|
| 1 | `^` | Exponentiation |
| 2 | `-` (unary) | Negation |
| 3 | `*`, `/`, `\`, `MOD` | Multiplication, Division, Integer Division, Modulo |
| 4 | `+`, `-` | Addition, Subtraction |
| 5 | `=`, `<>`, `<`, `>`, `<=`, `>=` | Comparison |
| 6 | `NOT` | Logical NOT |
| 7 | `AND` | Logical AND |
| 8 | `OR`, `XOR` | Logical OR, XOR |
| 9 | `EQV`, `IMP` | Equivalence, Implication |

**Note:** Parentheses override default precedence.

### Arithmetic Operators

| Operator | Operation | Example |
|----------|-----------|---------|
| `+` | Addition | `5 + 3` → 8 |
| `-` | Subtraction | `5 - 3` → 2 |
| `*` | Multiplication | `5 * 3` → 15 |
| `/` | Division (float) | `5 / 3` → 1.666... |
| `\` | Integer Division | `5 \ 3` → 1 |
| `MOD` | Modulo (remainder) | `5 MOD 3` → 2 |
| `^` | Exponentiation | `2 ^ 3` → 8 |
| `-` (unary) | Negation | `-5` |

### Comparison Operators

| Operator | Meaning | Notes |
|----------|---------|-------|
| `=` | Equal | Also used for assignment |
| `<>` | Not equal | |
| `<` | Less than | |
| `>` | Greater than | |
| `<=` | Less than or equal | |
| `>=` | Greater than or equal | |

**String comparisons** are case-sensitive and use ASCII values.

### Logical/Boolean Operators

| Operator | Operation | Truth Table |
|----------|-----------|-------------|
| `NOT` | Negation | `NOT TRUE` = FALSE |
| `AND` | Conjunction | TRUE only if both TRUE |
| `OR` | Disjunction | TRUE if either TRUE |
| `XOR` | Exclusive OR | TRUE if exactly one TRUE |
| `EQV` | Equivalence | TRUE if both same |
| `IMP` | Implication | FALSE only if TRUE IMP FALSE |

**Boolean values:** TRUE = -1, FALSE = 0

**Note:** These are bitwise operators. They work on all bits of numeric values.

### String Operator

| Operator | Operation |
|----------|-----------|
| `+` | String concatenation |

---

## Control Flow

### IF...THEN (Single Line)

```basic
IF condition THEN statement [ELSE statement]
IF condition GOTO label
```

### IF...THEN...END IF (Multi-Line)

```basic
IF condition THEN
    statements
[ELSEIF condition THEN
    statements]
[ELSE
    statements]
END IF
```

**Rules:**
- Condition is true if non-zero
- `THEN` required for multi-line (optional with `GOTO`)
- `END IF` required for multi-line blocks
- Use colon `:` to chain multiple statements on one line

### SELECT CASE

```basic
SELECT CASE testExpression
    CASE value
        statements
    CASE value1 TO value2          ' Range
        statements
    CASE value1, value2, value3    ' List
        statements
    CASE IS > value                ' Comparison (=, <>, <, >, <=, >=)
        statements
    CASE ELSE
        statements
END SELECT
```

**Rules:**
- Executes first matching CASE, then exits
- `SELECT EVERYCASE` checks all cases (QB64 extension)
- Works with strings or numbers

### GOTO / GOSUB

```basic
GOTO label
GOSUB label
...
label:
    statements
    RETURN        ' For GOSUB only
```

---

## Loops

### FOR...NEXT

```basic
FOR counter = start TO end [STEP increment]
    statements
NEXT [counter]
```

**Rules:**
- Default STEP is +1
- If `start < end`, STEP must be positive (or default)
- If `start > end`, STEP must be negative
- Counter is incremented past `end` upon exit
- `EXIT FOR` to break early
- Multiple NEXT: `NEXT j, i` (innermost first)

### WHILE...WEND

```basic
WHILE condition
    statements
WEND
```

- Loop while condition is non-zero (true)
- `EXIT WHILE` to break early (QB64)

### DO...LOOP

```basic
' Test before loop:
DO WHILE condition
    statements
LOOP

DO UNTIL condition
    statements
LOOP

' Test after loop (executes at least once):
DO
    statements
LOOP WHILE condition

DO
    statements
LOOP UNTIL condition

' Infinite loop:
DO
    statements
    IF exitCondition THEN EXIT DO
LOOP
```

---

## Variables & Declarations

### DIM (Variable Declaration)

```basic
' Simple variables
DIM variable AS type
DIM variable AS STRING * length    ' Fixed-length string

' With type suffix
DIM variable%                      ' Integer
DIM variable$                      ' String

' Arrays
DIM array(upperBound) AS type
DIM array(lower TO upper) AS type

' Multiple declarations
DIM a AS INTEGER, b AS STRING, c AS DOUBLE

' QB64 alternative syntax
DIM AS INTEGER a, b, c
```

**Rules:**
- Array lower bound defaults to 0 (use `OPTION BASE 1` to change)
- `DIM SHARED` makes variable accessible to SUB/FUNCTION
- Use `REDIM` for dynamic arrays

### LET (Assignment)

```basic
LET variable = expression
variable = expression              ' LET is optional
```

### CONST (Constants)

```basic
CONST name = value
CONST PI = 3.14159, E = 2.71828
```

- Type inferred from value
- Cannot be changed after declaration

### DEFtype (Default Type by Letter)

```basic
DEFINT A-Z      ' All variables default to INTEGER
DEFLNG A-Z      ' All variables default to LONG
DEFSNG A-Z      ' All variables default to SINGLE
DEFDBL A-Z      ' All variables default to DOUBLE
DEFSTR A-Z      ' All variables default to STRING
```

### _DEFINE (QB64)

```basic
_DEFINE A-Z AS LONG
_DEFINE letter[-range] AS type
```

---

## Input/Output

### PRINT

```basic
PRINT expression
PRINT expr1; expr2               ' Concatenate (no space)
PRINT expr1, expr2               ' Tab to next 14-column zone
PRINT "text"; variable
PRINT                            ' Blank line
PRINT expr;                      ' Suppress newline
```

**Special Functions:**
- `TAB(column)` - Move to column
- `SPC(n)` - Print n spaces
- `SPACE$(n)` - Return string of n spaces

### PRINT USING

```basic
PRINT USING "format"; expression
PRINT USING "###.##"; 12.5       ' "12.50"
PRINT USING "Hello, &!"; "World" ' "Hello, World!"
```

### INPUT

```basic
INPUT variable
INPUT "prompt"; variable         ' Shows "prompt? "
INPUT "prompt", variable         ' Shows "prompt?"
INPUT ; variable                 ' Stay on same line
INPUT "prompt"; a, b, c          ' Multiple variables
```

**Rules:**
- Prompt must be a literal string (not a variable)
- Use `LINE INPUT` for strings that may contain commas

### LINE INPUT

```basic
LINE INPUT variable$
LINE INPUT "prompt"; variable$
```

---

## Procedures

### SUB (Subroutine)

```basic
SUB name [(parameters)]
    statements
END SUB

' With parameter types
SUB Calculate (x AS INTEGER, y AS DOUBLE)
    ...
END SUB
```

**Calling:**
```basic
name arg1, arg2              ' Direct call
CALL name(arg1, arg2)        ' With CALL keyword
```

**Rules:**
- Parameters matched by position and type
- `STATIC` preserves local variable values between calls
- `EXIT SUB` for early return
- `DIM SHARED` variables accessible without parameters

### FUNCTION

```basic
FUNCTION name [(parameters)]
    statements
    name = returnValue         ' Set return value
END FUNCTION

' With return type
FUNCTION Add& (a AS INTEGER, b AS INTEGER)
    Add& = a + b
END FUNCTION
```

**Rules:**
- Return type indicated by suffix on function name
- Return value assigned to function name
- Cannot use `CALL` with functions
- `EXIT FUNCTION` for early return

### DECLARE (Optional in QB64)

```basic
DECLARE SUB name (parameters)
DECLARE FUNCTION name (parameters)
```

QB64 ignores DECLARE statements; define types in the procedure itself.

---

## Comments

```basic
' This is a comment (apostrophe)
REM This is also a comment

x = 5 ' Inline comment after code
```

---

## Literals

### Numeric Literals

| Format | Example | Value |
|--------|---------|-------|
| Integer | `42` | 42 |
| Negative | `-42` | -42 |
| Float | `3.14` | 3.14 |
| Scientific | `1.5E10` | 1.5×10¹⁰ |
| Double scientific | `1.5D10` | 1.5×10¹⁰ (double) |
| Hexadecimal | `&HFF` | 255 |
| Octal | `&O77` | 63 |
| Binary | `&B1010` | 10 |

### String Literals

```basic
"Hello, World!"
"She said ""Hello"""    ' Embedded quotes
```

### Type Suffix on Literals

```basic
100%     ' INTEGER
100&     ' LONG
100&&    ' _INTEGER64
100!     ' SINGLE
100#     ' DOUBLE
100##    ' _FLOAT
```

---

## Keywords by Category

### Control Flow
`IF`, `THEN`, `ELSE`, `ELSEIF`, `END IF`, `SELECT CASE`, `CASE`, `CASE IS`, `CASE ELSE`, `END SELECT`, `GOTO`, `GOSUB`, `RETURN`, `END`, `STOP`, `SYSTEM`

### Loops
`FOR`, `TO`, `STEP`, `NEXT`, `WHILE`, `WEND`, `DO`, `LOOP`, `UNTIL`, `EXIT FOR`, `EXIT WHILE`, `EXIT DO`, `_CONTINUE`

### Declarations
`DIM`, `REDIM`, `AS`, `SHARED`, `STATIC`, `CONST`, `COMMON`, `TYPE`, `END TYPE`, `LET`, `OPTION BASE`

### Type Keywords
`INTEGER`, `LONG`, `SINGLE`, `DOUBLE`, `STRING`, `_BIT`, `_BYTE`, `_INTEGER64`, `_FLOAT`, `_UNSIGNED`, `_OFFSET`, `_MEM`

### Procedures
`SUB`, `END SUB`, `FUNCTION`, `END FUNCTION`, `CALL`, `DECLARE`, `BYVAL`, `EXIT SUB`, `EXIT FUNCTION`

### I/O
`PRINT`, `INPUT`, `LINE INPUT`, `WRITE`, `OPEN`, `CLOSE`, `GET`, `PUT`, `SEEK`, `LOF`, `EOF`, `FREEFILE`

### Operators
`AND`, `OR`, `NOT`, `XOR`, `EQV`, `IMP`, `MOD`

### Screen/Graphics
`SCREEN`, `CLS`, `COLOR`, `LOCATE`, `PRINT`, `PSET`, `LINE`, `CIRCLE`, `PAINT`, `DRAW`

---

## QB64 Extensions (Partial List)

These begin with underscore (`_`) and are QB64-specific:

- `_DEFINE`, `_UNSIGNED`, `_BIT`, `_BYTE`, `_INTEGER64`, `_FLOAT`, `_OFFSET`, `_MEM`
- `_KEYHIT`, `_KEYDOWN`, `_MOUSEX`, `_MOUSEY`, `_MOUSEBUTTON`
- `_SNDOPEN`, `_SNDPLAY`, `_SNDCLOSE`
- `_LOADIMAGE`, `_PUTIMAGE`, `_NEWIMAGE`, `_FREEIMAGE`
- `_DISPLAY`, `_AUTODISPLAY`, `_LIMIT`, `_DELAY`
- `_CLIPBOARD$`, `_SCREENIMAGE`
- `_RGB`, `_RGB32`, `_RGBA`, `_RGBA32`
- `_ANDALSO`, `_ORELSE` (short-circuit evaluation)
- `_CONTINUE` (continue loop iteration)

---

## Special Syntax

### Line Continuation (QB64)

```basic
longExpression = value1 + value2 + _
                 value3 + value4
```

### Multiple Statements Per Line

```basic
a = 1 : b = 2 : PRINT a + b
```

### Line Numbers (Legacy)

```basic
10 PRINT "Hello"
20 GOTO 10
```

---

## References

- [QB64 Phoenix Edition Wiki](https://qb64phoenix.com/qb64wiki/)
- [QB64 Keyword Reference - By Usage](https://qb64phoenix.com/qb64wiki/index.php/Keyword_Reference_-_By_usage)
- [QB64 Tutorial](https://www.qb64tutorial.com/)

---

*This document is a condensed reference for parser implementation. For complete details, consult the QB64 Wiki.*
