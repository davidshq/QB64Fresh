# QuickBasic 4.5 Language Specification

**Purpose:** Specification of Microsoft QuickBasic 4.5 (QB4.5) as the compatibility base for QB64Fresh and QB64PE.  
**Sources:** *Microsoft QuickBASIC: Language Reference* and *Microsoft QuickBASIC: Programming in BASIC* (Microsoft Corporation © 1987, 1988, for IBM Personal Computers and Compatibles). This repo uses plain-text exports of these manuals in `docs/qb_language_reference.txt` and `docs/qb_programming_in_basic.txt` for reference. QB64PE and QB64Fresh specs.  
**Created:** 2026-01-25  
**Scope:** QB4.5 only — no QB64 or other extensions.

This document describes the **QuickBasic 4.5** language as defined in those Microsoft manuals. It is intended to match the coverage of the Language Reference (Part 1–2) and the programming/statement material in *Programming in BASIC*. QB64PE and QB64Fresh extend this set; their specs document those extensions.

**Attribution and originality.** Technical content (syntax, behavior, limits) is derived from and checked against the Microsoft publications. This specification is an **original summary** written in our own words; it does **not** reproduce the manuals’ prose, tables, or worked examples. For exact wording, full tables, figures, and examples, use the original Microsoft manuals.

---

## Table of Contents

1. [Lexical Structure](#1-lexical-structure)
2. [Data Types](#2-data-types)
3. [Operators](#3-operators)
4. [Programs, Modules, and Expressions](#4-programs-modules-and-expressions)
5. [Statements](#5-statements)
6. [Control Flow](#6-control-flow)
7. [Procedures](#7-procedures)
8. [User-Defined Types](#8-user-defined-types)
9. [Arrays](#9-arrays)
10. [File I/O](#10-file-io)
11. [Built-in Functions](#11-built-in-functions)
12. [Metacommands](#12-metacommands)
13. [Graphics](#13-graphics)
14. [Error and Event Trapping](#14-error-and-event-trapping)
15. [DOS and System Integration](#15-dos-and-system-integration)
16. [Memory and Binary I/O](#16-memory-and-binary-io)

---

## 1. Lexical Structure

### 1.1 Case Insensitivity

QuickBasic 4.5 is **case-insensitive** for keywords, identifiers, and built-in names:

```basic
PRINT "hello"
print "hello"
```

### 1.2 Character Set

The BASIC character set includes letters (A–Z, a–z), digits (0–9), and special characters with fixed meanings: `!` `#` `$` `%` `&` `'` `(` `)` `*` `+` `,` `-` `.` `/` `:` `;` `<` `=` `>` `?` `@` `[` `]` `\` `^` `_`. The underscore `_` can denote line continuation when the source is prepared in an **external** editor; the QuickBASIC 4.5 built-in editor does **not** recognize `_` as line continuation.

### 1.3 Program Line and Line Length

- One or more statements per line, separated by colons.
- **Line length:** In the QB 4.5 editor, lines are limited to **256 characters**. When using an external editor, `_` as the last character on a line continues the logical line onto the next; after joining, the line-length limit is relaxed. Underscores cannot continue `DATA` or `REM` lines.
- **Line identifiers:** Optional. Two forms:
  - **Line number:** 0–65,529. Avoid 0: `ON ERROR GOTO 0` disables error trapping (does not branch to line 0); `RESUME 0` resumes at the line where the error occurred, not at line 0.
  - **Alphanumeric label:** 1–40 letters/digits, must start with a letter, end with `:`. Keywords not allowed. Only one identifier per line. Case-insensitive.
- Line numbers do **not** control execution order in QuickBASIC; order is top-to-bottom in the source. `ERL` returns only the last **line number** before an error (not a label). `RESUME` and `RESUME NEXT` do not require a line identifier.
- **IF...THEN** with a label: use `IF condition THEN GOTO label`; `IF condition THEN label` is not allowed.

```basic
10 PRINT "Hello"
20 a = 1 : b = 2 : PRINT a + b
MyLabel:
    GOSUB MyLabel
```

**Non-executable statements** (do not advance execution): `COMMON`, `CONST`, `DATA`, `DECLARE`, `DEFtype`, `DIM` (static arrays only), `OPTION BASE`, `SHARED`, `STATIC`, `TYPE`...`END TYPE`, and comments (`REM` or `'`).

### 1.4 Comments

```basic
' Single-quote comment (preferred)
REM Traditional BASIC comment
x = 5   ' Inline comment
```

### 1.5 Identifiers

- Begin with a letter (A–Z, a–z).
- May contain letters, digits (0–9), and periods (`.`).
- May end with a type suffix: `%` (INTEGER), `&` (LONG), `!` (SINGLE), `#` (DOUBLE), `$` (STRING).
- **Maximum length:** 40 characters.
- Cannot be reserved words.

```basic
count%
name$
My.Module.Variable
```

### 1.6 Numeric Literals

| Format       | Example      | Description        |
|-------------|--------------|--------------------|
| Integer     | `42`, `-17`  | Decimal            |
| Float       | `3.14`, `.5`, `5.` | Decimal point |
| Scientific (E) | `1.5E10`, `1.5e-3` | Single-precision exponent |
| Scientific (D) | `1.5D10`, `1.5D-3` | Double-precision exponent |
| Hexadecimal | `&H1A2B`, `&HFF` | Prefix `&H`  |
| Octal       | `&O177`, `&O77`  | Prefix `&O`   |

**Type suffixes on literals (QB4.5):**

```basic
100%   ' INTEGER
100&   ' LONG
100!   ' SINGLE
100#   ' DOUBLE
```

**Note:** `&B` (binary), `&&`, `##`, `%%`, and `_`-prefixed suffixes are **not** in QB4.5.

### 1.7 String Literals

```basic
"Hello, World!"
"She said ""Hello"""   ' Embedded quotes doubled
""                    ' Empty string
```

### 1.8 Reserved Keywords (QB4.5)

Representative set: `IF`, `THEN`, `ELSE`, `ELSEIF`, `END IF`, `SELECT`, `CASE`, `END SELECT`, `GOTO`, `GOSUB`, `RETURN`, `ON`, `END`, `STOP`, `SYSTEM`, `FOR`, `TO`, `STEP`, `NEXT`, `WHILE`, `WEND`, `DO`, `LOOP`, `UNTIL`, `EXIT`, `DIM`, `REDIM`, `AS`, `SHARED`, `STATIC`, `CONST`, `COMMON`, `TYPE`, `END TYPE`, `LET`, `OPTION`, `BASE`, `DECLARE`, `SUB`, `END SUB`, `FUNCTION`, `END FUNCTION`, `CALL`, `BYVAL`, `DEF`, `FN`, `AND`, `OR`, `NOT`, `XOR`, `EQV`, `IMP`, `MOD`, `PRINT`, `INPUT`, `LINE`, `WRITE`, `OPEN`, `CLOSE`, `GET`, `PUT`, `SEEK`, `DATA`, `READ`, `RESTORE`, `REM`, `USING`, `IS`, `SLEEP`, `KEY`, `PEN`, `PLAY`, `STRIG`, `TIMER`, `COM`, `SIGNAL`, `UEVENT`, `LOCK`, `UNLOCK`, `FIELD`, `LSET`, `RSET`, `BLOAD`, `BSAVE`, `CLEAR`, `RESET`, `RUN`, `CHAIN`, `NAME`, `KILL`, `MKDIR`, `RMDIR`, `CHDIR`, `SHELL`, `ENVIRON`, `INP`, `OUT`, `WAIT`, `DEF SEG`, `PEEK`, `POKE`, `VARPTR`, `VARSEG`, `CALL ABSOLUTE`, `INTERRUPT`, `INTERRUPTX`, `ERROR`, `RESUME`, `TRON`, `TROFF`, `LPRINT`, `FILES`, `FREE`, `SETMEM`, `IOCTL`, `LPOS`, `VIEW`, `WINDOW`, `PMAP`, `PCOPY`, `PALETTE`, `STICK`, `STRETCH`, `SMOOTH`, and others as in the QB4.5 manuals.

---

## 2. Data Types

### 2.1 Numeric Types (QB4.5)

| Type     | Suffix | Size   | Range |
|----------|--------|--------|-------|
| INTEGER  | `%`    | 2 bytes| -32,768 to 32,767 |
| LONG     | `&`    | 4 bytes| -2,147,483,648 to 2,147,483,647 |
| SINGLE   | `!`    | 4 bytes| approximately ±3.4×10³⁸ |
| DOUBLE   | `#`    | 8 bytes| approximately ±1.79×10³⁰⁸ |

**Default:** If no suffix and no `DEFINT`/`DEFLNG`/`DEFSNG`/`DEFDBL`/`DEFSTR` applies, the default is **SINGLE**.

### 2.2 String Type

| Type         | Suffix | Description                          |
|--------------|--------|--------------------------------------|
| STRING       | `$`    | Variable length (up to 32,767 chars)  |
| STRING * n   | `$`    | Fixed length of n characters        |

### 2.3 Default Type (DEFxxx)

```basic
DEFINT A-Z    ' Variables A–Z default to INTEGER
DEFLNG A-Z    ' LONG
DEFSNG A-Z    ' SINGLE
DEFDBL A-Z    ' DOUBLE
DEFSTR A-Z    ' STRING
DEFINT A-M    ' Ranges: A–M INTEGER,
DEFSTR N-Z    '          N–Z STRING
```

### 2.4 Type Conversion

**Explicit conversion functions:**

```basic
CINT(x)   ' to INTEGER
CLNG(x)   ' to LONG
CSNG(x)   ' to SINGLE
CDBL(x)   ' to DOUBLE
```

Implicit widening (e.g. INTEGER→LONG→SINGLE→DOUBLE) occurs in expressions; narrowing may truncate or overflow.

---

## 3. Operators

### 3.1 Precedence (highest to lowest)

| Level | Operators        | Description        |
|-------|------------------|--------------------|
| 1     | `^`              | Exponentiation     |
| 2     | `-` (unary)      | Negation           |
| 3     | `*`, `/`         | Multiplication, division |
| 4     | `\`              | Integer division   |
| 5     | `MOD`            | Modulo             |
| 6     | `+`, `-`         | Addition, subtraction |
| 7     | `=`, `<>`, `<`, `>`, `<=`, `>=` | Relational |
| 8     | `NOT`            | Logical NOT        |
| 9     | `AND`            | Logical AND        |
| 10    | `OR`             | Logical OR         |
| 11    | `XOR`            | Logical XOR        |
| 12    | `EQV`            | Equivalence        |
| 13    | `IMP`            | Implication        |

### 3.2 Arithmetic

| Operator | Operation        | Example   |
|----------|------------------|-----------|
| `+`      | Addition         | `5 + 3`   |
| `-`      | Subtraction      | `5 - 3`   |
| `*`      | Multiplication   | `5 * 3`   |
| `/`      | Float division   | `5 / 3`   |
| `\`      | Integer division | `5 \ 3`   |
| `MOD`    | Modulo           | `5 MOD 3` |
| `^`      | Exponentiation   | `2 ^ 3`   |
| `-` (unary) | Negation      | `-5`      |

**Arithmetic notes:** When `^` and unary `-` are adjacent, negation is done first (e.g. `4^-2` = .0625, not -16). Exceptions to “two consecutive operators need parentheses”: `*-`, `*+`, `^-`, `^+`. **Integer division** (`\`): operands are rounded to integer or long; quotient is truncated. **Modulo** (`MOD`): remainder of integer division. **Overflow** and **division by zero** cause run-time errors (trappable with `ON ERROR`). **Functional operators:** intrinsic and user-defined functions (e.g. `SQR`, `FUNCTION`/`DEF FN`) are used in expressions.

### 3.3 Relational

| Operator | Meaning | Legacy alternatives |
|----------|---------|----------------------|
| `=`      | Equal   | —                    |
| `<>`     | Not equal | `><`               |
| `<`      | Less than | —                  |
| `>`      | Greater than | —                |
| `<=`     | Less or equal | `=<`             |
| `>=`     | Greater or equal | `=>`          |

Comparisons use ASCII for strings.

### 3.4 Logical / Bitwise

| Operator | Operation   | Notes        |
|----------|-------------|--------------|
| `NOT`    | Bitwise NOT | `NOT 0` = -1 |
| `AND`    | Bitwise AND |              |
| `OR`     | Bitwise OR  |              |
| `XOR`    | Bitwise XOR |              |
| `EQV`    | Equivalence |              |
| `IMP`    | Implication |              |

**Boolean:** TRUE = -1, FALSE = 0.

### 3.5 String

| Operator | Operation           |
|----------|---------------------|
| `+`      | String concatenation |

---

## 4. Programs, Modules, and Expressions

### 4.1 Programs and Modules

A QuickBASIC program is made of one or more **modules** (source files). A module can contain **module-level code** (the main program, declarative statements, and error/event handling) and **procedures** (`SUB`, `FUNCTION`, `DEF FN`). The **main module** has the program entry point. **DEF FN** is module-level only: it must be defined before use, cannot be recursive, and cannot be called from another module. To simulate **passing by value** when calling a `SUB` or `FUNCTION`, pass an expression in parentheses, e.g. `Transform((A#))`.

### 4.2 Expressions

- **Numeric:** Arithmetic and relational expressions; function calls (intrinsic or user-defined) returning numbers.
- **String:** Concatenation and string functions.
- **Boolean:** Used in `IF`, `WHILE`, `DO`, etc.; TRUE = -1, FALSE = 0.

Array access uses `name(subscripts)`; the same syntax is used for function calls, distinguished by context (user-defined vs. built-in).

---

## 5. Statements

### 5.1 Assignment

```basic
variable = expression
LET variable = expression   ' LET optional
array(i) = value
udt.field = value
```

### 5.2 Variable Declaration

```basic
DIM variable AS type
DIM count AS INTEGER
DIM name AS STRING
DIM count%   ' type by suffix
DIM buffer AS STRING * 80
DIM a AS INTEGER, b AS STRING, c AS DOUBLE
DIM SHARED globalVar AS INTEGER
STATIC localVar AS INTEGER
REDIM dynamicArray(100) AS INTEGER
COMMON SHARED v1 AS INTEGER, v2 AS STRING
COMMON v1, v2, v3
```

**Note:** `REDIM _PRESERVE`, `DIM AS type a, b, c`, and `DIM variable AS type ABSOLUTE` are either QB64 or optional/advanced QB4.5; see your QB4.5 manual for `ABSOLUTE` and `COMMON` block ordering.

### 5.3 Constants

```basic
CONST PI = 3.14159
CONST NAME$ = "QB4.5"
CONST MAX = 1000, MIN = 10
```

### 5.4 DATA / READ / RESTORE

```basic
DATA 1, 2, 3, "John", "Jane"
READ a, b, c, x$, y$
RESTORE           ' First DATA
RESTORE labelName ' DATA at label
labelName:
DATA 100, 200
```

### 5.5 SWAP

```basic
SWAP a, b
SWAP array(i), array(j)
```

### 5.6 LSET / RSET

Used with `FIELD` in random-access files:

```basic
LSET field$ = value$   ' Left-justify in field
RSET field$ = value$   ' Right-justify in field
```

### 5.7 PRINT USING

```basic
PRINT USING format$; expr1, expr2, ...
```

Format symbols include: `#`, `.`, `,`, `+`, `-`, `$$`, `**`, `^^^^`, `!`, `\  \`, `&`, `_` (literal). See QB4.5 manuals for full list.

### 5.8 CLEAR and CLS

```basic
CLEAR                      ' Clear variables/arrays
CLEAR , , stackSize       ' Set stack size
CLEAR , , , segment       ' Set data segment (advanced)
CLS                        ' Clear screen
CLS 0                      ' Clear with color 0
CLS , color                ' Clear with color
```

### 5.9 PRINT shorthand

`?` is shorthand for `PRINT`:

```basic
? "Hello"
? x; y; z
```

---

## 6. Control Flow

### 6.1 IF...THEN...ELSE

**Single-line:**

```basic
IF condition THEN statement
IF condition THEN stmt1 ELSE stmt2
IF condition GOTO label      ' GOTO required when target is a label
IF condition THEN 500        ' Line number allowed without GOTO
```

**Block:**

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
ELSE
    statements
END IF
```

### 6.2 SELECT CASE

```basic
SELECT CASE expr
    CASE value
        statements
    CASE value1, value2, value3
        statements
    CASE value1 TO value2
        statements
    CASE IS > value
        statements
    CASE ELSE
        statements
END SELECT
```

### 6.3 FOR...NEXT

```basic
FOR i = start TO end
    statements
NEXT i

FOR i = 1 TO 10 STEP 2
    statements
NEXT i

FOR i = 10 TO 1 STEP -1
    statements
NEXT i
```

`EXIT FOR` exits the loop. Multiple variables in `NEXT` (e.g. `NEXT j, i`) are allowed for nested loops.

### 6.4 WHILE...WEND

```basic
WHILE condition
    statements
WEND
```

### 6.5 DO...LOOP

```basic
DO WHILE condition
    statements
LOOP

DO UNTIL condition
    statements
LOOP

DO
    statements
LOOP WHILE condition

DO
    statements
LOOP UNTIL condition

DO
    statements
    IF done THEN EXIT DO
LOOP
```

### 6.6 GOTO / GOSUB

```basic
GOTO label
GOSUB label
...
label:
    statements
    RETURN   ' for GOSUB
```

**Computed:**

```basic
ON expr GOTO label1, label2, label3
ON expr GOSUB label1, label2, label3
```

### 6.7 END / STOP

```basic
END      ' End program
STOP     ' Halt (resumable in QB environment)
```

---

## 7. Procedures

### 7.1 SUB

```basic
SUB name
    statements
END SUB

SUB name (p1 AS type, p2 AS type)
    statements
END SUB
```

**Call:**

```basic
name
name arg1, arg2
CALL name(arg1, arg2)
```

`CALLS` (with `SEG`) is a QB4.5 convention for segment:offset; see your QB4.5 docs.

### 7.2 FUNCTION

```basic
FUNCTION name AS type
    statements
    name = returnValue
END FUNCTION

FUNCTION name (p1 AS type) AS type
    statements
    name = returnValue
END FUNCTION
```

Return type can be given as `AS type` or by suffix on the name (e.g. `FUNCTION Add& (...)`).

### 7.3 DEF FN

**Single-line:**
```basic
DEF FNAdd (a, b) = a + b
DEF FNGreet$ (name$) = "Hello, " + name$
result = FNAdd(5, 3)
msg$ = FNGreet$("World")
```

**Multi-line (DEF FN...END DEF):**
```basic
DEF FNLog10 (X)
    FNLog10 = LOG(X) / LOG(10.0)
END DEF
result = FNLog10(100)
```

DEF FN arguments are **passed by value**. DEF FN cannot be recursive and must be defined before use; it is module-level only (not callable from other modules). The name always begins with `FN`.

### 7.4 Parameters

- **By reference** (default for SUB and FUNCTION): the address is passed; changes affect the caller.
- **By value:** `BYVAL` in the `SUB`/`FUNCTION` or `DECLARE` parameter list. For SUB/FUNCTION, passing an expression in parentheses, e.g. `SubName((x))`, simulates by-value.

### 7.5 DECLARE

```basic
DECLARE SUB MySub (x AS INTEGER, y AS STRING)
DECLARE FUNCTION MyFunc& (a AS INTEGER)
```

Used for forward or external declarations. `CDECL` is for C convention when calling external code.

### 7.6 Scope

- `DIM SHARED` at module level: visible in all procedures.
- `SHARED` in a procedure: use module-level variable of that name.
- `STATIC` in a procedure: local but persists between calls.

### 7.7 EXIT

```basic
EXIT SUB
EXIT FUNCTION
EXIT FOR
EXIT DO
```

`EXIT WHILE` is a QB64 extension; QB4.5 has `WHILE`/`WEND` without `EXIT WHILE`.

---

## 8. User-Defined Types

```basic
TYPE PersonType
    firstName AS STRING * 20
    lastName AS STRING * 20
    age AS INTEGER
    salary AS DOUBLE
END TYPE

DIM person AS PersonType
person.firstName = "John"
person.age = 30

DIM employees(100) AS PersonType
```

Nested types and `CUSTOMTYPE` (alias for `TYPE`) are QB4.5.

---

## 9. Arrays

### 9.1 Declaration

- **Maximum array dimensions:** 8 (QB4.5 limit). **Static** arrays: total size up to 65,535 bytes; **dynamic** arrays: limited by available memory. Procedure argument count: up to 60.
- Default lower bound: 0 if `OPTION BASE` is not used; `OPTION BASE 1` sets lower bound 1.

```basic
OPTION BASE 0   ' 0-based (default)
OPTION BASE 1   ' 1-based

DIM arr(10) AS INTEGER           ' 0 to 10 (or 1 to 10 with BASE 1)
DIM arr(1 TO 10) AS INTEGER      ' 1 to 10
DIM matrix(10, 10) AS DOUBLE
DIM cube(5, 5, 5) AS SINGLE
DIM arr(-5 TO 5) AS INTEGER
```

### 9.2 Dynamic Arrays

Use `$DYNAMIC` (or no `$STATIC`) and `REDIM`:

```basic
$DYNAMIC
REDIM arr(n) AS INTEGER
ERASE arr   ' Deallocate
```

`REDIM` without `_PRESERVE` does not preserve elements; `_PRESERVE` is QB64.

### 9.3 Bounds

```basic
LBOUND(array)
LBOUND(array, dimension)
UBOUND(array)
UBOUND(array, dimension)
```

### 9.4 Dual Namespace

Scalars and arrays are in different namespaces: `x` and `x()` can coexist with different types.

---

## 10. File I/O

### 10.1 OPEN

```basic
OPEN file$ FOR INPUT AS #n
OPEN file$ FOR OUTPUT AS #n
OPEN file$ FOR APPEND AS #n
OPEN file$ FOR RANDOM AS #n LEN = reclen
OPEN file$ FOR BINARY AS #n

OPEN file$ FOR INPUT ACCESS READ AS #n
OPEN file$ FOR OUTPUT ACCESS WRITE AS #n
OPEN file$ FOR RANDOM ACCESS READ WRITE AS #n
OPEN file$ FOR RANDOM LOCK READ AS #n
OPEN file$ FOR RANDOM LOCK WRITE AS #n
OPEN file$ FOR RANDOM LOCK READ WRITE AS #n
OPEN file$ FOR RANDOM SHARED AS #n

OPEN "COMn:baud,parity,data,stop" AS #n   ' Serial communications (OPEN COM)
n = FREEFILE
OPEN file$ FOR INPUT AS #n
```

### 10.2 Sequential and Screen Output

```basic
PRINT #n, expr
PRINT #n, expr1; expr2
PRINT #n, expr1, expr2
WRITE #n, expr1, expr2        ' File: comma-separated, strings quoted
WRITE expr1, expr2            ' Screen: same format as WRITE#
INPUT #n, var1, var2
LINE INPUT #n, line$
```

**VIEW PRINT [top TO bottom]:** Sets the text viewport (rows used by `PRINT`, `INPUT`, etc.). `VIEW PRINT` with no arguments resets to the full screen.

### 10.3 Random and Binary

```basic
GET #n, recordNumber, variable
PUT #n, recordNumber, variable
GET #n, position, variable   ' BINARY
PUT #n, position, variable   ' BINARY
SEEK #n, position
position = SEEK(n)
```

### 10.4 LOCK / UNLOCK

```basic
LOCK #n
LOCK #n, record
LOCK #n, start TO end
UNLOCK #n
UNLOCK #n, record
UNLOCK #n, start TO end
```

### 10.5 FIELD (Random-Access Files)

```basic
FIELD #n, width AS var$, width AS var$, ...
LSET var$ = value$
RSET var$ = value$
PUT #n, recordNumber
GET #n, recordNumber
```

### 10.6 File Functions and Statements

```basic
CLOSE #n
CLOSE
RESET
EOF(n)
LOC(n)
LOF(n)
FREEFILE
FILEATTR(n, attr)
```

**FILEATTR(n, 1)** returns file mode: 1=INPUT, 2=OUTPUT, 4=RANDOM, 8=APPEND, 32=BINARY. **FILEATTR(n, 2)** returns the DOS file handle.

### 10.7 File System

```basic
NAME old$ AS new$
KILL filespec$
MKDIR path$
RMDIR path$
CHDIR path$
FILES
FILES filespec$
```

---

## 11. Built-in Functions

### 11.1 Math

`ABS`, `SGN`, `INT`, `FIX`, `CINT`, `CLNG`, `CSNG`, `CDBL`, `SQR`, `EXP`, `LOG`, `SIN`, `COS`, `TAN`, `ATN`, `RND`, `RND(n)`, `RANDOMIZE`, `RANDOMIZE seed`.

### 11.2 String

`LEN`, `LEFT$`, `RIGHT$`, `MID$`, `MID$(s$,start,len)=...`, `INSTR`, `INSTR(start,s$,find$)`, `UCASE$`, `LCASE$`, `LTRIM$`, `RTRIM$`, `SPACE$`, `STRING$`, `CHR$`, `ASC`, `VAL`, `STR$`, `HEX$`, `OCT$`.

### 11.3 Binary-Data Conversion (GET/PUT, FIELD)

`MKI$`, `MKL$`, `MKS$`, `MKD$`, `CVI`, `CVL`, `CVS`, `CVD`, `MKSMBF$`, `MKDMBF$`, `CVSMBF`, `CVDMBF`.

### 11.4 I/O and Screen

`INKEY$`, `INPUT$(n)`, `INPUT$(n,#n)`, `CSRLIN`, `POS(0)`, `TAB`, `SPC`, `LPOS`, `LOCATE`, `WIDTH`, `SCREEN` (function: read char/attribute), `POINT` (graphics).

### 11.5 Date/Time

**Functions:** `DATE$`, `TIME$`, `TIMER`.  
**Statements:** `DATE$ = string$` sets the system date; `TIME$ = string$` sets the system time. Format for setting: `DATE$` "MM-DD-YYYY" or "MM-DD-YY"; `TIME$` "HH:MM" or "HH:MM:SS".

### 11.6 System / Environment

`COMMAND$`, `ENVIRON$`, `ENVIRON$(n)`, `FRE`, `SHELL` (statement and function form).

### 11.7 Arrays and Types

`LBOUND`, `UBOUND`, `LEN` (for UDT and `STRING*n`).

### 11.8 Error and Device

`ERR`, `ERL`, `ERDEV`, `ERDEV$`.

### 11.9 Graphics and Hardware (Joystick, Light Pen)

`STICK`, `STRIG`, `PEN`, `PMAP`, `PALETTE` (statement; no `_PALETTECOLOR` in QB4.5).

### 11.10 Legacy I/O and System

`INP`, `OUT`, `WAIT`, `IOCTL`, `IOCTL$`, `VARPTR`, `VARSEG`, `SADD`, `PEEK`, `VARPTR$` (for some array cases). Behavior is hardware/DOS-specific.

---

## 12. Metacommands

QB4.5 metacommands begin with `$` and must appear **inside a comment** (`'` or `REM`). No space between `$` and the name. String arguments in single quotes. Multiple metacommands in one comment are separated by space or tab.

| Metacommand        | Description                               |
|--------------------|-------------------------------------------|
| `$STATIC`          | Arrays allocated at compile time. `ERASE` reinitializes (zero/null); `REDIM` has no effect. |
| `$DYNAMIC`         | Arrays allocated at run time. `ERASE` frees memory; `REDIM` can resize. |
| `$INCLUDE: 'file'` | Insert source from another file. Must be last on the line. Included files must **not** contain `SUB` or `FUNCTION`. |

**Implicitly dimensioned** arrays (not in a `DIM`) are always treated as `$STATIC`.

---

## 13. Graphics

### 13.1 Screen Modes

```basic
SCREEN mode
SCREEN 0    ' Text (40/80 columns)
SCREEN 1    ' 320x200, 4 colors
SCREEN 2    ' 640x200, 2 colors
SCREEN 7    ' 320x200, 16 colors
SCREEN 8    ' 640x200, 16 colors
SCREEN 9    ' 640x350, 16 colors
SCREEN 12   ' 640x480, 16 colors
SCREEN 13   ' 320x200, 256 colors
```

Additional parameters (e.g. `colorswitch`, `apage`, `vpage`) are documented in QB4.5.

### 13.2 Primitives and Attributes

```basic
COLOR foreground, background
CLS
CLS 0
CLS , color
LOCATE row, col
LOCATE row, col, cursor
WIDTH columns
WIDTH columns, rows
```

### 13.3 Drawing

```basic
PSET (x, y), color
PSET STEP (dx, dy), color
PRESET (x, y)
PRESET STEP (dx, dy)
LINE (x1,y1)-(x2,y2), color
LINE (x1,y1)-(x2,y2), color, B
LINE (x1,y1)-(x2,y2), color, BF
LINE -(x2,y2), color
LINE STEP(dx,dy)-(x2,y2), color
CIRCLE (x, y), radius, color
CIRCLE (x, y), r, color, start, end
CIRCLE (x, y), r, color, , , aspect
CIRCLE STEP(dx,dy), radius, color
PAINT (x, y), fillColor, borderColor
PAINT STEP (dx, dy), fillColor, borderColor
DRAW commandstring$
```

### 13.4 GET/PUT (Graphics)

```basic
GET (x1,y1)-(x2,y2), array()
PUT (x, y), array(), action
```

`action`: `PSET`, `PRESET`, `AND`, `OR`, `XOR`.

### 13.5 Viewport and Logical Coordinates

```basic
VIEW (x1,y1)-(x2,y2)
VIEW (x1,y1)-(x2,y2), color, border
VIEW SCREEN (x1,y1)-(x2,y2) ...
VIEW    ' Full screen
WINDOW (x1,y1)-(x2,y2)
WINDOW SCREEN (x1,y1)-(x2,y2)
WINDOW  ' Physical coords
PMAP(coord, function)
PCOPY sourcePage, destPage
PALETTE index, color
PALETTE USING array()
```

### 13.6 Other

```basic
POINT(x, y)       ' Pixel color
STICK(n)
STRIG(n)
PEN ON|OFF|STOP
PEN(n)
```

`STRETCH` and `SMOOTH` are QB4.5 modes for scaling (see QB4.5 docs).

---

## 14. Error and Event Trapping

### 14.1 Error Handling

```basic
ON ERROR GOTO label
ON ERROR GOTO 0    ' Disable
ON ERROR RESUME NEXT
ERR
ERL
ERDEV
ERDEV$
ERROR errornum
RESUME
RESUME NEXT
RESUME label
```

### 14.2 Event Trapping (ON event GOSUB)

```basic
ON KEY(n) GOSUB label
KEY(n) ON | OFF | STOP
KEY n, keyString$
KEY LIST
KEY ON | OFF

ON TIMER(seconds) GOSUB label
TIMER ON | OFF | STOP

ON PLAY(n) GOSUB label
PLAY ON | OFF | STOP

ON PEN GOSUB label
PEN ON | OFF | STOP

ON STRIG(n) GOSUB label
STRIG(n) ON | OFF | STOP

ON COM(n) GOSUB label
COM(n) ON | OFF | STOP

ON UEVENT GOSUB label
UEVENT ON | OFF | STOP
UEVENT

ON SIGNAL(n) GOSUB label
SIGNAL ON | OFF | STOP
```

---

## 15. DOS and System Integration

### 15.1 Shell and Process

```basic
SHELL [command$]
SHELL("command$")   ' Function: return code
SYSTEM
END
STOP
SLEEP [seconds]
SLEEP   ' Wait for key
```

### 15.2 Chaining and Running

```basic
RUN
RUN "program"
RUN linenum
CHAIN "program"
CHAIN "program", ALL
CHAIN "program", , DELETE "file"
```

`COMMON` variables are passed across `CHAIN` when not using `ALL`.

### 15.3 Environment and Command Line

```basic
ENVIRON string$
x$ = ENVIRON$(name$)
x$ = ENVIRON$(n)
x$ = COMMAND$
```

### 15.4 Debug and Utility

```basic
TRON
TROFF
LPRINT ...
FILES
FILES filespec$
```

### 15.5 Hardware Ports (Legacy)

```basic
v = INP(port)
OUT port, value
WAIT port, mask, xorMask
```

### 15.6 Interrupts and Machine Code (Legacy)

```basic
CALL INTERRUPT intnum, inregs, outregs
CALL INTERRUPTX intnum, inregs, outregs
CALL INT86OLD inregs, outregs      ' 16-bit; from QB.QLB
CALL INT86XOLD inregs, outregs    ' 32-bit; from QB.QLB
CALL ABSOLUTE [address]
CALLS name(args)                   ' Non-BASIC: segment:offset (SEG)
```

`CALL INT86OLD` and `CALL INT86XOLD` are supplied in **QB.QLB**; load that library to use them. Requires `DEF SEG`, appropriate register types, and is platform-specific.

### 15.7 Device I/O

```basic
IOCTL #n, string$
x$ = IOCTL$(#n)
LPOS(n)
```

---

## 16. Memory and Binary I/O

### 16.1 Segment and Peek/Poke

```basic
DEF SEG [=segment]
x = PEEK(address)
POKE address, value
```

### 16.2 Variable Addresses

```basic
VARPTR(variable)
VARSEG(variable)
VARPTR$(array())
SADD(string$)
```

### 16.3 Binary Files

```basic
BSAVE file$, offset, length
BLOAD file$[, offset]
```

`offset` is segment:offset or `VARPTR`-based, depending on context.

### 16.4 Memory Configuration

```basic
SETMEM bytes
FRE(0)
FRE(-1)
FREE
```

---

## References

- **Microsoft QuickBASIC: Language Reference.** For IBM Personal Computers and Compatibles. Microsoft Corporation © 1987, 1988. (Part 1: Language Fundamentals; Part 2: Statement and Function Reference; Appendix A: Keyboard Scan Codes and ASCII; Appendix B: Error Messages.) This project uses a plain-text export in `docs/qb_language_reference.txt`.
- **Microsoft QuickBASIC: Programming in BASIC.** For IBM Personal Computers and Compatibles. Microsoft Corporation © 1987, 1988. (Ch 1–7: programming topics; Ch 8–9: Statement/Function Summary and Quick-Reference Tables; Appendix A–I: BASICA conversion, version differences, Limits, Reserved Words, Metacommands, Compiling/Linking, Quick Libraries, Error Messages.) This project uses a plain-text export in `docs/qb_programming_in_basic.txt`.
- **`docs/reference/QUICKBASIC_4.5_OTHER_SOURCE_CONTENT.md`** — Lists material in those two manuals that this specification does not include (e.g. full alphabetical entries, appendices, tutorials, BC/LINK/LIB, Quick Libraries).
- QB64PE and QB64Fresh specs in this repo — for QB4.5 vs QB64 distinctions and extensions.

---

*This specification aims to cover the QB4.5 language as defined in the Microsoft Language Reference and Programming in BASIC. For per-statement syntax, BASICA differences, limits, reserved words, and tooling (BC, LINK, LIB, Quick Libraries), see the original manuals and the OTHER document.*
