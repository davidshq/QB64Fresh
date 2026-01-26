# Content in the Microsoft QuickBASIC Manuals Not in the QuickBasic 4.5 Language Specification

**Purpose:** Lists everything present in *Microsoft QuickBASIC: Language Reference* and *Microsoft QuickBASIC: Programming in BASIC* (Microsoft Corporation © 1987, 1988) that **is not** included in `QUICKBASIC_4.5_LANGUAGE_SPECIFICATION.md`. This repo uses plain-text exports in `docs/qb_language_reference.txt` and `docs/qb_programming_in_basic.txt` for reference.  
**Created:** 2026-01-25

Use this document to find where to look in those two manuals for material the spec omits.

**Attribution and originality.** This document summarizes the **structure and topics** of the Microsoft manuals (chapters, sections, appendixes, table names) so readers know where to look. It is written in our own words and does **not** reproduce the manuals’ prose, examples, or layouts. Chapter and section titles used to refer to the originals (e.g. “Control-Flow Structures,” “SUB and FUNCTION Procedures”) are from those publications. For exact wording, full tables, figures, and examples, use the original Microsoft manuals.

---

## 1. Language Reference — Omitted Content

*(Microsoft QuickBASIC: Language Reference; this project’s copy: `docs/qb_language_reference.txt`.)*

### 1.1 Front Matter and Conventions

- **Document conventions:** Typography for keywords, placeholders, `«optional»`, `{choice1|choice2}`, key names (ENTER, CTRL+E, ALT+F1), “defined terms,” acronyms. The spec does not define notation.
- **Reference page format:** For each statement/function the Language Reference gives: **Action**, **Syntax** (with `«»`, `{ }`, …), **Remarks**, **See Also**, **Differences from BASICA**, **Example** (often with full program and output). The spec summarizes by topic; it does not provide this per-entry format or the “Differences from BASICA” sections.

### 1.2 Part 1 — Language Fundamentals (Ch 1–4): Deeper Detail Only in Sources

- **Ch 1 Language Elements:** Table 1.1 (character-by-character names: ENTER, `!`, `#`, …, `_`). The spec summarizes the character set; the full table is only in the source.
- **Ch 2 Data Types:** Figure 2.1 (bit layout of INTEGER, LONG, SINGLE, DOUBLE); Table 2.1 (types of numeric constants); Table 2.2 (variable-type memory requirements); rules for single vs double precision constants (E exponent, `!`, `#`, digit count); two’s complement and IEEE floating-point explanation; the FLPT.BAS-style discussion of exponent, mantissa, bias, normalization. The spec gives ranges and sizes; it does not include the figures, tables, or representational detail.
- **Ch 3 Expressions and Operators:** Full hierarchy with (a)(b)(c) sub-ordering; left-to-right for same level; note on `4^-2`; series like `3+5+6` and order of FUNCTION calls; Tables 3.1 (relational), 3.2 (logical), 3.3 (truth tables); floating-point comparison caveats (compiled vs environment, `A!=B!/3.0`); logical operands -2,147,483,648 to +2,147,483,647; `NOT X = -(X+1)`; string comparison examples (`"AA"<"AB"`, `"kg">"KG"`, etc.). The spec states precedence and main rules; it does not reproduce the tables, examples, or caveats.
- **Ch 4 Programs and Modules:** Structure of main vs non-main modules; “module-level code”; detailed comparison of SUB vs GOSUB, FUNCTION vs DEF FN (locals, globals, multi-module, recursion, by-ref vs by-value); the `(A#)` trick for by-value; recursion example (Reverse$) and CLEAR stack. The spec outlines programs/modules and DEF FN; the narrative and examples are only in the source.

### 1.3 Part 2 — Per-Statement/Function Pages (Alphabetical)

For **each** of the following, the Language Reference has a **full reference page** (Action, Syntax, Remarks, See Also, BASICA differences, Example). The spec only groups or mentions them by topic; it does **not** provide:

- The exact **syntax** with `«»`, `{ }`, and `…`
- The **“Differences from BASICA”** subsection
- The full **Example** (and often **Output**)
- The **See Also** list
- The **Remarks** in full (edge cases, limits, interaction with other statements)

**Statements and functions with full reference pages in the Language Reference (all omitted as above):**

ABS, ASC, ATN, BEEP, BLOAD, BSAVE, CALL (BASIC), CALL/CALLS (Non-BASIC), CALL ABSOLUTE, **CALL INT86OLD**, **CALL INT86XOLD** (and INT86XOLD register layout), CALL INTERRUPT, CALL INTERRUPTX, CDBL, CHAIN, CHDIR, CHR$, CINT, CIRCLE, CLEAR, CLNG, CLOSE, CLS, COLOR, COM (statements), COMMON, CONST, COS, CSNG, CSRLIN, CVI/CVS/CVL/CVD, CVSMBF/CVDMBF, DATA, **DATE$** (Function), **DATE$** (Statement), DECLARE (BASIC), DECLARE (Non-BASIC), DEF FN, DEF SEG, DEFtype, DIM, DO…LOOP, DRAW, END, ENVIRON, ENVIRON$ (and ENVIRON statement), EOF, ERASE, ERDEV/ERDEV$, ERR/ERL, ERROR, EXIT, EXP, FIELD, **FILEATTR** (including Table R.2: 1=INPUT, 2=OUTPUT, 4=RANDOM, 8=APPEND, 32=BINARY; attribute 2 = DOS handle), FILES, FIX, FOR…NEXT, FRE, FREEFILE, FUNCTION, GET (File I/O), GET (Graphics), GOSUB…RETURN, GOTO, HEX$, IF…THEN…ELSE, INKEY$, INP, INPUT, INPUT$, INPUT #, INSTR, INT, IOCTL, IOCTL$, KEY (n), KEY (statements), KILL, LBOUND, LCASE$, LEFT$, LEN, LET, LINE, LINE INPUT, LINE INPUT #, LOC, LOCATE, LOCK…UNLOCK, LOF, LOG, LPOS, LPRINT/LPRINT USING, LSET, LTRIM$, MID$ (function), MID$ (statement), MKD$/MKI$/MKL$/MKS$, MKDIR, MKSMBF$/MKDMBF$, NAME, OCT$, ON ERROR, **ON event** (ON KEY, ON TIMER, ON PLAY, ON PEN, ON STRIG, ON COM, ON UEVENT, ON SIGNAL — each with its key/parameter conventions), ON…GOSUB/ON…GOTO, ON UEVENT GOSUB, **OPEN**, **OPEN COM**, OPTION BASE, OUT, PAINT, PALETTE/PALETTE USING, PCOPY, PEEK, PEN (function), PEN ON/OFF/STOP, PLAY (function), PLAY (statement), PLAY ON/OFF/STOP, PMAP, POINT, POKE, POS, PRESET, PRINT, PRINT USING, PRINT #/PRINT # USING, PSET, PUT (File I/O), PUT (Graphics), RANDOMIZE, READ, REDIM, REM, RESET, RESTORE, RESUME, RETURN, RIGHT$, RMDIR, RND, RSET, RTRIM$, RUN, SADD, SCREEN (function), SCREEN (statement), SEEK (function), SEEK (statement), SELECT CASE, SETMEM, SGN, SHARED, SHELL, SIN, SLEEP, SOUND, SPC, SQR, STATIC, STICK, STOP, STR$, STRIG (function and statement), STRIG ON/OFF/STOP, STRING$, SUB, SWAP, SYSTEM, TAB, TAN, **TIME$** (Function), **TIME$** (Statement), TIMER, TIMER ON/OFF/STOP, TRON/TROFF, TYPE, UBOUND, UCASE$, UEVENT, UNLOCK, VAL, VARPTR/VARSEG, VARPTR$, **VIEW**, **VIEW PRINT**, WAIT, WHILE…WEND, WIDTH, WINDOW, **WRITE**, **WRITE #**.

### 1.4 Appendix A — Keyboard Scan Codes and ASCII Character Codes

- **A.1 Keyboard Scan Codes:** Full table (key, scan code, ASCII/extended with SHIFT/CTRL/ALT). The spec does not include this table.
- **A.2 ASCII Character Codes:** Full 0–255 code chart (e.g. control, printable, extended). The spec does not include this.

### 1.5 Appendix B — Error Messages

- **B.1 Error-Message Display:** How errors appear in the QB environment vs stand-alone .EXE (address vs line number when /D, /E, /X).
- **B.2 Invocation, Compile-Time, and Run-Time Error Messages:** Alphabetical and (where applicable) numeric list of all error texts and codes.
- **B.3 LINK Error Messages**
- **B.4 LIB Error Messages**

The spec refers to ERR, ERL, ON ERROR, RESUME; it does **not** list individual error codes, LINK, or LIB messages.

### 1.6 Tables and Figures (in Language Reference)

- **Figure 2.1** BASIC Numeric Representations  
- **Figure R.1** WINDOW and WINDOW SCREEN  
- **Table 2.1** Types of Numeric Constants  
- **Table 2.2** Variable-Type Memory Requirements  
- **Table 3.1** Relational Operators and Their Functions  
- **Table 3.2** BASIC Logical Operators  
- **Table 3.3** Values Returned by Logical Operations  
- **Table R.1** INT86OLD and INT86XOLD Register Values  
- **Table R.2** FILEATTR Mode Codes (reproduced in spec in prose; the table itself is only in the source)  
- **Table R.3** Values for Bits per Pixel per Plane and for Planes  
- **Table R.4** Keyboard Scan Codes  
- **Table R.5** SCREEN Color and Attribute Ranges  
- **Table R.6** MDPA Screen Modes  
- **Table R.7** Hercules Screen Modes  
- **Table R.8** CGA Screen Modes  
- **Table R.9** EGA Screen Modes  
- **Table R.10** Default Attributes: SCREEN 10, Monochrome Display  
- **Table R.11** Color Values: SCREEN 10, Monochrome Display  
- **Table R.12** VGA Screen Modes  
- **Table R.13** MCGA Screen Modes  
- **Table R.14** Default Attributes and Colors for SCREEN Modes 1 and 9  
- **Table R.15** Default Attributes and Colors for SCREEN Modes 2 and 11  
- **Table R.16** Default Attributes and Colors for SCREEN Modes 0, 7, 8, 9, 12, and 13  

### 1.7 Index

The Language Reference Index (statement/function names, concepts). The spec has no index.

---

## 2. Programming in BASIC — Omitted Content

*(Microsoft QuickBASIC: Programming in BASIC; this project’s copy: `docs/qb_programming_in_basic.txt`.)*

### 2.1 Introduction and Conventions

- **The QuickBASIC Language** and **The QuickBASIC Environment:** High-level overview.
- **Using This Manual:** Selected Programming Topics vs The Heart of BASIC; how to use the appendixes.
- **Document Conventions:** Same style as the Language Reference.
- **Programming Style in this Manual:** Style choices used in the sample programs.

### 2.2 Part 1 — Selected Programming Topics (Ch 1–7): Tutorial and Narrative

The spec does **not** include the **teaching narrative**, **design guidance**, or **full sample programs** from:

- **Ch 1 Control-Flow Structures:** 1.1–1.5: changing execution order, Boolean expressions, block IF, SELECT CASE vs ON…GOSUB, FOR…NEXT (EXIT FOR, pausing), WHILE…WEND, DO…LOOP (tests, EXIT DO), and sample programs (CHECK.BAS, CRLF.BAS).
- **Ch 2 SUB and FUNCTION Procedures:** 2.1–2.11: procedures vs GOSUB/DEF FN, defining and calling, passing (constants, variables, arrays, records, DECLARE, by reference/by value), SHARED (with procedures, with all in module, with other modules, aliasing), STATIC, recursion (factorial, stack), CHAIN, and sample (WHEREIS.BAS).
- **Ch 3 File and Device I/O:** 3.1–3.6: printing (PRINT, PRINT USING, TAB, SPC, WIDTH, VIEW PRINT), INPUT, LINE INPUT, INPUT$, INKEY$, cursor (LOCATE, shape, CSRLIN/POS), file organization, sequential vs random, OPEN/CLOSE, sequential (records, write, read, append, WRITE #, LINE INPUT #), random (records, FIELD, LSET/RSET, GET/PUT, sequential read, record numbers), binary (SEEK, comparison with random), devices (OPEN COM, COM statements), and samples (CAL.BAS, INDEX.BAS, TERMINAL.BAS).
- **Ch 4 String Processing:** 4.1–4.11: strings, variable vs fixed-length, concatenation, comparison, search (INSTR), LEFT$/RIGHT$/MID$, generation (SPACE$, STRING$, CHR$), case (UCASE$/LCASE$), conversion (VAL, STR$), and sample (STRTONUM.BAS).
- **Ch 5 Graphics:** 5.1–5.11: hardware needs, pixels, PSET/PRESET, LINE (STEP, boxes, dotted), CIRCLE (arcs, pie, aspect), VIEW, WINDOW (coordinate order, view vs physical), COLOR, PALETTE/PALETTE USING, PAINT (tiling, pattern size, modes 1, 2, 8), DRAW, GET/PUT, animation, screen pages, and samples (BAR.BAS, MANDEL.BAS, EDPAT.BAS).
- **Ch 6 Error and Event Trapping:** 6.1–6.6: ON ERROR, writing a handler, ERR, RESUME; event trapping (polling vs trapping, ONevent, ON KEY/PLAY/PEN/STRIG/COM/UEVENT/SIGNAL, suspend/disable), KEY (user-defined, shifted), PLAY, trapping in SUB/FUNCTION, across modules, BC-compiled programs, and sample (FILERR.BAS).
- **Ch 7 Programming with Modules:** 7.1–7.11: why modules, main module, procedure-only modules, creating and loading, DECLARE with multiple modules, COMMON and variable sharing, development, **compiling and linking modules**, **Quick Libraries (creating)**, and tips.

### 2.3 Part 2 — Heart of BASIC

- **Ch 8 Statement and Function Summary:** Condensed one-paragraph (or short) description of each statement/function. The spec groups by topic; it does not provide this **condensed summary** list.
- **Ch 9 Quick-Reference Tables:**
  - 9.1 Summary of Control-Flow Statements  
  - 9.2 Summary of Statements Used in BASIC Procedures  
  - 9.3 Summary of Standard I/O Statements  
  - 9.4 Summary of File I/O Statements  
  - 9.5 Summary of String-Processing Statements and Functions  
  - 9.6 Summary of Graphics Statements and Functions  
  - 9.7 Summary of Trapping Statements and Functions  

### 2.4 Appendix A — Converting BASICA Programs to QuickBASIC

- Narrative and rules for converting BASICA to QuickBASIC. The spec does not describe BASICA or conversion.

### 2.5 Appendix B — Differences from Previous Versions of QuickBASIC

- **B.1–B.5:** Version-by-version (e.g. 4.0 vs 4.5) changes: new features, changed behavior, deprecated or removed items, include-file restrictions, file compatibility. The spec does not include version history.

### 2.6 Appendix C — Limits in QuickBASIC

- **Table C.1 QuickBASIC Limits:** The **full** table is only in the source. The spec cites some limits (e.g. 8 dimensions, 40-char names, 32,767 string, 256 editor line, 65,535-byte static array, procedure args 60). The **complete** C.1 includes additionally, among others:
  - Min/max for integers, long, single, double (positive/negative)
  - Array: static 65,535 bytes, dynamic “available memory”; dimensions 1–8; subscripts
  - Procedure args 0–60; nesting of include files 0–5; procedure size (interpreted) 0–64K; module size (compiled) 0–64K
  - Data files: open 0–255, record number, record size 1–32,767, path 1–127, error message numbers 1–255
  - Editing: text box 0–128, “Search for” 1–128, “Change to” 0–40, placemarkers 0–4, watchpoints 0–8, Immediate-window lines 0–10, View characters 0–255, COMMAND$ length 0–124  

### 2.7 Appendix D — Keyboard Scan Codes and ASCII Character Codes

- Same scope as the Language Reference Appendix A: full scan-code and ASCII tables. The spec does not include these.

### 2.8 Appendix E — BASIC Reserved Words

- **Complete alphabetical list** of reserved words (e.g. ABS, ACCESS, ALIAS, ANY, APPEND, AS, … through XOR). The spec gives a “representative” set; the **full** list is only in the source.

### 2.9 Appendix F — Metacommands

- **F.1 Metacommand Syntax:** `REM $METACOMMAND [[ : argument ]]`; `$` in comment; multiple per line; string args in single quotes; `REM x$STATIC` to disable. The spec describes `$STATIC`, `$DYNAMIC`, `$INCLUDE`; the **exact** syntax and the “x$” disabling trick are in the source.
- **F.2 $INCLUDE:** “Last on line,” included files must not contain SUB/FUNCTION, BASICA ,A. The spec mentions restrictions; the **full** F.2 is in the source.
- **F.3 $STATIC and $DYNAMIC:** Effect on ERASE and REDIM; implicitly dimensioned always $STATIC. The spec summarizes; the **full** F.3 is in the source.

### 2.10 Appendix G — Compiling and Linking from DOS

- **G.1** BC, LINK, LIB: roles.
- **G.2** Compiling and linking process; /E when using LINK outside the environment; LIB and INCLUDE.
- **G.3** BC: source/object/listing, prompts, **G.3.1** file names (case, extensions, paths), **G.3.2** **BC options** (/A, /AH, /C, /D, /E, /MBF, /O, /R, /S, /V, /W, /X, /ZD, /ZI).
- **G.4** LINK: syntax, prompts, response file, **G.4.1** defaults, **G.4.2** file names, **G.4.3** libraries, **G.4.4** memory and VM.TMP, **G.4.5** mixed-language (Pascal, FORTRAN, assembly, DGROUP, NMALLOC), **G.4.6** **LINK options** (/HE, /PAUSE, /MAP, /NOD, /E, /SE, /CO, /NOI, etc.).
- **G.5** LIB: creating/altering stand-alone libraries; LIB options (e.g. /P:pagesize).

The spec does **not** describe BC, LINK, or LIB.

### 2.11 Appendix H — Creating and Using Quick Libraries

- **H.1** Types: .QLB (Quick Library) vs .LIB.
- **H.2** Advantages of Quick Libraries.
- **H.3** Creating: from environment; **H.3.1–H.3.3** unloading, loading, Make Library dialog; **H.3.1–H.3.2** files (QB, BC, LINK, LIB, BQLB45.LIB); event trapping note (e.g. TIMER OFF in library).
- **H.4** Using: **H.4.1** loading (`QB /L`, `QB /RUN prog /L lib`), **H.4.2** floating-point in .QLB, **H.4.3** QLBDUMP.BAS.
- **H.5** QB.QLB: INTERRUPT, INT86OLD, ABSOLUTE.
- **H.6** .QLB extension convention.
- **H.7** Making .QLB from command line (LINK /Q, BQLB45.LIB).
- **H.8** Other languages in Quick Libraries: building, leading zeros in first segment, **H.8.3 B_OnExit**.
- **H.9** Memory (FRE(-1), .QLB size).
- **H.10** Making compact .EXE (module layout, QLBDUMP).

The spec does **not** describe Quick Libraries, .QLB, or B_OnExit.

### 2.12 Appendix I — Error Messages

- **I.1** Error-message display (environment vs stand-alone, “Error n in module … at address” vs “in line …”).
- **I.2** Alphabetical list of invocation, compile-time, and run-time errors.
- **Table I.1** Run-time error codes in numerical order (e.g. 2, 3, 4, 5, 6, 7, 9, … 53, 54, 55, 56, 57, 58, 59, …).
- **I.3** LINK error messages.
- **I.4** LIB error messages.

The spec does **not** list these.

### 2.13 Figures and Tables (Programming in BASIC)

- All figures and tables in the Programming in BASIC volume (e.g. control-flow, procedures, I/O, string, graphics, trapping, Quick-Reference Tables, and those in Appendixes C–I). The spec does not reproduce these.

---

## 3. Summary: What to Get Where

| Need | In spec? | Where in sources |
|------|----------|------------------|
| Per-statement syntax with `«»`, `{ }` | No | Language Reference Part 2 (each name); Programming in BASIC Ch 8 |
| “Differences from BASICA” | No | Language Reference Part 2, each entry |
| Full worked examples and output | No | Language Reference Part 2; Programming in BASIC Ch 1–7 |
| Character set table (Table 1.1) | No | Language Reference Ch 1 |
| Numeric representation (Figure 2.1, Tables 2.1–2.2, IEEE/ two’s complement) | No | Language Reference Ch 2 |
| Operator tables and truth table (3.1–3.3) | No | Language Reference Ch 3 |
| SCREEN / FILEATTR / INT86OLD tables (R.1–R.16) | No | Language Reference Tables |
| Keyboard Scan Codes | No | Language Reference App A; Programming in BASIC App D |
| ASCII 0–255 | No | Language Reference App A; Programming in BASIC App D |
| Error message list (invocation, compile, run-time, LINK, LIB) | No | Language Reference App B; Programming in BASIC App I |
| Limits (full Table C.1) | Partial | Programming in BASIC App C |
| Reserved words (full list) | No | Programming in BASIC App E |
| Metacommand syntax in full (F.1–F.3) | Partial | Programming in BASIC App F |
| BC options and prompts | No | Programming in BASIC App G |
| LINK options and use | No | Programming in BASIC App G |
| LIB and stand-alone libraries | No | Programming in BASIC App G |
| Quick Libraries (.QLB), B_OnExit, QLBDUMP | No | Programming in BASIC App H |
| BASICA conversion | No | Programming in BASIC App A |
| Version differences (e.g. 4.0 vs 4.5) | No | Programming in BASIC App B |
| Tutorial narrative (control flow, procedures, I/O, strings, graphics, trapping, modules) | No | Programming in BASIC Ch 1–7 |
| Condensed statement/function summary | No | Programming in BASIC Ch 8 |
| Quick-Reference Tables (9.1–9.7) | No | Programming in BASIC Ch 9 |
| Document conventions and programming style | No | Both manuals; introductions |
| Index | No | Language Reference |

---

*For the QB4.5 language itself, use `QUICKBASIC_4.5_LANGUAGE_SPECIFICATION.md`. For the items in this list, use the original Microsoft manuals; this project’s plain-text exports are in `docs/qb_language_reference.txt` and `docs/qb_programming_in_basic.txt`.*
