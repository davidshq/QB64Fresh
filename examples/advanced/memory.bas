' ============================================================================
' Memory Operations
' ============================================================================
' Demonstrates: DEF SEG, VARSEG, VARPTR, PEEK, POKE
' Note: These use QB64Fresh's sandboxed conventional memory (cmem)
' ============================================================================

PRINT "=== Memory Operations Demo ==="
PRINT
PRINT "Note: QB64Fresh uses sandboxed 'conventional memory' (cmem)"
PRINT "for PEEK/POKE operations, similar to QB64pe's approach."
PRINT

' ============================================================================
' Part 1: VARPTR and VARSEG
' ============================================================================
PRINT "--- VARPTR and VARSEG ---"
PRINT

DIM myInt AS INTEGER
DIM myLong AS LONG
DIM myString AS STRING

myInt = 12345
myLong = 987654321
myString = "Hello!"

PRINT "Variable addresses:"
PRINT "  myInt:    VARSEG="; VARSEG(myInt); " VARPTR="; VARPTR(myInt)
PRINT "  myLong:   VARSEG="; VARSEG(myLong); " VARPTR="; VARPTR(myLong)
PRINT "  myString: VARSEG="; VARSEG(myString); " VARPTR="; VARPTR(myString)
PRINT

' ============================================================================
' Part 2: DEF SEG, PEEK, and POKE
' ============================================================================
PRINT "--- DEF SEG, PEEK, POKE ---"
PRINT

' Set segment to variable's segment
DEF SEG = VARSEG(myInt)

' Read bytes from integer
DIM addr AS INTEGER
addr = VARPTR(myInt)

PRINT "Reading myInt ("; myInt; ") byte by byte:"
PRINT "  Byte 0 (low):  "; PEEK(addr)
PRINT "  Byte 1 (high): "; PEEK(addr + 1)
PRINT "  Reconstructed: "; PEEK(addr) + PEEK(addr + 1) * 256
PRINT

' Modify a value using POKE
PRINT "Modifying myInt using POKE..."
POKE addr, 0        ' Low byte = 0
POKE addr + 1, 1    ' High byte = 1  (value = 256)
PRINT "  After POKE: myInt ="; myInt
PRINT

' ============================================================================
' Part 3: Working with Arrays
' ============================================================================
PRINT "--- Arrays in Memory ---"
PRINT

DIM numbers(4) AS INTEGER
FOR i = 0 TO 4
    numbers(i) = (i + 1) * 100
NEXT i

PRINT "Array contents:"
FOR i = 0 TO 4
    PRINT "  numbers("; i; ") ="; numbers(i)
NEXT i
PRINT

DEF SEG = VARSEG(numbers(0))
addr = VARPTR(numbers(0))

PRINT "Reading array from memory (2 bytes per INTEGER):"
FOR i = 0 TO 4
    DIM lo AS INTEGER, hi AS INTEGER
    lo = PEEK(addr + i * 2)
    hi = PEEK(addr + i * 2 + 1)
    PRINT "  Element"; i; ": lo="; lo; " hi="; hi; " value="; lo + hi * 256
NEXT i
PRINT

' ============================================================================
' Part 4: BLOAD and BSAVE Concept
' ============================================================================
PRINT "--- BLOAD and BSAVE ---"
PRINT

DIM buffer(99) AS _UNSIGNED _BYTE

' Fill buffer with test data
FOR i = 0 TO 99
    buffer(i) = i
NEXT i

' Save to file
DEF SEG = VARSEG(buffer(0))
BSAVE "memory_test.bin", VARPTR(buffer(0)), 100

PRINT "Saved 100 bytes to memory_test.bin"

' Clear buffer
FOR i = 0 TO 99
    buffer(i) = 0
NEXT i

' Load back
DEF SEG = VARSEG(buffer(0))
BLOAD "memory_test.bin", VARPTR(buffer(0))

PRINT "Loaded data back from file"
PRINT "First 10 bytes: ";
FOR i = 0 TO 9
    PRINT buffer(i);
NEXT i
PRINT
PRINT

' Reset DEF SEG
DEF SEG

' Clean up
KILL "memory_test.bin"

' ============================================================================
' Reference
' ============================================================================
PRINT "--- Memory Commands Reference ---"
PRINT
PRINT "  DEF SEG [= segment]     Set current memory segment"
PRINT "  VARSEG(variable)        Get segment of variable"
PRINT "  VARPTR(variable)        Get offset of variable"
PRINT "  PEEK(offset)            Read byte from current segment"
PRINT "  POKE offset, value      Write byte to current segment"
PRINT "  BSAVE file$, offset, length   Save memory to file"
PRINT "  BLOAD file$, offset     Load file to memory"
PRINT

PRINT "Demo complete."
END
