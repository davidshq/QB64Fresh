' ============================================================================
' Hello World - Basic I/O Example
' ============================================================================
' Demonstrates: PRINT, INPUT, string concatenation
' ============================================================================

PRINT "Hello, World!"
PRINT

' Get user input
DIM userName AS STRING
INPUT "What is your name? ", userName
PRINT "Hello, "; userName; "!"
PRINT

' Formatted output
PRINT "Welcome to QB64Fresh!"
PRINT "====================="
PRINT
PRINT "Press any key to exit..."

' Wait for keypress (QB64 extension)
SLEEP

END
