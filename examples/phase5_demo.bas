' QB64Fresh Phase 5 Demo - System Integration, Mouse, and Clipboard
' This demonstrates the new Phase 5 features

' ==================== System Integration ====================

PRINT "=== Phase 5: System Integration Demo ==="
PRINT

' File operations
PRINT "Testing file operations..."

' Create a test file
testfile$ = "test_phase5.tmp"
OPEN testfile$ FOR OUTPUT AS #1
PRINT #1, "Test content"
CLOSE #1

' Check if it exists
exists% = _FILEEXISTS(testfile$)
PRINT "  File exists check: "; exists%

' Rename the file
newname$ = "test_phase5_renamed.tmp"
NAME testfile$ AS newname$
exists% = _FILEEXISTS(newname$)
PRINT "  After rename, exists: "; exists%

' Delete the file
KILL newname$
exists% = _FILEEXISTS(newname$)
PRINT "  After KILL, exists: "; exists%

PRINT

' Directory operations
PRINT "Testing directory operations..."

testdir$ = "test_phase5_dir"
MKDIR testdir$
exists% = _DIREXISTS(testdir$)
PRINT "  After MKDIR, dir exists: "; exists%
RMDIR testdir$
exists% = _DIREXISTS(testdir$)
PRINT "  After RMDIR, dir exists: "; exists%

PRINT

' Shell command
PRINT "Testing SHELL command..."
SHELL "echo Hello from shell command"
PRINT

' Mouse and clipboard test (requires graphics mode)
' These will return 0/empty in text mode
mx% = _MOUSEX
my% = _MOUSEY
PRINT "Mouse position: "; mx%; ","; my%

PRINT "=== Phase 5 Demo Complete ==="
END
