' ============================================================================
' File System Operations
' ============================================================================
' Demonstrates: MKDIR, RMDIR, CHDIR, KILL, NAME, _FILEEXISTS, _DIREXISTS
' ============================================================================

PRINT "=== File System Operations Demo ==="
PRINT

DIM testDir AS STRING
DIM testFile AS STRING
DIM newName AS STRING

testDir = "test_directory"
testFile = "test_file.txt"
newName = "renamed_file.txt"

' ============================================================================
' Directory Operations
' ============================================================================
PRINT "--- Directory Operations ---"
PRINT

' Create directory
PRINT "Creating directory '"; testDir; "'..."
MKDIR testDir

IF _DIREXISTS(testDir) THEN
    PRINT "  Directory created successfully!"
ELSE
    PRINT "  Failed to create directory."
END IF
PRINT

' Show current directory
PRINT "Current directory:"
SHELL "pwd"       ' Linux/Mac
' SHELL "cd"      ' Windows
PRINT

' ============================================================================
' File Operations
' ============================================================================
PRINT "--- File Operations ---"
PRINT

' Create a test file in the new directory
DIM fullPath AS STRING
fullPath = testDir + "/" + testFile

PRINT "Creating file '"; fullPath; "'..."
OPEN fullPath FOR OUTPUT AS #1
PRINT #1, "This is a test file."
PRINT #1, "Created by QB64Fresh!"
CLOSE #1

IF _FILEEXISTS(fullPath) THEN
    PRINT "  File created successfully!"
ELSE
    PRINT "  Failed to create file."
END IF
PRINT

' Check file existence
PRINT "Checking file existence:"
PRINT "  '"; fullPath; "' exists: "; _FILEEXISTS(fullPath)
PRINT "  'nonexistent.txt' exists: "; _FILEEXISTS("nonexistent.txt")
PRINT

' Rename file
DIM newPath AS STRING
newPath = testDir + "/" + newName

PRINT "Renaming '"; fullPath; "' to '"; newPath; "'..."
NAME fullPath AS newPath

IF _FILEEXISTS(newPath) THEN
    PRINT "  File renamed successfully!"
    PRINT "  Old name exists: "; _FILEEXISTS(fullPath)
    PRINT "  New name exists: "; _FILEEXISTS(newPath)
ELSE
    PRINT "  Failed to rename file."
END IF
PRINT

' Read the renamed file to verify
PRINT "Reading renamed file:"
OPEN newPath FOR INPUT AS #1
DO WHILE NOT EOF(1)
    DIM textLine AS STRING
    LINE INPUT #1, textLine
    PRINT "  "; textLine
LOOP
CLOSE #1
PRINT

' ============================================================================
' Shell Command
' ============================================================================
PRINT "--- SHELL Command ---"
PRINT

PRINT "Listing directory contents with SHELL:"
SHELL "ls -la " + testDir     ' Linux/Mac
' SHELL "dir " + testDir      ' Windows
PRINT

' ============================================================================
' Clean Up
' ============================================================================
PRINT "--- Cleanup ---"
PRINT

' Delete file
PRINT "Deleting file..."
KILL newPath

IF NOT _FILEEXISTS(newPath) THEN
    PRINT "  File deleted."
ELSE
    PRINT "  Failed to delete file."
END IF

' Remove directory
PRINT "Removing directory..."
RMDIR testDir

IF NOT _DIREXISTS(testDir) THEN
    PRINT "  Directory removed."
ELSE
    PRINT "  Failed to remove directory (may not be empty)."
END IF
PRINT

' ============================================================================
' Reference
' ============================================================================
PRINT "--- File System Commands Reference ---"
PRINT
PRINT "Directory Operations:"
PRINT "  MKDIR path$        Create directory"
PRINT "  RMDIR path$        Remove empty directory"
PRINT "  CHDIR path$        Change current directory"
PRINT "  _DIREXISTS(path$)  Check if directory exists"
PRINT
PRINT "File Operations:"
PRINT "  KILL file$                 Delete file"
PRINT "  NAME old$ AS new$          Rename file"
PRINT "  _FILEEXISTS(file$)         Check if file exists"
PRINT "  SHELL command$             Execute system command"
PRINT

PRINT "Demo complete."
END
