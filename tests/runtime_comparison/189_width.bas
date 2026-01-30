$CONSOLE:ONLY
' Runtime comparison 189: WIDTH (columns) effect on PRINT
' Set narrow width, print long string without newline; output should wrap at WIDTH columns.
WIDTH 10
PRINT "123456789012345";
PRINT "X"
PRINT "done"
END
