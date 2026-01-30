$CONSOLE:ONLY
' Runtime comparison: nested GOSUB
PRINT "main"
GOSUB a
PRINT "back"
END

a:
PRINT " in a"
GOSUB b
PRINT " after b"
RETURN

b:
PRINT "  in b"
RETURN
