$CONSOLE:ONLY
' Runtime comparison: multiple GOSUB to same label
PRINT "1"
GOSUB helper
PRINT "2"
GOSUB helper
PRINT "3"
END

helper:
PRINT " helper"
RETURN
