$CONSOLE:ONLY
' Runtime comparison: GOSUB 3+ levels
PRINT "main"
GOSUB level1
PRINT "back main"
END

level1:
PRINT "L1"
GOSUB level2
RETURN

level2:
PRINT "L2"
GOSUB level3
RETURN

level3:
PRINT "L3"
RETURN
