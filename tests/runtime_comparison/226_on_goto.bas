$CONSOLE:ONLY
' Runtime comparison: ON n GOTO line1, line2, ...
DIM n AS LONG
n = 1
ON n GOTO L1, L2
PRINT "fall1"
END
L1:
PRINT "L1"
n = 2
ON n GOTO L1, L2
PRINT "fall2"
END
L2:
PRINT "L2"
END
