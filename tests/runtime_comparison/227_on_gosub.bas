$CONSOLE:ONLY
' Runtime comparison: ON n GOSUB line1, line2, ...
ON 1 GOSUB S1, S2
PRINT "after 1"
ON 2 GOSUB S1, S2
PRINT "after 2"
END
S1:
PRINT "S1"
RETURN
S2:
PRINT "S2"
RETURN
