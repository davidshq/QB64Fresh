$CONSOLE:ONLY
' Runtime comparison 4: GOSUB/RETURN
PRINT "main 1"
GOSUB sub1
PRINT "main 2"
GOSUB sub1
PRINT "main 3"
END

sub1:
PRINT " in sub1"
RETURN
