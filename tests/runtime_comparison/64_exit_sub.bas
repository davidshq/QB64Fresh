$CONSOLE:ONLY
' Runtime comparison: EXIT SUB
CALL mySub
PRINT "back"
END

SUB mySub
    PRINT "in sub"
    EXIT SUB
    PRINT "never"
END SUB
