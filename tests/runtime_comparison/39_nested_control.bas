$CONSOLE:ONLY
' Runtime comparison: nested IF and FOR
DIM i AS LONG, j AS LONG
FOR i = 1 TO 2
    IF i = 1 THEN
        FOR j = 1 TO 2
            PRINT "a"; j;
        NEXT j
    ELSE
        PRINT "b";
    END IF
NEXT i
PRINT
END
