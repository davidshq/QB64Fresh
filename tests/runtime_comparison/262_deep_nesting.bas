$CONSOLE:ONLY
' Runtime comparison: Deeply nested IF/FOR/SELECT
DIM i AS LONG, j AS LONG, k AS LONG
FOR i = 1 TO 2
    FOR j = 1 TO 2
        FOR k = 1 TO 2
            IF i = 1 THEN
                IF j = 1 THEN
                    IF k = 1 THEN
                        PRINT "111"
                    ELSE
                        PRINT "112"
                    END IF
                ELSE
                    PRINT "12x"
                END IF
            ELSE
                PRINT "2xx"
            END IF
        NEXT k
    NEXT j
NEXT i
PRINT "done"
END
