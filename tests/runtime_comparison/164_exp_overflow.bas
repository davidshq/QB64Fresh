$CONSOLE:ONLY
' Runtime comparison: EXP overflow / LOG domain
ON ERROR GOTO handler
PRINT EXP(700)
PRINT "no overflow"
END
handler:
PRINT "err"; ERR; "erl"; ERL
RESUME NEXT
