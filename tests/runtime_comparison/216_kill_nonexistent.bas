$CONSOLE:ONLY
' Runtime comparison: KILL on nonexistent file (error code)
ON ERROR GOTO handler
KILL "rt_216_nonexistent_xyz.txt"
PRINT "no error"
END
handler:
PRINT "err"; ERR; "erl"; ERL
RESUME NEXT
