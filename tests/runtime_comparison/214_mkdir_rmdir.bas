$CONSOLE:ONLY
' Runtime comparison: MKDIR, then RMDIR
ON ERROR GOTO handler
MKDIR "rt_214_dir"
PRINT "mkdir ok"
RMDIR "rt_214_dir"
PRINT "rmdir ok"
PRINT "done"
END
handler:
PRINT "err"; ERR
RESUME NEXT
