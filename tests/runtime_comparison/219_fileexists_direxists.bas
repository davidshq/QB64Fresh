$CONSOLE:ONLY
' Runtime comparison: _FILEEXISTS, _DIREXISTS (if supported)
ON ERROR GOTO handler
OPEN "rt_219_f.txt" FOR OUTPUT AS #1
CLOSE #1
PRINT _FILEEXISTS("rt_219_f.txt")
PRINT _FILEEXISTS("rt_219_nonexist.txt")
MKDIR "rt_219_d"
PRINT _DIREXISTS("rt_219_d")
RMDIR "rt_219_d"
KILL "rt_219_f.txt"
PRINT "done"
END
handler:
PRINT "err"; ERR
RESUME NEXT
