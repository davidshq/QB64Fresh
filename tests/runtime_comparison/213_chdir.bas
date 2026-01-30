$CONSOLE:ONLY
' Runtime comparison: CHDIR then file open with relative path
ON ERROR GOTO handler
MKDIR "rt_213_dir"
CHDIR "rt_213_dir"
OPEN "rt_213_f.txt" FOR OUTPUT AS #1
PRINT #1, "ok"
CLOSE #1
CHDIR ".."
OPEN "rt_213_dir/rt_213_f.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT s
CLOSE #2
KILL "rt_213_dir/rt_213_f.txt"
RMDIR "rt_213_dir"
PRINT "done"
END
handler:
PRINT "err"; ERR
RESUME NEXT
