$CONSOLE:ONLY
' Runtime comparison: NAME (rename file)
OPEN "rt_86_old.txt" FOR OUTPUT AS #1
PRINT #1, "content"
CLOSE #1
NAME "rt_86_old.txt" AS "rt_86_new.txt"
OPEN "rt_86_new.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT "renamed:"; s
CLOSE #2
KILL "rt_86_new.txt"
PRINT "done"
END
