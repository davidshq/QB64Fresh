$CONSOLE:ONLY
' Runtime comparison: EOF on empty file
OPEN "rt_180_empty.txt" FOR OUTPUT AS #1
CLOSE #1
OPEN "rt_180_empty.txt" FOR INPUT AS #2
PRINT "eof:"; EOF(2)
CLOSE #2
KILL "rt_180_empty.txt"
PRINT "done"
END
