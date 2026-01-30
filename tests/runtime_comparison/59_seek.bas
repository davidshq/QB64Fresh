$CONSOLE:ONLY
' Runtime comparison: SEEK
OPEN "rt_59_seek.txt" FOR OUTPUT AS #1
PRINT #1, "abc"
PRINT #1, "def"
CLOSE #1
OPEN "rt_59_seek.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT "pos after read:"; SEEK(2)
LINE INPUT #2, s
PRINT "pos after read2:"; SEEK(2)
CLOSE #2
KILL "rt_59_seek.txt"
PRINT "done"
END
