$CONSOLE:ONLY
' Runtime comparison: LOF
OPEN "rt_96_lof.txt" FOR OUTPUT AS #1
PRINT #1, "hello"
CLOSE #1
OPEN "rt_96_lof.txt" FOR INPUT AS #2
PRINT "lof:"; LOF(2)
CLOSE #2
KILL "rt_96_lof.txt"
PRINT "done"
END
