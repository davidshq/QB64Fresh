$CONSOLE:ONLY
' Runtime comparison: LOC after PRINT # / WRITE #
OPEN "rt_181_loc.txt" FOR OUTPUT AS #1
PRINT "loc after open:"; LOC(1)
PRINT #1, "abc"
PRINT "loc after print:"; LOC(1)
WRITE #1, "x"
PRINT "loc after write:"; LOC(1)
CLOSE #1
OPEN "rt_181_loc.txt" FOR INPUT AS #2
PRINT "loc input:"; LOC(2)
CLOSE #2
KILL "rt_181_loc.txt"
PRINT "done"
END
