$CONSOLE:ONLY
' Runtime comparison: LOF on binary file
OPEN "rt_182_bin.txt" FOR BINARY AS #1
PRINT #1, "12345";
PRINT "lof:"; LOF(1)
CLOSE #1
OPEN "rt_182_bin.txt" FOR BINARY AS #2
PRINT "lof read:"; LOF(2)
CLOSE #2
KILL "rt_182_bin.txt"
PRINT "done"
END
