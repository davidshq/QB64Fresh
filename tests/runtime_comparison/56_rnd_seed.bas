$CONSOLE:ONLY
' Runtime comparison: RANDOMIZE, RND (output may differ between runtimes)
RANDOMIZE 1
PRINT "rnd1:"; RND
PRINT "rnd2:"; RND
PRINT "rnd0:"; RND(0)
END
