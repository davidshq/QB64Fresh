$CONSOLE:ONLY
' Runtime comparison: RND(0) returns last generated value (no advance).
' Same seed so sequence is deterministic per runtime; RND(0) must repeat previous.
RANDOMIZE 1
a = RND
b = RND
c = RND(0)
PRINT "rnd1:"; a
PRINT "rnd2:"; b
PRINT "rnd0:"; c
PRINT "rnd0_eq_rnd2:"; (c = b)
END
