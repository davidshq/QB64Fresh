$CONSOLE:ONLY
' Runtime comparison: RND(negative) reseeds and returns first value of new sequence.
RANDOMIZE 1
a = RND
b = RND
first = RND(-5)
second = RND
PRINT "after_seed1:"; a; b
PRINT "after_rnd_neg5:"; first; second
END
