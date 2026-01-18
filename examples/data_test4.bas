' Test DATA/READ/RESTORE with explicit DIM (no strings)
DATA 10, 20, 30, 3.14159

DIM a AS INTEGER
DIM b AS INTEGER
DIM c AS INTEGER
DIM pi AS DOUBLE

READ a, b, c, pi

PRINT "Values: "; a; b; c
PRINT "Pi: "; pi

RESTORE
READ a
PRINT "After RESTORE: "; a

END
