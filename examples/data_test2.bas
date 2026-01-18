' Test DATA/READ/RESTORE functionality (numeric only)
DATA 10, 20, 30, 3.14159

DIM a AS INTEGER
DIM b AS INTEGER
DIM c AS INTEGER
DIM pi AS DOUBLE

READ a, b, c, pi

PRINT "Values: "; a; b; c
PRINT "Pi: "; pi

' Test RESTORE - reread
RESTORE
READ a
PRINT "After RESTORE: "; a

END
