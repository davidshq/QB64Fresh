' Test DATA/READ/RESTORE functionality
DATA 1, 2, 3, "hello", 3.14159

DIM a AS INTEGER
DIM b AS INTEGER
DIM c AS INTEGER
DIM s AS STRING
DIM pi AS DOUBLE

READ a, b, c
PRINT "First three numbers: "; a; b; c

READ s, pi
PRINT "String: "; s
PRINT "Pi: "; pi

' Test RESTORE - reread the data
RESTORE
READ a
PRINT "After RESTORE, first number: "; a

END
