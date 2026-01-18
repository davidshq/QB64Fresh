' Test multi-dimensional array indexing
DIM matrix(2, 3) AS INTEGER

' Fill the 3x4 matrix (0-2, 0-3)
matrix(0, 0) = 1
matrix(0, 1) = 2
matrix(0, 2) = 3
matrix(0, 3) = 4
matrix(1, 0) = 5
matrix(1, 1) = 6
matrix(1, 2) = 7
matrix(1, 3) = 8
matrix(2, 0) = 9
matrix(2, 1) = 10
matrix(2, 2) = 11
matrix(2, 3) = 12

PRINT "Matrix contents:"
PRINT matrix(0, 0); matrix(0, 1); matrix(0, 2); matrix(0, 3)
PRINT matrix(1, 0); matrix(1, 1); matrix(1, 2); matrix(1, 3)
PRINT matrix(2, 0); matrix(2, 1); matrix(2, 2); matrix(2, 3)

' Test 1D array too
DIM arr(5) AS INTEGER
arr(0) = 100
arr(5) = 555
PRINT "1D array: "; arr(0); arr(5)

END
