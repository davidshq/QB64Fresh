' ============================================================================
' Binary and Random Access File I/O
' ============================================================================
' Demonstrates: BINARY mode, RANDOM mode, GET, PUT, LOF, SEEK
' ============================================================================

PRINT "=== Binary and Random Access File Demo ==="
PRINT

' Define a record type for random access
TYPE StudentRecord
    id AS INTEGER
    grade AS SINGLE
END TYPE

DIM student AS StudentRecord
DIM filename AS STRING

' ============================================================================
' Part 1: Random Access Files
' ============================================================================
PRINT "--- Random Access Files ---"
PRINT

filename = "students.dat"

' Write records
PRINT "Writing student records..."

OPEN filename FOR RANDOM AS #1 LEN = LEN(student)

' Write 5 student records
FOR i = 1 TO 5
    student.id = i
    student.grade = 70 + i * 5
    PUT #1, i, student
    PRINT "  Wrote record"; i; ": ID="; student.id; ", Grade:"; student.grade
NEXT i

CLOSE #1
PRINT

' Read records
PRINT "Reading student records..."

OPEN filename FOR RANDOM AS #1 LEN = LEN(student)

' Read all records
numRecords = LOF(1) \ LEN(student)
PRINT "  File contains"; numRecords; "records"
PRINT

FOR i = 1 TO numRecords
    GET #1, i, student
    PRINT "  Record"; i; ": ID="; student.id; ", Grade="; student.grade
NEXT i

CLOSE #1
PRINT

' Random access (read record 3 directly)
PRINT "Reading record 3 directly..."
OPEN filename FOR RANDOM AS #1 LEN = LEN(student)
GET #1, 3, student
PRINT "  Record 3: ID="; student.id; ", Grade:"; student.grade
CLOSE #1
PRINT

' Update a record
PRINT "Updating record 2..."
OPEN filename FOR RANDOM AS #1 LEN = LEN(student)
student.id = 2
student.grade = 95.5
PUT #1, 2, student
CLOSE #1

' Verify update
OPEN filename FOR RANDOM AS #1 LEN = LEN(student)
GET #1, 2, student
PRINT "  Record 2 now: ID="; student.id; ", Grade:"; student.grade
CLOSE #1
PRINT

' ============================================================================
' Part 2: Binary Files
' ============================================================================
PRINT "--- Binary Files ---"
PRINT

filename = "binary_test.dat"

' Write binary data
PRINT "Writing binary data..."

OPEN filename FOR BINARY AS #1

' Write various data types
DIM intVal AS INTEGER
DIM longVal AS LONG
DIM singleVal AS SINGLE
DIM doubleVal AS DOUBLE

intVal = 12345
longVal = 123456789
singleVal = 3.14159
doubleVal = 2.718281828459

PUT #1, , intVal
PUT #1, , longVal
PUT #1, , singleVal
PUT #1, , doubleVal

' Write an array of bytes
DIM bytes(9) AS _UNSIGNED _BYTE
FOR i = 0 TO 9
    bytes(i) = i * 10
    PUT #1, , bytes(i)
NEXT i

CLOSE #1
PRINT "  Wrote integer, long, single, double, and 10 bytes"
PRINT

' Read binary data
PRINT "Reading binary data..."

OPEN filename FOR BINARY AS #1

GET #1, , intVal
GET #1, , longVal
GET #1, , singleVal
GET #1, , doubleVal

PRINT "  Integer:"; intVal
PRINT "  Long:"; longVal
PRINT "  Single:"; singleVal
PRINT "  Double:"; doubleVal

PRINT "  Bytes: ";
FOR i = 0 TO 9
    DIM b AS _UNSIGNED _BYTE
    GET #1, , b
    PRINT b;
NEXT i
PRINT

PRINT "  File size:"; LOF(1); "bytes"

CLOSE #1
PRINT

' ============================================================================
' Part 3: SEEK to Position
' ============================================================================
PRINT "--- SEEK Demo ---"
PRINT

OPEN filename FOR BINARY AS #1

' Seek to position and read
SEEK #1, 1                   ' Start of file
GET #1, , intVal
PRINT "  At position 1: INTEGER ="; intVal

SEEK #1, 3                   ' After the integer (2 bytes)
GET #1, , longVal
PRINT "  At position 3: LONG ="; longVal

' Get current position
PRINT "  Current position:"; SEEK(1)

CLOSE #1
PRINT

' ============================================================================
' Clean Up
' ============================================================================
PRINT "Cleaning up test files..."
KILL "students.dat"
KILL "binary_test.dat"
PRINT "  Files deleted."

PRINT
PRINT "Demo complete."
END
