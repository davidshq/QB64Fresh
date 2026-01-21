' ============================================================================
' User-Defined Types (TYPE)
' ============================================================================
' Demonstrates: TYPE/END TYPE, arrays of types, nested types
' ============================================================================

' Define a simple type
TYPE Point
    x AS INTEGER
    y AS INTEGER
END TYPE

' Define a type with various numeric fields
TYPE Measurement
    id AS INTEGER
    value AS DOUBLE
    count AS LONG
    average AS SINGLE
END TYPE

' Define a type that contains another type
TYPE Rectangle
    topLeft AS Point
    bottomRight AS Point
    clr AS INTEGER
END TYPE

' ============================================================================
' Main Program
' ============================================================================

PRINT "=== Simple Type (Point) ==="

DIM p1 AS Point
DIM p2 AS Point

p1.x = 10
p1.y = 20

p2.x = 100
p2.y = 200

PRINT "Point 1: ("; p1.x; ","; p1.y; ")"
PRINT "Point 2: ("; p2.x; ","; p2.y; ")"
PRINT

PRINT "=== Numeric Type (Measurement) ==="

DIM m AS Measurement

m.id = 1
m.value = 123.456
m.count = 1000000
m.average = 45.67

PRINT "Measurement:"
PRINT "  ID:      "; m.id
PRINT "  Value:   "; m.value
PRINT "  Count:   "; m.count
PRINT "  Average: "; m.average
PRINT

PRINT "=== Array of Types ==="

DIM points(1 TO 4) AS Point

' Define corners of a square
points(1).x = 0: points(1).y = 0
points(2).x = 100: points(2).y = 0
points(3).x = 100: points(3).y = 100
points(4).x = 0: points(4).y = 100

PRINT "Square corners:"
FOR i = 1 TO 4
    PRINT "  Point"; i; ": ("; points(i).x; ","; points(i).y; ")"
NEXT i
PRINT

PRINT "=== Nested Types (Rectangle) ==="

DIM rect AS Rectangle

rect.topLeft.x = 0
rect.topLeft.y = 0
rect.bottomRight.x = 100
rect.bottomRight.y = 50
rect.clr = 15

PRINT "Rectangle:"
PRINT "  Top-Left:     ("; rect.topLeft.x; ","; rect.topLeft.y; ")"
PRINT "  Bottom-Right: ("; rect.bottomRight.x; ","; rect.bottomRight.y; ")"
PRINT "  Color:        "; rect.clr
PRINT

' Calculate width and height
DIM w AS INTEGER, h AS INTEGER
w = rect.bottomRight.x - rect.topLeft.x
h = rect.bottomRight.y - rect.topLeft.y
PRINT "  Width:  "; w
PRINT "  Height: "; h

END
