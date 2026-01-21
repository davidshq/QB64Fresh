' ============================================================================
' Graphics Drawing Primitives
' ============================================================================
' Demonstrates: SCREEN, PSET, LINE, CIRCLE, PAINT, CLS
' ============================================================================

' Initialize graphics mode
' SCREEN 12 = 640x480, 16 colors
SCREEN 12

' Clear screen with blue background
COLOR 15, 1    ' White on blue
CLS

' Title
LOCATE 1, 25
PRINT "Graphics Drawing Demo"

' --- Draw Points (PSET) ---
' Draw a pattern of points
FOR x = 50 TO 100
    FOR y = 50 TO 100
        IF (x + y) MOD 2 = 0 THEN
            PSET (x, y), 14    ' Yellow
        END IF
    NEXT y
NEXT x

' --- Draw Lines ---
' Horizontal line
LINE (150, 50)-(300, 50), 15          ' White

' Vertical line
LINE (150, 60)-(150, 120), 15         ' White

' Diagonal line
LINE (160, 60)-(300, 120), 10         ' Light green

' Box outline
LINE (320, 50)-(450, 120), 12, B      ' Light red, Box

' Filled box
LINE (470, 50)-(580, 120), 9, BF      ' Light blue, Box Filled

' --- Draw Circles ---
' Simple circle
CIRCLE (100, 200), 40, 14             ' Yellow

' Circle with aspect ratio (ellipse)
CIRCLE (220, 200), 50, 13, , , 0.5    ' Light magenta, squashed

' Arc (partial circle)
CIRCLE (350, 200), 40, 11, 0, 3.14    ' Light cyan, half circle

' --- Fill Areas (PAINT) ---
' Draw and fill a shape
CIRCLE (500, 200), 45, 12             ' Light red outline
PAINT (500, 200), 4, 12               ' Red fill, bounded by light red

' --- Complex Shape ---
' Draw a house
' Walls
LINE (50, 300)-(200, 400), 6, BF      ' Brown filled box

' Roof
LINE (50, 300)-(125, 250), 8          ' Dark gray
LINE (125, 250)-(200, 300), 8

' Door
LINE (100, 350)-(150, 400), 0, BF     ' Black door

' Window
LINE (160, 320)-(190, 350), 11, B     ' Cyan window outline
LINE (175, 320)-(175, 350), 11        ' Window cross
LINE (160, 335)-(190, 335), 11

' --- Concentric Circles ---
FOR r = 10 TO 50 STEP 10
    CIRCLE (400, 350), r, r \ 10 + 9
NEXT r

' --- Spiral Pattern ---
DIM angle AS SINGLE
DIM radius AS SINGLE
DIM cx AS INTEGER, cy AS INTEGER
cx = 550
cy = 350

FOR i = 0 TO 200
    angle = i * 0.15
    radius = i * 0.25
    x = cx + COS(angle) * radius
    y = cy + SIN(angle) * radius
    PSET (x, y), 10 + (i MOD 6)
NEXT i

' Status message
LOCATE 29, 20
PRINT "Press any key to exit..."

SLEEP
END
