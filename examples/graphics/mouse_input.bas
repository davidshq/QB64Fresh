' ============================================================================
' Mouse Input
' ============================================================================
' Demonstrates: _MOUSEINPUT, _MOUSEX, _MOUSEY, _MOUSEBUTTON, drawing
' ============================================================================

SCREEN 12   ' 640x480, 16 colors

' Current drawing color
DIM drawColor AS INTEGER
drawColor = 15  ' White

' Brush size
DIM brushSize AS INTEGER
brushSize = 3

' Drawing mode (1=draw, 0=move only)
DIM isDrawing AS INTEGER
isDrawing = 0

' Clear and draw UI
CLS
GOSUB DrawUI

' Main loop
DO
    ' Process all pending mouse events
    WHILE _MOUSEINPUT
        mx = _MOUSEX
        my = _MOUSEY
        lb = _MOUSEBUTTON(1)   ' Left button
        rb = _MOUSEBUTTON(2)   ' Right button

        ' Check if clicking on color palette (top area)
        IF my < 30 THEN
            IF lb THEN
                ' Select color from palette
                newColor = mx \ 40
                IF newColor >= 0 AND newColor <= 15 THEN
                    drawColor = newColor
                    GOSUB DrawUI
                END IF
            END IF
        ELSEIF my > 450 THEN
            ' Bottom toolbar
            IF lb THEN
                IF mx > 10 AND mx < 60 THEN
                    ' Clear button
                    LINE (0, 30)-(640, 450), 0, BF
                ELSEIF mx > 70 AND mx < 120 THEN
                    ' Brush size +
                    IF brushSize < 20 THEN brushSize = brushSize + 1
                    GOSUB DrawUI
                ELSEIF mx > 130 AND mx < 180 THEN
                    ' Brush size -
                    IF brushSize > 1 THEN brushSize = brushSize - 1
                    GOSUB DrawUI
                END IF
            END IF
        ELSE
            ' Drawing area
            IF lb THEN
                ' Draw with left mouse button
                CIRCLE (mx, my), brushSize, drawColor
                PAINT (mx, my), drawColor, drawColor
            ELSEIF rb THEN
                ' Erase with right mouse button
                CIRCLE (mx, my), brushSize, 0
                PAINT (mx, my), 0, 0
            END IF
        END IF
    WEND

    ' Display mouse position
    LOCATE 30, 50
    PRINT "X:"; mx; " Y:"; my; "   "

    _LIMIT 120   ' 120 FPS for smooth drawing

LOOP UNTIL _KEYHIT = 27

END

' ============================================================================
DrawUI:
    ' Draw color palette at top
    FOR c = 0 TO 15
        LINE (c * 40, 0)-(c * 40 + 38, 28), c, BF
        ' Highlight selected color
        IF c = drawColor THEN
            LINE (c * 40, 0)-(c * 40 + 38, 28), 15, B
            LINE (c * 40 + 1, 1)-(c * 40 + 37, 27), 15, B
        END IF
    NEXT c

    ' Draw toolbar at bottom
    LINE (0, 450)-(640, 480), 8, BF   ' Gray background

    ' Clear button
    LINE (10, 455)-(60, 475), 7, BF
    LINE (10, 455)-(60, 475), 15, B
    COLOR 0
    LOCATE 29, 3
    PRINT "CLR"

    ' Brush size controls
    LINE (70, 455)-(120, 475), 7, BF
    LINE (70, 455)-(120, 475), 15, B
    LOCATE 29, 11
    PRINT " + "

    LINE (130, 455)-(180, 475), 7, BF
    LINE (130, 455)-(180, 475), 15, B
    LOCATE 29, 18
    PRINT " - "

    ' Brush size display
    COLOR 15
    LOCATE 29, 26
    PRINT "Brush:"; brushSize; " "

    ' Instructions
    LOCATE 29, 40
    PRINT "LMB=Draw RMB=Erase ESC=Quit"

    COLOR drawColor
RETURN
