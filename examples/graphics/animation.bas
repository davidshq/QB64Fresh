' ============================================================================
' Simple Animation
' ============================================================================
' Demonstrates: Animation loop, _LIMIT, double buffering concept
' ============================================================================

SCREEN 12   ' 640x480, 16 colors

' Ball properties
DIM ballX AS SINGLE, ballY AS SINGLE
DIM ballDX AS SINGLE, ballDY AS SINGLE
DIM ballRadius AS INTEGER

ballX = 320
ballY = 240
ballDX = 3.5
ballDY = 2.5
ballRadius = 20

' Animation loop
DO
    ' Clear the screen
    CLS

    ' Draw border
    LINE (10, 10)-(630, 470), 7, B

    ' Update ball position
    ballX = ballX + ballDX
    ballY = ballY + ballDY

    ' Bounce off walls
    IF ballX - ballRadius < 10 OR ballX + ballRadius > 630 THEN
        ballDX = -ballDX
        ballX = ballX + ballDX * 2  ' Prevent sticking
    END IF

    IF ballY - ballRadius < 10 OR ballY + ballRadius > 470 THEN
        ballDY = -ballDY
        ballY = ballY + ballDY * 2
    END IF

    ' Draw the ball (filled circle with outline)
    CIRCLE (ballX, ballY), ballRadius, 12       ' Red outline
    PAINT (ballX, ballY), 4, 12                 ' Red fill

    ' Draw ball highlight
    CIRCLE (ballX - 5, ballY - 5), 5, 15        ' White highlight

    ' Display info
    LOCATE 1, 1
    PRINT "Bouncing Ball Demo"
    LOCATE 2, 1
    PRINT "Position:"; INT(ballX); ","; INT(ballY)
    LOCATE 3, 1
    PRINT "Velocity:"; ballDX; ","; ballDY

    LOCATE 29, 1
    PRINT "Press ESC to exit"

    ' Limit to 60 FPS
    _LIMIT 60

    ' Check for ESC key
LOOP UNTIL _KEYHIT = 27

END
