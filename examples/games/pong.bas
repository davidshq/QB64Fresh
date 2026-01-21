' ============================================================================
' Pong - Two Player Game
' ============================================================================
' Classic Pong demonstrating: game loop, collision detection, two-player input
' ============================================================================

SCREEN 12   ' 640x480, 16 colors

' Game constants
CONST SCREEN_W = 640
CONST SCREEN_H = 480
CONST PADDLE_W = 15
CONST PADDLE_H = 80
CONST BALL_SIZE = 12
CONST PADDLE_SPEED = 8
CONST WIN_SCORE = 5

' Paddle positions
DIM p1Y AS SINGLE    ' Player 1 (left)
DIM p2Y AS SINGLE    ' Player 2 (right)

' Ball position and velocity
DIM ballX AS SINGLE, ballY AS SINGLE
DIM ballDX AS SINGLE, ballDY AS SINGLE

' Scores
DIM score1 AS INTEGER, score2 AS INTEGER

' Game state
DIM gameRunning AS INTEGER

' Initialize
GOSUB ResetGame

' Main loop
DO
    ' Handle input
    ' Player 1: W/S keys
    IF _KEYDOWN(ASC("w")) OR _KEYDOWN(ASC("W")) THEN
        p1Y = p1Y - PADDLE_SPEED
    END IF
    IF _KEYDOWN(ASC("s")) OR _KEYDOWN(ASC("S")) THEN
        p1Y = p1Y + PADDLE_SPEED
    END IF

    ' Player 2: Up/Down arrow keys
    IF _KEYDOWN(18432) THEN  ' Up arrow
        p2Y = p2Y - PADDLE_SPEED
    END IF
    IF _KEYDOWN(20480) THEN  ' Down arrow
        p2Y = p2Y + PADDLE_SPEED
    END IF

    ' Space to start/restart
    IF _KEYHIT = 32 THEN
        IF NOT gameRunning THEN
            gameRunning = -1
        END IF
    END IF

    ' ESC to quit
    IF _KEYHIT = 27 THEN EXIT DO

    ' Keep paddles on screen
    IF p1Y < 0 THEN p1Y = 0
    IF p1Y > SCREEN_H - PADDLE_H THEN p1Y = SCREEN_H - PADDLE_H
    IF p2Y < 0 THEN p2Y = 0
    IF p2Y > SCREEN_H - PADDLE_H THEN p2Y = SCREEN_H - PADDLE_H

    ' Update ball if game is running
    IF gameRunning THEN
        GOSUB UpdateBall
    END IF

    ' Draw everything
    GOSUB DrawGame

    ' Check for winner
    IF score1 >= WIN_SCORE OR score2 >= WIN_SCORE THEN
        GOSUB ShowWinner
        GOSUB ResetGame
    END IF

    _LIMIT 60

LOOP

END

' ============================================================================
ResetGame:
    p1Y = SCREEN_H / 2 - PADDLE_H / 2
    p2Y = SCREEN_H / 2 - PADDLE_H / 2
    score1 = 0
    score2 = 0
    gameRunning = 0
    GOSUB ResetBall
RETURN

' ============================================================================
ResetBall:
    ballX = SCREEN_W / 2
    ballY = SCREEN_H / 2

    ' Random direction
    IF RND > 0.5 THEN
        ballDX = 5
    ELSE
        ballDX = -5
    END IF
    ballDY = (RND - 0.5) * 6
RETURN

' ============================================================================
UpdateBall:
    ' Move ball
    ballX = ballX + ballDX
    ballY = ballY + ballDY

    ' Bounce off top/bottom
    IF ballY < 0 THEN
        ballY = 0
        ballDY = -ballDY
    END IF
    IF ballY > SCREEN_H - BALL_SIZE THEN
        ballY = SCREEN_H - BALL_SIZE
        ballDY = -ballDY
    END IF

    ' Check paddle collisions
    ' Player 1 paddle (left side)
    IF ballX < 30 + PADDLE_W THEN
        IF ballY + BALL_SIZE > p1Y AND ballY < p1Y + PADDLE_H THEN
            IF ballX > 30 THEN
                ballX = 30 + PADDLE_W
                ballDX = -ballDX * 1.05   ' Speed up slightly
                ' Add spin based on where ball hit paddle
                hitPos = (ballY + BALL_SIZE / 2 - p1Y) / PADDLE_H
                ballDY = (hitPos - 0.5) * 10
            END IF
        END IF
    END IF

    ' Player 2 paddle (right side)
    IF ballX + BALL_SIZE > SCREEN_W - 30 - PADDLE_W THEN
        IF ballY + BALL_SIZE > p2Y AND ballY < p2Y + PADDLE_H THEN
            IF ballX < SCREEN_W - 30 THEN
                ballX = SCREEN_W - 30 - PADDLE_W - BALL_SIZE
                ballDX = -ballDX * 1.05
                hitPos = (ballY + BALL_SIZE / 2 - p2Y) / PADDLE_H
                ballDY = (hitPos - 0.5) * 10
            END IF
        END IF
    END IF

    ' Cap ball speed
    IF ABS(ballDX) > 15 THEN ballDX = SGN(ballDX) * 15
    IF ABS(ballDY) > 12 THEN ballDY = SGN(ballDY) * 12

    ' Score points
    IF ballX < 0 THEN
        score2 = score2 + 1
        GOSUB ResetBall
    END IF
    IF ballX > SCREEN_W THEN
        score1 = score1 + 1
        GOSUB ResetBall
    END IF
RETURN

' ============================================================================
DrawGame:
    CLS

    ' Draw center line
    FOR y = 0 TO SCREEN_H STEP 20
        LINE (SCREEN_W / 2 - 2, y)-(SCREEN_W / 2 + 2, y + 10), 8, BF
    NEXT y

    ' Draw paddles
    LINE (30, p1Y)-(30 + PADDLE_W, p1Y + PADDLE_H), 15, BF
    LINE (SCREEN_W - 30 - PADDLE_W, p2Y)-(SCREEN_W - 30, p2Y + PADDLE_H), 15, BF

    ' Draw ball
    LINE (ballX, ballY)-(ballX + BALL_SIZE, ballY + BALL_SIZE), 14, BF

    ' Draw scores
    COLOR 15
    LOCATE 2, 18
    PRINT score1
    LOCATE 2, 62
    PRINT score2

    ' Player labels
    LOCATE 3, 5
    PRINT "Player 1"
    LOCATE 3, 70
    PRINT "Player 2"

    ' Controls
    LOCATE 29, 5
    PRINT "W/S keys"
    LOCATE 29, 65
    PRINT "Arrow keys"

    ' Instructions
    IF NOT gameRunning THEN
        LOCATE 15, 25
        PRINT "Press SPACE to start"
    END IF

    LOCATE 1, 30
    PRINT "First to"; WIN_SCORE; "wins!"
RETURN

' ============================================================================
ShowWinner:
    CLS
    COLOR 14
    IF score1 >= WIN_SCORE THEN
        LOCATE 12, 25
        PRINT "PLAYER 1 WINS!"
    ELSE
        LOCATE 12, 25
        PRINT "PLAYER 2 WINS!"
    END IF

    COLOR 15
    LOCATE 14, 20
    PRINT "Final Score:"; score1; "-"; score2

    LOCATE 18, 18
    PRINT "Press SPACE to play again"
    LOCATE 19, 22
    PRINT "Press ESC to quit"

    DO
        k = _KEYHIT
        IF k = 32 THEN EXIT DO       ' Space
        IF k = 27 THEN END           ' ESC
        _LIMIT 30
    LOOP
RETURN
