' ============================================================================
' Snake Game
' ============================================================================
' Classic snake game demonstrating: arrays, keyboard input, game loop
' ============================================================================

SCREEN 12   ' 640x480, 16 colors

CONST GRID_SIZE = 20
CONST GRID_WIDTH = 30
CONST GRID_HEIGHT = 22
CONST OFFSET_X = 40
CONST OFFSET_Y = 30

' Snake data
DIM SHARED snakeX(500) AS INTEGER
DIM SHARED snakeY(500) AS INTEGER
DIM SHARED snakeLength AS INTEGER
DIM SHARED direction AS INTEGER   ' 0=right, 1=down, 2=left, 3=up

' Food position
DIM SHARED foodX AS INTEGER
DIM SHARED foodY AS INTEGER

' Game state
DIM SHARED score AS INTEGER
DIM SHARED gameOver AS INTEGER
DIM SHARED speed AS INTEGER

RANDOMIZE TIMER

' Initialize game
GOSUB InitGame

' Main game loop
DO
    ' Handle input
    k = _KEYHIT

    SELECT CASE k
        CASE 18432    ' Up arrow
            IF direction <> 1 THEN direction = 3
        CASE 20480    ' Down arrow
            IF direction <> 3 THEN direction = 1
        CASE 19200    ' Left arrow
            IF direction <> 0 THEN direction = 2
        CASE 19712    ' Right arrow
            IF direction <> 2 THEN direction = 0
        CASE 27       ' ESC
            gameOver = 1
    END SELECT

    ' Update game
    GOSUB UpdateSnake

    ' Draw
    GOSUB DrawGame

    ' Game speed (higher score = faster)
    _LIMIT speed

LOOP UNTIL gameOver

' Game over screen
COLOR 15
LOCATE 14, 25
PRINT "GAME OVER!"
LOCATE 16, 23
PRINT "Final Score:"; score
LOCATE 18, 20
PRINT "Press any key to exit..."
SLEEP

END

' ============================================================================
InitGame:
    ' Initialize snake in the middle
    snakeLength = 3
    snakeX(0) = GRID_WIDTH \ 2
    snakeY(0) = GRID_HEIGHT \ 2
    snakeX(1) = snakeX(0) - 1
    snakeY(1) = snakeY(0)
    snakeX(2) = snakeX(0) - 2
    snakeY(2) = snakeY(0)

    direction = 0   ' Moving right
    score = 0
    gameOver = 0
    speed = 8       ' Starting speed

    ' Place first food
    GOSUB PlaceFood
RETURN

' ============================================================================
PlaceFood:
    ' Place food at random location not on snake
    DIM valid AS INTEGER
    DO
        valid = 1
        foodX = INT(RND * GRID_WIDTH)
        foodY = INT(RND * GRID_HEIGHT)

        ' Check not on snake
        FOR i = 0 TO snakeLength - 1
            IF foodX = snakeX(i) AND foodY = snakeY(i) THEN
                valid = 0
                EXIT FOR
            END IF
        NEXT i
    LOOP UNTIL valid
RETURN

' ============================================================================
UpdateSnake:
    IF gameOver THEN RETURN

    ' Calculate new head position
    DIM newX AS INTEGER, newY AS INTEGER
    newX = snakeX(0)
    newY = snakeY(0)

    SELECT CASE direction
        CASE 0: newX = newX + 1  ' Right
        CASE 1: newY = newY + 1  ' Down
        CASE 2: newX = newX - 1  ' Left
        CASE 3: newY = newY - 1  ' Up
    END SELECT

    ' Check wall collision
    IF newX < 0 OR newX >= GRID_WIDTH OR newY < 0 OR newY >= GRID_HEIGHT THEN
        gameOver = 1
        RETURN
    END IF

    ' Check self collision
    FOR i = 0 TO snakeLength - 1
        IF newX = snakeX(i) AND newY = snakeY(i) THEN
            gameOver = 1
            RETURN
        END IF
    NEXT i

    ' Check food collision
    DIM ate AS INTEGER
    ate = 0
    IF newX = foodX AND newY = foodY THEN
        ate = 1
        score = score + 10
        ' Speed up slightly
        IF speed < 20 THEN speed = speed + 1
        GOSUB PlaceFood
    END IF

    ' Move snake body
    IF ate THEN
        snakeLength = snakeLength + 1
    END IF

    FOR i = snakeLength - 1 TO 1 STEP -1
        snakeX(i) = snakeX(i - 1)
        snakeY(i) = snakeY(i - 1)
    NEXT i

    ' Update head
    snakeX(0) = newX
    snakeY(0) = newY
RETURN

' ============================================================================
DrawGame:
    CLS

    ' Draw border
    LINE (OFFSET_X - 2, OFFSET_Y - 2)-(OFFSET_X + GRID_WIDTH * GRID_SIZE + 1, OFFSET_Y + GRID_HEIGHT * GRID_SIZE + 1), 7, B

    ' Draw food (red)
    DIM fx AS INTEGER, fy AS INTEGER
    fx = OFFSET_X + foodX * GRID_SIZE
    fy = OFFSET_Y + foodY * GRID_SIZE
    LINE (fx + 2, fy + 2)-(fx + GRID_SIZE - 3, fy + GRID_SIZE - 3), 12, BF

    ' Draw snake
    FOR i = 0 TO snakeLength - 1
        DIM sx AS INTEGER, sy AS INTEGER
        sx = OFFSET_X + snakeX(i) * GRID_SIZE
        sy = OFFSET_Y + snakeY(i) * GRID_SIZE

        IF i = 0 THEN
            ' Head (bright green)
            LINE (sx + 1, sy + 1)-(sx + GRID_SIZE - 2, sy + GRID_SIZE - 2), 10, BF
        ELSE
            ' Body (green)
            LINE (sx + 2, sy + 2)-(sx + GRID_SIZE - 3, sy + GRID_SIZE - 3), 2, BF
        END IF
    NEXT i

    ' Draw score
    COLOR 15
    LOCATE 1, 1
    PRINT "Score:"; score
    LOCATE 1, 60
    PRINT "ESC to quit"

    ' Draw controls hint
    LOCATE 30, 25
    PRINT "Use Arrow Keys to move"
RETURN
