' ============================================================================
' Number Guessing Game
' ============================================================================
' A simple console game demonstrating: RND, INPUT, loops, conditionals
' ============================================================================

RANDOMIZE TIMER   ' Seed random number generator

DIM secretNumber AS INTEGER
DIM guess AS INTEGER
DIM attempts AS INTEGER
DIM maxNumber AS INTEGER
DIM playAgain AS STRING

CLS
PRINT "========================================"
PRINT "       NUMBER GUESSING GAME"
PRINT "========================================"
PRINT

DO
    ' Set difficulty
    PRINT "Choose difficulty:"
    PRINT "  1. Easy   (1-50,  10 attempts)"
    PRINT "  2. Medium (1-100, 7 attempts)"
    PRINT "  3. Hard   (1-200, 6 attempts)"
    PRINT

    DIM difficulty AS INTEGER
    INPUT "Enter choice (1-3): ", difficulty

    SELECT CASE difficulty
        CASE 1
            maxNumber = 50
            attempts = 10
        CASE 2
            maxNumber = 100
            attempts = 7
        CASE 3
            maxNumber = 200
            attempts = 6
        CASE ELSE
            maxNumber = 100
            attempts = 7
    END SELECT

    ' Generate secret number
    secretNumber = INT(RND * maxNumber) + 1

    PRINT
    PRINT "I'm thinking of a number between 1 and"; maxNumber
    PRINT "You have"; attempts; "attempts to guess it."
    PRINT

    DIM won AS INTEGER
    won = 0

    ' Game loop
    DO WHILE attempts > 0 AND won = 0
        PRINT "Attempts remaining:"; attempts
        INPUT "Your guess: ", guess

        IF guess < 1 OR guess > maxNumber THEN
            PRINT "Please enter a number between 1 and"; maxNumber
        ELSEIF guess < secretNumber THEN
            PRINT "Too LOW! Try higher."
            PRINT
            attempts = attempts - 1
        ELSEIF guess > secretNumber THEN
            PRINT "Too HIGH! Try lower."
            PRINT
            attempts = attempts - 1
        ELSE
            won = 1
        END IF
    LOOP

    ' Result
    PRINT
    IF won THEN
        PRINT "=========================================="
        PRINT " CONGRATULATIONS! You guessed it!"
        PRINT " The number was"; secretNumber
        PRINT " Attempts used:"; (10 - attempts) + 1
        PRINT "=========================================="
    ELSE
        PRINT "=========================================="
        PRINT " GAME OVER!"
        PRINT " The secret number was"; secretNumber
        PRINT "=========================================="
    END IF

    PRINT
    INPUT "Play again? (Y/N): ", playAgain
    PRINT

LOOP WHILE UCASE$(playAgain) = "Y"

PRINT "Thanks for playing!"
END
