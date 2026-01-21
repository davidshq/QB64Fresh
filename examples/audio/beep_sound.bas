' ============================================================================
' BEEP and SOUND
' ============================================================================
' Demonstrates: BEEP, SOUND for generating tones
' ============================================================================

PRINT "=== BEEP and SOUND Demo ==="
PRINT

' --- BEEP ---
PRINT "Playing BEEP..."
BEEP
PRINT "  (default system beep)"
PRINT

SLEEP 1

' --- SOUND ---
PRINT "Playing SOUND tones..."
PRINT

' SOUND frequency, duration
' frequency = Hz (37-32767)
' duration = clock ticks (18.2 per second)

' Low tone
PRINT "  Low tone (200 Hz)..."
SOUND 200, 9    ' 200 Hz for 0.5 seconds

SLEEP 1

' Medium tone
PRINT "  Medium tone (440 Hz - A4 note)..."
SOUND 440, 9

SLEEP 1

' High tone
PRINT "  High tone (880 Hz - A5 note)..."
SOUND 880, 9

SLEEP 1

' --- Musical Scale ---
PRINT
PRINT "Playing C major scale..."

' Note frequencies (Hz)
CONST C4 = 262
CONST D4 = 294
CONST E4 = 330
CONST F4 = 349
CONST G4 = 392
CONST A4 = 440
CONST B4 = 494
CONST C5 = 523

DIM notes(7) AS INTEGER
notes(0) = C4
notes(1) = D4
notes(2) = E4
notes(3) = F4
notes(4) = G4
notes(5) = A4
notes(6) = B4
notes(7) = C5

FOR i = 0 TO 7
    SOUND notes(i), 5
NEXT i

PRINT "  Scale complete!"
PRINT

SLEEP 1

' --- Siren Effect ---
PRINT "Playing siren effect..."

FOR cycle = 1 TO 3
    ' Rising
    FOR freq = 300 TO 800 STEP 20
        SOUND freq, 1
    NEXT freq
    ' Falling
    FOR freq = 800 TO 300 STEP -20
        SOUND freq, 1
    NEXT freq
NEXT cycle

PRINT "  Siren complete!"
PRINT

' --- Ascending Tones ---
PRINT "Playing ascending tones..."
FOR freq = 100 TO 2000 STEP 100
    SOUND freq, 2
NEXT freq
PRINT "  Complete!"

PRINT
PRINT "Demo finished."
END
