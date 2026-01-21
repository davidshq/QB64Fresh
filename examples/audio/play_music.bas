' ============================================================================
' PLAY Command (Music Macro Language)
' ============================================================================
' Demonstrates: PLAY statement with MML strings
' ============================================================================

PRINT "=== PLAY Command (MML) Demo ==="
PRINT
PRINT "The PLAY command uses Music Macro Language (MML)"
PRINT "to create music from string commands."
PRINT

' --- Basic Notes ---
PRINT "Playing basic notes (C D E F G A B)..."
PLAY "C D E F G A B"
PRINT

SLEEP 1

' --- Note Length ---
PRINT "Playing notes with different lengths..."
PRINT "  L4 = quarter note, L8 = eighth, L2 = half, L1 = whole"
PLAY "L4 C L8 D E L4 F L2 G"
PRINT

SLEEP 1

' --- Octaves ---
PRINT "Playing across octaves..."
PRINT "  O3 = low, O4 = middle, O5 = high"
PLAY "O3 C O4 C O5 C O4 C O3 C"
PRINT

SLEEP 1

' --- Tempo ---
PRINT "Playing at different tempos..."
PRINT "  T120 = normal, T60 = slow, T240 = fast"
PLAY "T120 L8 C D E F G A B > C"   ' > raises octave
SLEEP 1
PLAY "T240 L8 C D E F G A B > C"
PRINT

SLEEP 1

' --- Sharps and Flats ---
PRINT "Playing sharps (+) and flats (-)..."
PLAY "C C+ D D+ E F F+ G G+ A A+ B > C"
PRINT

SLEEP 1

' --- Rests ---
PRINT "Playing with rests (P = pause)..."
PLAY "C P8 E P8 G P4 > C"
PRINT

SLEEP 1

' --- Simple Songs ---
PRINT
PRINT "Playing 'Twinkle Twinkle Little Star'..."
PLAY "T120 L4"
PLAY "C C G G A A G2 F F E E D D C2"
PLAY "G G F F E E D2 G G F F E E D2"
PLAY "C C G G A A G2 F F E E D D C2"
PRINT

SLEEP 1

PRINT "Playing 'Mary Had a Little Lamb'..."
PLAY "T140 L4"
PLAY "E D C D E E E2 D D D2 E G G2"
PLAY "E D C D E E E E D D E D C1"
PRINT

SLEEP 1

PRINT "Playing 'Frere Jacques' (Are You Sleeping)..."
PLAY "T120 L4"
PLAY "C D E C C D E C"           ' Frere Jacques
PLAY "E F G2 E F G2"             ' Dormez vous
PLAY "L8 G A G F L4 E C"         ' Sonnez les matines
PLAY "L8 G A G F L4 E C"
PLAY "L4 C O3 G O4 C2"           ' Din dan don
PLAY "C O3 G O4 C2"
PRINT

PRINT
PRINT "MML Reference:"
PRINT "  A-G  = Notes"
PRINT "  + -  = Sharp/Flat"
PRINT "  O    = Octave (1-7)"
PRINT "  > <  = Octave up/down"
PRINT "  L    = Length (1=whole, 4=quarter, 8=eighth)"
PRINT "  T    = Tempo (32-255)"
PRINT "  P    = Pause/Rest"
PRINT

PRINT "Demo complete."
END
