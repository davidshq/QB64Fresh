' ============================================================================
' Sound File Playback
' ============================================================================
' Demonstrates: _SNDOPEN, _SNDPLAY, _SNDSTOP, _SNDLOOP, _SNDVOL
' ============================================================================

PRINT "=== Sound File Playback Demo ==="
PRINT
PRINT "This demo shows how to load and play sound files."
PRINT "Supported formats: WAV, OGG, MP3, FLAC"
PRINT

' Note: You need actual sound files to test this
' Create simple test files or use your own

DIM soundFile AS STRING
DIM soundHandle AS LONG

' --- Loading a Sound File ---
PRINT "Loading sound file..."
PRINT

' Try to load a sound file (change path as needed)
soundFile = "test_sound.wav"

soundHandle = _SNDOPEN(soundFile)

IF soundHandle = 0 THEN
    PRINT "Could not load '"; soundFile; "'"
    PRINT "Note: You need a valid sound file to test playback."
    PRINT
    PRINT "To test this demo:"
    PRINT "  1. Place a WAV file named 'test_sound.wav' in the same directory"
    PRINT "  2. Or modify the soundFile variable to point to your sound file"
    PRINT
    PRINT "Demonstrating API without actual playback..."
    PRINT
    GOTO DemoAPI
END IF

PRINT "Sound loaded! Handle:"; soundHandle
PRINT

' --- Playing Sound ---
PRINT "Press 1 to play sound"
PRINT "Press 2 to play looped"
PRINT "Press 3 to stop"
PRINT "Press 4 to set volume 50%"
PRINT "Press 5 to set volume 100%"
PRINT "Press ESC to exit"
PRINT

DO
    k = _KEYHIT

    SELECT CASE k
        CASE ASC("1")
            PRINT "Playing..."
            _SNDPLAY soundHandle

        CASE ASC("2")
            PRINT "Playing looped..."
            _SNDLOOP soundHandle

        CASE ASC("3")
            PRINT "Stopping..."
            _SNDSTOP soundHandle

        CASE ASC("4")
            PRINT "Volume: 50%"
            _SNDVOL soundHandle, 0.5

        CASE ASC("5")
            PRINT "Volume: 100%"
            _SNDVOL soundHandle, 1.0

        CASE 27
            EXIT DO
    END SELECT

    _LIMIT 30
LOOP

' Clean up
_SNDCLOSE soundHandle
PRINT
PRINT "Sound file closed."
GOTO TheEnd

' --- API Demo (no file) ---
DemoAPI:
PRINT "Sound File API Reference:"
PRINT "========================="
PRINT
PRINT "Loading:"
PRINT "  handle = _SNDOPEN(filename$)"
PRINT "  ' Returns handle > 0 on success, 0 on failure"
PRINT
PRINT "Playback:"
PRINT "  _SNDPLAY handle       ' Play once"
PRINT "  _SNDLOOP handle       ' Play looped"
PRINT "  _SNDSTOP handle       ' Stop playback"
PRINT "  _SNDPAUSE handle      ' Pause playback"
PRINT
PRINT "Volume:"
PRINT "  _SNDVOL handle, volume!   ' 0.0 to 1.0"
PRINT
PRINT "Position:"
PRINT "  _SNDSETPOS handle, seconds!"
PRINT "  pos! = _SNDGETPOS(handle)"
PRINT "  len! = _SNDLEN(handle)"
PRINT
PRINT "Status:"
PRINT "  playing = _SNDPLAYING(handle)  ' -1 if playing"
PRINT "  paused = _SNDPAUSED(handle)    ' -1 if paused"
PRINT
PRINT "Cleanup:"
PRINT "  _SNDCLOSE handle"
PRINT

TheEnd:
PRINT "Demo complete."
END
