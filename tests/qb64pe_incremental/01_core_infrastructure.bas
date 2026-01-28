'==============================================================================
' Test: Core Infrastructure
' Purpose: Verify basic global includes compile correctly
' Expected: ~0.5s compilation time
'==============================================================================

' Minimal QB64pe setup
DEFLNG A-Z
$CONSOLE

' Core global includes (small, simple files)
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Minimal code to exercise the includes
DIM test AS INTEGER
test = 1

PRINT "Core infrastructure test"
PRINT "Version: "; Version$
