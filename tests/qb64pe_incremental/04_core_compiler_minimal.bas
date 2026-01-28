'==============================================================================
' Test: Core Compiler - Minimal Infrastructure
' Purpose: Test minimal compiler infrastructure needed for built-in functions
' Expected: ~2s compilation time
'==============================================================================
' This test includes the minimal infrastructure needed to test built-in
' function registration. It extracts key sections from qb64pe.bas:
'   - idstruct TYPE definition
'   - clearid SUB
'   - regid SUB
'   - Basic initialization
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Utility headers
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bi'
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bi'
$INCLUDE:'../../../QB64pe/source/utilities/statevars.bi'
$INCLUDE:'../../../QB64pe/source/utilities/type.bi'

' Hash table (needed for symbol table)
$INCLUDE:'../../../QB64pe/source/utilities/hash.bi'
$INCLUDE:'../../../QB64pe/source/utilities/hash.bas'

' Elements utility (needed for const_eval)
$INCLUDE:'../../../QB64pe/source/utilities/elements.bas'

' Constant evaluation (needed for built-in functions)
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bas'

' NOTE: The following sections need to be extracted from qb64pe.bas:
'   - idstruct TYPE (lines 596-642)
'   - clearid SUB (line 14476+)
'   - regid SUB (line 21849+)
'   - ids() array initialization
'
' For now, this is a placeholder. Use extract-qb64pe-section.sh to extract
' these sections, then include them here.

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Core compiler minimal infrastructure test"
PRINT "NOTE: This test needs idstruct, clearid, and regid to be extracted"
