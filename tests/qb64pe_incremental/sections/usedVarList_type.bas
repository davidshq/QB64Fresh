'==============================================================================
' Extracted Section: usedVarList_type
' Source: qb64pe.bas lines 181-188
'==============================================================================
' NOTE: This is a raw extraction. Include this file AFTER setting up:
'   - Core includes (version, settings, constants)
'   - Required utility headers
'   - TYPE definitions this section depends on
'==============================================================================

TYPE usedVarList
    AS LONG id, linenumber, includeLevel, includedLine, scope, localIndex
    AS LONG arrayElementSize
    AS _BYTE used, watch, isarray, displayFormat 'displayFormat: 0=DEC;1=HEX;2=BIN;3=OCT
    AS STRING name, cname, varType, includedFile, subfunc
    AS STRING watchRange, indexes, elements, elementTypes 'for Arrays and UDTs
    AS STRING elementOffset, storage
END TYPE
