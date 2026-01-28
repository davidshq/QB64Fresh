'==============================================================================
' Extracted Section: Label_Type
' Source: qb64pe.bas lines 468-475
'==============================================================================
' NOTE: This is a raw extraction. Include this file AFTER setting up:
'   - Core includes (version, settings, constants)
'   - Required utility headers
'   - TYPE definitions this section depends on
'==============================================================================

TYPE Label_Type
    State AS _UNSIGNED _BYTE '0=label referenced, 1=label created
    cn AS STRING * 256
    Scope AS LONG
    Data_Offset AS _INTEGER64 'offset within data
    Data_Referenced AS _UNSIGNED _BYTE 'set to 1 if data is referenced (data_offset will be used to create the data offset variable)
    Error_Line AS LONG 'the line number to reference on errors
    Scope_Restriction AS LONG 'cannot exist inside this scope (post checked)
