'==============================================================================
' Extracted Section: ids_init
' Source: qb64pe.bas lines 644-656
'==============================================================================
' NOTE: This is a raw extraction. Include this file AFTER setting up:
'   - Core includes (version, settings, constants)
'   - Required utility headers
'   - TYPE definitions this section depends on
'==============================================================================

DIM SHARED id AS idstruct

DIM SHARED idn AS LONG
DIM SHARED ids_max AS LONG
ids_max = 1024
REDIM SHARED ids(1 TO ids_max) AS idstruct
REDIM SHARED cmemlist(1 TO ids_max + 1) AS INTEGER 'variables that must be in cmem
REDIM SHARED sfcmemargs(1 TO ids_max + 1) AS STRING * 100 's/f arg that must be in cmem
REDIM SHARED arrayelementslist(1 TO ids_max + 1) AS INTEGER 'arrayelementslist (like cmemlist) helps to resolve the number of elements in arrays with an unknown number of elements. Note: arrays with an unknown number of elements have .arrayelements=-1


'create blank id template for idclear to copy (stops strings being set to chr$(0))
DIM SHARED cleariddata AS idstruct
