$CONSOLE:ONLY
' Runtime comparison: _STRCMP, _STRICMP
PRINT _STRCMP("abc", "abc")
PRINT _STRCMP("abc", "abd")
PRINT _STRCMP("abd", "abc")
PRINT _STRICMP("ABC", "abc")
PRINT _STRICMP("aBc", "AbC")
END
