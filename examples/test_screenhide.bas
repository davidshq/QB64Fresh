' Test $SCREENHIDE directive
' This program should hide the window on startup

$SCREENHIDE
SCREEN 12

PRINT "Window should be hidden"
PRINT "Press any key to show window..."
SLEEP

_SCREENSHOW
PRINT "Window is now visible"
PRINT "Press any key to hide window again..."
SLEEP

_SCREENHIDE
PRINT "Window is hidden again"
PRINT "Press any key to exit..."
SLEEP
