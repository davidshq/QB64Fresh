' ============================================================================
' Networking Demo for QB64Fresh
' ============================================================================
' This demonstrates the QB64-compatible networking functions:
' - _OPENHOST(port)       - Start a TCP server on a port
' - _OPENCONNECTION(host) - Accept incoming connection (non-blocking)
' - _OPENCLIENT(string)   - Connect to a TCP server
' - _CONNECTED(handle)    - Check if connection is still active
'
' QB64 networking uses a handle-based model where:
' - Positive handles are for internal use (reserved for file handles)
' - Negative handles are returned for network connections
' - A handle of 0 means the operation failed
' ============================================================================

PRINT "=== QB64Fresh Networking Demo ==="
PRINT

' Demo 1: Check how _OPENHOST works (attempt to open a server)
PRINT "Attempting to start a server on port 12345..."
serverHandle = _OPENHOST(12345)

IF serverHandle <> 0 THEN
    PRINT "Server started successfully! Handle:"; serverHandle

    ' In a real application, you would loop checking for connections:
    ' DO
    '     clientHandle = _OPENCONNECTION(serverHandle)
    '     IF clientHandle <> 0 THEN
    '         PRINT "Client connected!"
    '         ' ... handle the connection ...
    '     END IF
    '     _LIMIT 60
    ' LOOP

    PRINT "Server is listening... (demo will exit)"
ELSE
    PRINT "Could not start server (port may be in use)"
END IF

PRINT

' Demo 2: Client connection (this will fail since we have no target server)
' In QB64, the connection string format is: "TCP/IP:port:address"
PRINT "Attempting to connect to localhost:12346..."
clientHandle = _OPENCLIENT("TCP/IP:12346:127.0.0.1")

IF clientHandle <> 0 THEN
    PRINT "Connected! Handle:"; clientHandle

    ' Check if still connected
    IF _CONNECTED(clientHandle) THEN
        PRINT "Connection is active"
    ELSE
        PRINT "Connection closed"
    END IF
ELSE
    PRINT "Could not connect (expected - no server running on 12346)"
END IF

PRINT
PRINT "=== Networking Demo Complete ==="
