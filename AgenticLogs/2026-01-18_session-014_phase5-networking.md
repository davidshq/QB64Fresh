# Session 014: Phase 5 - Networking

**Date:** 2026-01-18
**Focus:** QB64 Networking Extensions

## Session Goals

Implement QB64 networking functions:
- `_OPENHOST(port)` - Open TCP server on port
- `_OPENCONNECTION(host_handle)` - Accept client connection
- `_OPENCLIENT("TCP/IP:port:address")` - Connect to TCP server
- `_CONNECTED(handle)` - Check connection status
- Network stream I/O (via existing file I/O with handles)

## Implementation Progress

| Feature | Lexer | Semantic | Parser | Codegen | Runtime | Status |
|---------|-------|----------|--------|---------|---------|--------|
| _OPENHOST | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |
| _OPENCONNECTION | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |
| _OPENCLIENT | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |
| _CONNECTED | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |

## Technical Design

### QB64 Networking Model

QB64 uses a handle-based networking model:
1. Server creates a host with `_OPENHOST(port)` → returns host handle
2. Server accepts connections with `_OPENCONNECTION(host)` → returns connection handle
3. Client connects with `_OPENCLIENT("TCP/IP:port:address")` → returns connection handle
4. Both use `_CONNECTED(handle)` to check if connection is active
5. Data transfer uses `GET #handle` and `PUT #handle` (existing file I/O)

### Implementation Approach

Runtime will use Rust's `std::net` for cross-platform TCP:
- `TcpListener` for server hosts
- `TcpStream` for connections
- Non-blocking mode for `_OPENCONNECTION` to avoid blocking the program

## Session Notes

### Files Modified

1. **src/lexer/token.rs** - Added `OpenHost`, `OpenConnection`, `OpenClient`, `Connected` tokens
2. **src/semantic/mod.rs** - Registered networking built-in functions with signatures
3. **src/parser/expressions.rs** - Added token handling in `parse_prefix` for networking functions
4. **src/codegen/c_backend/expr.rs** - Added function name mappings (e.g., `_OPENHOST` → `qb_net_openhost`)
5. **runtime/src/io.rs** - Complete networking runtime implementation

### Runtime Implementation Details

The networking runtime uses Rust's `std::net` module:

```rust
enum NetHandle {
    Host(TcpListener),
    Connection(TcpStream),
}
```

Key design decisions:
- **Negative handles**: Network handles are negative to distinguish from file handles
- **Non-blocking accept**: `_OPENCONNECTION` uses non-blocking mode to prevent blocking
- **Connection string parsing**: `_OPENCLIENT` parses QB64 format `"TCP/IP:port:address"`
- **Global handle storage**: Thread-safe `Mutex<HashMap<i64, NetHandle>>` for handle management

### Example Created

`examples/networking_demo.bas` - Demonstrates:
- Starting a TCP server with `_OPENHOST`
- Accepting connections with `_OPENCONNECTION`
- Client connection with `_OPENCLIENT`
- Connection status checking with `_CONNECTED`

## Outstanding Work

- Network stream I/O (`PUT #handle` / `GET #handle`) requires file I/O integration
