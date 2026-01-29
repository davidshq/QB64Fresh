//! Graphics backend integration tests.
//!
//! These tests exercise the real SDL2 graphics backend. SDL2 can only be
//! initialized once per process, so tests are run serially via `#[serial]`.

#![cfg(feature = "graphics-sdl2")]

use qb64fresh_rt::{init_graphics, shutdown_graphics};
use serial_test::serial;

/// Initialize graphics, then shut down. Verifies SDL2 init/teardown without
/// conflicting with other tests (run serially).
#[test]
#[serial]
fn graphics_init_and_shutdown() {
    let r = init_graphics(320, 200);
    assert!(r.is_ok(), "init_graphics should succeed: {:?}", r.err());
    let r = shutdown_graphics();
    assert!(r.is_ok(), "shutdown_graphics should succeed: {:?}", r.err());
}

/// Second init after shutdown. Ensures we can init again after teardown.
#[test]
#[serial]
fn graphics_init_after_shutdown() {
    let _ = shutdown_graphics();
    let r = init_graphics(320, 200);
    assert!(
        r.is_ok(),
        "init_graphics after shutdown should succeed: {:?}",
        r.err()
    );
    let _ = shutdown_graphics();
}
