//! Audio backend integration tests.
//!
//! These tests exercise the real Rodio audio backend. Audio backends may have
//! single-init constraints, so tests are run serially via `#[serial]`.

#![cfg(feature = "audio-rodio")]

use qb64fresh_rt::{init_audio, shutdown_audio};
use serial_test::serial;

/// Initialize audio, then shut down. Verifies rodio init/teardown without
/// conflicting with other tests (run serially).
#[test]
#[serial]
fn audio_init_and_shutdown() {
    let r = init_audio();
    assert!(r.is_ok(), "init_audio should succeed: {:?}", r.err());
    let r = shutdown_audio();
    assert!(r.is_ok(), "shutdown_audio should succeed: {:?}", r.err());
}

/// Second init after shutdown. Ensures we can init again after teardown.
#[test]
#[serial]
fn audio_init_after_shutdown() {
    let _ = shutdown_audio();
    let r = init_audio();
    assert!(
        r.is_ok(),
        "init_audio after shutdown should succeed: {:?}",
        r.err()
    );
    let _ = shutdown_audio();
}
