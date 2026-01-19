//! Audio abstraction layer for QB64Fresh.
//!
//! This module provides a trait-based audio backend system that allows
//! multiple audio implementations to be plugged in without changing
//! the compiler or generated code.
//!
//! # Architecture
//!
//! ```text
//! Compiled Program
//!      |
//! C FFI API (qb_snd_*, qb_sound, qb_play, qb_beep)
//!      |
//! AudioBackend Trait
//!      |
//! Concrete Implementation (RodioBackend, MockBackend, etc.)
//! ```
//!
//! # Design Principles
//!
//! 1. **Single Responsibility**: Only handles audio playback
//! 2. **Backend Agnostic**: Compiler generates C code that calls stable FFI
//! 3. **Pluggable**: New backends added without changing existing code
//! 4. **Testable**: Mock backend enables headless testing

mod error;
pub mod mock;

#[cfg(feature = "audio-rodio")]
pub mod rodio_backend;

pub use error::{AudioError, AudioErrorKind};

#[cfg(feature = "audio-rodio")]
pub use rodio_backend::RodioBackend;

/// Trait for audio playback backends.
///
/// Each backend handles sound loading, playback, and synthesis
/// using different underlying audio libraries.
pub trait AudioBackend {
    // ============================================================================
    // Initialization & Cleanup
    // ============================================================================

    /// Initialize the audio system.
    fn initialize(&mut self) -> Result<(), AudioError>;

    /// Shut down the audio system.
    fn shutdown(&mut self) -> Result<(), AudioError>;

    /// Check if audio is initialized.
    fn is_initialized(&self) -> bool;

    // ============================================================================
    // Classic BASIC Sound
    // ============================================================================

    /// Play a simple beep sound (like QB BEEP statement).
    fn beep(&mut self) -> Result<(), AudioError>;

    /// Play a tone at the specified frequency for a duration.
    ///
    /// # Arguments
    /// - `frequency`: Frequency in Hz
    /// - `duration`: Duration in clock ticks (18.2 ticks/second)
    fn sound(&mut self, frequency: f64, duration: f64) -> Result<(), AudioError>;

    /// Play music using Music Macro Language (MML).
    ///
    /// # Arguments
    /// - `commands`: MML command string (e.g., "O4 L4 C D E F G")
    fn play(&mut self, commands: &str) -> Result<(), AudioError>;

    // ============================================================================
    // QB64 Sound File Operations
    // ============================================================================

    /// Open a sound file and return a handle.
    ///
    /// # Arguments
    /// - `filename`: Path to the sound file
    ///
    /// # Returns
    /// Sound handle (positive for success, negative for error)
    fn snd_open(&mut self, filename: &str) -> i32;

    /// Close a sound handle and free resources.
    fn snd_close(&mut self, handle: i32) -> Result<(), AudioError>;

    /// Play a loaded sound.
    fn snd_play(&mut self, handle: i32) -> Result<(), AudioError>;

    /// Stop a playing sound.
    fn snd_stop(&mut self, handle: i32) -> Result<(), AudioError>;

    /// Pause a playing sound.
    fn snd_pause(&mut self, handle: i32) -> Result<(), AudioError>;

    /// Resume a paused sound.
    fn snd_resume(&mut self, handle: i32) -> Result<(), AudioError>;

    /// Play a sound in a continuous loop.
    fn snd_loop(&mut self, handle: i32) -> Result<(), AudioError>;

    /// Set the volume for a sound.
    ///
    /// # Arguments
    /// - `handle`: Sound handle
    /// - `volume`: Volume level (0.0 to 1.0)
    fn snd_vol(&mut self, handle: i32, volume: f64) -> Result<(), AudioError>;

    /// Set the stereo balance for a sound.
    ///
    /// # Arguments
    /// - `handle`: Sound handle
    /// - `balance`: Balance (-1.0 = left, 0.0 = center, 1.0 = right)
    fn snd_bal(&mut self, handle: i32, balance: f64) -> Result<(), AudioError>;

    /// Get the length of a sound in seconds.
    fn snd_len(&self, handle: i32) -> f64;

    /// Get the current playback position in seconds.
    fn snd_getpos(&self, handle: i32) -> f64;

    /// Set the playback position in seconds.
    fn snd_setpos(&mut self, handle: i32, position: f64) -> Result<(), AudioError>;

    /// Check if a sound is currently playing.
    fn snd_playing(&self, handle: i32) -> bool;

    /// Check if a sound is paused.
    fn snd_paused(&self, handle: i32) -> bool;

    /// Get the sample rate (typically 44100 or 48000).
    fn snd_rate(&self) -> i32 {
        48000 // Default sample rate
    }

    // ============================================================================
    // Raw Audio Synthesis
    // ============================================================================

    /// Open a raw audio stream for synthesis.
    ///
    /// # Returns
    /// Stream handle
    fn snd_openraw(&mut self) -> i32 {
        -1 // Default: not supported
    }

    /// Write a mono sample to the raw audio stream.
    ///
    /// # Arguments
    /// - `sample`: Sample value (-1.0 to 1.0)
    fn snd_raw(&mut self, sample: f64) -> Result<(), AudioError> {
        let _ = sample;
        Ok(()) // Default: no-op
    }

    /// Write stereo samples to the raw audio stream.
    ///
    /// # Arguments
    /// - `left`: Left channel sample (-1.0 to 1.0)
    /// - `right`: Right channel sample (-1.0 to 1.0)
    fn snd_raw_stereo(&mut self, left: f64, right: f64) -> Result<(), AudioError> {
        let _ = (left, right);
        Ok(()) // Default: no-op
    }

    /// Get the number of seconds of audio currently queued.
    fn snd_rawlen(&self) -> f64 {
        0.0 // Default: no queue
    }
}

/// Global audio backend instance.
///
/// This is used to bridge from C FFI code to the Rust implementation.
pub static mut AUDIO_BACKEND: Option<Box<dyn AudioBackend>> = None;

/// Initialize the global audio backend.
///
/// Uses RodioBackend when the `audio-rodio` feature is enabled,
/// otherwise falls back to MockAudioBackend.
pub fn init_audio() -> Result<(), AudioError> {
    #[cfg(feature = "audio-rodio")]
    {
        let mut backend = Box::new(RodioBackend::new());
        backend.initialize()?;
        unsafe {
            AUDIO_BACKEND = Some(backend);
        }
    }

    #[cfg(not(feature = "audio-rodio"))]
    {
        let mut backend = Box::new(mock::MockAudioBackend::new());
        backend.initialize()?;
        unsafe {
            AUDIO_BACKEND = Some(backend);
        }
    }

    Ok(())
}

/// Shut down the global audio backend.
pub fn shutdown_audio() -> Result<(), AudioError> {
    unsafe {
        if let Some(ref mut backend) = AUDIO_BACKEND {
            backend.shutdown()?;
            AUDIO_BACKEND = None;
        }
    }
    Ok(())
}
