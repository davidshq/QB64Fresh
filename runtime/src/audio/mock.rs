//! Mock audio backend for testing without actual audio hardware.
//!
//! This backend silently accepts all audio operations without producing
//! any actual sound. Useful for:
//! - Headless testing
//! - CI environments without audio devices
//! - Debugging audio logic without sound

use super::{AudioBackend, AudioError};
use std::collections::HashMap;

/// Sound state for mock backend.
#[derive(Debug, Clone)]
struct MockSound {
    /// Whether the sound is currently playing.
    playing: bool,
    /// Whether the sound is paused.
    paused: bool,
    /// Volume level (0.0 to 1.0).
    volume: f64,
    /// Stereo balance (-1.0 to 1.0).
    balance: f64,
    /// Sound duration in seconds.
    length: f64,
    /// Current playback position.
    position: f64,
    /// Whether the sound is looping.
    looping: bool,
}

impl Default for MockSound {
    fn default() -> Self {
        Self {
            playing: false,
            paused: false,
            volume: 1.0,
            balance: 0.0,
            length: 1.0, // Default 1 second
            position: 0.0,
            looping: false,
        }
    }
}

/// Mock audio backend that accepts operations without producing sound.
#[derive(Debug)]
pub struct MockAudioBackend {
    /// Whether initialized.
    initialized: bool,
    /// Loaded sounds.
    sounds: HashMap<i32, MockSound>,
    /// Next available handle.
    next_handle: i32,
    /// MIDI sound bank (soundfont) path for _MIDISOUNDBANK (stored for tests).
    midi_sound_bank_path: Option<String>,
}

impl MockAudioBackend {
    /// Create a new mock audio backend.
    pub fn new() -> Self {
        Self {
            initialized: false,
            sounds: HashMap::new(),
            next_handle: 1,
            midi_sound_bank_path: None,
        }
    }
}

impl Default for MockAudioBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl AudioBackend for MockAudioBackend {
    fn initialize(&mut self) -> Result<(), AudioError> {
        if self.initialized {
            return Err(AudioError::already_initialized());
        }
        self.initialized = true;
        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), AudioError> {
        self.initialized = false;
        self.sounds.clear();
        self.next_handle = 1;
        Ok(())
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn beep(&mut self) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        // Mock: do nothing
        Ok(())
    }

    fn sound(&mut self, _frequency: f64, _duration: f64) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        // Mock: do nothing
        Ok(())
    }

    fn play(&mut self, _commands: &str) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        // Mock: do nothing (would parse MML in a real backend)
        Ok(())
    }

    fn snd_open(&mut self, _filename: &str) -> i32 {
        if !self.initialized {
            return -1;
        }

        // Create a mock sound
        let handle = self.next_handle;
        self.next_handle += 1;
        self.sounds.insert(handle, MockSound::default());
        handle
    }

    fn snd_close(&mut self, handle: i32) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if self.sounds.remove(&handle).is_none() {
            return Err(AudioError::invalid_handle(handle));
        }
        Ok(())
    }

    fn snd_play(&mut self, handle: i32) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.playing = true;
            sound.paused = false;
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_stop(&mut self, handle: i32) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.playing = false;
            sound.paused = false;
            sound.position = 0.0;
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_pause(&mut self, handle: i32) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            if sound.playing {
                sound.paused = true;
                sound.playing = false;
            }
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_resume(&mut self, handle: i32) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            if sound.paused {
                sound.paused = false;
                sound.playing = true;
            }
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_loop(&mut self, handle: i32) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.looping = true;
            sound.playing = true;
            sound.paused = false;
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_vol(&mut self, handle: i32, volume: f64) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.volume = volume.clamp(0.0, 1.0);
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_bal(&mut self, handle: i32, balance: f64) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.balance = balance.clamp(-1.0, 1.0);
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_len(&self, handle: i32) -> f64 {
        self.sounds.get(&handle).map(|s| s.length).unwrap_or(0.0)
    }

    fn snd_getpos(&self, handle: i32) -> f64 {
        self.sounds.get(&handle).map(|s| s.position).unwrap_or(0.0)
    }

    fn snd_setpos(&mut self, handle: i32, position: f64) -> Result<(), AudioError> {
        if !self.initialized {
            return Err(AudioError::not_initialized());
        }
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.position = position.max(0.0).min(sound.length);
            Ok(())
        } else {
            Err(AudioError::invalid_handle(handle))
        }
    }

    fn snd_playing(&self, handle: i32) -> bool {
        self.sounds.get(&handle).map(|s| s.playing).unwrap_or(false)
    }

    fn snd_paused(&self, handle: i32) -> bool {
        self.sounds.get(&handle).map(|s| s.paused).unwrap_or(false)
    }

    fn set_midi_sound_bank_path(&mut self, path: Option<&str>) {
        self.midi_sound_bank_path = path.map(String::from);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_backend_lifecycle() {
        let mut backend = MockAudioBackend::new();
        assert!(!backend.is_initialized());

        backend.initialize().unwrap();
        assert!(backend.is_initialized());

        backend.shutdown().unwrap();
        assert!(!backend.is_initialized());
    }

    #[test]
    fn test_mock_sound_operations() {
        let mut backend = MockAudioBackend::new();
        backend.initialize().unwrap();

        // Open a sound
        let handle = backend.snd_open("test.wav");
        assert!(handle > 0);

        // Play/pause/stop
        backend.snd_play(handle).unwrap();
        assert!(backend.snd_playing(handle));

        backend.snd_pause(handle).unwrap();
        assert!(backend.snd_paused(handle));
        assert!(!backend.snd_playing(handle));

        backend.snd_resume(handle).unwrap();
        assert!(backend.snd_playing(handle));

        backend.snd_stop(handle).unwrap();
        assert!(!backend.snd_playing(handle));

        // Close
        backend.snd_close(handle).unwrap();
    }

    #[test]
    fn test_mock_volume_and_balance() {
        let mut backend = MockAudioBackend::new();
        backend.initialize().unwrap();

        let handle = backend.snd_open("test.wav");

        backend.snd_vol(handle, 0.5).unwrap();
        backend.snd_bal(handle, -0.5).unwrap();

        // Volume clamping
        backend.snd_vol(handle, 2.0).unwrap(); // Should clamp to 1.0
        backend.snd_vol(handle, -1.0).unwrap(); // Should clamp to 0.0

        backend.snd_close(handle).unwrap();
    }

    #[test]
    fn test_mock_error_handling() {
        let mut backend = MockAudioBackend::new();

        // Operations on uninitialized backend should fail
        assert!(backend.beep().is_err());
        assert!(backend.sound(440.0, 1.0).is_err());
        assert!(backend.play("O4C").is_err());

        // snd_open returns -1 when not initialized
        assert_eq!(backend.snd_open("test.wav"), -1);

        backend.initialize().unwrap();

        // Double initialization should fail
        assert!(backend.initialize().is_err());

        // Invalid handle operations should fail
        assert!(backend.snd_close(999).is_err());
        assert!(backend.snd_play(999).is_err());
        assert!(backend.snd_stop(999).is_err());
        assert!(backend.snd_vol(999, 0.5).is_err());
    }

    #[test]
    fn test_mock_basic_sound_operations() {
        let mut backend = MockAudioBackend::new();
        backend.initialize().unwrap();

        // BEEP should succeed
        assert!(backend.beep().is_ok());

        // SOUND should succeed
        assert!(backend.sound(440.0, 0.5).is_ok());

        // PLAY (MML) should succeed
        assert!(backend.play("O4L4CDEFGAB").is_ok());
    }

    #[test]
    fn test_mock_multiple_sounds() {
        let mut backend = MockAudioBackend::new();
        backend.initialize().unwrap();

        // Open multiple sounds
        let h1 = backend.snd_open("sound1.wav");
        let h2 = backend.snd_open("sound2.wav");
        let h3 = backend.snd_open("sound3.wav");

        assert!(h1 > 0);
        assert!(h2 > 0);
        assert!(h3 > 0);
        assert_ne!(h1, h2);
        assert_ne!(h2, h3);

        // Play all
        backend.snd_play(h1).unwrap();
        backend.snd_play(h2).unwrap();
        backend.snd_play(h3).unwrap();

        assert!(backend.snd_playing(h1));
        assert!(backend.snd_playing(h2));
        assert!(backend.snd_playing(h3));

        // Close first, others should still work
        backend.snd_close(h1).unwrap();
        assert!(backend.snd_playing(h2));
        assert!(backend.snd_playing(h3));

        // Shutdown clears all sounds
        backend.shutdown().unwrap();
        assert!(!backend.snd_playing(h2));
        assert!(!backend.snd_playing(h3));
    }

    #[test]
    fn test_mock_looping() {
        let mut backend = MockAudioBackend::new();
        backend.initialize().unwrap();

        let handle = backend.snd_open("loop.wav");
        // snd_loop starts looped playback
        backend.snd_loop(handle).unwrap();
        assert!(backend.snd_playing(handle));

        backend.snd_close(handle).unwrap();
    }

    #[test]
    fn test_mock_query_functions() {
        let mut backend = MockAudioBackend::new();
        backend.initialize().unwrap();

        let handle = backend.snd_open("test.wav");

        // Query functions return defaults for mock
        assert_eq!(backend.snd_len(handle), 1.0); // Default length
        assert_eq!(backend.snd_getpos(handle), 0.0); // Default position

        // Invalid handle queries return 0
        assert_eq!(backend.snd_len(999), 0.0);
        assert_eq!(backend.snd_getpos(999), 0.0);
        assert!(!backend.snd_playing(999));
        assert!(!backend.snd_paused(999));
    }
}
