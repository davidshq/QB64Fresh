//! Rodio-based audio backend for QB64Fresh.
//!
//! This module provides a real audio backend using the rodio library,
//! which is a pure Rust audio library that works across platforms.
//!
//! # Features
//!
//! - BEEP: Plays a short beep tone
//! - SOUND: Plays tones at specified frequencies
//! - PLAY: Parses and plays MML (Music Macro Language) strings
//! - _SNDOPEN/_SNDPLAY/etc: Loads and plays audio files

use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::time::Duration;

use rodio::{Decoder, OutputStream, OutputStreamHandle, Sink, Source};

use super::error::{AudioError, AudioErrorKind};
use super::AudioBackend;

/// Sound handle information
struct SoundHandle {
    sink: Sink,
    source_data: Option<Vec<u8>>,
    volume: f32,
    balance: f32,
    length_secs: f64,
    is_looping: bool,
}

/// Rodio-based audio backend.
pub struct RodioBackend {
    /// Output stream (must be kept alive)
    _stream: Option<OutputStream>,
    /// Stream handle for creating sinks
    stream_handle: Option<OutputStreamHandle>,
    /// Whether the backend is initialized
    initialized: AtomicBool,
    /// Next handle ID to assign
    next_handle: AtomicI32,
    /// Active sound handles
    sounds: HashMap<i32, SoundHandle>,
}

impl RodioBackend {
    /// Create a new RodioBackend (uninitialized).
    pub fn new() -> Self {
        Self {
            _stream: None,
            stream_handle: None,
            initialized: AtomicBool::new(false),
            next_handle: AtomicI32::new(1),
            sounds: HashMap::new(),
        }
    }

    /// Get the stream handle or return an error.
    fn get_stream_handle(&self) -> Result<&OutputStreamHandle, AudioError> {
        self.stream_handle.as_ref().ok_or_else(|| {
            AudioError::new(
                AudioErrorKind::NotInitialized,
                "Audio system not initialized",
            )
        })
    }

    /// Generate a sine wave tone.
    fn generate_tone(&self, frequency: f64, duration_secs: f64) -> Result<(), AudioError> {
        let handle = self.get_stream_handle()?;
        let sink = Sink::try_new(handle).map_err(|e| {
            AudioError::new(
                AudioErrorKind::PlaybackFailed,
                format!("Failed to create sink: {}", e),
            )
        })?;

        // Create a sine wave source
        let source = SineWave::new(frequency as f32)
            .take_duration(Duration::from_secs_f64(duration_secs))
            .amplify(0.3); // Reduce volume to avoid clipping

        sink.append(source);
        sink.sleep_until_end();

        Ok(())
    }
}

impl Default for RodioBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl AudioBackend for RodioBackend {
    fn initialize(&mut self) -> Result<(), AudioError> {
        if self.initialized.load(Ordering::SeqCst) {
            return Ok(());
        }

        let (stream, handle) = OutputStream::try_default().map_err(|e| {
            AudioError::new(
                AudioErrorKind::InitializationFailed,
                format!("Failed to initialize audio output: {}", e),
            )
        })?;

        self._stream = Some(stream);
        self.stream_handle = Some(handle);
        self.initialized.store(true, Ordering::SeqCst);

        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), AudioError> {
        // Stop all playing sounds
        for (_, handle) in self.sounds.drain() {
            handle.sink.stop();
        }

        self._stream = None;
        self.stream_handle = None;
        self.initialized.store(false, Ordering::SeqCst);

        Ok(())
    }

    fn is_initialized(&self) -> bool {
        self.initialized.load(Ordering::SeqCst)
    }

    fn beep(&mut self) -> Result<(), AudioError> {
        // Standard PC beep: 800Hz for ~0.25 seconds
        self.generate_tone(800.0, 0.25)
    }

    fn sound(&mut self, frequency: f64, duration: f64) -> Result<(), AudioError> {
        // Duration is in clock ticks (18.2 ticks/second)
        let duration_secs = duration / 18.2;

        if frequency <= 0.0 {
            // Frequency 0 or negative means pause
            std::thread::sleep(Duration::from_secs_f64(duration_secs));
            return Ok(());
        }

        self.generate_tone(frequency, duration_secs)
    }

    fn play(&mut self, commands: &str) -> Result<(), AudioError> {
        // Parse MML (Music Macro Language) and play
        let mut octave = 4;
        let mut tempo = 120; // BPM
        let mut note_length = 4; // Quarter note default
        let mut chars = commands.chars().peekable();

        while let Some(c) = chars.next() {
            match c.to_ascii_uppercase() {
                'O' => {
                    // Set octave (1-7)
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if let Ok(o) = num.parse::<u8>() {
                        octave = o.clamp(1, 7);
                    }
                }
                'L' => {
                    // Set default note length
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if let Ok(l) = num.parse::<u8>() {
                        note_length = l.clamp(1, 64);
                    }
                }
                'T' => {
                    // Set tempo
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if let Ok(t) = num.parse::<u16>() {
                        tempo = t.clamp(32, 255);
                    }
                }
                'C' | 'D' | 'E' | 'F' | 'G' | 'A' | 'B' => {
                    // Play a note
                    let note_num = match c.to_ascii_uppercase() {
                        'C' => 0,
                        'D' => 2,
                        'E' => 4,
                        'F' => 5,
                        'G' => 7,
                        'A' => 9,
                        'B' => 11,
                        _ => 0,
                    };

                    // Check for sharp/flat
                    let semitone_adjust = match chars.peek() {
                        Some(&'+') | Some(&'#') => {
                            chars.next();
                            1
                        }
                        Some(&'-') => {
                            chars.next();
                            -1
                        }
                        _ => 0,
                    };

                    // Check for note length override
                    let mut this_length = note_length;
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if let Ok(l) = num.parse::<u8>() {
                        this_length = l.clamp(1, 64);
                    }

                    // Check for dotted note
                    let dotted = if chars.peek() == Some(&'.') {
                        chars.next();
                        true
                    } else {
                        false
                    };

                    // Calculate frequency (A4 = 440Hz)
                    let midi_note = (octave as i32 + 1) * 12 + note_num as i32 + semitone_adjust;
                    let frequency = 440.0 * 2.0_f64.powf((midi_note as f64 - 69.0) / 12.0);

                    // Calculate duration
                    let beat_duration = 60.0 / tempo as f64;
                    let mut duration = beat_duration * (4.0 / this_length as f64);
                    if dotted {
                        duration *= 1.5;
                    }

                    self.generate_tone(frequency, duration)?;
                }
                'P' | 'R' => {
                    // Pause/Rest
                    let mut this_length = note_length;
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if let Ok(l) = num.parse::<u8>() {
                        this_length = l.clamp(1, 64);
                    }

                    let beat_duration = 60.0 / tempo as f64;
                    let duration = beat_duration * (4.0 / this_length as f64);
                    std::thread::sleep(Duration::from_secs_f64(duration));
                }
                '<' => {
                    // Decrease octave
                    if octave > 1 {
                        octave -= 1;
                    }
                }
                '>' => {
                    // Increase octave
                    if octave < 7 {
                        octave += 1;
                    }
                }
                ' ' | '\t' | '\n' | '\r' => {
                    // Whitespace is ignored
                }
                _ => {
                    // Unknown command - ignore
                }
            }
        }

        Ok(())
    }

    fn snd_open(&mut self, filename: &str) -> i32 {
        let handle_id = self.next_handle.fetch_add(1, Ordering::SeqCst);

        // Try to open and decode the file
        let file = match File::open(filename) {
            Ok(f) => f,
            Err(_) => return -1,
        };

        let reader = BufReader::new(file);
        let decoder = match Decoder::new(reader) {
            Ok(d) => d,
            Err(_) => return -2,
        };

        // Get duration if possible
        let length_secs = decoder
            .total_duration()
            .map(|d| d.as_secs_f64())
            .unwrap_or(0.0);

        // Create a sink for this sound
        let stream_handle = match self.get_stream_handle() {
            Ok(h) => h,
            Err(_) => return -3,
        };

        let sink = match Sink::try_new(stream_handle) {
            Ok(s) => s,
            Err(_) => return -4,
        };

        // Read the entire file into memory for potential looping
        let file2 = match File::open(filename) {
            Ok(f) => f,
            Err(_) => return -5,
        };
        let mut source_data = Vec::new();
        if std::io::Read::read_to_end(&mut BufReader::new(file2), &mut source_data).is_err() {
            return -6;
        }

        sink.pause(); // Start paused

        self.sounds.insert(
            handle_id,
            SoundHandle {
                sink,
                source_data: Some(source_data),
                volume: 1.0,
                balance: 0.0,
                length_secs,
                is_looping: false,
            },
        );

        handle_id
    }

    fn snd_close(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.remove(&handle) {
            sound.sink.stop();
        }
        Ok(())
    }

    fn snd_play(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get_mut(&handle) {
            if let Some(ref data) = sound.source_data {
                // Decode from memory
                let cursor = std::io::Cursor::new(data.clone());
                if let Ok(decoder) = Decoder::new(cursor) {
                    sound.sink.clear();
                    sound.sink.append(decoder);
                    sound.sink.set_volume(sound.volume);
                    sound.sink.play();
                }
            }
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_stop(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.sink.stop();
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_pause(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get(&handle) {
            sound.sink.pause();
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_resume(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get(&handle) {
            sound.sink.play();
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_loop(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.is_looping = true;
            // For proper looping, we'd need to append an infinite source
            // This is a simplified implementation
            if let Some(ref data) = sound.source_data {
                let cursor = std::io::Cursor::new(data.clone());
                if let Ok(decoder) = Decoder::new(cursor) {
                    sound.sink.clear();
                    sound.sink.append(decoder.repeat_infinite());
                    sound.sink.set_volume(sound.volume);
                    sound.sink.play();
                }
            }
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_vol(&mut self, handle: i32, volume: f64) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.volume = volume.clamp(0.0, 1.0) as f32;
            sound.sink.set_volume(sound.volume);
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_bal(&mut self, _handle: i32, _balance: f64) -> Result<(), AudioError> {
        // Balance control would require spatial audio support
        // For now, we just accept the call without doing anything
        Ok(())
    }

    fn snd_len(&self, handle: i32) -> f64 {
        self.sounds
            .get(&handle)
            .map(|s| s.length_secs)
            .unwrap_or(0.0)
    }

    fn snd_getpos(&self, _handle: i32) -> f64 {
        // Getting the current position requires tracking playback
        // This is a simplified implementation
        0.0
    }

    fn snd_setpos(&mut self, _handle: i32, _position: f64) -> Result<(), AudioError> {
        // Seeking in the audio stream is complex with rodio
        // This is a simplified implementation
        Ok(())
    }

    fn snd_playing(&self, handle: i32) -> bool {
        self.sounds
            .get(&handle)
            .map(|s| !s.sink.is_paused() && !s.sink.empty())
            .unwrap_or(false)
    }

    fn snd_paused(&self, handle: i32) -> bool {
        self.sounds
            .get(&handle)
            .map(|s| s.sink.is_paused())
            .unwrap_or(false)
    }

    fn snd_rate(&self) -> i32 {
        48000 // Default sample rate
    }
}

/// Simple sine wave audio source.
struct SineWave {
    frequency: f32,
    sample_rate: u32,
    sample_index: u64,
}

impl SineWave {
    fn new(frequency: f32) -> Self {
        Self {
            frequency,
            sample_rate: 48000,
            sample_index: 0,
        }
    }
}

impl Iterator for SineWave {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        let sample = (2.0 * std::f32::consts::PI * self.frequency * self.sample_index as f32
            / self.sample_rate as f32)
            .sin();
        self.sample_index = self.sample_index.wrapping_add(1);
        Some(sample)
    }
}

impl Source for SineWave {
    fn current_frame_len(&self) -> Option<usize> {
        None
    }

    fn channels(&self) -> u16 {
        1 // Mono
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn total_duration(&self) -> Option<Duration> {
        None // Infinite
    }
}
