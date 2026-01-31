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
//! - _SNDOPEN/_SNDPLAY/etc: Loads and plays audio files (WAV, OGG, etc., and .mid via soundfont)
//! - MIDI: When opening a `.mid` file and `_MIDISOUNDBANK` is set, synthesizes MIDI to PCM with rustysynth

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::BufReader;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use rodio::{Decoder, OutputStream, OutputStreamBuilder, Sink, Source};

use super::error::{AudioError, AudioErrorKind};
use super::AudioBackend;

#[cfg(feature = "audio-midi")]
use super::midi;

/// Source data for a sound: either encoded bytes (WAV/OGG/etc.) or pre-rendered PCM (e.g. MIDI).
enum SoundSource {
    /// Encoded audio file bytes (decoded by rodio on play).
    Encoded(Vec<u8>),
    /// Pre-rendered stereo float PCM (e.g. from MIDI synthesis). Interleaved L,R,L,R...
    Pcm { samples: Vec<f32>, sample_rate: u32 },
}

/// Sound handle information
struct SoundHandle {
    sink: Sink,
    source: Option<SoundSource>,
    volume: f32,
    balance: f32,
    length_secs: f64,
    is_looping: bool,
    /// When playback started (for position tracking)
    play_start_time: Option<Instant>,
    /// Position offset when playback started (for seeking)
    play_start_position: f64,
    /// Sample rate of the audio
    sample_rate: u32,
    /// Number of channels
    channels: u16,
}

/// Rodio-based audio backend.
pub struct RodioBackend {
    /// Output stream (must be kept alive, provides mixer for creating sinks)
    stream: Option<OutputStream>,
    /// Whether the backend is initialized
    initialized: AtomicBool,
    /// Next handle ID to assign
    next_handle: AtomicI32,
    /// Active sound handles
    sounds: HashMap<i32, SoundHandle>,
    /// Raw audio sample queue (shared with the source)
    raw_samples: Arc<Mutex<VecDeque<f32>>>,
    /// Raw audio sink (for playback)
    raw_sink: Option<Sink>,
    /// Whether raw audio is active
    raw_active: bool,
    /// MIDI sound bank (soundfont) path for _MIDISOUNDBANK; used when playing .mid files
    midi_sound_bank_path: Option<String>,
}

impl RodioBackend {
    /// Create a new RodioBackend (uninitialized).
    pub fn new() -> Self {
        Self {
            stream: None,
            initialized: AtomicBool::new(false),
            next_handle: AtomicI32::new(1),
            sounds: HashMap::new(),
            raw_samples: Arc::new(Mutex::new(VecDeque::new())),
            raw_sink: None,
            raw_active: false,
            midi_sound_bank_path: None,
        }
    }

    /// Get the output stream or return an error.
    fn get_stream(&self) -> Result<&OutputStream, AudioError> {
        self.stream.as_ref().ok_or_else(|| {
            AudioError::new(
                AudioErrorKind::NotInitialized,
                "Audio system not initialized",
            )
        })
    }

    /// Generate a sine wave tone.
    fn generate_tone(&self, frequency: f64, duration_secs: f64) -> Result<(), AudioError> {
        let stream = self.get_stream()?;
        let sink = Sink::connect_new(stream.mixer());

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

        let stream = OutputStreamBuilder::open_default_stream().map_err(|e| {
            AudioError::new(
                AudioErrorKind::InitializationFailed,
                format!("Failed to initialize audio output: {}", e),
            )
        })?;

        self.stream = Some(stream);
        self.initialized.store(true, Ordering::SeqCst);

        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), AudioError> {
        // Stop all playing sounds
        for (_, handle) in self.sounds.drain() {
            handle.sink.stop();
        }

        self.stream = None;
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

        let stream = match self.get_stream() {
            Ok(s) => s,
            Err(_) => return -3,
        };
        let sink = Sink::connect_new(stream.mixer());
        sink.pause();

        // MIDI: if file is .mid and we have a soundfont, synthesize to PCM
        #[cfg(feature = "audio-midi")]
        if filename.len() >= 4 && filename[filename.len() - 4..].eq_ignore_ascii_case(".mid") {
            if let Some(ref sf_path) = self.midi_sound_bank_path {
                match midi::render_midi_to_pcm(filename, sf_path) {
                    Ok((samples, sample_rate)) => {
                        let length_secs = (samples.len() as f64) / (sample_rate as f64 * 2.0); // stereo
                        self.sounds.insert(
                            handle_id,
                            SoundHandle {
                                sink,
                                source: Some(SoundSource::Pcm {
                                    samples,
                                    sample_rate,
                                }),
                                volume: 1.0,
                                balance: 0.0,
                                length_secs,
                                is_looping: false,
                                play_start_time: None,
                                play_start_position: 0.0,
                                sample_rate,
                                channels: 2,
                            },
                        );
                        return handle_id;
                    }
                    Err(_) => {
                        // Fall through to try as regular file (e.g. misnamed or soundfont error)
                    }
                }
            }
            // No soundfont set: .mid requires _MIDISOUNDBANK
            if self.midi_sound_bank_path.is_none() {
                return -7; // Distinct code: MIDI file but no soundfont
            }
        }

        // Regular encoded audio file
        let file = match File::open(filename) {
            Ok(f) => f,
            Err(_) => return -1,
        };
        let reader = BufReader::new(file);
        let decoder = match Decoder::new(reader) {
            Ok(d) => d,
            Err(_) => return -2,
        };

        let sample_rate = decoder.sample_rate();
        let channels = decoder.channels();
        let length_secs = decoder
            .total_duration()
            .map(|d| d.as_secs_f64())
            .unwrap_or(0.0);

        let file2 = match File::open(filename) {
            Ok(f) => f,
            Err(_) => return -5,
        };
        let mut source_data = Vec::new();
        if std::io::Read::read_to_end(&mut BufReader::new(file2), &mut source_data).is_err() {
            return -6;
        }

        self.sounds.insert(
            handle_id,
            SoundHandle {
                sink,
                source: Some(SoundSource::Encoded(source_data)),
                volume: 1.0,
                balance: 0.0,
                length_secs,
                is_looping: false,
                play_start_time: None,
                play_start_position: 0.0,
                sample_rate,
                channels,
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
            sound.sink.clear();
            match &sound.source {
                Some(SoundSource::Encoded(data)) => {
                    let cursor = std::io::Cursor::new(data.clone());
                    if let Ok(decoder) = Decoder::new(cursor) {
                        if sound.balance.abs() > 0.01 {
                            sound
                                .sink
                                .append(BalancedSource::new(decoder, sound.balance));
                        } else {
                            sound.sink.append(decoder);
                        }
                    }
                }
                Some(SoundSource::Pcm {
                    samples,
                    sample_rate,
                }) => {
                    let src = PcmBufferSource::new(samples.clone(), *sample_rate, 0);
                    if sound.balance.abs() > 0.01 {
                        sound.sink.append(BalancedSource::new(src, sound.balance));
                    } else {
                        sound.sink.append(src);
                    }
                }
                None => {}
            }
            sound.sink.set_volume(sound.volume);
            sound.sink.play();
            sound.play_start_time = Some(Instant::now());
            sound.play_start_position = 0.0;
            sound.is_looping = false;
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
        if let Some(sound) = self.sounds.get_mut(&handle) {
            // Calculate current position before pausing
            if let Some(start_time) = sound.play_start_time {
                let elapsed = start_time.elapsed().as_secs_f64();
                sound.play_start_position += elapsed;
            }
            sound.play_start_time = None;
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
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.sink.play();
            // Restart position tracking from where we paused
            sound.play_start_time = Some(Instant::now());
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
            sound.sink.clear();
            sound.is_looping = true;
            match &sound.source {
                Some(SoundSource::Encoded(data)) => {
                    let cursor = std::io::Cursor::new(data.clone());
                    if let Ok(decoder) = Decoder::new(cursor) {
                        if sound.balance.abs() > 0.01 {
                            sound.sink.append(
                                BalancedSource::new(decoder, sound.balance).repeat_infinite(),
                            );
                        } else {
                            sound.sink.append(decoder.repeat_infinite());
                        }
                    }
                }
                Some(SoundSource::Pcm {
                    samples,
                    sample_rate,
                }) => {
                    let src =
                        PcmBufferSource::new(samples.clone(), *sample_rate, 0).repeat_infinite();
                    if sound.balance.abs() > 0.01 {
                        sound.sink.append(BalancedSource::new(src, sound.balance));
                    } else {
                        sound.sink.append(src);
                    }
                }
                None => {}
            }
            sound.sink.set_volume(sound.volume);
            sound.sink.play();
            sound.play_start_time = Some(Instant::now());
            sound.play_start_position = 0.0;
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

    fn snd_bal(&mut self, handle: i32, balance: f64) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get_mut(&handle) {
            sound.balance = balance.clamp(-1.0, 1.0) as f32;
            // Balance will be applied on next play/loop call
            // We can't easily change balance of already-playing audio in rodio
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_len(&self, handle: i32) -> f64 {
        self.sounds
            .get(&handle)
            .map(|s| s.length_secs)
            .unwrap_or(0.0)
    }

    fn snd_getpos(&self, handle: i32) -> f64 {
        if let Some(sound) = self.sounds.get(&handle) {
            let base_pos = sound.play_start_position;
            if let Some(start_time) = sound.play_start_time {
                if !sound.sink.is_paused() {
                    let elapsed = start_time.elapsed().as_secs_f64();
                    let pos = base_pos + elapsed;
                    // Handle looping: wrap around
                    if sound.is_looping && sound.length_secs > 0.0 {
                        return pos % sound.length_secs;
                    }
                    return pos.min(sound.length_secs);
                }
            }
            base_pos
        } else {
            0.0
        }
    }

    fn snd_setpos(&mut self, handle: i32, position: f64) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get_mut(&handle) {
            let position = position.clamp(0.0, sound.length_secs);
            sound.sink.clear();

            match &sound.source {
                Some(SoundSource::Encoded(data)) => {
                    let cursor = std::io::Cursor::new(data.clone());
                    if let Ok(decoder) = Decoder::new(cursor) {
                        let skipped = decoder.skip_duration(Duration::from_secs_f64(position));
                        if sound.balance.abs() > 0.01 {
                            let balanced = BalancedSource::new(skipped, sound.balance);
                            if sound.is_looping {
                                sound.sink.append(balanced.repeat_infinite());
                            } else {
                                sound.sink.append(balanced);
                            }
                        } else if sound.is_looping {
                            sound.sink.append(skipped.repeat_infinite());
                        } else {
                            sound.sink.append(skipped);
                        }
                    }
                }
                Some(SoundSource::Pcm {
                    samples,
                    sample_rate,
                }) => {
                    let start_sample =
                        ((position * *sample_rate as f64 * 2.0) as usize).min(samples.len());
                    let src = PcmBufferSource::new(samples.clone(), *sample_rate, start_sample);
                    if sound.balance.abs() > 0.01 {
                        if sound.is_looping {
                            sound
                                .sink
                                .append(BalancedSource::new(src.repeat_infinite(), sound.balance));
                        } else {
                            sound.sink.append(BalancedSource::new(src, sound.balance));
                        }
                    } else if sound.is_looping {
                        sound.sink.append(src.repeat_infinite());
                    } else {
                        sound.sink.append(src);
                    }
                }
                None => {}
            }

            sound.sink.set_volume(sound.volume);
            sound.play_start_position = position;
            sound.play_start_time = Some(Instant::now());
            if !sound.sink.is_paused() {
                sound.sink.play();
            }
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
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

    fn snd_copy(&mut self, handle: i32) -> i32 {
        if let Some(sound) = self.sounds.get(&handle) {
            let source = match &sound.source {
                Some(SoundSource::Encoded(data)) => Some(SoundSource::Encoded(data.clone())),
                Some(SoundSource::Pcm {
                    samples,
                    sample_rate,
                }) => Some(SoundSource::Pcm {
                    samples: samples.clone(),
                    sample_rate: *sample_rate,
                }),
                None => None,
            };
            if let Some(source) = source {
                let new_handle = self.next_handle.fetch_add(1, Ordering::SeqCst);
                let stream = match self.get_stream() {
                    Ok(s) => s,
                    Err(_) => return -1,
                };
                let sink = Sink::connect_new(stream.mixer());
                sink.pause();
                self.sounds.insert(
                    new_handle,
                    SoundHandle {
                        sink,
                        source: Some(source),
                        volume: sound.volume,
                        balance: sound.balance,
                        length_secs: sound.length_secs,
                        is_looping: false,
                        play_start_time: None,
                        play_start_position: 0.0,
                        sample_rate: sound.sample_rate,
                        channels: sound.channels,
                    },
                );
                new_handle
            } else {
                -1
            }
        } else {
            -1
        }
    }

    fn snd_playfile(&mut self, filename: &str, sync: bool) -> Result<(), AudioError> {
        let handle = self.snd_open(filename);
        if handle < 0 {
            return Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Failed to open file",
            ));
        }
        self.snd_play(handle)?;
        if sync {
            if let Some(sound) = self.sounds.get(&handle) {
                sound.sink.sleep_until_end();
            }
            self.snd_close(handle)?;
        }
        Ok(())
    }

    fn snd_playcopy(&mut self, handle: i32) -> Result<(), AudioError> {
        if let Some(sound) = self.sounds.get(&handle) {
            let stream = self.get_stream()?;
            let sink = Sink::connect_new(stream.mixer());
            match &sound.source {
                Some(SoundSource::Encoded(data)) => {
                    let cursor = std::io::Cursor::new(data.clone());
                    if let Ok(decoder) = Decoder::new(cursor) {
                        if sound.balance.abs() > 0.01 {
                            sink.append(BalancedSource::new(decoder, sound.balance));
                        } else {
                            sink.append(decoder);
                        }
                    }
                }
                Some(SoundSource::Pcm {
                    samples,
                    sample_rate,
                }) => {
                    let src = PcmBufferSource::new(samples.clone(), *sample_rate, 0);
                    if sound.balance.abs() > 0.01 {
                        sink.append(BalancedSource::new(src, sound.balance));
                    } else {
                        sink.append(src);
                    }
                }
                None => {}
            }
            sink.set_volume(sound.volume);
            sink.play();
            sink.detach();
            Ok(())
        } else {
            Err(AudioError::new(
                AudioErrorKind::InvalidHandle,
                "Invalid sound handle",
            ))
        }
    }

    fn snd_openraw(&mut self) -> i32 {
        if self.raw_active {
            return 0; // Already open, return success handle
        }

        let stream = match self.get_stream() {
            Ok(s) => s,
            Err(_) => return -1,
        };

        // Create a sink for raw audio
        let sink = Sink::connect_new(stream.mixer());

        // Create the raw audio source with shared sample queue
        let raw_source = RawAudioSource::new(Arc::clone(&self.raw_samples));
        sink.append(raw_source);
        sink.play();

        self.raw_sink = Some(sink);
        self.raw_active = true;

        0 // Return handle 0 for raw audio
    }

    fn snd_raw(&mut self, sample: f64) -> Result<(), AudioError> {
        if !self.raw_active {
            return Ok(()); // Silently ignore if not active
        }

        if let Ok(mut queue) = self.raw_samples.lock() {
            // Mono sample - duplicate for stereo output
            let s = (sample.clamp(-1.0, 1.0)) as f32;
            queue.push_back(s);
            queue.push_back(s);
        }
        Ok(())
    }

    fn snd_raw_stereo(&mut self, left: f64, right: f64) -> Result<(), AudioError> {
        if !self.raw_active {
            return Ok(()); // Silently ignore if not active
        }

        if let Ok(mut queue) = self.raw_samples.lock() {
            queue.push_back((left.clamp(-1.0, 1.0)) as f32);
            queue.push_back((right.clamp(-1.0, 1.0)) as f32);
        }
        Ok(())
    }

    fn snd_rawlen(&self) -> f64 {
        if !self.raw_active {
            return 0.0;
        }

        if let Ok(queue) = self.raw_samples.lock() {
            // Queue has stereo samples (2 per frame), at 48000 Hz
            let frames = queue.len() / 2;
            frames as f64 / 48000.0
        } else {
            0.0
        }
    }

    fn set_midi_sound_bank_path(&mut self, path: Option<&str>) {
        self.midi_sound_bank_path = path.map(String::from);
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
    fn current_span_len(&self) -> Option<usize> {
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

/// A source wrapper that applies stereo balance (panning).
///
/// Balance of -1.0 is full left, 0.0 is center, 1.0 is full right.
struct BalancedSource<S: Source<Item = f32>> {
    inner: S,
    balance: f32,
    /// Buffer for converting mono to stereo or processing stereo
    sample_buffer: Vec<f32>,
    buffer_pos: usize,
}

impl<S: Source<Item = f32>> BalancedSource<S> {
    fn new(source: S, balance: f32) -> Self {
        Self {
            inner: source,
            balance: balance.clamp(-1.0, 1.0),
            sample_buffer: Vec::new(),
            buffer_pos: 0,
        }
    }

    fn fill_buffer(&mut self) {
        self.sample_buffer.clear();
        self.buffer_pos = 0;

        let channels = self.inner.channels() as usize;

        // Read enough samples for one frame
        let mut frame = Vec::with_capacity(channels);
        for _ in 0..channels {
            if let Some(s) = self.inner.next() {
                frame.push(s);
            } else {
                return; // End of source
            }
        }

        // Calculate left/right gains from balance
        // At balance=0: both channels = 1.0
        // At balance=-1: left=1.0, right=0.0
        // At balance=1: left=0.0, right=1.0
        let left_gain = if self.balance <= 0.0 {
            1.0
        } else {
            1.0 - self.balance
        };
        let right_gain = if self.balance >= 0.0 {
            1.0
        } else {
            1.0 + self.balance
        };

        if channels == 1 {
            // Mono source - output as stereo with balance
            self.sample_buffer.push(frame[0] * left_gain);
            self.sample_buffer.push(frame[0] * right_gain);
        } else if channels >= 2 {
            // Stereo or more - apply balance to L/R
            self.sample_buffer.push(frame[0] * left_gain);
            self.sample_buffer.push(frame[1] * right_gain);
            // Pass through any additional channels unchanged
            for s in frame.iter().skip(2) {
                self.sample_buffer.push(*s);
            }
        }
    }
}

impl<S: Source<Item = f32>> Iterator for BalancedSource<S> {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        if self.buffer_pos >= self.sample_buffer.len() {
            self.fill_buffer();
        }

        if self.buffer_pos < self.sample_buffer.len() {
            let sample = self.sample_buffer[self.buffer_pos];
            self.buffer_pos += 1;
            Some(sample)
        } else {
            None
        }
    }
}

impl<S: Source<Item = f32>> Source for BalancedSource<S> {
    fn current_span_len(&self) -> Option<usize> {
        self.inner.current_span_len()
    }

    fn channels(&self) -> u16 {
        // Always output stereo if input is mono
        if self.inner.channels() == 1 {
            2
        } else {
            self.inner.channels()
        }
    }

    fn sample_rate(&self) -> u32 {
        self.inner.sample_rate()
    }

    fn total_duration(&self) -> Option<Duration> {
        self.inner.total_duration()
    }
}

/// A source for raw audio sample playback.
///
/// Reads samples from a shared queue and plays them.
struct RawAudioSource {
    samples: Arc<Mutex<VecDeque<f32>>>,
    sample_rate: u32,
}

impl RawAudioSource {
    fn new(samples: Arc<Mutex<VecDeque<f32>>>) -> Self {
        Self {
            samples,
            sample_rate: 48000,
        }
    }
}

impl Iterator for RawAudioSource {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        if let Ok(mut queue) = self.samples.lock() {
            queue.pop_front()
        } else {
            Some(0.0) // Return silence if lock fails
        }
        .or(Some(0.0)) // Return silence if queue is empty (keeps source alive)
    }
}

impl Source for RawAudioSource {
    fn current_span_len(&self) -> Option<usize> {
        None // Infinite stream
    }

    fn channels(&self) -> u16 {
        2 // Stereo
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn total_duration(&self) -> Option<Duration> {
        None // Infinite
    }
}

/// A rodio `Source` that plays pre-rendered stereo float PCM (e.g. from MIDI synthesis).
///
/// Samples are interleaved L, R, L, R, ... `start_index` allows seeking by skipping
/// that many samples at the start.
struct PcmBufferSource {
    samples: Vec<f32>,
    sample_rate: u32,
    index: usize,
}

impl PcmBufferSource {
    /// Creates a source from stereo-interleaved samples.
    ///
    /// * `samples` - Interleaved L,R,L,R... (length must be even)
    /// * `sample_rate` - Sample rate in Hz
    /// * `start_index` - First sample index to play (for seeking); use 0 for start
    fn new(samples: Vec<f32>, sample_rate: u32, start_index: usize) -> Self {
        let cap = (samples.len() / 2) * 2; // align to stereo frame
        Self {
            samples,
            sample_rate,
            index: start_index.min(cap),
        }
    }
}

impl Iterator for PcmBufferSource {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        if self.index < self.samples.len() {
            let s = self.samples[self.index];
            self.index += 1;
            Some(s)
        } else {
            None
        }
    }
}

impl Source for PcmBufferSource {
    fn current_span_len(&self) -> Option<usize> {
        let remaining = self.samples.len().saturating_sub(self.index);
        if remaining > 0 {
            Some(remaining)
        } else {
            None
        }
    }

    fn channels(&self) -> u16 {
        2
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn total_duration(&self) -> Option<Duration> {
        let remaining = self.samples.len().saturating_sub(self.index);
        if remaining == 0 {
            return None;
        }
        let secs = remaining as f64 / (self.sample_rate as f64 * 2.0);
        Some(Duration::from_secs_f64(secs))
    }
}
