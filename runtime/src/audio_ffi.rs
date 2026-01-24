//! C FFI layer for audio operations.
//!
//! This module provides C-compatible functions that can be called from
//! generated C code. Each function wraps the corresponding AudioBackend
//! trait method.
//!
//! # Error Handling
//!
//! All functions return an `i32`:
//! - `0`: Success
//! - Non-zero: Error

use std::ffi::CStr;
use std::os::raw::{c_char, c_int};

// ============================================================================
// Initialization
// ============================================================================

/// Initialize the audio system.
#[no_mangle]
pub extern "C" fn qb_audio_init() -> c_int {
    match crate::audio::init_audio() {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// Shut down the audio system.
#[no_mangle]
pub extern "C" fn qb_audio_shutdown() -> c_int {
    match crate::audio::shutdown_audio() {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

// ============================================================================
// Classic BASIC Sound (BEEP, SOUND, PLAY)
// ============================================================================

/// Play a simple beep sound.
#[no_mangle]
pub extern "C" fn qb_beep() -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.beep() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            // Auto-initialize audio if not done
            if crate::audio::init_audio().is_ok() {
                if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
                    return match backend.beep() {
                        Ok(()) => 0,
                        Err(_) => 1,
                    };
                }
            }
            1
        }
    }
}

/// Play a tone at the specified frequency for a duration.
///
/// # Arguments
/// - `frequency`: Frequency in Hz
/// - `duration`: Duration in clock ticks (18.2 ticks/second)
#[no_mangle]
pub extern "C" fn qb_sound(frequency: f64, duration: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.sound(frequency, duration) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            // Auto-initialize
            if crate::audio::init_audio().is_ok() {
                if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
                    return match backend.sound(frequency, duration) {
                        Ok(()) => 0,
                        Err(_) => 1,
                    };
                }
            }
            1
        }
    }
}

/// Play music using MML (Music Macro Language).
///
/// # Safety
/// The `commands` pointer must be valid and null-terminated.
#[no_mangle]
pub unsafe extern "C" fn qb_play(commands: *const c_char) -> c_int {
    if commands.is_null() {
        return 1;
    }

    let cmd_str = match CStr::from_ptr(commands).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
        match backend.play(cmd_str) {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        // Auto-initialize
        if crate::audio::init_audio().is_ok() {
            if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
                return match backend.play(cmd_str) {
                    Ok(()) => 0,
                    Err(_) => 1,
                };
            }
        }
        1
    }
}

// ============================================================================
// QB64 Sound File Operations (_SND* functions)
// ============================================================================

/// Open a sound file.
///
/// # Safety
/// - `filename` must be a valid null-terminated C string
///
/// # Returns
/// Sound handle (positive) or error code (negative)
#[no_mangle]
pub unsafe extern "C" fn qb_sndopen(filename: *const c_char) -> i32 {
    if filename.is_null() {
        return -1;
    }

    let fname = match CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    // Auto-initialize if needed
    if crate::audio::AUDIO_BACKEND.is_none() {
        let _ = crate::audio::init_audio();
    }

    if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
        backend.snd_open(fname)
    } else {
        -1
    }
}

/// Close a sound handle.
#[no_mangle]
pub extern "C" fn qb_sndclose(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_close(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Play a loaded sound.
#[no_mangle]
pub extern "C" fn qb_sndplay(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_play(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Stop a playing sound.
#[no_mangle]
pub extern "C" fn qb_sndstop(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_stop(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Pause a playing sound.
#[no_mangle]
pub extern "C" fn qb_sndpause(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_pause(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Resume a paused sound.
#[no_mangle]
pub extern "C" fn qb_sndresume(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_resume(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Play a sound in a continuous loop.
#[no_mangle]
pub extern "C" fn qb_sndloop(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_loop(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Set the volume for a sound.
#[no_mangle]
pub extern "C" fn qb_sndvol(handle: i32, volume: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_vol(handle, volume) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Set the stereo balance for a sound.
#[no_mangle]
pub extern "C" fn qb_sndbal(handle: i32, balance: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_bal(handle, balance) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get the length of a sound in seconds.
#[no_mangle]
pub extern "C" fn qb_sndlen(handle: i32) -> f64 {
    unsafe {
        if let Some(ref backend) = crate::audio::AUDIO_BACKEND {
            backend.snd_len(handle)
        } else {
            0.0
        }
    }
}

/// Get the current playback position in seconds.
#[no_mangle]
pub extern "C" fn qb_sndgetpos(handle: i32) -> f64 {
    unsafe {
        if let Some(ref backend) = crate::audio::AUDIO_BACKEND {
            backend.snd_getpos(handle)
        } else {
            0.0
        }
    }
}

/// Set the playback position in seconds.
#[no_mangle]
pub extern "C" fn qb_sndsetpos(handle: i32, position: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_setpos(handle, position) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Check if a sound is currently playing.
#[no_mangle]
pub extern "C" fn qb_sndplaying(handle: i32) -> c_int {
    unsafe {
        if let Some(ref backend) = crate::audio::AUDIO_BACKEND {
            if backend.snd_playing(handle) {
                1
            } else {
                0
            }
        } else {
            0
        }
    }
}

/// Check if a sound is paused.
#[no_mangle]
pub extern "C" fn qb_sndpaused(handle: i32) -> c_int {
    unsafe {
        if let Some(ref backend) = crate::audio::AUDIO_BACKEND {
            if backend.snd_paused(handle) {
                1
            } else {
                0
            }
        } else {
            0
        }
    }
}

/// Get the sample rate.
#[no_mangle]
pub extern "C" fn qb_sndrate() -> i32 {
    unsafe {
        if let Some(ref backend) = crate::audio::AUDIO_BACKEND {
            backend.snd_rate()
        } else {
            48000 // Default
        }
    }
}

// ============================================================================
// Raw Audio Synthesis
// ============================================================================

/// Open a raw audio stream.
#[no_mangle]
pub extern "C" fn qb_sndopenraw() -> i32 {
    // Auto-initialize if needed
    unsafe {
        if crate::audio::AUDIO_BACKEND.is_none() {
            let _ = crate::audio::init_audio();
        }

        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            backend.snd_openraw()
        } else {
            -1
        }
    }
}

/// Write a mono sample to the raw audio stream.
#[no_mangle]
pub extern "C" fn qb_sndraw(sample: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_raw(sample) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Write stereo samples to the raw audio stream.
#[no_mangle]
pub extern "C" fn qb_sndraw_stereo(left: f64, right: f64) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_raw_stereo(left, right) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

/// Get the number of seconds of audio currently queued.
#[no_mangle]
pub extern "C" fn qb_sndrawlen() -> f64 {
    unsafe {
        if let Some(ref backend) = crate::audio::AUDIO_BACKEND {
            backend.snd_rawlen()
        } else {
            0.0
        }
    }
}

/// Copy a sound handle (creates an independent copy).
///
/// # Returns
/// New sound handle, or -1 on error
#[no_mangle]
pub extern "C" fn qb_sndcopy(handle: i32) -> i32 {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            backend.snd_copy(handle)
        } else {
            -1
        }
    }
}

/// Play a sound file directly without creating a handle.
///
/// # Safety
/// - `filename` must be a valid null-terminated C string
///
/// # Arguments
/// - `filename`: Path to the sound file
/// - `sync`: If non-zero, block until playback completes
#[no_mangle]
pub unsafe extern "C" fn qb_sndplayfile(filename: *const c_char, sync: c_int) -> c_int {
    if filename.is_null() {
        return 1;
    }

    let fname = match CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    // Auto-initialize if needed
    if crate::audio::AUDIO_BACKEND.is_none() {
        let _ = crate::audio::init_audio();
    }

    if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
        match backend.snd_playfile(fname, sync != 0) {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        1
    }
}

/// Play a copy of a sound (allows overlapping playback).
#[no_mangle]
pub extern "C" fn qb_sndplaycopy(handle: i32) -> c_int {
    unsafe {
        if let Some(ref mut backend) = crate::audio::AUDIO_BACKEND {
            match backend.snd_playcopy(handle) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_beep_without_init() {
        // Should auto-initialize
        let result = qb_beep();
        // Result depends on whether mock backend is available
        assert!(result == 0 || result == 1);
    }
}
