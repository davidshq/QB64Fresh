//! Joystick/Gamepad support for QB64Fresh
//!
//! Provides classic BASIC joystick functions:
//! - STICK(n) - Get joystick axis position (0-254, 127 = center)
//! - STRIG(n) - Get joystick button state
//!
//! And QB64 extensions:
//! - _DEVICES - Number of input devices
//! - _DEVICE$ - Device name
//! - _DEVICEINPUT - Check for device input
//! - _AXIS - Get axis value (-1.0 to 1.0)
//! - _BUTTON - Get button state
//!
//! Uses SDL2 for cross-platform gamepad support.

#[cfg(feature = "graphics-sdl2")]
use std::sync::Mutex;

#[cfg(feature = "graphics-sdl2")]
use std::sync::OnceLock;

#[cfg(feature = "graphics-sdl2")]
static JOYSTICK_STATE: OnceLock<Mutex<JoystickState>> = OnceLock::new();

#[cfg(feature = "graphics-sdl2")]
fn get_joystick_state() -> &'static Mutex<JoystickState> {
    JOYSTICK_STATE.get_or_init(|| Mutex::new(JoystickState::new()))
}

#[cfg(feature = "graphics-sdl2")]
struct JoystickState {
    initialized: bool,
    // We store the subsystems to keep them alive
    // controller_subsystem: Option<GameControllerSubsystem>,
    // joystick_subsystem: Option<JoystickSubsystem>,
    // Open controllers/joysticks
    // controllers: Vec<GameController>,
    // joysticks: Vec<Joystick>,
    // Cached axis values (0-254 range, 127 = center)
    axes: [[i32; 6]; 4], // 4 joysticks, 6 axes each
    // Cached button states
    buttons: [[bool; 32]; 4], // 4 joysticks, 32 buttons each
}

#[cfg(feature = "graphics-sdl2")]
impl JoystickState {
    fn new() -> Self {
        Self {
            initialized: false,
            axes: [[127; 6]; 4],
            buttons: [[false; 32]; 4],
        }
    }
}

/// Classic BASIC STICK function.
///
/// Returns joystick axis position in range 0-254 (127 = center).
///
/// Arguments:
/// - 0: Joystick A, X axis
/// - 1: Joystick A, Y axis
/// - 2: Joystick B, X axis
/// - 3: Joystick B, Y axis
///
/// QB64 extensions (unofficial):
/// - 4+: Additional axes on first joystick
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub extern "C" fn qb_stick(axis: i32) -> i32 {
    #[cfg(feature = "graphics-sdl2")]
    {
        let state = get_joystick_state().lock().unwrap();

        // Map classic STICK arguments to joystick/axis
        let (joy_idx, axis_idx) = match axis {
            0 => (0, 0),                          // Joy A, X
            1 => (0, 1),                          // Joy A, Y
            2 => (1, 0),                          // Joy B, X
            3 => (1, 1),                          // Joy B, Y
            n if n >= 4 => (0, (n - 4) as usize), // Extended axes
            _ => return 127,                      // Invalid, return center
        };

        if joy_idx < state.axes.len() && axis_idx < state.axes[joy_idx].len() {
            state.axes[joy_idx][axis_idx]
        } else {
            127 // Center/neutral
        }
    }

    #[cfg(not(feature = "graphics-sdl2"))]
    {
        let _ = axis;
        127 // Return center position when joystick not available
    }
}

/// Classic BASIC STRIG function.
///
/// Returns joystick button state.
///
/// Arguments:
/// - 0: Button 1 on Joystick A pressed since last STRIG(0)
/// - 1: Button 1 on Joystick A currently pressed
/// - 2: Button 2 on Joystick A pressed since last STRIG(2)
/// - 3: Button 2 on Joystick A currently pressed
/// - 4: Button 1 on Joystick B pressed since last STRIG(4)
/// - 5: Button 1 on Joystick B currently pressed
/// - 6: Button 2 on Joystick B pressed since last STRIG(6)
/// - 7: Button 2 on Joystick B currently pressed
///
/// Returns -1 if pressed, 0 if not pressed.
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub extern "C" fn qb_strig(button: i32) -> i32 {
    #[cfg(feature = "graphics-sdl2")]
    {
        let state = get_joystick_state().lock().unwrap();

        // Map classic STRIG arguments to joystick/button
        // Odd numbers are "currently pressed", even are "pressed since last call"
        // For simplicity, we treat both the same (current state)
        let (joy_idx, btn_idx) = match button {
            0 | 1 => (0, 0), // Joy A, Button 1
            2 | 3 => (0, 1), // Joy A, Button 2
            4 | 5 => (1, 0), // Joy B, Button 1
            6 | 7 => (1, 1), // Joy B, Button 2
            _ => return 0,   // Invalid
        };

        if joy_idx < state.buttons.len() && btn_idx < state.buttons[joy_idx].len() {
            if state.buttons[joy_idx][btn_idx] {
                -1
            } else {
                0
            }
        } else {
            0
        }
    }

    #[cfg(not(feature = "graphics-sdl2"))]
    {
        let _ = button;
        0 // Button not pressed
    }
}

/// Get number of input devices (QB64 _DEVICES function).
///
/// Returns the number of available input devices including keyboard and mouse.
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub extern "C" fn qb_devices() -> i32 {
    #[cfg(feature = "graphics-sdl2")]
    {
        // Keyboard (1) + Mouse (1) + joysticks
        // For now, return 2 (keyboard + mouse)
        // TODO: Actually enumerate SDL2 joysticks
        2
    }

    #[cfg(not(feature = "graphics-sdl2"))]
    {
        2 // Keyboard and mouse
    }
}

/// Get axis value for a device (QB64 _AXIS function).
///
/// Returns axis value in range -1.0 to 1.0.
///
/// # Arguments
/// * `device` - Device number (1-based)
/// * `axis` - Axis number (1-based)
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub extern "C" fn qb_axis(device: i32, axis: i32) -> f64 {
    #[cfg(feature = "graphics-sdl2")]
    {
        let state = get_joystick_state().lock().unwrap();

        // Device 1 = keyboard (no axes), Device 2 = mouse (no axes)
        // Device 3+ = joysticks
        let joy_idx = device - 3;
        let axis_idx = axis - 1;

        if joy_idx >= 0
            && (joy_idx as usize) < state.axes.len()
            && axis_idx >= 0
            && (axis_idx as usize) < state.axes[joy_idx as usize].len()
        {
            // Convert from 0-254 range to -1.0 to 1.0
            let raw = state.axes[joy_idx as usize][axis_idx as usize];
            (raw as f64 - 127.0) / 127.0
        } else {
            0.0
        }
    }

    #[cfg(not(feature = "graphics-sdl2"))]
    {
        let _ = (device, axis);
        0.0
    }
}

/// Get button state for a device (QB64 _BUTTON function).
///
/// Returns -1 if pressed, 0 if not pressed.
///
/// # Arguments
/// * `device` - Device number (1-based)
/// * `button` - Button number (1-based)
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub extern "C" fn qb_button(device: i32, button: i32) -> i32 {
    #[cfg(feature = "graphics-sdl2")]
    {
        let state = get_joystick_state().lock().unwrap();

        // Device 3+ = joysticks
        let joy_idx = device - 3;
        let btn_idx = button - 1;

        if joy_idx >= 0
            && (joy_idx as usize) < state.buttons.len()
            && btn_idx >= 0
            && (btn_idx as usize) < state.buttons[joy_idx as usize].len()
        {
            if state.buttons[joy_idx as usize][btn_idx as usize] {
                -1
            } else {
                0
            }
        } else {
            0
        }
    }

    #[cfg(not(feature = "graphics-sdl2"))]
    {
        let _ = (device, button);
        0
    }
}

/// Update joystick state from SDL2 events.
///
/// This should be called from the graphics event loop.
///
/// # Safety
/// Must be called from the same thread that handles SDL2 events.
#[cfg(feature = "graphics-sdl2")]
pub fn update_joystick_axis(joy_idx: u32, axis_idx: u8, value: i16) {
    if let Ok(mut state) = get_joystick_state().lock() {
        if (joy_idx as usize) < state.axes.len() && (axis_idx as usize) < state.axes[0].len() {
            // Convert SDL2 -32768..32767 to BASIC 0-254 range
            let normalized = ((value as i32 + 32768) * 254 / 65535) as i32;
            state.axes[joy_idx as usize][axis_idx as usize] = normalized.clamp(0, 254);
        }
    }
}

#[cfg(feature = "graphics-sdl2")]
pub fn update_joystick_button(joy_idx: u32, button_idx: u8, pressed: bool) {
    if let Ok(mut state) = get_joystick_state().lock() {
        if (joy_idx as usize) < state.buttons.len()
            && (button_idx as usize) < state.buttons[0].len()
        {
            state.buttons[joy_idx as usize][button_idx as usize] = pressed;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stick_default_center() {
        // Without joystick connected, should return center position
        assert_eq!(qb_stick(0), 127);
        assert_eq!(qb_stick(1), 127);
        assert_eq!(qb_stick(2), 127);
        assert_eq!(qb_stick(3), 127);
    }

    #[test]
    fn test_strig_default_not_pressed() {
        // Without joystick connected, should return not pressed
        assert_eq!(qb_strig(0), 0);
        assert_eq!(qb_strig(1), 0);
    }
}
