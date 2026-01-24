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
        let was_pressed = if (joy_idx as usize) < state.buttons.len()
            && (button_idx as usize) < state.buttons[0].len()
        {
            state.buttons[joy_idx as usize][button_idx as usize]
        } else {
            false
        };

        if (joy_idx as usize) < state.buttons.len()
            && (button_idx as usize) < state.buttons[0].len()
        {
            state.buttons[joy_idx as usize][button_idx as usize] = pressed;
        }

        // Trigger event if button was just pressed (transition from false to true)
        if pressed && !was_pressed {
            drop(state); // Release joystick state lock before acquiring handler lock
            trigger_strig_event(joy_idx, button_idx);
        }
    }
}

// ============================================================================
// STRIG Event Handler Infrastructure
// ============================================================================

use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Mutex as StdMutex;

/// Handler state for a single STRIG button.
///
/// Each button can have an associated event handler that fires when pressed.
#[derive(Clone, Default)]
struct StrigHandler {
    /// Event ID (0 = no handler registered).
    id: u32,
    /// Handler state: 0=OFF, 1=ON, 2=STOP.
    active: u8,
    /// Number of pending events (incremented on button press, decremented on dispatch).
    pending: u8,
}

/// Maximum number of STRIG handlers (8 buttons: 0-7).
const MAX_STRIG_HANDLERS: usize = 8;

/// Global STRIG handler registry.
static STRIG_HANDLERS: StdMutex<[StrigHandler; MAX_STRIG_HANDLERS]> = StdMutex::new(
    [const {
        StrigHandler {
            id: 0,
            active: 0,
            pending: 0,
        }
    }; MAX_STRIG_HANDLERS],
);

/// Flag indicating at least one STRIG event is pending.
/// This is checked by the generated code at event check points.
static STRIG_EVENT_PENDING: AtomicBool = AtomicBool::new(false);

/// The event ID of the currently dispatching handler (for nested event prevention).
static STRIG_CURRENT_EVENT: AtomicU32 = AtomicU32::new(0);

/// Register a STRIG event handler.
///
/// Called by `ON STRIG(n) GOSUB label` codegen.
/// The event_id is a unique identifier that the generated switch statement uses
/// to dispatch to the correct label.
///
/// # Arguments
/// * `button_num` - STRIG button number (0-7)
/// * `event_id` - Unique event ID for this handler (generated by codegen)
#[no_mangle]
pub extern "C" fn qb_on_strig(button_num: i32, event_id: u32) {
    if button_num < 0 || button_num >= MAX_STRIG_HANDLERS as i32 {
        return;
    }

    if let Ok(mut handlers) = STRIG_HANDLERS.lock() {
        handlers[button_num as usize].id = event_id;
        // Default to OFF until STRIG(n) ON is called
        handlers[button_num as usize].active = 0;
        handlers[button_num as usize].pending = 0;
    }
}

/// Control STRIG event trapping.
///
/// Called by `STRIG(n) ON/OFF/STOP` codegen.
///
/// # Arguments
/// * `button_num` - STRIG button number (0-7)
/// * `mode` - 0=OFF, 1=ON, 2=STOP
#[no_mangle]
pub extern "C" fn qb_strig_control(button_num: i32, mode: i32) {
    if button_num < 0 || button_num >= MAX_STRIG_HANDLERS as i32 {
        return;
    }

    if let Ok(mut handlers) = STRIG_HANDLERS.lock() {
        handlers[button_num as usize].active = mode.clamp(0, 2) as u8;

        // If switching from STOP to ON, pending events should now fire
        if mode == 1 && handlers[button_num as usize].pending > 0 {
            STRIG_EVENT_PENDING.store(true, Ordering::SeqCst);
        }

        // If switching to OFF, clear pending events
        if mode == 0 {
            handlers[button_num as usize].pending = 0;
        }
    }
}

/// Check if a STRIG event is pending and return its event ID.
///
/// Called by the generated event check code (QB_CHECK_STRIG_EVENTS macro).
/// Returns 0 if no event is pending, otherwise returns the event ID to dispatch.
///
/// If an event is pending but already being dispatched (re-entrant check),
/// returns 0 to prevent nested handler calls.
#[no_mangle]
pub extern "C" fn qb_strig_check_event() -> u32 {
    // Fast path: no events pending
    if !STRIG_EVENT_PENDING.load(Ordering::SeqCst) {
        return 0;
    }

    // Don't allow re-entrant event dispatch
    if STRIG_CURRENT_EVENT.load(Ordering::SeqCst) != 0 {
        return 0;
    }

    if let Ok(mut handlers) = STRIG_HANDLERS.lock() {
        for i in 0..MAX_STRIG_HANDLERS {
            let handler = &mut handlers[i];
            // Check: handler is ON (active=1), has pending events, and has a valid ID
            if handler.active == 1 && handler.pending > 0 && handler.id != 0 {
                handler.pending -= 1;
                let event_id = handler.id;

                // Update global pending flag if no more events
                let any_pending = handlers.iter().any(|h| h.active == 1 && h.pending > 0);
                if !any_pending {
                    STRIG_EVENT_PENDING.store(false, Ordering::SeqCst);
                }

                // Mark as currently dispatching
                STRIG_CURRENT_EVENT.store(event_id, Ordering::SeqCst);
                return event_id;
            }
        }

        // No events to dispatch
        STRIG_EVENT_PENDING.store(false, Ordering::SeqCst);
    }

    0
}

/// Called after a STRIG event handler returns.
///
/// This clears the "currently dispatching" flag to allow new events.
#[no_mangle]
pub extern "C" fn qb_strig_event_done() {
    STRIG_CURRENT_EVENT.store(0, Ordering::SeqCst);
}

/// Trigger a STRIG event for a specific button press.
///
/// Called internally when a joystick button is pressed.
/// Maps the (joystick, button) pair to a STRIG button number.
fn trigger_strig_event(joy_idx: u32, button_idx: u8) {
    // Map (joystick, button) to STRIG button number:
    // Joy 0, Button 0 -> STRIG(0)
    // Joy 0, Button 1 -> STRIG(2)
    // Joy 1, Button 0 -> STRIG(4)
    // Joy 1, Button 1 -> STRIG(6)
    // STRIG(1,3,5,7) are "current state" checks, not event triggers
    let strig_button = match (joy_idx, button_idx) {
        (0, 0) => 0, // Joy A, Button 1
        (0, 1) => 2, // Joy A, Button 2
        (1, 0) => 4, // Joy B, Button 1
        (1, 1) => 6, // Joy B, Button 2
        _ => return, // Other buttons don't have STRIG handlers
    };

    if let Ok(mut handlers) = STRIG_HANDLERS.lock() {
        let handler = &mut handlers[strig_button];
        // Only queue event if handler is ON (1) or STOP (2)
        if handler.active > 0 && handler.id != 0 {
            // Increment pending count (saturate at 255)
            handler.pending = handler.pending.saturating_add(1);

            // If handler is ON, mark events as pending
            if handler.active == 1 {
                STRIG_EVENT_PENDING.store(true, Ordering::SeqCst);
            }
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
