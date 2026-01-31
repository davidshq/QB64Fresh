//! Event trapping system for QB64Fresh runtime.
//!
//! This module implements event handlers for:
//! - `ON KEY(n) GOSUB` - Keyboard event trapping (keys 1-31)
//! - `ON TIMER(n) GOSUB` - Timer-based event trapping
//! - `ON UEVENT GOSUB` - User-defined event trapping
//!
//! # Architecture
//!
//! Event handlers are registered with label addresses (computed goto targets).
//! The generated C code checks for events and jumps to the registered labels.
//! This module provides the infrastructure to:
//! 1. Register handlers (store label addresses)
//! 2. Check for events (return which events occurred)
//! 3. Control event trapping (ON/OFF/STOP)
//!
//! The actual jumping to labels happens in the generated C code, not in Rust.
//!
//! ## Keyboard Events (ON KEY)
//!
//! Keyboard events use SDL2's event system. Key numbers 1-31 map to:
//! - 1-10: Function keys F1-F10
//! - 11-14: Cursor keys (Up, Left, Right, Down)
//! - 15-25: User-defined keys
//! - 30-31: Special keys
//!
//! Events are checked during the graphics event loop and during blocking operations
//! (INPUT, _DELAY, etc.).
//!
//! ## Timer Events (ON TIMER)
//!
//! Timer events use a separate thread that sleeps for the specified interval,
//! then triggers the handler. Multiple timers can be active simultaneously.
//!
//! ## User Events (ON UEVENT)
//!
//! User events are triggered programmatically via `UEVENT` statement.
//! They use a simple flag-based system.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Instant;

/// Event handler target (label address for computed goto).
///
/// # Safety
/// Raw pointers are not Send by default, but these are just memory addresses
/// (computed goto labels) that are stored and never dereferenced across threads.
/// We mark this as Send to allow storing in Mutex.
pub type EventHandler = *mut ();

// Safety: EventHandler is just a memory address (computed goto label).
// It's never dereferenced, just stored and passed to C code for computed goto.
unsafe impl Send for KeyEventRegistry {}
unsafe impl Send for TimerEventRegistry {}
unsafe impl Send for UserEventRegistry {}
unsafe impl Send for EventRegistry {}

/// Key event handler registry.
///
/// Maps key numbers (1-31) to their handler labels.
/// Key 0 means "disable handler for this key".
#[derive(Default)]
pub struct KeyEventRegistry {
    /// Map from key number to handler label
    handlers: HashMap<i32, EventHandler>,
    /// Map from key number to enabled state (true = ON, false = OFF, None = STOP)
    enabled: HashMap<i32, Option<bool>>,
    /// Queue of key events waiting to be processed
    /// Stores key numbers that have been pressed and need handler invocation
    event_queue: Vec<i32>,
}

impl KeyEventRegistry {
    /// Creates a new key event registry.
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
            enabled: HashMap::new(),
            event_queue: Vec::new(),
        }
    }

    /// Registers a key event handler.
    ///
    /// # Arguments
    ///
    /// * `key_num` - Key number (1-31, or 0 to disable all key trapping)
    /// * `target` - Handler label address (ignored if key_num == 0)
    pub fn register(&mut self, key_num: i32, target: EventHandler) {
        if key_num == 0 {
            // Disable all key trapping (QB64 behavior: ON KEY(0) disables all)
            self.handlers.clear();
            self.enabled.clear();
            self.event_queue.clear();
        } else if key_num >= 1 && key_num <= 31 {
            // Only register valid key numbers (1-31)
            self.handlers.insert(key_num, target);
            // Default to ON when registering
            if !self.enabled.contains_key(&key_num) {
                self.enabled.insert(key_num, Some(true));
            }
        }
        // Ignore invalid key numbers (< 0 or > 31)
    }

    /// Sets the control mode for a key (ON/OFF/STOP).
    ///
    /// # Arguments
    ///
    /// * `key_num` - Key number
    /// * `mode` - 0 = OFF, 1 = ON, 2 = STOP
    pub fn set_control(&mut self, key_num: i32, mode: i32) {
        let enabled = match mode {
            0 => Some(false), // OFF
            1 => Some(true),  // ON
            2 => None,        // STOP (suspended)
            _ => return,
        };
        self.enabled.insert(key_num, enabled);
    }

    /// Gets the handler for a key if it's enabled.
    ///
    /// Returns None if the key has no handler or is disabled/stopped.
    pub fn get_handler(&self, key_num: i32) -> Option<EventHandler> {
        if let Some(enabled) = self.enabled.get(&key_num) {
            if *enabled == Some(true) {
                return self.handlers.get(&key_num).copied();
            }
        }
        None
    }

    /// Queues a key event for processing.
    ///
    /// Called by the graphics backend when a key is pressed.
    /// Only queues if the key has a handler and is enabled.
    /// Validates key_num is in range 1-31.
    pub fn queue_key_event(&mut self, key_num: i32) {
        // Validate key number
        if key_num < 1 || key_num > 31 {
            return;
        }

        if let Some(handler) = self.get_handler(key_num) {
            if handler != std::ptr::null_mut() {
                // Only queue if not already in queue (avoid duplicates)
                // This prevents the same key from being queued multiple times
                // until it's processed
                if !self.event_queue.contains(&key_num) {
                    self.event_queue.push(key_num);
                }
            }
        }
    }

    /// Gets and removes the next key event from the queue.
    ///
    /// Returns the key number, or 0 if no event is pending.
    pub fn pop_event(&mut self) -> i32 {
        self.event_queue.pop().unwrap_or(0)
    }

    /// Checks if there's a pending key event.
    pub fn has_event(&self) -> bool {
        !self.event_queue.is_empty()
    }

    /// Clears all handlers (called on RUN).
    pub fn clear_all(&mut self) {
        self.handlers.clear();
        self.enabled.clear();
        self.event_queue.clear();
    }
}

/// Timer event handler.
#[derive(Clone)]
struct TimerHandler {
    /// Interval in seconds
    interval: f32,
    /// Handler label address
    target: EventHandler,
    /// Last time the timer fired
    last_fire: Instant,
}

/// Timer event registry.
///
/// Manages multiple active timers. Each timer fires at its specified interval.
#[derive(Default)]
pub struct TimerEventRegistry {
    /// Active timers
    timers: Vec<TimerHandler>,
    /// Control mode: Some(true) = ON, Some(false) = OFF, None = STOP
    enabled: Option<bool>,
}

impl TimerEventRegistry {
    /// Creates a new timer event registry.
    pub fn new() -> Self {
        Self {
            timers: Vec::new(),
            enabled: Some(true), // Default to ON
        }
    }

    /// Registers a timer event handler.
    ///
    /// # Arguments
    ///
    /// * `interval` - Interval in seconds (must be > 0)
    /// * `target` - Handler label address
    pub fn register(&mut self, interval: f32, target: EventHandler) {
        // Validate interval
        if interval <= 0.0 {
            return; // Invalid interval, ignore
        }

        // Remove existing timer with same target (replace it)
        self.timers.retain(|t| t.target != target);

        // Add new timer
        self.timers.push(TimerHandler {
            interval,
            target,
            last_fire: Instant::now(),
        });
    }

    /// Sets the control mode (ON/OFF/STOP).
    ///
    /// # Arguments
    ///
    /// * `mode` - 0 = OFF, 1 = ON, 2 = STOP
    pub fn set_control(&mut self, mode: i32) {
        self.enabled = match mode {
            0 => Some(false), // OFF
            1 => Some(true),  // ON
            2 => None,        // STOP (suspended)
            _ => return,
        };
    }

    /// Checks for timers that should fire and returns their handlers.
    ///
    /// Returns a list of handler targets that should be invoked.
    /// This mutates the timer state (updates last_fire).
    pub fn check_timers(&mut self) -> Vec<EventHandler> {
        if self.enabled != Some(true) {
            return Vec::new();
        }

        let now = Instant::now();
        let mut to_fire = Vec::new();

        for timer in &mut self.timers {
            let elapsed = now.duration_since(timer.last_fire);
            if elapsed.as_secs_f32() >= timer.interval {
                to_fire.push(timer.target);
                timer.last_fire = now; // Update last_fire to prevent immediate re-firing
            }
        }

        to_fire
    }

    /// Clears all timers (called on RUN).
    pub fn clear_all(&mut self) {
        self.timers.clear();
        self.enabled = Some(true);
    }
}

/// User event registry.
///
/// Manages user-defined events triggered via UEVENT statement.
#[derive(Default)]
pub struct UserEventRegistry {
    /// Handler label address
    handler: Option<EventHandler>,
    /// Pending event flag
    pending: bool,
    /// Control mode: Some(true) = ON, Some(false) = OFF, None = STOP
    enabled: Option<bool>,
}

impl UserEventRegistry {
    /// Creates a new user event registry.
    pub fn new() -> Self {
        Self {
            handler: None,
            pending: false,
            enabled: Some(true), // Default to ON
        }
    }

    /// Registers a user event handler.
    ///
    /// # Arguments
    ///
    /// * `target` - Handler label address
    pub fn register(&mut self, target: EventHandler) {
        self.handler = Some(target);
        // Default to ON when registering if not already set
        if self.enabled.is_none() {
            self.enabled = Some(true);
        }
    }

    /// Sets the control mode (ON/OFF/STOP).
    ///
    /// # Arguments
    ///
    /// * `mode` - 0 = OFF, 1 = ON, 2 = STOP
    pub fn set_control(&mut self, mode: i32) {
        self.enabled = match mode {
            0 => Some(false), // OFF
            1 => Some(true),  // ON
            2 => None,        // STOP (suspended)
            _ => return,
        };
    }

    /// Triggers a user event.
    ///
    /// Sets the pending flag if the handler is enabled.
    pub fn trigger(&mut self) {
        if self.enabled == Some(true) && self.handler.is_some() {
            self.pending = true;
        }
    }

    /// Checks if there's a pending user event and returns the handler.
    ///
    /// Clears the pending flag after checking.
    pub fn check_event(&mut self) -> Option<EventHandler> {
        if self.pending && self.enabled == Some(true) {
            self.pending = false;
            self.handler
        } else {
            None
        }
    }

    /// Clears the handler (called on RUN).
    pub fn clear_all(&mut self) {
        self.handler = None;
        self.pending = false;
        self.enabled = Some(true);
    }
}

/// Global event registry.
///
/// Thread-safe wrapper around all event registries.
#[derive(Clone)]
pub struct EventRegistry {
    keys: Arc<Mutex<KeyEventRegistry>>,
    timers: Arc<Mutex<TimerEventRegistry>>,
    user: Arc<Mutex<UserEventRegistry>>,
}

impl EventRegistry {
    /// Creates a new event registry.
    pub fn new() -> Self {
        Self {
            keys: Arc::new(Mutex::new(KeyEventRegistry::new())),
            timers: Arc::new(Mutex::new(TimerEventRegistry::new())),
            user: Arc::new(Mutex::new(UserEventRegistry::new())),
        }
    }

    /// Gets the key event registry.
    pub fn keys(&self) -> &Arc<Mutex<KeyEventRegistry>> {
        &self.keys
    }

    /// Gets the timer event registry.
    pub fn timers(&self) -> &Arc<Mutex<TimerEventRegistry>> {
        &self.timers
    }

    /// Gets the user event registry.
    pub fn user(&self) -> &Arc<Mutex<UserEventRegistry>> {
        &self.user
    }

    /// Clears all event handlers (called on RUN).
    pub fn clear_all(&self) {
        self.keys.lock().unwrap().clear_all();
        self.timers.lock().unwrap().clear_all();
        self.user.lock().unwrap().clear_all();
    }
}

/// Global event registry instance.
static EVENT_REGISTRY: Mutex<Option<EventRegistry>> = Mutex::new(None);

/// Set by `qb64_custom_event(QB64_EVENT_CLOSE, ...)`. Main loop should check and exit.
/// libqb-compatible: QB64pe sets exit_value |= 1 on close event.
#[no_mangle]
pub static mut qb64_exit_requested: i32 = 0;

/// Event type constants (libqb event.h compatibility).
pub const QB64_EVENT_CLOSE: i32 = 1;
pub const QB64_EVENT_KEY: i32 = 2;
pub const QB64_EVENT_RELATIVE_MOUSE_MOVEMENT: i32 = 3;
pub const QB64_EVENT_FILE_DROP: i32 = 4;

/// Gets or creates the global event registry.
fn get_registry() -> EventRegistry {
    let mut registry = EVENT_REGISTRY.lock().unwrap();
    if registry.is_none() {
        *registry = Some(EventRegistry::new());
    }
    registry.as_ref().unwrap().clone()
}

/// Registers a key event handler.
///
/// # Safety
/// This function is safe to call from C, but `target` must be a valid label address.
#[no_mangle]
pub unsafe extern "C" fn qb_on_key(key_num: i32, target: *mut ()) {
    let registry = get_registry();
    registry.keys().lock().unwrap().register(key_num, target);
}

/// Controls key event trapping (ON/OFF/STOP).
///
/// # Arguments
///
/// * `key_num` - Key number
/// * `mode` - 0 = OFF, 1 = ON, 2 = STOP
#[no_mangle]
pub extern "C" fn qb_key_control(key_num: i32, mode: i32) {
    let registry = get_registry();
    registry.keys().lock().unwrap().set_control(key_num, mode);
}

/// Registers a timer event handler.
///
/// # Safety
/// This function is safe to call from C, but `target` must be a valid label address.
#[no_mangle]
pub unsafe extern "C" fn qb_on_timer(interval: f32, target: *mut ()) {
    let registry = get_registry();
    registry.timers().lock().unwrap().register(interval, target);
}

/// Controls timer event trapping (ON/OFF/STOP).
///
/// # Arguments
///
/// * `mode` - 0 = OFF, 1 = ON, 2 = STOP
#[no_mangle]
pub extern "C" fn qb_timer_control(mode: i32) {
    let registry = get_registry();
    registry.timers().lock().unwrap().set_control(mode);
}

/// Registers a user event handler.
///
/// # Safety
/// This function is safe to call from C, but `target` must be a valid label address.
#[no_mangle]
pub unsafe extern "C" fn qb_on_uevent(target: *mut ()) {
    let registry = get_registry();
    registry.user().lock().unwrap().register(target);
}

/// Controls user event trapping (ON/OFF/STOP).
///
/// # Arguments
///
/// * `mode` - 0 = OFF, 1 = ON, 2 = STOP
#[no_mangle]
pub extern "C" fn qb_uevent_control(mode: i32) {
    let registry = get_registry();
    registry.user().lock().unwrap().set_control(mode);
}

/// Triggers a user event.
#[no_mangle]
pub extern "C" fn qb_uevent_trigger() {
    let registry = get_registry();
    registry.user().lock().unwrap().trigger();
}

/// Clears all event handlers (called on RUN).
#[no_mangle]
pub extern "C" fn qb_events_clear_all() {
    let registry = get_registry();
    registry.clear_all();
}

// -----------------------------------------------------------------------------
// Legacy event stubs (ON COM, ON PEN, ON SIGNAL) — external runtime parity
// -----------------------------------------------------------------------------
// These are DOS-era event mechanisms that don't map to modern systems.
// Stubs warn once and are no-ops so generated code linking against
// libqb64fresh_rt (--runtime external) resolves the symbols.

static ON_COM_WARN: std::sync::Once = std::sync::Once::new();
static ON_PEN_WARN: std::sync::Once = std::sync::Once::new();
static ON_SIGNAL_WARN: std::sync::Once = std::sync::Once::new();

/// ON COM — serial port event trapping (not implemented; stub for link parity).
///
/// # Safety
/// Safe to call from C; target is a label address and is not dereferenced.
#[no_mangle]
pub unsafe extern "C" fn qb_on_com(_port_num: i32, _target: *mut ()) {
    ON_COM_WARN.call_once(|| {
        eprintln!("QB64Fresh: ON COM is not implemented (serial port event trapping)");
    });
}

/// COM(n) ON/OFF/STOP — control serial port event trapping (stub).
#[no_mangle]
pub extern "C" fn qb_com_control(_port_num: i32, _mode: i32) {
    ON_COM_WARN.call_once(|| {
        eprintln!("QB64Fresh: ON COM is not implemented (serial port event trapping)");
    });
}

/// ON PEN — light pen event trapping (not implemented; stub for link parity).
///
/// # Safety
/// Safe to call from C; target is a label address and is not dereferenced.
#[no_mangle]
pub unsafe extern "C" fn qb_on_pen(_target: *mut ()) {
    ON_PEN_WARN.call_once(|| {
        eprintln!("QB64Fresh: ON PEN is not implemented (light pen event trapping)");
    });
}

/// PEN ON/OFF/STOP — control light pen event trapping (stub).
#[no_mangle]
pub extern "C" fn qb_pen_control(_mode: i32) {
    ON_PEN_WARN.call_once(|| {
        eprintln!("QB64Fresh: ON PEN is not implemented (light pen event trapping)");
    });
}

/// ON SIGNAL — BASIC signal trapping (not implemented; stub for link parity).
///
/// # Safety
/// Safe to call from C; target is a label address and is not dereferenced.
#[no_mangle]
pub unsafe extern "C" fn qb_on_signal(_signal_num: i32, _target: *mut ()) {
    ON_SIGNAL_WARN.call_once(|| {
        eprintln!("QB64Fresh: ON SIGNAL is not implemented (BASIC signal trapping)");
    });
}

/// SIGNAL(n) ON/OFF/STOP — control signal trapping (stub).
#[no_mangle]
pub extern "C" fn qb_signal_control(_signal_num: i32, _mode: i32) {
    ON_SIGNAL_WARN.call_once(|| {
        eprintln!("QB64Fresh: ON SIGNAL is not implemented (BASIC signal trapping)");
    });
}

/// Custom event callback (libqb qb64_custom_event).
///
/// Called by graphics/window code to report close, key, relative mouse, file drop.
/// Returns 0 if event was handled, -1 if unknown or unhandled.
///
/// # Arguments
///
/// * `event` - One of QB64_EVENT_CLOSE, QB64_EVENT_KEY, QB64_EVENT_RELATIVE_MOUSE_MOVEMENT, QB64_EVENT_FILE_DROP
/// * `v1`..`v8` - Event-specific integer parameters
/// * `p1`, `p2` - Event-specific pointers (e.g. HDROP for file drop on Windows)
///
/// # Safety
///
/// Safe to call from C; p1/p2 may be null or platform-specific handles.
#[no_mangle]
pub unsafe extern "C" fn qb64_custom_event(
    event: i32,
    v1: i32,
    v2: i32,
    _v3: i32,
    _v4: i32,
    _v5: i32,
    _v6: i32,
    _v7: i32,
    _v8: i32,
    _p1: *mut std::ffi::c_void,
    _p2: *mut std::ffi::c_void,
) -> i32 {
    if event == QB64_EVENT_CLOSE {
        qb64_exit_requested |= 1;
        return 0;
    }
    if event == QB64_EVENT_KEY {
        // QB64pe handles PAUSE/BREAK via keydown_vk/keyup_vk; we stub for now
        let _ = (v1, v2);
        return -1;
    }
    if event == QB64_EVENT_RELATIVE_MOUSE_MOVEMENT {
        // QB64pe calls qb64_custom_event_relative_mouse_movement(v1, v2); stub for now
        let _ = (v1, v2);
        return 0;
    }
    if event == QB64_EVENT_FILE_DROP {
        // QB64pe on Windows: sets totalDroppedFiles from HDROP; stub on other platforms
        return 0;
    }
    -1
}

/// Queues a key event from the graphics backend.
///
/// This function is called by the graphics backend when a key is pressed.
/// It queues the key number for processing by the generated C code.
///
/// # Arguments
///
/// * `key_num` - Key number (1-31)
#[no_mangle]
pub extern "C" fn qb_queue_key_event(key_num: i32) {
    let registry = get_registry();
    let mut keys = registry.keys().lock().unwrap();
    keys.queue_key_event(key_num);
}

/// Checks for a pending key event and returns the key number.
///
/// Returns 0 if no key event is pending.
/// The generated C code should check this and jump to the registered handler.
///
/// This function is called from the generated C code during event polling.
/// It checks the key event queue and returns the first pending key number.
#[no_mangle]
pub extern "C" fn qb_check_key_event() -> i32 {
    let registry = get_registry();
    let keys = registry.keys().lock().unwrap();

    // Return first key in queue, or 0 if empty
    keys.event_queue.first().copied().unwrap_or(0)
}

/// Checks for a pending timer event.
///
/// Returns 1 if a timer event is pending, 0 otherwise.
/// This is a non-mutating check - it doesn't update timer state.
/// The generated C code should check this, then call qb_get_timer_handler() to get the handler.
#[no_mangle]
pub extern "C" fn qb_check_timer_event() -> i32 {
    let registry = get_registry();
    let timers = registry.timers().lock().unwrap();

    // Check if any timer should fire (without mutating state)
    if timers.enabled != Some(true) {
        return 0;
    }

    let now = std::time::Instant::now();
    for timer in &timers.timers {
        let elapsed = now.duration_since(timer.last_fire);
        if elapsed.as_secs_f32() >= timer.interval {
            return 1;
        }
    }
    0
}

/// Checks for a pending user event.
///
/// Returns 1 if a user event is pending, 0 otherwise.
/// This is a non-mutating check - it doesn't consume the event.
/// The generated C code should check this, then call qb_get_uevent_handler() to get the handler.
#[no_mangle]
pub extern "C" fn qb_check_uevent() -> i32 {
    let registry = get_registry();
    let user = registry.user().lock().unwrap();

    if user.pending && user.enabled == Some(true) && user.handler.is_some() {
        1
    } else {
        0
    }
}

/// Gets the handler label for a key event and removes it from the queue.
///
/// Returns the label address for the key event, or NULL if no event.
/// The generated C code uses this to get the label to jump to.
/// This also removes the FIRST occurrence of the key from the queue (consumes the event).
///
/// # Arguments
///
/// * `key_num` - Key number from qb_check_key_event()
///
/// # Safety
/// Returns a raw pointer that should be used with computed goto.
///
/// # Note
/// The typical usage pattern is:
/// 1. Call `qb_check_key_event()` to get the key number
/// 2. Call `qb_get_key_handler(key_num)` with that key number to get the handler
/// This removes the first occurrence of that key from the queue.
#[no_mangle]
pub unsafe extern "C" fn qb_get_key_handler(key_num: i32) -> *mut () {
    if key_num == 0 || key_num < 1 || key_num > 31 {
        return std::ptr::null_mut();
    }

    let registry = get_registry();
    let mut keys = registry.keys().lock().unwrap();

    // Remove the FIRST occurrence of this key from the queue (consume the event)
    // Note: queue_key_event() prevents duplicates, so there should only be one
    if let Some(pos) = keys.event_queue.iter().position(|&k| k == key_num) {
        keys.event_queue.remove(pos);
    }

    // Return the handler for this key
    keys.get_handler(key_num).unwrap_or(std::ptr::null_mut())
}

/// Gets the handler label for a timer event.
///
/// Returns the label address for the active timer, or NULL if no timer fired.
/// The generated C code uses this to get the label to jump to.
///
/// # Safety
/// Returns a raw pointer that should be used with computed goto.
#[no_mangle]
pub unsafe extern "C" fn qb_get_timer_handler() -> *mut () {
    let registry = get_registry();
    let mut timers = registry.timers().lock().unwrap();

    let to_fire = timers.check_timers();
    if !to_fire.is_empty() {
        to_fire[0] // Return first timer that fired
    } else {
        std::ptr::null_mut()
    }
}

/// Gets the handler label for a user event.
///
/// Returns the label address for the user event handler, or NULL if no event.
/// The generated C code uses this to get the label to jump to.
///
/// # Safety
/// Returns a raw pointer that should be used with computed goto.
#[no_mangle]
pub unsafe extern "C" fn qb_get_uevent_handler() -> *mut () {
    let registry = get_registry();
    let mut user = registry.user().lock().unwrap();
    user.check_event().unwrap_or(std::ptr::null_mut())
}
