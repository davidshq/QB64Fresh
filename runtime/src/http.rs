//! HTTP client state for libqb_http_* API.
//!
//! Provides handle-based HTTP GET: open URL, buffer response body, then read
//! via get_length / get / get_fixed. Matches QB64pe libqb http.h semantics
//! (libcurl) for compatibility.

use std::collections::HashMap;
use std::ffi::CString;
use std::sync::Mutex;

/// Per-handle state: response body buffer and metadata.
struct HttpHandle {
    /// Response body (consumed from front by get/get_fixed).
    buffer: Vec<u8>,
    /// HTTP status code, or -1 if not available.
    status_code: i32,
    /// Effective URL as C string; valid for handle lifetime (for FFI get_url).
    effective_url_c: Option<CString>,
    /// Content-Length from header if present.
    content_length: Option<u64>,
    /// True after close; handle may still be in map until removed.
    closed: bool,
}

/// Global HTTP handle table and init flag.
struct HttpState {
    handles: HashMap<i32, HttpHandle>,
    initialized: bool,
}

static HTTP_STATE: Mutex<Option<HttpState>> = Mutex::new(None);

fn with_state<T, F>(f: F) -> T
where
    F: FnOnce(&mut HttpState) -> T,
{
    let mut guard = HTTP_STATE.lock().unwrap();
    if guard.is_none() {
        *guard = Some(HttpState {
            handles: HashMap::new(),
            initialized: false,
        });
    }
    f(guard.as_mut().unwrap())
}

/// Initialize the HTTP subsystem. Idempotent.
pub fn http_init() {
    with_state(|s| {
        s.initialized = true;
    });
}

/// Shut down the HTTP subsystem: close all handles and clear state.
pub fn http_stop() {
    let mut guard = HTTP_STATE.lock().unwrap();
    if let Some(ref mut s) = *guard {
        s.handles.clear();
        s.initialized = false;
    }
}

/// Open a URL with the given handle id. Performs a blocking GET and buffers
/// the response. Returns true on success, false on error (invalid URL,
/// connection failed, etc.).
#[cfg(feature = "http")]
pub fn http_open(url: &str, handle_id: i32) -> bool {
    use reqwest::blocking::ClientBuilder;

    let client = match ClientBuilder::new()
        .redirect(reqwest::redirect::Policy::limited(10))
        .build()
    {
        Ok(c) => c,
        Err(_) => return false,
    };

    let response = match client.get(url).send() {
        Ok(r) => r,
        Err(_) => return false,
    };

    let status_code = response.status().as_u16() as i32;
    let effective_url = response.url().as_str().to_string();
    let effective_url_c = CString::new(effective_url.as_bytes()).ok();
    let content_length = response.content_length();
    let body = match response.bytes() {
        Ok(b) => b.to_vec(),
        Err(_) => return false,
    };

    with_state(|s| {
        s.handles.insert(
            handle_id,
            HttpHandle {
                buffer: body,
                status_code,
                effective_url_c,
                content_length,
                closed: false,
            },
        );
    });
    true
}

/// Stub when http feature is disabled: open always fails.
#[cfg(not(feature = "http"))]
pub fn http_open(_url: &str, _handle_id: i32) -> bool {
    false
}

/// Close the handle and remove it from the table. Returns true if the handle existed.
pub fn http_close(handle_id: i32) -> bool {
    with_state(|s| s.handles.remove(&handle_id).is_some())
}

/// Returns: 1 if handle exists and is not closed, 0 if closed, -1 if invalid handle.
pub fn http_connected(handle_id: i32) -> i32 {
    with_state(|s| match s.handles.get(&handle_id) {
        Some(h) => {
            if h.closed {
                0
            } else {
                1
            }
        }
        None => -1,
    })
}

/// Bytes available to read. Returns Some(len) or None if invalid handle.
pub fn http_get_length(handle_id: i32) -> Option<usize> {
    with_state(|s| s.handles.get(&handle_id).map(|h| h.buffer.len()))
}

/// Content-Length from header. Returns Some(len) or None if not present / invalid handle.
pub fn http_get_content_length(handle_id: i32) -> Option<u64> {
    with_state(|s| s.handles.get(&handle_id).and_then(|h| h.content_length))
}

/// HTTP status code, or -1 if invalid handle.
pub fn http_get_status_code(handle_id: i32) -> i32 {
    with_state(|s| {
        s.handles
            .get(&handle_id)
            .map(|h| h.status_code)
            .unwrap_or(-1)
    })
}

/// Effective URL (after redirects) as raw C pointer. Returns null if invalid
/// handle or no URL. Valid until handle is closed; caller must not free.
pub fn http_get_url_ptr(handle_id: i32) -> *const std::os::raw::c_char {
    with_state(|s| {
        s.handles
            .get(&handle_id)
            .and_then(|h| h.effective_url_c.as_ref())
            .map(|c| c.as_ptr())
            .unwrap_or(std::ptr::null())
    })
}

/// Read up to `buf_len` bytes into `buf`, set `*bytes_read` to actual count.
/// Returns true on success, false if invalid handle.
pub fn http_get(handle_id: i32, buf: &mut [u8], bytes_read: &mut usize) -> bool {
    with_state(|s| {
        let h = match s.handles.get_mut(&handle_id) {
            Some(h) => h,
            None => return false,
        };
        let n = h.buffer.len().min(buf.len());
        buf[..n].copy_from_slice(&h.buffer[..n]);
        h.buffer.drain(..n);
        *bytes_read = n;
        true
    })
}

/// Read exactly `len` bytes into `buf`. Returns true if that many bytes were
/// available and read, false otherwise.
pub fn http_get_fixed(handle_id: i32, buf: &mut [u8], len: usize) -> bool {
    if buf.len() < len {
        return false;
    }
    with_state(|s| {
        let h = match s.handles.get_mut(&handle_id) {
            Some(h) => h,
            None => return false,
        };
        if h.buffer.len() < len {
            return false;
        }
        buf[..len].copy_from_slice(&h.buffer[..len]);
        h.buffer.drain(..len);
        true
    })
}
