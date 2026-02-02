//! Minimal NDJSON debug logger for agent instrumentation.
//! Writes to workspace .cursor/debug.log for IDE-window debugging.

const LOG_PATH: &str = "/home/dave/repos/qb64contain/QB64Fresh/.cursor/debug.log";
const LOG_PATH_FALLBACK: &str = "/tmp/qb64fresh_debug.log";

/// Append one NDJSON line. `data` is raw JSON object content, e.g. "\"width\":1280,\"height\":400".
#[allow(dead_code)]
pub fn log(location: &str, message: &str, data: &str, hypothesis_id: &str) {
    let ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;
    let line = format!(
        "{{\"location\":\"{}\",\"message\":\"{}\",\"data\":{{{}}},\"timestamp\":{},\"sessionId\":\"debug-session\",\"runId\":\"run1\",\"hypothesisId\":\"{}\"}}\n",
        location, message, data, ts, hypothesis_id
    );
    if let Some(parent) = std::path::Path::new(LOG_PATH).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(LOG_PATH)
    {
        let _ = std::io::Write::write_all(&mut f, line.as_bytes());
    }
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(LOG_PATH_FALLBACK)
    {
        let _ = std::io::Write::write_all(&mut f, line.as_bytes());
    }
}
