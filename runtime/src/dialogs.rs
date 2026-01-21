//! Native file dialog support for QB64Fresh
//!
//! Provides cross-platform file dialogs using the `rfd` crate:
//! - _OPENFILEDIALOG$ - Select file(s) to open
//! - _SAVEFILEDIALOG$ - Select file to save
//! - _SELECTFOLDERDIALOG$ - Select a folder
//! - _MESSAGEBOX - Show a message box with buttons
//! - _INPUTBOX$ - Show an input dialog

use crate::string::{qb_string_empty, qb_string_from_bytes, QbString};
use std::ffi::CStr;
use std::os::raw::c_char;

/// Open a file dialog and return the selected file path.
///
/// # Arguments
/// * `title` - Dialog title (C string, can be NULL for default)
/// * `initial_dir` - Initial directory (C string, can be NULL for current dir)
/// * `filter` - File filter pattern like "*.txt" or "*.bas;*.bi" (C string, can be NULL)
///
/// # Returns
/// A QbString containing the selected path, or empty string if cancelled.
///
/// # Safety
/// All string parameters must be valid C strings or NULL.
#[no_mangle]
pub unsafe extern "C" fn qb_openfiledialog(
    title: *const c_char,
    initial_dir: *const c_char,
    filter: *const c_char,
) -> *mut QbString {
    #[cfg(feature = "dialogs")]
    {
        use rfd::FileDialog;

        let mut dialog = FileDialog::new();

        // Set title if provided
        if !title.is_null() {
            if let Ok(t) = CStr::from_ptr(title).to_str() {
                if !t.is_empty() {
                    dialog = dialog.set_title(t);
                }
            }
        }

        // Set initial directory if provided
        if !initial_dir.is_null() {
            if let Ok(dir) = CStr::from_ptr(initial_dir).to_str() {
                if !dir.is_empty() {
                    dialog = dialog.set_directory(dir);
                }
            }
        }

        // Set file filter if provided
        if !filter.is_null() {
            if let Ok(f) = CStr::from_ptr(filter).to_str() {
                if !f.is_empty() {
                    // Parse filter pattern (e.g., "*.txt" or "*.bas;*.bi")
                    let extensions: Vec<&str> = f
                        .split(';')
                        .filter_map(|ext| ext.trim().strip_prefix("*."))
                        .collect();
                    if !extensions.is_empty() {
                        dialog = dialog.add_filter("Files", &extensions);
                    }
                }
            }
        }

        // Show dialog and get result
        if let Some(path) = dialog.pick_file() {
            if let Some(path_str) = path.to_str() {
                return qb_string_from_bytes(path_str.as_ptr(), path_str.len());
            }
        }

        qb_string_empty()
    }

    #[cfg(not(feature = "dialogs"))]
    {
        let _ = (title, initial_dir, filter);
        eprintln!("Warning: File dialogs require the 'dialogs' feature");
        qb_string_empty()
    }
}

/// Open a save file dialog and return the selected file path.
///
/// # Arguments
/// * `title` - Dialog title (C string, can be NULL for default)
/// * `initial_dir` - Initial directory (C string, can be NULL for current dir)
/// * `default_name` - Default file name (C string, can be NULL)
/// * `filter` - File filter pattern like "*.txt" (C string, can be NULL)
///
/// # Returns
/// A QbString containing the selected path, or empty string if cancelled.
///
/// # Safety
/// All string parameters must be valid C strings or NULL.
#[no_mangle]
pub unsafe extern "C" fn qb_savefiledialog(
    title: *const c_char,
    initial_dir: *const c_char,
    default_name: *const c_char,
    filter: *const c_char,
) -> *mut QbString {
    #[cfg(feature = "dialogs")]
    {
        use rfd::FileDialog;

        let mut dialog = FileDialog::new();

        // Set title if provided
        if !title.is_null() {
            if let Ok(t) = CStr::from_ptr(title).to_str() {
                if !t.is_empty() {
                    dialog = dialog.set_title(t);
                }
            }
        }

        // Set initial directory if provided
        if !initial_dir.is_null() {
            if let Ok(dir) = CStr::from_ptr(initial_dir).to_str() {
                if !dir.is_empty() {
                    dialog = dialog.set_directory(dir);
                }
            }
        }

        // Set default file name if provided
        if !default_name.is_null() {
            if let Ok(name) = CStr::from_ptr(default_name).to_str() {
                if !name.is_empty() {
                    dialog = dialog.set_file_name(name);
                }
            }
        }

        // Set file filter if provided
        if !filter.is_null() {
            if let Ok(f) = CStr::from_ptr(filter).to_str() {
                if !f.is_empty() {
                    let extensions: Vec<&str> = f
                        .split(';')
                        .filter_map(|ext| ext.trim().strip_prefix("*."))
                        .collect();
                    if !extensions.is_empty() {
                        dialog = dialog.add_filter("Files", &extensions);
                    }
                }
            }
        }

        // Show dialog and get result
        if let Some(path) = dialog.save_file() {
            if let Some(path_str) = path.to_str() {
                return qb_string_from_bytes(path_str.as_ptr(), path_str.len());
            }
        }

        qb_string_empty()
    }

    #[cfg(not(feature = "dialogs"))]
    {
        let _ = (title, initial_dir, default_name, filter);
        eprintln!("Warning: File dialogs require the 'dialogs' feature");
        qb_string_empty()
    }
}

/// Open a folder selection dialog and return the selected folder path.
///
/// # Arguments
/// * `title` - Dialog title (C string, can be NULL for default)
/// * `initial_dir` - Initial directory (C string, can be NULL for current dir)
///
/// # Returns
/// A QbString containing the selected folder path, or empty string if cancelled.
///
/// # Safety
/// All string parameters must be valid C strings or NULL.
#[no_mangle]
pub unsafe extern "C" fn qb_selectfolderdialog(
    title: *const c_char,
    initial_dir: *const c_char,
) -> *mut QbString {
    #[cfg(feature = "dialogs")]
    {
        use rfd::FileDialog;

        let mut dialog = FileDialog::new();

        // Set title if provided
        if !title.is_null() {
            if let Ok(t) = CStr::from_ptr(title).to_str() {
                if !t.is_empty() {
                    dialog = dialog.set_title(t);
                }
            }
        }

        // Set initial directory if provided
        if !initial_dir.is_null() {
            if let Ok(dir) = CStr::from_ptr(initial_dir).to_str() {
                if !dir.is_empty() {
                    dialog = dialog.set_directory(dir);
                }
            }
        }

        // Show folder picker dialog
        if let Some(path) = dialog.pick_folder() {
            if let Some(path_str) = path.to_str() {
                return qb_string_from_bytes(path_str.as_ptr(), path_str.len());
            }
        }

        qb_string_empty()
    }

    #[cfg(not(feature = "dialogs"))]
    {
        let _ = (title, initial_dir);
        eprintln!("Warning: File dialogs require the 'dialogs' feature");
        qb_string_empty()
    }
}

/// Message box button types (matches QB64 _MESSAGEBOX)
pub const QB_MB_OK: i32 = 0;
pub const QB_MB_OKCANCEL: i32 = 1;
pub const QB_MB_ABORTRETRYIGNORE: i32 = 2;
pub const QB_MB_YESNOCANCEL: i32 = 3;
pub const QB_MB_YESNO: i32 = 4;
pub const QB_MB_RETRYCANCEL: i32 = 5;

/// Message box return values
pub const QB_MBRET_OK: i32 = 1;
pub const QB_MBRET_CANCEL: i32 = 2;
pub const QB_MBRET_ABORT: i32 = 3;
pub const QB_MBRET_RETRY: i32 = 4;
pub const QB_MBRET_IGNORE: i32 = 5;
pub const QB_MBRET_YES: i32 = 6;
pub const QB_MBRET_NO: i32 = 7;

/// Show a message box dialog.
///
/// # Arguments
/// * `title` - Dialog title (C string)
/// * `message` - Message text (C string)
/// * `buttons` - Button type (QB_MB_* constant)
///
/// # Returns
/// Button clicked (QB_MBRET_* constant)
///
/// # Safety
/// String parameters must be valid C strings.
#[no_mangle]
pub unsafe extern "C" fn qb_messagebox_ex(
    title: *const c_char,
    message: *const c_char,
    buttons: i32,
) -> i32 {
    #[cfg(feature = "dialogs")]
    {
        use rfd::{MessageButtons, MessageDialog, MessageLevel};

        let title_str = if !title.is_null() {
            CStr::from_ptr(title).to_str().unwrap_or("Message")
        } else {
            "Message"
        };

        let message_str = if !message.is_null() {
            CStr::from_ptr(message).to_str().unwrap_or("")
        } else {
            ""
        };

        let rfd_buttons = match buttons {
            QB_MB_OK => MessageButtons::Ok,
            QB_MB_OKCANCEL => MessageButtons::OkCancel,
            QB_MB_YESNO => MessageButtons::YesNo,
            QB_MB_YESNOCANCEL => MessageButtons::YesNoCancel,
            // rfd doesn't support all QB64 button combinations directly
            // Fall back to OkCancel for unsupported types
            _ => MessageButtons::OkCancel,
        };

        let dialog = MessageDialog::new()
            .set_title(title_str)
            .set_description(message_str)
            .set_level(MessageLevel::Info)
            .set_buttons(rfd_buttons);

        match dialog.show() {
            rfd::MessageDialogResult::Ok => QB_MBRET_OK,
            rfd::MessageDialogResult::Cancel => QB_MBRET_CANCEL,
            rfd::MessageDialogResult::Yes => QB_MBRET_YES,
            rfd::MessageDialogResult::No => QB_MBRET_NO,
            _ => QB_MBRET_OK,
        }
    }

    #[cfg(not(feature = "dialogs"))]
    {
        let _ = (title, message, buttons);
        // Fallback: print to console and return OK
        if !message.is_null() {
            if let Ok(msg) = CStr::from_ptr(message).to_str() {
                eprintln!("{}", msg);
            }
        }
        QB_MBRET_OK
    }
}

#[cfg(test)]
mod tests {
    // Note: Dialog tests are difficult to automate as they require user interaction
    // These tests just verify the functions compile and don't panic with null inputs

    #[test]
    fn test_null_safety() {
        unsafe {
            // Should return empty string, not crash
            let result =
                super::qb_openfiledialog(std::ptr::null(), std::ptr::null(), std::ptr::null());
            assert!(!result.is_null());

            let result = super::qb_savefiledialog(
                std::ptr::null(),
                std::ptr::null(),
                std::ptr::null(),
                std::ptr::null(),
            );
            assert!(!result.is_null());

            let result = super::qb_selectfolderdialog(std::ptr::null(), std::ptr::null());
            assert!(!result.is_null());
        }
    }
}
