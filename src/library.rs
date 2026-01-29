//! Library management for `$USELIBRARY` directive support.
//!
//! This module handles discovery, validation, and inclusion of QB64pe-compatible
//! libraries. Libraries are organized with:
//!
//! - **Descriptor files**: `libraries/descriptors/{author/library}.ini`
//! - **Source directory**: `libraries/includes/{author/library}/`
//! - **Three inclusion points**: `IncAtTop`, `IncAfterMain`, `IncAtBottom`
//!
//! # Example
//!
//! ```no_run
//! use std::path::Path;
//! use qb64fresh::library::{LibraryManager, LibraryInfo};
//!
//! let manager = LibraryManager::new(Path::new("."));
//! match manager.load_library("author/library") {
//!     Ok(lib) => println!("Loaded library: {}", lib.name),
//!     Err(e) => eprintln!("Error: {}", e),
//! }
//! ```

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::preprocessor::PreprocessorError;

/// Information about a library's inclusion files.
#[derive(Debug, Clone)]
pub struct LibraryInfo {
    /// Library identifier (e.g., "author/library").
    pub name: String,
    /// Path to file included at top of program (if any).
    pub inc_at_top: Option<PathBuf>,
    /// Path to file included after main program (if any).
    pub inc_after_main: Option<PathBuf>,
    /// Path to file included at bottom of program (if any).
    pub inc_at_bottom: Option<PathBuf>,
}

/// Tracks library usage and prevents duplicates.
#[derive(Debug, Clone)]
struct LibraryUsage {
    /// Library identifier.
    name: String,
    /// Referrer (file/line that requested this library).
    referrer: String,
    /// Library information.
    info: LibraryInfo,
}

/// Manages library discovery and inclusion.
pub struct LibraryManager {
    /// Base directory for library discovery (searches `libraries/` subdirectory).
    base_path: PathBuf,
    /// Tracked libraries to prevent duplicates.
    libraries: Vec<LibraryUsage>,
}

impl LibraryManager {
    /// Creates a new library manager with the given base path.
    ///
    /// Libraries will be searched in `{base_path}/libraries/descriptors/` and
    /// source files in `{base_path}/libraries/includes/`.
    pub fn new(base_path: &Path) -> Self {
        Self {
            base_path: base_path.to_path_buf(),
            libraries: Vec::new(),
        }
    }

    /// Loads library information from a descriptor file.
    ///
    /// # Arguments
    ///
    /// * `library_name` - Library identifier in format "author/library"
    ///
    /// # Returns
    ///
    /// `LibraryInfo` if the library descriptor and all referenced files exist.
    ///
    /// # Errors
    ///
    /// Returns `PreprocessorError` if:
    /// - Descriptor file not found
    /// - Descriptor file cannot be read
    /// - Required inclusion files don't exist
    pub fn load_library(&self, library_name: &str) -> Result<LibraryInfo, PreprocessorError> {
        // Construct descriptor path: libraries/descriptors/{author/library}.ini
        let descriptor_path = self
            .base_path
            .join("libraries")
            .join("descriptors")
            .join(format!("{}.ini", library_name));

        if !descriptor_path.exists() {
            return Err(PreprocessorError::FileNotFound {
                path: format!("libraries/descriptors/{}.ini", library_name),
                from_file: self.base_path.clone(),
            });
        }

        // Read and parse INI file
        let ini_content =
            fs::read_to_string(&descriptor_path).map_err(|e| PreprocessorError::ReadError {
                path: descriptor_path.clone(),
                message: e.to_string(),
            })?;

        // Parse INI to extract [LIBRARY INCLUDES] section
        let includes = parse_ini_section(&ini_content, "[LIBRARY INCLUDES]");

        // Construct source directory path: libraries/includes/{author/library}/
        let source_dir = self
            .base_path
            .join("libraries")
            .join("includes")
            .join(library_name);

        // Helper function to validate and resolve a library file path
        // Prevents path traversal attacks by ensuring the resolved path is within source_dir
        let validate_library_path =
            |file_name: &str| -> Result<Option<PathBuf>, PreprocessorError> {
                if file_name.is_empty() {
                    return Ok(None);
                }

                // Prevent path traversal attacks
                if file_name.contains("..") || file_name.contains('/') || file_name.contains('\\') {
                    return Err(PreprocessorError::ReadError {
                        path: descriptor_path.clone(),
                        message: format!(
                            "Invalid library file path '{}': paths must be relative filenames within the library directory",
                            file_name
                        ),
                    });
                }

                let path = source_dir.join(file_name);

                // Ensure the resolved path is still within source_dir (prevent path traversal)
                // Use Path::starts_with() for proper path comparison
                if !path.starts_with(&source_dir) {
                    return Err(PreprocessorError::ReadError {
                        path: descriptor_path.clone(),
                        message: format!(
                            "Invalid library file path '{}': path traversal detected",
                            file_name
                        ),
                    });
                }

                if !path.exists() {
                    return Err(PreprocessorError::FileNotFound {
                        path: path.display().to_string(),
                        from_file: descriptor_path.clone(),
                    });
                }

                Ok(Some(path))
            };

        // Extract and validate inclusion file paths
        let inc_at_top = match includes.get("IncAtTop") {
            Some(name) => validate_library_path(name)?,
            None => None,
        };

        let inc_after_main = match includes.get("IncAfterMain") {
            Some(name) => validate_library_path(name)?,
            None => None,
        };

        let inc_at_bottom = match includes.get("IncAtBottom") {
            Some(name) => validate_library_path(name)?,
            None => None,
        };

        // Validate that at least one inclusion file exists
        if inc_at_top.is_none() && inc_after_main.is_none() && inc_at_bottom.is_none() {
            return Err(PreprocessorError::ReadError {
                path: descriptor_path,
                message: format!(
                    "No valid inclusion files found for library '{}'",
                    library_name
                ),
            });
        }

        Ok(LibraryInfo {
            name: library_name.to_string(),
            inc_at_top: inc_at_top.map(|p| p.canonicalize().unwrap_or(p)),
            inc_after_main: inc_after_main.map(|p| p.canonicalize().unwrap_or(p)),
            inc_at_bottom: inc_at_bottom.map(|p| p.canonicalize().unwrap_or(p)),
        })
    }

    /// Registers a library for inclusion, preventing duplicates.
    ///
    /// # Arguments
    ///
    /// * `library_name` - Library identifier
    /// * `referrer` - File/line that requested this library (e.g., "main.bas:42")
    ///
    /// # Returns
    ///
    /// `Ok(Some(LibraryInfo))` if library was newly registered,
    /// `Ok(None)` if library was already registered for this referrer,
    /// `Err` if library cannot be loaded.
    pub fn register_library(
        &mut self,
        library_name: &str,
        referrer: &str,
    ) -> Result<Option<LibraryInfo>, PreprocessorError> {
        // Check for duplicate (same library + same referrer)
        for usage in &self.libraries {
            if usage.name == library_name && usage.referrer == referrer {
                return Ok(None); // Already registered
            }
        }

        // Load library information
        let info = self.load_library(library_name)?;

        // Register it
        self.libraries.push(LibraryUsage {
            name: library_name.to_string(),
            referrer: referrer.to_string(),
            info: info.clone(),
        });

        Ok(Some(info))
    }

    /// Gets all libraries registered for "AtTop" inclusion (in reverse order for dependencies).
    pub fn get_at_top_libraries(&self) -> Vec<&LibraryInfo> {
        self.libraries
            .iter()
            .rev() // Reverse order to pull dependencies before they're needed
            .filter_map(|usage| usage.info.inc_at_top.as_ref().map(|_| &usage.info))
            .collect()
    }

    /// Gets all libraries registered for "AfterMain" inclusion (in order of appearance).
    pub fn get_after_main_libraries(&self) -> Vec<&LibraryInfo> {
        self.libraries
            .iter()
            .filter_map(|usage| usage.info.inc_after_main.as_ref().map(|_| &usage.info))
            .collect()
    }

    /// Gets all libraries registered for "AtBottom" inclusion (in order of appearance).
    pub fn get_at_bottom_libraries(&self) -> Vec<&LibraryInfo> {
        self.libraries
            .iter()
            .filter_map(|usage| usage.info.inc_at_bottom.as_ref().map(|_| &usage.info))
            .collect()
    }
}

/// Parses an INI file section and returns key-value pairs.
///
/// This is a simple INI parser that:
/// - Finds the specified section (e.g., `[LIBRARY INCLUDES]`)
/// - Extracts key=value pairs from that section
/// - Handles quoted values and trims whitespace
/// - Stops at the next section or end of file
///
/// # Arguments
///
/// * `content` - The INI file content
/// * `section_name` - The section name to parse (e.g., `[LIBRARY INCLUDES]`)
///
/// # Returns
///
/// A HashMap of key-value pairs from the section.
fn parse_ini_section(content: &str, section_name: &str) -> HashMap<String, String> {
    let mut result = HashMap::new();
    let section_name_lower = section_name.to_lowercase();
    let mut in_section = false;

    for line in content.lines() {
        let trimmed = line.trim();

        // Skip empty lines and comments
        if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('\'') {
            continue;
        }

        // Check for section header
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            let current_section = trimmed.to_lowercase();
            in_section = current_section == section_name_lower;
            continue;
        }

        // Parse key=value if we're in the target section
        if in_section && let Some(equal_pos) = trimmed.find('=') {
            let key = trimmed[..equal_pos].trim().to_string();

            // Skip empty keys
            if key.is_empty() {
                continue;
            }

            let value = trimmed[equal_pos + 1..].trim();

            // Remove quotes if present (must be at least 2 chars: "" or "x")
            let value = if value.len() >= 2 && value.starts_with('"') && value.ends_with('"') {
                &value[1..value.len() - 1]
            } else {
                value
            };

            // Remove comments (BASIC style ' or INI style ;)
            let value = if let Some(comment_pos) = value.find('\'') {
                &value[..comment_pos]
            } else if let Some(comment_pos) = value.find(';') {
                &value[..comment_pos]
            } else {
                value
            };

            result.insert(key, value.trim().to_string());
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    #[test]
    fn test_parse_ini_section() {
        let ini_content = r#"
[OTHER SECTION]
key1 = value1

[LIBRARY INCLUDES]
IncAtTop = AtTop.bas
IncAfterMain = AfterMain.bas
IncAtBottom = AtBottom.bas

[ANOTHER SECTION]
key2 = value2
"#;

        let result = parse_ini_section(ini_content, "[LIBRARY INCLUDES]");
        assert_eq!(result.get("IncAtTop"), Some(&"AtTop.bas".to_string()));
        assert_eq!(
            result.get("IncAfterMain"),
            Some(&"AfterMain.bas".to_string())
        );
        assert_eq!(result.get("IncAtBottom"), Some(&"AtBottom.bas".to_string()));
        assert_eq!(result.get("key1"), None); // From other section
    }

    #[test]
    fn test_parse_ini_with_quotes() {
        let ini_content = r#"
[LIBRARY INCLUDES]
IncAtTop = "AtTop.bas"
IncAfterMain = AfterMain.bas
"#;

        let result = parse_ini_section(ini_content, "[LIBRARY INCLUDES]");
        assert_eq!(result.get("IncAtTop"), Some(&"AtTop.bas".to_string()));
        assert_eq!(
            result.get("IncAfterMain"),
            Some(&"AfterMain.bas".to_string())
        );
    }

    #[test]
    fn test_parse_ini_with_comments() {
        let ini_content = r#"
[LIBRARY INCLUDES]
IncAtTop = AtTop.bas ' This is a comment
IncAfterMain = AfterMain.bas ; Another comment
"#;

        let result = parse_ini_section(ini_content, "[LIBRARY INCLUDES]");
        assert_eq!(result.get("IncAtTop"), Some(&"AtTop.bas".to_string()));
        assert_eq!(
            result.get("IncAfterMain"),
            Some(&"AfterMain.bas".to_string())
        );
    }

    #[test]
    fn test_parse_ini_edge_cases() {
        // Test empty key (should be skipped)
        let ini_content = r#"
[LIBRARY INCLUDES]
= value
IncAtTop = AtTop.bas
"#;
        let result = parse_ini_section(ini_content, "[LIBRARY INCLUDES]");
        assert_eq!(result.get(""), None); // Empty key should be skipped
        assert_eq!(result.get("IncAtTop"), Some(&"AtTop.bas".to_string()));

        // Test single quote (should not panic)
        let ini_content = r#"
[LIBRARY INCLUDES]
IncAtTop = "
"#;
        let result = parse_ini_section(ini_content, "[LIBRARY INCLUDES]");
        // Single quote should be handled safely (won't match starts_with and ends_with)
        assert_eq!(result.get("IncAtTop"), Some(&"\"".to_string()));

        // Test empty quoted string
        let ini_content = r#"
[LIBRARY INCLUDES]
IncAtTop = ""
"#;
        let result = parse_ini_section(ini_content, "[LIBRARY INCLUDES]");
        assert_eq!(result.get("IncAtTop"), Some(&"".to_string()));
    }

    #[test]
    fn test_load_library() {
        let temp_dir = TempDir::new().expect("creating temp directory should succeed");
        let temp_path = temp_dir.path();

        // Create library structure
        let descriptors_dir = temp_path.join("libraries").join("descriptors");
        fs::create_dir_all(&descriptors_dir).expect("creating descriptors dir should succeed");

        let includes_dir = temp_path
            .join("libraries")
            .join("includes")
            .join("author/library");
        fs::create_dir_all(&includes_dir).expect("creating includes dir should succeed");

        // Create descriptor file (parent dir already exists from create_dir_all above)
        let descriptor_path = descriptors_dir.join("author");
        fs::create_dir_all(&descriptor_path).expect("creating author dir should succeed");
        let descriptor_path = descriptor_path.join("library.ini");
        let mut descriptor =
            fs::File::create(&descriptor_path).expect("creating descriptor file should succeed");
        writeln!(descriptor, "[LIBRARY INCLUDES]").expect("writing to descriptor should succeed");
        writeln!(descriptor, "IncAtTop = AtTop.bas").expect("writing to descriptor should succeed");
        writeln!(descriptor, "IncAfterMain = AfterMain.bas")
            .expect("writing to descriptor should succeed");

        // Create library source files
        let at_top_file = includes_dir.join("AtTop.bas");
        fs::write(&at_top_file, "CONST LIB_VERSION = 1").expect("writing AtTop.bas should succeed");

        let after_main_file = includes_dir.join("AfterMain.bas");
        fs::write(&after_main_file, "SUB LibraryInit")
            .expect("writing AfterMain.bas should succeed");

        // Load library
        let manager = LibraryManager::new(temp_path);
        let lib = manager
            .load_library("author/library")
            .expect("loading library should succeed");

        assert_eq!(lib.name, "author/library");
        assert!(lib.inc_at_top.is_some());
        assert!(lib.inc_after_main.is_some());
        assert!(lib.inc_at_bottom.is_none());
    }

    #[test]
    fn test_register_library_prevents_duplicates() {
        let temp_dir = TempDir::new().expect("creating temp directory should succeed");
        let temp_path = temp_dir.path();

        // Create minimal library structure
        let descriptors_dir = temp_path.join("libraries").join("descriptors");
        fs::create_dir_all(&descriptors_dir).expect("creating descriptors dir should succeed");

        let includes_dir = temp_path
            .join("libraries")
            .join("includes")
            .join("test/lib");
        fs::create_dir_all(&includes_dir).expect("creating includes dir should succeed");

        // Create descriptor file (parent dir already exists from create_dir_all above)
        let descriptor_parent = descriptors_dir.join("test");
        fs::create_dir_all(&descriptor_parent).expect("creating test dir should succeed");
        let descriptor_path = descriptor_parent.join("lib.ini");
        let mut descriptor =
            fs::File::create(&descriptor_path).expect("creating descriptor file should succeed");
        writeln!(descriptor, "[LIBRARY INCLUDES]").expect("writing to descriptor should succeed");
        writeln!(descriptor, "IncAtTop = AtTop.bas").expect("writing to descriptor should succeed");

        let at_top_file = includes_dir.join("AtTop.bas");
        fs::write(&at_top_file, "CONST TEST = 1").expect("writing AtTop.bas should succeed");

        let mut manager = LibraryManager::new(temp_path);

        // First registration should succeed
        let result1 = manager
            .register_library("test/lib", "main.bas:10")
            .expect("first registration should succeed");
        assert!(result1.is_some());

        // Second registration with same referrer should return None (duplicate)
        let result2 = manager
            .register_library("test/lib", "main.bas:10")
            .expect("second registration should not error");
        assert!(result2.is_none());

        // Different referrer should register again
        let result3 = manager
            .register_library("test/lib", "other.bas:20")
            .expect("third registration should succeed");
        assert!(result3.is_some());
    }
}
