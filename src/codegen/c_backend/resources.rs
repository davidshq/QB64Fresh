//! Windows resource file generation for $EXEICON and $VERSIONINFO directives.
//!
//! This module generates Windows resource files (`.rc`, `manifest.h`, `.manifest`)
//! that can be compiled with `windres` (GCC) or `rc.exe` (MSVC) to embed metadata
//! into Windows executables.
//!
//! # Supported Directives
//!
//! - `$EXEICON:'filename'` - Sets the executable icon (can only be set once)
//! - `$VERSIONINFO:key=value` - Sets version information metadata
//!
//! # Generated Files
//!
//! - `icon.rc` - Resource script with icon and version info
//! - `manifest.h` - Header with manifest resource definitions
//! - `{filename}.manifest` - XML manifest file for Windows
//!
//! # Example
//!
//! ```basic
//! $EXEICON:'app.ico'
//! $VERSIONINFO:CompanyName="My Company"
//! $VERSIONINFO:ProductName="My App"
//! $VERSIONINFO:FileVersion="1.0.0.0"
//! $VERSIONINFO:FILEVERSION#=1,0,0,0
//! ```

use std::collections::HashMap;

use crate::codegen::error::{CodeGenError, CodeGenErrorKind};
use crate::semantic::typed_ir::TypedStatement;

/// Collected resource information from $EXEICON and $VERSIONINFO directives.
#[derive(Debug, Default)]
pub struct ResourceInfo {
    /// Icon filename (from $EXEICON, can only be set once)
    pub icon_file: Option<String>,
    /// Line number where $EXEICON was set (for error reporting)
    pub icon_line: Option<usize>,
    /// Version info fields (key -> value)
    pub version_info: HashMap<String, String>,
    /// Numeric file version (from FILEVERSION#)
    pub file_version_num: Option<String>,
    /// Numeric product version (from PRODUCTVERSION#)
    pub product_version_num: Option<String>,
}

impl ResourceInfo {
    /// Creates a new empty ResourceInfo.
    pub fn new() -> Self {
        Self::default()
    }

    /// Collects resource directives from a typed program.
    ///
    /// # Errors
    ///
    /// Returns errors if:
    /// - `$EXEICON` is set multiple times
    /// - Invalid version info keys are used
    pub fn collect_from_program(statements: &[TypedStatement]) -> Result<Self, Vec<CodeGenError>> {
        let mut info = Self::new();
        let mut errors = Vec::new();

        for stmt in statements {
            use crate::semantic::typed_ir::TypedStatementKind;
            match &stmt.kind {
                TypedStatementKind::MetaExeIcon { filename } => {
                    if info.icon_file.is_some() {
                        errors.push(
                            CodeGenError::new(CodeGenErrorKind::ResourceError {
                                message: format!(
                                    "$EXEICON already defined (first at line {}, duplicate at line {})",
                                    info.icon_line.unwrap_or(0),
                                    stmt.span.line
                                ),
                            })
                            .with_span(stmt.span),
                        );
                    } else {
                        info.icon_file = Some(filename.clone());
                        info.icon_line = Some(stmt.span.line);
                    }
                }
                TypedStatementKind::MetaVersionInfo { key, value } => {
                    // Validate key
                    let normalized_key = key.to_uppercase();
                    match normalized_key.as_str() {
                        "FILEVERSION#" | "PRODUCTVERSION#" => {
                            // Validate numeric version format (must be #,#,#,#)
                            if !is_valid_version_number(value) {
                                errors.push(
                                    CodeGenError::new(CodeGenErrorKind::ResourceError {
                                        message: format!(
                                            "Expected $VERSIONINFO:{}={} (4 comma-separated numeric values: major, minor, revision, build)",
                                            key, value
                                        ),
                                    })
                                    .with_span(stmt.span),
                                );
                            } else if normalized_key == "FILEVERSION#" {
                                info.file_version_num = Some(value.clone());
                            } else {
                                info.product_version_num = Some(value.clone());
                            }
                        }
                        "COMPANYNAME" | "FILEDESCRIPTION" | "FILEVERSION" | "INTERNALNAME"
                        | "LEGALCOPYRIGHT" | "LEGALTRADEMARKS" | "ORIGINALFILENAME"
                        | "PRODUCTNAME" | "PRODUCTVERSION" | "COMMENTS" | "WEB" => {
                            // Remove quotes if present (QB64pe allows quoted or unquoted)
                            let clean_value = remove_quotes(value);
                            info.version_info
                                .insert(normalized_key.clone(), clean_value);
                        }
                        _ => {
                            errors.push(
                                CodeGenError::new(CodeGenErrorKind::ResourceError {
                                    message: format!(
                                        "Invalid key '{}'. Use FILEVERSION#, PRODUCTVERSION#, CompanyName, FileDescription, FileVersion, InternalName, LegalCopyright, LegalTrademarks, OriginalFilename, ProductName, ProductVersion, Comments, or Web",
                                        key
                                    ),
                                })
                                .with_span(stmt.span),
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        if errors.is_empty() {
            Ok(info)
        } else {
            Err(errors)
        }
    }

    /// Checks if any resources need to be generated.
    pub fn has_resources(&self) -> bool {
        self.icon_file.is_some()
            || !self.version_info.is_empty()
            || self.file_version_num.is_some()
            || self.product_version_num.is_some()
    }
}

/// Generates Windows resource files from collected resource information.
///
/// # Arguments
///
/// * `info` - Collected resource information
/// * `output_base_name` - Base name for output files (without extension)
///
/// # Returns
///
/// A tuple of (icon.rc content, manifest.h content, manifest.xml content)
pub fn generate_resource_files(
    info: &ResourceInfo,
    output_base_name: &str,
) -> Result<(String, String, String), CodeGenError> {
    let mut rc_content = String::new();
    let mut manifest_h = String::new();
    let mut manifest_xml = String::new();

    // Generate icon.rc
    if info.icon_file.is_some()
        || !info.version_info.is_empty()
        || info.file_version_num.is_some()
        || info.product_version_num.is_some()
    {
        // Icon entry
        if let Some(icon_file) = &info.icon_file {
            // Escape quotes and backslashes in icon filename
            let escaped_icon = escape_rc_string(icon_file);
            rc_content.push_str(&format!("0 ICON \"{}\"\n", escaped_icon));
        }

        // Version info block
        if !info.version_info.is_empty()
            || info.file_version_num.is_some()
            || info.product_version_num.is_some()
        {
            rc_content.push('\n');
            rc_content.push_str("#include \"manifest.h\"\n");
            rc_content.push('\n');
            rc_content.push_str(&format!(
                "CREATEPROCESS_MANIFEST_RESOURCE_ID RT_MANIFEST \"{}.manifest\"\n",
                output_base_name
            ));
            rc_content.push('\n');
            rc_content.push_str("1 VERSIONINFO\n");

            // FILEVERSION and PRODUCTVERSION (numeric)
            if let Some(ver) = &info.file_version_num {
                rc_content.push_str(&format!("FILEVERSION     {}\n", ver));
            }
            if let Some(ver) = &info.product_version_num {
                rc_content.push_str(&format!("PRODUCTVERSION  {}\n", ver));
            }

            rc_content.push_str("BEGIN\n");
            rc_content.push_str("    BLOCK \"StringFileInfo\"\n");
            rc_content.push_str("    BEGIN\n");
            rc_content.push_str("        BLOCK \"040904E4\"\n");
            rc_content.push_str("        BEGIN\n");

            // String values (QB64pe format: VALUE "Key","Value\0")
            // Map from uppercase (stored key) to proper case (RC file format)
            let key_mapping: &[(&str, &str)] = &[
                ("COMPANYNAME", "CompanyName"),
                ("FILEDESCRIPTION", "FileDescription"),
                ("FILEVERSION", "FileVersion"),
                ("INTERNALNAME", "InternalName"),
                ("LEGALCOPYRIGHT", "LegalCopyright"),
                ("LEGALTRADEMARKS", "LegalTrademarks"),
                ("ORIGINALFILENAME", "OriginalFilename"),
                ("PRODUCTNAME", "ProductName"),
                ("PRODUCTVERSION", "ProductVersion"),
                ("COMMENTS", "Comments"),
                ("WEB", "Web"),
            ];

            for (uppercase_key, proper_case_key) in key_mapping {
                if let Some(value) = info.version_info.get(*uppercase_key) {
                    // Skip empty values (QB64pe behavior - only write if LEN > 0)
                    if !value.is_empty() {
                        // Escape quotes in the value for RC file
                        let escaped_value = escape_rc_string(value);
                        rc_content.push_str(&format!(
                            "            VALUE \"{}\",\"{}\\0\"\n",
                            proper_case_key, escaped_value
                        ));
                    }
                }
            }

            rc_content.push_str("        END\n");
            rc_content.push_str("    END\n");
            rc_content.push_str("    BLOCK \"VarFileInfo\"\n");
            rc_content.push_str("    BEGIN\n");
            rc_content.push_str("            VALUE \"Translation\", 0x409, 0x04E4\n");
            rc_content.push_str("    END\n");
            rc_content.push_str("END\n");
        }
    }

    // Generate manifest.h (when ANY version info is set, including numeric only)
    if !info.version_info.is_empty()
        || info.file_version_num.is_some()
        || info.product_version_num.is_some()
    {
        manifest_h.push_str("#ifndef RESOURCE_H\n");
        manifest_h.push_str("#define   RESOURCE_H\n");
        manifest_h.push_str("#ifdef    __cplusplus\n");
        manifest_h.push_str("extern \"C\" {\n");
        manifest_h.push_str("#endif\n");
        manifest_h.push_str("#ifdef    __cplusplus\n");
        manifest_h.push_str("}\n");
        manifest_h.push_str("#endif\n");
        manifest_h.push_str("#endif    /* RESOURCE_H */\n");
        manifest_h
            .push_str("#define CREATEPROCESS_MANIFEST_RESOURCE_ID 1 /*Defined manifest file*/\n");
        manifest_h.push_str("#define RT_MANIFEST                       24\n");
    }

    // Generate manifest.xml (when ANY version info is set, including numeric only)
    if !info.version_info.is_empty()
        || info.file_version_num.is_some()
        || info.product_version_num.is_some()
    {
        let company_name = info
            .version_info
            .get("COMPANYNAME")
            .map(|s| escape_xml(s.as_str()))
            .unwrap_or_default();
        let product_name = info
            .version_info
            .get("PRODUCTNAME")
            .map(|s| escape_xml(s.as_str()))
            .unwrap_or_default();
        let file_description = info
            .version_info
            .get("FILEDESCRIPTION")
            .map(|s| escape_xml(s.as_str()))
            .unwrap_or_default();

        manifest_xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
        manifest_xml.push_str(
            "<assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\n",
        );
        manifest_xml.push_str("<assemblyIdentity\n");
        manifest_xml.push_str("    version=\"1.0.0.0\"\n");
        manifest_xml.push_str("    processorArchitecture=\"*\"\n");
        manifest_xml.push_str(&format!(
            "    name=\"{}.{}.{}\"\n",
            company_name, product_name, product_name
        ));
        manifest_xml.push_str("    type=\"win32\"\n");
        manifest_xml.push_str("/>\n");
        manifest_xml.push_str(&format!(
            "<description>{}</description>\n",
            file_description
        ));
        manifest_xml.push_str("<dependency>\n");
        manifest_xml.push_str("    <dependentAssembly>\n");
        manifest_xml.push_str("        <assemblyIdentity\n");
        manifest_xml.push_str("            type=\"win32\"\n");
        manifest_xml.push_str("            name=\"Microsoft.Windows.Common-Controls\"\n");
        manifest_xml.push_str("            version=\"6.0.0.0\"\n");
        manifest_xml.push_str("            processorArchitecture=\"*\"\n");
        manifest_xml.push_str("            publicKeyToken=\"6595b64144ccf1df\"\n");
        manifest_xml.push_str("            language=\"*\"\n");
        manifest_xml.push_str("        />\n");
        manifest_xml.push_str("    </dependentAssembly>\n");
        manifest_xml.push_str("</dependency>\n");
        manifest_xml.push_str("</assembly>\n");
    }

    Ok((rc_content, manifest_h, manifest_xml))
}

/// Validates that a version number string is in the format #,#,#,#
fn is_valid_version_number(s: &str) -> bool {
    let parts: Vec<&str> = s.split(',').collect();
    if parts.len() != 4 {
        return false;
    }
    parts.iter().all(|part| part.trim().parse::<u32>().is_ok())
}

/// Removes surrounding quotes from a string value.
fn remove_quotes(s: &str) -> String {
    let s = s.trim();
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

/// Escapes special characters for XML content.
fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Escapes quotes in a string for RC file format.
/// RC files use double quotes, so we need to escape any quotes in the value.
fn escape_rc_string(s: &str) -> String {
    // RC files use double quotes, so escape any double quotes in the value
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_valid_version_number() {
        assert!(is_valid_version_number("1,0,0,0"));
        assert!(is_valid_version_number("4,3,0,0"));
        assert!(!is_valid_version_number("1,0,0"));
        assert!(!is_valid_version_number("1,0,0,0,0"));
        assert!(!is_valid_version_number("1.0.0.0"));
        assert!(!is_valid_version_number("abc"));
    }

    #[test]
    fn test_remove_quotes() {
        assert_eq!(remove_quotes("\"hello\""), "hello");
        assert_eq!(remove_quotes("'hello'"), "hello");
        assert_eq!(remove_quotes("hello"), "hello");
        assert_eq!(remove_quotes("  \"hello\"  "), "hello");
    }

    #[test]
    fn test_generate_resource_files() {
        let mut info = ResourceInfo::new();
        info.icon_file = Some("icon.ico".to_string());
        info.version_info
            .insert("COMPANYNAME".to_string(), "Test Company".to_string());
        info.version_info
            .insert("PRODUCTNAME".to_string(), "Test App".to_string());
        info.file_version_num = Some("1,0,0,0".to_string());

        let (rc, h, xml) = generate_resource_files(&info, "test").unwrap();

        assert!(rc.contains("0 ICON"));
        assert!(rc.contains("VERSIONINFO"));
        assert!(rc.contains("CompanyName"));
        assert!(h.contains("RESOURCE_H"));
        assert!(xml.contains("Test Company"));
    }
}
