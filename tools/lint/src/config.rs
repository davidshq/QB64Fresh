//! Configuration for the QB64Fresh linter.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::error::{LintError, LintResult};

/// Severity level for a lint diagnostic.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Informational hint - doesn't affect exit code.
    Hint,
    /// Warning - may indicate a problem.
    #[default]
    Warning,
    /// Error - definitely a problem, affects exit code.
    Error,
    /// Lint is disabled.
    Off,
}

impl Severity {
    /// Returns true if this severity should be reported.
    pub fn is_reported(&self) -> bool {
        !matches!(self, Severity::Off)
    }

    /// Returns true if this severity should cause a non-zero exit code.
    pub fn is_error(&self) -> bool {
        matches!(self, Severity::Error)
    }

    /// Returns the display name for this severity.
    pub fn name(&self) -> &'static str {
        match self {
            Severity::Hint => "hint",
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Off => "off",
        }
    }
}

/// Categories of lint rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LintCategory {
    /// Correctness lints - likely bugs.
    Correctness,
    /// Style lints - coding conventions.
    Style,
    /// Performance lints - inefficient patterns.
    Performance,
    /// Complexity lints - overly complex code.
    Complexity,
    /// Deprecated patterns - old constructs with better alternatives.
    Deprecated,
}

impl LintCategory {
    /// Returns the display name for this category.
    pub fn name(&self) -> &'static str {
        match self {
            LintCategory::Correctness => "correctness",
            LintCategory::Style => "style",
            LintCategory::Performance => "performance",
            LintCategory::Complexity => "complexity",
            LintCategory::Deprecated => "deprecated",
        }
    }
}

/// Configuration for a single lint rule.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleConfig {
    /// Severity level for this rule.
    pub severity: Severity,
}

impl Default for RuleConfig {
    fn default() -> Self {
        Self {
            severity: Severity::Warning,
        }
    }
}

/// Linter configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct LintConfig {
    /// Default severity for all rules (can be overridden per-rule).
    pub default_severity: Severity,

    /// Per-rule configuration, keyed by rule name.
    pub rules: HashMap<String, RuleConfig>,

    /// Enable all lints in a category.
    pub enable_categories: Vec<LintCategory>,

    /// Disable all lints in a category.
    pub disable_categories: Vec<LintCategory>,

    /// File patterns to exclude from linting.
    pub exclude: Vec<String>,

    /// Maximum number of diagnostics to report (0 = unlimited).
    pub max_diagnostics: usize,
}

impl Default for LintConfig {
    fn default() -> Self {
        Self {
            default_severity: Severity::Warning,
            rules: HashMap::new(),
            enable_categories: vec![LintCategory::Correctness],
            disable_categories: vec![],
            exclude: vec![],
            max_diagnostics: 0,
        }
    }
}

impl LintConfig {
    /// Creates a strict configuration that treats warnings as errors.
    pub fn strict() -> Self {
        Self {
            default_severity: Severity::Error,
            enable_categories: vec![
                LintCategory::Correctness,
                LintCategory::Style,
                LintCategory::Performance,
            ],
            ..Default::default()
        }
    }

    /// Creates a permissive configuration with only correctness lints.
    pub fn permissive() -> Self {
        Self {
            default_severity: Severity::Warning,
            enable_categories: vec![LintCategory::Correctness],
            disable_categories: vec![LintCategory::Style, LintCategory::Complexity],
            ..Default::default()
        }
    }

    /// Creates a configuration with all lints enabled.
    pub fn pedantic() -> Self {
        Self {
            default_severity: Severity::Warning,
            enable_categories: vec![
                LintCategory::Correctness,
                LintCategory::Style,
                LintCategory::Performance,
                LintCategory::Complexity,
                LintCategory::Deprecated,
            ],
            ..Default::default()
        }
    }

    /// Loads configuration from a TOML file.
    pub fn from_file(path: &Path) -> LintResult<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| LintError::ConfigReadError {
            path: path.to_path_buf(),
            source: e,
        })?;

        toml::from_str(&content).map_err(|e| LintError::ConfigParseError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })
    }

    /// Tries to find and load a configuration file from standard locations.
    ///
    /// Searches for (in order):
    /// 1. `.qb64fresh-lint.toml` in the current directory
    /// 2. `qb64fresh-lint.toml` in the current directory
    /// 3. `.qb64fresh-lint.toml` in parent directories
    pub fn discover() -> Option<Self> {
        let config_names = [".qb64fresh-lint.toml", "qb64fresh-lint.toml"];

        // Check current directory first
        for name in &config_names {
            let path = Path::new(name);
            if path.exists() {
                if let Ok(config) = Self::from_file(path) {
                    return Some(config);
                }
            }
        }

        // Walk up parent directories
        let mut current = std::env::current_dir().ok()?;
        loop {
            for name in &config_names {
                let path = current.join(name);
                if path.exists() {
                    if let Ok(config) = Self::from_file(&path) {
                        return Some(config);
                    }
                }
            }

            if !current.pop() {
                break;
            }
        }

        None
    }

    /// Gets the severity for a specific rule.
    pub fn severity_for(&self, rule_name: &str, default_category: LintCategory) -> Severity {
        // Check per-rule override first
        if let Some(rule_config) = self.rules.get(rule_name) {
            return rule_config.severity;
        }

        // Check if category is disabled
        if self.disable_categories.contains(&default_category) {
            return Severity::Off;
        }

        // Check if category is enabled
        if self.enable_categories.contains(&default_category) {
            return self.default_severity;
        }

        // Default to off for categories not explicitly enabled
        Severity::Off
    }

    /// Sets the severity for a specific rule.
    pub fn set_rule_severity(&mut self, rule_name: &str, severity: Severity) {
        self.rules
            .insert(rule_name.to_string(), RuleConfig { severity });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = LintConfig::default();
        assert_eq!(config.default_severity, Severity::Warning);
        assert!(config
            .enable_categories
            .contains(&LintCategory::Correctness));
    }

    #[test]
    fn test_severity_ordering() {
        assert!(Severity::Hint < Severity::Warning);
        assert!(Severity::Warning < Severity::Error);
    }

    #[test]
    fn test_rule_severity_override() {
        let mut config = LintConfig::default();
        config.set_rule_severity("unused_variable", Severity::Error);

        assert_eq!(
            config.severity_for("unused_variable", LintCategory::Correctness),
            Severity::Error
        );
    }

    #[test]
    fn test_category_disable() {
        let mut config = LintConfig::default();
        config.disable_categories.push(LintCategory::Style);

        assert_eq!(
            config.severity_for("some_style_lint", LintCategory::Style),
            Severity::Off
        );
    }

    #[test]
    fn test_toml_roundtrip() {
        let config = LintConfig::strict();
        let toml_str = toml::to_string_pretty(&config).unwrap();
        let parsed: LintConfig = toml::from_str(&toml_str).unwrap();

        assert_eq!(parsed.default_severity, config.default_severity);
    }
}
