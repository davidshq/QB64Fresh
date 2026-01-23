//! Configuration for the QB64Fresh formatter.

/// Keyword capitalization style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum KeywordCase {
    /// UPPERCASE keywords (traditional BASIC style)
    #[default]
    Upper,
    /// lowercase keywords
    Lower,
    /// Title Case keywords (e.g., Print, If, Then)
    Title,
    /// Preserve original casing from source
    Preserve,
}

impl KeywordCase {
    /// Convert a keyword string according to the case style.
    pub fn apply(&self, keyword: &str) -> String {
        match self {
            KeywordCase::Upper => keyword.to_uppercase(),
            KeywordCase::Lower => keyword.to_lowercase(),
            KeywordCase::Title => {
                let mut chars = keyword.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => {
                        first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                    }
                }
            }
            KeywordCase::Preserve => keyword.to_string(),
        }
    }
}

/// Indentation style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IndentStyle {
    /// Use spaces for indentation
    #[default]
    Spaces,
    /// Use tabs for indentation
    Tabs,
}

/// Formatter configuration.
#[derive(Debug, Clone)]
pub struct FormatterConfig {
    /// How to capitalize keywords.
    pub keyword_case: KeywordCase,

    /// Indentation style (spaces or tabs).
    pub indent_style: IndentStyle,

    /// Number of spaces per indentation level (if using spaces).
    pub indent_width: usize,

    /// Add space after keywords like IF, THEN, PRINT.
    pub space_after_keyword: bool,

    /// Add spaces around binary operators (=, +, -, etc.).
    pub space_around_operators: bool,

    /// Add space after commas in argument lists.
    pub space_after_comma: bool,

    /// Remove trailing whitespace from lines.
    pub trim_trailing_whitespace: bool,

    /// Ensure file ends with a newline.
    pub insert_final_newline: bool,

    /// Maximum line length (0 = no limit).
    pub max_line_length: usize,

    /// Align trailing comments to a specific column (0 = no alignment).
    pub align_comments: usize,

    /// Preserve blank lines between statements.
    pub preserve_blank_lines: bool,

    /// Maximum consecutive blank lines to preserve (0 = remove all).
    pub max_blank_lines: usize,

    /// Indent contents of SUB/FUNCTION definitions.
    pub indent_procedures: bool,

    /// Indent contents of IF/FOR/WHILE/DO blocks.
    pub indent_blocks: bool,

    /// Capitalize identifiers in standard library functions.
    pub normalize_builtins: bool,

    /// Number of blank lines to insert between SUB/FUNCTION definitions.
    /// Set to 0 to preserve original spacing.
    pub blank_lines_between_procedures: usize,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            keyword_case: KeywordCase::Upper,
            indent_style: IndentStyle::Spaces,
            indent_width: 4,
            space_after_keyword: true,
            space_around_operators: true,
            space_after_comma: true,
            trim_trailing_whitespace: true,
            insert_final_newline: true,
            max_line_length: 0,
            align_comments: 0,
            preserve_blank_lines: true,
            max_blank_lines: 2,
            indent_procedures: true,
            indent_blocks: true,
            normalize_builtins: false,
            blank_lines_between_procedures: 1,
        }
    }
}

impl FormatterConfig {
    /// Create a minimal configuration that makes few changes.
    pub fn minimal() -> Self {
        Self {
            keyword_case: KeywordCase::Preserve,
            indent_style: IndentStyle::Spaces,
            indent_width: 4,
            space_after_keyword: false,
            space_around_operators: false,
            space_after_comma: false,
            trim_trailing_whitespace: true,
            insert_final_newline: true,
            max_line_length: 0,
            align_comments: 0,
            preserve_blank_lines: true,
            max_blank_lines: 0,
            indent_procedures: false,
            indent_blocks: false,
            normalize_builtins: false,
            blank_lines_between_procedures: 0,
        }
    }

    /// Create a "pretty" configuration with common style choices.
    pub fn pretty() -> Self {
        Self::default()
    }

    /// Create a configuration matching QB64 IDE defaults.
    pub fn qb64_style() -> Self {
        Self {
            keyword_case: KeywordCase::Upper,
            indent_style: IndentStyle::Spaces,
            indent_width: 4,
            space_after_keyword: true,
            space_around_operators: true,
            space_after_comma: true,
            trim_trailing_whitespace: true,
            insert_final_newline: true,
            max_line_length: 0,
            align_comments: 40,
            preserve_blank_lines: true,
            max_blank_lines: 1,
            indent_procedures: true,
            indent_blocks: true,
            normalize_builtins: true,
            blank_lines_between_procedures: 1,
        }
    }

    /// Get the indentation string for a given level.
    pub fn indent_string(&self, level: usize) -> String {
        match self.indent_style {
            IndentStyle::Spaces => " ".repeat(self.indent_width * level),
            IndentStyle::Tabs => "\t".repeat(level),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_case_upper() {
        assert_eq!(KeywordCase::Upper.apply("print"), "PRINT");
        assert_eq!(KeywordCase::Upper.apply("if"), "IF");
        assert_eq!(KeywordCase::Upper.apply("THEN"), "THEN");
    }

    #[test]
    fn test_keyword_case_lower() {
        assert_eq!(KeywordCase::Lower.apply("PRINT"), "print");
        assert_eq!(KeywordCase::Lower.apply("If"), "if");
    }

    #[test]
    fn test_keyword_case_title() {
        assert_eq!(KeywordCase::Title.apply("PRINT"), "Print");
        assert_eq!(KeywordCase::Title.apply("if"), "If");
        assert_eq!(KeywordCase::Title.apply("ELSEIF"), "Elseif");
    }

    #[test]
    fn test_indent_string() {
        let config = FormatterConfig {
            indent_width: 2,
            indent_style: IndentStyle::Spaces,
            ..Default::default()
        };
        assert_eq!(config.indent_string(0), "");
        assert_eq!(config.indent_string(1), "  ");
        assert_eq!(config.indent_string(2), "    ");

        let config = FormatterConfig {
            indent_style: IndentStyle::Tabs,
            ..Default::default()
        };
        assert_eq!(config.indent_string(1), "\t");
        assert_eq!(config.indent_string(2), "\t\t");
    }
}
