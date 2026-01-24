//! Parser for C header declarations.
//!
//! Extracts function declarations, constants, and struct definitions from
//! tokenized C header content. Supports conditional compilation via
//! `#ifdef`/`#ifndef`/`#if` directives.

use super::lexer::{PreprocessorDirective, Token};
use super::{
    CConstant, CFunction, CParam, CStruct, CStructMember, HeaderParseResult, Platform,
    c_type_to_basic, parse_constant_value,
};

// ============================================================================
// Conditional Compilation State
// ============================================================================

/// Tracks preprocessor conditional compilation state.
///
/// This struct maintains a stack of conditional blocks to determine whether
/// code should be included based on `#ifdef`, `#ifndef`, `#if`, `#else`, and
/// `#endif` directives.
#[derive(Debug)]
struct PreprocessorState {
    /// Stack of conditional blocks. Each entry is (currently_active, has_been_active).
    /// - `currently_active`: whether we're currently including code
    /// - `has_been_active`: whether any branch in this if/elif/else chain has been active
    condition_stack: Vec<(bool, bool)>,
    /// Set of defined macros for this platform.
    defined_macros: std::collections::HashSet<String>,
}

impl PreprocessorState {
    /// Create a new preprocessor state for the given platform.
    fn new(platform: Platform) -> Self {
        let mut defined_macros = std::collections::HashSet::new();
        for macro_name in platform.predefined_macros() {
            defined_macros.insert((*macro_name).to_string());
        }
        Self {
            condition_stack: Vec::new(),
            defined_macros,
        }
    }

    /// Check if code should currently be included.
    fn is_active(&self) -> bool {
        self.condition_stack.iter().all(|(active, _)| *active)
    }

    /// Handle an #ifdef directive.
    fn handle_ifdef(&mut self, name: &str) {
        let is_defined = self.defined_macros.contains(name);
        self.condition_stack.push((is_defined, is_defined));
    }

    /// Handle an #ifndef directive.
    fn handle_ifndef(&mut self, name: &str) {
        let is_not_defined = !self.defined_macros.contains(name);
        self.condition_stack.push((is_not_defined, is_not_defined));
    }

    /// Handle an #if directive.
    ///
    /// This does basic evaluation of `defined(X)` expressions and simple
    /// boolean combinations. Complex expressions default to false.
    fn handle_if(&mut self, condition: &str) {
        let result = self.evaluate_condition(condition);
        self.condition_stack.push((result, result));
    }

    /// Handle an #elif directive.
    fn handle_elif(&mut self, condition: &str) {
        if let Some((active, has_been_active)) = self.condition_stack.pop() {
            if has_been_active {
                // A previous branch was taken, so this one isn't
                self.condition_stack.push((false, true));
            } else {
                // No branch taken yet, evaluate this one
                let result = self.evaluate_condition(condition);
                self.condition_stack.push((result, result));
            }
            // Restore the parent active state awareness
            let _ = active; // suppress warning
        }
    }

    /// Handle an #else directive.
    fn handle_else(&mut self) {
        if let Some((_, has_been_active)) = self.condition_stack.pop() {
            // #else is active only if no previous branch was taken
            self.condition_stack.push((!has_been_active, true));
        }
    }

    /// Handle an #endif directive.
    fn handle_endif(&mut self) {
        self.condition_stack.pop();
    }

    /// Handle a #define directive (adds to defined macros).
    fn handle_define(&mut self, name: &str) {
        self.defined_macros.insert(name.to_string());
    }

    /// Evaluate a preprocessor condition expression.
    ///
    /// Supports:
    /// - `defined(X)` and `defined X`
    /// - `!defined(X)`
    /// - Simple `||` and `&&` combinations
    /// - Bare macro names (check if defined)
    fn evaluate_condition(&self, condition: &str) -> bool {
        let condition = condition.trim();

        // Handle || (logical OR)
        if let Some(pos) = condition.find("||") {
            let left = &condition[..pos];
            let right = &condition[pos + 2..];
            return self.evaluate_condition(left) || self.evaluate_condition(right);
        }

        // Handle && (logical AND)
        if let Some(pos) = condition.find("&&") {
            let left = &condition[..pos];
            let right = &condition[pos + 2..];
            return self.evaluate_condition(left) && self.evaluate_condition(right);
        }

        // Handle !defined(X) or !defined X
        if condition.starts_with('!') {
            return !self.evaluate_condition(&condition[1..]);
        }

        // Handle defined(X) or defined X
        if condition.starts_with("defined") {
            let rest = condition["defined".len()..].trim();
            let name = if rest.starts_with('(') {
                // defined(X)
                rest.trim_start_matches('(').trim_end_matches(')').trim()
            } else {
                // defined X
                rest.split_whitespace().next().unwrap_or("")
            };
            return self.defined_macros.contains(name);
        }

        // Handle parenthesized expression
        if condition.starts_with('(') && condition.ends_with(')') {
            return self.evaluate_condition(&condition[1..condition.len() - 1]);
        }

        // Bare macro name - check if defined and non-zero
        // For simplicity, we just check if it's defined
        if condition.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return self.defined_macros.contains(condition);
        }

        // Can't evaluate - default to false
        false
    }
}

// ============================================================================
// Full Header Parser
// ============================================================================

/// Parse a complete header file with all declaration types.
pub fn parse_header_full(tokens: &[Token], platform: Platform) -> HeaderParseResult {
    let mut result = HeaderParseResult::default();
    let mut state = PreprocessorState::new(platform);
    let mut pos = 0;

    while pos < tokens.len() {
        match &tokens[pos] {
            Token::Preprocessor(directive) => {
                handle_preprocessor_directive(&mut state, &mut result, directive);
                pos += 1;
            }
            Token::Eof => break,
            _ if !state.is_active() => {
                // Skip tokens when inside inactive conditional
                pos += 1;
            }
            Token::Typedef => {
                // Could be typedef struct or typedef of a basic type
                if let Some((s, new_pos)) = try_parse_typedef_struct(tokens, pos) {
                    result.structs.push(s);
                    pos = new_pos;
                } else {
                    pos = skip_definition(tokens, pos);
                }
            }
            Token::Struct => {
                // struct Name { ... }; or struct { ... } var;
                if let Some((s, new_pos)) = try_parse_struct(tokens, pos) {
                    result.structs.push(s);
                    pos = new_pos;
                } else {
                    pos = skip_definition(tokens, pos);
                }
            }
            Token::Enum | Token::Union => {
                // Skip enum and union definitions
                pos = skip_definition(tokens, pos);
            }
            _ => {
                // Try to parse a function declaration
                if let Some((func, new_pos)) = try_parse_function(tokens, pos) {
                    result.functions.push(func);
                    pos = new_pos;
                } else {
                    pos += 1;
                }
            }
        }
    }

    result
}

/// Handle a preprocessor directive, updating state and extracting constants.
fn handle_preprocessor_directive(
    state: &mut PreprocessorState,
    result: &mut HeaderParseResult,
    directive: &PreprocessorDirective,
) {
    match directive {
        PreprocessorDirective::Define { name, value } => {
            // Always record the define for conditional compilation
            state.handle_define(name);

            // Only extract as constant if we're in active code
            if state.is_active() {
                if let Some(value_str) = value {
                    let constant = CConstant {
                        name: name.clone(),
                        value: parse_constant_value(value_str),
                    };
                    result.constants.push(constant);
                }
            }
        }
        PreprocessorDirective::FunctionMacro { name } => {
            // Record function macros as defined (for #ifdef checks)
            state.handle_define(name);
        }
        PreprocessorDirective::Ifdef { name } => {
            state.handle_ifdef(name);
        }
        PreprocessorDirective::Ifndef { name } => {
            state.handle_ifndef(name);
        }
        PreprocessorDirective::If { condition } => {
            state.handle_if(condition);
        }
        PreprocessorDirective::Elif { condition } => {
            state.handle_elif(condition);
        }
        PreprocessorDirective::Else => {
            state.handle_else();
        }
        PreprocessorDirective::Endif => {
            state.handle_endif();
        }
        PreprocessorDirective::Other => {
            // #include, #pragma, etc. - ignored
        }
    }
}

// ============================================================================
// Struct Parsing
// ============================================================================

/// Try to parse a `struct Name { ... };` definition.
fn try_parse_struct(tokens: &[Token], start: usize) -> Option<(CStruct, usize)> {
    let mut pos = start;

    // Expect 'struct'
    if !matches!(tokens.get(pos), Some(Token::Struct)) {
        return None;
    }
    pos += 1;

    // Get struct name (optional for anonymous structs)
    let name = match tokens.get(pos) {
        Some(Token::Ident(n)) => {
            pos += 1;
            n.clone()
        }
        Some(Token::LBrace) => String::new(), // Anonymous struct
        _ => return None,
    };

    // Expect '{'
    if !matches!(tokens.get(pos), Some(Token::LBrace)) {
        // This might be a forward declaration like `struct Foo;`
        if matches!(tokens.get(pos), Some(Token::Semicolon)) {
            return None; // Skip forward declarations
        }
        // Or a variable declaration like `struct Foo x;`
        return None;
    }
    pos += 1;

    // Parse members
    let mut members = Vec::new();
    while pos < tokens.len() {
        match tokens.get(pos) {
            Some(Token::RBrace) => {
                pos += 1;
                break;
            }
            Some(Token::Eof) => break,
            _ => {
                if let Some((member, new_pos)) = try_parse_struct_member(tokens, pos) {
                    members.push(member);
                    pos = new_pos;
                } else {
                    // Skip unknown token
                    pos += 1;
                }
            }
        }
    }

    // Expect ';' after '}'
    if matches!(tokens.get(pos), Some(Token::Semicolon)) {
        pos += 1;
    }

    // Skip anonymous structs (no name)
    if name.is_empty() {
        return None;
    }

    Some((CStruct { name, members }, pos))
}

/// Try to parse a `typedef struct { ... } Name;` definition.
fn try_parse_typedef_struct(tokens: &[Token], start: usize) -> Option<(CStruct, usize)> {
    let mut pos = start;

    // Expect 'typedef'
    if !matches!(tokens.get(pos), Some(Token::Typedef)) {
        return None;
    }
    pos += 1;

    // Expect 'struct'
    if !matches!(tokens.get(pos), Some(Token::Struct)) {
        return None;
    }
    pos += 1;

    // Optional struct tag name
    let _tag_name = match tokens.get(pos) {
        Some(Token::Ident(n)) => {
            pos += 1;
            Some(n.clone())
        }
        _ => None,
    };

    // Expect '{'
    if !matches!(tokens.get(pos), Some(Token::LBrace)) {
        return None;
    }
    pos += 1;

    // Parse members
    let mut members = Vec::new();
    while pos < tokens.len() {
        match tokens.get(pos) {
            Some(Token::RBrace) => {
                pos += 1;
                break;
            }
            Some(Token::Eof) => break,
            _ => {
                if let Some((member, new_pos)) = try_parse_struct_member(tokens, pos) {
                    members.push(member);
                    pos = new_pos;
                } else {
                    pos += 1;
                }
            }
        }
    }

    // Get typedef name
    let name = match tokens.get(pos) {
        Some(Token::Ident(n)) => {
            pos += 1;
            n.clone()
        }
        _ => return None,
    };

    // Expect ';'
    if matches!(tokens.get(pos), Some(Token::Semicolon)) {
        pos += 1;
    }

    Some((CStruct { name, members }, pos))
}

/// Try to parse a struct member declaration.
fn try_parse_struct_member(tokens: &[Token], pos: usize) -> Option<(CStructMember, usize)> {
    let (type_parts, is_pointer, mut pos) = parse_type(tokens, pos)?;

    // Get member name
    let name = match tokens.get(pos) {
        Some(Token::Ident(n)) => {
            pos += 1;
            n.clone()
        }
        _ => return None,
    };

    // Check for array size
    let array_size = if matches!(tokens.get(pos), Some(Token::LBracket)) {
        pos += 1; // consume '['

        // Get size
        let size = match tokens.get(pos) {
            Some(Token::IntegerLiteral(n)) => {
                pos += 1;
                Some(*n as usize)
            }
            Some(Token::Ident(_)) => {
                // Size is a macro - skip for now
                pos += 1;
                None
            }
            _ => None,
        };

        // Expect ']'
        if matches!(tokens.get(pos), Some(Token::RBracket)) {
            pos += 1;
        }

        size
    } else {
        None
    };

    // Expect ';'
    if matches!(tokens.get(pos), Some(Token::Semicolon)) {
        pos += 1;
    }

    let typ = c_type_to_basic(&type_parts.join(" "), is_pointer);

    Some((
        CStructMember {
            name,
            typ,
            array_size,
        },
        pos,
    ))
}

// ============================================================================
// Legacy Function Parser
// ============================================================================

/// Parse function declarations from tokens (legacy API).
///
/// This function provides backward compatibility with the old parser.
/// It skips preprocessor directives and typedef/struct/enum definitions.
pub fn parse_functions(tokens: &[Token]) -> Vec<CFunction> {
    let mut functions = Vec::new();
    let mut pos = 0;

    while pos < tokens.len() {
        match &tokens[pos] {
            // Skip preprocessor directives
            Token::Preprocessor(_) => {
                pos += 1;
            }
            // Skip typedef, struct, enum definitions
            Token::Typedef | Token::Struct | Token::Enum | Token::Union => {
                pos = skip_definition(tokens, pos);
            }
            Token::Eof => break,
            _ => {
                // Try to parse a function declaration
                if let Some((func, new_pos)) = try_parse_function(tokens, pos) {
                    functions.push(func);
                    pos = new_pos;
                } else {
                    pos += 1;
                }
            }
        }
    }

    functions
}

/// Skip a typedef, struct, or enum definition.
fn skip_definition(tokens: &[Token], mut pos: usize) -> usize {
    // Skip until we find a semicolon outside of braces
    let mut brace_depth: usize = 0;

    while pos < tokens.len() {
        match &tokens[pos] {
            Token::LBrace => brace_depth += 1,
            Token::RBrace => brace_depth = brace_depth.saturating_sub(1),
            Token::Semicolon if brace_depth == 0 => {
                return pos + 1;
            }
            Token::Eof => return pos,
            _ => {}
        }
        pos += 1;
    }
    pos
}

/// Try to parse a function declaration starting at the given position.
///
/// Returns the parsed function and the position after the declaration,
/// or None if this doesn't look like a function declaration.
fn try_parse_function(tokens: &[Token], start: usize) -> Option<(CFunction, usize)> {
    let mut pos = start;

    // Skip any leading modifiers (extern, static, inline, const)
    while matches!(
        tokens.get(pos),
        Some(Token::Extern | Token::Static | Token::Inline | Token::Const)
    ) {
        pos += 1;
    }

    // Parse return type
    let (return_type_parts, is_return_ptr, new_pos) = parse_type(tokens, pos)?;
    pos = new_pos;

    // Next should be the function name (identifier)
    let func_name = match tokens.get(pos) {
        Some(Token::Ident(name)) => name.clone(),
        _ => return None,
    };
    pos += 1;

    // Next should be opening paren
    if !matches!(tokens.get(pos), Some(Token::LParen)) {
        return None;
    }
    pos += 1;

    // Parse parameters
    let mut params = Vec::new();

    // Handle void parameter (no params)
    if matches!(
        (tokens.get(pos), tokens.get(pos + 1)),
        (Some(Token::Ident(name)), Some(Token::RParen)) if name == "void"
    ) {
        pos += 2; // Skip "void" and ")"
    } else {
        // Parse parameter list
        loop {
            match tokens.get(pos) {
                Some(Token::RParen) => {
                    pos += 1;
                    break;
                }
                Some(Token::Eof) => return None,
                Some(Token::Comma) => {
                    pos += 1;
                    continue;
                }
                _ => {
                    // Try to parse a parameter
                    if let Some((param, new_pos)) = parse_parameter(tokens, pos) {
                        params.push(param);
                        pos = new_pos;
                    } else {
                        // Couldn't parse parameter, skip to next comma or rparen
                        while !matches!(
                            tokens.get(pos),
                            Some(Token::Comma | Token::RParen | Token::Eof)
                        ) {
                            pos += 1;
                        }
                    }
                }
            }
        }
    }

    // Should end with semicolon (declaration) or lbrace (definition)
    // Skip lbrace and body for definitions
    match tokens.get(pos) {
        Some(Token::Semicolon) => pos += 1,
        Some(Token::LBrace) => {
            // Skip function body
            pos = skip_definition(tokens, pos);
        }
        _ => return None,
    }

    // Convert return type to BASIC
    let return_type = c_type_to_basic(&return_type_parts.join(" "), is_return_ptr);

    Some((
        CFunction {
            name: func_name,
            return_type,
            params,
        },
        pos,
    ))
}

/// Parse a C type (possibly with const and pointer modifiers).
///
/// Returns (type parts, is_pointer, new position).
fn parse_type(tokens: &[Token], mut pos: usize) -> Option<(Vec<String>, bool, usize)> {
    let mut type_parts = Vec::new();
    let mut is_pointer = false;

    // Collect type keywords
    loop {
        match tokens.get(pos) {
            Some(Token::Const) => {
                type_parts.push("const".to_string());
                pos += 1;
            }
            Some(Token::Ident(name)) => {
                // Check if this is a type name or could be the next identifier
                let is_type = matches!(
                    name.as_str(),
                    "void"
                        | "char"
                        | "short"
                        | "int"
                        | "long"
                        | "float"
                        | "double"
                        | "signed"
                        | "unsigned"
                        | "int8_t"
                        | "int16_t"
                        | "int32_t"
                        | "int64_t"
                        | "uint8_t"
                        | "uint16_t"
                        | "uint32_t"
                        | "uint64_t"
                        | "size_t"
                        | "ssize_t"
                        | "intptr_t"
                        | "uintptr_t"
                        | "ptrdiff_t"
                );

                if is_type || type_parts.is_empty() {
                    type_parts.push(name.clone());
                    pos += 1;
                } else {
                    break;
                }
            }
            Some(Token::Star) => {
                is_pointer = true;
                pos += 1;
            }
            _ => break,
        }
    }

    if type_parts.is_empty() {
        return None;
    }

    Some((type_parts, is_pointer, pos))
}

/// Parse a function parameter.
fn parse_parameter(tokens: &[Token], pos: usize) -> Option<(CParam, usize)> {
    let (type_parts, is_pointer, mut pos) = parse_type(tokens, pos)?;

    // Optional parameter name
    let name = if let Some(Token::Ident(name)) = tokens.get(pos) {
        // Check it's not a comma or rparen (unnamed param)
        if !matches!(
            tokens.get(pos + 1),
            Some(Token::Comma | Token::RParen | Token::LParen)
        ) || matches!(tokens.get(pos + 1), None)
        {
            pos += 1;
            name.clone()
        } else {
            pos += 1;
            name.clone()
        }
    } else {
        String::new()
    };

    // Handle array syntax (e.g., `int arr[]`)
    if matches!(tokens.get(pos), Some(Token::Ident(s)) if s == "[") {
        // Skip array brackets
        while !matches!(
            tokens.get(pos),
            Some(Token::Comma | Token::RParen | Token::Eof)
        ) {
            pos += 1;
        }
    }

    let typ = c_type_to_basic(&type_parts.join(" "), is_pointer);

    Some((CParam { name, typ }, pos))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::header_parser::ConstantValue;
    use crate::header_parser::lexer::tokenize;
    use crate::semantic::types::BasicType;

    // ========================================================================
    // Function Parsing Tests (Legacy API)
    // ========================================================================

    #[test]
    fn test_parse_simple_function() {
        let input = "int foo(int x);";
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "foo");
        assert_eq!(funcs[0].return_type, BasicType::Long);
        assert_eq!(funcs[0].params.len(), 1);
        assert_eq!(funcs[0].params[0].name, "x");
        assert_eq!(funcs[0].params[0].typ, BasicType::Long);
    }

    #[test]
    fn test_parse_void_function() {
        let input = "void bar(void);";
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "bar");
        assert_eq!(funcs[0].return_type, BasicType::Void);
        assert_eq!(funcs[0].params.len(), 0);
    }

    #[test]
    fn test_parse_string_function() {
        let input = "char* strdup(const char* s);";
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "strdup");
        assert_eq!(funcs[0].return_type, BasicType::String);
        assert_eq!(funcs[0].params.len(), 1);
        assert_eq!(funcs[0].params[0].typ, BasicType::String);
    }

    #[test]
    fn test_parse_multiple_functions() {
        let input = r#"
            int foo(int x);
            double bar(float a, float b);
            void baz(void);
        "#;
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 3);
        assert_eq!(funcs[0].name, "foo");
        assert_eq!(funcs[1].name, "bar");
        assert_eq!(funcs[2].name, "baz");
    }

    #[test]
    fn test_skip_typedef() {
        let input = r#"
            typedef struct { int x; } MyStruct;
            int foo(void);
        "#;
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name, "foo");
    }

    // ========================================================================
    // Full Header Parser Tests
    // ========================================================================

    #[test]
    fn test_parse_header_full_basic() {
        let input = r#"
            #define VERSION 100
            struct Point { int x; int y; };
            int add(int a, int b);
        "#;
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.constants.len(), 1);
        assert_eq!(result.constants[0].name, "VERSION");
        assert_eq!(result.constants[0].value, ConstantValue::Integer(100));

        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.structs[0].name, "Point");
        assert_eq!(result.structs[0].members.len(), 2);

        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].name, "add");
    }

    #[test]
    fn test_ifdef_windows_on_windows() {
        let input = r#"
            #ifdef WIN32
            int win_func();
            #endif
            int cross_platform();
        "#;
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Windows);

        // On Windows, both functions should be included
        assert_eq!(result.functions.len(), 2);
        assert!(result.functions.iter().any(|f| f.name == "win_func"));
        assert!(result.functions.iter().any(|f| f.name == "cross_platform"));
    }

    #[test]
    fn test_ifdef_windows_on_linux() {
        let input = r#"
            #ifdef WIN32
            int win_func();
            #endif
            int cross_platform();
        "#;
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        // On Linux, only cross_platform should be included
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].name, "cross_platform");
    }

    #[test]
    fn test_ifndef() {
        let input = r#"
            #ifndef _WIN32
            int unix_func();
            #endif
        "#;
        let tokens = tokenize(input);

        // On Linux, unix_func should be included
        let result_linux = parse_header_full(&tokens, Platform::Linux);
        assert_eq!(result_linux.functions.len(), 1);
        assert_eq!(result_linux.functions[0].name, "unix_func");

        // On Windows, unix_func should NOT be included
        let result_windows = parse_header_full(&tokens, Platform::Windows);
        assert_eq!(result_windows.functions.len(), 0);
    }

    #[test]
    fn test_ifdef_else() {
        let input = r#"
            #ifdef WIN32
            int win_func();
            #else
            int unix_func();
            #endif
        "#;
        let tokens = tokenize(input);

        // On Windows
        let result_win = parse_header_full(&tokens, Platform::Windows);
        assert_eq!(result_win.functions.len(), 1);
        assert_eq!(result_win.functions[0].name, "win_func");

        // On Linux
        let result_linux = parse_header_full(&tokens, Platform::Linux);
        assert_eq!(result_linux.functions.len(), 1);
        assert_eq!(result_linux.functions[0].name, "unix_func");
    }

    #[test]
    fn test_nested_ifdef() {
        let input = r#"
            #ifdef __unix__
            int unix_func();
            #ifdef __linux__
            int linux_specific();
            #endif
            #endif
        "#;
        let tokens = tokenize(input);

        let result = parse_header_full(&tokens, Platform::Linux);
        assert_eq!(result.functions.len(), 2);
        assert!(result.functions.iter().any(|f| f.name == "unix_func"));
        assert!(result.functions.iter().any(|f| f.name == "linux_specific"));
    }

    #[test]
    fn test_if_defined() {
        let input = r#"
            #if defined(WIN32) || defined(_WIN32)
            int win_func();
            #endif
        "#;
        let tokens = tokenize(input);

        let result = parse_header_full(&tokens, Platform::Windows);
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].name, "win_func");

        let result = parse_header_full(&tokens, Platform::Linux);
        assert_eq!(result.functions.len(), 0);
    }

    #[test]
    fn test_if_not_defined() {
        let input = r#"
            #if !defined(_WIN32)
            int non_win_func();
            #endif
        "#;
        let tokens = tokenize(input);

        let result = parse_header_full(&tokens, Platform::Linux);
        assert_eq!(result.functions.len(), 1);

        let result = parse_header_full(&tokens, Platform::Windows);
        assert_eq!(result.functions.len(), 0);
    }

    // ========================================================================
    // Constant Parsing Tests
    // ========================================================================

    #[test]
    fn test_parse_define_integer() {
        let input = "#define FOO 123";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.constants.len(), 1);
        assert_eq!(result.constants[0].name, "FOO");
        assert_eq!(result.constants[0].value, ConstantValue::Integer(123));
    }

    #[test]
    fn test_parse_define_hex() {
        let input = "#define HEX 0xFF";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.constants.len(), 1);
        assert_eq!(result.constants[0].name, "HEX");
        assert_eq!(result.constants[0].value, ConstantValue::Integer(255));
    }

    #[test]
    fn test_parse_define_no_value() {
        let input = "#define FEATURE_ENABLED";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        // Constants without values are not added to constants list
        // (they're only used for #ifdef checks)
        assert_eq!(result.constants.len(), 0);
    }

    #[test]
    fn test_skip_function_macros() {
        let input = "#define MAX(a, b) ((a) > (b) ? (a) : (b))";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        // Function-like macros should not be in constants
        assert_eq!(result.constants.len(), 0);
    }

    #[test]
    fn test_constants_in_conditional() {
        let input = r#"
            #ifdef WIN32
            #define WIN_CONST 1
            #else
            #define UNIX_CONST 2
            #endif
        "#;
        let tokens = tokenize(input);

        let result = parse_header_full(&tokens, Platform::Windows);
        assert_eq!(result.constants.len(), 1);
        assert_eq!(result.constants[0].name, "WIN_CONST");

        let result = parse_header_full(&tokens, Platform::Linux);
        assert_eq!(result.constants.len(), 1);
        assert_eq!(result.constants[0].name, "UNIX_CONST");
    }

    // ========================================================================
    // Struct Parsing Tests
    // ========================================================================

    #[test]
    fn test_parse_simple_struct() {
        let input = "struct Point { int x; int y; };";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.structs[0].name, "Point");
        assert_eq!(result.structs[0].members.len(), 2);
        assert_eq!(result.structs[0].members[0].name, "x");
        assert_eq!(result.structs[0].members[0].typ, BasicType::Long);
        assert_eq!(result.structs[0].members[1].name, "y");
    }

    #[test]
    fn test_parse_typedef_struct() {
        let input = "typedef struct { int x; int y; } Point;";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.structs[0].name, "Point");
        assert_eq!(result.structs[0].members.len(), 2);
    }

    #[test]
    fn test_parse_typedef_struct_with_tag() {
        let input = "typedef struct _Point { int x; int y; } Point;";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.structs[0].name, "Point"); // Uses typedef name, not tag
    }

    #[test]
    fn test_parse_struct_with_array() {
        let input = "struct Person { char name[64]; int age; };";
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.structs[0].members.len(), 2);
        assert_eq!(result.structs[0].members[0].name, "name");
        assert_eq!(result.structs[0].members[0].array_size, Some(64));
        assert_eq!(result.structs[0].members[1].name, "age");
        assert_eq!(result.structs[0].members[1].array_size, None);
    }

    #[test]
    fn test_parse_struct_various_types() {
        let input = r#"
            struct Data {
                char c;
                short s;
                int i;
                long long ll;
                float f;
                double d;
                char* str;
                void* ptr;
            };
        "#;
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.structs.len(), 1);
        let members = &result.structs[0].members;
        assert_eq!(members.len(), 8);

        assert_eq!(members[0].typ, BasicType::Byte); // char
        assert_eq!(members[1].typ, BasicType::Integer); // short
        assert_eq!(members[2].typ, BasicType::Long); // int
        assert_eq!(members[3].typ, BasicType::Integer64); // long long
        assert_eq!(members[4].typ, BasicType::Single); // float
        assert_eq!(members[5].typ, BasicType::Double); // double
        assert_eq!(members[6].typ, BasicType::String); // char*
        assert_eq!(members[7].typ, BasicType::Offset); // void*
    }

    // ========================================================================
    // Integration Tests
    // ========================================================================

    #[test]
    fn test_full_header_integration() {
        let input = r#"
            #define VERSION 100
            #ifdef WIN32
            int win_func();
            #endif
            struct Point { int x; int y; };
            int cross_platform();
        "#;
        let tokens = tokenize(input);
        let result = parse_header_full(&tokens, Platform::Linux);

        assert_eq!(result.constants.len(), 1); // VERSION
        assert_eq!(result.structs.len(), 1); // Point
        assert_eq!(result.functions.len(), 1); // cross_platform (not win_func)
    }

    #[test]
    fn test_backward_compatibility() {
        // Verify parse_functions still works
        let input = r#"
            #define VERSION 100
            struct Point { int x; int y; };
            int foo(int x);
            void bar(void);
        "#;
        let tokens = tokenize(input);
        let funcs = parse_functions(&tokens);

        assert_eq!(funcs.len(), 2);
        assert_eq!(funcs[0].name, "foo");
        assert_eq!(funcs[1].name, "bar");
    }

    // ========================================================================
    // Preprocessor State Tests
    // ========================================================================

    #[test]
    fn test_preprocessor_state_basic() {
        let mut state = PreprocessorState::new(Platform::Linux);

        assert!(state.is_active());

        state.handle_ifdef("__linux__"); // defined on Linux
        assert!(state.is_active());

        state.handle_endif();
        assert!(state.is_active());

        state.handle_ifdef("WIN32"); // not defined on Linux
        assert!(!state.is_active());

        state.handle_endif();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_nested() {
        let mut state = PreprocessorState::new(Platform::Linux);

        state.handle_ifdef("__unix__"); // true
        assert!(state.is_active());

        state.handle_ifdef("WIN32"); // false (nested)
        assert!(!state.is_active());

        state.handle_endif();
        assert!(state.is_active()); // back to outer scope

        state.handle_endif();
        assert!(state.is_active());
    }

    #[test]
    fn test_evaluate_condition_defined() {
        let state = PreprocessorState::new(Platform::Windows);

        assert!(state.evaluate_condition("defined(WIN32)"));
        assert!(state.evaluate_condition("defined WIN32"));
        assert!(!state.evaluate_condition("defined(__linux__)"));
    }

    #[test]
    fn test_evaluate_condition_or() {
        let state = PreprocessorState::new(Platform::Windows);

        assert!(state.evaluate_condition("defined(WIN32) || defined(__linux__)"));
        assert!(!state.evaluate_condition("defined(__linux__) || defined(__APPLE__)"));
    }

    #[test]
    fn test_evaluate_condition_and() {
        let state = PreprocessorState::new(Platform::Windows);

        assert!(!state.evaluate_condition("defined(WIN32) && defined(__linux__)"));
        assert!(state.evaluate_condition("defined(WIN32) && defined(_WIN32)"));
    }

    #[test]
    fn test_evaluate_condition_not() {
        let state = PreprocessorState::new(Platform::Linux);

        assert!(state.evaluate_condition("!defined(WIN32)"));
        assert!(!state.evaluate_condition("!defined(__linux__)"));
    }
}
