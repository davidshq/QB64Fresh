//! Formatting rules for different token types.
//!
//! This module contains the logic for deciding how to format various tokens
//! and their surrounding whitespace.

use qb64fresh::lexer::TokenKind;

/// Determines if a token kind is a BASIC keyword.
pub fn is_keyword(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::If
            | TokenKind::Then
            | TokenKind::Else
            | TokenKind::ElseIf
            | TokenKind::End
            | TokenKind::EndIf
            | TokenKind::For
            | TokenKind::To
            | TokenKind::Step
            | TokenKind::Next
            | TokenKind::While
            | TokenKind::Wend
            | TokenKind::Do
            | TokenKind::Loop
            | TokenKind::Until
            | TokenKind::Select
            | TokenKind::Case
            | TokenKind::EveryCase
            | TokenKind::Goto
            | TokenKind::Gosub
            | TokenKind::Return
            | TokenKind::Exit
            | TokenKind::Stop
            | TokenKind::System
            | TokenKind::Sleep
            | TokenKind::Wait
            | TokenKind::Delay
            | TokenKind::Limit
            | TokenKind::KeyClear
            | TokenKind::Call
            | TokenKind::Is
            | TokenKind::ByVal
            | TokenKind::Swap
            | TokenKind::Continue
            | TokenKind::Run
            | TokenKind::Chain
            | TokenKind::Tron
            | TokenKind::Troff
            | TokenKind::Lprint
            | TokenKind::Files
            | TokenKind::Field
            | TokenKind::Lset
            | TokenKind::Rset
            | TokenKind::Dim
            | TokenKind::Redim
            | TokenKind::Erase
            | TokenKind::As
            | TokenKind::Shared
            | TokenKind::Static
            | TokenKind::Const
            | TokenKind::Type
            | TokenKind::Declare
            | TokenKind::Library
            | TokenKind::Dynamic
            | TokenKind::Alias
            | TokenKind::Sub
            | TokenKind::Function
            | TokenKind::Let
            | TokenKind::Print
            | TokenKind::Input
            | TokenKind::Open
            | TokenKind::Close
            | TokenKind::Read
            | TokenKind::Data
            | TokenKind::Restore
            | TokenKind::Write
            | TokenKind::Get
            | TokenKind::Put
            | TokenKind::Line
            | TokenKind::Seek
            | TokenKind::Access
            | TokenKind::Binary
            | TokenKind::Random
            | TokenKind::Randomize
            | TokenKind::Output
            | TokenKind::Append
            | TokenKind::Using
            | TokenKind::Len
            | TokenKind::Lock
            | TokenKind::Unlock
            | TokenKind::Screen
            | TokenKind::Cls
            | TokenKind::Color
            | TokenKind::Locate
            | TokenKind::Pset
            | TokenKind::Preset
            | TokenKind::Circle
            | TokenKind::Paint
            | TokenKind::Palette
            | TokenKind::Pcopy
            | TokenKind::Display
            | TokenKind::Width
            | TokenKind::View
            | TokenKind::Window
            | TokenKind::Draw
            | TokenKind::Beep
            | TokenKind::Sound
            | TokenKind::Play
            | TokenKind::Kill
            | TokenKind::Name
            | TokenKind::Mkdir
            | TokenKind::Rmdir
            | TokenKind::Chdir
            | TokenKind::Shell
            | TokenKind::On
            | TokenKind::ErrorKw
            | TokenKind::Resume
            | TokenKind::Option
            | TokenKind::Base
            | TokenKind::Def
            | TokenKind::Fn
            | TokenKind::Common
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Not
            | TokenKind::Xor
            | TokenKind::Eqv
            | TokenKind::Imp
            | TokenKind::AndAlso
            | TokenKind::OrElse
            | TokenKind::Mod
            | TokenKind::Integer
            | TokenKind::Long
            | TokenKind::Single
            | TokenKind::Double
            | TokenKind::String_
            | TokenKind::BitType
            | TokenKind::Byte
            | TokenKind::Integer64
            | TokenKind::Float
            | TokenKind::Offset
            | TokenKind::Unsigned
    )
}

/// Determines if a token is a binary operator that should have spaces around it.
pub fn is_binary_operator(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Backslash
            | TokenKind::Caret
            | TokenKind::Equals
            | TokenKind::NotEquals
            | TokenKind::LessThan
            | TokenKind::GreaterThan
            | TokenKind::LessEquals
            | TokenKind::GreaterEquals
            | TokenKind::NotEqualsLegacy
            | TokenKind::LessEqualsLegacy
            | TokenKind::GreaterEqualsLegacy
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Xor
            | TokenKind::Eqv
            | TokenKind::Imp
            | TokenKind::AndAlso
            | TokenKind::OrElse
            | TokenKind::Mod
    )
}

/// Determines if a token kind starts a block that should increase indentation.
pub fn starts_block(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::If
            | TokenKind::For
            | TokenKind::While
            | TokenKind::Do
            | TokenKind::Select
            | TokenKind::Sub
            | TokenKind::Function
            | TokenKind::Type
    )
}

/// Determines if a token kind ends a block that should decrease indentation.
pub fn ends_block(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::End | TokenKind::EndIf | TokenKind::Next | TokenKind::Wend | TokenKind::Loop
    )
}

/// Determines if a token is a continuation keyword (ELSE, ELSEIF, CASE).
/// These decrease indentation for the line but don't close the block.
pub fn is_continuation(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Else | TokenKind::ElseIf | TokenKind::Case)
}

/// Token kind to canonical keyword string mapping.
pub fn keyword_text(kind: &TokenKind) -> Option<&'static str> {
    Some(match kind {
        TokenKind::If => "IF",
        TokenKind::Then => "THEN",
        TokenKind::Else => "ELSE",
        TokenKind::ElseIf => "ELSEIF",
        TokenKind::End => "END",
        TokenKind::EndIf => "ENDIF",
        TokenKind::For => "FOR",
        TokenKind::To => "TO",
        TokenKind::Step => "STEP",
        TokenKind::Next => "NEXT",
        TokenKind::While => "WHILE",
        TokenKind::Wend => "WEND",
        TokenKind::Do => "DO",
        TokenKind::Loop => "LOOP",
        TokenKind::Until => "UNTIL",
        TokenKind::Select => "SELECT",
        TokenKind::Case => "CASE",
        TokenKind::EveryCase => "EVERYCASE",
        TokenKind::Goto => "GOTO",
        TokenKind::Gosub => "GOSUB",
        TokenKind::Return => "RETURN",
        TokenKind::Exit => "EXIT",
        TokenKind::Stop => "STOP",
        TokenKind::System => "SYSTEM",
        TokenKind::Sleep => "SLEEP",
        TokenKind::Wait => "WAIT",
        TokenKind::Delay => "_DELAY",
        TokenKind::Limit => "_LIMIT",
        TokenKind::KeyClear => "_KEYCLEAR",
        TokenKind::Call => "CALL",
        TokenKind::Is => "IS",
        TokenKind::ByVal => "BYVAL",
        TokenKind::Swap => "SWAP",
        TokenKind::Continue => "_CONTINUE",
        TokenKind::Run => "RUN",
        TokenKind::Chain => "CHAIN",
        TokenKind::Tron => "TRON",
        TokenKind::Troff => "TROFF",
        TokenKind::Lprint => "LPRINT",
        TokenKind::Files => "FILES",
        TokenKind::Field => "FIELD",
        TokenKind::Lset => "LSET",
        TokenKind::Rset => "RSET",
        TokenKind::Dim => "DIM",
        TokenKind::Redim => "REDIM",
        TokenKind::Erase => "ERASE",
        TokenKind::As => "AS",
        TokenKind::Shared => "SHARED",
        TokenKind::Static => "STATIC",
        TokenKind::Const => "CONST",
        TokenKind::Type => "TYPE",
        TokenKind::Declare => "DECLARE",
        TokenKind::Library => "LIBRARY",
        TokenKind::Dynamic => "DYNAMIC",
        TokenKind::Alias => "ALIAS",
        TokenKind::Sub => "SUB",
        TokenKind::Function => "FUNCTION",
        TokenKind::Let => "LET",
        TokenKind::Print => "PRINT",
        TokenKind::Input => "INPUT",
        TokenKind::Open => "OPEN",
        TokenKind::Close => "CLOSE",
        TokenKind::Read => "READ",
        TokenKind::Data => "DATA",
        TokenKind::Restore => "RESTORE",
        TokenKind::Write => "WRITE",
        TokenKind::Get => "GET",
        TokenKind::Put => "PUT",
        TokenKind::Line => "LINE",
        TokenKind::Seek => "SEEK",
        TokenKind::Access => "ACCESS",
        TokenKind::Binary => "BINARY",
        TokenKind::Random => "RANDOM",
        TokenKind::Randomize => "RANDOMIZE",
        TokenKind::Output => "OUTPUT",
        TokenKind::Append => "APPEND",
        TokenKind::Using => "USING",
        TokenKind::Len => "LEN",
        TokenKind::Lock => "LOCK",
        TokenKind::Unlock => "UNLOCK",
        TokenKind::Screen => "SCREEN",
        TokenKind::Cls => "CLS",
        TokenKind::Color => "COLOR",
        TokenKind::Locate => "LOCATE",
        TokenKind::Pset => "PSET",
        TokenKind::Preset => "PRESET",
        TokenKind::Circle => "CIRCLE",
        TokenKind::Paint => "PAINT",
        TokenKind::Palette => "PALETTE",
        TokenKind::Pcopy => "PCOPY",
        TokenKind::Display => "_DISPLAY",
        TokenKind::Width => "WIDTH",
        TokenKind::View => "VIEW",
        TokenKind::Window => "WINDOW",
        TokenKind::Draw => "DRAW",
        TokenKind::Beep => "BEEP",
        TokenKind::Sound => "SOUND",
        TokenKind::Play => "PLAY",
        TokenKind::Kill => "KILL",
        TokenKind::Name => "NAME",
        TokenKind::Mkdir => "MKDIR",
        TokenKind::Rmdir => "RMDIR",
        TokenKind::Chdir => "CHDIR",
        TokenKind::Shell => "SHELL",
        TokenKind::On => "ON",
        TokenKind::ErrorKw => "ERROR",
        TokenKind::Resume => "RESUME",
        TokenKind::Option => "OPTION",
        TokenKind::Base => "BASE",
        TokenKind::Def => "DEF",
        TokenKind::Fn => "FN",
        TokenKind::Common => "COMMON",
        TokenKind::Preserve => "_PRESERVE",
        TokenKind::And => "AND",
        TokenKind::Or => "OR",
        TokenKind::Not => "NOT",
        TokenKind::Xor => "XOR",
        TokenKind::Eqv => "EQV",
        TokenKind::Imp => "IMP",
        TokenKind::AndAlso => "_ANDALSO",
        TokenKind::OrElse => "_ORELSE",
        TokenKind::Mod => "MOD",
        TokenKind::Integer => "INTEGER",
        TokenKind::Long => "LONG",
        TokenKind::Single => "SINGLE",
        TokenKind::Double => "DOUBLE",
        TokenKind::String_ => "STRING",
        TokenKind::BitType => "_BIT",
        TokenKind::Byte => "_BYTE",
        TokenKind::Integer64 => "_INTEGER64",
        TokenKind::Float => "_FLOAT",
        TokenKind::Offset => "_OFFSET",
        TokenKind::Unsigned => "_UNSIGNED",
        _ => return None,
    })
}

/// Determines if this token should have no space before it.
pub fn no_space_before(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Comma
            | TokenKind::Semicolon
            | TokenKind::RightParen
            | TokenKind::Colon
            | TokenKind::DollarSign
            | TokenKind::Percent
            | TokenKind::Ampersand
            | TokenKind::Exclamation
            | TokenKind::Dot
    )
}

/// Determines if this token should have no space after it.
pub fn no_space_after(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::LeftParen | TokenKind::Hash | TokenKind::Dot | TokenKind::Not
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_keyword() {
        assert!(is_keyword(&TokenKind::If));
        assert!(is_keyword(&TokenKind::Print));
        assert!(!is_keyword(&TokenKind::Identifier));
        assert!(!is_keyword(&TokenKind::Plus));
    }

    #[test]
    fn test_is_binary_operator() {
        assert!(is_binary_operator(&TokenKind::Plus));
        assert!(is_binary_operator(&TokenKind::Equals));
        assert!(is_binary_operator(&TokenKind::And));
        assert!(!is_binary_operator(&TokenKind::If));
        assert!(!is_binary_operator(&TokenKind::LeftParen));
    }

    #[test]
    fn test_keyword_text() {
        assert_eq!(keyword_text(&TokenKind::Print), Some("PRINT"));
        assert_eq!(keyword_text(&TokenKind::Delay), Some("_DELAY"));
        assert_eq!(keyword_text(&TokenKind::Identifier), None);
    }
}
