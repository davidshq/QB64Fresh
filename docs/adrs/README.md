# Architecture Decision Records

This directory contains Architecture Decision Records (ADRs) for QB64Fresh.

## Index

- [ADR-0001: Implementation Language (Rust)](ADR-0001-implementation-language.md)
- [ADR-0002: Code Generation Backend (C Intermediate)](ADR-0002-code-generation-backend.md)
- [ADR-0003: Runtime Library Approach (Hybrid Rust)](ADR-0003-runtime-library-approach.md)
- [ADR-0004: Build System and Tooling (Cargo)](ADR-0004-build-system-tooling.md)
- [ADR-0005: Testing Framework](ADR-0005-testing-framework.md)
- [ADR-0006: Graphics System Architecture](ADR-0006-graphics-system.md)
- [ADR-0007: Audio System Architecture](ADR-0007-audio-system.md)
- [ADR-0008: C Interoperability (DECLARE LIBRARY)](ADR-0008-c-interoperability.md)
- [ADR-0009: LSP Architecture](ADR-0009-lsp-architecture.md)
- [ADR-0010: Parser Modularization](ADR-0010-parser-modularization.md)
- [ADR-0011: Error Handling](ADR-0011-error-handling.md)
- [ADR-0012: Preprocessor Architecture](ADR-0012-preprocessor-architecture.md)
- [ADR-0013: Debugger Architecture (DAP, tools/debug)](ADR-0013-debugger-architecture.md)
- [ADR-0014: Scope and Intentionally Excluded Features](ADR-0014-scope-and-excluded-features.md)
- [ADR-0015: No-Sandbox Execution Model](ADR-0015-no-sandbox-execution-model.md)
- [ADR-0016: Intentional Behavioral Differences from QB64pe](ADR-0016-intentional-behavioral-differences.md)
- [ADR-0017: Generated Code Is Ephemeral](ADR-0017-generated-code-is-ephemeral.md)
- [ADR-0018: Compiler Execution Resource Limits](ADR-0018-compiler-execution-resource-limits.md)

## Format

Each ADR follows this structure:
- **Title**: Short descriptive name
- **Status**: Accepted, Proposed, Deprecated, or Superseded
- **Date**: When the decision was made
- **Context**: The issue or problem being addressed
- **Decision**: What was decided and why
- **Consequences**: Positive and negative outcomes

## About ADRs

ADRs document significant architectural decisions made in the project. They provide context for future maintainers and help explain why the codebase is structured the way it is.
