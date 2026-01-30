# QB64Fresh Documentation

Welcome to the QB64Fresh documentation! This directory contains comprehensive guides, references, and design documents for the QB64Fresh BASIC compiler.

---

## 📚 Quick Navigation

| For... | Start Here |
|--------|-----------|
| **New users** | [GETTING_STARTED.md](GETTING_STARTED.md) |
| **Contributors** | [DEVELOPMENT.md](DEVELOPMENT.md) |
| **Architecture overview** | [ARCHITECTURE.md](ARCHITECTURE.md) |
| **Language reference** | [QB64Fresh_LANGUAGE_REFERENCE.md](QB64Fresh_LANGUAGE_REFERENCE.md) |
| **API reference** | [reference/](reference/) |

---

## 🚀 Getting Started & User Guides

| Document | Description |
|----------|-------------|
| [GETTING_STARTED.md](GETTING_STARTED.md) | Tutorial: compile and run your first BASIC program |
| [QB64Fresh_HANDBOOK.md](QB64Fresh_HANDBOOK.md) | Comprehensive user guide: types, control flow, graphics, audio, files, C interop |
| [QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md](QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md) | Guide for migrating from QB64pe to QB64Fresh |

---

## 📖 Language Reference

| Document | Description |
|----------|-------------|
| [QB64Fresh_LANGUAGE_REFERENCE.md](QB64Fresh_LANGUAGE_REFERENCE.md) | Complete language reference: syntax, statements, functions |
| [QB64pe/QB64PE_LANGUAGE_SPECIFICATION.md](QB64pe/QB64PE_LANGUAGE_SPECIFICATION.md) | Detailed QB64PE language specification (reference for compatibility) |
| [QuickBasic/QUICKBASIC_4.5_LANGUAGE_SPECIFICATION.md](QuickBasic/QUICKBASIC_4.5_LANGUAGE_SPECIFICATION.md) | QuickBASIC 4.5 language specification (historical reference) |
| [QuickBasic/QUICKBASIC_4.5_OTHER_SOURCE_CONTENT.md](QuickBasic/QUICKBASIC_4.5_OTHER_SOURCE_CONTENT.md) | Additional QuickBASIC 4.5 reference material |

---

## 🏗️ Architecture & Design

| Document | Description |
|----------|-------------|
| [ARCHITECTURE.md](ARCHITECTURE.md) | High-level architecture: compiler pipeline, module organization, design decisions |
| [adrs/](adrs/) | **Architecture Decision Records** - 18 ADRs documenting key design decisions |
| [GRAPHICS.md](GRAPHICS.md) | Graphics system: architecture, backends (SDL2/Mock), implementation, supported statements |
| [QB64pe/QB64PE_ARCHITECTURE.md](QB64pe/QB64PE_ARCHITECTURE.md) | Analysis of QB64PE architecture (reference for compatibility) |

### Architecture Decision Records (ADRs)

See [adrs/README.md](adrs/README.md) for the complete index. Key decisions:

- **ADR-0001**: Implementation language (Rust)
- **ADR-0002**: Code generation backend (C intermediate)
- **ADR-0006**: Graphics system (trait-based with SDL2)
- **ADR-0007**: Audio system (trait-based with Rodio)
- **ADR-0008**: C interoperability (DECLARE LIBRARY)
- **ADR-0009**: LSP architecture
- **ADR-0013**: Debugger architecture (DAP)
- **ADR-0016**: Intentional behavioral differences from QB64pe
- **ADR-0017**: Generated code is ephemeral (fix code generator, never patch output)
- **ADR-0018**: Compiler execution resource limits (memory limits)

---

## 👨‍💻 Development Guides

| Document | Description |
|----------|-------------|
| [DEVELOPMENT.md](DEVELOPMENT.md) | Developer onboarding: setup, build, testing, contributing |
| [TESTING.md](TESTING.md) | Testing guide: unit tests, integration tests, golden tests, fuzzing |
| [DEBUGGING.md](DEBUGGING.md) | Debugging guide: using the debugger, DAP protocol, breakpoints, watch expressions |
| [reference/HEADER_PARSER_API.md](reference/HEADER_PARSER_API.md) | Rust API reference for the C header parser (`header-parsing` feature) |

---

## 🔧 System-Specific Documentation

| Document | Description |
|----------|-------------|
| [GRAPHICS.md](GRAPHICS.md) | Graphics system: SDL2 backend, mock backend, runtime modes, stub behavior |
| [SECURITY_MODEL.md](SECURITY_MODEL.md) | Security considerations: SHELL, file operations, no sandbox execution |
| [MEMORY_LIMITS.md](MEMORY_LIMITS.md) | Memory usage limits: preventing system crashes when compiling large programs |
| [QB64pe/QB64PE_DEBUGGING.md](QB64pe/QB64PE_DEBUGGING.md) | QB64PE debugging features (reference) |
| [QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md](QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md) | **QB64Fresh vs QB64pe (all differences):** intentional design choices and behavioral/architectural differences (single doc) |
| [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) | *(Redirect → QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md)* |

---

## 📋 Reference Documentation

| Document | Description |
|----------|-------------|
| [reference/HEADER_PARSER_API.md](reference/HEADER_PARSER_API.md) | Complete Rust API for C header parsing (programmatic interface) |
| [reference/README.md](reference/README.md) | Overview of API reference documentation |

---

## 📝 Planning & Future Work

| Document | Description |
|----------|-------------|
| [ThingsToDo/PARTIAL_IMPLEMENTATIONS.md](ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) | Partial implementations audit & runtime implementation plan |
| [ThingsToDo/RUNTIME_ARCHITECTURE_PERSPECTIVES.md](ThingsToDo/RUNTIME_ARCHITECTURE_PERSPECTIVES.md) | Runtime architecture considerations and trade-offs |
| [ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md](ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md) | Testing infrastructure plans and improvements |
| [ThingsToDo/INSTALLER_PLAN.md](ThingsToDo/INSTALLER_PLAN.md) | Distribution and installer planning |
| [ThingsToDo/OPENGL_SUPPORT.md](ThingsToDo/OPENGL_SUPPORT.md) | OpenGL support planning and considerations |
| [ThingsToDo/QB64Fresh_AND_MODERN_LANGUAGES.md](ThingsToDo/QB64Fresh_AND_MODERN_LANGUAGES.md) | Comparison with modern languages and use cases |
| [ThingsToDo/QB64PE_IDE_FUNCTIONALITY_CHECKLIST_TODO.md](ThingsToDo/QB64PE_IDE_FUNCTIONALITY_CHECKLIST_TODO.md) | QB64PE IDE features checklist (for reference) |
| [ThingsToDo/INFORM/INFORM_FUNCTIONALITY.md](ThingsToDo/INFORM/INFORM_FUNCTIONALITY.md) | InForm WYSIWYG UI designer documentation |
| [ThingsToDo/INFORM/INFORM_EXPERT_DISCUSSION.md](ThingsToDo/INFORM/INFORM_EXPERT_DISCUSSION.md) | Expert discussion on InForm enhancements |

---

## 📦 Historical & Archive

| Document | Description |
|----------|-------------|
| [archive/BOOTSTRAP_PLAN_FULL.md](archive/BOOTSTRAP_PLAN_FULL.md) | Complete history of compiling QB64pe with QB64Fresh (59K-line bootstrap) |
| [archive/QB64PE_ARCHITECTURE_ANALYSIS.md](archive/QB64PE_ARCHITECTURE_ANALYSIS.md) | Original QB64PE codebase analysis (used to inform QB64Fresh design) |
| [archive/QB64PE_IDE_FUNCTIONALITY_CHECKLIST.md](archive/QB64PE_IDE_FUNCTIONALITY_CHECKLIST.md) | QB64PE IDE features checklist (historical reference) |
| [archive/STUB_FUNCTIONS_FULL.md](archive/STUB_FUNCTIONS_FULL.md) | Complete list of stub functions (historical reference) |
| [archive/TESTING-COMPLETED.md](archive/TESTING-COMPLETED.md) | Completed testing milestones |
| [archive/TODO-completed.md](archive/TODO-completed.md) | Completed TODO items |
| [archive/README.md](archive/README.md) | Archive directory overview |

---

## 🗂️ Directory Structure

```
docs/
├── DOCS-README.md              # This file
├── GETTING_STARTED.md          # User tutorial
├── DEVELOPMENT.md              # Developer guide
├── ARCHITECTURE.md             # Architecture overview
├── GRAPHICS.md                 # Graphics system guide
├── DEBUGGING.md                # Debugging guide
├── TESTING.md                  # Testing guide
├── SECURITY_MODEL.md           # Security documentation
├── MEMORY_LIMITS.md            # Memory usage limits
├── QB64Fresh_HANDBOOK.md       # Comprehensive user guide
├── QB64Fresh_LANGUAGE_REFERENCE.md  # Language reference
│
├── adrs/                       # Architecture Decision Records
│   ├── README.md
│   └── ADR-0001 through ADR-0018
│
├── reference/                  # API reference documentation
│   ├── README.md
│   └── HEADER_PARSER_API.md
│
├── QB64pe/                     # QB64PE reference and migration
│   ├── QB64PE_ARCHITECTURE.md
│   ├── QB64PE_LANGUAGE_SPECIFICATION.md
│   ├── QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md
│   ├── QB64Fresh_VS_QB64pe_DIFFERENCES.md   # Single doc: intentional + behavioral differences
│   ├── QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md  # (Redirect → above)
│   └── QB64PE_DEBUGGING.md
│
├── QuickBasic/                 # QuickBASIC 4.5 reference
│   ├── QUICKBASIC_4.5_LANGUAGE_SPECIFICATION.md
│   └── QUICKBASIC_4.5_OTHER_SOURCE_CONTENT.md
│
├── ThingsToDo/                 # Planning and future work
│   ├── PARTIAL_IMPLEMENTATIONS.md
│   ├── RUNTIME_ARCHITECTURE_PERSPECTIVES.md
│   ├── TESTING_INFRASTRUCTURE_PLAN.md
│   ├── INSTALLER_PLAN.md
│   ├── OPENGL_SUPPORT.md
│   ├── QB64Fresh_AND_MODERN_LANGUAGES.md
│   ├── QB64PE_IDE_FUNCTIONALITY_CHECKLIST_TODO.md
│   └── INFORM/
│
└── archive/                    # Historical documentation
    ├── README.md
    ├── BOOTSTRAP_PLAN_FULL.md
    ├── QB64PE_ARCHITECTURE_ANALYSIS.md
    └── ...
```

---

## 🔍 Finding What You Need

### I want to...

- **Learn QB64Fresh basics** → [GETTING_STARTED.md](GETTING_STARTED.md) → [QB64Fresh_HANDBOOK.md](QB64Fresh_HANDBOOK.md)
- **Understand the architecture** → [ARCHITECTURE.md](ARCHITECTURE.md) → [adrs/](adrs/)
- **Contribute code** → [DEVELOPMENT.md](DEVELOPMENT.md) → [TESTING.md](TESTING.md)
- **Use graphics** → [GRAPHICS.md](GRAPHICS.md)
- **Debug programs** → [DEBUGGING.md](DEBUGGING.md)
- **Migrate from QB64PE** → [QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md](QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md)
- **Look up language syntax** → [QB64Fresh_LANGUAGE_REFERENCE.md](QB64Fresh_LANGUAGE_REFERENCE.md)
- **Understand design decisions** → [adrs/](adrs/)
- **Use the header parser API** → [reference/HEADER_PARSER_API.md](reference/HEADER_PARSER_API.md)

---

## 📊 Documentation Status

| Category | Status | Notes |
|----------|--------|-------|
| **User Guides** | ✅ Complete | GETTING_STARTED, HANDBOOK, MIGRATION_GUIDE |
| **Language Reference** | ✅ Complete | QB64Fresh_LANGUAGE_REFERENCE, QB64PE spec |
| **Architecture** | ✅ Complete | ARCHITECTURE.md, 18 ADRs |
| **Development** | ✅ Complete | DEVELOPMENT, TESTING, DEBUGGING |
| **System Docs** | ✅ Complete | GRAPHICS, SECURITY_MODEL, MEMORY_LIMITS |
| **API Reference** | ✅ Complete | Header parser API |
| **Planning** | 📝 In Progress | Various ThingsToDo documents |

---

## 🤝 Contributing to Documentation

Documentation improvements are welcome! When adding or updating docs:

1. **User-facing docs** → Keep in `docs/` root
2. **API references** → Add to `docs/reference/`
3. **Architecture decisions** → Create new ADR in `docs/adrs/`
4. **Historical docs** → Move to `docs/archive/` when no longer actively referenced

See [DEVELOPMENT.md](DEVELOPMENT.md) for contribution guidelines.

---

*Last updated: 2026-01-26*
