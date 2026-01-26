# QB64Fresh Licensing Information

QB64Fresh uses Rust dependencies from crates.io to provide functionality. These dependencies have their own licenses that you must respect when distributing QB64Fresh or programs compiled by QB64Fresh.

## License Overview

**QB64Fresh itself:** MIT License (see root `LICENSE` file)

**Dependencies:** Almost all Rust crates used by QB64Fresh are permissively licensed (MIT, Apache-2.0, or BSD). This makes compliance straightforward - typically just including license notices is sufficient.

## Compiler Dependencies

These are the main dependencies used by the QB64Fresh compiler itself:

| Crate | License | Purpose |
|-------|---------|---------|
| logos | MIT OR Apache-2.0 | Lexer generation |
| ariadne | MIT OR Apache-2.0 | Error reporting with diagnostics |
| clap | MIT OR Apache-2.0 | CLI argument parsing |
| log | MIT OR Apache-2.0 | Logging facade |
| env_logger | MIT OR Apache-2.0 | Logging implementation |
| thiserror | MIT OR Apache-2.0 | Error handling |
| tower-lsp | MIT OR Apache-2.0 | LSP server framework |
| tokio | MIT | Async runtime (for LSP) |
| serde | MIT OR Apache-2.0 | Serialization framework |
| serde_json | MIT OR Apache-2.0 | JSON serialization |

## Runtime Dependencies

These are used by the runtime library (`qb64fresh-runtime`) that compiled programs link against:

| Crate | License | Purpose | Feature Flag |
|-------|---------|---------|--------------|
| libc | MIT OR Apache-2.0 | C library bindings | Always included |
| sdl2 | MIT OR zlib | Graphics support (SDL2 bindings) | `graphics-sdl2` |
| image | MIT OR Apache-2.0 | Image loading (_LOADIMAGE) | `graphics-sdl2` |
| rodio | MIT OR Apache-2.0 | Audio support | `audio-rodio` |
| rfd | MIT OR Apache-2.0 | Native file dialogs | `dialogs` |
| freetype-rs | MIT OR Apache-2.0 | Font rendering (_LOADFONT) | `freetype` |
| lazy_static | MIT OR Apache-2.0 | Global initialization | `freetype` |

**Note:** SDL2 itself (the C library) is zlib-licensed. The `sdl2` Rust crate is MIT OR Apache-2.0.

## Development Dependencies

These are only used during development/testing and are not included in distributed binaries:

| Crate | License | Purpose |
|-------|---------|---------|
| pretty_assertions | MIT OR Apache-2.0 | Test assertion formatting |
| tempfile | MIT OR Apache-2.0 | Temporary file creation for tests |
| proptest | MIT OR Apache-2.0 | Property-based testing |
| criterion | Apache-2.0 OR MIT | Benchmarking |
| cargo-husky | MIT OR Apache-2.0 | Git hooks management |

## License Compliance

### For Source Code Distribution

When distributing QB64Fresh source code (e.g., on GitHub):
- All dependency licenses are available in each crate's source code
- No additional license files need to be bundled
- This README serves as documentation of what licenses are used

### For Binary Distribution

When distributing QB64Fresh binaries:
- All dependencies are permissively licensed (MIT/Apache-2.0)
- Including this README with the binary satisfies license requirements
- No additional license files need to be bundled

### For Programs Compiled by QB64Fresh

Programs compiled by QB64Fresh may link against the runtime library. The runtime library uses the dependencies listed above. Since all are permissively licensed, compiled programs can be distributed without additional license requirements beyond:
- Including the QB64Fresh MIT license if you modify QB64Fresh itself
- Including this README if you distribute the runtime library separately

## Generating License Reports

To generate an up-to-date license report, install and use `cargo-licenses`:

```bash
cargo install cargo-licenses
cargo licenses --summary
cargo licenses --json > licenses.json
cargo licenses --tsv > licenses.tsv
```

This will show all transitive dependencies and their licenses.

## Key Differences from QB64pe

QB64pe includes a `licenses/` folder with license text files because:
1. They statically link C/C++ libraries (not Rust crates)
2. Some libraries (like LGPL) require source code or object files to be available
3. They bundle all libraries with the compiler distribution

QB64Fresh:
1. Uses Rust crates from crates.io (all permissively licensed)
2. License text is available in each crate's source code
3. For source distribution, no license files need to be bundled
4. This README documents what licenses are used

## Questions?

If you have questions about license compliance, please open an issue on the QB64Fresh repository.
