//! Build script for qb64fresh-runtime.
//!
//! When feature `opengl` is enabled, compiles the vendored OpenGL C wrappers
//! (gl_wrappers.c) and links against system libGL and libGLU.

use std::env;
use std::path::Path;

fn main() {
    let opengl = env::var("CARGO_FEATURE_OPENGL").is_ok();
    if opengl {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR set by Cargo");
        let c_src = Path::new(&manifest_dir).join("c_src").join("gl_wrappers.c");
        if c_src.exists() {
            cc::Build::new().file(&c_src).compile("gl_wrappers");
            println!("cargo:rustc-link-lib=GL");
            println!("cargo:rustc-link-lib=GLU");
        }
    }
}
