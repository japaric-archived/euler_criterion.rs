//! Compilers

use std::path::{Path, PathBuf};
use std::process::Command;

use command::{Error, Run};
use tempdir::TempDir;

use context::Context;

/// A compiler
#[derive(Debug, RustcDecodable)]
pub struct Compiler {
    /// Compiler command: `gcc`, `rustc`
    command: String,
    /// (Optimization) flags to pass to the compiler: [`-O`]
    flags: Vec<String>,
    /// Template of the output file: `a.out`, `*`
    output: String,
    /// Version flag: `-v`, `-V`
    version: String,
}

impl Compiler {
    /// Returns the compiler command
    pub fn command(&self) -> &str {
        &self.command
    }

    /// Compile `source` in a temporary directory, and return the compiler output
    pub fn compile(&self, source: &Path, ctxt: &Context) -> Result<Output, Error> {
        const PREFIX: &'static str = "euler";

        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Compiler::compile: ", $template), $($args),*)
            }
        }

        let temp_dir = try!(TempDir::new_in(ctxt.temp_dir(), PREFIX));
        let mut cmd = Command::new(&self.command);
        cmd.args(&self.flags);
        cmd.arg(source);
        cmd.current_dir(temp_dir.path());

        _debug!("run `{:?}` at {:?}", cmd, temp_dir.path());
        try!(cmd.run(false));

        let path = if self.output.contains('*') {
            let stem = source.file_stem().unwrap_or_else(|| unsafe {
                debug_unreachable!();
            });

            // XXX This shouldn't use `to_string_lossy()`, but there is no way to use `replace`
            // with `OsStr`s
            temp_dir.path().join(self.output.replace("*", &stem.to_string_lossy()))
        } else {
            temp_dir.path().join(&self.output)
        };

        Ok(Output {
            path: path,
            _temp_dir: temp_dir,
        })
    }

    /// Returns the compiler version
    ///
    /// This calls e.g. `rustc -V`. On success, a concatenation of `stdout` and `stderr` (in that
    /// order) is returned.
    pub fn version(&self) -> Result<Vec<u8>, Error> {
        Command::new(&self.command).arg(&self.version).run(true)
    }
}

/// Compiler output
///
/// The compiler output lives in a temporary directory. Both the output and the temporary directory
/// will be deleted when this object goes out of scope.
pub struct Output {
    _temp_dir: TempDir,
    path: PathBuf,
}

impl Output {
    /// Returns the absolute path to the main artifact
    pub fn path(&self) -> &Path {
        &self.path
    }
}
