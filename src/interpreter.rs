//! Interpreters

use std::process::Command;

use command::{Error, Run};

/// An interpreter
#[derive(Debug, RustcDecodable)]
pub struct Interpreter {
    /// Interpreter command: `python`
    command: String,
    /// (Optimization) flags to pass to the interpreter: [`-O`]
    flags: Vec<String>,
    /// Version flag: `--version`
    version: String,
}

impl Interpreter {
    /// Returns the interpreter command
    pub fn command(&self) -> &str {
        &self.command
    }

    /// Returns the flags that must be passed to interpreter
    pub fn flags(&self) -> &[String] {
        &self.flags
    }

    /// Returns the interpreter version
    ///
    /// This calls e.g. `python -V`. On success, a concatenation of `stdout` and `stderr` (in that
    /// order) is returned.
    pub fn version(&self) -> Result<Vec<u8>, Error> {
        Command::new(&self.command).arg(&self.version).run(true)
    }
}
