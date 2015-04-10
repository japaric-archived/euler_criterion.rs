//! Commands

use std::{fmt, io};
use std::process::Command;

/// Error that may arise from executing a `Command`
#[derive(Debug)]
pub enum Error {
    /// Command failed, exit code was not zero. First field is `Command`, second field is `stderr`
    Failed(String, String),
    /// IO error
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match *self {
            Failed(ref cmd, ref stderr) => {
                write!(f, "`{}` failed:\n{}", cmd, stderr)
            },
            Io(ref e) => e.fmt(f),
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

/// `Command` extension trait
pub trait Run {
    /// Execute the command, and return `stdout`, and optionally `stderr` as well
    fn run(&mut self, plus_stderr: bool) -> Result<Vec<u8>, Error>;
}

impl Run for Command {
    fn run(&mut self, plus_stderr: bool) -> Result<Vec<u8>, Error> {
        let output = try!(self.output());

        if output.status.success() {
            if plus_stderr {
                Ok(output.stdout + &output.stderr)
            } else {
                Ok(output.stdout)
            }
        } else {
            let cmd = format!("{:?}", self);
            let stderr = String::from_utf8(output.stderr).unwrap_or_else(|e| {
                String::from_utf8_lossy(&e.into_bytes()).into_owned()
            });
            Err(Error::Failed(cmd, stderr))
        }
    }
}
