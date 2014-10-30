use std::io::process::{Command, ProcessOutput};
use std::{os, str};

#[deriving(Decodable)]
pub struct Interpreter {
    command: String,
    flags: Vec<String>,
    version: String,
}

impl Interpreter {
    pub fn command(&self) -> &str {
        self.command.as_slice()
    }

    pub fn execute(&self, source: &Path) -> Command {
        let cwd = os::getcwd();
        let mut cmd = Command::new(self.command.as_slice());
        cmd.args(self.flags[]).arg(cwd.join(source));

        cmd
    }

    // Replaces the version flag field (e.g. `--version`) by its output (`Python 2.7.8`)
    pub fn fetch_version(&mut self) {
        let mut cmd = Command::new(self.command.as_slice());
        cmd.arg(self.version.as_slice());

        match cmd.output() {
            Err(_) => panic!("Couldn't get version of {}", self.command),
            Ok(ProcessOutput { status: exit, output: out, error: err }) => if exit.success() {
                let mut v = String::from_utf8(out).unwrap();
                v.push_str(str::from_utf8(err[]).unwrap());
                self.version = v;
            } else {
                panic!("{}:\n{}", cmd, String::from_utf8(err).unwrap());
            }
        }
    }

    pub fn version(&self) -> &str {
        self.version.as_slice()
    }
}
