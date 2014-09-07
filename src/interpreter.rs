use std::io::process::{Command, ProcessOutput};
use std::os;

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
        cmd.args(self.flags.as_slice()).arg(cwd.join(source));

        cmd
    }

    // Replaces the version flag field (e.g. `--version`) by its output (`Python 2.7.8`)
    pub fn fetch_version(&mut self) {
        let mut cmd = Command::new(self.command.as_slice());
        cmd.arg(self.version.as_slice());

        match cmd.output() {
            Err(_) => fail!("Couldn't get version of {}", self.command),
            Ok(ProcessOutput { status: exit, output: out, error: err }) => if exit.success() {
                self.version =
                    String::from_utf8(out).unwrap().
                        append(String::from_utf8(err).unwrap().as_slice());
            } else {
                fail!("{}:\n{}", cmd, String::from_utf8(err).unwrap());
            }
        }
    }

    pub fn version(&self) -> &str {
        self.version.as_slice()
    }
}
