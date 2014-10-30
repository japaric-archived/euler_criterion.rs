use std::io::process::{Command, InheritFd, ProcessOutput};
use std::io::{fs, TempDir};
use std::str;

#[deriving(Decodable)]
pub struct Compiler {
    command: String,
    flags: Vec<String>,
    output: String,
    version: String,
}

impl Compiler {
    pub fn command(&self) -> &str {
        self.command.as_slice()
    }

    pub fn compile(&self, source: &Path) -> Option<CompilerOutput> {
        let basename = source.filename_str().unwrap();
        let filename = basename.split('.').next().unwrap();
        let temp_dir = TempDir::new(self.command.as_slice()).unwrap();

        fs::copy(source, &temp_dir.path().join(basename)).ok().
            expect("Couldn't copy the source file");

        let mut cmd = Command::new(self.command.as_slice());
        cmd.args(self.flags[]).
            arg(basename).
            cwd(temp_dir.path()).
            stdout(InheritFd(1)).
            stderr(InheritFd(2));

        match cmd.output() {
            Err(_) => None,
            Ok(ProcessOutput { status: exit, .. }) => if exit.success() {
                Some(CompilerOutput {
                    path: temp_dir.path().join(self.output.replace("*", filename)),
                    temp_dir: temp_dir,
                })
            } else {
                None
            },
        }

    }

    // Replaces the version flag field (e.g. `-v`) by its output (`rustc 0.12.0-pre`)
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

struct CompilerOutput {
    path: Path,
    temp_dir: TempDir,
}

impl CompilerOutput {
    pub fn command(&self) -> Command {
        Command::new(&self.path)
    }

    pub fn temp_dir(self) -> TempDir {
        self.temp_dir
    }
}
