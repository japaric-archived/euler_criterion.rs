use std::{fs, str};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempdir::TempDir;

#[derive(RustcDecodable)]
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
        let basename = source.file_name().unwrap();
        let filename = source.file_stem().unwrap();
        let temp_dir = TempDir::new(self.command.as_slice()).unwrap();

        fs::copy(source, &temp_dir.path().join(basename)).ok().
            expect("Couldn't copy the source file");

        let mut cmd = Command::new(self.command.as_slice());
        cmd.args(&self.flags).
            arg(basename).
            current_dir(temp_dir.path());

        match cmd.output() {
            Err(_) => None,
            Ok(Output { status: exit, .. }) => if exit.success() {
                Some(CompilerOutput {
                    path: temp_dir.path().join(self.output.replace("*", &filename.to_string_lossy())),
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
            Ok(Output { status: exit, stdout: out, stderr: err }) => if exit.success() {
                let mut v = String::from_utf8(out).unwrap();
                v.push_str(str::from_utf8(&err).unwrap());
                self.version = v;
            } else {
                panic!("{:?}:\n{}", cmd, String::from_utf8(err).unwrap());
            }
        }
    }

    pub fn version(&self) -> &str {
        self.version.as_slice()
    }
}

struct CompilerOutput {
    path: PathBuf,
    temp_dir: TempDir,
}

impl CompilerOutput {
    pub fn command(&self) -> Box<Fn() -> Command> {
        let path = self.path.to_path_buf();

        Box::new(move || {
            Command::new(&path)
        })
    }

    pub fn temp_dir(self) -> TempDir {
        self.temp_dir
    }
}
