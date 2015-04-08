use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use criterion::Criterion;
use tempdir::TempDir;

use solution::Solution;

pub struct Executable<'s, 'l: 's, 'p: 's> {
    cmd: Box<Fn() -> Command + 'static>,
    solution: &'s Solution<'l, 'p>,
    _temp_dir: Option<TempDir>,
}

impl<'s, 'l, 'p> Executable<'s, 'l, 'p> {
    pub fn new(solution: &'s Solution<'l, 'p>) -> Option<Executable<'s, 'l, 'p>> {
        let pid = solution.problem().id();
        let name = solution.language().name();

        let compiler_output = match solution.language().compiler() {
            Some(compiler) => {
                info!("{}: Compiling {}", pid, name);

                match compiler.compile(solution.source()) {
                    Some(output) => Some(output),
                    None => panic!("Compilation error"),
                }
            },
            None => None,
        };

        match solution.language().interpreter() {
            Some(interpreter) => Some(Executable {
                cmd: interpreter.execute(solution.source()),
                solution: solution,
                _temp_dir: None
            }),
            None => match compiler_output {
                Some(output) => Some(Executable {
                    cmd: output.command(),
                    solution: solution,
                    _temp_dir: Some(output.temp_dir()),
                }),
                None => {
                    // No compiler and no interpreter!
                    unreachable!();
                },
            },
        }
    }

    pub fn bench(&self) {
        let pid = self.solution.problem().id();
        let language = self.solution.language().name();
        let pid_dir = self.solution.problem().directory();

        let id = format!("{}/{}", pid, language);
        let mut cmd = (self.cmd)();
        cmd.current_dir(pid_dir);
        Criterion::default().bench_program(id.as_slice(), cmd);

        // Make raw data available
        let pid = self.solution.problem().id();
        let lang = self.solution.language().name();
        let from = PathBuf::from(format!(".criterion/{}/{}/new/estimates.json", pid, lang));
        let to = PathBuf::from(format!("raw/{}/{}", pid, lang));

        fs::create_dir_all(&to).ok().expect("Couldn't create raw directory");

        fs::copy(&from, &to.join(from.file_name().unwrap())).
            ok().expect("Couldn't copy estimates.json");
    }

    pub fn validate(&self) -> Option<bool> {
        let answer = self.solution.problem().answer().as_bytes();
        let name = self.solution.language().name();
        let pid = self.solution.problem().id();
        let pid_dir = self.solution.problem().directory();

        info!("{}: Validating {}", pid, name);
        match (self.cmd)().current_dir(pid_dir).arg("-a").output() {
            Err(_) => panic!("Runtime error"),
            Ok(Output { stdout: out, .. }) => Some(out == answer)
        }
    }
}
