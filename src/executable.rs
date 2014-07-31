use criterion::Criterion;
use std::io::{fs,File,TempDir,UserDir};
use std::io::process::{Command,InheritFd,ProcessOutput};

use solution::Solution;

pub struct Executable<'s, 'l, 'p> {
    cmd: Command,
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
                    None => fail!("Compilation error"),
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
        Criterion::default().bench_prog(id, self.cmd.clone().cwd(pid_dir));

        // Make raw data available
        let pid = self.solution.problem().id();
        let lang = self.solution.language().name();
        let from = Path::new(format!(".criterion/{}/{}/new/bootstrap/estimates.json", pid, lang));
        let to = Path::new(format!("raw/{}/{}", pid, lang));

        fs::mkdir_recursive(&to, UserDir).ok().expect("Couldn't create raw directory");

        // HACK Use python pretty printer, because Criterion data is ugly JSON encoded
        let mut pp = Command::new("python").
            args(["-m", "json.tool"]).
            spawn().
            ok().expect("Couldn't spawn python -m json.tool");

        {
            let mut stdin = pp.stdin.take_unwrap();
            let json = File::open(&from).
                read_to_string().
                ok().expect("Couldn't find estimates.json");
            stdin.write_str(json.as_slice()).
                ok().expect("Couldn't send input to python -m json.tool");
        }

        let pretty = pp.wait_with_output().ok().expect("Pretty printing failed").output;

        File::create(&to.join(from.filename().unwrap())).
            write(pretty.as_slice()).
            ok().expect("Couldn't write estimates.json");
    }

    pub fn validate(&self) -> Option<bool> {
        let answer = self.solution.problem().answer().as_bytes();
        let name = self.solution.language().name();
        let pid = self.solution.problem().id();
        let pid_dir = self.solution.problem().directory();

        info!("{}: Validating {}", pid, name)
        match self.cmd.clone().cwd(pid_dir).arg("-a").stderr(InheritFd(2)).output() {
            Err(_) => fail!("Runtime error"),
            Ok(ProcessOutput { output: out, .. }) => Some(out.as_slice() == answer)
        }
    }
}
