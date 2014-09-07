use std::io::{File, UserDir, fs};
use std::hash;

use language::Language;
use problem::Problem;

pub struct Solution<'l, 'p> {
    language: &'l Language,
    problem: &'p Problem,
    source: Path,
}

impl<'l, 'p> Solution<'l, 'p> {
    pub fn new(language: &'l Language, problem: &'p Problem) -> Option<Solution<'l, 'p>> {
        let mut source = problem.directory().join(problem.id());
        source.set_extension(language.extension());
        let source = source;

        let pid = problem.id();
        let name = language.name();

        if source.exists() {
            info!("{}: Found {} source file ", pid, name);

            match File::open(&source).read_to_string() {
                Err(_) => {
                    info!("{}: Couldn't read {} source file", pid, name);

                    None
                },
                Ok(string) => {
                    let hash_dir = Path::new(".hashes");
                    fs::mkdir_recursive(&hash_dir, UserDir).ok().
                        expect("Couldn't create .hashes directory");

                    let string = match language.compiler() {
                        Some(compiler) => string.append(compiler.get_version().as_slice()),
                        None => string,
                    };

                    let string = match language.interpreter() {
                        Some(interpreter) => string.append(interpreter.get_version().as_slice()),
                        None => string,
                    };

                    let hash_file = hash_dir.join(pid).with_extension(name);
                    let hash = hash::hash(&string).to_string();

                    if hash_file.exists() {
                        let old_hash = File::open(&hash_file).read_to_string().ok().
                            expect("Couldn't open hash file");

                        if old_hash == hash {
                            info!("{}: {} source hasn't changed, skipping", pid, name);
                            return None;
                        } else {
                            info!("{}: {} source has changed, benchmarking", pid, name);
                        }
                    }

                    File::create(&hash_file).write_str(hash.as_slice()).ok().
                        expect("Couldn't write hash file");

                    Some(Solution {
                        language: language,
                        problem: problem,
                        source: source,
                    })
                }
            }

        } else {
            None
        }
    }

    pub fn language(&self) -> &'l Language {
        self.language
    }

    pub fn problem(&self) -> &'p Problem {
        self.problem
    }

    pub fn source<'a>(&'a self) -> &'a Path {
        &self.source
    }
}
