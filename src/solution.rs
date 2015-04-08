use std::fs::{File, PathExt, self};
use std::hash::{SipHasher, self};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use language::Language;
use problem::Problem;

pub struct Solution<'l, 'p> {
    language: &'l Language,
    problem: &'p Problem,
    source: PathBuf,
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

            let mut string = String::new();
            match File::open(&source).and_then(|mut f| {
                f.read_to_string(&mut string)
            }) {
                Err(_) => {
                    info!("{}: Couldn't read {} source file", pid, name);

                    None
                },
                Ok(_) => {
                    let hash_dir = Path::new(".hashes");
                    fs::create_dir_all(&hash_dir).ok().
                        expect("Couldn't create .hashes directory");

                    let mut string = match language.compiler() {
                        Some(compiler) => {
                            string.push_str(compiler.version());
                            string
                        },
                        None => string,
                    };

                    let string = match language.interpreter() {
                        Some(interpreter) => {
                            string.push_str(interpreter.version());
                            string
                        },
                        None => string,
                    };

                    let hash_file = hash_dir.join(pid).with_extension(name);
                    let hash = hash::hash::<_, SipHasher>(&string).to_string();

                    if hash_file.exists() {
                        let mut old_hash = String::new();
                        File::open(&hash_file).and_then(|mut f| {
                            f.read_to_string(&mut old_hash)
                        }).ok().
                            expect("Couldn't open hash file");

                        if old_hash == hash {
                            info!("{}: {} source hasn't changed, skipping", pid, name);
                            return None;
                        } else {
                            info!("{}: {} source has changed, benchmarking", pid, name);
                        }
                    }

                    File::create(&hash_file).and_then(|mut f| {
                        f.write_all(hash.as_bytes())
                    }).ok().
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

    pub fn language(&self) -> &Language {
        self.language
    }

    pub fn problem(&self) -> &Problem {
        self.problem
    }

    pub fn source(&self) -> &Path {
        &self.source
    }
}
