use std::fs::{File, PathExt, self};
use std::io::{Read, Write};
use std::path::Path;

use rustc_serialize::json;

use compiler::Compiler;
use interpreter::Interpreter;

#[derive(RustcDecodable)]
pub struct Language {
    compiler: Option<Compiler>,
    extension: String,
    interpreter: Option<Interpreter>,
    name: String,
}

impl Language {
    pub fn compiler(&self) -> Option<&Compiler> {
        self.compiler.as_ref()
    }

    pub fn extension(&self) -> &str {
        self.extension.as_slice()
    }

    pub fn interpreter(&self) -> Option<&Interpreter> {
        self.interpreter.as_ref()
    }

    pub fn name(&self) -> &str {
        self.name.as_slice()
    }
}

pub fn all() -> Vec<Language> {
    let version_dir = Path::new("versions");
    fs::create_dir_all(version_dir).ok().
        expect("Couldn't create the versions directory");

    fs::read_dir(&Path::new("languages")).
        ok().
        expect("languages directory not found").
        map(|entry| entry.unwrap().path()).
        filter(|file| file.is_file()).
        map(|file| {
            let file_ = file.display();

            let mut string = String::new();
            match File::open(&file).and_then(|mut f| f.read_to_string(&mut string)) {
                Err(e) => panic!("`{}`: {}", file_, e),
                Ok(_) => match json::decode::<Language>(string.as_slice()) {
                    Err(e) => panic!("`{}`: {}", file_, e),
                    Ok(mut language) => {
                        info!("Found {}", language.name);

                        match language.compiler {
                            Some(ref mut compiler) => {
                                compiler.fetch_version();

                                File::create(&version_dir.join(compiler.command())).
                                    and_then(|mut f| {
                                        f.write_all(compiler.version().as_bytes())
                                    }).
                                    ok().
                                    expect("Couldn't write to versions directory");
                            },
                            None => {},
                        }

                        match language.interpreter {
                            Some(ref mut interpreter) => {
                                interpreter.fetch_version();

                                File::create(&version_dir.join(interpreter.command())).
                                    and_then(|mut f| {
                                        f.write_all(interpreter.version().as_bytes())
                                    }).
                                    ok().
                                    expect("Couldn't write to versions directory");
                            },
                            None => {},
                        }

                        if language.compiler.is_none() && language.interpreter.is_none() {
                            panic!("{}: No compiler and no interpreter found", language.name)
                        }

                        language
                    },
                }
            }
        }).
        collect()
}
