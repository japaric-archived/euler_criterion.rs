use serialize::json;
use std::io::{fs, File, UserDir};

use compiler::Compiler;
use interpreter::Interpreter;

#[deriving(Decodable)]
pub struct Language {
    compiler: Option<Compiler>,
    extension: String,
    interpreter: Option<Interpreter>,
    name: String,
}

impl Language {
    pub fn compiler<'a>(&'a self) -> Option<&'a Compiler> {
        self.compiler.as_ref()
    }

    pub fn extension<'a>(&'a self) -> &'a str {
        self.extension.as_slice()
    }

    pub fn interpreter<'a>(&'a self) -> Option<&'a Interpreter> {
        self.interpreter.as_ref()
    }

    pub fn name<'a>(&'a self) -> &'a str {
        self.name.as_slice()
    }
}

pub fn all() -> Vec<Language> {
    let version_dir = Path::new("versions");
    fs::mkdir_recursive(&version_dir, UserDir).ok().
        expect("Couldn't create the versions directory");

    fs::readdir(&Path::new("languages")).
        ok().
        expect("languages directory not found").
        iter().
        filter(|file| file.is_file()).
        map(|file| {
            let file_ = file.display();

            match File::open(file).read_to_string() {
                Err(e) => fail!("`{}`: {}", file_, e),
                Ok(string) => match json::decode::<Language>(string.as_slice()) {
                    Err(e) => fail!("`{}`: {}", file_, e),
                    Ok(mut language) => {
                        info!("Found {}", language.name);

                        match language.compiler {
                            Some(ref mut compiler) => {
                                compiler.fetch_version();

                                File::create(&version_dir.join(compiler.command())).
                                    write_str(compiler.version()).
                                    ok().
                                    expect("Couldn't write to versions directory");
                            },
                            None => {},
                        }

                        match language.interpreter {
                            Some(ref mut interpreter) => {
                                interpreter.fetch_version();

                                File::create(&version_dir.join(interpreter.command())).
                                    write_str(interpreter.version()).
                                    ok().
                                    expect("Couldn't write to versions directory");
                            },
                            None => {},
                        }

                        if language.compiler.is_none() && language.interpreter.is_none() {
                            fail!("{}: No compiler and no interpreter found", language.name)
                        }

                        language
                    },
                }
            }
        }).
        collect()
}
