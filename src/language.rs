//! Programming languages

use std::ffi::OsStr;
use std::fs::{File, ReadDir, self};
use std::io::{Read, Write, self};
use std::path::Path;

use rustc_serialize::json;

use compiler::Compiler;
use context::Context;
use interpreter::Interpreter;

/// A programming languages
#[derive(Debug, RustcDecodable)]
pub struct Language {
    compiler: Option<Compiler>,
    extension: String,
    interpreter: Option<Interpreter>,
    name: String,
}

impl Language {
    /// Returns the language compiler, if any
    pub fn compiler(&self) -> Option<&Compiler> {
        self.compiler.as_ref()
    }

    /// Returns the extension used by this language source files
    pub fn extension(&self) -> &str {
        &self.extension
    }

    /// Returns the language interpreter, if any
    pub fn interpreter(&self) -> Option<&Interpreter> {
        self.interpreter.as_ref()
    }

    /// Returns the name of the language
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// An iterator over the `language` directory
pub struct Languages<'a> {
    iter: ReadDir,
    version_dir: &'a Path,
}

impl<'a> Languages<'a> {
    /// Retrieves all the programming languages specified in the `language` directory
    pub fn all(ctxt: &'a Context) -> io::Result<Languages<'a>> {
        fs::read_dir(ctxt.language_dir()).map(|iter| {
            Languages {
                iter: iter,
                version_dir: ctxt.version_dir(),
            }
        })
    }
}

impl<'a> Iterator for Languages<'a> {
    type Item = Language;

    fn next(&mut self) -> Option<Language> {
        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Languages::next: ", $template), $($args),*)
            }
        }

        for entry in &mut self.iter {
            let entry = match entry {
                Err(e) => {
                    _debug!("skipping entry ({})", e);
                    continue
                },
                Ok(entry) => entry,
            };

            let ref path = entry.path();

            if path.extension() != Some(OsStr::new("json")) {
                _debug!("{:?} doesn't have json extension, skipping", path);
                continue
            }

            let mut string = String::new();
            if let Err(e) = (|| {
                try!(File::open(path)).read_to_string(&mut string)
            })() {
                _debug!("error reading {:?} ({})", path, e);
                continue
            }

            let language: Language = match json::decode(&string) {
                Err(e) => {
                    _debug!("error decoding {:?} ({})", path, e);
                    continue
                },
                Ok(language) => language,
            };

            _debug!("found {}", language.name);

            if language.compiler.is_none() && language.interpreter.is_none() {
                _debug!("error {} doesn't have a compiler or interpreter", language.name);
                continue
            }

            if let Some(ref compiler) = language.compiler {
                use command::Error::*;

                match compiler.version() {
                    Err(Io(e)) => {
                        _debug!("couldn't get {} compiler version ({})", language.name, e);
                        continue
                    },
                    Err(Failed(code, _)) => {
                        _debug!("couldn't get {} compiler version ({})", language.name, code);
                        continue
                    },
                    Ok(ref version) => {
                        let ref version_file = self.version_dir.join(compiler.command());

                        if let Err(e) = (|| {
                            try!(File::create(version_file)).write_all(version)
                        })() {
                            _debug!("couldn't write {:?} ({})", version_file, e);
                            continue
                        }
                    },
                }
            }

            if let Some(ref interpreter) = language.interpreter {
                use command::Error::*;

                match interpreter.version() {
                    Err(Io(e)) => {
                        _debug!("couldn't get {} interpreter version ({})", language.name, e);
                        continue
                    },
                    Err(Failed(code, _)) => {
                        _debug!("couldn't get {} interpreter version ({})", language.name, code);
                        continue
                    },
                    Ok(ref version) => {
                        let ref version_file = self.version_dir.join(interpreter.command());

                        if let Err(e) = (|| {
                            try!(File::create(version_file)).write_all(version)
                        })() {
                            _debug!("couldn't write {:?} ({})", version_file, e);
                            continue
                        }
                    },
                }
            }

            _debug!("{:?}", language);

            return Some(language)
        }

        None
    }
}
