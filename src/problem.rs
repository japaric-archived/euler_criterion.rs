//! Problems

use std::fs::{File, PathExt, ReadDir, self};
use std::io::{Read, self};
use std::path::{Path, PathBuf};

use context::Context;

/// A problem
#[derive(Debug)]
pub struct Problem {
    answer: String,
    directory: PathBuf,
    id: String,
}

impl Problem {
    /// Returns the answer to the problem
    pub fn answer(&self) -> &str {
        &self.answer
    }

    /// Returns the absolute path to the problem's directory
    pub fn directory(&self) -> &Path {
        &self.directory
    }

    /// Returns the id of the problem
    pub fn id(&self) -> &str {
        &self.id
    }
}

/// An iterator over the `problem` directory
pub struct Problems {
    iter: ReadDir,
}

impl Problems {
    /// Retrieves all the problems inside the `problem` directory
    pub fn all(ctxt: &Context) -> io::Result<Problems> {
        fs::read_dir(ctxt.problem_dir()).map(|iter| {
            Problems {
                iter: iter
            }
        })
    }
}

impl Iterator for Problems {
    type Item = Problem;

    fn next(&mut self) -> Option<Problem> {
        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Problems::next: ", $template), $($args),*)
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

            let path = entry.path();

            if !path.is_dir() {
                _debug!("{:?} is not a directory, skipping", path);
                continue
            }

            let directory = path;

            _debug!("looking into {:?}", directory);

            let id = directory.file_name().unwrap_or_else(|| unsafe {
                debug_unreachable!();
            }).to_string_lossy().into_owned();

            let mut answer_file = directory.join(&id);
            answer_file.set_extension("ans");

            let mut answer = String::new();
            if let Err(e) = (|| {
                try!(File::open(answer_file)).read_to_string(&mut answer)
            })() {
                _debug!("error reading the answer file ({})", e);
                continue
            }

            let problem = Problem {
                answer: answer,
                directory: directory,
                id: id,
            };

            _debug!("{:?}", problem);

            return Some(problem)
        }

        None
    }
}
