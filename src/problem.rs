use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

pub struct Problem {
    answer: String,
    directory: PathBuf,
    id: String,
}

impl Problem {
    pub fn new(directory: PathBuf) -> Option<Problem> {
        let id = directory.file_name().unwrap().to_string_lossy().into_owned();
        let answer_file = {
            let mut path = directory.clone();
            path.push(&id);
            path.set_extension("ans");
            path
        };

        let mut answer = String::new();
        match File::open(&answer_file).and_then(|mut f| {
            f.read_to_string(&mut answer)
        }) {
            Err(_) => None,
            Ok(_) => {
                info!("{}: Found answer file", id);

                Some(Problem {
                    answer: answer,
                    directory: directory,
                    id: id,
                })
            },
        }
    }

    pub fn answer(&self) -> &str {
        self.answer.as_slice()
    }

    pub fn directory(&self) -> &Path {
        &self.directory
    }

    pub fn id(&self) -> &str {
        self.id.as_slice()
    }
}
