use std::io::File;

pub struct Problem {
    answer: String,
    directory: Path,
    id: String,
}

impl Problem {
    pub fn new(directory: Path) -> Option<Problem> {
        let id = directory.filename_str().unwrap().to_string();
        let answer = directory.join(format!("{}.ans", id));

        match File::open(&answer).read_to_string() {
            Err(_) => None,
            Ok(answer) => {
                info!("{}: Found answer file", id);

                Some(Problem {
                    answer: answer,
                    directory: directory,
                    id: id,
                })
            },
        }
    }

    pub fn answer<'a>(&'a self) -> &'a str {
        self.answer.as_slice()
    }

    pub fn directory<'a>(&'a self) -> &'a Path {
        &self.directory
    }

    pub fn id<'a>(&'a self) -> &'a str {
        self.id.as_slice()
    }
}
