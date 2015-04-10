//! Context

use std::fs::PathExt;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

/// Errors that may arise when creating a new context
#[derive(Debug)]
pub enum Error {
    /// IO error
    Io(io::Error),
    /// `language` directory not found
    LanguageDirNotFound,
    /// `problem` directory not found
    ProblemDirNotFound,
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

/// A context
pub struct Context {
    criterion_dir: PathBuf,
    current_dir: PathBuf,
    hash_dir: PathBuf,
    language_dir: PathBuf,
    output_dir: PathBuf,
    plot_dir: PathBuf,
    problem_dir: PathBuf,
    temp_dir: PathBuf,
    version_dir: PathBuf,
}

impl Context {
    /// Creates a new context around the current directory
    pub fn new() -> Result<Context, Error> {
        const CRITERION_DIR: &'static str = ".criterion";
        const HASH_DIR: &'static str = "hash";
        const LANGUAGE_DIR: &'static str = "languages";
        const OUTPUT_DIR: &'static str = "output";
        const PLOT_DIR: &'static str = "plots";
        const PROBLEM_DIR: &'static str = "problems";
        const TEMP_DIR: &'static str = "tmp";
        const VERSION_DIR: &'static str = "version";

        let current_dir = try!(env::current_dir());

        let language_dir= current_dir.join(LANGUAGE_DIR);

        if !language_dir.is_dir() {
            return Err(Error::LanguageDirNotFound)
        }

        let problem_dir = current_dir.join(PROBLEM_DIR);

        if !problem_dir.is_dir() {
            return Err(Error::ProblemDirNotFound)
        }

        let output_dir = current_dir.join(OUTPUT_DIR);
        let criterion_dir = output_dir.join(CRITERION_DIR);
        let hash_dir = output_dir.join(HASH_DIR);
        let plot_dir = output_dir.join(PLOT_DIR);
        let temp_dir = output_dir.join(TEMP_DIR);
        let version_dir = output_dir.join(VERSION_DIR);

        try!(fs::create_dir_all(&hash_dir));
        try!(fs::create_dir_all(&plot_dir));
        try!(fs::create_dir_all(&temp_dir));
        try!(fs::create_dir_all(&version_dir));

        Ok(Context {
            criterion_dir: criterion_dir,
            current_dir: current_dir,
            hash_dir: hash_dir,
            language_dir: language_dir,
            output_dir: output_dir,
            plot_dir: plot_dir,
            problem_dir: problem_dir,
            temp_dir: temp_dir,
            version_dir: version_dir,
        })
    }

    /// Returns the absolute path to the `.criterion` directory
    pub fn criterion_dir(&self) -> &Path {
        &self.criterion_dir
    }

    /// Returns the absolute path to the current directory
    pub fn current_dir(&self) -> &Path {
        &self.current_dir
    }

    /// Returns the absolute path to the `hash` directory
    pub fn hash_dir(&self) -> &Path {
        &self.hash_dir
    }

    /// Returns the absolute path to the `language` directory
    pub fn language_dir(&self) -> &Path {
        &self.language_dir
    }

    /// Returns the absolute path to the `output` directory
    pub fn output_dir(&self) -> &Path {
        &self.output_dir
    }

    /// Returns the absolute path to the `plot` directory
    pub fn plot_dir(&self) -> &Path {
        &self.plot_dir
    }

    /// Returns the absolute path to the `problem` directory
    pub fn problem_dir(&self) -> &Path {
        &self.problem_dir
    }

    /// Returns the absolute path to the temporary directory
    pub fn temp_dir(&self) -> &Path {
        &self.temp_dir
    }

    /// Returns the absolute path to the `version` directory
    pub fn version_dir(&self) -> &Path {
        &self.version_dir
    }
}
