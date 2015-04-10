//! Solutions

use std::{env, str};
use std::fs::{File, PathExt};
use std::hash::{SipHasher, self};
use std::io::{Read, Write, self};
use std::path::PathBuf;
use std::process::Command;
use std::slice::Iter;

use criterion::Criterion;

use command::{Error, Run};
use compiler::Output;
use context::Context;
use language::Language;
use problem::Problem;

/// A solution
pub struct Solution<'a> {
    ctxt: &'a Context,
    language: &'a Language,
    output: Option<Output>,
    problem: &'a Problem,
    source: PathBuf,
}

impl<'a> Solution<'a> {
    /// Benchmarks this solution
    pub fn bench(&mut self, ctxt: &Context) -> Result<(), Error> {
        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Solution::bench: ", $template), $($args),*)
            }
        }

        try!(self.compile());

        let pid = self.problem.id();
        let lang = self.language.name();
        let id = format!("{}/{}", pid, lang);
        let ref cwd = try!(env::current_dir());

        try!(env::set_current_dir(ctxt.output_dir()));
        Criterion::default().bench_program(&id, self.cmd());
        try!(env::set_current_dir(cwd));
        Ok(())
    }

    /// Checks if the solution has changed
    pub fn has_changed(&self) -> io::Result<bool> {
        let ref source = self.source;
        let hash_dir = self.ctxt.hash_dir();
        let mut hash_file = hash_dir.join(self.problem.id());
        hash_file.set_extension(self.language.name());

        if !hash_file.exists() {
            debug!("Solution::has_changed: no previous hash file for {:?}", source);

            return Ok(true)
        }

        let mut old_hash = String::with_capacity(20);
        try!(try!(File::open(hash_file)).read_to_string(&mut old_hash));

        let new_hash = hash::hash::<_, SipHasher>(&try!(self.source_code())).to_string();

        debug!("Solution::has_changed: old hash: {}, new hash: {}", old_hash, new_hash);

        Ok(new_hash != old_hash)
    }

    /// Updates the checksum of the solution
    pub fn update_checksum(&self) -> io::Result<()> {
        let hash_dir = self.ctxt.hash_dir();
        let mut hash_file = hash_dir.join(self.problem.id());
        hash_file.set_extension(self.language.name());

        let hash = hash::hash::<_, SipHasher>(&try!(self.source_code()));

        debug!("Solution::update_checksum: {:?} checksum is {}", self.source, hash);

        try!(try!(File::create(hash_file)).write_all(hash.to_string().as_bytes()));

        Ok(())
    }

    /// Verify that the solution's answer matches the expected answer
    pub fn verify(&mut self) -> Result<bool, Error> {
        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Solution::verify: ", $template), $($args),*)
            }
        }

        try!(self.compile());

        let mut cmd = self.cmd();
        cmd.arg("-a");
        _debug!("running `{:?}`", cmd);
        cmd.run(false).map(|output| {
            let answer = self.problem.answer();


            if output == answer.as_bytes() {
                _debug!("OK",);
                true
            } else {
                _debug!("got {:?}, expected {}", str::from_utf8(&output), answer);
                false
            }
        })
    }

    fn cmd(&self) -> Command {
        let mut cmd =
            match (self.output.as_ref().map(|output| output.path()), self.language.interpreter()) {
                (Some(output), Some(interpreter)) => {
                    let mut cmd = Command::new(interpreter.command());
                    cmd.args(interpreter.flags()).arg(output);
                    cmd
                },
                (Some(output), None) => {
                    Command::new(output)
                },
                (None, Some(interpreter)) => {
                    let mut cmd = Command::new(interpreter.command());
                    cmd.args(interpreter.flags()).arg(&self.source);
                    cmd
                },
                (None, None) => unsafe {
                    debug_unreachable!();
                },
            };

        cmd.current_dir(self.problem.directory());
        cmd
    }

    /// Compile the source code, if applicable/necessary
    fn compile(&mut self) -> Result<(), Error> {
        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Solution::compile: ", $template), $($args),*)
            }
        }

        if let Some(compiler) = self.language.compiler() {
            if self.output.is_none() {
                self.output = Some(try!(compiler.compile(&self.source, self.ctxt)));
                _debug!("compiled {:?}", self.output.as_ref().unwrap_or_else(|| unsafe {
                    debug_unreachable!();
                }).path());
            } else {
                _debug!("already compiled",);
            }
        } else {
            _debug!("no compiler",);
        }

        Ok(())
    }

    /// Fetches the source code of the solution
    fn source_code(&self) -> io::Result<String> {
        let mut string = String::new();
        try!(try!(File::open(&self.source)).read_to_string(&mut string));
        Ok(string)
    }
}

/// An iterator over the solutions to a problem
pub struct Solutions<'a> {
    context: &'a Context,
    languages: Iter<'a, Language>,
    problem: &'a Problem,
}

impl<'a> Solutions<'a> {
    /// Returns an iterator over the solutions (written in different languages) to this problem
    pub fn new(
        context: &'a Context,
        problem: &'a Problem,
        languages: &'a [Language],
    ) -> Solutions<'a> {
        Solutions {
            context: context,
            languages: languages.iter(),
            problem: problem,
        }
    }
}

impl<'a> Iterator for Solutions<'a> {
    type Item = Solution<'a>;

    fn next(&mut self) -> Option<Solution<'a>> {
        // Prefixed debug message
        macro_rules! _debug {
            ($template:expr, $($args:expr),*) => {
                debug!(concat!("Solutions::next: ", $template), $($args),*)
            }
        }

        let context = self.context;
        let problem = self.problem;

        for language in &mut self.languages {
            let mut source = problem.directory().join(problem.id());
            source.set_extension(language.extension());

            if !source.exists() {
                _debug!("{:?} not found", source);
                continue
            }

            let solution = Solution {
                ctxt: context,
                language: language,
                output: None,
                problem: problem,
                source: source,
            };

            _debug!("found solution to problem {:?} in {}", problem.id(), language.name());

            return Some(solution)
        }

        None
    }
}
