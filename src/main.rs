//#![deny(warnings)]
#![feature(core)]
#![feature(hash)]
#![feature(path_ext)]

#[macro_use] extern crate log;

extern crate criterion;
extern crate env_logger;
extern crate rustc_serialize;
extern crate tempdir;

use std::fs::{File, self};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str;

use criterion::Criterion;

use executable::Executable;
use problem::Problem;
use solution::Solution;

mod compiler;
mod executable;
mod interpreter;
mod language;
mod problem;
mod solution;

fn lscpu() {
    match Command::new("lscpu").output() {
        Err(_) => panic!("Couldn't spawn `lscpu`"),
        Ok(po) => match str::from_utf8(&po.stdout) {
            Err(_) => panic!("Couldn't parse the output of `lscpu`"),
            Ok(output) => {
                let s = output.lines().filter(|line| {
                    !line.starts_with("CPU MHz:")
                }).collect::<Vec<_>>().connect("\n");

                match File::create(&Path::new("lscpu")).and_then(|mut f| {
                    f.write_all(s.as_bytes())
                }) {
                    Err(_) => panic!("Couln't write to the cpu file"),
                    Ok(_) => {},
                }
            }
        },
    }
}

fn main() {
    env_logger::init().unwrap();

    let languages = language::all();

    lscpu();

    for problem in fs::read_dir(&Path::new("problems")).unwrap().filter_map(|dir| {
        Problem::new(dir.unwrap().path())
    }) {
        let mut processed = 0;

        for solution in languages.iter().filter_map(|language| {
            Solution::new(language, &problem)
        }) {
            match Executable::new(&solution) {
                Some(executable) => if executable.validate() == Some(true) {
                    executable.bench();
                    processed += 1;
                },
                None => {},
            }
        }

        if processed > 0 {
            Criterion::default().summarize(problem.id());

            let pid = problem.id();
            let summary_plot =
                PathBuf::from(format!(".criterion/{}/summary/new/violin_plot.svg", pid));
            let plots_dir = Path::new("plots");

            fs::create_dir_all(&plots_dir).ok().
                expect("Couldn't create the plots directory");
            fs::copy(&summary_plot, &plots_dir.join(pid).with_extension("svg")).ok().
                expect("Couldn't copy summary plot to the plots directory");
        }
    }
}
