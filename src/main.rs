#![feature(phase)]

#[phase(plugin, link)] extern crate log;
extern crate criterion;
extern crate serialize;

use criterion::Criterion;
use std::io::{Command, File, UserDir, fs};
use std::str;

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
        Err(_) => fail!("Couldn't spawn `lscpu`"),
        Ok(po) => match str::from_utf8(po.output[]) {
            None => fail!("Couldn't parse the output of `lscpu`"),
            Some(output) => {
                let s = output.lines().filter(|line| {
                    !line.starts_with("CPU MHz:")
                }).collect::<Vec<_>>().connect("\n");

                match File::create(&Path::new("lscpu")).write_str(s.as_slice()) {
                    Err(_) => fail!("Couln't write to the cpu file"),
                    Ok(_) => {},
                }
            }
        },
    }
}

fn main() {
    let languages = language::all();

    lscpu();

    for problem in fs::readdir(&Path::new("problems")).unwrap().into_iter().filter_map(|dir| {
        Problem::new(dir)
    }) {
        let mut processed = 0u;

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
            let summary_plot = Path::new(".criterion").join(pid).join("summary/new/medians.svg");
            let plots_dir = Path::new("plots");

            fs::mkdir_recursive(&plots_dir, UserDir).ok().
                expect("Couldn't create the plots directory");
            fs::copy(&summary_plot, &plots_dir.join(pid).with_extension("svg")).ok().
                expect("Couldn't copy summary plot to the plots directory");
        }
    }
}
