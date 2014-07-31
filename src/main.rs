#![feature(phase)]

#[phase(plugin, link)] extern crate log;
extern crate criterion;
extern crate serialize;

use criterion::Criterion;
use std::io::{UserDir,fs};

use executable::Executable;
use problem::Problem;
use solution::Solution;

mod compiler;
mod executable;
mod interpreter;
mod language;
mod problem;
mod solution;

fn main() {
    let languages = language::all();

    for problem in fs::readdir(&Path::new("problems")).unwrap().move_iter().filter_map(|dir| {
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
