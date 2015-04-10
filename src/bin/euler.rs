#![feature(path_ext)]

extern crate criterion;
extern crate env_logger;
extern crate euler;

use std::env;
use std::fs::{PathExt, self};

use criterion::Criterion;

use euler::command::Error;
use euler::context::Context;
use euler::language::Languages;
use euler::problem::Problems;
use euler::profile;
use euler::solution::Solutions;

fn main() {
    env_logger::init().unwrap();

    let ref ctxt = Context::new().unwrap();
    profile::cpu(ctxt).unwrap();

    let languages: Vec<_> = Languages::all(ctxt).unwrap().collect();

    for ref prob in Problems::all(ctxt).unwrap() {
        for mut sol in Solutions::new(ctxt, prob, &languages) {
            if let Err(e) = (|| -> Result<(), Error> {
                if try!(sol.has_changed()) && try!(sol.verify()) {
                    try!(sol.bench(ctxt));
                    try!(sol.update_checksum());
                }

                Ok(())
            })() {
                println!("{}", e);
            }
        }

        if let Err(e) = (|| -> Result<(), Error> {
            if ctxt.criterion_dir().join(prob.id()).is_dir() {
                try!(env::set_current_dir(ctxt.output_dir()));
                Criterion::default().summarize(prob.id());
                try!(env::set_current_dir(ctxt.current_dir()));
            }

            let src = ctxt.criterion_dir().join(prob.id()).join("summary/new/violin_plot.svg");

            if src.exists() {
                let mut dst = ctxt.plot_dir().join(prob.id());
                dst.set_extension("svg");

                try!(fs::copy(src, dst));
            }

            Ok(())
        })() {
            println!("{}", e);
        }
    }
}
