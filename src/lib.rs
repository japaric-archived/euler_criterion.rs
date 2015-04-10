//! Benchmark solutions to Project Euler problems written in different languages

#![deny(missing_docs)]
#![deny(warnings)]

#![feature(core)]
#![feature(hash)]
#![feature(path_ext)]

#[macro_use] extern crate debug_unreachable;
#[macro_use] extern crate log;

extern crate criterion;
extern crate rustc_serialize;
extern crate tempdir;

pub mod command;
pub mod compiler;
pub mod context;
pub mod interpreter;
pub mod language;
pub mod problem;
pub mod profile;
pub mod solution;
