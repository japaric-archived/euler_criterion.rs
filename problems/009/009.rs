#![feature(core)]
#![feature(test)]

#[macro_use]
extern crate debug_unreachable;

fn solution() -> u32 {
    const PERIMETER: u32 = 1_000;

    for c in PERIMETER / 3 + 1..PERIMETER / 2 {
        for b in (PERIMETER - c) / 2 + 1..c {
            let a = PERIMETER - b - c;

            if a * a + b * b == c * c {
                return a * b * c
            }
        }
    }

    unsafe {
        debug_unreachable!();
    }
}

fn main() {
    extern crate test;
    extern crate time;

    use std::env;
    use std::ffi::OsStr;
    use std::io::{BufRead, self};

    if let Some(arg) = env::args_os().skip(1).next() {
        if arg.as_os_str() == OsStr::new("-a") {
            return println!("{}", solution())
        }
    }

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let iters: u64 = line.unwrap().trim().parse().unwrap();

        let start = time::precise_time_ns();
        for _ in (0..iters) {
            test::black_box(solution());
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}

// Cargo.toml
//
// [dependencies]
// debug_unreachable = "*"
// time = "*"
