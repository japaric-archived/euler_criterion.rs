#![feature(test)]

extern crate num;

fn solution(input: &str) -> String {
    use std::ops::Add;

    use num::{BigUint, Zero};

    input.
        lines().
        filter_map(|line| line.trim().parse::<BigUint>().ok()).
        fold(BigUint::zero(), Add::add).
        to_string()[0..10].
        to_string()
}

fn main() {
    extern crate test;
    extern crate time;

    use std::env;
    use std::ffi::OsStr;
    use std::fs::File;
    use std::io::{BufRead, Read, self};

    let mut input = String::new();
    File::open("013.txt").unwrap().read_to_string(&mut input).unwrap();

    if let Some(arg) = env::args_os().skip(1).next() {
        if arg.as_os_str() == OsStr::new("-a") {
            return println!("{}", solution(&input))
        }
    }

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let iters: u64 = line.unwrap().trim().parse().unwrap();

        let start = time::precise_time_ns();
        for _ in (0..iters) {
            test::black_box(solution(&input));
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}

// Cargo.toml
//
// [dependencies]
// num = "*"
// time = "*"
