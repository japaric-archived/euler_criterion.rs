#![feature(core)]
#![feature(test)]

extern crate num;

fn solution() -> u32 {
    use std::ops::Mul;

    use num::One;
    use num::bigint::{BigUint, ToBigUint};

    (2..100)
        .map(|x| x.to_biguint().unwrap())
        .fold(BigUint::one(), Mul::mul)
        .to_string()
        .chars()
        .filter_map(|c| c.to_digit(10))
        .sum()
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
// num = "*"
// time = "*"
