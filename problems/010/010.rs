#![feature(collections)]
#![feature(step_by)]
#![feature(test)]

extern crate cast;

use cast::From;

fn solution() -> u64 {
    use std::collections::BitVec;

    const LIMIT: usize = 2_000_000;
    const SIZE: usize = (LIMIT - 1) / 2;

    let mut sieve = BitVec::from_elem(SIZE, false);
    let mut sum = 2;

    for i in 0..SIZE {
        if !sieve[i] {
            let p = 2 * u64::from(i) + 3;

            sum += p;

            for j in (p * p..u64::from(LIMIT)).step_by(2 * p) {
                let j = (j - 3) / 2;

                sieve.set(usize::from(j), true);
            }
        }
    }

    sum
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
// time = "*"
//
// [dependencies.cast]
// git = "https://github.com/japaric/cast.rs"
