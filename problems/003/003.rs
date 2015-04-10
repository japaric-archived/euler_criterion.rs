#![feature(core)]
#![feature(step_by)]
#![feature(test)]

fn solution() -> u64 {
    let mut n = 600_851_475_143;

    for factor in (3..).step_by(2) {
        while n % factor == 0 {
            n /= factor;
        }

        if factor * factor > n {
            return n;
        } else if n == 1 {
            return factor;
        }
    }

    unreachable!();
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
