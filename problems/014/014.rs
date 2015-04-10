#![feature(core)]
#![feature(test)]

extern crate cast;

use cast::From;

fn solution() -> u64 {
    use std::iter;

    fn collatz_length(n: u64, lengths: &mut [u64]) -> u64 {
        match lengths.get(usize::from(n)) {
            Some(&length) if length != 0 => length,
            _ => {
                let length = 1 + collatz_length(if n % 2 == 0 {
                    n / 2
                } else {
                    3 * n + 1
                }, lengths);

                if let Some(n) = lengths.get_mut(usize::from(n)) {
                    *n = length;
                }

                length
            },
        }
    }

    const LIMIT: usize = 1_000_000;

    let ref mut lengths = iter::repeat(0).take(LIMIT).collect::<Vec<_>>();
    lengths[1] = 1;

    (2..u64::from(LIMIT + 1)).max_by(|&n| collatz_length(n, lengths)).unwrap()
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
// [dependencies.cast]
// git = "https://github.com/japaric/cast.rs"
//
// [dependencies]
// time = "*"
