#![feature(test)]

extern crate test;

fn solution() -> u32 {
    let (mut sum, mut curr, mut next) = (0, 1, 2);

    while curr < 4000000 {
        if curr % 2 == 0 {
            sum += curr
        }

        let temp = next;
        next += curr;
        curr = temp;
    }

    // Tell the compiler to not optimize away this whole routine
    test::black_box(&mut sum);

    sum
}

fn main() {
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
