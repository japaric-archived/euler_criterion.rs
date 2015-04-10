#![feature(test)]

fn solution() -> u32 {
    fn is_palindrome(n: u32) -> bool {
        let (mut reversed, mut temp) = (0, n);

        while temp != 0 {
            reversed = 10 * reversed + temp % 10;
            temp /= 10;
        }

        reversed == n
    }

    let mut max = 0;

    for a in 100..1_000 {
        for b in 100..a {
            let p = a * b;

            if p > max && is_palindrome(p) {
                max = p;
            }
        }
    }

    max
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
