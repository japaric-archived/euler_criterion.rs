#![feature(test)]

fn solution(input: &str) -> u64 {
    const WINDOW: usize = 13;

    let (mut digits, mut max, mut pos) = ([0; WINDOW], 0, 0);

    for digit in input.chars().filter_map(|c| char::to_digit(c, 10)) {
        digits[pos] = digit as u64;

        let product = digits.iter().fold(1, |product, &digit| product * digit);

        if product > max {
            max = product;
        }

        pos = (pos + 1) % WINDOW;
    }

    max
}

fn main() {
    extern crate test;
    extern crate time;

    use std::env;
    use std::ffi::OsStr;
    use std::fs::File;
    use std::io::{BufRead, Read, self};
    use std::path::Path;

    let mut input = String::new();
    File::open(Path::new("008.txt")).unwrap().read_to_string(&mut input).unwrap();

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
// time = "*"
