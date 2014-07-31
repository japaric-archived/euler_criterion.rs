extern crate test;
extern crate time;

use std::char;
use std::io::{stdio,File};
use std::os;

static WINDOW: uint = 13;

fn solution(contents: &str) -> uint {
    let (mut digits, mut max, mut pos) = ([0u8, ..WINDOW], 0, 0);

    for digit in contents.as_slice().chars().filter_map(|c| char::to_digit(c, 10)) {
        digits[pos] = digit as u8;

        let prod = digits.iter().fold(1, |p, &f| p * f as uint);

        if prod > max {
            max = prod;
        }

        pos = (pos + 1) % WINDOW;
    }

    max
}

fn main() {
    let contents = File::open(&Path::new("008.txt")).read_to_string().unwrap();
    let contents = contents.as_slice();

    match os::args().as_slice() {
        [_, ref flag] if flag.as_slice() == "-a" => return println!("{}", solution(contents)),
        _ => {},
    }

    for line in stdio::stdin().lines() {
        let iters: u64 = from_str(line.unwrap().as_slice().trim()).unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution(contents));
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}