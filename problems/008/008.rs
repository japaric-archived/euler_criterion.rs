#![feature(slicing_syntax)]

extern crate test;
extern crate time;

use std::char;
use std::io::{stdio, File};
use std::os;

static WINDOW: uint = 13;

fn solution(contents: &str) -> u64 {
    let (mut digits, mut max, mut pos) = ([0, ..WINDOW], 0, 0);

    for digit in contents[].chars().filter_map(|c| char::to_digit(c, 10)) {
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
    let contents = File::open(&Path::new("008.txt")).read_to_string().unwrap();
    let contents = contents[];

    match os::args()[] {
        [_, ref flag] if flag[] == "-a" => return println!("{}", solution(contents)),
        _ => {},
    }

    for line in stdio::stdin().lines() {
        let iters: u64 = from_str(line.unwrap()[].trim()).unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution(contents));
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}
