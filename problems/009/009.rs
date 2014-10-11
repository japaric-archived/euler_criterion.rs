#![feature(slicing_syntax)]

extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

const PERIMETER: u32 = 1_000;

fn solution() -> u32 {
    for c in range(PERIMETER / 3 + 1, PERIMETER / 2) {
        for b in range((PERIMETER - c) / 2 + 1, c) {
            let a = PERIMETER - b - c;

            if a * a + b * b == c * c {
                return a * b * c
            }
        }
    }

    unreachable!();
}

fn main() {
    match os::args()[] {
        [_, ref flag] if flag[] == "-a" => return println!("{}", solution()),
        _ => {},
    }

    for line in stdio::stdin().lines() {
        let iters: u64 = from_str(line.unwrap()[].trim()).unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution());
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}
