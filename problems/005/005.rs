#![feature(slicing_syntax)]

extern crate num;
extern crate test;
extern crate time;

use num::integer;
use std::io::stdio;
use std::os;

fn solution() -> u64 {
    range(3, 21).fold(2, |p, f| integer::lcm(p, f))
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
