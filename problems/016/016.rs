#![feature(slicing_syntax)]

extern crate num;
extern crate test;
extern crate time;

use num::bigint::ToBigUint;
use std::io::stdio;
use std::iter::AdditiveIterator;
use std::{char, os};

fn solution() -> uint {
    std::num::pow(2u.to_biguint().unwrap(), 1000).
        to_string()[].
        chars().
        filter_map(|c| char::to_digit(c, 10)).
        sum()
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
