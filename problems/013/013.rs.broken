#![feature(slicing_syntax)]

extern crate num;
extern crate test;
extern crate time;

use num::bigint::BigUint;
use std::io::{File, stdio};
use std::iter::AdditiveIterator;
use std::os;

fn solution(input: &str) -> String {
    input.
        lines().
        filter_map(|line| from_str::<BigUint>(line.trim())).
        sum().
        to_string()[0..10].
        to_string()
}

fn main() {
    let contents = File::open(&Path::new("013.txt")).read_to_string().unwrap();
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
