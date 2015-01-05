#![feature(slicing_syntax)]

extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

fn solution() -> u32 {
    use std::iter::AdditiveIterator;

    range(0u32, 1_000).filter(|&x| x % 3 == 0 || x % 5 == 0).sum()
}

// XXX "Imperative" style, roughly the same speed
//fn solution() -> u32 {
    //let mut sum = 0;

    //for x in range(0, 1_000) {
        //if x % 3 == 0 || x % 5 == 0 {
            //sum += x;
        //}
    //}

    //sum
//}

fn main() {
    match os::args()[] {
        [_, ref flag] if flag[] == "-a" => return println!("{}", solution()),
        _ => {},
    }

    for line in stdio::stdin().lock().lines() {
        let iters: u64 = line.unwrap()[].trim().parse().unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution());
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}
