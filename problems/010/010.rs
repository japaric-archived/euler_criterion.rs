extern crate test;
extern crate time;

use std::collections::Bitv;
use std::io::stdio;
use std::iter;
use std::os;

static LIMIT: uint = 2_000_000;
static SIZE: uint = (LIMIT - 1) / 2;

fn solution() -> uint {
    let mut sieve = Bitv::with_capacity(SIZE, false);
    let mut sum = 2;

    for i in range(0, SIZE) {
        if !sieve.get(i) {
            let p = 2 * i + 3;

            sum += p;

            for j in iter::range_step(p * p, LIMIT, 2 * p) {
                let j = (j - 3) / 2;

                sieve.set(j, true);
            }
        }
    }

    sum
}

fn main() {
    match os::args()[] {
        [_, ref flag] if flag.as_slice() == "-a" => return println!("{}", solution()),
        _ => {},
    }

    for line in stdio::stdin().lines() {
        let iters: u64 = from_str(line.unwrap().as_slice().trim()).unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution());
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}
