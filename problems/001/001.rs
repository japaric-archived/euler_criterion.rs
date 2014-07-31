extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

fn solution() -> uint {
    let mut sum = 0;

    for x in range(0, 1_000) {
        if x % 3 == 0 || x % 5 == 0 {
            sum += x;
        }
    }

    sum
}

// XXX Shorter, but 10% slower
//fn solution() -> uint {
    //use std::iter::AdditiveIterator;

    //range(0, 1_000u).filter(|x| x % 3 == 0 || x % 5 == 0).sum()
//}

fn main() {
    match os::args().as_slice() {
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
