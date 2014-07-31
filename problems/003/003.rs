extern crate test;
extern crate time;

use std::io::stdio;
use std::iter;
use std::os;

fn solution() -> uint {
    let mut n = 600_851_475_143;

    for factor in iter::count(3, 2) {
        while n % factor == 0 {
            n /= factor;
        }

        if factor * factor > n {
            return n;
        } else if n == 1 {
            return factor;
        }
    }

    unreachable!();
}

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