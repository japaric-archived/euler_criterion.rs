#![feature(slicing_syntax)]

extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

fn is_palindrome(n: u32) -> bool {
    let (mut reversed, mut temp) = (0, n);

    while temp != 0 {
        reversed = 10 * reversed + temp % 10;
        temp /= 10;
    }

    reversed == n
}

fn solution() -> u32 {
    let mut max = 0;

    for a in range(100, 1_000) {
        for b in range(100, a) {
            let p = a * b;

            if p > max && is_palindrome(p) {
                max = p;
            }
        }
    }

    max
}

fn main() {
    match os::args()[] {
        [_, ref flag] if flag[] == "-a" => return println!("{}", solution()),
        _ => {},
    }

    for line in stdio::stdin().lock().lines() {
        let iters: u64 = from_str(line.unwrap()[].trim()).unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution());
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}
