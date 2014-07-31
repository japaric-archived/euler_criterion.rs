extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

fn is_palindrome(n: uint) -> bool {
    let (mut reversed, mut temp) = (0, n);

    while temp != 0 {
        reversed = 10 * reversed + temp % 10;
        temp /= 10;
    }

    reversed == n
}

fn solution() -> uint {
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
