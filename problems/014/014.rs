extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

static LIMIT: uint = 1_000_000;

fn solution() -> uint {
    let mut lengths = Vec::from_elem(LIMIT + 1, 0);
    let lengths = lengths.as_mut_slice();
    lengths[1] = 1;

    range(2, LIMIT + 1).max_by(|&n| collatz_length(n, lengths)).unwrap()
}

fn collatz_length(n: uint, lengths: &mut [uint]) -> uint {
    match lengths.get(n) {
        Some(&length) if length != 0 => length,
        _ => {
            let length = 1 + collatz_length(if n % 2 == 0 { n / 2 } else { 3 * n + 1 }, lengths);

            if n < lengths.len() {
                lengths[n] = length;
            }

            length
        },
    }
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
