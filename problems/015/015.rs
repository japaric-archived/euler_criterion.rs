#![feature(slicing_syntax)]

extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

const SIZE: uint = 20;
const STRIDE: uint = SIZE + 1;

fn solution() -> u64 {
    let mut grid = [1; STRIDE * STRIDE];

    for i in range(1, STRIDE) {
        for j in range(1, STRIDE) {
            grid[i * STRIDE + j] = grid[(i - 1) * STRIDE + j] + grid[i * STRIDE + j - 1];
        }
    }

    grid[SIZE * STRIDE + SIZE]
}

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
