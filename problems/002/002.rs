extern crate test;
extern crate time;

use std::io::stdio;
use std::os;

fn solution() -> uint {
    let (mut sum, mut curr, mut next) = (0, 1, 2);

    while curr < 4000000 {
        if curr % 2 == 0 {
            sum += curr
        }

        let temp = next;
        next += curr;
        curr = temp;
    }

    // XXX Compiler don't be an smart ass! Don't optimize me away!
    test::black_box(&mut sum);

    sum
}

// XXX Fancier, but 40% slower
//struct Fibonacci {
    //curr: uint,
    //next: uint,
//}

//fn fibonacci() -> Fibonacci {
    //Fibonacci { curr: 1, next: 2 }
//}

//impl Iterator<uint> for Fibonacci {
    //fn next(&mut self) -> Option<uint> {
        //use std::mem;

        //let Fibonacci { curr: curr, next: next } = *self;

        //Some(mem::replace(&mut self.curr, mem::replace(&mut self.next, curr + next)))
    //}
//}

//fn solution() -> uint {
    //use std::iter::AdditiveIterator;

    //fibonacci().
        //filter(|x| x % 2 == 0).
        //take_while(|&x| x < 4_000_000).
        //sum()
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
