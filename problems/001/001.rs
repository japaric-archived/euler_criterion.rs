#![feature(core)]
#![feature(test)]

fn solution() -> u32 {
    (0..1_000).filter(|&x| x % 3 == 0 || x % 5 == 0).sum()
}

// XXX "Imperative" style, roughly the same speed
//fn solution() -> u32 {
    //let mut sum = 0;

    //for x in (0..1_000) {
        //if x % 3 == 0 || x % 5 == 0 {
            //sum += x;
        //}
    //}

    //sum
//}

fn main() {
    extern crate test;
    extern crate time;

    use std::env;
    use std::ffi::OsStr;
    use std::io::{BufRead, self};

    if let Some(arg) = env::args_os().skip(1).next() {
        if arg.as_os_str() == OsStr::new("-a") {
            return println!("{}", solution())
        }
    }

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let iters: u64 = line.unwrap().trim().parse().unwrap();

        let start = time::precise_time_ns();
        for _ in (0..iters) {
            test::black_box(solution());
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}

// Cargo.toml
//
// [dependencies]
// time = "*"
