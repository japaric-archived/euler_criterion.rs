#![feature(core)]
#![feature(test)]

fn solution() -> u32 {
    use std::collections::HashMap;

    struct Primes {
        map: HashMap<u32, u32>,
        n: u32,
    }

    impl Iterator for Primes {
        type Item = u32;

        fn next(&mut self) -> Option<u32> {
            loop {
                self.n += 1;

                let q = self.n;

                match self.map.remove(&q) {
                    None => {
                        self.map.insert(q * q, q);

                        return Some(q);
                    },
                    Some(p) => {
                        let mut x = p + q;

                        while self.map.contains_key(&x) {
                            x += p;
                        }

                        self.map.insert(x, p);
                    },
                }
            }
        }
    }

    fn primes(capacity: usize) -> Primes {
        Primes {
            map: HashMap::with_capacity(capacity),
            n: 1,
        }
    }

    const TARGET: usize = 10_000;

    primes(TARGET).nth(TARGET).unwrap()
}

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
