#![feature(test)]

fn solution(input: &str) -> u32 {
    use std::cmp;

    enum Direction {
        South,
        SouthEast,
        SouthWest,
        West,
    }

    /// A square grid of numbers
    struct Grid {
        data: Vec<u32>,
        size: usize,
    }

    impl Grid {
        fn from_str(grid: &str) -> Grid {
            let size = grid.lines().count();

            assert!(grid.lines().all(|line| line.split(' ').count() == size));

            let mut data = Vec::with_capacity(size * size);
            for line in grid.lines() {
                for word in line.split(' ') {
                    data.push(word.parse().unwrap());
                }
            }

            Grid {
                data: data,
                size: size,
            }
        }

        /// Whether one can walk `steps` in `direction` from `start` without leaving the grid
        fn can_walk(&self, start: &(usize, usize), direction: &Direction, steps: usize) -> bool {
            let &(row, col) = start;
            let size = self.size;

            match *direction {
                Direction::South => {
                    row + steps < size
                },
                Direction::SouthEast => {
                    row + steps < size && col.checked_sub(steps).map(|end| {
                        end < size
                    }).unwrap_or(false)
                },
                Direction::SouthWest => row + steps < size && col + steps < size,
                Direction::West => col + steps < size,
            }
        }

        /// Gets the element at position `(row, col)`
        unsafe fn get_unchecked(&self, &(row, col): &(usize, usize)) -> &u32 {
            debug_assert!(row < self.size && col < self.size);

            self.data.get_unchecked(row * self.size + col)
        }

        /// Returns the product of `n` numbers along `direction` starting from `start`
        unsafe fn product(
            &self,
            start: &(usize, usize),
            direction: &Direction,
            n: usize,
        ) -> u32 {
            fn step(start: &mut (usize, usize), direction: &Direction) {
                match *direction {
                    Direction::South => start.0 += 1,
                    Direction::SouthEast => {
                        start.0 += 1;
                        start.1 -= 1;
                    },
                    Direction::SouthWest => {
                        start.0 += 1;
                        start.1 += 1;
                    },
                    Direction::West => start.1 += 1,
                }
            }

            debug_assert!(start.0 < self.size && start.1 < self.size);

            let ref mut pos = start.clone();
            let mut product = 1;

            for i in 0..n {
                product *= *self.get_unchecked(pos);
                step(pos, direction);
            }

            product
        }
    }

    const DIRECTIONS: &'static [Direction] = &[
        Direction::South, Direction::SouthEast, Direction::SouthWest, Direction::West,
    ];
    const STEPS: usize = 4;

    let grid = Grid::from_str(input);

    (0..grid.size).fold(0, |product, row| {
        (0..grid.size).fold(product, |product, col| {
            DIRECTIONS.iter().fold(product, |product, direction| {
                let ref pos = (row, col);

                if grid.can_walk(pos, direction, STEPS) {
                    cmp::max(product, unsafe {
                        grid.product(pos, direction, STEPS)
                    })
                } else {
                    product
                }
            })
        })
    })
}

fn main() {
    extern crate test;
    extern crate time;

    use std::env;
    use std::ffi::OsStr;
    use std::fs::File;
    use std::io::{BufRead, Read, self};
    use std::path::Path;

    let mut input = String::new();
    File::open(Path::new("011.txt")).unwrap().read_to_string(&mut input).unwrap();

    if let Some(arg) = env::args_os().skip(1).next() {
        if arg.as_os_str() == OsStr::new("-a") {
            return println!("{}", solution(&input))
        }
    }

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let iters: u64 = line.unwrap().trim().parse().unwrap();

        let start = time::precise_time_ns();
        for _ in (0..iters) {
            test::black_box(solution(&input));
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}

// Cargo.toml
//
// [dependencies]
// time = "*"
