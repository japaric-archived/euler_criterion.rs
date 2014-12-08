#![feature(slicing_syntax)]

extern crate test;
extern crate time;

use std::io::{stdio, File};
use std::os;

use Direction::{South, SouthEast, SouthWest, West};

enum Direction {
    South,
    SouthEast,
    SouthWest,
    West,
}

static DIRECTIONS: &'static [Direction] = &[South, SouthEast, SouthWest, West];

impl Direction {
    fn step(self) -> (uint, uint) {
        match self {
            South => (1, 0),
            SouthEast => (1, -1),
            SouthWest => (1, 1),
            West => (0, 1),
        }
    }
}

struct Grid {
    data: Vec<u32>,
    size: uint,
}

impl Grid {
    fn from_str(grid: &str) -> Grid {
        let size = grid.lines().count();

        assert!(grid.lines().all(|line| line.words().count() == size), "Grid is not square");

        let mut data = Vec::with_capacity(size * size);
        for line in grid.lines() {
            for word in line.words() {
                data.push(from_str(word).unwrap());
            }
        }

        Grid {
            data: data,
            size: size,
        }
    }

    fn get(&self, row: uint, col: uint) -> Option<u32> {
        if row < self.size && col < self.size {
            Some(unsafe { *self.data[].unsafe_get(row * self.size + col) })
        } else {
            None
        }
    }

    fn product(
        &self,
        (row, col): (uint, uint),
        direction: Direction,
        window: uint
    ) -> Option<u32> {
        let (row_step, col_step) = direction.step();
        let mut p = match self.get(row, col) {
            Some(elem) => elem,
            None => return None,
        };

        for i in range(1, window) {
            match self.get(row + i * row_step, col + i * col_step) {
                Some(elem) => p *= elem,
                None => return None,
            }
        }

        Some(p)
    }
}

fn solution(grid: &str) -> u32 {
    let grid = Grid::from_str(grid);

    range(0, grid.size).fold(0, |max, row| {
        range(0, grid.size).fold(max, |max, col| {
            DIRECTIONS.iter().fold(max, |max, &dir| {
                match grid.product((row, col), dir, 4) {
                    None => max,
                    Some(p) => if p > max { p } else { max },
                }
            })
        })
    })
}

fn main() {
    let contents = File::open(&Path::new("011.txt")).read_to_string().unwrap();
    let contents = contents[];

    match os::args()[] {
        [_, ref flag] if flag[] == "-a" => return println!("{}", solution(contents)),
        _ => {},
    }

    for line in stdio::stdin().lock().lines() {
        let iters: u64 = from_str(line.unwrap()[].trim()).unwrap();

        let start = time::precise_time_ns();
        for _ in range(0, iters) {
            test::black_box(solution(contents));
        }
        let end = time::precise_time_ns();

        println!("{}", end - start);
    }
}
