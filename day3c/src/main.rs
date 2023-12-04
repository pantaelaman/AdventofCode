use itertools::Itertools;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

#[derive(PartialEq, Eq, Debug)]
enum Adjacency {
    NONE,
    ANY,
    GEAR(usize, usize),
}
use Adjacency::*;

impl std::ops::BitOr for Adjacency {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (GEAR(x, y), _) => GEAR(x, y),
            (_, GEAR(x, y)) => GEAR(x, y),
            (NONE, NONE) => NONE,
            _ => ANY,
        }
    }
}

impl Adjacency {
    fn is_some(&self) -> bool {
        match self {
            NONE => false,
            _ => true,
        }
    }
}

const NEIGHBOURS: [(isize, isize); 8] = [
    (-1, -1),
    (0, -1),
    (1, -1),
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
];

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = part1(&mut reader);
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}

fn part1<F: Read>(reader: &mut BufReader<F>) -> (usize, usize) {
    let grid = reader
        .lines()
        .map(|l| l.unwrap().chars().collect_vec())
        .collect_vec();
    let size_x = grid.len();
    let size_y = grid[0].len();
    let mut buf: String = String::new();
    let mut adjacent = NONE;
    let mut sum = 0;
    let mut gears: HashMap<(usize, usize), Vec<usize>> = HashMap::new();
    for (y, row) in grid.iter().enumerate() {
        for (x, chr) in row.iter().enumerate() {
            if chr.is_digit(10) {
                if !adjacent.is_some() {
                    adjacent = check_adjacencies(x, y, size_x, size_y, &grid);
                    println!("");
                    if adjacent.is_some() {
                        println!("Adjacent @ {},{} : {}", x, y, chr);
                    }
                }
                buf.push(*chr);
            } else if buf.len() != 0 {
                if adjacent.is_some() {
                    let num = buf.parse::<usize>().unwrap();
                    sum += num;
                    match adjacent {
                        GEAR(gx, gy) => {
                            if gears.contains_key(&(gx, gy)) {
                                gears.get_mut(&(gx, gy)).unwrap().push(num);
                            } else {
                                gears.insert((gx, gy), vec![num]);
                            }
                        }
                        _ => {}
                    }
                    adjacent = NONE;
                }
                buf.clear();
            }
        }
        if buf.len() != 0 {
            if adjacent.is_some() {
                let num = buf.parse::<usize>().unwrap();
                sum += num;
                match adjacent {
                    GEAR(gx, gy) => {
                        if gears.contains_key(&(gx, gy)) {
                            gears.get_mut(&(gx, gy)).unwrap().push(num);
                        } else {
                            gears.insert((gx, gy), vec![num]);
                        }
                    }
                    _ => {}
                }
                adjacent = NONE;
            }
            buf.clear();
        }
    }

    println!("{:#?}", gears);

    (
        sum,
        gears
            .values()
            .filter_map(|vs| {
                if vs.len() == 2 {
                    Some(vs.iter().product::<usize>())
                } else {
                    None
                }
            })
            .sum(),
    )
}

fn check_adjacencies(
    x: usize,
    y: usize,
    size_x: usize,
    size_y: usize,
    grid: &Vec<Vec<char>>,
) -> Adjacency {
    NEIGHBOURS
        .iter()
        .map(|(dx, dy)| {
            let nx = (x as isize + dx) as usize;
            let ny = (y as isize + dy) as usize;
            if nx < size_x && ny < size_y {
                let chr = grid[ny][nx];
                if chr == '*' {
                    GEAR(nx, ny)
                } else if chr != '.' && chr.is_ascii_punctuation() {
                    ANY
                } else {
                    NONE
                }
            } else {
                NONE
            }
        })
        .inspect(|c| println!("{:?}", c))
        .reduce(|p, c| p | c)
        .unwrap()
}
