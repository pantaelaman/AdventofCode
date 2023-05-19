use std::fs::File;
use std::io::{BufRead, BufReader, Seek};

use itertools::Itertools;

type Input = BufReader<File>;

const ROWS: u8 = 128;
const COLS: u8 = 8;

fn main() {
    let mut input = BufReader::new(File::open("input.txt").unwrap());

    println!("Part 1: {}", part1(&mut input));
    input.rewind().unwrap();
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: &mut Input) -> usize {
    get_seat_ids(input.lines().map(|line| line.unwrap().to_owned()))
        .max()
        .unwrap()
}

fn part2(input: &mut Input) -> usize {
    get_seat_ids(input.lines().map(|line| line.unwrap().to_owned()))
        .sorted()
        .tuple_windows()
        .find_map(|(prev, next)| (next - prev == 2).then(|| next - 1))
        .unwrap()
}

fn get_seat_ids<I: Iterator<Item = String>>(input: I) -> impl Iterator<Item = usize> {
    input.map(|line| {
        let line = line;
        let mut chars = line.chars();

        let mut row_min: u8 = 0;
        let mut row_max: u8 = ROWS - 1;
        for _ in 0..7 {
            let diff = (row_max - row_min + 1) / 2;
            match chars.next().unwrap() {
                'F' => row_max -= diff,
                'B' => row_min += diff,
                _ => unimplemented!(),
            }
        }

        let mut col_min: u8 = 0;
        let mut col_max: u8 = COLS - 1;
        for _ in 0..3 {
            let diff = (col_max - col_min + 1) / 2;
            match chars.next().unwrap() {
                'L' => col_max -= diff,
                'R' => col_min += diff,
                _ => unimplemented!(),
            }
        }

        ((row_min as usize) * 8) + (col_min as usize)
    })
}
