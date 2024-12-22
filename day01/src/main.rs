use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek};

type Input<'f> = &'f mut BufReader<File>;

fn main() {
    let mut input = BufReader::new(File::open("input.txt").unwrap());

    println!("Part 1: {}", part1(&mut input));
    input.rewind().unwrap();
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: Input) -> usize {
    input
        .lines()
        .exactly_one()
        .unwrap()
        .unwrap()
        .chars()
        .map(|chr| chr.to_digit(10).unwrap() as usize)
        .collect_vec()
        .iter()
        .circular_tuple_windows()
        .filter_map(|(original, next)| {
            if original == next {
                Some(original)
            } else {
                None
            }
        })
        .sum()
}

fn part2(input: Input) -> usize {
    let digits = input
        .lines()
        .exactly_one()
        .unwrap()
        .unwrap()
        .chars()
        .map(|chr| chr.to_digit(10).unwrap() as usize)
        .collect_vec();
    let size = digits.len();

    let mut sum = 0;
    for (i, digit) in digits.iter().enumerate() {
        if digit == &digits[(i + (size / 2)) % size] {
            sum += digit;
        }
    }

    sum
}
