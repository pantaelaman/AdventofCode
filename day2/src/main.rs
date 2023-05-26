use itertools::{Itertools, MinMaxResult};
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
        .map(|line| {
            match line
                .unwrap()
                .split_whitespace()
                .map(|num| num.parse::<usize>().unwrap())
                .minmax()
            {
                MinMaxResult::MinMax(min, max) => max - min,
                _ => unreachable!(),
            }
        })
        .sum()
}

fn part2(input: Input) -> usize {
    input
        .lines()
        .map(|line| {
            line.unwrap()
                .split_whitespace()
                .map(|num| num.parse::<usize>().unwrap())
                .tuple_combinations()
                .find_map(|(num1, num2)| {
                    (num1 % num2 == 0)
                        .then(|| num1 / num2)
                        .or((num2 % num1 == 0).then(|| num2 / num1))
                })
                .unwrap()
        })
        .sum()
}
