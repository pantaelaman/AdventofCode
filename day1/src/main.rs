use itertools::Itertools;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek};

type Input<'f> = &'f mut BufReader<File>;

fn main() {
    let mut input = BufReader::new(File::open("input.txt").unwrap());

    println!("Part 1: {}", part1(&mut input));
    input.rewind().unwrap();
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: Input) -> i32 {
    input
        .lines()
        .map(|line| line.unwrap().parse::<i32>().unwrap())
        .fold(0, |acc, count| acc + count)
}

fn part2(input: Input) -> i32 {
    let changes = input
        .lines()
        .map(|line| line.unwrap().parse::<i32>().unwrap())
        .collect_vec();

    let mut set = HashSet::new();
    let mut acc = 0;
    set.insert(acc);

    for count in changes.iter().cycle() {
        acc += count;
        if !set.insert(acc) {
            return acc;
        }
    }

    unreachable!()
}
