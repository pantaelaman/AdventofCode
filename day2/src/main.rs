use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

type Input = Lines<BufReader<File>>;

fn main() {
    println!("Hello, world!");
    let input = BufReader::new(File::open("input.txt").unwrap()).lines();
    println!("{:?}", part2(input).unwrap());
}

fn part1(input: Input) -> Option<usize> {
    input
        .map(|line_result| {
            line_result.ok().map(|line| {
                let (policy, password) = line.split(": ").collect_tuple().unwrap();
                let (range, chr) = policy.split(" ").collect_tuple().unwrap();
                let chr = chr.parse::<char>().unwrap();
                let (min, max) = range
                    .split("-")
                    .map(|num| num.parse::<usize>().unwrap())
                    .collect_tuple()
                    .unwrap();
                let count = password
                    .chars()
                    .fold(0, |acc, cur_chr| if cur_chr == chr { acc + 1 } else { acc });
                min <= count && count <= max
            })
        })
        .try_fold(0, |acc, valid| Some(if valid? { acc + 1 } else { acc }))
}

fn part2(input: Input) -> Option<usize> {
    input
        .map(|line_result| {
            line_result.ok().map(|line| {
                let (policy, password) = line.split(": ").collect_tuple().unwrap();
                let (range, chr) = policy.split(" ").collect_tuple().unwrap();
                let chr = chr.parse::<char>().unwrap();
                let (p1, p2) = range
                    .split("-")
                    .map(|num| num.parse::<usize>().unwrap())
                    .collect_tuple()
                    .unwrap();

                (password.chars().nth(p1 - 1).unwrap() == chr)
                    != (password.chars().nth(p2 - 1).unwrap() == chr)
            })
        })
        .try_fold(0, |acc, valid| Some(if valid? { acc + 1 } else { acc }))
}
