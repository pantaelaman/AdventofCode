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
    let (triples, doubles) = input
        .lines()
        .map(|line| {
            line.unwrap()
                .chars()
                .counts()
                .values()
                .fold((false, false), |acc, value| {
                    if value == &3 {
                        (true, acc.1)
                    } else if value == &2 {
                        (acc.0, true)
                    } else {
                        acc
                    }
                })
        })
        .fold((0, 0), |mut acc, value| {
            if value.0 {
                acc.0 += 1;
            }
            if value.1 {
                acc.1 += 1;
            }
            acc
        });

    triples * doubles
}

fn part2(input: Input) -> String {
    input
        .lines()
        .map(|line| line.unwrap())
        .combinations(2)
        .find_map(|strs| {
            let mut allowance = None;
            for (i, (chr1, chr2)) in strs[0].chars().zip(strs[1].chars()).enumerate() {
                if chr1 != chr2 {
                    if allowance.is_some() {
                        return None;
                    } else {
                        allowance = Some(i);
                    }
                }
            }
            allowance.map(|index| {
                let mut str = strs[0].to_owned();
                str.remove(index);
                str
            })
        })
        .unwrap()
}
