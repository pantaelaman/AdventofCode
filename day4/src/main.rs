use itertools::Itertools;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};

fn main() {
    let file = std::fs::File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    println!("Part 1: {}", part1(&mut reader));
    reader.rewind().unwrap();
    println!("Part 2: {}", part2(&mut reader));
}

fn part1<F: Read>(reader: &mut BufReader<F>) -> usize {
    let mut valid = 0;
    for line in reader.lines() {
        let line = line.unwrap();
        if line.split_whitespace().duplicates().count() == 0 {
            valid += 1;
        }
    }
    valid
}

fn part2<F: Read>(reader: &mut BufReader<F>) -> usize {
    let mut valid = 0;
    for line in reader.lines() {
        let line = line.unwrap();
        if line
            .split_whitespace()
            .combinations(2)
            .filter(|ss| ss[0].chars().sorted().eq(ss[1].chars().sorted()))
            .count()
            == 0
        {
            valid += 1;
        }
    }
    valid
}
