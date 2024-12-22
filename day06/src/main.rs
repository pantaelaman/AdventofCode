use itertools::Itertools;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    println!("Part 1: {}", part1(&mut reader));
    reader.rewind().unwrap();
    println!("Part 2: {}", part2(&mut reader));
}

fn part1<F: Read>(reader: &mut BufReader<F>) -> usize {
    let mut banks = reader
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split_whitespace()
        .map(str::parse::<usize>)
        .map(Result::unwrap)
        .collect_vec();
    let num_banks = banks.len();
    let mut states: HashSet<Vec<usize>> = HashSet::new();
    let mut steps = 0;
    while states.insert(banks.clone()) {
        let (mut idx, count) = banks
            .iter()
            .cloned()
            .enumerate()
            .max_set_by(|(_, a), (_, b)| a.cmp(b))
            .first()
            .unwrap()
            .clone();
        banks[idx] = 0;
        for _ in 0..count {
            idx = (idx + 1) % num_banks;
            banks[idx] += 1;
        }
        steps += 1;
    }
    steps
}

fn part2<F: Read>(reader: &mut BufReader<F>) -> usize {
    let mut banks = reader
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split_whitespace()
        .map(str::parse::<usize>)
        .map(Result::unwrap)
        .collect_vec();
    let num_banks = banks.len();
    let mut states: HashSet<Vec<usize>> = HashSet::new();
    let mut steps = 0;
    let mut looping = false;
    let mut state = states.insert(banks.clone());
    while !looping || state {
        let (mut idx, count) = banks
            .iter()
            .cloned()
            .enumerate()
            .max_set_by(|(_, a), (_, b)| a.cmp(b))
            .first()
            .unwrap()
            .clone();
        banks[idx] = 0;
        for _ in 0..count {
            idx = (idx + 1) % num_banks;
            banks[idx] += 1;
        }
        if looping {
            steps += 1;
        }
        if !looping && !state {
            looping = true;
            states.clear();
        }
        state = states.insert(banks.clone());
    }
    steps
}
