use itertools::Itertools;
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
    let mut jumps = reader
        .lines()
        .map(|l| l.unwrap().parse::<i32>().unwrap())
        .collect_vec();
    let length = jumps.len();
    let mut ptr = 0;
    let mut steps = 0;
    while ptr >= 0 && ptr < length as i32 {
        jumps[ptr as usize] += 1;
        ptr += jumps[ptr as usize] - 1;
        steps += 1;
    }
    steps
}

fn part2<F: Read>(reader: &mut BufReader<F>) -> usize {
    let mut jumps = reader
        .lines()
        .map(|l| l.unwrap().parse::<i32>().unwrap())
        .collect_vec();
    let length = jumps.len();
    let mut ptr = 0;
    let mut steps = 0;
    while ptr >= 0 && ptr < length as i32 {
        let n = &mut jumps[ptr as usize];
        ptr += *n;
        if *n >= 3 {
            *n -= 1;
        } else {
            *n += 1;
        }
        steps += 1;
    }
    steps
}
