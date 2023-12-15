#![feature(ascii_char)]
use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read>(reader: &mut BufReader<F>) -> (usize, usize) {
    let line = reader.lines().next().unwrap().unwrap();
    let p1 = line.split(',').map(|s| hash_str(s)).sum();
    let mut map: Vec<Vec<(&str, usize)>> = Vec::new();
    map.extend(vec![Vec::new(); 256]);
    for instruction in line.split(',') {
        if let Some((label, value)) = instruction.split('=').collect_tuple() {
            let b = &mut map[hash_str(label)];
            let value = value.parse::<usize>().unwrap();
            if let Some(i) = b.iter().position(|(l, _)| l == &label) {
                b[i].1 = value;
            } else {
                b.push((label, value));
            }
        } else {
            let label = instruction.split('-').next().unwrap();
            let b = &mut map[hash_str(label)];
            if let Some(i) = b.iter().position(|(l, _)| l == &label) {
                b.remove(i);
            }
        }
    }
    let p2 = map
        .into_iter()
        .enumerate()
        .map(|(i, b)| {
            b.into_iter()
                .enumerate()
                .map(|(j, (_, l))| (i + 1) * (j + 1) * l)
                .sum::<usize>()
        })
        .sum();
    (p1, p2)
}

fn hash_str(data: &str) -> usize {
    let mut acc = 0;
    for chr in data.chars() {
        acc += chr.as_ascii().unwrap().to_u8() as usize;
        acc *= 17;
        acc %= 256;
    }
    acc
}
