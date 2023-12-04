use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = part1(&mut reader);
    println!("Part 1: {}", p1);
    reader.rewind().unwrap();
    println!("Part 2: {}", p2);
}

fn part1<F: Read>(reader: &mut BufReader<F>) -> (usize, usize) {
    let mut state = 0;
    let mut card_scores = Vec::new();
    let mut card_copies = HashMap::new();
    for (i, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        let mut data = line.split(':').skip(1).next().unwrap().split('|');
        let winning_nums = data
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<usize>().unwrap())
            .collect_vec();
        let num_winners = data
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<usize>().unwrap())
            .filter(|n| winning_nums.contains(n))
            .count();
        println!("{:?}", winning_nums);
        println!("{:?}", num_winners);
        if num_winners != 0 {
            card_scores.push(2usize.pow(num_winners as u32 - 1));
        } else {
            card_scores.push(0)
        }
        let num_copies = card_copies.get(&i).map(|n| *n).unwrap_or(0) + 1;
        println!("nm: {:?}", num_winners);
        println!("nc: {:?}", num_copies);
        for c in 0..num_winners {
            let ni = i + c + 1;
            if card_copies.contains_key(&ni) {
                *card_copies.get_mut(&ni).unwrap() += num_copies;
            } else {
                card_copies.insert(ni, num_copies);
            }
        }
        println!("{:#?}", card_copies);
        state += card_scores[i]
    }
    let num_cards = card_scores.len();
    let num_copies = card_copies
        .into_iter()
        .filter(|(k, _)| *k < num_cards)
        .map(|(_, v)| v)
        .sum::<usize>()
        + num_cards;
    (state, num_copies)
}

fn part2<F: Read>(reader: &mut BufReader<F>) -> usize {
    let mut state = 0;
    for line in reader.lines() {
        let line = line.unwrap();
    }
    0
}
