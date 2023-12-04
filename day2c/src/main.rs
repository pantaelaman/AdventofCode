use itertools::Itertools;
use regex::Regex;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}

fn puzzle<F: Read>(reader: &mut BufReader<F>) -> (u128, u128) {
    let re_id = Regex::new(r"Game (\d+): .*").unwrap();
    let re_counts = Regex::new(r"(\d+) (\w)\w+(?:(?:,|;) )?").unwrap();
    let mut id_sum = 0;
    let mut powers_sum = 0;
    for line in reader.lines() {
        let line = line.unwrap();
        let id = &re_id.captures(&line).unwrap()[1].parse::<u128>().unwrap();
        let counts = &re_counts
            .captures_iter(&line)
            .fold((0, 0, 0), |mut acc, cs| {
                println!("{:?}", cs);
                let num = cs[1].parse::<u128>().unwrap();
                match &cs[2] {
                    "r" => acc.0 = acc.0.max(num),
                    "g" => acc.1 = acc.1.max(num),
                    "b" => acc.2 = acc.2.max(num),
                    _ => unreachable!(),
                };
                acc
            });
        if counts.0 <= 12 && counts.1 <= 13 && counts.2 <= 14 {
            id_sum += id;
        }
        powers_sum += counts.0 * counts.1 * counts.2;
    }
    (id_sum, powers_sum)
}
