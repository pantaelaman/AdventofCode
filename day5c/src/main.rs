use itertools::Itertools;
use regex::Regex;
use std::fs::File;
use std::io::{BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read>(reader: &mut BufReader<F>) -> (isize, isize) {
    let re_seeds = Regex::new(r"seeds: ((?:\d+ )+(?:\d+))\n((?:.|\n)*)").unwrap();
    let re_map = Regex::new(r"(?:\w+)-to-(?:\w+) map:\n((?:.|\n)*)").unwrap();

    let mut contents = String::new();
    reader.read_to_string(&mut contents).unwrap();
    let captures = re_seeds.captures(&contents).unwrap();
    let seeds_p1 = captures[1]
        .split_whitespace()
        .map(|s| s.parse::<isize>().unwrap())
        .collect_vec();
    let seeds_p2 = seeds_p1
        .clone()
        .into_iter()
        .chunks(2)
        .into_iter()
        .map(|chunk| {
            let (a, b) = chunk.into_iter().collect_tuple().unwrap();
            a..(a + b)
        })
        .collect_vec();
    let maps = captures[2]
        .split("\n\n")
        .map(|s| {
            let captures = re_map.captures(s).expect(s);
            captures[1]
                .split('\n')
                .filter(|s| !s.is_empty())
                .fold(Vec::new(), |mut acc, r| {
                    let (dest, src, length) = r
                        .split_whitespace()
                        .map(|s| s.parse().unwrap())
                        .collect_tuple()
                        .expect(r);
                    acc.push((src..(src + length), dest - src));
                    acc
                })
        })
        .collect_vec();
    let p1 = seeds_p1
        .into_iter()
        .map(|seed| {
            println!("");
            maps.iter().fold(seed, |seed, map| {
                let new = map
                    .iter()
                    .find_map(|(range, offset)| range.contains(&seed).then_some(seed + offset))
                    .unwrap_or(seed);
                println!("{} -> {}", seed, new);
                new
            })
        })
        .min()
        .unwrap();
    let p2 = {
        maps.iter().fold(seeds_p2, |mut seeds_p2, map| {
            let mut new = Vec::new();
            while let Some(seeds) = seeds_p2.pop() {
                // println!("{:?} : {:?} %%{:?}%%", seeds, seeds_p2, new);
                new.push(
                    map.iter()
                        .find_map(|(range, offset)| {
                            let contains_front = range.contains(&seeds.start);
                            let contains_back = range.contains(&(seeds.end - 1));
                            if contains_front && contains_back {
                                Some((seeds.start + offset)..(seeds.end + offset))
                            } else if contains_front {
                                seeds_p2.push(range.end..seeds.end);
                                Some((seeds.start + offset)..(range.end + offset))
                            } else if contains_back {
                                seeds_p2.push(seeds.start..range.start);
                                Some((range.start + offset)..(seeds.end + offset))
                            } else {
                                None
                            }
                        })
                        .unwrap_or(seeds),
                );
            }
            println!("{:?}", new);
            new
        })
    }
    .into_iter()
    .map(|range| range.start)
    .min()
    .unwrap();
    (p1, p2)
}
