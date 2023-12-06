use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read + Seek>(reader: &mut BufReader<F>) -> (usize, usize) {
    let (times, distances) = reader
        .lines()
        .map(|line| {
            line.unwrap()
                .split_whitespace()
                .skip(1)
                .map(|s| s.parse::<usize>().unwrap())
                .collect_vec()
        })
        .collect_tuple()
        .unwrap();
    let p1 = times
        .into_iter()
        .zip(distances.into_iter())
        .map(solve_race)
        // .inspect(|el| println!("{}", el))
        .product();
    reader.rewind().unwrap();
    println!("");
    let p2 = reader
        .lines()
        .map(|line| {
            line.unwrap()
                .split_whitespace()
                .skip(1)
                .collect::<String>()
                .parse::<usize>()
                .unwrap()
        })
        .collect_tuple()
        .unwrap();
    (p1, solve_race(p2))
}

fn solve_race((t, d): (usize, usize)) -> usize {
    let part = ((t.pow(2) - (4 * d)) as f64).sqrt() / 2.;
    let half_time = t as f64 / 2.;
    println!("{}, {}", half_time + part, half_time - part);
    let top = (half_time + part).ceil() as usize;
    let bottom = if (half_time - part).ceil() == (half_time - part) {
        (half_time - part) + 1.
    } else {
        (half_time - part).ceil()
    } as usize;
    top - bottom
}
