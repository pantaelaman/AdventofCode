use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read>(reader: &mut BufReader<F>) -> (usize, usize) {
    let mut map: HashMap<String, [String; 2]> = HashMap::new();
    let re = Regex::new(r"(.{3}) = \((.{3}), (.{3})\)").unwrap();
    let mut lines = reader.lines();
    let pattern_line = lines.next().unwrap().unwrap();
    let pattern = pattern_line
        .chars()
        .map(|c| match c {
            'L' => 0,
            'R' => 1,
            _ => unreachable!(),
        })
        .cycle();
    for line in lines.skip(1) {
        let line = line.unwrap();
        let captures = re.captures(&line).unwrap();
        map.insert(
            captures[1].to_string(),
            [captures[2].to_string(), captures[3].to_string()],
        );
    }

    let mut p1 = 0;
    let mut next = &String::from("AAA");
    for step in pattern.clone() {
        next = &map.get(next).unwrap()[step];
        p1 += 1;
        if next == "ZZZ" {
            break;
        }
    }

    let nexts = map
        .keys()
        .filter(|k| k.chars().last().unwrap() == 'A')
        .map(|k| {
            let mut next = k;
            let mut count = 0;
            for step in pattern.clone() {
                next = &map.get(next).unwrap()[step];
                count += 1;
                println!("{count}: {step} {next}");
                if next.chars().last().unwrap() == 'Z' {
                    break;
                }
            }
            count
        });
    let p2 = nexts.reduce(|a, b| num::integer::lcm(a, b)).unwrap();

    (p1, p2)
}
