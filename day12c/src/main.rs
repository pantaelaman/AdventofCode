use grid::Grid;
use itertools::{repeat_n, Itertools};
use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Spring {
    Operational,
    Damaged,
    Unknown,
}

impl From<char> for Spring {
    fn from(value: char) -> Self {
        match value {
            '.' => Operational,
            '#' => Damaged,
            '?' => Unknown,
            _ => unimplemented!(),
        }
    }
}

use Spring::*;

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read>(reader: &mut BufReader<F>) -> (usize, usize) {
    let mut dynamic_pool = HashMap::new();
    reader.lines().fold((0, 0), |acc, line| {
        let line = line.unwrap();
        let (springs, groups) = line.split_whitespace().collect_tuple().unwrap();
        let springs = springs.chars().map(Spring::from).collect_vec();
        let groups = groups
            .split(',')
            .map(|s| s.parse::<usize>().unwrap())
            .collect_vec();
        let springsp2 = repeat_n(springs.clone(), 5)
            .interleave(repeat_n(vec![Unknown], 4))
            .concat();
        let groupsp2 = repeat_n(groups.clone(), 5).concat();
        dynamic_pool.clear();
        let p1 = solve(&mut dynamic_pool, &springs, &groups, 0, 0, 0);
        dynamic_pool.clear();
        let p2 = solve(&mut dynamic_pool, &springsp2, &groupsp2, 0, 0, 0);
        (acc.0 + p1, acc.1 + p2)
    })
}

fn solve(
    dynamic_pool: &mut HashMap<(usize, usize, usize), usize>,
    springs: &Vec<Spring>,
    groups: &Vec<usize>,
    i: usize,
    group_i: usize,
    curlen: usize,
) -> usize {
    let key = (i, group_i, curlen);
    if let Some(v) = dynamic_pool.get(&key) {
        return *v;
    }
    if i >= springs.len() {
        if (group_i == groups.len() && curlen == 0)
            || (group_i == groups.len() - 1 && groups[group_i] == curlen)
        {
            return 1;
        }
        return 0;
    }

    let mut num_perms = 0;
    let cur_spring = springs[i];
    for spring_option in [Operational, Damaged] {
        if cur_spring != spring_option && cur_spring != Unknown {
            continue;
        }
        match spring_option {
            Operational => {
                if curlen == 0 {
                    num_perms += solve(dynamic_pool, springs, groups, i + 1, group_i, 0);
                } else if group_i < groups.len() && groups[group_i] == curlen {
                    num_perms += solve(dynamic_pool, springs, groups, i + 1, group_i + 1, 0);
                }
            }
            Damaged => {
                num_perms += solve(dynamic_pool, springs, groups, i + 1, group_i, curlen + 1)
            }
            _ => unreachable!(),
        }
    }

    dynamic_pool.insert(key, num_perms);
    num_perms
}
