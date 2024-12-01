use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader, BufWriter},
};

use itertools::Itertools;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let mut reader = BufReader::new(file);

  let (mut left, mut right): (Vec<i32>, Vec<i32>) = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();
      line
        .split_whitespace()
        .map(|n| n.parse::<i32>().unwrap())
        .collect_tuple()
        .unwrap()
    })
    .unzip();

  left.sort();
  right.sort();

  let part1: u32 = left
    .iter()
    .zip(right.iter())
    .map(|(l, r)| l.abs_diff(*r))
    .sum();

  let mut right_set = HashMap::new();
  for n in right {
    *right_set.entry(n).or_insert(0) += 1;
  }

  let part2: i32 = left
    .iter()
    .map(|n| n * right_set.get(n).unwrap_or(&0))
    .sum();

  println!("Part 1: {}", part1);
  println!("Part 2: {}", part2);
}
