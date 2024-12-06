use itertools::Itertools;
use regex::Regex;
use std::{
  fs::File,
  io::{BufRead, BufReader},
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let regex = Regex::new(r#"\\\\|\\"|\\x[0-9a-f]{2}"#).unwrap();

  let total_p1: usize = lines
    .iter()
    .map(|line| regex.find_iter(line).map(|m| m.len() - 1).sum::<usize>() + 2)
    .sum();

  println!("Part 1: {}", total_p1);

  let total_p2: usize = lines
    .iter()
    .map(|line| line.chars().filter(|c| *c == '"' || *c == '\\').count() + 2)
    .sum();

  println!("Part 2: {}", total_p2);
}
