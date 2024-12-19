use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  let patterns: HashSet<&str> = lines[0].split(", ").collect();
  let regex =
    Regex::new(format!("^({})+$", patterns.iter().join("|")).as_str()).unwrap();

  let total = lines
    .iter()
    .skip(2)
    .filter(|line| regex.is_match(line))
    .count();

  println!("Part 1: {}", total);

  let mut cache = HashMap::new();
  let total = lines
    .iter()
    .skip(2)
    .map(|line| dfs_possibilities(line, &patterns, &mut cache))
    .sum::<usize>();

  println!("Part 2: {}", total);
}

fn dfs_possibilities<'a>(
  haystack: &'a str,
  patterns: &HashSet<&'a str>,
  cache: &mut HashMap<&'a str, usize>,
) -> usize {
  if let Some(val) = cache.get(haystack) {
    return val.clone();
  }

  if haystack == "" {
    return 1;
  }

  let total = patterns
    .iter()
    .filter(|p| haystack.starts_with(**p))
    .map(|pattern| {
      dfs_possibilities(&haystack[pattern.len()..], patterns, cache)
    })
    .sum();

  cache.insert(haystack, total);

  total
}
