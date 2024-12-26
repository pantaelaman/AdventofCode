use std::{
  collections::HashMap,
  fs::File,
  io::{stdin, BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let reader = BufReader::new(stdin());

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let mut locks: Vec<[usize; 5]> = Vec::new();
  let mut keys: Vec<[usize; 5]> = Vec::new();
  for scheme in lines.split(|s| s.is_empty()) {
    let mut template = [0; 5];
    for line in scheme.iter() {
      for (i, c) in line.chars().enumerate() {
        match c {
          '#' => template[i] += 1,
          _ => {}
        }
      }
    }

    for t in template.iter_mut() {
      *t -= 1;
    }

    if scheme[0].chars().any(|c| c == '#') {
      locks.push(template);
    } else {
      keys.push(template);
    }
  }

  let pairs = keys
    .iter()
    .cartesian_product(locks.iter())
    .filter(|(key, lock)| key.iter().zip(lock.iter()).all(|(k, l)| k + l <= 5))
    .collect_vec();

  println!("Part 1: {}", pairs.len());
}
