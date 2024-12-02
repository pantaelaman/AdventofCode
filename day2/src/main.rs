use itertools::Itertools;
use std::{
  fs::File,
  io::{BufRead, BufReader},
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let levels = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();

      line
        .split_whitespace()
        .map(|v| v.parse::<i32>().unwrap())
        .collect_vec()
    })
    .collect_vec();

  let num_safe_p1 = levels
    .iter()
    .filter(|records| check_diffs(*records))
    .count();

  println!("Part 1: {}", num_safe_p1);

  let num_safe_p2 = levels
    .iter()
    .filter(|records| {
      check_diffs(*records)
        || (0..records.len()).any(|i| {
          let mut possible = (*records).clone();
          possible.remove(i);
          check_diffs(&possible)
        })
    })
    .count();

  println!("Part 2: {}", num_safe_p2);
}

fn check_diffs(records: &Vec<i32>) -> bool {
  let records = records.into_iter().collect_vec();
  let trend = records.last().unwrap() > records.first().unwrap();

  records.into_iter().tuple_windows().all(|(p, c)| {
    let diff = c - p;
    diff >= -3 && diff != 0 && diff <= 3 && (diff > 0) == trend
  })
}
