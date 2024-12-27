use std::{collections::HashSet, io::stdin};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let weights = lines
    .iter()
    .map(|line| line.parse::<usize>().unwrap())
    .collect_vec();

  let min_qe = seek(3, &weights).unwrap();
  println!("Part 1: {}", min_qe);
  let min_qe = seek(4, &weights).unwrap();
  println!("Part 1: {}", min_qe);
}

fn seek(parts: usize, weights: &[usize]) -> Option<usize> {
  let target_weight = weights.iter().sum::<usize>() / parts;
  for k in 2..weights.len() {
    if let Some(qe) = weights
      .iter()
      .combinations(k)
      .filter(|cs| cs.iter().copied().sum::<usize>() == target_weight)
      .map(|cs| cs.into_iter().product::<usize>())
      .min()
    {
      return Some(qe);
    }
  }
  None
}
