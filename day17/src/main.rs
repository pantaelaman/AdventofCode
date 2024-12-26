use std::io::stdin;

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let containers = lines
    .iter()
    .map(|line| line.parse::<usize>().unwrap())
    .collect_vec();

  let total_coms = (2..=containers.len())
    .map(|c_size| {
      containers
        .iter()
        .combinations(c_size)
        .filter(|cs| cs.into_iter().copied().sum::<usize>() == 150)
        .count()
    })
    .filter(|v| v != &0)
    .collect_vec();

  println!("Part 1: {}", total_coms.iter().sum::<usize>());
  println!("Part 2: {}", total_coms[0]);
}
