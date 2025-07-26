use std::{collections::HashMap, io::stdin};

use itertools::Itertools;

fn main() {
  let mut joltages = stdin()
    .lines()
    .map(|line| line.unwrap().parse::<usize>().unwrap())
    .collect_vec();

  joltages.push(0);
  joltages.sort();

  let target = joltages.last().unwrap() + 3;

  let counts = joltages.iter().tuple_windows().map(|(a, b)| b - a).counts();

  let p1 = counts.get(&1).copied().unwrap_or_default()
    * (counts.get(&3).copied().unwrap_or_default() + 1);
  println!("Part 1: {}", p1);

  fn dfs(
    index: usize,
    target: &usize,
    joltages: &Vec<usize>,
    cache: &mut HashMap<usize, usize>,
  ) -> usize {
    if let Some(ends) = cache.get(&index) {
      return *ends;
    }

    let joltage = joltages[index];

    if joltage + 3 == *target {
      return 1;
    }

    let mut total_ends = 0;
    for i in (index + 1).. {
      if let Some(_) = joltages.get(i).filter(|v| **v <= joltage + 3) {
        total_ends += dfs(i, target, joltages, cache);
      } else {
        break;
      }
    }

    cache.insert(index, total_ends);

    total_ends
  }

  let mut cache = HashMap::new();

  let routes = dfs(0, &target, &joltages, &mut cache);

  println!("Part 2: {}", routes);
}
