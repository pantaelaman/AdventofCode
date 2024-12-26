use std::{collections::HashMap, io::stdin};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let regex = Regex::new(
    r"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).",
  )
  .unwrap();

  let mut deltas: HashMap<&str, HashMap<&str, i32>> = lines
    .iter()
    .map(|line| {
      let caps = regex.captures(line).unwrap();
      let (src, dir, delta, dest) = caps
        .iter()
        .skip(1)
        .map(|v| v.unwrap().as_str())
        .collect_tuple()
        .unwrap();
      let delta = delta.parse::<i32>().unwrap()
        * match dir {
          "gain" => 1,
          "lose" => -1,
          _ => unreachable!(),
        };

      (src, dest, delta)
    })
    .fold(HashMap::new(), |mut acc, (src, dest, delta)| {
      acc.entry(src).or_default().insert(dest, delta);
      acc
    });

  let best = find_best(&deltas);
  println!("Part 1: {best}");
  deltas.insert("", HashMap::new());
  let best = find_best(&deltas);
  println!("Part 2: {best}");
}

fn find_best<'k>(deltas: &HashMap<&'k str, HashMap<&'k str, i32>>) -> i32 {
  deltas
    .keys()
    .permutations(deltas.len())
    .map(|p| {
      p.into_iter()
        .circular_tuple_windows()
        .map(|(a, b)| {
          deltas.get(a).unwrap().get(b).unwrap_or(&0)
            + deltas.get(b).unwrap().get(a).unwrap_or(&0)
        })
        .sum::<i32>()
    })
    .max()
    .unwrap()
}
