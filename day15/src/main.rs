use std::{collections::HashMap, io::stdin};

use itertools::Itertools;
use spinoff::{spinners, Color};

const TARGET: usize = 30000000;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let starting = lines[0]
    .split(',')
    .map(|s| s.parse::<usize>().unwrap())
    .collect_vec();

  let p1 = calc_to(&starting, 2020);
  println!("Part 1: {}", p1);

  let p2 = calc_to(&starting, 30000000);
  println!("Part 2: {}", p2);
}

fn calc_to(starting: &[usize], target: usize) -> usize {
  let mut map: HashMap<usize, [usize; 2]> = starting
    .iter()
    .copied()
    .enumerate()
    .map(|(r, n)| (n, [r, r]))
    .collect();
  let mut last = map.get_mut(starting.last().unwrap()).unwrap();
  let mut collection = starting.iter().copied().collect_vec();
  for round in starting.len()..target {
    //println!("{round}");
    let spoken = last[0] - last[1];
    collection.push(spoken);
    last = map.entry(spoken).or_insert_with(|| [round, round]);
    last[1] = last[0];
    last[0] = round;
  }

  collection[target - 1]
}
