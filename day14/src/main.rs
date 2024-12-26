use std::{collections::HashMap, io::stdin};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let regex = Regex::new(
    r"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.",
  ).unwrap();

  let rds: HashMap<&str, Box<dyn Fn(usize) -> usize>> = lines
    .iter()
    .map(|line| {
      let caps = regex.captures(line).unwrap();
      let (name, speed, duration, breaks) = caps
        .iter()
        .skip(1)
        .map(|c| c.unwrap().as_str())
        .collect_tuple()
        .unwrap();

      (
        name,
        Box::new(reindeer(
          speed.parse().unwrap(),
          duration.parse().unwrap(),
          breaks.parse().unwrap(),
        )) as Box<dyn Fn(usize) -> usize>,
      )
    })
    .collect();

  let farthest = rds.values().map(|rd| rd(2503)).max().unwrap();
  println!("Part 1: {farthest}");

  let highest = (1..2503)
    .fold(HashMap::<&str, usize>::new(), |mut acc, secs| {
      for (name, _) in rds
        .iter()
        .map(|(name, rd)| (name, rd(secs)))
        .max_set_by_key(|(_, dist)| *dist)
      {
        *acc.entry(name).or_default() += 1;
      }
      acc
    })
    .into_values()
    .max()
    .unwrap();
  println!("Part 2: {highest}");
}

fn reindeer(
  speed: usize,
  duration: usize,
  breaks: usize,
) -> impl Fn(usize) -> usize {
  let total_cycle = duration + breaks;
  let total_cycle_dist = duration * speed;
  move |secs| {
    (secs / total_cycle) * total_cycle_dist
      + (secs % total_cycle).min(duration) * speed
  }
}
