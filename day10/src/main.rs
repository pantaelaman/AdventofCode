use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use pariter::{scope, IteratorExt};

type Position = (i32, i32);

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let map: HashMap<Position, u32> = lines
    .iter()
    .enumerate()
    .map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .filter_map(|(x, c)| c.to_digit(10).map(|d| ((x as i32, y as i32), d)))
        .collect()
    })
    .concat();

  let trailheads = map.iter().filter(|(_, v)| **v == 0).map(|(k, _)| k);

  let (score_1, score_2) = scope(|scope| {
    trailheads
      .into_iter()
      .parallel_map_scoped(scope, |p| {
        (find_peaks(*p, &map, false), find_peaks(*p, &map, true))
      })
      .reduce(|(a1, b1), (a2, b2)| (a1 + a2, b1 + b2))
      .unwrap()
  })
  .unwrap();

  println!("Part 1: {score_1}");
  println!("Part 2: {score_2}");
}

fn find_peaks(
  start: Position,
  map: &HashMap<Position, u32>,
  all_paths: bool,
) -> usize {
  let mut frontier = vec![start];
  let mut visited = HashSet::new();
  let mut peaks = 0;
  while !frontier.is_empty() {
    for (x, y) in std::mem::take(&mut frontier) {
      if !all_paths && visited.contains(&(x, y)) {
        continue;
      }
      visited.insert((x, y));
      let level = *map.get(&(x, y)).unwrap();
      if level == 9 {
        peaks += 1;
        continue;
      }
      let successors = raw_successors((x, y))
        .filter(|p| map.get(p).is_some_and(|v| *v == level + 1));
      frontier.extend(successors);
    }
  }
  peaks
}

fn raw_successors((x, y): Position) -> impl Iterator<Item = Position> {
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)].into_iter()
}
