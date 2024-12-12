use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use pathfinding::prelude::bfs_reach;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let map: HashMap<(i32, i32), char> = lines
    .iter()
    .enumerate()
    .map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .map(|(x, c)| ((x as i32, y as i32), c))
        .collect()
    })
    .concat();

  let mut regions: Vec<HashSet<(i32, i32)>> = Vec::new();
  let mut claimed: HashSet<(i32, i32)> = HashSet::new();

  for ((x, y), ty) in map.iter() {
    if claimed.contains(&(*x, *y)) {
      continue;
    }
    let region: HashSet<(i32, i32)> = bfs_reach((*x, *y), |(x, y)| {
      raw_successors((*x, *y))
        .into_iter()
        .filter(|p| map.get(p).is_some_and(|target| target == ty))
    })
    .collect();
    claimed.extend(region.iter().copied());
    regions.push(region);
  }

  let total = regions
    .iter()
    .map(|region| {
      let area = region.len();

      let perimeter = (area * 4)
        - (region
          .iter()
          .tuple_combinations()
          .filter(|((x1, y1), (x2, y2))| {
            let dx = (x1 - x2).abs();
            let dy = (y1 - y2).abs();
            (dx == 1 && dy == 0) || (dy == 1 && dx == 0)
          })
          .count()
          * 2);

      // println!("{ty}: {area} * {perimeter}");

      area * perimeter
    })
    .sum::<usize>();

  let discount = regions
    .iter()
    .map(|region| {
      let area = region.len();

      let sides = raw_successors((0, 0))
        .map(|(dx, dy)| {
          let edges = region
            .iter()
            .filter(|(x, y)| !region.contains(&(x + dx, y + dy)))
            .collect();
          combine_edges(edges)
        })
        .sum::<usize>();

      area * sides
    })
    .sum::<usize>();

  println!("Part 1: {}", total);
  println!("Part 2: {}", discount);
}

fn raw_successors((x, y): (i32, i32)) -> impl Iterator<Item = (i32, i32)> {
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)].into_iter()
}

fn combine_edges(edges: HashSet<&(i32, i32)>) -> usize {
  let mut claimed: HashSet<(i32, i32)> = HashSet::new();
  let mut sides = 0;

  for edge in edges.iter() {
    if claimed.contains(edge) {
      continue;
    }
    claimed.extend(bfs_reach(**edge, |p| {
      raw_successors(*p).filter(|c| edges.contains(c))
    }));
    sides += 1;
  }

  sides
}
