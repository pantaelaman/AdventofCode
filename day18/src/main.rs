use std::{
  collections::HashSet,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use pathfinding::prelude::astar;

const EXTENTS: i32 = 70;

type Position = (i32, i32);

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let bytes: Vec<Position> = lines
    .iter()
    .map(|l| {
      l.split(',')
        .map(|p| p.parse::<i32>().unwrap())
        .collect_tuple()
        .unwrap()
    })
    .collect();

  let mut walls = bytes
    .iter()
    .copied()
    .take(1024)
    .collect::<HashSet<Position>>();

  let heuristic = |(x, y): &Position| EXTENTS - x + EXTENTS - y;
  let success = |(x, y): &Position| x == &EXTENTS && y == &EXTENTS;
  let (_, cost) =
    astar(&(0, 0), |p| successors(p, &walls), heuristic, success).unwrap();

  println!("Part 1: {cost}");

  let mut bytes = bytes.iter().copied().skip(1024);
  let breaking_byte = loop {
    let new_byte = bytes.next().unwrap();
    walls.insert(new_byte);
    if astar(&(0, 0), |p| successors(p, &walls), heuristic, success).is_none() {
      break new_byte;
    }
  };

  println!("Part 2: {breaking_byte:?}");
}

fn successors<'w>(
  (x, y): &Position,
  walls: &'w HashSet<Position>,
) -> impl IntoIterator<Item = (Position, i32)> + use<'w> {
  [(*x + 1, *y), (*x - 1, *y), (*x, *y + 1), (*x, *y - 1)]
    .into_iter()
    .filter(|p| {
      p.0 >= 0
        && p.0 <= EXTENTS
        && p.1 >= 0
        && p.1 <= EXTENTS
        && !walls.contains(p)
    })
    .map(|p| (p, 1))
}
