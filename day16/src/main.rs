use std::{
  collections::HashSet,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use pathfinding::prelude::{dijkstra, yen};

type Position = (i32, i32);

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let mut start = (0, 0);
  let mut end = (0, 0);
  let mut walls: HashSet<Position> = HashSet::new();

  for (p, c) in lines.iter().enumerate().flat_map(|(y, line)| {
    line
      .chars()
      .enumerate()
      .map(move |(x, c)| ((x as i32, y as i32), c))
  }) {
    match c {
      '#' => {
        walls.insert(p);
      }
      'S' => {
        start = p;
      }
      'E' => {
        end = p;
      }
      _ => {}
    }
  }

  let successors = |(p, facing): &(Position, Position)| {
    let pos = *p;
    [
      (*facing, 1),
      (clockwise(*facing), 1001),
      (counterclockwise(*facing), 1001),
    ]
    .into_iter()
    .map(move |(facing, cost)| ((apply_facing(pos, facing), facing), cost))
    .filter(|((p, _), _)| !walls.contains(p))
  };
  let success = |(p, _): &(Position, Position)| p == &end;

  let (_, min_cost) = dijkstra(&(start, (1, 0)), successors, success).unwrap();

  println!("Part 1: {min_cost}");

  let k_shortest = yen(&(start, (1, 0)), successors, success, 10);
  let num_unique = k_shortest
    .into_iter()
    .take_while(|(_, cost)| cost == &min_cost)
    .flat_map(|(p, _)| p.into_iter())
    .map(|(p, _)| p)
    .unique()
    .count();

  println!("Part 2: {num_unique}");
}

fn apply_facing((x, y): Position, (dx, dy): Position) -> Position {
  (x + dx, y + dy)
}
fn clockwise((dx, dy): Position) -> Position {
  (-dy, dx)
}
fn counterclockwise((dx, dy): Position) -> Position {
  (dy, -dx)
}
