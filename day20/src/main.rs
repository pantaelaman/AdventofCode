use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use aocutil::{grid_input, Point};
use itertools::Itertools;
use pathfinding::prelude::dijkstra_reach;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let mut walls: HashSet<Point<i32>> = HashSet::new();
  let mut start = Point::default();
  for (p, c) in grid_input::<_, usize, usize>(lines.iter().map(|l| l.as_str()))
  {
    let p: Point<usize> = p;
    let p = Point {
      x: p.x as i32,
      y: p.y as i32,
    };
    match c {
      '#' => {
        walls.insert(p);
      }
      'S' => {
        start = p;
      }
      _ => {}
    }
  }

  let weight_map = dijkstra_reach(&start, |p, _| {
    p.orthogonal_neighbours()
      .filter(|p| !walls.contains(p))
      .map(|p| (p, 1))
  })
  .into_iter()
  .map(|r| (r.node, r.total_cost))
  .collect::<HashMap<Point<i32>, usize>>();

  let (p1, p2) = weight_map.iter().tuple_combinations().fold(
    (0, 0),
    |(p1, p2), ((a, wa), (b, wb))| {
      let raw_dist = a.x.abs_diff(b.x) + a.y.abs_diff(b.y);
      let weight_dist = wa.abs_diff(*wb);

      if weight_dist - raw_dist as usize >= 100 {
        (
          p1 + (raw_dist <= 2) as usize,
          p2 + (raw_dist <= 20) as usize,
        )
      } else {
        (p1, p2)
      }
    },
  );

  println!("Part 1: {p1}");
  println!("Part 2: {p2}");
}
