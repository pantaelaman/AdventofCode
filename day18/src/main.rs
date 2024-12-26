use std::{collections::HashSet, io::stdin};

use aocutil::{grid_input, Point};
use itertools::{iterate, Itertools};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let init: HashSet<Point<i32>> = grid_input(lines.iter().map(|s| s.as_str()))
    .filter_map(|(p, c): (Point<usize>, char)| {
      (c == '#').then_some(p.map(|v| v as i32))
    })
    .collect();
  let mut conway = iterate(init.clone(), run_step);
  println!("Part 1: {}", conway.nth(100).unwrap().len());
  let mut conway_corners = iterate(init.clone(), run_step_corners);
  println!("Part 2: {}", conway_corners.nth(100).unwrap().len());
}

fn run_step(grid: &HashSet<Point<i32>>) -> HashSet<Point<i32>> {
  (0..100)
    .cartesian_product(0..100)
    .map(Into::<Point<i32>>::into)
    .filter(|point| {
      let live_neighbours =
        point.all_neighbours().filter(|n| grid.contains(n)).count();
      if grid.contains(point) {
        live_neighbours == 2 || live_neighbours == 3
      } else {
        live_neighbours == 3
      }
    })
    .collect()
}

fn run_step_corners(grid: &HashSet<Point<i32>>) -> HashSet<Point<i32>> {
  let mut set = run_step(grid);
  set.insert(Point::new(0, 0));
  set.insert(Point::new(99, 0));
  set.insert(Point::new(0, 99));
  set.insert(Point::new(99, 99));
  set
}
