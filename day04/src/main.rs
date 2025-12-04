use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use aocutil::{grid_input, Point};
use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let rolls: HashSet<Point<i32>> = grid_input(lines.iter().map(|s| s.as_str()))
    .filter_map(|(p, c): (Point<usize>, _)| {
      (c == '@').then_some(p.map(|p| p as i32))
    })
    .collect();

  let rolls: HashMap<Point<i32>, usize> = rolls
    .iter()
    .map(|point| {
      (
        *point,
        point.all_neighbours().filter(|n| rolls.contains(n)).count(),
      )
    })
    .collect();

  let accessible: HashSet<_> = rolls
    .iter()
    .filter_map(|(p, c)| (*c < 4).then_some(*p))
    .collect();

  println!("Part 1: {}", accessible.len());

  let num_starting = rolls.len();
  let mut rolls = rolls;
  let mut accessible = accessible;

  while accessible.len() > 0 {
    rolls.retain(|p, _| !accessible.contains(p));

    accessible = accessible
      .into_iter()
      .flat_map(|p| p.all_neighbours())
      .filter(|adjacent| {
        let Some(roll) = rolls.get_mut(&adjacent) else {
          return false;
        };

        *roll -= 1;
        *roll < 4
      })
      .collect();
  }

  println!("Part 2: {}", num_starting - rolls.len());
}
