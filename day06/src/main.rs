use std::{
  collections::HashSet,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  let size = (lines[0].len(), lines.len());

  let mut guard = (0, 0);
  let mut facing = (0, -1);
  let grid: HashSet<(usize, usize)> = lines
    .iter()
    .enumerate()
    .map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .filter_map(|(x, c)| match c {
          '^' => {
            guard = (x, y);
            None
          }
          '.' => None,
          _ => Some((x, y)),
        })
        .collect()
    })
    .concat();

  let starting_guard = guard;

  let mut visited: HashSet<(usize, usize)> = HashSet::new();
  visited.insert(guard);
  while let Some(target) = apply(guard, facing, size) {
    if grid.contains(&target) {
      facing = (-facing.1, facing.0);
    } else {
      visited.insert(target);
      guard = target;
    }
  }

  println!("Part 1: {}", visited.len());

  let possible = (0..size.0)
    .cartesian_product(0..size.1)
    .filter(|p| !grid.contains(p))
    .filter(|p| {
      guard = starting_guard;
      facing = (0, -1);
      let mut visited: HashSet<((usize, usize), (isize, isize))> =
        HashSet::new();
      visited.insert((guard, facing));
      while let Some(target) = apply(guard, facing, size) {
        if grid.contains(&target) || &target == p {
          facing = (-facing.1, facing.0);
        } else {
          if !visited.insert((target, facing)) {
            return true;
          }
          guard = target;
        }
      }
      false
    })
    .count();

  println!("Part 2: {}", possible);
}

fn apply(
  (x, y): (usize, usize),
  (dx, dy): (isize, isize),
  (sx, sy): (usize, usize),
) -> Option<(usize, usize)> {
  x.checked_add_signed(dx)
    .and_then(|x| y.checked_add_signed(dy).map(|y| (x, y)))
    .and_then(|(x, y)| (x < sx && y < sy).then_some((x, y)))
}
