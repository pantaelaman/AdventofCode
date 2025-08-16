use std::{collections::HashMap, io::stdin, iter::successors};

use aocutil::{grid_input, Point};
use itertools::Itertools;

#[derive(Clone, Copy)]
enum Seat {
  Empty,
  Occupied,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let mut grid: HashMap<Point<i32>, Option<Seat>> =
    grid_input::<_, usize>(lines.iter().map(|s| s.as_str()))
      .filter_map(|(point, c)| {
        let val = match c {
          '.' => None,
          'L' => Some(Seat::Empty),
          '#' => Some(Seat::Occupied),
          _ => unreachable!(),
        };
        Some((point.map(|v| v as i32), val))
      })
      .collect();

  let mut visible_neighbours: HashMap<Point<i32>, Vec<Point<i32>>> =
    HashMap::new();
  for pos in grid.iter().filter_map(|(k, v)| v.map(|_| k)) {
    for direction in Point::default().all_neighbours() {
      if let Some(neighbour) =
        successors(Some(*pos), |prev| Some(*prev + direction))
          .skip(1)
          .map_while(|loc| grid.get(&loc).map(|v| (v, loc)))
          .find_map(|(seat, loc)| seat.map(|_| loc))
      {
        visible_neighbours.entry(*pos).or_default().push(neighbour);
      }
    }
  }

  println!(
    "Part 1: {}",
    run_sim(grid.clone(), Point::all_neighbours, 4)
  );
  println!(
    "Part 2: {}",
    run_sim(
      grid.clone(),
      |point| {
        visible_neighbours
          .get(&point)
          .map(|v| v.into_iter().copied())
          .unwrap_or_default()
      },
      5
    )
  );
}

fn run_sim<F, I>(
  mut grid: HashMap<Point<i32>, Option<Seat>>,
  neighbours: F,
  leave_threshold: usize,
) -> usize
where
  F: Fn(Point<i32>) -> I,
  I: IntoIterator<Item = Point<i32>>,
{
  let mut next_grid = HashMap::new();

  loop {
    next_grid.clear();

    let mut any_changes = false;
    for (point, seat) in
      grid.iter().filter_map(|(k, seat)| seat.map(|v| (k, v)))
    {
      let occupied_neighbours = neighbours(*point)
        .into_iter()
        .filter(|neighbour| {
          grid
            .get(&neighbour)
            .copied()
            .flatten()
            .is_some_and(|nb| matches!(nb, Seat::Occupied))
        })
        .count();

      let next_seat = if matches!(seat, Seat::Empty) && occupied_neighbours == 0
      {
        any_changes = true;
        Seat::Occupied
      } else if matches!(seat, Seat::Occupied)
        && occupied_neighbours >= leave_threshold
      {
        any_changes = true;
        Seat::Empty
      } else {
        seat
      };

      next_grid.insert(*point, Some(next_seat));
    }

    if !any_changes {
      break;
    }

    std::mem::swap(&mut grid, &mut next_grid);
  }

  let occupied_seats = grid
    .values()
    .filter(|seat| matches!(seat, Some(Seat::Occupied)))
    .count();

  occupied_seats
}
