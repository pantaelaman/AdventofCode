use std::io::stdin;

use aocutil::Point;
use itertools::Itertools;
use pathfinding::prelude::{astar, dijkstra_reach};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let favourite: i32 = lines[0].parse().unwrap();

  let target = Point::new(31, 39);

  let (_, cost) = astar(
    &Point::new(1, 1),
    |point| successors(*point, favourite),
    |point| point.manhattan(target),
    |point| point == &target,
  )
  .unwrap();

  println!("Part 1: {}", cost);

  let reachable = dijkstra_reach(&Point::new(1, 1), |point, cost| {
    if cost == 50 {
      return Vec::new();
    }
    successors(*point, favourite).collect_vec()
  });

  println!("Part 2: {}", reachable.count());
}

fn successors(
  point: Point<i32>,
  favourite: i32,
) -> impl Iterator<Item = (Point<i32>, i32)> {
  point
    .orthogonal_neighbours()
    .filter(|p| p.x >= 0 && p.y >= 0)
    .filter(move |p| {
      let poly = p.x * p.x + 3 * p.x + 2 * p.x * p.y + p.y + p.y * p.y;
      (poly + favourite).count_ones() % 2 == 0
    })
    .map(|p| (p, 1))
}
