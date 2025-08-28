use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use glam::{IVec3, IVec4};
use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let starting_cubes =
    aocutil::grid_input::<_, usize>(lines.iter().map(|s| s.as_str()))
      .filter_map(|(point, c)| {
        (c == '#').then_some(IVec3::new(point.x as i32, point.y as i32, 0))
      })
      .collect::<HashSet<IVec3>>();

  let mut cubes = starting_cubes.clone();

  for _ in 0..6 {
    run_cycle(&mut cubes, dim3_neighbours);
  }

  println!("Part 1: {}", cubes.len());

  let mut hcubes = starting_cubes.iter().map(|cube| cube.extend(0)).collect();

  for _ in 0..6 {
    run_cycle(&mut hcubes, dim4_neighbours);
  }

  println!("Part 2: {}", hcubes.len());
}

fn dim3_neighbours(cube: &IVec3) -> impl Iterator<Item = IVec3> {
  let cube = *cube;
  [const { -1..=1 }; 3]
    .into_iter()
    .multi_cartesian_product()
    .map(move |d| cube + IVec3::new(d[0], d[1], d[2]))
}

fn dim4_neighbours(hcube: &IVec4) -> impl Iterator<Item = IVec4> {
  let hcube = *hcube;
  [const { -1..=1 }; 4]
    .into_iter()
    .multi_cartesian_product()
    .map(move |d| hcube + IVec4::new(d[0], d[1], d[2], d[3]))
}

fn run_cycle<T, F, I>(cubes: &mut HashSet<T>, get_neighbours: F)
where
  T: std::hash::Hash + PartialEq + Eq + Copy,
  F: Fn(&T) -> I,
  I: Iterator<Item = T>,
{
  let mut next_cubes = HashSet::new();
  let mut neighbour_graph: HashMap<T, usize> = HashMap::new();

  for cube in cubes.iter() {
    let active_neighbours = get_neighbours(cube)
      .filter(|n| {
        if cubes.contains(n) {
          true
        } else {
          *neighbour_graph.entry(*n).or_default() += 1;
          false
        }
      })
      .count()
      - 1;

    if active_neighbours == 2 || active_neighbours == 3 {
      // keep it active
      next_cubes.insert(*cube);
    }
  }

  for cube in neighbour_graph
    .iter()
    .filter_map(|(n, v)| (*v == 3).then_some(n))
  {
    next_cubes.insert(*cube);
  }

  *cubes = next_cubes;
}
