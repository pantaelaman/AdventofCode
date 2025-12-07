use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let start = lines[0].chars().position(|c| c == 'S').unwrap();
  let splitters = lines[1..]
    .iter()
    .map(|line| {
      line
        .chars()
        .enumerate()
        .filter_map(|(i, c)| (c == '^').then_some(i))
        .collect::<HashSet<usize>>()
    })
    .collect_vec();

  let mut beams = HashMap::new();
  beams.insert(start, 1);

  let mut splits = 0;

  for row in splitters {
    if row.is_empty() {
      continue;
    }

    beams =
      beams
        .into_iter()
        .fold(HashMap::new(), |mut map, (beam, timelines)| {
          if row.contains(&beam) {
            splits += 1;
            *map.entry(beam + 1).or_default() += timelines;
            *map.entry(beam - 1).or_default() += timelines;
          } else {
            *map.entry(beam).or_default() += timelines;
          }

          map
        });
  }

  println!("Part 1: {}", splits);
  println!("Part 2: {}", beams.values().sum::<usize>());
}
