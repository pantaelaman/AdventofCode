#![feature(pattern)]
use std::{
  collections::{HashMap, HashSet},
  io::{stdin, BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let reader = BufReader::new(stdin());

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let mappings = lines
    .iter()
    .map(|line| line.split("-").collect_tuple().unwrap())
    .fold(
      HashMap::<&str, HashSet<&str>>::new(),
      |mut acc, (c1, c2)| {
        acc.entry(c1).or_default().insert(c2);
        acc.entry(c2).or_default().insert(c1);
        acc
      },
    );

  let num_matching_trios = mappings
    .iter()
    .combinations(3)
    .filter(|cs| {
      cs.iter().any(|(c, _)| c.chars().nth(0).unwrap() == 't')
        && cs
          .into_iter()
          .tuple_combinations()
          .all(|((c1, _), (_, c2s))| c2s.contains(*c1))
    })
    .count();

  println!("Part 1: {num_matching_trios}");

  let max_clique = bron_kerbosch(
    HashSet::new(),
    mappings.keys().copied().collect(),
    HashSet::new(),
    &mappings,
  )
  .unwrap();

  println!("Part 2: {}", max_clique.into_iter().sorted().join(","));
}

fn bron_kerbosch<'a>(
  r: HashSet<&'a str>,
  p: HashSet<&'a str>,
  mut x: HashSet<&'a str>,
  mapping: &HashMap<&'a str, HashSet<&'a str>>,
) -> Option<HashSet<&'a str>> {
  if p.is_empty() && x.is_empty() {
    return Some(r);
  }

  let mut max: Option<HashSet<&str>> = None;

  let pivot = p.union(&x).next().unwrap();
  let pivot_neighbours = mapping.get(pivot).unwrap();

  for v in p.difference(pivot_neighbours) {
    let mut next_r = r.clone();
    next_r.insert(*v);
    let neighbours = mapping.get(v).unwrap();
    let next_p = p.intersection(neighbours).copied().collect();
    let next_x = x.intersection(neighbours).copied().collect();
    if let Some(clique) = bron_kerbosch(next_r, next_p, next_x, mapping) {
      if let Some(ref prev_max) = max {
        if prev_max.len() < clique.len() {
          max = Some(clique);
        }
      } else {
        max = Some(clique);
      }
    }
    x.insert(v);
  }

  max
}
