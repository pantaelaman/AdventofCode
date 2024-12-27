#![feature(ascii_char)]
#![feature(slice_split_once)]
use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;
use rand::{rngs::OsRng, seq::SliceRandom};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let (raw_replacements, start) = lines.split_once(|e| e.is_empty()).unwrap();
  let replacements: HashMap<&str, HashSet<&str>> = raw_replacements
    .iter()
    .map(|l| l.split_once(" => ").unwrap())
    .fold(HashMap::new(), |mut acc, (src, dest)| {
      acc.entry(src).or_default().insert(dest);
      acc
    });

  println!("Part 1: {}", replace(&start[0], &replacements).len());

  let inverted = invert_replacements(&replacements);
  let inverted_rules = inverted
    .into_iter()
    .flat_map(|(src, dests)| dests.into_iter().map(move |dest| (src, dest)))
    .collect_vec();
  let steps = fabricate_molecule(&start[0], &inverted_rules);

  println!("Part 2: {steps}");
}

fn replace<'k>(
  start: &String,
  replacements: &HashMap<&'k str, HashSet<&'k str>>,
) -> HashSet<String> {
  let mut possibilites = HashSet::new();
  for (src, dest) in replacements
    .iter()
    .flat_map(|(k, vs)| vs.iter().map(move |v| (k, v)))
  {
    for (idx, _) in start.match_indices(src) {
      let (front, back) = start.split_at(idx);
      possibilites
        .insert(front.to_owned() + dest + back.strip_prefix(src).unwrap());
    }
  }
  possibilites
}

fn fabricate_molecule<'k>(
  start: &String,
  replacements: &Vec<(&'k str, &'k str)>,
) -> usize {
  loop {
    let mut rules = replacements.clone();
    let mut st = start.clone();
    rules.shuffle(&mut OsRng);
    let mut steps = 0;
    for k in 0..rules.len() {
      for l in 0..=k {
        let (src, dest) = rules[k - l];
        steps += st.matches(src).count();
        st = st.replace(src, dest);
      }
      if st == "e" {
        return steps;
      }
    }
  }
}

fn invert_replacements<'k>(
  replacements: &HashMap<&'k str, HashSet<&'k str>>,
) -> HashMap<&'k str, HashSet<&'k str>> {
  replacements
    .iter()
    .flat_map(|(k, vs)| vs.iter().map(move |v| (k, v)))
    .fold(HashMap::new(), |mut acc, (dest, src)| {
      acc.entry(src).or_default().insert(dest);
      acc
    })
}
