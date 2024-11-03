use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

fn char_to_idx(c: char) -> usize {
  c as usize - b'A' as usize
}

fn idx_to_char(i: usize) -> char {
  (i as u8 + b'A') as char
}

#[inline]
fn idx_to_duration(i: usize) -> usize {
  61 + i
}

const LETTERS: usize = 26;

fn main() {
  let file =
    File::open(std::env::args().nth(1).expect("Missing input file")).unwrap();
  let reader = BufReader::new(file);

  let regex =
    Regex::new(r"Step (\w) must be finished before step (\w) can begin.")
      .unwrap();

  let mut dependencies: [Vec<usize>; LETTERS] = Default::default();
  let mut dependents: [Vec<usize>; LETTERS] = Default::default();
  for line in reader.lines() {
    let line = line.unwrap();

    let caps = regex.captures(&line).unwrap();
    let required_step = char_to_idx(caps[1].chars().exactly_one().unwrap());
    let target_step = char_to_idx(caps[2].chars().exactly_one().unwrap());

    dependencies[target_step].push(required_step);
    dependents[required_step].push(target_step);
  }

  let mut finished: Vec<usize> = Vec::new();
  while finished.len() < LETTERS {
    finished.push(
      dependencies
        .iter()
        .enumerate()
        .filter(|(i, deps)| {
          !finished.contains(&i) && deps.iter().all(|n| finished.contains(&n))
        })
        .next()
        .unwrap()
        .0,
    );
  }
  let order = finished.into_iter().map(idx_to_char).collect::<String>();
  println!("Part 1: {}", order);

  let mut total_dependent_weight: [usize; LETTERS] = [0; LETTERS];
  for i in 0..LETTERS {
    let mut dependent_queue: Vec<usize> = Vec::new();
    dependent_queue.extend(dependents[i].iter());
    while let Some(dependent) = dependent_queue.pop() {
      total_dependent_weight[i] += 1;
      dependent_queue.extend(dependents[dependent].iter());
    }
  }

  let mut free_workers = 5;
  let mut timeline: HashMap<usize, Vec<usize>> = HashMap::new();
  let mut finished: Vec<usize> = Vec::new();
  let mut unfinished: HashSet<usize> = (0..LETTERS).collect();
  let mut time = 0;
  while finished.len() < LETTERS {
    if let Some(finishing) = timeline.get(&time) {
      finished.extend(finishing.iter());
      free_workers += finishing.len();
    }
    if free_workers == 0 {
      time += 1;
      continue;
    }
    let mut upcoming = unfinished
      .iter()
      .filter(|i| dependencies[**i].iter().all(|i| finished.contains(i)))
      .copied()
      .collect_vec();
    upcoming.sort_by_key(|i| idx_to_duration(*i));
    upcoming.reverse();
    for _ in 0..free_workers {
      if let Some(target) = upcoming.pop() {
        timeline
          .entry(time + idx_to_duration(target))
          .or_insert_with(Default::default)
          .push(target);
        unfinished.remove(&target);
        free_workers -= 1;
      } else {
        break;
      }
    }
    time += 1;
  }
  println!("Part 2: {:?}", time - 1);
}
