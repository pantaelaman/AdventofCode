use std::{
  collections::{vec_deque, HashMap, VecDeque},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::{repeat_n, Itertools};
use regex::Regex;

type Pattern = (bool, bool, bool, bool, bool);

fn char_to_bool(c: char) -> bool {
  match c {
    '.' => false,
    '#' => true,
    _ => unreachable!(),
  }
}

fn bool_to_char(b: bool) -> char {
  if b {
    '#'
  } else {
    '.'
  }
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);
  let mut lines = reader.lines();

  let initial_regex = Regex::new(r"initial state: ([\.\#]+)").unwrap();
  let pattern_regex = Regex::new(r"([\.\#]{5}) => ([\.\#])").unwrap();

  let first_line = lines.next().unwrap().unwrap();
  let caps = initial_regex.captures(&first_line).unwrap();

  let mut pots: VecDeque<bool> = caps[1].chars().map(char_to_bool).collect();
  let patterns: HashMap<Pattern, bool> = lines
    .skip(1)
    .map(|line| {
      let line = line.unwrap();
      let caps = pattern_regex.captures(&line).unwrap();
      (
        caps[1].chars().map(char_to_bool).collect_tuple().unwrap(),
        caps[2].chars().map(char_to_bool).exactly_one().unwrap(),
      )
    })
    .collect();

  let mut front = 0;
  let mut linear_convergence: VecDeque<i64> = VecDeque::new(); // please please please

  const TARGET_GENS: usize = 50000000000;
  let mut score = 0;
  for i in 0..TARGET_GENS {
    let next_pots = [false; 4]
      .iter()
      .chain(pots.iter())
      .chain([false; 4].iter())
      .copied()
      .tuple_windows()
      .map(|pattern| patterns.get(&pattern).copied().unwrap_or_default())
      .collect();
    pots = next_pots;
    front -= 2;
    // cull excess to save memory and time
    while let Some(false) = pots.front() {
      pots.pop_front();
      front += 1;
    }
    while let Some(false) = pots.back() {
      pots.pop_back();
    }

    score = pots
      .iter()
      .enumerate()
      .filter_map(|(i, p)| p.then_some(i as i64 + front))
      .sum::<i64>();
    if i == 19 {
      println!("Part 1: {}", score);
    }
    linear_convergence.push_back(score);
    if linear_convergence.len() <= 50 {
      continue;
    }
    linear_convergence.pop_front();
    if let Ok(slope) = linear_convergence
      .iter()
      .tuple_windows()
      .map(|(v1, v2)| v2 - v1)
      .all_equal_value()
    {
      score += (TARGET_GENS - i - 1) as i64 * slope;
      break;
    }
  }

  println!("Part 2: {}", score);
}
