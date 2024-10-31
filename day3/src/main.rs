use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader, Write},
};

use itertools::Itertools;
use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Claim {
  x: i32,
  y: i32,
  w: i32,
  h: i32,
}

impl Claim {
  fn get_constituents(&self) -> Vec<(i32, i32)> {
    (self.x..(self.x + self.w))
      .map(|x| (self.y..(self.y + self.h)).map(move |y| (x, y)))
      .flatten()
      .collect()
  }
}

fn main() {
  let file = File::open(
    std::env::args()
      .skip(1)
      .next()
      .expect("Needs input filename"),
  )
  .unwrap();
  let reader = BufReader::new(file);
  let mut ofile = File::create("doubles.txt").unwrap();

  let regex = Regex::new(r"\#\d+ @ (\d+),(\d+): (\d+)x(\d+)").unwrap();

  let claims = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();

      let caps = regex.captures(&line).expect("Malformed input");

      Claim {
        x: caps[1].parse().unwrap(),
        y: caps[2].parse().unwrap(),
        w: caps[3].parse().unwrap(),
        h: caps[4].parse().unwrap(),
      }
    })
    .collect_vec();

  let mut claimed_squares: HashMap<(i32, i32), u32> = HashMap::new();

  for claim in claims.iter() {
    for constituent in claim.get_constituents() {
      if let Some(v) = claimed_squares.get_mut(&constituent) {
        *v += 1;
      } else {
        claimed_squares.insert(constituent, 1);
      }
    }
  }

  let mut p2: Option<usize> = None;

  'outer: for (i, claim) in claims.iter().enumerate() {
    for constituent in claim.get_constituents() {
      if claimed_squares.get(&constituent) != Some(&1) {
        continue 'outer;
      }
    }
    p2 = Some(i + 1);
    break;
  }

  println!(
    "Part 1: {}",
    claimed_squares.values().filter(|v| **v > 1).count()
  );
  println!("Part 2: {}", p2.unwrap());
}
