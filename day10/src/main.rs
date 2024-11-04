use std::{
  cmp::{max, min},
  collections::HashSet,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

struct Point {
  initial_pos: (i64, i64),
  velocity: (i64, i64),
}

impl Point {
  pub fn calc_pos(&self, time: i64) -> (i64, i64) {
    (
      self.initial_pos.0 + (self.velocity.0 * time),
      self.initial_pos.1 + (self.velocity.1 * time),
    )
  }
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let regex = Regex::new(
    r"position=<\s*(\-?\d+),\s*(\-?\d+)> velocity=<\s*(\-?\d+),\s*(\-?\d+)>",
  )
  .unwrap();

  let points = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();
      let caps = regex.captures(&line).unwrap();
      Point {
        initial_pos: (caps[1].parse().unwrap(), caps[2].parse().unwrap()),
        velocity: (caps[3].parse().unwrap(), caps[4].parse().unwrap()),
      }
    })
    .collect_vec();

  let (smallest_time, (minx, maxx, miny, maxy)) = (0..20000)
    .map(|i| {
      let bounds = points.iter().map(|p| p.calc_pos(i)).fold(
        (i64::MAX, i64::MIN, i64::MAX, i64::MIN),
        |acc, (x, y)| {
          (min(acc.0, x), max(acc.1, x), min(acc.2, y), max(acc.3, y))
        },
      );
      (i, bounds)
    })
    .min_by_key(|(_, (minx, maxx, miny, maxy))| {
      (maxx - minx).checked_mul((maxy - miny)).unwrap_or(i64::MAX)
    })
    .unwrap();

  let points: HashSet<(i64, i64)> =
    points.iter().map(|p| p.calc_pos(smallest_time)).collect();

  println!("Part 1:");
  for y in 0..=(maxy - miny) {
    for x in 0..=(maxx - minx) {
      if points.contains(&(x + minx, y + miny)) {
        print!("#");
      } else {
        print!(" ");
      }
    }
    println!();
  }

  println!("Part 2: {}", smallest_time);
}
