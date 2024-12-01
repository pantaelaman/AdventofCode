use itertools::Itertools;
use std::{
  collections::{HashMap, HashSet},
  env::args,
  fs::File,
  io::{BufRead, BufReader},
};

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  pub fn new(x: i32, y: i32) -> Self {
    Point { x, y }
  }

  pub fn manhattan(&self, other: Point) -> u32 {
    self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
  }
}

impl std::ops::Add for Point {
  type Output = Point;
  fn add(self, rhs: Self) -> Self::Output {
    Point {
      x: self.x + rhs.x,
      y: self.y + rhs.y,
    }
  }
}

impl std::ops::Mul<Point> for i32 {
  type Output = Point;
  fn mul(self, rhs: Point) -> Self::Output {
    Point {
      x: rhs.x * self,
      y: rhs.y * self,
    }
  }
}

struct Line {
  start: Point,
  distance_offset: u32,
  length: u32,
  consumed_length: u32,
  direction: Point,
}

impl Line {
  pub fn new(
    start: Point,
    distance_offset: u32,
    length: u32,
    direction: char,
  ) -> Self {
    let direction = match direction {
      'U' => Point::new(0, 1),
      'D' => Point::new(0, -1),
      'L' => Point::new(-1, 0),
      'R' => Point::new(1, 0),
      _ => unimplemented!(),
    };

    Line {
      start,
      distance_offset,
      length,
      consumed_length: 0,
      direction,
    }
  }

  pub fn last(&self) -> Point {
    self.start + (self.length as i32 * self.direction)
  }
}

impl Iterator for Line {
  type Item = (Point, u32);

  fn next(&mut self) -> Option<Self::Item> {
    self.consumed_length += 1;
    if self.consumed_length <= self.length {
      Some((
        self.start + (self.consumed_length as i32 * self.direction),
        self.distance_offset + self.consumed_length,
      ))
    } else {
      None
    }
  }
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);
  let (first, second) = reader
    .lines()
    .map(|line| {
      let mut position = Point::new(0, 0);
      let mut distance_offset = 0;
      line
        .unwrap()
        .split(',')
        .map(|cmd| {
          let (direction, length) = cmd.split_at(1);
          let length = length.parse().unwrap();
          let line = Line::new(
            position,
            distance_offset,
            length,
            direction.chars().next().unwrap(),
          );
          position = Line::last(&line);
          distance_offset += length;
          line
        })
        .flatten()
        .collect::<HashMap<Point, u32>>()
    })
    .collect_tuple()
    .unwrap();

  let min_manhattan = first
    .keys()
    .filter(|k| second.contains_key(k))
    .map(|point| point.manhattan(Point::new(0, 0)))
    .min()
    .unwrap();

  let min_steps = first
    .iter()
    .filter_map(|(k, v)| {
      second.contains_key(k).then(|| second.get(k).unwrap() + v)
    })
    .min()
    .unwrap();

  println!("Part 1: {}", min_manhattan);
  println!("Part 2: {}", min_steps);
}
