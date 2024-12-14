use itertools::Itertools;
use regex::Regex;
use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

const WIDTH: i32 = 101;
const HEIGHT: i32 = 103;
//const WIDTH: i32 = 11;
//const HEIGHT: i32 = 7;

struct Robot {
  position: (i32, i32),
  velocity: (i32, i32),
}

impl Robot {
  fn step(&mut self) {
    self.position.0 = (self.position.0 + self.velocity.0).rem_euclid(WIDTH);
    self.position.1 = (self.position.1 + self.velocity.1).rem_euclid(HEIGHT);
  }
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let regex = Regex::new(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)").unwrap();

  let mut robots = lines
    .iter()
    .map(|line| {
      let caps_iter = regex.captures(&line).unwrap();
      let mut caps = caps_iter
        .iter()
        .skip(1)
        .map(|s| s.unwrap().as_str().parse::<i32>().unwrap());
      Robot {
        position: (caps.next().unwrap(), caps.next().unwrap()),
        velocity: (caps.next().unwrap(), caps.next().unwrap()),
      }
    })
    .collect_vec();

  let mut quadrants: HashMap<(bool, bool), usize> = HashMap::new();
  const ITERS: i32 = 100;
  for Robot {
    position: (x, y),
    velocity: (dx, dy),
  } in robots.iter()
  {
    let fx = (x + (dx * ITERS)).rem_euclid(WIDTH);
    let fy = (y + (dy * ITERS)).rem_euclid(HEIGHT);
    if fx == WIDTH / 2 || fy == HEIGHT / 2 {
      continue;
    }
    *quadrants
      .entry((fx < WIDTH / 2, fy < HEIGHT / 2))
      .or_default() += 1;
  }

  let mut i = 0;
  loop {
    i += 1;
    let mut seen = HashSet::new();
    for robot in robots.iter_mut() {
      robot.step();
      seen.insert(robot.position);
    }
    if seen.len() == robots.len() {
      println!("{i}:\n");
      for y in 0..HEIGHT {
        for x in 0..WIDTH {
          print!(
            "{}",
            if robots
              .iter()
              .find(|r| r.position == (x as i32, y as i32))
              .is_some()
            {
              '#'
            } else {
              '.'
            }
          );
        }
        println!();
      }
      break;
    }
  }

  let total = quadrants.values().product::<usize>();
  println!("Part 1: {total}");
  println!("Part 2: {i}");
}
