use itertools::Itertools;
use pathfinding::directed::dijkstra::dijkstra;
use regex::Regex;
use std::{collections::BTreeMap, fs::File, io::Read};

struct Machine {
  a: (i64, i64),
  b: (i64, i64),
  target: (i64, i64),
}

impl Machine {
  pub fn solve_p1(&self) -> usize {
    self.solve_target(self.target) as usize
  }

  fn solve_target(&self, (tx, ty): (i64, i64)) -> i64 {
    let b_num = (tx * self.a.1) - (ty * self.a.0);
    let b_den = (self.b.0 * self.a.1) - (self.a.0 * self.b.1);
    let a_num = (tx * self.b.1) - (ty * self.b.0);
    let a_den = (self.a.0 * self.b.1) - (self.b.0 * self.a.1);

    if b_num % b_den != 0 || a_num % a_den != 0 {
      0
    } else {
      3 * (a_num / a_den) + (b_num / b_den)
    }
  }

  pub fn solve_p2(&self) -> usize {
    self.solve_target((
      self.target.0 + 10000000000000,
      self.target.1 + 10000000000000,
    )) as usize
  }
}

fn main() {
  let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let mut buf = String::new();
  file.read_to_string(&mut buf).unwrap();

  let machine_regex = Regex::new(r"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)").unwrap();
  let machines = buf
    .split("\n\n")
    .map(|machine| {
      let caps = machine_regex.captures(machine).unwrap();
      let mut nums = caps
        .iter()
        .skip(1)
        .map(|s| s.unwrap().as_str().parse::<i64>().unwrap());
      Machine {
        a: (nums.next().unwrap(), nums.next().unwrap()),
        b: (nums.next().unwrap(), nums.next().unwrap()),
        target: (nums.next().unwrap(), nums.next().unwrap()),
      }
    })
    .collect_vec();

  let total_p1 = machines.iter().map(Machine::solve_p1).sum::<usize>();
  let total_p2 = machines.iter().map(Machine::solve_p2).sum::<usize>();
  println!("Part 1: {}", total_p1);
  println!("Part 2: {}", total_p2);
}
