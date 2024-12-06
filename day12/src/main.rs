use std::{
  collections::HashSet,
  fs::File,
  io::{BufRead, BufReader},
};

use gcd::Gcd;
use itertools::Itertools;
use regex::Regex;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let regex = Regex::new(r"\<x=(-?\d+), y=(-?\d+), z=(-?\d+)\>").unwrap();

  let mut moons: Vec<((i32, i32, i32), (i32, i32, i32))> = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();
      (
        regex
          .captures(&line)
          .unwrap()
          .iter()
          .skip(1)
          .map(|c| c.unwrap().as_str().parse::<i32>().unwrap())
          .collect_tuple()
          .unwrap(),
        (0, 0, 0),
      )
    })
    .collect_vec();

  let x_len = find_loop_length_by_key(&moons, |(x, _, _)| *x);
  let y_len = find_loop_length_by_key(&moons, |(_, y, _)| *y);
  let z_len = find_loop_length_by_key(&moons, |(_, _, z)| *z);

  for _ in 0..1000 {
    let old_moons = moons.clone();

    for ((x, y, z), (vx, vy, vz)) in moons.iter_mut() {
      for ((ox, oy, oz), _) in old_moons.iter() {
        *vx += (*ox - *x).signum();
        *vy += (*oy - *y).signum();
        *vz += (*oz - *z).signum();
      }
    }

    for ((x, y, z), (vx, vy, vz)) in moons.iter_mut() {
      *x += *vx;
      *y += *vy;
      *z += *vz;
    }
  }

  println!("Part 1: {}", calc_energy(&moons));
  println!("Part 2: {}", lcm(x_len, lcm(y_len, z_len)));
}

fn calc_energy(moons: &Vec<((i32, i32, i32), (i32, i32, i32))>) -> i32 {
  moons
    .iter()
    .map(|((x, y, z), (vx, vy, vz))| {
      (x.abs() + y.abs() + z.abs()) * (vx.abs() + vy.abs() + vz.abs())
    })
    .sum()
}

fn find_loop_length_by_key(
  init_moons: &Vec<((i32, i32, i32), (i32, i32, i32))>,
  key_fn: impl Fn(&(i32, i32, i32)) -> i32,
) -> usize {
  let mut vals = init_moons
    .iter()
    .map(|(pos, _)| (key_fn(pos), 0))
    .collect_vec();

  let mut previous = HashSet::new();
  for i in 0.. {
    let old_vals = vals.clone();

    for (val, vval) in vals.iter_mut() {
      for (oval, _) in old_vals.iter() {
        *vval += (*oval - *val).signum();
      }

      *val += *vval;
    }

    if !previous.insert(old_vals.clone()) {
      return i;
    }
  }

  unreachable!()
}

fn lcm(a: usize, b: usize) -> usize {
  a * b / a.gcd(b)
}
