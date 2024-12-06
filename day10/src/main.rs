#![feature(unsigned_signed_diff)]
use std::{
  collections::{HashMap, HashSet},
  f64::consts::{PI, TAU},
  fs::File,
  io::{BufRead, BufReader},
};

use gcd::Gcd;
use itertools::Itertools;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let asteroids: HashSet<(usize, usize)> = lines
    .iter()
    .enumerate()
    .map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .filter_map(|(x, c)| (c == '#').then_some((x, y)))
        .collect()
    })
    .concat();

  let (station, n) = asteroids
    .iter()
    .map(|asteroid| {
      (
        asteroid,
        asteroids
          .iter()
          .filter(|target| *target != asteroid)
          .map(|target| calc_slope(asteroid, target))
          .collect::<HashSet<(isize, isize)>>()
          .len(),
      )
    })
    .max_by_key(|(_, n)| *n)
    .unwrap();

  println!("Part 1: {} (at {:?})", n, station);

  let mut slopes: Vec<((isize, isize), Vec<(usize, usize)>)> = asteroids
    .iter()
    .filter(|target| target != &station)
    .map(|target| (*target, calc_slope(station, target)))
    .fold(
      HashMap::new(),
      |mut acc: HashMap<(isize, isize), Vec<(usize, usize)>>,
       (target, slope)| {
        acc.entry(slope).or_default().push(target);
        acc
      },
    )
    .into_iter()
    .collect_vec();

  for (_, asteroids) in slopes.iter_mut() {
    asteroids.sort_by(|(x1, y1), (x2, y2)| {
      (station.0.abs_diff(*x2) + station.1.abs_diff(*y2))
        .cmp(&(station.0.abs_diff(*x1) + station.1.abs_diff(*y1)))
    });
  }

  slopes.sort_by(|(slope1, _), (slope2, _)| {
    let a1 = calc_angle(slope1);
    let a2 = calc_angle(slope2);

    a1.total_cmp(&a2)
  });

  let mut i = 0;
  let num_slopes = slopes.len();
  let mut next = (0, 0);
  for _ in 0..200 {
    next = loop {
      if let Some(next) = slopes[i % num_slopes].1.pop() {
        break next;
      }
      i += 1;
    };
    i += 1;
  }

  println!("Part 2: {}", next.0 * 100 + next.1);
}

fn calc_slope(
  (ax, ay): &(usize, usize),
  (x, y): &(usize, usize),
) -> (isize, isize) {
  let udx = ax.checked_signed_diff(*x).unwrap();
  let udy = ay.checked_signed_diff(*y).unwrap();
  let gcd = (udx.abs() as usize).gcd(udy.abs() as usize) as isize;

  (udx / gcd, udy / gcd)
}

fn calc_angle((sx, sy): &(isize, isize)) -> f64 {
  ((*sy as f64).atan2(*sx as f64) + (3. * PI / 2.)) % TAU
}
