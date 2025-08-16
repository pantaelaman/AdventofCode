use std::{io::stdin, iter::successors};

use itertools::Itertools;

const REASONABLE_BOUND: i128 = 100000000000000;
//const REASONABLE_BOUND: i128 = 1000000;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let start = lines[0].parse::<i128>().unwrap();
  let buses = lines[1]
    .split(',')
    .enumerate()
    .filter_map(|(o, x)| x.parse::<i128>().ok().map(|v| (o as i128, v)))
    .collect_vec();

  let first_bus = buses
    .iter()
    .map(|(_, bus_timing)| (bus_timing, bus_timing - (start % bus_timing)))
    .min_by_key(|(_, v)| *v)
    .unwrap();
  println!("Part 1: {}", first_bus.0 * first_bus.1);

  let (a12, _) = buses
    .iter()
    .copied()
    .map(|(o, b)| (b - o, b))
    .reduce(|(a1, n1), (a2, n2)| {
      let (m1, m2) = eeuclid(n1, n2);
      let next_prod = n1 * n2;
      let x = (a1 * m2 * n2 + a2 * m1 * n1) % next_prod;
      (if x < 0 { x + next_prod } else { x }, next_prod)
    })
    .unwrap();

  println!("Part 2: {a12}");
}

fn eeuclid(a: i128, b: i128) -> (i128, i128) {
  let (mut cur_r, mut cur_s, mut cur_t) = (b, 0, 1);
  let (mut prev_r, mut prev_s, mut prev_t) = (a, 1, 0);
  let mut buf;

  while cur_r != 0 {
    let cur_q = prev_r / cur_r;

    buf = cur_r;
    cur_r = prev_r - cur_q * cur_r;
    prev_r = buf;

    buf = cur_s;
    cur_s = prev_s - cur_q * cur_s;
    prev_s = buf;

    buf = cur_t;
    cur_t = prev_t - cur_q * cur_t;
    prev_t = buf;
  }

  (prev_s, prev_t)
}
