use itertools::Itertools;
use std::{
  fs::File,
  io::{BufRead, BufReader},
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let dimensions = reader
    .lines()
    .map(|line| {
      line
        .unwrap()
        .split('x')
        .map(|v| v.parse::<usize>().unwrap())
        .collect_tuple::<(usize, usize, usize)>()
        .unwrap()
    })
    .collect_vec();

  let total_paper: usize = dimensions
    .iter()
    .map(|(l, w, h)| {
      let (a1, a2, a3) = (l * w, w * h, h * l);
      2 * a1 + 2 * a2 + 2 * a3 + a1.min(a2).min(a3)
    })
    .sum();

  println!("Part 1: {}", total_paper);

  let total_ribbon: usize = dimensions
    .iter()
    .map(|(l, w, h)| 2 * l + 2 * w + 2 * h - 2 * (l.max(w).max(h)) + l * w * h)
    .sum();

  println!("Part 2: {}", total_ribbon);
}
