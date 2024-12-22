#![feature(unsigned_signed_diff)]
#![feature(iter_map_windows)]
use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::{iterate, Itertools};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let total = lines
    .iter()
    .map(|line| line.parse::<usize>().unwrap())
    .map(|num| iterate(num, next_secret).nth(2000).unwrap())
    .sum::<usize>();

  println!("Part 1: {total}");

  let max_bananas = lines
    .iter()
    .map(|line| line.parse::<usize>().unwrap())
    .map(|num| {
      iterate(num, next_secret)
        .take(2001)
        .map(|n| n % 10)
        .map_windows(|nums: &[usize; 5]| {
          let change_seq = nums
            .iter()
            .tuple_windows()
            .map(|(x, y)| y.checked_signed_diff(*x).unwrap())
            .collect_vec();
          (change_seq, nums[4])
        })
        .unique_by(|(k, _)| k.clone())
    })
    .flatten()
    .fold(HashMap::<Vec<isize>, usize>::new(), |mut acc, (seq, v)| {
      *acc.entry(seq).or_default() += v;
      acc
    })
    .into_values()
    .max()
    .unwrap();

  println!("Part 2: {max_bananas}");
}

fn next_secret(secret: &usize) -> usize {
  let a = prune(secret ^ (secret << 6));
  let b = prune(a ^ (a >> 5));
  let c = prune(b ^ (b << 11));
  c
}

fn prune(secret: usize) -> usize {
  secret % 16777216
}
