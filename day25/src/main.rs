use std::io::stdin;

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");
  let regex = Regex::new(r"\d+").unwrap();
  let (row, col) = regex
    .find_iter(&contents)
    .map(|m| m.as_str().parse::<u64>().unwrap() - 1)
    .collect_tuple()
    .unwrap();

  let n = row + col;
  let triangular = (n * (n + 1)) / 2;
  let nth = triangular + col + 1;

  let val = (20151125 * mod_pow(252533, nth - 1, 33554393)) % 33554393;
  println!("Part 1: {val}");
}

fn mod_pow(base: u64, mut exp: u64, modulus: u64) -> u64 {
  let mut result = 1;
  let mut base = base % modulus;
  while exp > 0 {
    if exp % 2 == 1 {
      result = (result * base) % modulus;
    }
    exp >>= 1;
    base = (base * base) % modulus;
  }
  result
}
