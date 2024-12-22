use std::{
  fs::File,
  io::{stdin, BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let reader = BufReader::new(stdin());

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
}
