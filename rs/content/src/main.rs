use std::{
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
}
