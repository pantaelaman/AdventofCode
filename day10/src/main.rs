use std::io::stdin;

use itertools::{iterate, Itertools};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let mut game = iterate(lines[0].to_owned(), run_game);

  println!("Part 1: {}", game.nth(40).unwrap().len());
  println!("Part 2: {}", game.nth(9).unwrap().len());
}

fn run_game(inp: &String) -> String {
  let mut out = String::new();
  for (k, cs) in inp.chars().chunk_by(|c| *c).into_iter() {
    out.push_str(cs.count().to_string().as_str());
    out.push(k);
  }
  out
}
