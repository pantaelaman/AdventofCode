use std::io::stdin;

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let regex =
    Regex::new(r"a (\w)\w*?(?:(?:-compatible microchip)| generator)").unwrap();
  let mut items = lines
    .iter()
    .map(|line| regex.find_iter(line).count())
    .collect_vec();
  println!("Part 1: {}", total_moves(&items));
  items[0] += 4;
  println!("Part 2: {}", total_moves(&items));
}

#[inline(always)]
fn moves(num_items: usize) -> usize {
  num_items * 2 - 3
}

fn total_moves(items: &[usize]) -> usize {
  items
    .iter()
    .scan(0, |total, items| {
      *total += items;
      Some(*total)
    })
    .take(items.len() - 1)
    .map(moves)
    .sum::<usize>()
}
