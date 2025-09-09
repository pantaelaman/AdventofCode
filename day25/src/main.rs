use std::{io::stdin, iter::successors};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let card_key = lines[0].parse::<usize>().unwrap();
  let door_key = lines[1].parse::<usize>().unwrap();

  let card_loop_size = successors(Some(1), |v| Some((v * 7) % 20201227))
    .enumerate()
    .find_map(|(i, v)| (v == card_key).then_some(i))
    .unwrap();

  println!("Solution: {}", transform(door_key, card_loop_size));
}

fn transform(subject: usize, loop_size: usize) -> usize {
  successors(Some(1), |v| Some((v * subject) % 20201227))
    .nth(loop_size)
    .unwrap()
}
