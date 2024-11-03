use std::{collections::VecDeque, fs::File, io::Read, ops::SubAssign};

use itertools::repeat_n;
use regex::Regex;

fn play(num_players: usize, last_marble: usize) -> usize {
  let mut scores: Vec<usize> = {
    let mut scores = Vec::with_capacity(num_players);
    scores.extend(repeat_n(0, num_players));
    scores
  };
  let mut circle: VecDeque<usize> = VecDeque::new();
  circle.push_back(0);
  for marble in 1..=last_marble {
    if marble % 23 == 0 {
      circle.rotate_right(7);
      scores[marble % num_players] += marble + circle.pop_back().unwrap();
      circle.rotate_left(1);
      continue;
    }
    circle.rotate_left(1);
    circle.push_back(marble);
  }
  *scores.iter().max().unwrap()
}

fn main() {
  let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let content = {
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    content
  };

  let regex =
    Regex::new(r"(\d+) players; last marble is worth (\d+) points").unwrap();

  let caps = regex.captures(&content).unwrap();
  let num_players = caps[1].parse::<usize>().unwrap();
  let last_marble = caps[2].parse::<usize>().unwrap();

  println!("Part 1: {}", play(num_players, last_marble));
  println!("Part 2: {}", play(num_players, last_marble * 100));
}
