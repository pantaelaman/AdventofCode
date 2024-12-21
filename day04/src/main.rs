#![feature(strict_overflow_ops)]
use itertools::Itertools;
use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader},
};

const NEEDLE: [char; 4] = ['X', 'M', 'A', 'S'];

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let size = (lines.len(), lines[0].len());

  let letters: HashMap<(usize, usize), char> = lines
    .iter()
    .enumerate()
    .map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .map(move |(x, letter)| ((x, y), letter))
    })
    .flatten()
    .collect();

  let total_xmas: usize = letters
    .iter()
    .filter(|(_, v)| **v == 'X')
    .map(|(start, _)| count_matches_at(start.clone(), &letters, size))
    .sum();

  println!("Part 1: {}", total_xmas);

  let total_mas: usize = letters
    .iter()
    .filter(|(_, v)| **v == 'A')
    .filter(|((x, y), _)| {
      *x != 0 && *x != size.0 - 1 && *y != 0 && *y != size.1 - 1
    })
    .filter(|(start, _)| {
      opposite(*letters.get(&(start.0 - 1, start.1 - 1)).unwrap()).is_some_and(
        |v| {
          letters
            .get(&(start.0 + 1, start.1 + 1))
            .is_some_and(|u| *u == v)
        },
      ) && opposite(*letters.get(&(start.0 - 1, start.1 + 1)).unwrap())
        .is_some_and(|v| {
          letters
            .get(&(start.0 + 1, start.1 - 1))
            .is_some_and(|u| *u == v)
        })
    })
    .count();

  println!("Part 2: {}", total_mas);
}

fn count_matches_at(
  start: (usize, usize),
  letters: &HashMap<(usize, usize), char>,
  size: (usize, usize),
) -> usize {
  (-1..=1)
    .cartesian_product(-1..=1)
    .filter(|(dx, dy)| {
      start
        .0
        .checked_add_signed(dx * 3)
        .is_some_and(|v| v < size.0)
        && start
          .1
          .checked_add_signed(dy * 3)
          .is_some_and(|v| v < size.1)
    })
    .filter(|(dx, dy)| {
      for (i, follower) in NEEDLE.iter().enumerate().skip(1) {
        if letters
          .get(&(
            start.0.strict_add_signed(dx * i as isize),
            start.1.strict_add_signed(dy * i as isize),
          ))
          .unwrap()
          != follower
        {
          return false;
        }
      }
      true
    })
    .count()
}

#[inline]
fn opposite(inp: char) -> Option<char> {
  match inp {
    'M' => Some('S'),
    'S' => Some('M'),
    _ => None,
  }
}
