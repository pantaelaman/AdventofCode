use std::{
  fmt::Debug,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let regex = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let sum: usize = lines
    .iter()
    .map(|line| {
      regex
        .captures_iter(&line)
        .map(|caps| {
          caps
            .iter()
            .skip(1)
            .map(|v| v.unwrap().as_str().parse::<usize>().unwrap())
            .reduce(|acc, e| acc * e)
            .unwrap()
        })
        .sum::<usize>()
    })
    .sum();

  println!("Part 1: {}", sum);

  let mut on = true;

  let regex =
    Regex::new(r"(?:mul\((\d{1,3}),(\d{1,3})\))|(?:do\(\))|(?:don't\(\))")
      .unwrap();

  let sum: usize = lines
    .iter()
    .map(|line| {
      regex
        .captures_iter(&line)
        .map(|caps| {
          match caps.iter().next().unwrap().unwrap().as_str() {
            "do()" => {
              on = true;
              return 0;
            }
            "don't()" => {
              on = false;
              return 0;
            }
            _ => {}
          };

          if !on {
            return 0;
          }

          caps
            .iter()
            .skip(1)
            .map(|v| v.unwrap().as_str().parse::<usize>().unwrap())
            .reduce(|acc, e| acc * e)
            .unwrap()
        })
        .sum::<usize>()
    })
    .sum();

  println!("Part 2: {}", sum);
}
