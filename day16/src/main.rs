use std::{collections::HashMap, io::stdin};

use itertools::Itertools;

static ANALYSIS: phf::Map<&'static str, usize> = phf::phf_map! {
  "children" => 3,
  "cats" => 7,
  "samoyeds" => 2,
  "pomeranians" => 3,
  "akitas" => 0,
  "vizslas" => 0,
  "goldfish" => 5,
  "trees" => 3,
  "cars" => 2,
  "perfumes" => 1,
};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();

  let sues: Vec<HashMap<&str, usize>> = lines
    .iter()
    .map(|line| {
      let memories = line.split_once(": ").unwrap().1;
      memories
        .split(", ")
        .map(|p| p.split_once(": ").unwrap())
        .map(|(k, v)| (k, v.parse().unwrap()))
        .collect()
    })
    .collect_vec();

  let giver = sues
    .iter()
    .position(|memories| {
      memories.iter().all(|(k, v)| ANALYSIS.get(k).unwrap() == v)
    })
    .unwrap();
  println!("Part 1: {}", giver + 1);

  let giver = sues
    .iter()
    .position(|memories| {
      memories.iter().all(|(k, v)| {
        let analytical = ANALYSIS.get(k).unwrap();
        match *k {
          "cats" | "trees" => analytical < v,
          "pomeranians" | "goldfish" => analytical > v,
          _ => analytical == v,
        }
      })
    })
    .unwrap();
  println!("Part 2: {}", giver + 1);
}
