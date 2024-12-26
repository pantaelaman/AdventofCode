#![feature(iter_next_chunk)]
use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let regex = Regex::new(
    r"(\w+): \w+ (-?\d+), \w+ (-?\d+), \w+ (-?\d+), \w+ (-?\d+), \w+ (-?\d+)",
  )
  .unwrap();

  let ingredients: HashMap<&str, ([i32; 4], i32)> = lines
    .iter()
    .map(|line| {
      let caps = regex.captures(line).unwrap();
      let mut cs = caps.iter().skip(1).map(|c| c.unwrap().as_str());
      let name = cs.next().unwrap();
      let mut cs = cs.map(|c| c.parse::<i32>().unwrap());
      let vals = cs.next_chunk::<4>().unwrap();
      let cals = cs.exactly_one().unwrap();
      (name, (vals, cals))
    })
    .collect();

  let best = seek_best(
    &ingredients,
    100,
    [0; 4],
    0,
    HashSet::new(),
    &score_qualities,
  );
  println!("Part 1: {best}");
  let best = seek_best(
    &ingredients,
    100,
    [0; 4],
    0,
    HashSet::new(),
    &score_qualities_caloric,
  );
  println!("Part 2: {best}");
}

fn seek_best<'k>(
  ingredients: &HashMap<&'k str, ([i32; 4], i32)>,
  teaspoons: usize,
  qualities: [i32; 4],
  calories: i32,
  used: HashSet<&'k str>,
  score: &impl Fn(&[i32; 4], i32) -> i32,
) -> i32 {
  let possibles = ingredients.keys().filter(|k| !used.contains(*k));

  if teaspoons == 0 {
    return score(&qualities, calories);
  }

  match possibles.exactly_one() {
    Ok(possible) => {
      let ingredient = ingredients.get(possible).unwrap();
      score(
        &apply_qualities(&ingredient.0, teaspoons, &qualities),
        calories + ingredient.1 * teaspoons as i32,
      )
    }
    Err(mut possibles) => {
      let possible = possibles.next().unwrap();
      let ingredient = ingredients.get(possible).unwrap();
      (0..=teaspoons)
        .into_iter()
        .map(|times| {
          let mut next_used = used.clone();
          next_used.insert(possible);
          let next_qualities =
            apply_qualities(&ingredient.0, times, &qualities);
          seek_best(
            ingredients,
            teaspoons - times,
            next_qualities,
            calories + ingredient.1 * times as i32,
            next_used,
            score,
          )
        })
        .max()
        .unwrap()
    }
  }
}

fn apply_qualities<'k>(
  addition: &[i32; 4],
  times: usize,
  qualities: &[i32; 4],
) -> [i32; 4] {
  addition
    .iter()
    .zip(qualities.iter())
    .map(|(a, b)| a * times as i32 + b)
    .next_chunk()
    .unwrap()
}

fn score_qualities(qualities: &[i32; 4], _: i32) -> i32 {
  qualities.into_iter().map(|q| q.max(&0)).product()
}

fn score_qualities_caloric(qualities: &[i32; 4], calories: i32) -> i32 {
  if calories != 500 {
    return 0;
  }
  qualities.into_iter().map(|q| q.max(&0)).product()
}
