use std::{
  io::stdin,
  ops::{Add, Mul},
};

use itertools::{izip, Itertools};

type N = i64;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let mut parsed = lines
    .iter()
    .map(|line| line.split_whitespace())
    .collect_vec();

  let ops: Vec<fn(N, N) -> N> = parsed
    .pop()
    .unwrap()
    .into_iter()
    .map(|op| match op {
      "+" => N::add as fn(N, N) -> N,
      "*" => N::mul as fn(N, N) -> N,
      _ => unimplemented!(),
    })
    .collect_vec();

  let num_numberlines = parsed.len(); // lines after removing operators

  let inputs: Vec<Vec<N>> = parsed
    .into_iter()
    .map(|nums| nums.map(|num| num.parse::<N>().unwrap()).collect_vec())
    .collect_vec();

  let solution = inputs
    .into_iter()
    .reduce(|top, bottom| {
      izip!(top, ops.iter(), bottom)
        .map(|(t, o, b)| o(t, b))
        .collect_vec()
    })
    .unwrap()
    .into_iter()
    .sum::<N>();

  println!("Part 1: {}", solution);

  let solution = lines
    .into_iter()
    .take(num_numberlines)
    .fold(Vec::new(), |acc, line| {
      let mut acc = if acc.len() == 0 {
        vec![String::new(); line.len()]
      } else {
        acc
      };

      acc.iter_mut().zip(line.chars()).for_each(|(s, c)| {
        if !c.is_whitespace() {
          s.push(c);
        }
      });

      acc
    })
    .split(|s| s.is_empty())
    .zip(ops)
    .map(|(group, op)| {
      group
        .iter()
        .map(|n| n.parse::<N>().unwrap())
        .reduce(op)
        .unwrap()
    })
    .sum::<N>();

  println!("Part 2: {}", solution);
}
