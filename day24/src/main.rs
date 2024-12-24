#![feature(fn_traits)]
use std::{
  collections::HashMap,
  io::{stdin, BufRead, BufReader},
  ops::{BitAnd, BitOr, BitXor},
};

use itertools::Itertools;

fn main() {
  let reader = BufReader::new(stdin());

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let (raw_init_states, cmds) =
    lines.split(|s| s.is_empty()).collect_tuple().unwrap();

  let init_states: HashMap<&str, bool> = raw_init_states
    .into_iter()
    .map(|s| {
      let (wire, value) = s.split(": ").collect_tuple().unwrap();
      println!("{wire} -> {value}");
      (wire, value.parse::<usize>().unwrap() == 1)
    })
    .collect();

  let mut wires: HashMap<&str, ([&str; 2], fn(bool, bool) -> bool)> = cmds
    .into_iter()
    .map(|s| {
      let (inps, out) = s.split(" -> ").collect_tuple().unwrap();
      let (a, op, b) = inps.split(" ").collect_tuple().unwrap();
      let op: fn(bool, bool) -> bool = match op {
        "XOR" => bool::bitxor,
        "OR" => bool::bitor,
        "AND" => bool::bitand,
        _ => unimplemented!(),
      };
      (out, ([a, b], op))
    })
    .collect();

  let mut cache: HashMap<&str, bool> = HashMap::new();
  let bits = wires
    .keys()
    .filter(|s| s.chars().nth(0).unwrap() == 'z')
    .sorted()
    .rev()
    .map(|wire| solve_key(wire, &init_states, &wires, &mut cache))
    .fold(0, |acc, b| (acc << 1) | (b as usize));

  println!("Part 1: {bits}");
}

fn solve_key<'a, 'b>(
  wire: &'a str,
  init_states: &'a HashMap<&'a str, bool>,
  wires: &'a HashMap<&'a str, ([&'a str; 2], fn(bool, bool) -> bool)>,
  cache: &'b mut HashMap<&'a str, bool>,
) -> bool {
  if let Some(v) = cache.get(wire) {
    return *v;
  }
  if let Some(v) = init_states.get(wire) {
    return *v;
  }

  let (inps, op) = wires.get(wire).unwrap();
  let inp1 = solve_key(inps[0], init_states, wires, cache);
  let inp2 = solve_key(inps[1], init_states, wires, cache);
  let value = op(inp1, inp2);
  cache.insert(wire, value);

  value
}
