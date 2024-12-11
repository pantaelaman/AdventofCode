use std::{collections::HashMap, fs::File, io::Read};

use itertools::Itertools;

fn main() {
  let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let mut buf = String::new();
  file.read_to_string(&mut buf).unwrap();

  let stones = buf
    .trim()
    .split_whitespace()
    .map(|s| s.parse::<usize>().unwrap())
    .collect_vec();

  let mut cache = HashMap::new();
  let (p1, p2) = stones
    .iter()
    .map(|stone| {
      (
        calc_stone(*stone, 25, &mut cache),
        calc_stone(*stone, 75, &mut cache),
      )
    })
    .reduce(|(a1, b1), (a2, b2)| (a1 + a2, b1 + b2))
    .unwrap();

  println!("Part 1: {p1}");
  println!("Part 2: {p2}");
}

fn calc_stone(
  stone: usize,
  iterations: usize,
  cache: &mut HashMap<(usize, usize), usize>,
) -> usize {
  if let Some(val) = cache.get(&(stone, iterations)) {
    return *val;
  }

  let val = if iterations == 0 {
    1
  } else if stone == 0 {
    calc_stone(1, iterations - 1, cache)
  } else {
    let n = stone.ilog10();
    if n % 2 == 1 {
      let boundary: usize = 10usize.pow((n / 2) + 1);
      calc_stone(stone / boundary, iterations - 1, cache)
        + calc_stone(stone % boundary, iterations - 1, cache)
    } else {
      calc_stone(stone * 2024, iterations - 1, cache)
    }
  };

  cache.insert((stone, iterations), val);

  val
}
