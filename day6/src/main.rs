use itertools::Itertools;
use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader},
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let orbiters: HashMap<String, String> = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();

      line
        .split(')')
        .map(|s| s.to_owned())
        .rev()
        .collect_tuple()
        .unwrap()
    })
    .collect();

  let mut cached: HashMap<String, usize> = HashMap::new();
  let total_orbits = orbiters
    .keys()
    .map(|orbiter| calc_num_orbits(orbiter, &orbiters, &mut cached))
    .sum::<usize>();

  println!("Part 1: {}", total_orbits);

  let santa_path = build_path("SAN", &orbiters);
  let you_path = build_path("YOU", &orbiters);

  let first_common = santa_path
    .into_iter()
    .find(|part| you_path.contains(part))
    .unwrap();

  let common_orbits = cached.get(first_common).unwrap();
  let total_moves = cached.get("SAN").unwrap() - common_orbits
    + cached.get("YOU").unwrap()
    - common_orbits
    - 2;

  println!("Part 2: {}", total_moves);
}

fn calc_num_orbits(
  key: &String,
  orbiters: &HashMap<String, String>,
  cached: &mut HashMap<String, usize>,
) -> usize {
  if let Some(val) = cached.get(key) {
    return *val;
  }
  let val = orbiters
    .get(key)
    .map(|orbitee| calc_num_orbits(orbitee, orbiters, cached) + 1)
    .unwrap_or(0);
  cached.insert(key.clone(), val);
  val
}

fn build_path<'a>(
  key: &'a str,
  orbiters: &'a HashMap<String, String>,
) -> Vec<&'a str> {
  let mut path = Vec::new();

  let mut current_key = key;
  loop {
    path.push(current_key);
    if let Some(new_key) = orbiters.get(current_key) {
      current_key = new_key;
    } else {
      break;
    }
  }

  path
}
