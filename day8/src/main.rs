use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::{iterate, Itertools};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let mut map: HashMap<char, HashSet<(i32, i32)>> = HashMap::new();
  let (mut maxx, mut maxy) = (0, 0);

  for (y, line) in lines.iter().enumerate() {
    let y = y as i32;
    maxy = maxy.max(y);
    for (x, c) in line.chars().enumerate() {
      let x = x as i32;
      maxx = maxx.max(x);
      if c == '.' {
        continue;
      }
      map.entry(c).or_default().insert((x, y));
    }
  }

  let mut antinodes: HashSet<(i32, i32)> = HashSet::new();

  for antennae in map.values() {
    for ((x1, y1), (x2, y2)) in antennae.iter().tuple_combinations() {
      let rise = y2 - y1;
      let run = x2 - x1;
      antinodes.insert((x2 + run, y2 + rise));
      antinodes.insert((x1 - run, y1 - rise));
    }
  }

  let num_antinodes = antinodes
    .iter()
    .filter(|(x, y)| *x >= 0 && *x <= maxx && *y >= 0 && *y <= maxy)
    .count();

  println!("{}", num_antinodes);

  antinodes.clear();
  for antennae in map.values() {
    for ((x1, y1), (x2, y2)) in antennae.iter().tuple_combinations() {
      let rise = y2 - y1;
      let run = x2 - x1;
      antinodes.extend(
        iterate((*x2, *y2), |(x2, y2)| (*x2 + run, *y2 + rise))
          .take_while(|p| in_bounds(p, maxx, maxy)),
      );
      antinodes.extend(
        iterate((*x1, *y1), |(x1, y1)| (*x1 - run, *y1 - rise))
          .take_while(|p| in_bounds(p, maxx, maxy)),
      );
    }
  }

  println!("{}", antinodes.len());
}

fn in_bounds((x, y): &(i32, i32), maxx: i32, maxy: i32) -> bool {
  *x >= 0 && *x <= maxx && *y >= 0 && *y <= maxy
}
