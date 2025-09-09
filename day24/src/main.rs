use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
  q: i32,
  r: i32,
}

impl std::fmt::Debug for Coord {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}, {}, <{}>)", self.q, self.r, self.s())
  }
}

impl Coord {
  fn q(&self) -> i32 {
    self.q
  }

  fn r(&self) -> i32 {
    self.r
  }

  fn s(&self) -> i32 {
    -self.q - self.r
  }
}

impl std::ops::Add for Coord {
  type Output = Coord;

  fn add(self, rhs: Self) -> Self::Output {
    Self {
      q: self.q + rhs.q,
      r: self.r + rhs.r,
    }
  }
}

impl std::ops::Sub for Coord {
  type Output = Coord;

  fn sub(self, rhs: Self) -> Self::Output {
    Self {
      q: self.q - rhs.q,
      r: self.r - rhs.r,
    }
  }
}

impl std::ops::Mul<Coord> for i32 {
  type Output = Coord;

  fn mul(self, rhs: Coord) -> Self::Output {
    Coord {
      q: self * rhs.q,
      r: self * rhs.r,
    }
  }
}

impl std::ops::Neg for Coord {
  type Output = Coord;

  fn neg(self) -> Self::Output {
    Self {
      q: -self.q,
      r: -self.r,
    }
  }
}

macro_rules! coord {
  ($q:expr, $r:expr) => {
    Coord { q: $q, r: $r }
  };
}

const NORTHWEST: Coord = coord!(0, -1);
const NORTHEAST: Coord = coord!(1, -1);
const WEST: Coord = coord!(-1, 0);
const SOUTHWEST: Coord = coord!(-1, 1);
const SOUTHEAST: Coord = coord!(0, 1);
const EAST: Coord = coord!(1, 0);
const NEIGHBOURS: [Coord; 6] =
  [WEST, NORTHWEST, NORTHEAST, EAST, SOUTHEAST, SOUTHWEST];

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let visited_tiles = lines
    .iter()
    .map(|line| {
      line
        .chars()
        .batching(|cs| match cs.next() {
          Some('e') => Some(EAST),
          Some('w') => Some(WEST),
          Some('n') => match cs.next() {
            Some('e') => Some(NORTHEAST),
            Some('w') => Some(NORTHWEST),
            _ => None,
          },
          Some('s') => match cs.next() {
            Some('e') => Some(SOUTHEAST),
            Some('w') => Some(SOUTHWEST),
            _ => None,
          },
          _ => None,
        })
        .reduce(std::ops::Add::add)
        .unwrap()
    })
    .counts();

  let mut black_tiles = visited_tiles
    .into_iter()
    .filter_map(|(tile, v)| (v % 2 == 1).then_some(tile))
    .collect::<HashSet<Coord>>();
  println!("Part 1: {}", black_tiles.len());

  for _ in 0..100 {
    let mut potential_blacks: HashMap<Coord, usize> = HashMap::new();
    let mut next_tiles = HashSet::new();
    for tile in black_tiles.iter() {
      let nblack_neighbours = NEIGHBOURS
        .iter()
        .map(|n| *tile + *n)
        .inspect(|t| {
          *potential_blacks.entry(*t).or_default() += 1;
        })
        .filter(|t| black_tiles.contains(t))
        .count();
      if nblack_neighbours != 0 && nblack_neighbours <= 2 {
        next_tiles.insert(*tile);
      }
    }
    next_tiles.extend(
      potential_blacks
        .into_iter()
        .filter_map(|(tile, v)| (v == 2).then_some(tile)),
    );
    black_tiles = next_tiles;
  }

  println!("Part 2: {}", black_tiles.len());
}
