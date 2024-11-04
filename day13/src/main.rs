use std::{
  cmp::max,
  collections::{btree_set::Intersection, HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
  ops::Index,
};

use itertools::Itertools;

enum Track {
  Right,
  Left,
  Hori,
  Vert,
  Intersection,
}

impl Track {
  fn of_cart(minecart: &Minecart) -> Self {
    if minecart.direction.0 != 0 {
      Self::Hori
    } else {
      Self::Vert
    }
  }
}

impl Track {
  pub fn as_char(&self) -> char {
    match self {
      Self::Right => '/',
      Self::Left => '\\',
      Self::Hori => '-',
      Self::Vert => '|',
      Self::Intersection => '+',
    }
  }
}

impl TryFrom<char> for Track {
  type Error = char;

  fn try_from(value: char) -> Result<Self, char> {
    match value {
      '/' => Ok(Self::Right),
      '\\' => Ok(Self::Left),
      '-' => Ok(Self::Hori),
      '|' => Ok(Self::Vert),
      '+' => Ok(Self::Intersection),
      value => Err(value),
    }
  }
}

struct Minecart {
  direction: (i64, i64),
  intersection_n: usize,
}

impl Minecart {
  pub fn apply(&self, point: (i64, i64)) -> (i64, i64) {
    (point.0 + self.direction.0, point.1 + self.direction.1)
  }

  pub fn update(&mut self, track: &Track) {
    match track {
      Track::Hori | Track::Vert => {}
      Track::Right => {
        self.direction = (-self.direction.1, -self.direction.0);
      }
      Track::Left => {
        self.direction = (self.direction.1, self.direction.0);
      }
      Track::Intersection => {
        match self.intersection_n {
          0 => self.direction = (self.direction.1, -self.direction.0),
          2 => self.direction = (-self.direction.1, self.direction.0),
          _ => {}
        }
        self.intersection_n = (self.intersection_n + 1) % 3;
      }
    }
  }

  pub fn as_char(&self) -> char {
    match self.direction {
      (1, 0) => '>',
      (0, 1) => 'v',
      (-1, 0) => '<',
      (0, -1) => '^',
      _ => unreachable!(),
    }
  }
}

impl TryFrom<char> for Minecart {
  type Error = char;

  fn try_from(value: char) -> Result<Self, Self::Error> {
    Ok(Minecart {
      direction: match value {
        '>' => (1, 0),
        '<' => (-1, 0),
        'v' => (0, 1),
        '^' => (0, -1),
        value => return Err(value),
      },
      intersection_n: 0,
    })
  }
}

fn print_state(
  minecarts: &Vec<((i64, i64), Minecart)>,
  map: &HashMap<(i64, i64), Track>,
  crash: Option<(i64, i64)>,
  maxx: i64,
  maxy: i64,
) {
  for y in 0..=maxy {
    for x in 0..=maxx {
      if crash.map(|c| c == (x, y)).unwrap_or(false) {
        print!("X");
        continue;
      }
      if let Some((_, minecart)) = minecarts.iter().find(|(c, _)| c == &(x, y))
      {
        print!("{}", minecart.as_char());
        continue;
      }
      print!("{}", map.get(&(x, y)).map(|t| t.as_char()).unwrap_or(' '));
    }
    println!();
  }
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let mut minecarts: Vec<((i64, i64), Minecart)> = Vec::new();
  let map: HashMap<(i64, i64), Track> = reader
    .lines()
    .enumerate()
    .map(|(y, line)| {
      line
        .unwrap()
        .chars()
        .enumerate()
        .filter(|(_, c)| *c != ' ')
        .map(|(x, c)| {
          (
            (x as i64, y as i64),
            c.try_into().unwrap_or_else(|c: char| {
              let minecart: Minecart = c.try_into().unwrap();
              let track = Track::of_cart(&minecart);
              minecarts.push(((x as i64, y as i64), minecart));
              track
            }),
          )
        })
        .collect()
    })
    .concat();

  let (maxx, maxy) = map
    .keys()
    .copied()
    .reduce(|(maxx, maxy), (x, y)| (max(maxx, x), max(maxy, y)))
    .unwrap();

  let mut crashes = Vec::new();
  loop {
    let mut frozen_state: HashSet<(i64, i64)> =
      minecarts.iter().map(|(c, _)| c).copied().collect();
    for (coord, mut minecart) in std::mem::take(&mut minecarts).into_iter() {
      if !frozen_state.contains(&coord) {
        // skip if its been crashed into
        continue;
      }
      let ncoord = minecart.apply(coord);
      minecart.update(map.get(&ncoord).unwrap());
      if frozen_state.contains(&ncoord) {
        // gets here if the crashee hasn't yet updated (still exists in the frozen state)
        crashes.push(ncoord);
        frozen_state.remove(&ncoord);
        continue;
      } else if let Some(i) = minecarts.iter().position(|(c, _)| c == &ncoord) {
        // gets here if the crashee has already updated, and needs to be removed
        crashes.push(ncoord);
        minecarts.remove(i);
        continue;
      }
      frozen_state.remove(&coord);
      minecarts.push((ncoord, minecart));
    }
    minecarts.sort_by(|(c1, _), (c2, _)| c1.1.cmp(&c2.1).then(c1.0.cmp(&c2.0)));
    if minecarts.len() <= 1 {
      break;
    }
  }

  println!("Part 1: {:?}", crashes[0]);
  println!("Part 2: {:?}", minecarts[0].0);
}
