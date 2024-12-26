use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader},
  usize,
};

use aocutil::{grid_input, Point};
use bimap::BiHashMap;
use itertools::{repeat_n, Itertools};

static DIRECTIONAL: phf::Map<char, Point<i16>> = phf::phf_map! {
  '<' => Point::new(0, 0),
  'v' => Point::new(1, 0),
  '>' => Point::new(2, 0),
  '^' => Point::new(1, 1),
  'A' => Point::new(2, 1),
};

static NUMERIC: phf::Map<char, Point<i16>> = phf::phf_map! {
  '0' => Point::new(1, 0),
  'A' => Point::new(2, 0),
  '1' => Point::new(0, 1),
  '2' => Point::new(1, 1),
  '3' => Point::new(2, 1),
  '4' => Point::new(0, 2),
  '5' => Point::new(1, 2),
  '6' => Point::new(2, 2),
  '7' => Point::new(0, 3),
  '8' => Point::new(1, 3),
  '9' => Point::new(2, 3),
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let numeric = grid_input([" 0A", "123", "456", "789"])
    .filter(|(_, c)| *c != ' ')
    .map(|(p, c)| (c, p.map(|v: usize| v as i16)))
    .collect::<HashMap<char, Point<i16>>>();

  let directional = grid_input(["<v>", " ^A"])
    .filter(|(_, c)| *c != ' ')
    .map(|(p, c)| (c, p.map(|v: usize| v as i16)))
    .collect::<HashMap<char, Point<i16>>>();

  let mut nonnumeric_cache = HashMap::new();
  let answer = lines
    .iter()
    .map(|code| {
      let mut robot = *numeric.get(&'A').unwrap();
      let len = code
        .chars()
        .map(|c| {
          let target = *numeric.get(&c).unwrap();
          let best = best_movement(
            robot,
            target,
            4,
            true,
            &directional,
            Point::new(0, 0),
            &mut nonnumeric_cache,
          );
          robot = target;
          best
        })
        .sum::<usize>();
      let num = code[0..3].parse::<usize>().unwrap();
      num * len
    })
    .sum::<usize>();

  println!("Part 2: {answer}");
}

fn solve_code(
  path: String,
  depth: usize,
  directional: &HashMap<char, Point<i16>>,
  cache: &mut HashMap<(Point<i16>, Point<i16>, usize), usize>,
) -> usize {
  if depth == 0 {
    println!("{path}");
    return path.len();
  }

  //println!("{depth} : {path}");

  let mut robot = *directional.get(&'A').unwrap();
  let total_cost = path
    .chars()
    .map(|c| {
      let target = *directional.get(&c).unwrap();
      let best = best_movement(
        robot,
        target,
        depth,
        false,
        directional,
        Point::new(0, 1),
        cache,
      );
      robot = target;
      best
    })
    .sum();
  //println!("{depth} <- {total_cost}");
  total_cost
}

fn best_movement(
  robot: Point<i16>,
  target: Point<i16>,
  depth: usize,
  skip_cache: bool,
  directional: &HashMap<char, Point<i16>>,
  avoid: Point<i16>,
  cache: &mut HashMap<(Point<i16>, Point<i16>, usize), usize>,
) -> usize {
  if !skip_cache {
    if let Some(v) = cache.get(&(robot, target, depth)) {
      return *v;
    }
  }

  let mut minimum = usize::MAX;
  let mut frontier = vec![(robot, String::new())];
  while !frontier.is_empty() {
    for (point, so_far) in std::mem::take(&mut frontier) {
      if point == avoid {
        continue;
      }
      if point == target {
        let mut next_path = so_far;
        next_path.push('A');
        minimum =
          minimum.min(solve_code(next_path, depth - 1, directional, cache));
        continue;
      }
      if point.x < target.x {
        let mut next_path = so_far.clone();
        next_path.push('>');
        frontier.push((point + Point::new(1, 0), next_path));
      } else if point.x > target.x {
        let mut next_path = so_far.clone();
        next_path.push('<');
        frontier.push((point + Point::new(-1, 0), next_path));
      }
      if point.y < target.y {
        let mut next_path = so_far.clone();
        next_path.push('^');
        frontier.push((point + Point::new(0, 1), next_path));
      } else if point.y > target.y {
        let mut next_path = so_far.clone();
        next_path.push('v');
        frontier.push((point + Point::new(0, -1), next_path));
      }
    }
  }

  if !skip_cache {
    cache.insert((robot, target, depth), minimum);
  }
  minimum
}

//fn solve_input(
//  code: &str,
//  pad: &BiHashMap<Point<i16>, char>,
//  robot: &mut Point<i16>,
//) -> String {
//  let mut path = String::new();
//  for c in code.chars() {
//    let target = pad.get_by_right(&c).unwrap();
//    let x_dist = target.x - robot.x;
//    let y_dist = target.y - robot.y;
//
//    if x_dist > 0 {
//      path.extend(repeat_n('>', x_dist.abs() as usize));
//    }
//    if y_dist > 0 {
//      path.extend(repeat_n('^', y_dist.abs() as usize));
//    }
//    if y_dist < 0 {
//      path.extend(repeat_n('v', y_dist.abs() as usize));
//    }
//    if x_dist < 0 {
//      path.extend(repeat_n('<', x_dist.abs() as usize));
//    }
//    path.push('A');
//
//    *robot = *target;
//  }
//  path
//}
