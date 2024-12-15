use std::{
  collections::HashSet,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;

type Position = (i32, i32);

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  let (map_lines, instr_lines) = lines
    .split(|s| s.trim().is_empty())
    .collect_tuple()
    .unwrap();

  let mut walls_p1: HashSet<Position> = HashSet::new();
  let mut boxes_p1: HashSet<Position> = HashSet::new();
  let mut robot_p1 = (0, 0);
  let mut walls_p2: HashSet<Position> = HashSet::new();
  let mut boxes_p2: HashSet<Position> = HashSet::new();
  let mut robot_p2 = (0, 0);
  for ((x, y), c) in map_lines.iter().enumerate().flat_map(|(y, line)| {
    line
      .chars()
      .enumerate()
      .map(move |(x, c)| ((x as i32, y as i32), c))
  }) {
    match c {
      '#' => {
        walls_p1.insert((x, y));
        walls_p2.insert((x * 2, y));
        walls_p2.insert((x * 2 + 1, y));
      }
      'O' => {
        boxes_p1.insert((x, y));
        boxes_p2.insert((x * 2, y));
      }
      '@' => {
        robot_p1 = (x, y);
        robot_p2 = (x * 2, y);
      }
      _ => {}
    }
  }

  for facing in instr_lines
    .iter()
    .flat_map(|line| line.chars().map(|c| instr_to_facing(c)))
  {
    match can_move_p1(robot_p1, facing, &boxes_p1, &walls_p1, vec![]) {
      Some(positions) => {
        robot_p1 = apply_facing(robot_p1, facing);
        for position in positions.into_iter().rev() {
          boxes_p1.remove(&position);
          boxes_p1.insert(apply_facing(position, facing));
        }
      }
      None => {}
    }
    match can_move_p2(robot_p2, facing, &boxes_p2, &walls_p2, vec![]) {
      Some(positions) => {
        robot_p2 = apply_facing(robot_p2, facing);
        for position in positions.into_iter().rev() {
          boxes_p2.remove(&position);
          boxes_p2.insert(apply_facing(position, facing));
        }
      }
      None => {}
    }
  }

  let gps_total_p1 = boxes_p1.iter().map(|(x, y)| y * 100 + x).sum::<i32>();
  println!("Part 1: {gps_total_p1}");

  let gps_total_p2 = boxes_p2.iter().map(|(x, y)| y * 100 + x).sum::<i32>();
  println!("Part 2: {gps_total_p2}");
}

fn can_move_p1(
  p: Position,
  facing: Position,
  boxes: &HashSet<Position>,
  walls: &HashSet<Position>,
  mut prev: Vec<Position>,
) -> Option<Vec<Position>> {
  let target = apply_facing(p, facing);
  if walls.contains(&target) {
    None
  } else if boxes.contains(&target) {
    prev.push(target);
    can_move_p1(target, facing, boxes, walls, prev)
  } else {
    Some(prev)
  }
}

fn can_move_p2(
  p: Position,
  facing: Position,
  boxes: &HashSet<Position>,
  walls: &HashSet<Position>,
  mut prev: Vec<Position>,
) -> Option<Vec<Position>> {
  let target = apply_facing(p, facing);
  if walls.contains(&target) {
    None
  } else if boxes.contains(&target) {
    prev.push(target);
    // don't hit yourself
    if facing != (1, 0) {
      prev = can_move_p2(target, facing, boxes, walls, prev)?;
    }
    can_move_p2(apply_facing(target, (1, 0)), facing, boxes, walls, prev)
  } else if boxes.contains(&apply_facing(target, (-1, 0))) {
    prev.push(apply_facing(target, (-1, 0)));
    // don't hit yourself
    if facing != (-1, 0) {
      prev = can_move_p2(target, facing, boxes, walls, prev)?;
    }
    can_move_p2(apply_facing(target, (-1, 0)), facing, boxes, walls, prev)
  } else {
    Some(prev)
  }
}

fn instr_to_facing(instr: char) -> Position {
  match instr {
    '^' => (0, -1),
    'v' => (0, 1),
    '<' => (-1, 0),
    '>' => (1, 0),
    _ => unimplemented!(),
  }
}

fn apply_facing((x, y): Position, (dx, dy): Position) -> Position {
  (x + dx, y + dy)
}
