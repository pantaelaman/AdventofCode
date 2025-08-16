use std::io::stdin;

use aocutil::Point;
use itertools::Itertools;

#[derive(Clone, Copy)]
struct Instruction {
  ty: InstructionType,
  amount: i32,
}

#[derive(Clone, Copy)]
enum InstructionType {
  N,
  S,
  E,
  W,
  L,
  R,
  F,
}

#[derive(Clone, Copy)]
enum Facing {
  North,
  East,
  South,
  West,
}

impl Facing {
  fn right(self) -> Self {
    match self {
      Self::North => Self::East,
      Self::East => Self::South,
      Self::South => Self::West,
      Self::West => Self::North,
    }
  }

  fn left(self) -> Self {
    match self {
      Self::North => Self::West,
      Self::East => Self::North,
      Self::South => Self::East,
      Self::West => Self::South,
    }
  }

  fn as_point(&self) -> Point<i32> {
    match self {
      Self::North => Point::new(0, 1),
      Self::East => Point::new(1, 0),
      Self::South => Point::new(0, -1),
      Self::West => Point::new(-1, 0),
    }
  }
}

struct Ship {
  position: Point<i32>,
  waypoint: Point<i32>,
  facing: Facing,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let instrs = lines
    .iter()
    .map(|line| {
      let ty = match line.chars().next().unwrap() {
        'N' => InstructionType::N,
        'S' => InstructionType::S,
        'E' => InstructionType::E,
        'W' => InstructionType::W,
        'L' => InstructionType::L,
        'R' => InstructionType::R,
        'F' => InstructionType::F,
        _ => unreachable!(),
      };

      let amount = line[1..].parse::<i32>().unwrap();

      Instruction { ty, amount }
    })
    .collect_vec();

  let mut ship = Ship {
    position: Point::default(),
    waypoint: Point::new(10, 1),
    facing: Facing::East,
  };

  for instr in instrs.iter() {
    match instr.ty {
      InstructionType::N => {
        ship.position = ship.position + Facing::North.as_point() * instr.amount;
      }
      InstructionType::E => {
        ship.position = ship.position + Facing::East.as_point() * instr.amount;
      }
      InstructionType::S => {
        ship.position = ship.position + Facing::South.as_point() * instr.amount;
      }
      InstructionType::W => {
        ship.position = ship.position + Facing::West.as_point() * instr.amount;
      }
      InstructionType::F => {
        ship.position = ship.position + ship.facing.as_point() * instr.amount;
      }
      InstructionType::L => {
        for _ in 0..(instr.amount / 90) {
          ship.facing = ship.facing.left();
        }
      }
      InstructionType::R => {
        for _ in 0..(instr.amount / 90) {
          ship.facing = ship.facing.right();
        }
      }
    }
  }

  println!("Part 1: {}", ship.position.manhattan(Point::default()));

  ship.position = Point::default();
  ship.facing = Facing::East;

  for instr in instrs.iter() {
    match instr.ty {
      InstructionType::N => {
        ship.waypoint = ship.waypoint + Facing::North.as_point() * instr.amount;
      }
      InstructionType::E => {
        ship.waypoint = ship.waypoint + Facing::East.as_point() * instr.amount;
      }
      InstructionType::S => {
        ship.waypoint = ship.waypoint + Facing::South.as_point() * instr.amount;
      }
      InstructionType::W => {
        ship.waypoint = ship.waypoint + Facing::West.as_point() * instr.amount;
      }
      InstructionType::F => {
        ship.position = ship.position + ship.waypoint * instr.amount;
      }
      InstructionType::L => {
        for _ in 0..(instr.amount / 90) {
          ship.waypoint = Point::new(-ship.waypoint.y, ship.waypoint.x);
        }
      }
      InstructionType::R => {
        for _ in 0..(instr.amount / 90) {
          ship.waypoint = Point::new(ship.waypoint.y, -ship.waypoint.x);
        }
      }
    }
  }

  println!("Part 2: {}", ship.position.manhattan(Point::default()));
}
