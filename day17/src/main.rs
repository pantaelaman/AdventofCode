use std::{collections::HashSet, fs::File};

use intcode::{
  file_to_values, OwnedContext, Program, ProgrammedInput, SingletonValue, Value,
};
use itertools::Itertools;

type Position = (Value, Value);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Turn {
  Right = 'R' as u8,
  Left = 'L' as u8,
}

fn main() {
  let mut args = std::env::args().skip(1).peekable();
  let file = File::open(args.next().unwrap()).unwrap();
  let values = file_to_values(file);

  let context = OwnedContext::new(ProgrammedInput::new([]), Vec::new());
  let mut program = Program::new(values.clone(), context);
  program.run_until_completed();
  let input = std::mem::take(&mut program.context.output)
    .into_iter()
    .map(|d| char::from_u32(d as u32).unwrap())
    .collect::<String>();

  let mut scaffolds = HashSet::new();
  let mut vacuum: Position = (0, 0);
  let mut facing: Position = (0, 0);
  let (mut maxx, mut maxy) = (0, 0);
  for (y, line) in input.lines().enumerate() {
    let y = y as Value;
    maxy = maxy.max(y);
    for (x, c) in line.chars().enumerate() {
      let x = x as Value;
      maxx = maxx.max(x);
      match c {
        '.' => continue,
        '^' | 'v' | '<' | '>' => {
          vacuum = (x, y);
          facing = match c {
            '^' => (0, -1),
            'v' => (0, 1),
            '<' => (-1, 0),
            '>' => (1, 0),
            _ => unreachable!(),
          }
        }
        '#' => {}
        _ => unimplemented!(),
      }
      scaffolds.insert((x, y));
    }
  }

  let target = *scaffolds
    .iter()
    .filter(|p| {
      p != &&vacuum
        && successors(**p)
          .into_iter()
          .filter(|c| scaffolds.contains(&c))
          .count()
          == 1
    })
    .exactly_one()
    .unwrap();

  for y in 0..=maxy {
    for x in 0..=maxx {
      print!(
        "{}",
        if (x, y) == vacuum {
          '@'
        } else if (x, y) == target {
          '$'
        } else if scaffolds.contains(&(x, y)) {
          '#'
        } else {
          '.'
        }
      );
    }
    println!();
  }

  let alignment = scaffolds
    .iter()
    .filter(|p| successors(**p).into_iter().all(|c| scaffolds.contains(&c)))
    .fold(0, |acc, (x, y)| acc + x * y);

  println!("Part 1: {}", alignment);

  let mut path: Vec<(Turn, usize)> = Vec::new();
  while vacuum != target {
    let next = apply_facing(vacuum, facing);
    if scaffolds.contains(&next) {
      vacuum = next;
      path.last_mut().unwrap().1 += 1;
      continue;
    }
    let (turn, new_facing) = [Turn::Left, Turn::Right]
      .into_iter()
      .map(|turn| (turn, apply_turn(facing, turn)))
      .filter(|(_, facing)| scaffolds.contains(&apply_facing(vacuum, *facing)))
      .exactly_one()
      .unwrap();
    facing = new_facing;
    path.push((turn, 0));
  }
  println!(
    "{}",
    path.iter().fold(String::new(), |mut acc, (turn, steps)| {
      if !acc.is_empty() {
        acc.push(',');
      }
      acc.push(*turn as u8 as char);
      acc.push(',');
      acc.push_str(format!("{}", steps).as_str());
      acc
    })
  );

  if args.peek().is_none() {
    println!(
      "Good luck! Once solved, add [main program] [A] [B] [C] as arguments"
    );
    return;
  }

  let input = args.join("\n");
  println!("{}", input);

  let context = OwnedContext::new(
    ProgrammedInput::new(
      input
        .chars()
        .chain(['\n', 'n', '\n'])
        .map(|c| c as u8 as Value),
    ),
    SingletonValue::from(0),
  );

  let mut p2_values = values.clone();
  p2_values[0] = 2;
  let mut program = Program::new(p2_values, context);
  program.run_until_completed();

  println!("Part 2: {}", *program.context.output);
}

fn successors((x, y): Position) -> impl Iterator<Item = Position> {
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)].into_iter()
}

#[inline]
fn apply_facing((x, y): Position, (dx, dy): Position) -> Position {
  (x + dx, y + dy)
}

fn apply_turn((dx, dy): Position, direction: Turn) -> Position {
  match direction {
    Turn::Left => (dy, -dx),
    Turn::Right => (-dy, dx),
  }
}
