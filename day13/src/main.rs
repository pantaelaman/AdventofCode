use std::{
  collections::{HashMap, HashSet},
  fs::File,
};

use intcode::{
  file_to_values, OwnedContext, Program, ProgrammedInput, SingletonValue,
  StatefulContext, Value,
};
use itertools::Itertools;

#[derive(Default)]
struct State {
  paddle: (Value, Value),
  ball: (Value, Value),
  blocks: HashSet<(Value, Value)>,
  check: bool,
  output_queue: Vec<Value>,
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let values = file_to_values(file);

  let context =
    OwnedContext::new(ProgrammedInput::new([]), SingletonValue::from(0));

  let mut program = Program::new(values.clone(), context);
  let mut grid: HashMap<(Value, Value), Value> = HashMap::new();
  while !program.is_complete() {
    program.run_until_interrupt();
    let x = *program.context.output;
    program.run_until_interrupt();
    let y = *program.context.output;
    program.run_until_interrupt();
    let tile = *program.context.output;

    grid.insert((x, y), tile);
  }

  let num_blocks = grid.values().filter(|tile| **tile == 2).count();
  println!("Part 1: {}", num_blocks);

  let context = StatefulContext::new(
    State::default(),
    |s: &mut State| {
      s.check = true;
      (s.ball.0 - s.paddle.0).signum()
    },
    |s: &mut State, o| s.output_queue.push(o),
  );

  let mut game_values = values.clone();
  game_values[0] = 2;
  let mut program = Program::new(game_values, context);

  let mut score = 0;
  while !program.is_complete() {
    if program.context.state.check {
      if program.context.state.blocks.is_empty() {
        break;
      }
      program.context.state.check = false;
    }

    if !program.run_for(3) {
      break;
    }
    let (x, y, tile) = std::mem::take(&mut program.context.state.output_queue)
      .into_iter()
      .collect_tuple()
      .unwrap();

    if (x, y) == (-1, 0) {
      score = tile;
      continue;
    }

    match tile {
      0 => {
        program.context.state.blocks.remove(&(x, y));
      }
      1 => {}
      2 => {
        program.context.state.blocks.insert((x, y));
      }
      3 => {
        program.context.state.paddle = (x, y);
      }
      4 => {
        program.context.state.ball = (x, y);
      }
      _ => unimplemented!(),
    };
  }

  println!("Part 2: {}", score);
}
