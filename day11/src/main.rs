use std::{
  collections::{HashSet, VecDeque},
  fs::File,
};

use intcode::{
  file_to_values, IOPipeline, OwnedContext, Program, RefContext,
  SingletonValue, Value,
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let values = file_to_values(file);

  let p1_context = OwnedContext::new(VecDeque::new(), SingletonValue::from(0));
  let p2_context = OwnedContext::new(VecDeque::new(), SingletonValue::from(0));

  let mut p1_program = Program::new(values.clone(), p1_context);
  let mut p2_program = Program::new(values.clone(), p2_context);

  let mut position = (0, 0);
  let mut facing = (0, 1);
  let mut painted: HashSet<(Value, Value)> = HashSet::new();
  let mut white: HashSet<(Value, Value)> = HashSet::new();
  while !p1_program.is_complete() {
    painted.insert(position);
    step_program(&mut p1_program, &mut white, &mut position, &mut facing);
  }

  println!("Part 1: {}", painted.len());

  let mut position = (0, 0);
  let mut facing = (0, 1);
  let mut white: HashSet<(Value, Value)> = HashSet::new();
  white.insert((0, 0));
  while !p2_program.is_complete() {
    step_program(&mut p2_program, &mut white, &mut position, &mut facing);
  }

  let (minx, miny, maxx, maxy) = white.iter().fold(
    (Value::MAX, Value::MAX, Value::MIN, Value::MIN),
    |(minx, miny, maxx, maxy), (x, y)| {
      (*x.min(&minx), *y.min(&miny), *x.max(&maxx), *y.max(&maxy))
    },
  );

  for y in miny..=maxy {
    let y = (maxy - y) + miny;
    for x in minx..=maxx {
      let x = (maxx - x) + minx;
      print!("{}", if white.contains(&(x, y)) { '#' } else { ' ' });
    }
    println!();
  }
}

fn turn(facing: (Value, Value), direction: Value) -> (Value, Value) {
  match direction {
    0 => (facing.1, -facing.0),
    1 => (-facing.1, facing.0),
    _ => unimplemented!(),
  }
}

fn step_program(
  program: &mut Program<OwnedContext<VecDeque<Value>, SingletonValue>>,
  white: &mut HashSet<(Value, Value)>,
  position: &mut (Value, Value),
  facing: &mut (Value, Value),
) {
  program
    .context
    .input
    .push_back(white.contains(&position) as Value);

  program.run_until_interrupt();
  match *program.context.output {
    0 => white.remove(position),
    1 => white.insert(*position),
    _ => unimplemented!(),
  };

  program.run_until_interrupt();
  *facing = turn(*facing, *program.context.output);
  position.0 += facing.0;
  position.1 += facing.1;
}
