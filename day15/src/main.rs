use intcode::{file_to_values, OwnedContext, Program, SingletonValue, Value};
use pathfinding::directed::bfs::bfs;
use std::{
  collections::{HashMap, HashSet, VecDeque},
  fs::File,
};

type Position = (Value, Value);

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let values = file_to_values(file);

  let context = OwnedContext::new(VecDeque::new(), SingletonValue::from(0));
  let mut program = Program::new(values, context);

  let mut visited: HashSet<(Position, Position)> = HashSet::new();
  let mut walls: HashSet<Position> = HashSet::new();
  let mut position = (0, 0);
  let mut facing = (0, -1);
  let mut oxygen = None;
  visited.insert((position, facing));

  // walk to first wall
  loop {
    program.context.input.push_back(facing_to_dir(facing));
    program.run_until_interrupt();
    if *program.context.output == 0 {
      break;
    }
  }

  loop {
    let mut new_facing = facing;
    for rot in ROT_PRIORITY.iter() {
      new_facing = rot(facing);
      program.context.input.push_back(facing_to_dir(new_facing));
      program.run_until_interrupt();
      if *program.context.output == 0 {
        walls.insert(apply_direction(position, new_facing));
        continue;
      }
      break;
    }
    facing = new_facing;
    if update_to_target(
      &mut position,
      &facing,
      *program.context.output,
      &mut visited,
      &mut oxygen,
    ) {
      break;
    }
  }

  let walkable: HashSet<Position> = visited.iter().map(|(p, _)| *p).collect();

  let (minx, miny, maxx, maxy) = walls.iter().chain(walkable.iter()).fold(
    (Value::MAX, Value::MAX, Value::MIN, Value::MIN),
    |(minx, miny, maxx, maxy), (x, y)| {
      (minx.min(*x), miny.min(*y), maxx.max(*x), maxy.max(*y))
    },
  );

  // let mut frontier = vec![(0, 0)];
  // let mut steps = 0;
  // let mut previous = HashMap::new();
  // 'bfs: loop {
  //   for (x, y) in std::mem::take(&mut frontier) {
  //     if (x, y) == oxygen.unwrap() {
  //       break 'bfs;
  //     }
  //     let successors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  //       .into_iter()
  //       .filter(|p| !walls.contains(p))
  //       .filter_map(|p| {
  //         (!previous.contains_key(&p)).then(|| {
  //           previous.insert(p, (x, y));
  //           p
  //         })
  //       });
  //     frontier.extend(successors);
  //   }
  //   steps += 1;
  // }

  // let mut path = HashSet::new();
  // let mut next_prev = oxygen.unwrap();
  // while let Some(prev_node) = previous.get(&next_prev) {
  //   path.insert(next_prev);
  //   next_prev = *prev_node;
  // }

  // println!("Part 1: {}", steps);

  let path = bfs(
    &(0, 0),
    |(x, y): &Position| {
      [(*x + 1, *y), (*x - 1, *y), (*x, *y + 1), (*x, *y - 1)]
        .into_iter()
        .filter(|p| !walls.contains(p))
    },
    |p: &Position| p == oxygen.as_ref().unwrap(),
  )
  .unwrap()
  .into_iter()
  .collect::<HashSet<Position>>();

  let mut count = 0;
  for y in miny..=maxy {
    for x in minx..=maxx {
      print!(
        "{}",
        if (x, y) == (0, 0) {
          'O'
        } else if oxygen.as_ref().map(|p| &(x, y) == p).unwrap_or_default() {
          '%'
        } else if walls.contains(&(x, y)) {
          '\u{2588}'
        } else if path.contains(&(x, y)) {
          count += 1;
          '#'
        } else {
          '.'
        }
      );
    }
    println!();
  }

  println!("{}", count);
  println!("Part 1: {}", path.len() - 1);
}

fn update_to_target(
  position: &mut Position,
  facing: &Position,
  output: Value,
  visited: &mut HashSet<(Position, Position)>,
  oxygen: &mut Option<Position>,
) -> bool {
  let target = apply_direction(*position, *facing);
  match output {
    1 => {}
    2 => *oxygen = Some(target),
    _ => unimplemented!(),
  }
  *position = target;
  !visited.insert((target, *facing))
}

fn apply_direction((x, y): Position, (dx, dy): Position) -> Position {
  (x + dx, y + dy)
}

fn facing_to_dir(facing: Position) -> Value {
  match facing {
    (0, -1) => 1,
    (0, 1) => 2,
    (-1, 0) => 3,
    (1, 0) => 4,
    _ => unimplemented!(),
  }
}

const ROT_PRIORITY: [fn(Position) -> Position; 4] =
  [rot_counterclockwise, rot_none, rot_clockwise, rot_invert];

fn rot_none((dx, dy): Position) -> Position {
  (dx, dy)
}

fn rot_clockwise((dx, dy): Position) -> Position {
  (-dy, dx)
}

fn rot_counterclockwise((dx, dy): Position) -> Position {
  (dy, -dx)
}

fn rot_invert((dx, dy): Position) -> Position {
  (-dx, -dy)
}
