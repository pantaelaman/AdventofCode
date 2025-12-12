use std::{collections::HashSet, io::stdin};

use intcode::{OwnedContext, Program, ProgrammedInput, SingletonValue};
use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let values = lines
    .iter()
    .flat_map(|line| line.split(',').map(|n| n.parse::<i128>().unwrap()))
    .collect_vec();

  let verify = move |x, y| {
    let mut program = Program::new(
      values.iter().copied(),
      OwnedContext::new(ProgrammedInput::new([x, y]), SingletonValue::from(0)),
    );
    program.run_until_interrupt();
    *program.context.output == 1
  };

  let mut included = HashSet::new();
  for y in 0..50 {
    for x in 0..50 {
      if verify(x, y) {
        included.insert((x, y));
        print!("#");
      } else {
        print!(".");
      }
    }
    println!();
  }

  println!("Part 1: {}", included.len());

  let (lx, ly) = included
    .iter()
    .max_by(|(ax, ay), (bx, by)| ay.cmp(by).then(ax.cmp(bx).reverse()))
    .unwrap();

  let (mut dx, mut dy) = (*lx, *ly);

  loop {
    dy += 1;
    while !verify(dx, dy) {
      dx += 1;
    }

    if verify(dx + 99, dy - 99) {
      break;
    }
  }

  let px = dx;
  let py = dy - 99;

  println!("Part 2: {}", px * 10000 + py);
}
