use std::{
  cmp::{max, min},
  io::stdin,
};

use itertools::{iterate, Itertools};

struct Instruction {
  direction: i32,
  count: i32,
}

fn pos_mod(n: i32, m: i32) -> i32 {
  (n % m + m) % m
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let instrs = lines
    .into_iter()
    .map(|line| {
      let (first, last) = line.split_at(1);
      let direction = match first {
        "L" => -1,
        "R" => 1,
        _ => unimplemented!(),
      };
      let count = last.parse::<i32>().unwrap();

      Instruction { direction, count }
    })
    .collect_vec();

  let password = instrs
    .iter()
    .scan(50, |i, instr| {
      println!("{i}");
      *i += instr.direction * instr.count;
      *i %= 100;
      Some(*i)
    })
    .filter(|i| *i == 0)
    .count();

  println!("Part 1: {}", password);

  let password = instrs
    .iter()
    .fold((50, 0), |(i, clicks), instr| {
      let short_count =
        min(instr.count, (100 - i * instr.direction).rem_euclid(100));
      let i0 = (i + short_count * instr.direction) % 100;
      let added_clicks =
        (i0 == 0) as i32 - (i == 0) as i32 + (instr.count - short_count) / 100;
      let raw_i = i0 + (instr.count - short_count) * instr.direction;

      println!(
        "{} + {} -> {} + {} => {} ({} : {})",
        i,
        short_count * instr.direction,
        i0,
        (instr.count - short_count) * instr.direction,
        raw_i.rem_euclid(100),
        added_clicks,
        clicks + added_clicks
      );

      (raw_i.rem_euclid(100), clicks + added_clicks)
    })
    .1;

  println!("Part 2: {}", password);
}
