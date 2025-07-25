#![feature(strict_overflow_ops)]
use std::{collections::HashSet, io::stdin};

use itertools::Itertools;

#[derive(Clone, Copy)]
struct Instr {
  op: Op,
  val: isize,
}

#[derive(Clone, Copy)]
enum Op {
  Acc,
  Jmp,
  Nop,
}

enum ProgramResult {
  Indefinite(isize),
  Terminated(isize),
}

fn run_program(instrs: &[Instr]) -> ProgramResult {
  let mut visited = HashSet::new();
  let mut isp = 0;
  let mut acc = 0;

  loop {
    if visited.contains(&isp) {
      break ProgramResult::Indefinite(acc);
    } else if isp == instrs.len() {
      break ProgramResult::Terminated(acc);
    }

    visited.insert(isp);

    let instr = &instrs[isp];

    match instr.op {
      Op::Acc => acc += instr.val,
      Op::Jmp => {
        isp = isp.strict_add_signed(instr.val);
        continue;
      }
      Op::Nop => {}
    }
    isp += 1;
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let instrs = lines
    .into_iter()
    .map(|line| {
      let (raw_op, raw_val) = line.split(' ').collect_tuple().unwrap();

      let op = match raw_op {
        "acc" => Op::Acc,
        "jmp" => Op::Jmp,
        "nop" => Op::Nop,
        _ => unimplemented!(),
      };

      let val = raw_val.parse().unwrap();

      Instr { op, val }
    })
    .collect_vec();

  println!(
    "Part 1: {}",
    match run_program(&instrs) {
      ProgramResult::Indefinite(val) => val,
      _ => unreachable!(),
    }
  );

  let acc = (0..instrs.len())
    .filter(|i| matches!(instrs[*i].op, Op::Jmp | Op::Nop))
    .map(|i| {
      let local_instrs = {
        let mut instrs = instrs.clone();
        match instrs[i].op {
          Op::Jmp => instrs[i].op = Op::Nop,
          Op::Nop => instrs[i].op = Op::Jmp,
          Op::Acc => unreachable!(),
        }
        instrs
      };

      run_program(&local_instrs)
    })
    .find_map(|res| match res {
      ProgramResult::Indefinite(_) => None,
      ProgramResult::Terminated(val) => Some(val),
    })
    .unwrap();

  println!("Part 2: {}", acc);
}
