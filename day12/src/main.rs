#![feature(strict_overflow_ops)]
use std::{collections::HashMap, io::stdin, str::FromStr};

use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
enum Src {
  Reg(char),
  Lit(i32),
}

impl Src {
  fn get(&self, regs: &HashMap<char, i32>) -> i32 {
    match self {
      Self::Reg(reg) => regs.get(reg).copied().unwrap_or_default(),
      Self::Lit(val) => *val,
    }
  }
}

impl FromStr for Src {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let first_c = s.chars().next().unwrap();
    if first_c.is_digit(10) {
      Ok(Self::Lit(s.parse().unwrap()))
    } else {
      Ok(Self::Reg(first_c))
    }
  }
}

#[derive(Debug, Clone, Copy)]
enum Instr {
  Cpy(Src, char),
  Inc(char),
  Dec(char),
  Jnz(Src, isize),
}

impl Instr {
  fn run(&self, registers: &mut HashMap<char, i32>, isp: &mut usize) -> bool {
    match self {
      Self::Cpy(src, reg) => {
        registers.insert(*reg, src.get(&registers));
      }
      Self::Inc(reg) => {
        *registers.entry(*reg).or_default() += 1;
      }
      Self::Dec(reg) => {
        *registers.entry(*reg).or_default() -= 1;
      }
      Self::Jnz(src, offset) => {
        if src.get(&registers) != 0 {
          *isp = isp.strict_add_signed(*offset);
          return true;
        }
      }
    }
    false
  }
}

impl FromStr for Instr {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let mut parts = s.split_whitespace();

    Ok(match parts.next().unwrap() {
      "cpy" => Self::Cpy(
        parts.next().unwrap().parse().unwrap(),
        parts.next().unwrap().chars().next().unwrap(),
      ),
      "inc" => Self::Inc(parts.next().unwrap().chars().next().unwrap()),
      "dec" => Self::Dec(parts.next().unwrap().chars().next().unwrap()),
      "jnz" => Self::Jnz(
        parts.next().unwrap().parse().unwrap(),
        parts.next().unwrap().parse().unwrap(),
      ),
      _ => return Err(()),
    })
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();

  let instrs: Vec<Instr> =
    lines.iter().map(|line| line.parse().unwrap()).collect_vec();

  let mut isp = 0;
  let mut registers: HashMap<char, i32> = HashMap::new();
  while let Some(instr) = instrs.get(isp) {
    if !instr.run(&mut registers, &mut isp) {
      isp += 1;
    }
  }

  println!(
    "Part 1: {}",
    registers.get(&'a').copied().unwrap_or_default()
  );

  let mut isp = 0;
  let mut registers: HashMap<char, i32> = HashMap::new();
  registers.insert('c', 1);
  while let Some(instr) = instrs.get(isp) {
    if !instr.run(&mut registers, &mut isp) {
      isp += 1;
    }
  }

  println!(
    "Part 2: {}",
    registers.get(&'a').copied().unwrap_or_default()
  );
}
